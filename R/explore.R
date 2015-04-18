pvcor = function(country = "USA"){
  library(knitr)
  library(ggplot2)

  stu = rawStudent(country)
  scorenames = paste(rep(paste("PV", 1:5, sep=""), each = 2), c("MATH", "READ"), sep="")
  y = stu[,scorenames]
  m = cor(y)
  v = m[lower.tri(m)]
  qplot(v, geom = "histogram", binwidth = .025) + xlab("correlation")
}

matching.hist = function(country = "USA"){
  library(ggplot2)
  s = student(country)
  d = s$dictionary
  ggplot(d) + geom_histogram(aes(x = Matching), binwidth=.0125) + xlab("Matching heuristic")
}

missing.var.hist = function(country = "USA"){
  library(ggplot2)
  x = rawStudent(country)
  numna = data.frame(Missing = apply(x, 2, function(i){mean(is.na(i))}))
  ggplot(numna) + geom_histogram(aes(x = Missing), binwidth = 0.025) + xlab("Percent Missing")
}

plot.matching.by.issue = function(country = "USA"){
  library(ggplot2)
  library(plyr)

  lst = student(country)
  d = lst$dictionary

  order.by.issue = ddply(d, "Issue", function(df){
    data.frame(Issue = df$Issue[1], Matching = quantile(df$Matching, 0.75))
  })

  d$Issue = ordered(d$Issue, levels = order.by.issue$Issue[order(order.by.issue$Matching)])

  ggplot(d) + geom_boxplot(aes(x = Issue, y = Matching))  + geom_point(aes(x = Issue, y = Matching), alpha = 0.5) + theme_bw() + theme(axis.text.x = element_text(angle = -90, hjust = -.01, vjust = .5)) + ylab("Matching score")
}

top.matching.vars = function(country = "USA", n = 10, y.arg = "Description"){
  library(ggplot2)

  lst = student(country)

  d = lst$dictionary
  d = d[order(d$Matching, decreasing = T),]

  d$Factor = ordered(d$Factor, rev(d$Factor))
  d$Description = ordered(d$Description, rev(d$Description))

  pl = ggplot(d[1:n,]) + geom_point(aes_string(x = "Matching", y = y.arg)) + xlab("Matching score") + theme(axis.text.x = element_text(angle = -90, hjust = -.01, vjust = .5))
  if(y.arg == "Factor")
    pl = pl + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  pl
}



imputation.check = function(lst = getSubset(), new.max.na = 11){
  library(ggplot2)
  library(reshape2)
  library(gdata)

  x = lst$x
  x$id = 1:dim(x)[1]
  x$missing = lst$num.missing > new.max.na
  x$missing = factor(x$missing, levels = c(T, F), labels = paste(c(">", "<="), new.max.na))
  x$Success = lst$y

  m  = melt(x, id.vars = c("id", "Success", "missing"))

  des = lst$dictionary$Description
  names(des) = as.character(lst$dictionary$Factor)

  m$Description = des[as.character(m$variable)] 
  m$Description = substr(m$Description, 0, 35)

  ggplot(m) + 
    geom_boxplot(aes(x = missing, y = value)) + 
    geom_point(aes(x = missing, y = value), alpha = 0.25) + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = -90, hjust = -.01, vjust = .5),
strip.text.x = element_text(size = 5)) + 
    xlab("Number of survey question missing values") +
    ylab("value") + 
    facet_wrap(~Description, scales = "free_x") 
}