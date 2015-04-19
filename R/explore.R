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

matching.dot.plot = function(country = "USA", .issue = "top_20", n = 20, max.na = floor(0.75*n), y.arg = "Description"){
  library(ggplot2)

  lst = getSubset(country = country, .issue = .issue, n = n, max.na = max.na)

  d = lst$dictionary
  d = d[order(d$Matching, decreasing = T),]

  d$Factor = ordered(d$Factor, rev(d$Factor))
  d$Description = ordered(d$Description, rev(d$Description))

  pl = ggplot(d) + geom_point(aes_string(x = "Matching", y = y.arg)) + xlab("Matching score") + theme(axis.text.x = element_text(angle = -90, hjust = -.01, vjust = .5))
  if(y.arg == "Factor")
    pl = pl + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  pl
}

subset.missing.hist = function(country = "USA", .issue = "top_20", n = 20, max.na = floor(0.75*n), by = "student"){
  library(ggplot2)

  lst = getSubset(country = country, .issue = .issue, n = n, max.na = max.na)

  if(by == "student"){
    ggplot(data.frame(nm = lst$na.student)) + 
      geom_histogram(aes(x = nm), binwidth = 1) + 
      xlab("Number of missing values for each student")
  } else {
    ggplot(data.frame(nm = lst$na.variable)) + 
      geom_histogram(aes(x = nm), binwidth = .05) + 
      xlab("Percentage of missing values for each variable")
  }
}

imputation.check = function(country = "USA", .issue = "top_20", n = 20, max.na = floor(0.75*n)){
  library(ggplot2)
  library(reshape2)
  library(gdata)

  lst = getSubset(country = country, .issue = .issue, n = n, max.na = max.na)
  colnames(lst$x) = colnames(lst$x.na) = lst$dictionary$Description 

  m1  = melt(lst$x, id.vars = NULL)
  m2  = melt(lst$x.na, id.vars = NULL)
  m2 = subset(m2, complete.cases(m2))

  m1$dataset = "Imputed"
  m2$dataset = "Original"

  m = rbind(m1, m2)

  ggplot(m) + 
    geom_boxplot(aes(x = dataset, y = value)) + 
    geom_point(aes(x = dataset, y = value), alpha = 0.25) + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = -90, hjust = -.01, vjust = .5),
strip.text.x = element_text(size = 5)) + 
    facet_wrap(~variable, scales = "free_x") 
}