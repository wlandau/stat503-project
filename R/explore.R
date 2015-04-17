pvcor = function(){
  library(knitr)
  library(ggplot2)

  stu = rawStudent()
  scorenames = paste(rep(paste("PV", 1:5, sep=""), each = 2), c("MATH", "READ"), sep="")
  y = stu[,scorenames]
  m = cor(y)
  v = m[lower.tri(m)]
  qplot(v, geom = "histogram", binwidth = .025) + xlab("correlation")
}

missing.hist = function(){
  library(ggplot2)
  x = dataWithMissings()[, -1]
  numna = data.frame(Missing = apply(x, 2, function(i){mean(is.na(i))}))
  ggplot(numna) + geom_histogram(aes(x = Missing), binwidth = 0.025) + xlab("Percent Missing")
}

matchingHist = function(){
  library(ggplot2)
  ggplot(data.frame(Matching = subsetVariables()$Matching)) + geom_histogram(aes(x = Matching), binwidth=.0125) + xlab("Matching heuristic")
}

student.factor.matchings.plot = function(n = NULL, y.arg = "Factor"){
  library(ggplot2)
  x = subsetVariables()

  pl = ggplot(x[1:n,]) + geom_point(aes_string(x = "Matching", y = y.arg)) + xlab("Matching score") + theme(axis.text.x = element_text(angle = -90, hjust = -.01, vjust = .5))
  if(y.arg == "Factor")
    pl = pl + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  pl
}

explore_by_issue = function(){
  library(ggplot2)
  library(plyr)

  x = subsetVariables()
  x$Issue = as.factor(unlist(issue(x$Factor)))

  by.issue = ddply(x, "Issue", function(df){
    data.frame(Issue = df$Issue[1], Matching = median(df$Matching))
  })

  x$Issue = ordered(x$Issue, levels = by.issue$Issue[order(by.issue$Matching)])

  ggplot(x) + geom_boxplot(aes(x = Issue, y = Matching))  + geom_point(aes(x = Issue, y = Matching), alpha = 0.5) + theme_bw() + theme(axis.text.x = element_text(angle = -90, hjust = -.01, vjust = .5)) + ylab("Matching score")
}

more.missing.parcoord = function(){
  library(ggplot2)
  library(reshape2)
  library(gdata)

  d = imputedUSA()
  d$id = 1:dim(d)[1]
  d$missing = d$number.missing > 11
  d$missing = factor(d$missing, levels = c(T, F), labels = c("> 11", "<= 11"))

  m  = melt(d, id.vars = c("id", "Performance", "number.missing", "missing"))

  dict = read.csv("../dictionaries/student-dict.csv", head = T)
  des = as.character(dict$description)
  vars = as.character(dict$variable)
  names(des) = vars

  m$Description = des[as.character(m$variable)] 
  m$Description[m$Description == "Vignette Classroom Management - Students Frequently Interrupt/Teacher Arrives Late"] = "students interrupt / teacher late"
  m$Description[m$Description == "Vignette Classroom Management - Students Frequently Interrupt/Teacher Arrives Early"] = "students interrupt / teacher early"
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

var.hist = function(){
  library(ggplot2)
  library(reshape2)

  d = imputedUSA()
  d$id = 1:dim(d)[1]
  d$Performance = factor(d$Performance, levels = c(-1, 1), labels = c("low", "high"))

  m  = melt(d, id.vars = c("id", "Performance", "number.missing"))

  dict = read.csv("../dictionaries/student-dict.csv", head = T)
  des = as.character(dict$description)
  vars = as.character(dict$variable)
  names(des) = vars

  m$Description = des[as.character(m$variable)] 
  m$Description[m$Description == "Vignette Classroom Management - Students Frequently Interrupt/Teacher Arrives Late"] = "students interrupt / teacher late"
  m$Description[m$Description == "Vignette Classroom Management - Students Frequently Interrupt/Teacher Arrives Early"] = "students interrupt / teacher early"
  m$Description = substr(m$Description, 0, 35)

  ggplot(m) + 
    geom_histogram(aes(x = value), binwidth = .25) + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = -90, hjust = -.01, vjust = .5), strip.text.x = element_text(size = 5)) + 
    ylab("value") + 
    facet_wrap(~Description, scales = "free_x")
}


response.parcoord = function(){
  library(ggplot2)
  library(reshape2)

  d = imputedUSA()
  d$id = 1:dim(d)[1]
  d$Performance = factor(d$Performance, levels = c(-1, 1), labels = c("low", "high"))

  m  = melt(d, id.vars = c("id", "Performance", "number.missing"))

  dict = read.csv("../dictionaries/student-dict.csv", head = T)
  des = as.character(dict$description)
  vars = as.character(dict$variable)
  names(des) = vars

  m$Description = des[as.character(m$variable)] 
  m$Description[m$Description == "Vignette Classroom Management - Students Frequently Interrupt/Teacher Arrives Late"] = "students interrupt / teacher late"
  m$Description[m$Description == "Vignette Classroom Management - Students Frequently Interrupt/Teacher Arrives Early"] = "students interrupt / teacher early"
  m$Description = substr(m$Description, 0, 35)

  ggplot(m) + 
    geom_boxplot(aes(x = Performance, y = value)) + 
    geom_point(aes(x = Performance, y = value), alpha = 0.25) + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = -90, hjust = -.01, vjust = .5), strip.text.x = element_text(size = 5)) + 
    ylab("value") + 
    facet_wrap(~Description, scales = "free_x")
}

mds.plot = function(){
  library(ggplot2)
  library(reshape2)
  library(vegan)

  d = imputedUSA()
  d$Performance = factor(d$Performance, levels = c(-1, 1), labels = c("low", "high"))

  wch = sample.int(dim(d)[1], 250)
  d = d[wch,]

  x = d[, !(colnames(d) %in% c("Performance", "number.missing"))]
  
  f = "../cache/mds.rds"
  if(file.exists(f)){
    x.mds = readRDS(f)
  } else {
    x.mds<-metaMDS(dist(x), k=2) 
    saveRDS(x.mds, f)
  }
  colnames(x.mds$points)<-c("MDS1", "MDS2") 
  df = data.frame(Performance = d$Performance, x.mds$points)
  qplot(MDS1, MDS2, data=df, color=Performance) + theme(aspect.ratio=1)
}

varcor = function(){
  library(ggplot2)
  d = imputedUSA()
  x = d[, !(colnames(d) %in% c("Performance", "number.missing"))]

  m = cor(x)
  v = m[lower.tri(m)]
  qplot(v, geom = "histogram", binwidth = .05)
}

