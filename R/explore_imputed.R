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
    xlab("Number of survey questions missed") +
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
  x = d[, !(colnames(d) %in% c("Performance", "number.missing"))]
  
  x.mds<-metaMDS(dist(x), k=2) 
  colnames(x.mds$points)<-c("MDS1", "MDS2") 
  df = data.frame(Performance = d$Performance, x.mds$points)
  qplot(MDS1, MDS2, data=df, color=Performance) + theme(aspect.ratio=1)
}
