source("explore_matching.R")

hc = function(tab = T){
  library(knitr)
  library(ggplot2)

  d = imputedUSA()
  x = d[, !(names(d) %in% c("Performance", "number.missing"))]
  x = apply(x, 2, scale)

  hc = hclust(dist(x), method = "ward.D")
  hc2 = hclust(as.dist(1 - cor(t(x))^2), method = "ward.D")

  if(!tab){
    par(mfrow = c(1, 2))
    plot(hc, main = "Euclidian distance")
    plot(hc2, main = "Correlation distance")
    par(mfrow = c(1, 1))
  } else {
    cl = cutree(hc, 3)
    cl2 = cutree(hc2, 3)
    kable(table(cl, cl2), caption = "\\label{tab:hctab} Confusion matrix ")
  }
}

parcoord.clustering = function(Cluster, title = ""){
  library(ggplot2)
  library(reshape2)
  library(gdata)

  d = imputedUSA()
  d$id = 1:dim(d)[1]
  d$Cluster = factor(Cluster)

  m  = melt(d, id.vars = c("id", "Performance", "number.missing", "Cluster"))

  dict = read.csv("../dictionaries/student-dict.csv", head = T)
  des = as.character(dict$description)
  vars = as.character(dict$variable)
  names(des) = vars

  m$Description = des[as.character(m$variable)] 
  m$Description[m$Description == "Vignette Classroom Management - Students Frequently Interrupt/Teacher Arrives Late"] = "students interrupt / teacher late"
  m$Description[m$Description == "Vignette Classroom Management - Students Frequently Interrupt/Teacher Arrives Early"] = "students interrupt / teacher early"
  m$Description = substr(m$Description, 0, 35)

  ggplot(m) + 
    geom_boxplot(aes(x = Cluster, y = value)) + 
    geom_point(aes(x = Cluster, y = value, color = Performance), alpha = 0.25) + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = -90, hjust = -.01, vjust = .5),
strip.text.x = element_text(size = 5)) + 
    xlab("Number of survey question missing values") +
    ylab("value") + 
    labs(title = title) +
    facet_wrap(~Description, scales = "free_x") 
}

hceucparcoord = function(){
  d = imputedUSA()
  x = d[, !(names(d) %in% c("Performance", "number.missing"))]
  hc = hclust(dist(x), method = "ward.D")
  cl = cutree(hc, 3)
  parcoord.clustering(cl)
}

hccorparcoord = function(){
  d = imputedUSA()
  x = d[, !(names(d) %in% c("Performance", "number.missing"))]
  hc = hclust(as.dist(1 - cor(t(x))^2), method = "ward.D")
  cl = cutree(hc, 3)
  parcoord.clustering(cl)
}

somplot = function(){
  library(kohonen)
  library(fpc)
  
  d = imputedUSA()
  x = d[, !(names(d) %in% c("Performance", "number.missing"))]
  x = apply(x, 2, scale)

  s = som(x, grid = somgrid(xdim = 2, ydim = 2))
  cl = s$unit.classif

 parcoord.clustering(cl)
}
