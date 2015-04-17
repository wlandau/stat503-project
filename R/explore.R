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