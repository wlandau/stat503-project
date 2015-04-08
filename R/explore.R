pvcor = function(){
  library(knitr)
  stu = studentUSA2012()
  scorenames = paste(rep(paste("PV", 1:5, sep=""), each = 2), c("MATH", "READ"), sep="")
  y = stu[,scorenames]
  m = round(cor(y), 2)
  colnames(m) = paste(c("mth", "rd"), rep(1:5, each = 2), sep="")
  kable(m, caption="\\label{tab:pv} correlation matrix of the raw reading and math scores of USA students. All the scores are tightly correlated, so predicting them as continuous responses may not be productive, especially since most of the student data is categorical. However, there is a way to make the responses more manageable. First, I remove the middle 50\\% of students (students for whom the sum of all the reading and math scores is between the 25th and 75th percentile). Second, I censor the data: students scoring above the 75th percentile overall are designated high-performing, and those scoring below the 25th percentile are designated low-performing.")
}