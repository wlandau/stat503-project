extremeStudentUSA2012 = function(){
  stu = studentUSA2012()
  scorenames = paste(rep(paste("PV", 1:5, sep=""), each = 2), c("MATH", "READ"), sep="")
  scores = stu[,scorenames]
  totscores = apply(scores, 1, sum)
  keep = (totscores < quantile(totscores, .25)) | (totscores > quantile(totscores, .75))
  stu$perform = factor(totscores > median(totscores), levels = c(T, F), labels = c("high", "low"))
  stu$PVTOTAL = totscores
  stu = stu[keep,]
  stu
}

student.factor.impurities = function(){

  f = "../cache/student_factor_impurities.rds"
  if(file.exists(f))
    return(readRDS(f))

  stu = extremeStudentUSA2012()

  candidates = stu[,8:500]
  candidates = candidates[, sapply(candidates, 
    function(x){is.factor(x) & length(levels(x)) < 10 & !all(is.na(x))})]

  impurities = sapply(candidates, function(x){
    y = stu$perform[!is.na(x)]
    x = x[!is.na(x)]

    possible.splits.0 = lapply(0:length(levels(x)), function(l) combn(levels(x),l))
  possible.splits = lapply(possible.splits.0, function(x){as.data.frame(t(x))})

    splits = NULL
    for(df in possible.splits){
      df = as.matrix(df)
      for(i in 1:dim(df)[1]){
        split.on = df[i,]
        splits = cbind(splits, x %in% split.on)
      }
    }

    min(apply(splits, 2, function(z){
      if(length(unique(z)) < 2){
        0.5
      } else {
        p = mean(y[z] == "high")
        2*p*(1-p)
      }
    }))
  })

  saveRDS(impurities, f)
  impurities
}

student.factor.impurities.plot = function(){
  library(ggplot2)
  Impurity = student.factor.impurities()
  Factor = names(Impurity)
  Factor = ordered(Factor, Factor[rev(order(Impurity))])

  d = read.csv("../dictionaries/student-dict.csv", head = T)
  rownames(d) = d$variable
  Description = as.character(d[as.character(Factor),]$description)

  if(Description[83] == Description[226])
    Description[226] = "Perceived Control - Can Succeed with Enough Effort (rep 2)"

  Description = ordered(Description, Description[rev(order(Impurity))])

  x = data.frame(Impurity, Factor, Description)
  s = subset(x, Impurity < .2)
  ggplot(s) + geom_point(aes(x = Impurity, y = Description))
}
