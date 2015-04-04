student.factor.purities = function(){
  stu = readRDS("../cache/usaStudent2012.rds") 
  scorenames = paste(rep(paste("PV", 1:5, sep=""), each = 2), c("MATH", "READ"), sep="")
  scores = stu[,scorenames]
  totscores = apply(scores, 1, sum)
  keep = (totscores < quantile(totscores, .25)) | (totscores > quantile(totscores, .75))
  saveRDS(keep, "../cache/keptstudents.rds")
  stu$perform = factor(totscores > median(totscores), levels = c(T, F), labels = c("high", "low"))
  stu$PVTOTAL = totscores
  stu = stu[keep,]
  saveRDS(stu, "../cache/extreme_students.rds")

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
}
