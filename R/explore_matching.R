match_transform = function(a){
  a = as.vector(a)
  ua = unique(a)
  as.vector(factor(a, levels = ua, labels = 1:length(ua)))
}

matching = function(a, b){
  a = match_transform(a)
  b = match_transform(b)
  n = sum(a == b)
  N = length(a)
  max(n, N - n)/N
}

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

good.factors = function(){
  stu = extremeStudentUSA2012()
  candidates = stu[,8:500]
  candidates[, sapply(candidates, 
    function(x){is.factor(x) & length(levels(x)) < 10 & !all(is.na(x))})]
}

missing.hist = function(){
  library(ggplot2)
  x = good.factors()
  numna = data.frame(Missing = apply(x, 2, function(i){mean(is.na(i))}))
  ggplot(numna) + geom_histogram(aes(x = Missing), binwidth = 0.025) + xlab("Percent Missing")
}

student.factor.matchings = function(){

  f = "../cache/student_factor_matching.rds"
  if(file.exists(f))
    return(readRDS(f))

  stu = extremeStudentUSA2012()
  candidates = good.factors()

  matchings = sapply(candidates, function(x){
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

    max(apply(splits, 2, function(z){
      matching(y, z)
    }))
  })

  saveRDS(matchings, f)
  matchings
}

subsetVariables = function(n = NULL, y.arg = "Factor", cheatvars = T){
  Matching = student.factor.matchings()
  Factor = names(Matching)
  Factor = ordered(Factor, Factor[order(Matching)])

  d = read.csv("../dictionaries/student-dict.csv", head = T)
  rownames(d) = d$variable
  Description = as.character(d[as.character(Factor),]$description)
  Description = gsub("\x92", "", Description)

  if(Description[83] == Description[226])
    Description[226] = "Perceived Control - Can Succeed with Enough Effort (rep 2)"

  Description[Factor == "ST84Q01"] = "Class Mngmt: Students Interrupt/Teacher Early"
  Description[Factor == "ST84Q03"] = "Class Mngmt: Students Interrupt/Teacher Late"

  Description = ordered(Description, Description[order(Matching)])

  x = data.frame(Matching, Factor, Description)
  x = x[rev(order(Matching)),]

  if(!cheatvars){
    remove = grepl("Self-Efficacy", x$Description) | grepl("Familiarity with", x$Description) | grepl("Experience with", x$Description)
    x = x[!remove,]
  }

  if(!is.null(n))
    x = x[1:n,]
  x
}


student.factor.matchings.plot = function(n = NULL, y.arg = "Factor", cheatvars = T){
  library(ggplot2)
  x = subsetVariables(n, y.arg, cheatvars)

  pl = ggplot(x) + geom_point(aes_string(x = "Matching", y = y.arg)) + xlab("Matching Coefficient")
  if(y.arg == "Factor")
    pl = pl + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  pl
}

dataWithMissings = function(){
  f = "../cache/dataWithMissings.rds"
  if(file.exists(f))
    return(readRDS(f))

  x = subsetVariables(n = 20, cheatvars = F)
  stu = extremeStudentUSA2012()
  d = stu[, c("perform", as.character(x$Factor))]
  saveRDS(d, f)
  d
}

missingByStudent = function(){
  d = dataWithMissings()
  nas = data.frame(nas = apply(d, 1, function(x){sum(is.na(x))}))
  ggplot(nas) + geom_histogram(aes(x = nas), binwidth = 1) + xlab("Number missing")
}

imputedUSA = function(){
  f = "../cache/imputedUSA.rds"
  if(file.exists(f))
    return(readRDS(f))

  library(DMwR)

  d = dataWithMissings()
  nas = apply(d, 1, function(x){sum(is.na(x))})
  keep = nas <= 13
  d = d[keep, ]

  dnum = as.data.frame(sapply(d, function(x){
    scale(as.numeric(factor(x, levels = levels(x), labels = 1:length(levels(x)))))
  }))
  dnum$perform[dnum$perform > 0] = 1
  dnum$perform[dnum$perform < 0] = -1

  ki = knnImputation(dnum[,-1], k = 10)
  imputed = data.frame(Performance = dnum$perform, ki)
  saveRDS(imputed, f)
  imputed
}

