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
  candidates = candidates[, sapply(candidates, 
    function(x){is.factor(x) & length(levels(x)) < 10 & !all(is.na(x))})]

  d = read.csv("../dictionaries/student-dict.csv", head = T)
  rownames(d) = d$variable
  Factor = colnames(candidates)

  Description = as.character(d[as.character(Factor),]$description)
  Description = gsub("\x92", "", Description)

  if(Description[83] == Description[226])
    Description[226] = "Perceived Control - Can Succeed with Enough Effort (rep 2)"

  Description[Factor == "ST84Q01"] = "Class Mngmt: Students Interrupt/Teacher Early"
  Description[Factor == "ST84Q03"] = "Class Mngmt: Students Interrupt/Teacher Late"

   remove = grepl("Self-Efficacy", Description) | 
  grepl("Familiarity with", Description) |
  grepl("Experience with", Description) | 
  grepl("Openness for", Description) | 
  grepl("Problem Text Message", Description) | 
  grepl("Problem Route Selection", Description) | 
  grepl("Problem Ticket Machine", Description) | 
  grepl("Student Questionnaire Form", Description) |
  grepl("Overclaiming", Description) | 
  (Factor %in% c("ISCEDD", "ISCEDL", "ISCEDO"))
  
  ret = candidates[, !remove]
}

missing.hist = function(){
  library(ggplot2)
  x = dataWithMissings()[, -1]
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

subsetVariables = function(){
  Matching = student.factor.matchings()
  Factor = names(Matching)
  Factor = ordered(Factor, Factor[order(Matching)])

  d = read.csv("../dictionaries/student-dict.csv", head = T)
  rownames(d) = d$variable
  Description = as.character(d[as.character(Factor),]$description)
  Description = gsub("\x92", "", Description)

  for(lvl in names(table(Description)[table(Description) > 1])){
    for(j in which(Description == lvl))
      Description[j] = paste(lvl, " (rep", j, ")", sep="")
  }

  Description[Factor == "ST84Q01"] = "Class Mngmt: Students Interrupt/Teacher Early"
  Description[Factor == "ST84Q03"] = "Class Mngmt: Students Interrupt/Teacher Late"

  Description = ordered(Description, Description[order(Matching)])

  x = data.frame(Matching, Factor, Description)
  x[rev(order(Matching)),]
}

matchingHist = function(){
  library(ggplot2)
  ggplot(data.frame(Matching = subsetVariables()$Matching)) + geom_histogram(aes(x = Matching), binwidth=.0125) + xlab("Matching score")
}

student.factor.matchings.plot = function(n = NULL, y.arg = "Factor"){
  library(ggplot2)
  x = subsetVariables()

  pl = ggplot(x[1:n,]) + geom_point(aes_string(x = "Matching", y = y.arg)) + xlab("Matching score") + theme(axis.text.x = element_text(angle = -90, hjust = -.01, vjust = .5))
  if(y.arg == "Factor")
    pl = pl + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  pl
}

dataWithMissings = function(){
  f = "../cache/dataWithMissings.rds"
  if(file.exists(f))
    return(readRDS(f))

  x = subsetVariables()[1:20,]
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
  imputed = data.frame(Performance = as.factor(dnum$perform), ki)
  saveRDS(imputed, f)
  imputed
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



