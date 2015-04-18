regions = function(country = "USA"){
  if(country == "USA")
    return(c("United States of America", "Connecticut (USA)", "Florida (USA)", "Massachusetts (USA)"))
  else
    return(country)
}

cacheRawStudent = function(countries = c("USA", "Germany", "Japan", "Peru")){
  if(!("student2012" %in% ls())){
    print("loading PISA 2012 student dataset...")
    load(url("http://beta.icm.edu.pl/PISAcontest/data/student2012.rda"))
    print("Done.")
  }

  if(!file.exists("../cache/"))
    dir.create("../cache/")

  for(country in countries){
    f = paste("../cache/", country, "_rawStudent.rds", sep="")
    if(!file.exists(f)){
      rawStudent2012 = subset(student2012, CNT %in% regions(country))
      saveRDS(rawStudent2012, f)
    }
  }
}

rawStudent = function(country = "USA"){
  f = paste("../cache/", country, "_rawStudent.rds", sep="")
  if(!file.exists(f))
    cacheRawStudent()
  return(readRDS(f))
}

censorStudentSuccess = function(country = "USA"){
  stu = rawStudent(country)
  scorenames = paste(rep(paste("PV", 1:5, sep=""), each = 2), c("MATH", "READ"), sep="")
  scores = stu[,scorenames]
  totscores = apply(scores, 1, sum)
  keep = (totscores < quantile(totscores, .25)) | (totscores > quantile(totscores, .75))
  stu$success = factor(totscores > median(totscores), levels = c(T, F), labels = c("high", "low"))
  stu$PVTOTAL = totscores
  stu = stu[keep,]
  stu
}

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

matchings = function(x, y){
  sapply(x, function(v){
    y = y[!is.na(v)]
    v = v[!is.na(v)]
    possible.splits.0 = lapply(0:length(levels(v)), function(l) combn(levels(v),l))
    possible.splits = lapply(possible.splits.0, function(v){as.data.frame(t(v))})

    splits = NULL
    for(df in possible.splits){
      df = as.matrix(df)
      for(i in 1:dim(df)[1]){
        split.on = df[i,]
        splits = cbind(splits, v %in% split.on)
      }
    }

    max(apply(splits, 2, function(z){
      matching(y, z)
    }))
  })
}

student = function(country = "USA"){
  source("../R/issue.R")

  f = paste("../cache/", country, "_student.rds", sep="")
  if(file.exists(f))
    return(readRDS(f))

  stu = censorStudentSuccess(country)
  candidates = stu[,8:500]
  candidates = candidates[, sapply(candidates, 
    function(x){is.factor(x) & length(levels(x)) < 10 & !all(is.na(x))})]

  d = read.csv("../dictionaries/student-dict.csv", head = T)
  rownames(d) = d$variable
  Factor = colnames(candidates)

  Description = as.character(d[as.character(Factor),]$description)
  names(Description) = Factor
  Description = gsub("\x92", "", Description)

  if(Description[83] == Description[226])
    Description[226] = "Perceived ctrl - Succeed w/ Effort (rep 2)"

  Description[Factor == "ST84Q01"] = "Teacher Early/Students Interrupt"
  Description[Factor == "ST84Q03"] = "Teacher Late/Students Interrupt"

  Issue = issue(Factor)

  keep = (Issue != "self-efficacy-familiarity-experience") & (Issue != "other")
  x = candidates[, keep]

  dictionary = data.frame(
    Factor = colnames(x),
    Description = Description[colnames(x)],
    Issue = issue(colnames(x)),
    Matching = matchings(x, stu$success)[colnames(x)]
  )

  dictionary$Factor = ordered(dictionary$Factor, dictionary$Factor)
  s = list(y = stu$success, x = x, dictionary = dictionary)

  s$dictionary = s$dictionary[order(s$dictionary$Matching, decreasing = T),]
  s$x = s$x[, as.character(s$dictionary$Factor)]

  saveRDS(s, f)
  s
}