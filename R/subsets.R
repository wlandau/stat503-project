impute = function(lst, max.na){
  library(DMwR)

  x = lst$x
  y = lst$y

  na.student = apply(x, 1, function(v){sum(is.na(v))})
  na.variable = apply(x, 2, function(v){mean(is.na(v))})

  keep = na.student <= max.na
  x = x[keep,]
  y = y[keep]
  na.student.keep = na.student[keep]

  keep = na.variable < 0.7
  x = x[,keep]
  lst$dictionary = lst$dictionary[keep,]

  x.num = as.data.frame(sapply(x, function(v){
    scale(as.numeric(factor(v, levels = levels(v), labels = 1:length(levels(v)))))
  }))

  y.num = as.integer(as.character(factor(y, levels = c("high", "low"), labels = c(1, -1))))
  x.na = x.num
  x.num = knnImputation(x.num, k = 10)
  list(x = x.num, x.na = x.na, y = y, y.num = y.num, dictionary = lst$dictionary, na.student = na.student, na.student.keep = na.student.keep, na.variable = na.variable)
}

getSubset = function(country = "USA", .issue = "top_20", n = 20, max.na = floor(0.75*n)){
  f = paste("../cache/", country, "_", .issue, ".rds", sep="")
  if(file.exists(f))
    return(readRDS(f))

  s = student(country)

  if(.issue != "top_20"){
    s$x = s$x[, s$dictionary$Issue == .issue]
    s$dictionary = s$dictionary[s$dictionary$Issue == .issue,]
  }

  if(!is.null(n)){
    n0 = dim(s$dictionary)[1]
    if(n > n0){
      n = n0
      max.na = floor(0.75*n)
    }

    s$dictionary = s$dictionary[1:n,]
    s$x = s$x[, 1:n]
  } 

  if(n < 6)
    max.na = 1

  s = impute(s, max.na)
  saveRDS(s, f)
  s
}
