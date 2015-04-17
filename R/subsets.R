impute = function(lst, max.na){
  library(DMwR)

  x = lst$x
  y = lst$y

  nas = apply(x, 1, function(v){sum(is.na(v))})
  keep = nas <= max.na
  d = d[keep, ]
  nas = nas[keep]

  dnum = as.data.frame(sapply(d, function(x){
    scale(as.numeric(factor(x, levels = levels(x), labels = 1:length(levels(x)))))
  }))
  dnum$success[dnum$success > 0] = 1
  dnum$success[dnum$success < 0] = -1

  ki = knnImputation(dnum[,-1], k = 10)
  imputed = data.frame(successance = as.factor(dnum$success), ki)
  imputed$number.missing = nas
  saveRDS(imputed, f)
  imputed
}

getSubset = function(issue = "top_20", n = 10, max.na = n/2){
  f = paste("../cache/imputed_", issue, ".rds", sep="")
  if(file.exists(f))
    return(readRDS(f))

  s = student()

  if(issue != "top.20"){
    s$x = s$x[, s$dictionary$Issue == issue]
    s$dictionary = s$dictionary[s$dictionary$Issue == issue,]
  }

  s$dictionary = s$dictionary[order(s$dictionary$Matching, decreasing = T),][1:n,]
  s$x = s$x[, as.character(s$dictionary$Factor)] 
}
