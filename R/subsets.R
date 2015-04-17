impute = function(lst, max.na){
  library(DMwR)

  x = lst$x
  y = lst$y

  nas = apply(x, 1, function(v){sum(is.na(v))})
  keep = nas <= max.na

  x = x[keep, ]
  y = y[keep]
  nas = nas[keep]

  x.num = as.data.frame(sapply(x, function(v){
    scale(as.numeric(factor(v, levels = levels(v), labels = 1:length(levels(v)))))
  }))

  y.num = as.integer(as.character(factor(y, levels = c("high", "low"), labels = c(1, -1))))

  x.num = knnImputation(x.num, k = 10)
  list(x = x.num, y = y, y.num = y.num, dictionary = lst$dictionary)
}

getSubset = function(issue = "top_20", n = 10, max.na = n/2){
  f = paste("../cache/imputed_", issue, ".rds", sep="")
  if(file.exists(f))
    return(readRDS(f))

  s = student()

  if(issue != "top_20"){
    s$x = s$x[, s$dictionary$Issue == issue]
    s$dictionary = s$dictionary[s$dictionary$Issue == issue,]
  }

  s$dictionary = s$dictionary[order(s$dictionary$Matching, decreasing = T),][1:n,]
  s$x = s$x[, as.character(s$dictionary$Factor)]
  impute(s, max.na)
}
