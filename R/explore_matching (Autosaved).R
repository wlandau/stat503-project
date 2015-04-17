missing.hist = function(){
  library(ggplot2)
  x = dataWithMissings()[, -1]
  numna = data.frame(Missing = apply(x, 2, function(i){mean(is.na(i))}))
  ggplot(numna) + geom_histogram(aes(x = Missing), binwidth = 0.025) + xlab("Percent Missing")
}

matchingHist = function(){
  library(ggplot2)
  ggplot(data.frame(Matching = subsetVariables()$Matching)) + geom_histogram(aes(x = Matching), binwidth=.0125) + xlab("Matching heuristic")
}

student.factor.matchings.plot = function(n = NULL, y.arg = "Factor"){
  library(ggplot2)
  x = subsetVariables()

  pl = ggplot(x[1:n,]) + geom_point(aes_string(x = "Matching", y = y.arg)) + xlab("Matching score") + theme(axis.text.x = element_text(angle = -90, hjust = -.01, vjust = .5))
  if(y.arg == "Factor")
    pl = pl + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  pl
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


