# source("getData.R")

fullusa = mergeUSA2012()
d = fullusa[, c("SC14Q03", math, reading)] # teacher shortages
d.nona = subset(d, !is.na(SC14Q03))

library(randomForest)
rf = randomForest(SC14Q03 ~. , data = d.nona)
