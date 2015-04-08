

randomforest = function(){
  library(randomForest)
  d = imputedUSA()
  rf = randomForest(Performance ~., data = d)
}