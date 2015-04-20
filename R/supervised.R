error.rates = function(country = "USA", .issue = "top_20"){
  set.seed(0)
  library(class)
  library(MASS)
  library(randomForest)
  library(nnet)
  library(e1071)

  source("../R/student.R")
  source("../R/subsets.R")

  lst = getSubset(country = country, .issue = .issue)

  n = length(lst$y)
  i = sample.int(n, floor(0.75*n))
  t = 1:n %in% i

  d = cbind(y = lst$y, lst$x)
  train = d[t,]
  test = d[!t,]

  d.num = cbind(y = lst$y.num, lst$x)
  train.num = d.num[t,]
  test.num = d.num[!t,]

  err = c()
  m = ncol(train) - 1

# logit

  train.num01 = train.num
  test.num01 = test.num

  train.num01$y[train.num01$y < 1] = 0
  test.num01$y[test.num01$y < 1] = 0
  mylogit = glm(y~., data = train.num01, family = binomial)
  yhat = predict(mylogit, newdata = test.num01, type = "response")
  yhat = yhat > 0.5
  err["logit"] = mean(yhat != test.num01$y)

# randomForest 

  rf = randomForest(y~., data = train)
  yhat = predict(rf, newdata = test)
  err["randomForest"] = mean(yhat != test$y)

# bagging

#  rf = randomForest(y~., data = train, mtry = m)
#  yhat = predict(rf, newdata = test)
#  err["bagging"] = mean(yhat != test$y)

# neuralNet(size = 2)

  listoffactors <- colnames(lst$x)
  f = as.formula(paste("y~",paste(listoffactors,collapse="+")))
  nn = nnet(f, data = train, size = 2)
  yhat = predict(nn, newdata = test, type = "class")
  err["neuralNet(size = 2)"] = mean(yhat != test$y)


# neuralNet(size = m/4)

  listoffactors <- colnames(lst$x)
  f = as.formula(paste("y~",paste(listoffactors,collapse="+")))
  nn = nnet(f, data = train, size = floor(m/4))
  yhat = predict(nn, newdata = test, type = "class")
  err["neuralNet(size = m/4)"] = mean(yhat != test$y)

# neuralNet(size = 3m/4)

  listoffactors <- colnames(lst$x)
  f = as.formula(paste("y~",paste(listoffactors,collapse="+")))
  nn = nnet(f, data = train, size = floor(3*m/4))
  yhat = predict(nn, newdata = test, type = "class")
  err["neuralNet(size = 3*m/4)"] = mean(yhat != test$y)

# svm(linear)
 
   sv = svm(y~., data = train, kernel = "linear") 
   yhat = predict(sv, newdata = test)
   err["svm(linear)"] = mean(yhat != test$y)

# svm(cubic)
 
   sv = svm(y~., data = train, kernel = "polynomial") 
   yhat = predict(sv, newdata = test)
   err["svm(cubic)"] = mean(yhat != test$y)

# svm(radial)
 
   sv = svm(y~., data = train, kernel = "radial") 
   yhat = predict(sv, newdata = test)
   err["svm(radial)"] = mean(yhat != test$y)

# lda

  ld = lda(y~., data = train)
  yhat = predict(ld, newdata = test)$class
   err["lda"] = mean(yhat != test$y)

# qda

  qd = qda(y~., data = train)
  yhat = predict(qd, newdata = test)$class
   err["qda"] = mean(yhat != test$y)
  
# nearestNeighbors

  for(k in c(5, 10, 25)){
    yhat = knn(train[,colnames(train) != "y"], test[colnames(test) != "y"], train$y, k = k)
    err[paste("knn(k = ", k, ")", sep="")] = mean(yhat != test$y)
  }

  err
}

err.one.country = function(country = "USA", issues = c("top_20", "teaching", "attitude-interest", "parent.backgrounds", "school.possessions")){
  library(ggplot2)

  f = paste("../cache/err_", country, "_", paste(issues, collapse = "_"), ".rds", sep = "")
  if(file.exists(f)){
    err = readRDS(f)
  } else {
    err = data.frame(Misclassification = c(), Method = c(), Issue = c())
    for(i in issues){
      e = error.rates(country, i)
      err = rbind(err, data.frame(Misclassification = e, Method = names(e), Issue = i))
    }
    saveRDS(err, f)
  }

  ggplot(err) + 
    geom_line(aes(x = Method, y = Misclassification, group = Issue, color = Issue)) +
    theme_bw() + theme(axis.text.x = element_text(angle = -90, hjust = -.01, vjust = .5)) + labs(title = country)
}

all.err = function(){
  library(gridExtra)

  p1 =  err.one.country(country = "USA", issues = c("top_20", "teaching", "attitude-interest", "parent.backgrounds", "school.possessions"))
  p2 =  err.one.country(country = "Japan", issues = c("teaching", "attitude-interest", "parent.backgrounds", "study-learn.outside.school", "course.content", "school.possessions"))
  p3 =  err.one.country(country = "Germany",  issues = c("teaching", "attitude-interest", "parent.backgrounds", "international-language", "attendance-truancy-repeat", "school.possessions"))
  p4 =  err.one.country(country = "Peru", issues = c("teaching", "attitude-interest", "parent.backgrounds", "attendance-truancy-repeat", "school.possessions"))

  grid.arrange(p1, p2, p3, p4, ncol = 2)
}