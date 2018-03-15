library(ISLR)
library(e1071)

set.seed(20180314)

data(Default)

kToTest <- c(2, 5, 10, 20, 50, 100, 200, 10000) 

res <- numeric(length(kToTest))

for(k in 1:length(kToTest)) {
  currentK <- kToTest[k]
  kfoldIndex <- matrix(sample(1:nrow(Default)), ncol = currentK)
  
  resCurrent <- numeric(currentK)
  
  for(i in 1:currentK) {
    currentTrain <- as.vector(kfoldIndex[, -i])
    currentTest <- kfoldIndex[, i]
    DefaultTrain <- Default[currentTrain, ]
    DefaultTest <- Default[currentTest, ]
    
    model <- naiveBayes(default ~ ., data = DefaultTrain)
    
    out <- predict(model, newdata = DefaultTest)
    
    resCurrent[i] <- mean(out!= DefaultTest[, 1])
  }
  print(currentK)
  # print(resCurrent)
  res[k] <- mean(resCurrent)
}

rbind(kToTest, res)
