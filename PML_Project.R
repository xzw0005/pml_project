setwd("F:/Coursera/DataScience_JohnsHopkins/8_PracticalMachineLearning/pml_project/")
trainData = read.csv("./pml-training.csv", na.strings = c("NA", ""))
dim(trainData)
test20 = read.csv("./pml-testing.csv", na.strings = c("NA", ""))
missing = apply(trainData, 2, function(x) { sum(is.na(x)) })
mydata = trainData[, which(missing == 0)]
test20 = test20[, which(missing == 0)]
dim(mydata)
str(mydata)
table(mydata$classe)
train[, 1]
# cross validation
library(caret)
set.seed(123)
inTrain = createDataPartition(y = mydata$classe, p = .7, list = FALSE)
training = mydata[inTrain, -(1:6)]
testing = mydata[-inTrain, -(1:6)]
dim(training)
str(training)
## Model Selection
set.seed(666)
fitControl = trainControl(method = "cv", number = 6)
gbmMod = train(classe ~ ., data = training, method = "gbm",
               trControl = fitControl, verbose = FALSE)
gbmPred = predict(gbmMod, newdata = testing)
confusionMatrix(gbmPred, testing$classe)
rfMod = train(classe ~ ., data = training, method = "rf",
              trControl = fitControl)
rfMod$finalModel
rfPred = predict(rfMod, newdata = testing)
names(confusionMatrix(rfPred, testing$classe))
confusionMatrix(rfPred, testing$classe)

pred20 = predict(rfMod, newdata = test20[-(1:6)])
data.frame(pred20)
# Note: answers = c("B", "A", "B", "A", "A", "E", "D", "B", "A", "A", "B", "C", "B", "A", "E", "E", "A", "B", "B", "B")
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(pred20)