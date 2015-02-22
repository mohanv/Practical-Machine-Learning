library(caret)
library(randomForest)




set.seed(12345)

training0 <- read.csv(url("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"), na.strings=c("NA"," ",""))
sample0 <- read.csv(url("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"), na.strings=c("NA"," ",""))

# clean the data by removing columns with NAs etc
training1 <- apply(training0, 2, function(x) {sum(is.na(x))})
training <- training0[,which(training1 == 0)]
training <- training[8:length(training)]

sample1 <- apply(sample0, 2, function(x) {sum(is.na(x))})
sample <- sample0[,which(sample1 == 0)]
sample <- sample[8:length(sample)]


inTrain <- createDataPartition(y=training$classe, p=0.7, list=FALSE)
trainData <- training[inTrain, ]; 
testData <- training[-inTrain, ]

modelFit <- randomForest(classe ~ ., data = trainData)

modelPred <- predict(modelFit, testData)
confusionMatrix(testData$classe, modelPred)


modelPredSample <- predict(modelFit,sample)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(modelPredSample)