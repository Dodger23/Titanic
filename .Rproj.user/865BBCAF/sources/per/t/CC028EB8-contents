library(caret)
library(ROSE)
library(rpart)
library(e1071)
set.seed(333)
data = read.csv("train.csv")
TEST = read.csv("test.csv")
inTrain = createDataPartition(data$Survived , p = 0.75 , list = FALSE)
training = data[inTrain , ]
testing = data[-inTrain , ]






