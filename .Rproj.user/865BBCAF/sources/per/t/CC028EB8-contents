library(caret)
library(ROSE)
library(rpart)
library(e1071)
set.seed(333)

#Reading training and Quiz data
data = read.csv("data/train.csv" , na.strings = c("NA" , ""))
TEST = read.csv("data/test.csv" , na.strings = c("NA" ,""))


clean = function(data)
{
  data$Survived = as.factor(data$Survived)
  c= c("Name" , "PassengerId"  , "Cabin")
  data = data[, -which(names(data) %in% c )]
  data[is.na(data$Age) , "Age"] = median(data$Age , na.rm = TRUE)
  data[is.na(data$Embarked) , "Embarked"] = levels(data$Embarked)[runif(1 , 1 , 3)]
  data
}


explore = function (data)
{
  str(data)
  
  find_na = apply(is.na(data),2,sum)
  find_na_percent = apply(is.na(data) , 2 , sum  ) / nrow(data) *100
  c = names(find_na)[find_na > 0]
  print(find_na[ which(names(find_na) %in% c )] )
  print(find_na_percent[ which(names(find_na_percent) %in% c )])
  data = clean(data)
  
  png(filename = "images/featuresPlot.png" , width = 1366 , height = 768 , units = "px")
  p = featurePlot(x = data[,-which(names(data) %in% c("Ticket" , "Survived"))] ,y = data[,1] ,
              plot = "pairs" , 
              auto.key = list(columns = 2)
              )
  print(p)
  dev.off()
}

explore(data)


# Splitting data to training and testing data 
inTrain = createDataPartition(data$Survived , p = 0.75 , list = FALSE)
training = data[inTrain , ]
testing = data[-inTrain , ]

#cleaning training and testing
training = clean(training)
testing = clean(testing)




