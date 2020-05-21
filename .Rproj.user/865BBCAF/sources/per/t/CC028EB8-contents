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
  c= c("Name" , "PassengerId"  , "Cabin")
  data = data[, -which(names(data) %in% c )]
  
  if(sum(names(data) %in% "Survived") > 0 )
    data$Survived = as.factor(data$Survived)
  if(sum(is.na(data$Age)) > 0)
    data[is.na(data$Age) , "Age"] = median(data$Age , na.rm = TRUE)
  if(sum(is.na(data$Embarked)) > 0)
    data[is.na(data$Embarked) , "Embarked"] = levels(data$Embarked)[runif(1 , 1 , 3)]
  if(sum(is.na(data$Fare)) > 0)
    data[is.na(data$Fare) , "Fare"] = median(data$Fare , na.rm = TRUE)  
  
  data
}

explore = function (data)
{
  str(data)
  
  # Find number of NA's in each variable 
  find_na = apply(is.na(data),2,sum)
  
  # Find the percentage of NA's in each variable 
  find_na_percent = apply(is.na(data) , 2 , sum  ) / nrow(data) *100
  
  # Saving the names of variables which contain NA values 
  c = names(find_na)[find_na > 0]
  
  # Print names of each variable that contains NA values and the nummber of NA values in it
  print(find_na[ which(names(find_na) %in% c )] )
  
  # Print names of each variable that contains NA values and the percentage of NA values in it
  print(find_na_percent[ which(names(find_na_percent) %in% c )])
  
  # Cleaning the data 
  data = clean(data)
  
  # Plotting relations bettween variables to find patterns 
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
testing  = clean(testing)


support_vectors_machine = function (training , testing)
{
  training = training[ , -which(names(training) %in% "Ticket" )]
  testing = testing[ , -which(names(testing) %in% "Ticket" )]
  
  
  # Training the model using support vectors machoine with a linear kernel 
  modelFit = svm(Survived ~ . ,
                 data = training , 
                 type = "C-classification",
                 kernel = "linear"
                 )
  
  # Predicting on the testing data and showing accuracy 
  pred = predict(modelFit , testing)
  print(confusionMatrix(pred , testing$Survived))
  print(confusionMatrix(pred , testing$Survived)$overall[1] *100 )
  c = list(pred , confusionMatrix(pred , testing$Survived)$overall[1] *100 , modelFit)
  c
}


support_vectors_machine_model = support_vectors_machine(training , testing)
support_vectors_machine_model[[2]]



random_forest = function(training , testing )
{
  
  training = training[ , -which(names(training) %in% "Ticket" )]
  testing = testing[ , -which(names(testing) %in% "Ticket" )]
  
  
  # Saving a call to the trainControl() with method = Cross Validation 
  tCall = trainControl(method = "cv" , number  = 5)
  
  # Training the model on the training data with random forest and Cross Validation 
  modelFit = train(Survived ~ . , method = "rf" , data = training , trControl = tCall)
  
  # Predicting on the testing data and showing accuracy 
  pred = predict(modelFit , testing)
  print(confusionMatrix(pred , testing$Survived))
  print(confusionMatrix(pred , testing$Survived)$overall[1] *100 )
  c = list(pred , confusionMatrix(pred , testing$Survived)$overall[1] *100, modelFit)
  c
}

#random_forest_model = random_forest(training , testing )
random_forest_model[[2]]



naive_bayes = function(training , testing )
{
  
  # Training the model on the training data with naive bayes and enabling laplace for smoothing effect
  modelFit = naiveBayes(Survived ~ . , method = "nb" , data = training  , laplace = 1)
  
  #Predicting on the testing data and showing accuracy 
  pred = predict(modelFit , testing)
  print(confusionMatrix(pred , testing$Survived))
  print(confusionMatrix(pred , testing$Survived)$overall[1] *100 )
  c = list(pred , confusionMatrix(pred , testing$Survived)$overall[1] *100 , modelFit)
  c
}

naive_bayes_model = naive_bayes(training , testing )
naive_bayes_model[[2]]



decision_tree = function(training , testing )
{
  
  training = training[ , -which(names(training) %in% "Ticket" )]
  testing = testing[ , -which(names(testing) %in% "Ticket" )]
  
  # Training the model on the training data
  modelFit  = rpart(Survived ~ . , data = training)
  modelFit
  
  #Predicting on the testing data and showing accuracy 
  pred = predict(modelFit , newdata = testing)
  p = vector()
  for(i in 1:length(pred[,2]))
  {
    if(pred[i , 1] >= pred[i, 2])
      p = c(p , 0)
    else
      p = c(p , 1)
  }
  p = as.factor(p)
  print(confusionMatrix(p , testing$Survived))
  print(confusionMatrix(p , testing$Survived)$overall[1] *100 )
  c = list(p , confusionMatrix(p , testing$Survived)$overall[1] *100, modelFit)
  c
}

decision_tree_model = decision_tree(training , testing )
decision_tree_model [[2]]




linear_discriminant_analysis = function(training , testing )
{
  
  # Training the model on the training data with linear discriminant analysis
  modelFit = naiveBayes(Survived ~ . , method = "lda" , data = training  )
  
  #Predicting on the testing data and showing accuracy 
  pred = predict(modelFit , testing)
  print(confusionMatrix(pred , testing$Survived))
  print(confusionMatrix(pred , testing$Survived)$overall[1] *100 )
  c = list(pred , confusionMatrix(pred , testing$Survived)$overall[1] *100 , modelFit)
  c
}


linear_discriminant_analysis_model = linear_discriminant_analysis(training , testing )
linear_discriminant_analysis_model[[2]]



ada_boost = function(training , testing)
{
  library(ada)
  
  training = training[ , -which(names(training) %in% "Ticket" )]
  testing = testing[ , -which(names(testing) %in% "Ticket" )]
  
  # Training the model on the training data with ada boost 
  modelFit = ada(Survived ~ . , data = training )
  modelFit
  
  #Predicting on the testing data and showing accuracy 
  pred = predict(modelFit , testing)
  print(confusionMatrix(pred , testing$Survived))
  print(confusionMatrix(pred , testing$Survived)$overall[1] *100 )
  c = list(pred , confusionMatrix(pred , testing$Survived)$overall[1] *100 , modelFit)
  c
}

ada_boost_model = ada_boost(training , testing )
ada_boost_model[[2]]




models_pred = vector()
models_acc = vector()
models_modelFit = vector()

models_pred = c(  support_vectors_machine_model[1] , 
                  random_forest_model[1],
                  naive_bayes_model[1],
                  decision_tree_model[1],
                  linear_discriminant_analysis_model[1],
                  ada_boost_model[1]
)

models_acc= c(  support_vectors_machine_model[2] , 
                  random_forest_model[2],
                  naive_bayes_model[2],
                  decision_tree_model[2],
                  linear_discriminant_analysis_model[2],
                  ada_boost_model[2]
)

models_modelFit= c(  support_vectors_machine_model[3] , 
                  random_forest_model[3],
                  naive_bayes_model[3],
                  decision_tree_model[3],
                  linear_discriminant_analysis_model[3],
                  ada_boost_model[3]
)

c = vector()
for(i in models_acc )
{
  names(i[[1]]) = NULL
  c = c(c , i[[1]])
}
models_acc = c
modelsDF = data.frame (model = c("support vectors machine" ,
                                 "random forest" ,
                                 "naive bayes" ,
                                 "decision tree",
                                 "linear discriminant analysis",
                                 "ada boosting"
                                 ) 
                       , 
                       accuracy = models_acc
                       )

head(modelsDF)

names(models_pred[1][[1]] ) = NULL


predDF = data.frame(pred1 = models_pred[[1]] ,
                    pred2  =models_pred[[2]] ,
                    pred3 = models_pred[[3]] ,
                    pred4 = models_pred[[4]] ,
                    pred5 = models_pred[[5]] ,
                    pred6 = models_pred[[6]] ,
                    Survived = testing$Survived
                    )


combModelFit = train(Survived ~ .  , method = "gam" , data = predDF)
combPred = predict(combModelFit , predDF )
confusionMatrix(combPred , testing$Survived)$overall[1]


Quiz = function (models_modelFit , TEST , combModelFit)
{
    
  pred1 = predict(models_modelFit[[1]] , TEST)
  pred2 = predict(models_modelFit[[2]] , TEST)
  pred3 = predict(models_modelFit[[3]] , TEST)
  pred4 = predict(models_modelFit[[4]] , TEST)
  pred5 = predict(models_modelFit[[5]] , TEST)
  pred6 = predict(models_modelFit[[6]] , TEST)
  
  names(pred1) = NULL
  p = vector()
  for(i in 1:length(pred4[,2]))
  {
    if(pred4[,1]>= pred4[,2])
      p = c(p , 0)
    else
      p = c(p , 1)
  }
  pred4 = p 
  
  pred1 = as.factor(pred1)
  pred2 = as.factor(pred2)
  pred3 = as.factor(pred3)
  pred4 = as.factor(pred4)
  pred5 = as.factor(pred5)
  pred6 = as.factor(pred6)
  
  
  predVDF = data.frame(pred1   = pred1, pred2 = pred2 ,  pred3  = pred3 ,
                       pred4 = pred4 , pred5  = pred5 , pred6 = pred6 
                       )
  combPredV = predict(combModelFit , predVDF)
  combPredV
}
pid = TEST$PassengerId
TEST = clean(TEST)
sol = Quiz(models_modelFit , TEST , combModelFit)

solution = data.frame(PassengerId  = pid , Survived = sol)
write.csv(solution , "solution.csv" ,row.names = FALSE)

