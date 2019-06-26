library(readr)
library(lubridate)
library(caret)
library(tidyverse)

trainingDF <- read.csv("./UJIndoorLoc/trainingData.csv", 
                       header = TRUE, 
                       sep = ",")
trainingDF<-distinct(trainingDF)  
trainingDF <- trainingDF[!duplicated(trainingDF[,521:529]),]
validationDF <- read.csv("./UJIndoorLoc/validationData.csv", 
                         header = TRUE, 
                         sep = ",")

trainingDF <- filter(trainingDF, BUILDINGID == 1)
validationDF <- filter(validationDF, BUILDINGID == 1)

# remove the rows with no variance-----------------------------------------
trainingDF <- trainingDF[apply(trainingDF[,1:520], 1, var) != 0,]

# select all waps variable ---------------------------------
waps_only_training <- trainingDF[, c(1:520)]
waps_only_validation <-validationDF[,c(1:520)]

# convert 100 to - 105 ---------------------------------------------------
waps_only_training[waps_only_training == 100] <- -105
waps_only_validation[waps_only_validation == 100] <- -105

#normalize the data (0-1) ---------------------------------------------------------
waps_only_training <- as.data.frame(t(apply(waps_only_training, 1, function(x) (x - min(x))/(max(x)-min(x)))))
waps_only_validation <- as.data.frame(t(apply(waps_only_validation, 1, function(x) (x - min(x))/(max(x)-min(x)))))

# change value more then -30 and less than -90 to -105(very weak signal) ----------------------------
waps_only_training[waps_only_training < (0.65)] <- 0

waps_only_validation[waps_only_validation < (0.65)] <- 0

# remove waps with no variance ---------------------------------
training_dataSet <- waps_only_training[ ,apply(waps_only_training, 2, var) != 0] 
validation_dataSet <- waps_only_validation[,apply(waps_only_validation, 2, var) != 0] #520 ---> 348

#Removed all Waps that are not share between training and validation data ---------
training <- (colnames(training_dataSet)%in%colnames(validation_dataSet))
training_dataSet <- training_dataSet[,-c(which(training==FALSE))]  

# add FLOOR to data set ---------------------------------
training_dataSet$FLOOR <- trainingDF$FLOOR
validation_dataSet$FLOOR <- validationDF$FLOOR

# convert Floor to factor ---------------------------------------------------------
training_dataSet$FLOOR <- as.factor(training_dataSet$FLOOR)
validation_dataSet$FLOOR <- as.factor(validation_dataSet$FLOOR)

# 
# validation_dataSet$LATITUDE <- validationDF$LATITUDE
# validation_dataSet$LONGITUDE <- validationDF$LONGITUDE
# validation_dataSet$USERID <- validationDF$USERID
# validation_dataSet$PHONEID <- validationDF$PHONEID
# validation_dataSet$USERID <- as.factor(validation_dataSet$USERID)
# validation_dataSet$PHONEID <- as.factor(validation_dataSet$PHONEID)

# remove the rows with no variance-----------------------------------------
training_dataSet <- training_dataSet[apply(training_dataSet[,1:(ncol(training_dataSet)-1)], 1, var) != 0,]

# train control for training-------------------------------------
ctrl1 <- trainControl(method = "repeatedcv", 
                      number=10, 
                      repeats=1)

# train model with svm to predict floor-------------------------------------
B1_svmModel<- train(FLOOR ~., 
                    data = training_dataSet, 
                    method = "svmLinear",
                    #tuneLength = 3,
                    trControl = ctrl1)
B1_svmModel
# Accuracy   Kappa    
# 0.9842417  0.9788711

B1_svm_PredictResult <- predict(B1_svmModel, newdata = validation_dataSet)
confusionMatrix(B1_svm_PredictResult,validation_dataSet$FLOOR)
# Accuracy   Kappa
# 0.9251      0.89

# train model with ranger to predict floor ---------------------------
B1_rangerModel<- train(FLOOR ~., 
                    data = training_dataSet, 
                    method = "ranger",
                    #tuneLength = 3,
                    trControl = ctrl1)
B1_rangerModel
# Accuracy   Kappa    
# 0.9919843  0.9892529

B1_ranger_PredictResult <- predict(B1_rangerModel, newdata = validation_dataSet)
confusionMatrix(B1_ranger_PredictResult,validation_dataSet$FLOOR)
# Accuracy   Kappa
# 0.9511     0.9268

# train model with knn to predict floor ---------------------------
B1_knnModel<- train(FLOOR ~., 
                       data = training_dataSet, 
                       method = "knn",
                       tuneLength = 3,
                       trControl = ctrl1)
B1_knnModel
# k  Accuracy   Kappa    
# 5  0.9829069  0.9770847

B1_knn_PredictResult <- predict(B1_knnModel, newdata = validation_dataSet)
confusionMatrix(B1_knn_PredictResult,validation_dataSet$FLOOR)
# Accuracy   Kappa
# 0.9251     0.8892

validation_dataSet$LATITUDE <- validationDF$LATITUDE
validation_dataSet$LONGITUDE <- validationDF$LONGITUDE
validation_dataSet<-validation_dataSet%>%
  mutate(error.model=abs(as.numeric(B1_ranger_PredictResult)-(as.numeric(FLOOR))))  
validation_dataSet$error.model<-as.character(validation_dataSet$error.model)
df <- filter(validation_dataSet, validation_dataSet$error.model != 0)
trainingDF$PHONEID <- as.factor(trainingDF$PHONEID)
trainingDF$USERID <- as.factor(trainingDF$USERID)
trainingDF$SPACEID <- as.factor(trainingDF$SPACEID)
trainingDF$RELATIVEPOSITION <- as.factor(trainingDF$RELATIVEPOSITION)

validationDF$PHONEID <- as.factor(validationDF$PHONEID)
validationDF$USERID <- as.factor(validationDF$USERID)
validationDF$SPACEID <- as.factor(validationDF$SPACEID)
validationDF$RELATIVEPOSITION <- as.factor(validationDF$RELATIVEPOSITION)

dd <- trainingDF %>% filter(USERID != 13)
ggplot() + 
  geom_point(data=trainingDF, aes(x= LONGITUDE, y= LATITUDE, color= USERID)) +
  xlim(-7579, -7404)+
  ylim(4864810, 4864960)+
  facet_grid(. ~ FLOOR) 
ggplot() + 
  geom_point(data=validationDF, aes(x= LONGITUDE, y= LATITUDE, color= RELATIVEPOSITION)) +
  xlim(-7579, -7404)+
  ylim(4864810, 4864960)+
  facet_grid(. ~ FLOOR) 
ggplot() + 
  geom_point(data=df, aes(x= LONGITUDE, y= LATITUDE)) +
  xlim(-7579, -7404)+
  ylim(4864810, 4864960)+
  facet_grid(. ~ FLOOR) 
