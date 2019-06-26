library(readr)
library(lubridate)
library(caret)
library(tidyverse)
library(anchors)
library(magrittr)
library(parallel)
library(doParallel)

trainingDF <- read.csv("./UJIndoorLoc/trainingData.csv", 
                       header = TRUE, 
                       sep = ",")
trainingDF<-distinct(trainingDF)  
trainingDF <- trainingDF[!duplicated(trainingDF[,521:529]),]
validationDF <- read.csv("./UJIndoorLoc/validationData.csv", 
                         header = TRUE, 
                         sep = ",")

trainingDF <- filter(trainingDF, BUILDINGID == 0)
validationDF <- filter(validationDF, BUILDINGID == 0)

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
waps_only_training[waps_only_training < (0.40)] <- 0

waps_only_validation[waps_only_validation < (0.40)] <- 0

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

# remove the rows with no variance-----------------------------------------
training_dataSet <- training_dataSet[apply(training_dataSet[,1:(ncol(training_dataSet)-1)], 1, var) != 0,]

# train control for training -------------------------
ctrl1 <- trainControl(method = "repeatedcv", 
                      number=10, 
                      repeats=1)

# train model with svm to predict floor-------------------------------------
B0_svmModel<- train(FLOOR ~., 
                    data = training_dataSet, 
                    method = "svmLinear",
                    tuneLength = 3,
                    trControl = ctrl1)
B0_svmModel
# Accuracy   Kappa    
# 0.9873775  0.9831019

B0_svm_PredictResult <- predict(B0_svmModel, newdata = validation_dataSet)
confusionMatrix(B0_svm_PredictResult,validation_dataSet$FLOOR)
# Accuracy   Kappa
# 0.9646     0.9499

# train model with ranger to predict floor-------------------------------------
B0_rangerModel<- train(FLOOR ~., 
                    data = training_dataSet, 
                    method = "ranger",
                    #tuneLength = 3,
                    trControl = ctrl1)
B0_rangerModel
# Accuracy   Kappa    
# 0.9967648  0.9956691

B0_ranger_PredictResult <- predict(B0_rangerModel, newdata = validation_dataSet)
confusionMatrix(B0_ranger_PredictResult,validation_dataSet$FLOOR)
# Accuracy   Kappa
#0.972      0.9604

# train model with knn to predict floor-------------------------------------
B0_knnModel<- train(FLOOR ~., 
                       data = training_dataSet, 
                       method = "knn",
                       tuneLength = 3,
                       trControl = ctrl1)
B0_knnModel
# k  Accuracy   Kappa    
# 5  0.9938563  0.9917749

B0_knn_PredictResult <- predict(B0_knnModel, newdata = validation_dataSet)
confusionMatrix(B0_knn_PredictResult,validation_dataSet$FLOOR)
# Accuracy   Kappa
# 0.9627     0.9473 

validation_dataSet$LATITUDE <- validationDF$LATITUDE
validation_dataSet$LONGITUDE <- validationDF$LONGITUDE
validation_dataSet<-validation_dataSet%>%
  mutate(error.model=abs(as.numeric(B0_ranger_PredictResult)-(as.numeric(FLOOR))))  
validation_dataSet$error.model<-as.character(validation_dataSet$error.model)
df <- filter(validation_dataSet, validation_dataSet$error.model != 0)
trainingDF$PHONEID <- as.factor(trainingDF$PHONEID)
trainingDF$USERID <- as.factor(trainingDF$USERID)
validationDF$PHONEID <- as.factor(validationDF$PHONEID)
validationDF$USERID <- as.factor(validationDF$USERID)

ggplot() + 
  geom_point(data=trainingDF, aes(x= LONGITUDE, y= LATITUDE, color= USERID)) +
  xlim(-7700, -7575)+
  ylim(4864875, 4865025)+
  facet_grid(. ~ FLOOR) 

ggplot() + 
  geom_point(data=validationDF, aes(x= LONGITUDE, y= LATITUDE, color= PHONEID)) +
  xlim(-7700, -7575)+
  ylim(4864875, 4865025)+
  facet_grid(. ~ FLOOR) 

ggplot() + 
  geom_point(data=df, aes(x= LONGITUDE, y= LATITUDE)) +
  xlim(-7700, -7575)+
  ylim(4864875, 4865025)+
  facet_grid(. ~ FLOOR) 
