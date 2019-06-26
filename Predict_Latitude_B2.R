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

trainingDF <- filter(trainingDF, BUILDINGID == 2)
validationDF_B2_Latitude <- filter(validationDF, BUILDINGID == 2)

# select all waps variable ---------------------------------
waps_only_training <- trainingDF[, c(1:520)]
waps_only_validation <-validationDF_B2_Latitude[,c(1:520)]

# change value more then -30 and less than -90 to -105(very weak signal) ----------------------------
waps_only_training[waps_only_training > (-30)] <- -105
waps_only_training[waps_only_training< (-80)] <- -105

waps_only_validation[waps_only_validation > (-30)] <- -105
waps_only_validation[waps_only_validation< (-80)] <- -105

# remove waps with no variance ---------------------------------
training_dataSet <- waps_only_training[,apply(waps_only_training, 2, var) != 0] 
validation_dataSet <- waps_only_validation[,apply(waps_only_validation, 2, var) != 0] #520 ---> 348

#Removed all Waps that are not share between training and validation data ---------
training <- (colnames(training_dataSet)%in%colnames(validation_dataSet))
training_dataSet <- training_dataSet[,-c(which(training==FALSE))]  

validation<-(colnames(validation_dataSet)%in%colnames(training_dataSet))
validation_dataSet<-validation_dataSet[,-c(which(validation==FALSE))]  


# add LATITUDE to data set ---------------------------------
training_dataSet$LATITUDE <- trainingDF$LATITUDE
validation_dataSet$LATITUDE <- validationDF_B2_Latitude$LATITUDE

# remove the rows with no variance-----------------------------------------
training_dataSet <- training_dataSet[apply(training_dataSet[,1:(ncol(training_dataSet)-1)], 1, var) != 0,]

training_dataSet[,1:71] <- abs (training_dataSet[,1:71])
training_dataSet[,1:71] <- log(training_dataSet[,1:71])
validation_dataSet[,1:71] <- abs (validation_dataSet[,1:71])
validation_dataSet[,1:71] <- log(validation_dataSet[,1:71])

# traincontrol to predict Latitude-------------------------------------
ctrl1 <- trainControl(method = "repeatedcv", 
                      number=10, 
                      repeats=1,
                      verboseIter = TRUE)


# train model with Knn to predict Latitude-------------------------------------
B2_knnModel<- train(LATITUDE ~ ., 
                    data = training_dataSet, 
                    method = "knn",
                    trControl = ctrl1)
B2_knnModel
# k  RMSE      Rsquared   MAE     
# 5  4.837143  0.9719987  2.811990

B2_knn_PredictResult <- predict(B2_knnModel, newdata = validation_dataSet)
postResample(B2_knn_PredictResult,validation_dataSet$LATITUDE)
# RMSE  Rsquared       MAE 
# 8.1180605 0.9226945 5.3064037
validationDF_B2_Latitude$Predicted_LATITUDE <- B2_knn_PredictResult

# train model with rf to predict Latitude-------------------------------------
B2_rfModel<- train(LATITUDE ~ ., 
                    data = training_dataSet, 
                    method = "ranger",
                    trControl = ctrl1)
B2_rfModel
# RMSE       Rsquared   MAE     
# 4.868151  0.9716538

B2_rf_PredictResult <- predict(B2_rfModel, newdata = validation_dataSet)
postResample(B2_rf_PredictResult,validation_dataSet$LATITUDE)
# RMSE       Rsquared   MAE 
# train model with xgbTree to predict Latitude-------------------------------------
B2_xgbModel<- train(LATITUDE ~ ., 
                   data = training_dataSet, 
                   method = "xgbTree",
                   trControl = ctrl1)
B2_xgbModel
# RMSE       Rsquared   MAE 

B2_xgb_PredictResult <- predict(B2_xgbModel, newdata = validation_dataSet)
postResample(B2_xgb_PredictResult,validation_dataSet$LATITUDE)
# RMSE       Rsquared   MAE 