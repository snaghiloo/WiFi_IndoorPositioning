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

trainingDF <- filter(trainingDF, BUILDINGID == 1)
validationDF <- filter(validationDF, BUILDINGID == 1)

# select all waps variable ---------------------------------
waps_only_training <- trainingDF[, c(1:520)]
waps_only_validation <-validationDF[,c(1:520)]

# change value more then -30 and less than -90 to -105(very weak signal) ----------------------------
waps_only_training[waps_only_training > (-30)] <- -105
waps_only_training[waps_only_training< (-95)] <- -105

waps_only_validation[waps_only_validation > (-30)] <- -105
waps_only_validation[waps_only_validation< (-95)] <- -105

# remove waps with no variance ---------------------------------
training_dataSet <- waps_only_training[,apply(waps_only_training, 2, var) != 0] 
validation_dataSet <- waps_only_validation[,apply(waps_only_validation, 2, var) != 0] #520 ---> 348

#Removed all Waps that are not share between training and validation data ---------
training <- (colnames(training_dataSet)%in%colnames(validation_dataSet))
training_dataSet <- training_dataSet[,-c(which(training==FALSE))]  

validation<-(colnames(validation_dataSet)%in%colnames(training_dataSet))
validation_dataSet<-validation_dataSet[,-c(which(validation==FALSE))]  

#----------------------------------------
remove_waps <- c("WAP107")
training_dataSet <- training_dataSet[,!(names(training_dataSet) %in% remove_waps)]
validation_dataSet <- validation_dataSet[,!(names(validation_dataSet) %in% remove_waps)]

# add LATITUDE to data set ---------------------------------
training_dataSet$LONGITUDE <- trainingDF$LONGITUDE
validation_dataSet$LONGITUDE <- validationDF$LONGITUDE

# remove the rows with no variance-----------------------------------------
training_dataSet <- training_dataSet[apply(training_dataSet[,1:(ncol(training_dataSet)-1)], 1, var) != 0,]

training_dataSet[,1:143] <- abs (training_dataSet[,1:143])
training_dataSet[,1:143] <- log(training_dataSet[,1:143])
validation_dataSet[,1:143] <- abs (validation_dataSet[,1:143])
validation_dataSet[,1:143] <- log(validation_dataSet[,1:143])
# traincontrol to predict Latitude-------------------------------------
ctrl1 <- trainControl(method = "repeatedcv", 
                      number=10, 
                      repeats=1)


# train model with Knn to predict Latitude-------------------------------------
B1_knnModel<- train(LONGITUDE ~ ., 
                    data = training_dataSet, 
                    method = "knn",
                    trControl = ctrl1)
B1_knnModel
# k  RMSE      Rsquared   MAE     
# 5  5.320436  0.9880060  2.570233

B1_knn_PredictResult <- predict(B1_knnModel, newdata = validation_dataSet)
postResample(B1_knn_PredictResult,validation_dataSet$LONGITUDE)
# RMSE  Rsquared       MAE 
# 9.2727398 0.9595292 5.9717755
validationDF_B1_Latitude$Predicted_LONGITUDE <- B1_knn_PredictResult

# train model with Knn to predict Latitude-------------------------------------
B1_xgbModel<- train(LONGITUDE ~ ., 
                    data = training_dataSet, 
                    method = "xgbTree",
                    trControl = ctrl1)
B1_xgbModel
# k  RMSE      Rsquared   MAE     
# 5  5.320436  0.9880060  2.570233

B1_xgb_PredictResult <- predict(B1_xgbModel, newdata = validation_dataSet)
postResample(B1_xgb_PredictResult,validation_dataSet$LONGITUDE)
# RMSE  Rsquared       MAE 
# 13.1648838  0.9196547  9.8223577
# train model with Knn to predict Latitude-------------------------------------
B1_rangerModel<- train(LONGITUDE ~ ., 
                    data = training_dataSet, 
                    method = "ranger",
                    trControl = ctrl1)
B1_rangerModel
# k  RMSE      Rsquared   MAE     
# 5  5.320436  0.9880060  2.570233

B1_ranger_PredictResult <- predict(B1_rangerModel, newdata = validation_dataSet)
postResample(B1_ranger_PredictResult,validation_dataSet$LONGITUDE)
# RMSE  Rsquared       MAE 
# 8.4308103 0.9663449 6.0410020
