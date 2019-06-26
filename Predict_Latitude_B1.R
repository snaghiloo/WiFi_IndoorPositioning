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
validationDF_B1_Latitude <- filter(validationDF, BUILDINGID == 1)

# remove the rows with no variance-----------------------------------------
trainingDF <- trainingDF[apply(trainingDF[,1:520], 1, var) != 0,]

# select all waps variable ---------------------------------
waps_only_training <- trainingDF[, c(1:520)]
waps_only_validation <-validationDF_B1_Latitude[,c(1:520)]

# convert 100 to - 105 ---------------------------------------------------
waps_only_training[waps_only_training == 100] <- -105
waps_only_validation[waps_only_validation == 100] <- -105

#normalize the data (0-1) ---------------------------------------------------------
waps_only_training <- as.data.frame(t(apply(waps_only_training, 1, function(x) (x - min(x))/(max(x)-min(x)))))
waps_only_validation <- as.data.frame(t(apply(waps_only_validation, 1, function(x) (x - min(x))/(max(x)-min(x)))))

# change value more then -30 and less than -90 to -105(very weak signal) ----------------------------
waps_only_training[waps_only_training < (0.65)] <- 0
#waps_only_validation[waps_only_validation < (0.65)] <- 0

# remove waps with no variance ---------------------------------
training_dataSet <- waps_only_training[,apply(waps_only_training, 2, var) != 0] 
validation_dataSet <- waps_only_validation[,apply(waps_only_validation, 2, var) != 0] #520 ---> 348

#Removed all Waps that are not share between training and validation data ---------
training <- (colnames(training_dataSet)%in%colnames(validation_dataSet))
training_dataSet <- training_dataSet[,-c(which(training==FALSE))]  

# validation<-(colnames(validation_dataSet)%in%colnames(training_dataSet))
# validation_dataSet<-validation_dataSet[,-c(which(validation==FALSE))]  

# add LATITUDE to data set ---------------------------------
training_dataSet$LATITUDE <- trainingDF$LATITUDE
validation_dataSet$LATITUDE <- validationDF_B1_Latitude$LATITUDE

# remove the rows with no variance-----------------------------------------
training_dataSet <- training_dataSet[apply(training_dataSet[,1:(ncol(training_dataSet)-1)], 1, var) != 0,]

# traincontrol to predict Latitude-------------------------------------
ctrl1 <- trainControl(method = "repeatedcv", 
                      number=10, 
                      repeats=1,
                      verboseIter = TRUE)


# train model with Knn to predict Latitude-------------------------------------
B1_knnModel<- train(LATITUDE ~ ., 
                    data = training_dataSet, 
                    method = "knn",
                    trControl = ctrl1)
B1_knnModel
# k  RMSE      Rsquared   MAE     
# 5  6.723683  0.9663620  4.33794

B1_knn_PredictResult <- predict(B1_knnModel, newdata = validation_dataSet)
postResample(B1_knn_PredictResult,validation_dataSet$LATITUDE)
# RMSE  Rsquared       MAE 
# 9.2365784 0.9311471 6.3842121
validationDF_B1_Latitude$Predicted_LATITUDE <- B1_knn_PredictResult

# train model with xgbTree to predict Latitude-------------------------------------
B1_xgbModel<- train(LATITUDE ~ ., 
                    data = training_dataSet, 
                    method = "xgbTree",
                    trControl = ctrl1)
B1_xgbModel
# RMSE      Rsquared   MAE     
# 8.059820  0.9519064   5.935914

B1_xgb_PredictResult <- predict(B1_xgbModel, newdata = validation_dataSet)
postResample(B1_xgb_PredictResult,validation_dataSet$LATITUDE)
# RMSE  Rsquared       MAE 
# 15.5058765  0.8796186 10.9165815

# train model with renger to predict Latitude-------------------------------------
B1_rengerModel<- train(LATITUDE ~ ., 
                    data = training_dataSet, 
                    method = "ranger",
                    trControl = ctrl1)
B1_rengerModel
# RMSE      Rsquared   MAE     
# 6.142036  0.9719431   4.139587

B1_renger_PredictResult <- predict(B1_rengerModel, newdata = validation_dataSet)
postResample(B1_renger_PredictResult,validation_dataSet$LATITUDE)
# RMSE  Rsquared       MAE 
# 9.4830502 0.9273637 6.6927337