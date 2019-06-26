library(readr)
library(lubridate)
library(caret)
library(tidyverse)
# library(anchors)
# library(magrittr)
# library(parallel)
# library(doParallel)

trainingDF <- read.csv("./UJIndoorLoc/trainingData.csv", 
                       header = TRUE, 
                       sep = ",")
trainingDF<-distinct(trainingDF)  
trainingDF <- trainingDF[!duplicated(trainingDF[,521:529]),]

validationDF <- read.csv("./UJIndoorLoc/validationData.csv", 
                         header = TRUE, 
                         sep = ",")

trainingDF <- filter(trainingDF, BUILDINGID == 0)
validationDF_B0_Latitude <- filter(validationDF, BUILDINGID == 0)

# select all waps variable ---------------------------------
waps_only_training <- trainingDF[, c(1:520)]
waps_only_validation <-validationDF_B0_Latitude[,c(1:520)]

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
remove_waps <- c("WAP043","WAP294","WAP324")#
training_dataSet <- training_dataSet[,!(names(training_dataSet) %in% remove_waps)]
validation_dataSet <- validation_dataSet[,!(names(validation_dataSet) %in% remove_waps)]

# add LATITUDE to data set ---------------------------------
training_dataSet$LATITUDE <- trainingDF$LATITUDE
validation_dataSet$LATITUDE <- validationDF_B0_Latitude$LATITUDE

# remove the rows with no variance-----------------------------------------
training_dataSet <- training_dataSet[apply(training_dataSet[,1:(ncol(training_dataSet)-1)], 1, var) != 0,]
#validation_dataSet <- validation_dataSet[apply(validation_dataSet[,1:(ncol(training_dataSet)-1)], 1, var) != 0,]

training_dataSet[,1:129] <- abs (training_dataSet[,1:129])
training_dataSet[,1:129] <- log(training_dataSet[,1:129])
validation_dataSet[,1:129] <- abs (validation_dataSet[,1:129])
validation_dataSet[,1:129] <- log(validation_dataSet[,1:129])
# traincontrol to predict Latitude-------------------------------------
ctrl1 <- trainControl(method = "repeatedcv", 
                      number=10, 
                      repeats=1,
                      verboseIter = TRUE)


# train model with Knn to predict Latitude-------------------------------------

B0_knnModel<- train(LATITUDE ~ ., 
                 data = training_dataSet, 
                 method = "knn",
                 # tuneLength = 5,
                 trControl = ctrl1)
B0_knnModel
# k  RMSE  Rsquared       MAE      
# 5  2.727323  0.9930918  1.679634

B0_knn_PredictResult <- predict(B0_knnModel, newdata = validation_dataSet)
postResample(B0_knn_PredictResult,validation_dataSet$LATITUDE)
# RMSE  Rsquared       MAE  
# 4.8295337 0.9772477 3.0838237
validationDF_B0_Latitude$Predicted_LATITUDE <- B0_knn_PredictResult
# train model with svm to predict Latitude-------------------------------------
B0_xgbModel<- train(LATITUDE ~., 
                    data = training_dataSet, 
                    method = "xgbTree",
                    tuneLength = 3,
                    trControl = ctrl1)
B0_xgbModel
# RMSE  Rsquared       MAE
#4.943099  0.9771843  3.652678

B0_xgb_PredictResult <- predict(B0_xgbModel, newdata = validation_dataSet)
postResample(B0_xgb_PredictResult,validation_dataSet$LATITUDE)
# RMSE  Rsquared       MAE 
# 7.4534849 0.9453669 5.4261127

# train model with RF(ranger) to predict Latitude-------------------------------------
B0_rfModel<- train(LATITUDE ~ ., 
                    data = training_dataSet, 
                    method = "ranger",
                    # tuneLength = 5,
                    trControl = ctrl1)
B0_rfModel
# RMSE      Rsquared   MAE    
# 2.838415  0.9925649  1.906100

B0_rf_PredictResult <- predict(B0_rfModel, newdata = validation_dataSet)
postResample(B0_rf_PredictResult,validation_dataSet$LATITUDE)
# RMSE      Rsquared   MAE  
# 5.330057 0.972966 3.786228