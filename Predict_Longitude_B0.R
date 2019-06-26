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
validationDF <- filter(validationDF, BUILDINGID == 0)

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
remove_waps <- c("WAP043","WAP294","WAP324")#
training_dataSet <- training_dataSet[,!(names(training_dataSet) %in% remove_waps)]
validation_dataSet <- validation_dataSet[,!(names(validation_dataSet) %in% remove_waps)]

# add LATITUDE to data set ---------------------------------
training_dataSet$LONGITUDE <- trainingDF$LONGITUDE
validation_dataSet$LONGITUDE <- validationDF$LONGITUDE

# remove the rows with no variance-----------------------------------------
training_dataSet <- training_dataSet[apply(training_dataSet[,1:(ncol(training_dataSet)-1)], 1, var) != 0,]

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

B0_knnModel<- train(LONGITUDE ~ ., 
                    data = training_dataSet, 
                    method = "knn",
                    # tuneLength = 5,
                    trControl = ctrl1)
B0_knnModel
# k  RMSE      Rsquared   MAE     
# 5  3.364960  0.9819790  1.963424

B0_knn_PredictResult <- predict(B0_knnModel, newdata = validation_dataSet)
postResample(B0_knn_PredictResult,validation_dataSet$LONGITUDE)
# RMSE  Rsquared       MAE 
# 5.6760647 0.9541959 3.4516288
validationDF_B0_Latitude$Predicted_LONGITUDE <- B0_knn_PredictResult

# train model with xgbTree to predict Latitude-------------------------------------
B0_xgbTreeModel<- train(LONGITUDE ~., 
                    data = training_dataSet, 
                    method = "xgbTree",
                    tuneLength = 3,
                    trControl = ctrl1)
B0_xgbTreeModel
# RMSE   Rsquared    
# 

B0_xgbTree_PredictResult <- predict(B0_xgbTreeModel, newdata = validation_dataSet)
postResample(B0_xgbTree_PredictResult,validation_dataSet$LONGITUDE)
# RMSE   Rsquared
#

# train model with RF(ranger) to predict Latitude-------------------------------------
B0_rfModel<- train(LONGITUDE ~ ., 
                   data = training_dataSet, 
                   method = "ranger",
                   # tuneLength = 5,
                   trControl = ctrl1)
B0_rfModel
# k  RMSE      Rsquared     
# 5  

B0_rf_PredictResult <- predict(B0_rfModel, newdata = validation_dataSet)
postResample(B0_rf_PredictResult,validation_dataSet$LONGITUDE)
# RMSE  Rsquared  
# 