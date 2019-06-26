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
validationDF <- filter(validationDF, BUILDINGID == 2)

# select all waps variable ---------------------------------
waps_only_training <- trainingDF[, c(1:520)]
waps_only_validation <-validationDF[,c(1:520)]

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
training_dataSet$LONGITUDE <- trainingDF$LONGITUDE
validation_dataSet$LONGITUDE <- validationDF$LONGITUDE

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
B2_knnModel<- train(LONGITUDE ~ ., 
                    data = training_dataSet, 
                    method = "knn",
                    trControl = ctrl1)
B2_knnModel
# k  RMSE      Rsquared   MAE     
# 5  5.736009  0.9631672  3.176295

B2_knn_PredictResult <- predict(B2_knnModel, newdata = validation_dataSet)
postResample(B2_knn_PredictResult,validation_dataSet$LONGITUDE)
# RMSE Rsquared      MAE 
# 10.0198337  0.9034815  6.4735135
validationDF_B2_Latitude$Predicted_LONGITUDE <- B2_knn_PredictResult
holeValidationData <- rbind(validationDF_B0_Latitude,
                            validationDF_B1_Latitude,
                            validationDF_B2_Latitude)
ggplot(holeValidationData, aes(x = LONGITUDE, y = LATITUDE)) + 
  geom_point (stat="identity", position=position_dodge(),color="grey53") + 
  labs(x = "Longitude", y = "Latitude", title = "Predictions per Floor")  +
  facet_wrap(~FLOOR) + 
  geom_point(data = holeValidationData, aes(x = Predicted_LONGITUDE, y = Predicted_LATITUDE), color="blue")

# train model with Knn to predict Latitude-------------------------------------
B2_xgbModel<- train(LONGITUDE ~ ., 
                    data = training_dataSet, 
                    method = "xgbTree",
                    trControl = ctrl1)
B2_xgbModel
# k  RMSE      Rsquared   MAE     
# 5  5.736009  0.9631672  3.176295

B2_xgb_PredictResult <- predict(B2_xgbModel, newdata = validation_dataSet)
postResample(B2_xgb_PredictResult,validation_dataSet$LONGITUDE)
# RMSE Rsquared      MAE 
# 10.0198337  0.9034815  6.4735135
# train model with ranger to predict Latitude-------------------------------------
B2_rangerModel<- train(LONGITUDE ~ ., 
                    data = training_dataSet, 
                    method = "ranger",
                    trControl = ctrl1)
B2_rangerModel
# k  RMSE      Rsquared   MAE     
# 5  5.736009  0.9631672  3.176295

B2_ranger_PredictResult <- predict(B2_rangerModel, newdata = validation_dataSet)
postResample(B2_ranger_PredictResult,validation_dataSet$LONGITUDE)
# RMSE Rsquared      MAE 
# 10.0198337  0.9034815  6.4735135