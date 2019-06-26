library(readr)
library(lubridate)
library(caret)
library(tidyverse)

trainingDF <- read.csv("./UJIndoorLoc/trainingData.csv", 
                       header = TRUE, 
                       sep = ",")
trainingDF<-distinct(trainingDF)  # 19937 ---> 19300
trainingDF <- trainingDF[!duplicated(trainingDF[,521:529]),]
validationDF <- read.csv("./UJIndoorLoc/validationData.csv", 
                         header = TRUE, 
                         sep = ",")
# select all waps variable ---------------------------------
waps_only_training <- trainingDF[, c(1:520)]
waps_only_validation <-validationDF[,c(1:520)]

# change value more then -30 and less than -90 to -105(very weak signal) ----------------------------
waps_only_training[waps_only_training > (-30)] <- -105
waps_only_training[waps_only_training< (-90)] <- -105

waps_only_validation[waps_only_validation > (-30)] <- -105
waps_only_validation[waps_only_validation< (-90)] <- -105

# remove waps with no variance ---------------------------------
training_dataSet <- waps_only_training[,apply(waps_only_training, 2, var) != 0] #520 ---> 404
validation_dataSet <- waps_only_validation[,apply(waps_only_validation, 2, var) != 0] #520 ---> 348

#Removed all Waps that are not share between training and validation data ---------
training <- (colnames(training_dataSet)%in%colnames(validation_dataSet))
training_dataSet <- training_dataSet[,-c(which(training==FALSE))]  # 404 ---> 293

validation<-(colnames(validation_dataSet)%in%colnames(training_dataSet))
validation_dataSet<-validation_dataSet[,-c(which(validation==FALSE))]  #348 ---> 293

#add Building ID to training and validation data Set -----------------------------
training_dataSet$BUILDINGID <- trainingDF$BUILDINGID
validation_dataSet$BUILDINGID <- validationDF$BUILDINGID

#remove WAPs that never provided the best signal to any of trainset instanse------
remove_waps <- c("WAP268","WAP323")
training_dataSet <- training_dataSet[,!(names(training_dataSet) %in% remove_waps)]
validation_dataSet <- validation_dataSet[,!(names(validation_dataSet) %in% remove_waps)]

# remove all waps that selected as highWap in more than 1 building----------
remove_waps <- c("WAP046","WAP113","WAP172","WAP173","WAP175",
                 "WAP180","WAP181","WAP189","WAP248", "WAP114", "WAP478")
training_dataSet <- training_dataSet[,!(names(training_dataSet) %in% remove_waps)]
validation_dataSet <- validation_dataSet[,!(names(validation_dataSet) %in% remove_waps)]

# remove the rows with no variance-----------------------------------------
training_dataSet <- training_dataSet[apply(training_dataSet[,1:280], 1, var) != 0,]

#Add HighWAP -------------------------
training_dataSet<-training_dataSet %>% 
  mutate(HighWAP=NA)
validation_dataSet<-validation_dataSet %>% 
  mutate(HighWAP=NA)

training_dataSet<-training_dataSet %>% 
  mutate(HighWAP=colnames(training_dataSet[,1:280])[apply(training_dataSet[,1:280],1,which.max)])
validation_dataSet<-validation_dataSet %>% 
  mutate(HighWAP=colnames(validation_dataSet[,1:280])[apply(validation_dataSet[,1:280],1,which.max)])

# Transform BUILDINGID and HighWAP to factor
training_dataSet$HighWAP<-as.factor(training_dataSet$HighWAP)
validation_dataSet$HighWAP<-as.factor(validation_dataSet$HighWAP)

training_dataSet$BUILDINGID <- as.factor(training_dataSet$BUILDINGID)
validation_dataSet$BUILDINGID <- as.factor(validation_dataSet$BUILDINGID)

# ## Remove from "max_WAP" those WAPs that never provided the best signal to 
# ## any of the instance in training set-----------------------------------
# outersect <- function(x, y) {
#   x[!x%in%y]
# }
# 
# remove_WAPs<-unique(
#   outersect(validation_dataSet$HighWAP,
#             training_dataSet$HighWAP)
# )
# 
# remove_WAPs #ٌWAP268,WAP323

# # Are there same HighWAP in different Buildings? --------------------------
# RepWAPS<-training_dataSet  %>%
#   dplyr::select(HighWAP, BUILDINGID) %>%
#   distinct(HighWAP, BUILDINGID)
# 
# RepWAPS<-sort(RepWAPS$HighWAP[duplicated(RepWAPS$HighWAP)]) 
# RepWAPS #WAP046 WAP113 WAP172 WAP173 WAP175 WAP180 WAP181 WAP248 WAP248 WAP114 WAP478

# # Prepare Parallel Process
# cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
# registerDoParallel(cluster)


# 10 fold cross validation    
ctrl1 <- trainControl(method = "repeatedcv", 
                      number=10, 
                      repeats=1 #,allowParallel = TRUE
                      )
#predict Building ID with SVM ----------------------------------------------
svmModel<- train(BUILDINGID ~ HighWAP, 
                 data = training_dataSet, 
                 method = "svmLinear",
                 #tuneLength = 3,
                 trControl = ctrl1 )

svmModel
# Accuracy   Kappa    
# 0.9998581  0.9997687
svm_PredictResult <- predict(svmModel, newdata =  validation_dataSet) 
confusionMatrix(svm_PredictResult,validation_dataSet$BUILDINGID)
#Accuracy = 1, Kappa = 1

# svm_PredictResult <- predict(svmModel, newdata = training_dataSet)
# confusionMatrix(svm_PredictResult,training_dataSet$BUILDINGID)

#predict Building ID with ranger ----------------------------------------------
rangerModel<- train(BUILDINGID ~HighWAP, 
                 data = training_dataSet, 
                 method = "ranger",
                 #tuneLength = 3,
                 trControl = ctrl1)

rangerModel

ranger_PredictResult <- predict(rangerModel, newdata =  validation_dataSet) 
confusionMatrix(ranger_PredictResult,validation_dataSet$BUILDINGID)
#Accuracy = 1, Kappa = 1

#predict Building ID with c5.0 ----------------------------------------------
C5Model<- train(BUILDINGID ~ HighWAP, 
                 data = training_dataSet, 
                 method = "C5.0",
                 #tuneLength = 3,
                 trControl = ctrl1 )

C5Model
# Accuracy   Kappa
# 0.9997873  0.9996533
C5_PredictResult <- predict(C5Model, newdata =  validation_dataSet) 
confusionMatrix(C5_PredictResult,validation_dataSet$BUILDINGID)
# Accuracy   Kappa
#1           1