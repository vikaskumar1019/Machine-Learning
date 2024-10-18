# Loading packages
library(tidyverse)
library(caret)
library(randomForest)
library(Hmisc)

######### DATA
# Use data frames created by 2_prep_data_impute.Rmd script

########################## RANDOM FOREST MODELS

# FOLD 1
############## 1.1
fold1.1_impute <- train(age_year ~., 
                        data = train1.1[, !(colnames(train1.1) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold1.1_impute_prediction <- predict(fold1.1_impute, newdata=test1.1, type="raw")
# save it in a data frame with age and the predictors
fold1.1_impute_prediction_combined <- test1.1 %>% 
  mutate(bio_age = fold1.1_impute_prediction)

############## 1.2
fold1.2_impute <- train(age_year ~., 
                        data = train1.2[, !(colnames(train1.2) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold1.2_impute_prediction <- predict(fold1.2_impute, newdata=test1.2, type="raw")
# save it in a data frame with age and the predictors
fold1.2_impute_prediction_combined <- test1.2 %>% 
  mutate(bio_age = fold1.2_impute_prediction)

############## 1.3
fold1.3_impute <- train(age_year ~., 
                        data = train1.3[, !(colnames(train1.3) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold1.3_impute_prediction <- predict(fold1.3_impute, newdata=test1.3, type="raw")
# save it in a data frame with age and the predictors
fold1.3_impute_prediction_combined <- test1.3 %>% 
  mutate(bio_age = fold1.3_impute_prediction)

############## 1.4
fold1.4_impute <- train(age_year ~., 
                        data = train1.4[, !(colnames(train1.4) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold1.4_impute_prediction <- predict(fold1.4_impute, newdata=test1.4, type="raw")
# save it in a data frame with age and the predictors
fold1.4_impute_prediction_combined <- test1.4 %>% 
  mutate(bio_age = fold1.4_impute_prediction)

############## 1.5
fold1.5_impute <- train(age_year ~., 
                        data = train1.5[, !(colnames(train1.5) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold1.5_impute_prediction <- predict(fold1.5_impute, newdata=test1.5, type="raw")
# save it in a data frame with age and the predictors
fold1.5_impute_prediction_combined <- test1.5 %>% 
  mutate(bio_age = fold1.5_impute_prediction)




# FOLD 2
############## 2.1
fold2.1_impute <- train(age_year ~., 
                        data = train2.1[, !(colnames(train2.1) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold2.1_impute_prediction <- predict(fold2.1_impute, newdata=test2.1, type="raw")
# save it in a data frame with age and the predictors
fold2.1_impute_prediction_combined <- test2.1 %>% 
  mutate(bio_age = fold2.1_impute_prediction)

############## 2.2
fold2.2_impute <- train(age_year ~., 
                        data = train2.2[, !(colnames(train2.2) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold2.2_impute_prediction <- predict(fold2.2_impute, newdata=test2.2, type="raw")
# save it in a data frame with age and the predictors
fold2.2_impute_prediction_combined <- test2.2 %>% 
  mutate(bio_age = fold2.2_impute_prediction)

############## 2.3
fold2.3_impute <- train(age_year ~., 
                        data = train2.3[, !(colnames(train2.3) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold2.3_impute_prediction <- predict(fold2.3_impute, newdata=test2.3, type="raw")
# save it in a data frame with age and the predictors
fold2.3_impute_prediction_combined <- test2.3 %>% 
  mutate(bio_age = fold2.3_impute_prediction)

############## 2.4
fold2.4_impute <- train(age_year ~., 
                        data = train2.4[, !(colnames(train2.4) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold2.4_impute_prediction <- predict(fold2.4_impute, newdata=test2.4, type="raw")
# save it in a data frame with age and the predictors
fold2.4_impute_prediction_combined <- test2.4 %>% 
  mutate(bio_age = fold2.4_impute_prediction)

############## 2.5
fold2.5_impute <- train(age_year ~., 
                        data = train2.5[, !(colnames(train2.5) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold2.5_impute_prediction <- predict(fold2.5_impute, newdata=test2.5, type="raw")
# save it in a data frame with age and the predictors
fold2.5_impute_prediction_combined <- test2.5 %>% 
  mutate(bio_age = fold2.5_impute_prediction)




# FOLD 3
############## 3.1
fold3.1_impute <- train(age_year ~., 
                        data = train3.1[, !(colnames(train3.1) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold3.1_impute_prediction <- predict(fold3.1_impute, newdata=test3.1, type="raw")
# save it in a data frame with age and the predictors
fold3.1_impute_prediction_combined <- test3.1 %>% 
  mutate(bio_age = fold3.1_impute_prediction)

############## 3.2
fold3.2_impute <- train(age_year ~., 
                        data = train3.2[, !(colnames(train3.2) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold3.2_impute_prediction <- predict(fold3.2_impute, newdata=test3.2, type="raw")
# save it in a data frame with age and the predictors
fold3.2_impute_prediction_combined <- test3.2 %>% 
  mutate(bio_age = fold3.2_impute_prediction)

############## 3.3
fold3.3_impute <- train(age_year ~., 
                        data = train3.3[, !(colnames(train3.3) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold3.3_impute_prediction <- predict(fold3.3_impute, newdata=test3.3, type="raw")
# save it in a data frame with age and the predictors
fold3.3_impute_prediction_combined <- test3.3 %>% 
  mutate(bio_age = fold3.3_impute_prediction)

############## 3.4
fold3.4_impute <- train(age_year ~., 
                        data = train3.4[, !(colnames(train3.4) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold3.4_impute_prediction <- predict(fold3.4_impute, newdata=test3.4, type="raw")
# save it in a data frame with age and the predictors
fold3.4_impute_prediction_combined <- test3.4 %>% 
  mutate(bio_age = fold3.4_impute_prediction)

############## 3.5
fold3.5_impute <- train(age_year ~., 
                        data = train3.5[, !(colnames(train3.5) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold3.5_impute_prediction <- predict(fold3.5_impute, newdata=test3.5, type="raw")
# save it in a data frame with age and the predictors
fold3.5_impute_prediction_combined <- test3.5 %>% 
  mutate(bio_age = fold3.5_impute_prediction)




# FOLD 4
############## 4.1
fold4.1_impute <- train(age_year ~., 
                        data = train4.1[, !(colnames(train4.1) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold4.1_impute_prediction <- predict(fold4.1_impute, newdata=test4.1, type="raw")
# save it in a data frame with age and the predictors
fold4.1_impute_prediction_combined <- test4.1 %>% 
  mutate(bio_age = fold4.1_impute_prediction)

############## 4.2
fold4.2_impute <- train(age_year ~., 
                        data = train4.2[, !(colnames(train4.2) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold4.2_impute_prediction <- predict(fold4.2_impute, newdata=test4.2, type="raw")
# save it in a data frame with age and the predictors
fold4.2_impute_prediction_combined <- test4.2 %>% 
  mutate(bio_age = fold4.2_impute_prediction)

############## 4.3
fold4.3_impute <- train(age_year ~., 
                        data = train4.3[, !(colnames(train4.3) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold4.3_impute_prediction <- predict(fold4.3_impute, newdata=test4.3, type="raw")
# save it in a data frame with age and the predictors
fold4.3_impute_prediction_combined <- test4.3 %>% 
  mutate(bio_age = fold4.3_impute_prediction)

############## 4.4
fold4.4_impute <- train(age_year ~., 
                        data = train4.4[, !(colnames(train4.4) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold4.4_impute_prediction <- predict(fold4.4_impute, newdata=test4.4, type="raw")
# save it in a data frame with age and the predictors
fold4.4_impute_prediction_combined <- test4.4 %>% 
  mutate(bio_age = fold4.4_impute_prediction)

############## 4.5
fold4.5_impute <- train(age_year ~., 
                        data = train4.5[, !(colnames(train4.5) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold4.5_impute_prediction <- predict(fold4.5_impute, newdata=test4.5, type="raw")
# save it in a data frame with age and the predictors
fold4.5_impute_prediction_combined <- test4.5 %>% 
  mutate(bio_age = fold4.5_impute_prediction)




# FOLD 5
############## 5.1
fold5.1_impute <- train(age_year ~., 
                        data = train5.1[, !(colnames(train5.1) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold5.1_impute_prediction <- predict(fold5.1_impute, newdata=test5.1, type="raw")
# save it in a data frame with age and the predictors
fold5.1_impute_prediction_combined <- test5.1 %>% 
  mutate(bio_age = fold5.1_impute_prediction)

############## 5.2
fold5.2_impute <- train(age_year ~., 
                        data = train5.2[, !(colnames(train5.2) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold5.2_impute_prediction <- predict(fold5.2_impute, newdata=test5.2, type="raw")
# save it in a data frame with age and the predictors
fold5.2_impute_prediction_combined <- test5.2 %>% 
  mutate(bio_age = fold5.2_impute_prediction)

############## 5.3
fold5.3_impute <- train(age_year ~., 
                        data = train5.3[, !(colnames(train5.3) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold5.3_impute_prediction <- predict(fold5.3_impute, newdata=test5.3, type="raw")
# save it in a data frame with age and the predictors
fold5.3_impute_prediction_combined <- test5.3 %>% 
  mutate(bio_age = fold5.3_impute_prediction)

############## 5.4
fold5.4_impute <- train(age_year ~., 
                        data = train5.4[, !(colnames(train5.4) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold5.4_impute_prediction <- predict(fold5.4_impute, newdata=test5.4, type="raw")
# save it in a data frame with age and the predictors
fold5.4_impute_prediction_combined <- test5.4 %>% 
  mutate(bio_age = fold5.4_impute_prediction)

############## 5.5
fold5.5_impute <- train(age_year ~., 
                        data = train5.5[, !(colnames(train5.5) %in% c("anon_sname"))], 
                        method = "rf",
                        ntree = 2000,
                        trControl = trainControl("cv", number = 5), # cross validation within each of the folds to optimize the predictions for the training data set (see this file from Mauna: Biological Aging/ NestedCVDiagram.png)
                        tuneLength = 10, # will test 10 mtry parameters
                        metric="Rsquared", # optimal parameters are those that maximize Rsquared
                        maximize=TRUE,
                        importance = TRUE)
# Predict age from model
fold5.5_impute_prediction <- predict(fold5.5_impute, newdata=test5.5, type="raw")
# save it in a data frame with age and the predictors
fold5.5_impute_prediction_combined <- test5.5 %>% 
  mutate(bio_age = fold5.5_impute_prediction)
