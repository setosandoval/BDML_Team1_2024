# ========================================================================== #
# Problem Set 1                                                              #
# Big Data and Machine Learning - 202402                                     #
#                                                                            #
# Script: Predictive models                                                  #
#                                                                            #
# Team 1: - Sergio Sandoval                                                  #
#         - María Fernanda Blanco                                            #
#         - Juan Fernando Barberi                                            #
#         - Juan Gutierrez                                                   #
# ========================================================================== #

# ENVIRONMENT SETUP AND DATA UPLOADING =========================================

# Set working directory
setwd("/Users/sergiosandovalcamargo/Documents/BDML/Repositorio P. Sets/BDML_Team1_2024/Problem Set 2")

# Clean the environment
rm(list = ls())

# Seed
set.seed(202028276)

# Load necessary libraries using pacman
library(pacman)
p_load(
  rio,        # Data import/export
  tidyverse,  # Data manipulation/visualization
  skimr,      # Data summary
  stargazer,  # LaTeX table generation
  boot,       # Bootstrap analysis
  dplyr,      # Data wrangling
  caret,      # Machine learning models
  ggplot2,    # Plotting
  gridExtra,  # Arrange multiple plots
  ranger,     # Random forests
  xgboost     # Gradient boosting
)


# Data
train <- readRDS("stores/data/work/train_clean.rds")
test <- readRDS("stores/data/work/test_clean.rds")


# SUB-TRAIN AND SUB-TEST =======================================================

# Create a 70/30 split between training and testing sets
inTrain <- createDataPartition(
  y = train$poor,  
  p = 0.7,          # 70% of the data for training
  list = FALSE      
)

# Split the data into sub_train (70%) and sub_test (30%)
sub_train <- train[inTrain, ]
sub_test  <- train[-inTrain, ]

# Check the proportion of 'poor' in each subset
prop.table(table(train$poor))
prop.table(table(sub_train$poor))
prop.table(table(sub_test$poor))



# F1 Function ==================================================================

# Function to calculate F1-score 
calculate_f1 <- function(true_labels, predicted_labels) {

  confusion <- confusionMatrix(as.factor(predicted_labels), as.factor(true_labels))$table
  
  TP <- confusion[2, 2]  
  FP <- confusion[1, 2]  
  FN <- confusion[2, 1]  
  
  # Calculate precision and recall
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  
  # Calculate F1-score
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  return(f1)
}


# Function to calculate F1-score manually for a given threshold
calculate_f1_manual <- function(threshold, true_labels, predicted_probs) {
  preds <- ifelse(predicted_probs >= threshold, "Yes", "No")
  
  confusion <- confusionMatrix(as.factor(preds), as.factor(true_labels))$table

  TP <- confusion[2,2]
  FP <- confusion[1,2]
  FN <- confusion[2,1]
  
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  
  f1 <- 2 * (precision * recall) / (precision + recall)
  return(f1)
}




# ================================== MODELS ================================== #



# 1) Model: Logit ==============================================================

ctrl <- trainControl(method = "cv",
                     number = 5,                  # 5-fold cross-validation
                     classProbs = TRUE,           # Enable class probabilities
                     summaryFunction = prSummary, # Precision, recall, and F1-score
                     savePredictions = TRUE,      # Save predicted values
                     verboseIter = TRUE)          # Show iteration progress

# Logit model on sub-train data
model1_logit_sub <- train(poor ~ ., 
                     data = sub_train, 
                     method = "glm", 
                     family = "binomial", 
                     metric = "F",               # F1-score as metric
                     trControl = ctrl)

model1_logit_sub

# Predict probabilities on sub_test
test_preds <- predict(model1_logit_sub, newdata = sub_test, type = "raw")

# F1 score out of sample
f1_score <- calculate_f1(true_labels = sub_test$poor, predicted_labels = test_preds)
print(paste("F1-score Threshold:", f1_score))  # 0.60

# Logit model on full train data
model1_logit <- train(poor ~ ., 
                      data = train, 
                      method = "glm", 
                      family = "binomial", 
                      metric = "F",               # F1-score as metric
                      trControl = ctrl)

model1_logit

# Submission
predictSample <- test   %>% 
  mutate(pobre = predict(model1_logit, newdata = test, type = "raw")) %>% 
  select(id,pobre)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre=="Yes",1,0)) %>% 
  select(id,pobre)

head(predictSample)

name<- paste0("stores/submissions/01_Logit.csv") 
write.csv(predictSample,name, row.names = FALSE)



# 2) Model: LDA ================================================================

# LDA model on sub-train data
model2_lda_sub <- train(poor ~ ., 
                        data = sub_train, 
                        method = "lda",          
                        metric = "F",             
                        trControl = ctrl)

model2_lda_sub

# Predict class labels on sub_test
test_preds <- predict(model2_lda_sub, newdata = sub_test, type = "raw")

# F1 score out of sample
f1_score <- calculate_f1(true_labels = sub_test$poor, predicted_labels = test_preds)
print(paste("F1-score LDA:", f1_score))  # 0.56

# LDA model on full train data
model2_lda <- train(poor ~ ., 
                    data = train, 
                    method = "lda",          
                    metric = "F",             
                    trControl = ctrl)

model2_lda

# Submission
predictSample <- test   %>% 
  mutate(pobre = predict(model2_lda, newdata = test, type = "raw")) %>% 
  select(id,pobre)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre=="Yes",1,0)) %>% 
  select(id,pobre)

head(predictSample)

name<- paste0("stores/submissions/02_LDA.csv") 
write.csv(predictSample,name, row.names = FALSE)



# 3) Model: QDA ================================================================

# QDA model on sub-train data
model3_qda_sub <- train(poor ~ ., 
                        data = sub_train, 
                        method = "qda",           
                        metric = "F",             
                        trControl = ctrl)

model3_qda_sub

# Predict class labels on sub_test
test_preds <- predict(model3_qda_sub, newdata = sub_test, type = "raw")

# F1 score out of sample
f1_score <- calculate_f1(true_labels = sub_test$poor, predicted_labels = test_preds)
print(paste("F1-score QDA:", f1_score))  #0.54

# QDA model on full train data
model3_qda <- train(poor ~ ., 
                    data = train, 
                    method = "qda",          
                    metric = "F",            
                    trControl = ctrl)

model3_qda

# Submission
predictSample <- test %>% 
  mutate(pobre = predict(model3_qda, newdata = test, type = "raw")) %>% 
  select(id, pobre)

predictSample <- predictSample %>% 
  mutate(pobre = ifelse(pobre == "Yes", 1, 0)) %>% 
  select(id, pobre)

head(predictSample)

name <- paste0("stores/submissions/03_QDA.csv") 
write.csv(predictSample, name, row.names = FALSE)


# 4) Model: Elastic Net ========================================================

# Grid
tune_grid_enet <- expand.grid(
  alpha = seq(0.1, 1, length = 5),    
  lambda = 10^seq(-2, 1, length = 5)) 

# Train Elastic Net on sub-train data
model4_en_sub <- train(poor ~ ., 
                         data = sub_train, 
                         method = "glmnet",        # Elastic Net method
                         metric = "F",             # F1-score as metric
                         trControl = ctrl,         # Control setup
                         tuneGrid = tune_grid_enet) # Hyperparameter grid

model4_en_sub

# Predict class labels on sub_test
test_preds <- predict(model4_en_sub, newdata = sub_test, type = "raw")

# F1 score out of sample
f1_score <- calculate_f1(true_labels = sub_test$poor, predicted_labels = test_preds)
print(paste("F1-score Elastic Net:", f1_score))  # 0.55

# Elastic Net model on full train data
model4_en <- train(poor ~ ., 
                   data = train, 
                   method = "glmnet", 
                   metric = "F", 
                   trControl = ctrl, 
                   tuneGrid = tune_grid_enet)

model4_en

# Submission
predictSample <- test %>% 
  mutate(pobre = predict(model4_en, newdata = test, type = "raw")) %>% 
  select(id, pobre)

predictSample <- predictSample %>% 
  mutate(pobre = ifelse(pobre == "Yes", 1, 0)) %>% 
  select(id, pobre)

head(predictSample)

name <- paste0("stores/submissions/04_EN_alpha_0.1_lambda_0.001.csv") 
write.csv(predictSample, name, row.names = FALSE)



# 5) Model: CART ===============================================================

# Hyperparameter grid for CART
tune_grid_cart <- expand.grid(
  cp = seq(0.001, 0.1, length = 10))  

# Train CART model on sub-train data
model5_cart_sub <- train(poor ~ ., 
                         data = sub_train, 
                         method = "rpart",          
                         metric = "F",             
                         trControl = ctrl,          
                         tuneGrid = tune_grid_cart) 

model5_cart_sub

# Predict class labels on sub_test
test_preds <- predict(model5_cart_sub, newdata = sub_test, type = "raw")

# F1 score out of sample
f1_score <- calculate_f1(true_labels = sub_test$poor, predicted_labels = test_preds)
print(paste("F1-score CART:", f1_score))  # Prints the F1-score

# Train CART model on full train data
model5_cart <- train(poor ~ ., 
                     data = train, 
                     method = "rpart", 
                     metric = "F", 
                     trControl = ctrl, 
                     tuneGrid = tune_grid_cart)

model5_cart

# Submission
predictSample <- test %>% 
  mutate(pobre = predict(model5_cart, newdata = test, type = "raw")) %>% 
  select(id, pobre)

predictSample <- predictSample %>% 
  mutate(pobre = ifelse(pobre == "Yes", 1, 0)) %>% 
  select(id, pobre)

head(predictSample)

name <- paste0("stores/submissions/05_CART_cp_0.001.csv") 
write.csv(predictSample, name, row.names = FALSE)



# 6) Model: Random Forest ======================================================

# Hyperparameter grid for Random Forest
tune_grid_rf <- expand.grid(
  mtry = c(5, 6, 8),            
  splitrule = "gini",           
  min.node.size = c(10, 20))  

# Train Random Forest model on sub-train data
model6_rf_sub <- train(poor ~ ., 
                       data = sub_train, 
                       method = "ranger",           
                       metric = "F",               
                       trControl = ctrl,          
                       tuneGrid = tune_grid_rf,    
                       num.trees = 200)             

model6_rf_sub

# Predict class labels on sub_test
test_preds <- predict(model6_rf_sub, newdata = sub_test, type = "raw")

# F1 score out of sample
f1_score <- calculate_f1(true_labels = sub_test$poor, predicted_labels = test_preds)
print(paste("F1-score Random Forest:", f1_score))  # 0.60

# Train Random Forest model on full train data
model6_rf <- train(poor ~ ., 
                   data = train, 
                   method = "ranger", 
                   metric = "F", 
                   trControl = ctrl, 
                   tuneGrid = tune_grid_rf, 
                   num.trees = 200)

model6_rf

# Submission
predictSample <- test %>% 
  mutate(pobre = predict(model6_rf, newdata = test, type = "raw")) %>% 
  select(id, pobre)

predictSample <- predictSample %>% 
  mutate(pobre = ifelse(pobre == "Yes", 1, 0)) %>% 
  select(id, pobre)

head(predictSample)

name <- paste0("stores/submissions/06_n200_RF_mtry_5.csv") 
write.csv(predictSample, name, row.names = FALSE)



# 7) Model: XGBoost ============================================================

# Hyperparameter grid for XGBoost
tune_grid_xgb <- expand.grid(
  nrounds = 200,         
  max_depth = c(4, 6),            
  eta = c(0.05, 0.2),            
  gamma = c(0,1),                     
  colsample_bytree = 0.8,         
  min_child_weight = 10,         
  subsample = 0.8               
)

# Train XGBoost model on sub-train data
model7_xgb_sub <- train(poor ~ ., 
                        data = sub_train, 
                        method = "xgbTree",        
                        metric = "F",               
                        trControl = ctrl,       
                        tuneGrid = tune_grid_xgb,  
                        nthread = 4)              

model7_xgb_sub

# Predict class labels on sub_test
test_preds <- predict(model7_xgb_sub, newdata = sub_test, type = "raw")

# F1 score out of sample
f1_score <- calculate_f1(true_labels = sub_test$poor, predicted_labels = test_preds)
print(paste("F1-score XGBoost:", f1_score))  # 0.64

# Train XGBoost model on full train data
model7_xgb <- train(poor ~ ., 
                    data = train, 
                    method = "xgbTree", 
                    metric = "F", 
                    trControl = ctrl, 
                    tuneGrid = tune_grid_xgb, 
                    nthread = 4)

model7_xgb

# Submission
predictSample <- test %>% 
  mutate(pobre = predict(model7_xgb, newdata = test, type = "raw")) %>% 
  select(id, pobre)

predictSample <- predictSample %>% 
  mutate(pobre = ifelse(pobre == "Yes", 1, 0)) %>% 
  select(id, pobre)

head(predictSample)

name <- paste0("stores/submissions/07_XGB_n200_eta_0.2_gamma_0.csv") 
write.csv(predictSample, name, row.names = FALSE)



# =============================== SMOTE MODELS =============================== #



# 8) Model: Logit SMOTE ========================================================

ctrl <- trainControl(method = "cv",
                     number = 5,                  
                     classProbs = TRUE,           
                     summaryFunction = prSummary,
                     savePredictions = TRUE,      
                     verboseIter = TRUE,
                     sampling = "smote")          # SMOTE sample

# Logit model on sub-train data
model8_logit_sub <- train(poor ~ ., 
                          data = sub_train, 
                          method = "glm", 
                          family = "binomial", 
                          metric = "F",               
                          trControl = ctrl)

model8_logit_sub

# Predict probabilities on sub_test
test_preds <- predict(model8_logit_sub, newdata = sub_test, type = "raw")

# F1 score out of sample
f1_score <- calculate_f1(true_labels = sub_test$poor, predicted_labels = test_preds)
print(paste("F1-score Threshold:", f1_score))  # 0.63

# Logit model on full train data
model8_logit <- train(poor ~ ., 
                      data = train, 
                      method = "glm", 
                      family = "binomial", 
                      metric = "F",              
                      trControl = ctrl)

model8_logit

# Submission
predictSample <- test   %>% 
  mutate(pobre = predict(model8_logit, newdata = test, type = "raw")) %>% 
  select(id,pobre)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre=="Yes",1,0)) %>% 
  select(id,pobre)

head(predictSample)

name<- paste0("stores/submissions/08_Logit_SMOTE.csv") 
write.csv(predictSample,name, row.names = FALSE)



# 9) Model: LDA SMOTE ==========================================================

# LDA model on sub-train data
model9_lda_sub <- train(poor ~ ., 
                        data = sub_train, 
                        method = "lda",          
                        metric = "F",             
                        trControl = ctrl)

model9_lda_sub

# Predict class labels on sub_test
test_preds <- predict(model9_lda_sub, newdata = sub_test, type = "raw")

# F1 score out of sample
f1_score <- calculate_f1(true_labels = sub_test$poor, predicted_labels = test_preds)
print(paste("F1-score LDA:", f1_score))  # 0.60

# LDA model on full train data
model9_lda <- train(poor ~ ., 
                    data = train, 
                    method = "lda",          
                    metric = "F",             
                    trControl = ctrl)

model9_lda

# Submission
predictSample <- test   %>% 
  mutate(pobre = predict(model9_lda, newdata = test, type = "raw")) %>% 
  select(id,pobre)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre=="Yes",1,0)) %>% 
  select(id,pobre)

head(predictSample)

name<- paste0("stores/submissions/09_LDA_SMOTE.csv") 
write.csv(predictSample,name, row.names = FALSE)



# 10) Model: QDA SMOTE =========================================================

# QDA model on sub-train data
model10_qda_sub <- train(poor ~ ., 
                        data = sub_train, 
                        method = "qda",           
                        metric = "F",             
                        trControl = ctrl)

model10_qda_sub

# Predict class labels on sub_test
test_preds <- predict(model10_qda_sub, newdata = sub_test, type = "raw")

# F1 score out of sample
f1_score <- calculate_f1(true_labels = sub_test$poor, predicted_labels = test_preds)
print(paste("F1-score QDA:", f1_score))  #0.53

# QDA model on full train data
model10_qda <- train(poor ~ ., 
                    data = train, 
                    method = "qda",          
                    metric = "F",            
                    trControl = ctrl)

model10_qda

# Submission
predictSample <- test %>% 
  mutate(pobre = predict(model10_qda, newdata = test, type = "raw")) %>% 
  select(id, pobre)

predictSample <- predictSample %>% 
  mutate(pobre = ifelse(pobre == "Yes", 1, 0)) %>% 
  select(id, pobre)

head(predictSample)

name <- paste0("stores/submissions/10_QDA_SMOTE.csv") 
write.csv(predictSample, name, row.names = FALSE)


# 11) Model: Elastic Net SMOTE =================================================

# Grid
tune_grid_enet <- expand.grid(
  alpha = seq(0.1, 1, length = 5),    
  lambda = 10^seq(-2, 1, length = 5)) 

# Train Elastic Net on sub-train data
model11_en_sub <- train(poor ~ ., 
                       data = sub_train, 
                       method = "glmnet",       
                       metric = "F",             
                       trControl = ctrl,         
                       tuneGrid = tune_grid_enet) 

model11_en_sub

# Predict class labels on sub_test
test_preds <- predict(model11_en_sub, newdata = sub_test, type = "raw")

# F1 score out of sample
f1_score <- calculate_f1(true_labels = sub_test$poor, predicted_labels = test_preds)
print(paste("F1-score Elastic Net:", f1_score))  # 0.58

# Elastic Net model on full train data
model11_en <- train(poor ~ ., 
                   data = train, 
                   method = "glmnet", 
                   metric = "F", 
                   trControl = ctrl, 
                   tuneGrid = tune_grid_enet)

model11_en

# Submission
predictSample <- test %>% 
  mutate(pobre = predict(model11_en, newdata = test, type = "raw")) %>% 
  select(id, pobre)

predictSample <- predictSample %>% 
  mutate(pobre = ifelse(pobre == "Yes", 1, 0)) %>% 
  select(id, pobre)

head(predictSample)

name <- paste0("stores/submissions/11_EN_alpha_0.1_lambda_10_SMOTE.csv") 
write.csv(predictSample, name, row.names = FALSE)



# 12) Model: CART SMOTE ========================================================

# Hyperparameter grid for CART
tune_grid_cart <- expand.grid(
  cp = seq(0.001, 0.1, length = 10))  

# Train CART model on sub-train data
model12_cart_sub <- train(poor ~ ., 
                         data = sub_train, 
                         method = "rpart",          
                         metric = "F",             
                         trControl = ctrl,          
                         tuneGrid = tune_grid_cart) 

model12_cart_sub

# Predict class labels on sub_test
test_preds <- predict(model12_cart_sub, newdata = sub_test, type = "raw")

# F1 score out of sample
f1_score <- calculate_f1(true_labels = sub_test$poor, predicted_labels = test_preds)
print(paste("F1-score CART:", f1_score))  # 0.59

# Train CART model on full train data
model12_cart <- train(poor ~ ., 
                     data = train, 
                     method = "rpart", 
                     metric = "F", 
                     trControl = ctrl, 
                     tuneGrid = tune_grid_cart)

model12_cart

# Submission
predictSample <- test %>% 
  mutate(pobre = predict(model12_cart, newdata = test, type = "raw")) %>% 
  select(id, pobre)

predictSample <- predictSample %>% 
  mutate(pobre = ifelse(pobre == "Yes", 1, 0)) %>% 
  select(id, pobre)

head(predictSample)

name <- paste0("stores/submissions/12_CART_cp_0.001_SMOTE.csv") 
write.csv(predictSample, name, row.names = FALSE)



# 13) Model: Random Forest SMOTE ===============================================

# Hyperparameter grid for Random Forest
tune_grid_rf <- expand.grid(
  mtry = c(5, 6, 8),            
  splitrule = "gini",           
  min.node.size = c(10, 20))  

# Train Random Forest model on sub-train data
model13_rf_sub <- train(poor ~ ., 
                       data = sub_train, 
                       method = "ranger",           
                       metric = "F",               
                       trControl = ctrl,          
                       tuneGrid = tune_grid_rf,    
                       num.trees = 200)             

model13_rf_sub

# Predict class labels on sub_test
test_preds <- predict(model13_rf_sub, newdata = sub_test, type = "raw")

# F1 score out of sample
f1_score <- calculate_f1(true_labels = sub_test$poor, predicted_labels = test_preds)
print(paste("F1-score Random Forest:", f1_score))  # 0.62

# Train Random Forest model on full train data
model13_rf <- train(poor ~ ., 
                   data = train, 
                   method = "ranger", 
                   metric = "F", 
                   trControl = ctrl, 
                   tuneGrid = tune_grid_rf, 
                   num.trees = 200)

model13_rf

# Submission
predictSample <- test %>% 
  mutate(pobre = predict(model13_rf, newdata = test, type = "raw")) %>% 
  select(id, pobre)

predictSample <- predictSample %>% 
  mutate(pobre = ifelse(pobre == "Yes", 1, 0)) %>% 
  select(id, pobre)

head(predictSample)

name <- paste0("stores/submissions/13_n200_RF_mtry_5_SMOTE.csv") 
write.csv(predictSample, name, row.names = FALSE)



# 14) Model: XGBoost SMOTE =====================================================

# Hyperparameter grid for XGBoost
tune_grid_xgb <- expand.grid(
  nrounds = 200,         
  max_depth = c(4, 6),            
  eta = c(0.05, 0.2),            
  gamma = c(0,1),                     
  colsample_bytree = 0.8,         
  min_child_weight = 10,         
  subsample = 0.8               
)

# Train XGBoost model on sub-train data
model14_xgb_sub <- train(poor ~ ., 
                        data = sub_train, 
                        method = "xgbTree",        
                        metric = "F",               
                        trControl = ctrl,       
                        tuneGrid = tune_grid_xgb,  
                        nthread = 4)              

model14_xgb_sub

# Predict class labels on sub_test
test_preds <- predict(model14_xgb_sub, newdata = sub_test, type = "raw")

# F1 score out of sample
f1_score <- calculate_f1(true_labels = sub_test$poor, predicted_labels = test_preds)
print(paste("F1-score XGBoost:", f1_score))  # 0.64

# Train XGBoost model on full train data
model14_xgb <- train(poor ~ ., 
                    data = train, 
                    method = "xgbTree", 
                    metric = "F", 
                    trControl = ctrl, 
                    tuneGrid = tune_grid_xgb, 
                    nthread = 4)

model14_xgb

# Submission
predictSample <- test %>% 
  mutate(pobre = predict(model14_xgb, newdata = test, type = "raw")) %>% 
  select(id, pobre)

predictSample <- predictSample %>% 
  mutate(pobre = ifelse(pobre == "Yes", 1, 0)) %>% 
  select(id, pobre)

head(predictSample)

name <- paste0("stores/submissions/14_XGB_n200_eta_0.2_gamma_0_SMOTE.csv") 
write.csv(predictSample, name, row.names = FALSE)



# =============================== FINAL MODELS =============================== #



# 15) Model: Logit Kaggle =======================================================

ctrl <- trainControl(method = "cv",
                     number = 5,                  
                     classProbs = TRUE,           
                     summaryFunction = prSummary,
                     savePredictions = TRUE,      
                     verboseIter = TRUE,
                     sampling = "smote")          # SMOTE sample

# Logit model on sub-train data
model15_logit_sub <- train(poor ~ ., 
                          data = sub_train, 
                          method = "glm", 
                          family = "binomial", 
                          metric = "F",               
                          trControl = ctrl)

model15_logit_sub

# Predict probabilities on sub_test
test_probs <- predict(model15_logit_sub, newdata = sub_test, type = "prob")[, "Yes"]

# Search for the optimal threshold to maximize F1-score
thresholds <- seq(0.05, 0.95, by = 0.01)  
f1_scores <- sapply(thresholds, calculate_f1_manual, true_labels = sub_test$poor, predicted_probs = test_probs)

# Find the threshold with the highest F1-score
optimal_threshold <- thresholds[which.max(f1_scores)]
optimal_f1 <- max(f1_scores)

# Print the optimal threshold and F1-score
print(paste("Optimal Threshold:", optimal_threshold))       # 0.61
print(paste("F1-score at Optimal Threshold:", optimal_f1))  # 0.64

# Logit model on full train data
model15_logit <- train(poor ~ ., 
                      data = train, 
                      method = "glm", 
                      family = "binomial", 
                      metric = "F",             
                      trControl = ctrl)

model15_logit

# Submission
test_probs <- predict(model15_logit_sub, newdata = test, type = "prob")[, "Yes"]

# Use optimal threshold founded before
final_test_preds <- ifelse(test_probs >= optimal_threshold, "Yes", "No")

# SUBMISSION
predictSample <- test %>% 
  mutate(pobre = ifelse(final_test_preds == "Yes", 1, 0)) %>% 
  select(id,pobre)

head(predictSample)

name<- paste0("stores/submissions/15_LOGIT_SMOTE.csv") 
write.csv(predictSample, name, row.names = FALSE)



# 16) Model: Random Forest Kaggle ==============================================

# Hyperparameter grid for Random Forest
tune_grid_rf <- expand.grid(
  mtry = 5,            
  splitrule = "gini",           
  min.node.size = 20)  

# Train Random Forest model on sub-train data
model16_rf_sub <- train(poor ~ ., 
                        data = sub_train, 
                        method = "ranger",           
                        metric = "F",               
                        trControl = ctrl,          
                        tuneGrid = tune_grid_rf,    
                        num.trees = 200,
                        importance = "impurity")             

model16_rf_sub

# Predict probabilities on sub_test
test_probs <- predict(model16_rf_sub, newdata = sub_test, type = "prob")[, "Yes"]

# Search for the optimal threshold to maximize F1-score
thresholds <- seq(0.05, 0.95, by = 0.01)  
f1_scores <- sapply(thresholds, calculate_f1_manual, true_labels = sub_test$poor, predicted_probs = test_probs)

# Find the threshold with the highest F1-score
optimal_threshold <- thresholds[which.max(f1_scores)]
optimal_f1 <- max(f1_scores)

# Print the optimal threshold and F1-score
print(paste("Optimal Threshold:", optimal_threshold))       # 0.39
print(paste("F1-score at Optimal Threshold:", optimal_f1))  # 0.65

# Train Random Forest model on full train data
model16_rf <- train(poor ~ ., 
                    data = train, 
                    method = "ranger", 
                    metric = "F", 
                    trControl = ctrl, 
                    tuneGrid = tune_grid_rf, 
                    num.trees = 200,
                    importance = "impurity")

model16_rf

# Submission
test_probs <- predict(model16_rf, newdata = test, type = "prob")[, "Yes"]

# Use optimal threshold founded before
final_test_preds <- ifelse(test_probs >= optimal_threshold, "Yes", "No")

# SUBMISSION
predictSample <- test %>% 
  mutate(pobre = ifelse(final_test_preds == "Yes", 1, 0)) %>% 
  select(id,pobre)

head(predictSample)

name <- paste0("stores/submissions/16_n200_RF_mtry_5_SMOTE.csv") 
write.csv(predictSample, name, row.names = FALSE)



# 17) Model: XGBoost Kaggle ====================================================

# Hyperparameter grid for XGBoost
tune_grid_xgb <- expand.grid(
  nrounds = 200,         
  max_depth = 4,            
  eta = 0.2,            
  gamma = 0,                     
  colsample_bytree = 0.8,         
  min_child_weight = 10,         
  subsample = 0.8               
)

# Train XGBoost model on sub-train data
model17_xgb_sub <- train(poor ~ ., 
                         data = sub_train, 
                         method = "xgbTree",        
                         metric = "F",               
                         trControl = ctrl,       
                         tuneGrid = tune_grid_xgb,  
                         nthread = 4)              

model17_xgb_sub

# Predict probabilities on sub_test
test_probs <- predict(model17_xgb_sub, newdata = sub_test, type = "prob")[, "Yes"]

# Search for the optimal threshold to maximize F1-score
thresholds <- seq(0.05, 0.95, by = 0.01)  
f1_scores <- sapply(thresholds, calculate_f1_manual, true_labels = sub_test$poor, predicted_probs = test_probs)

# Find the threshold with the highest F1-score
optimal_threshold <- thresholds[which.max(f1_scores)]
optimal_f1 <- max(f1_scores)

# Print the optimal threshold and F1-score
print(paste("Optimal Threshold:", optimal_threshold))       # 0.34
print(paste("F1-score at Optimal Threshold:", optimal_f1))  # 0.67

# Train XGBoost model on full train data
model17_xgb <- train(poor ~ ., 
                     data = train, 
                     method = "xgbTree", 
                     metric = "F", 
                     trControl = ctrl, 
                     tuneGrid = tune_grid_xgb, 
                     nthread = 4)

model17_xgb

# Submission
predictSample <- test %>% 
  mutate(pobre = predict(model17_xgb, newdata = test, type = "raw")) %>% 
  select(id, pobre)

predictSample <- predictSample %>% 
  mutate(pobre = ifelse(pobre == "Yes", 1, 0)) %>% 
  select(id, pobre)

head(predictSample)

name <- paste0("stores/submissions/17_XGB_n200_eta_0.2_gamma_0_SMOTE.csv") 
write.csv(predictSample, name, row.names = FALSE)



# Variable Importance ==========================================================

# RF
importance_values <- ranger::importance(model16_rf$finalModel)

importance_df <- data.frame(
  Variable = names(importance_values),
  Importance = importance_values)

importance_df_top40 <- importance_df %>%
  arrange(desc(Importance)) %>%
  head(40)

rf_importance_plot <- ggplot(importance_df_top40, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 40 Variable Importance in Random Forest", x = "Variable", y = "Importance")

ggsave("views/RF_var_importance.pdf", plot = rf_importance_plot, width = 10, height = 8)


# XGB
final_xgb <- model17_xgb$finalModel
importance_matrix <- xgb.importance(model = final_xgb)

top_40_importance <- head(importance_matrix, 40)
xgb.plot.importance(top_40_importance)

importance_df <- as.data.frame(top_40_importance)

xgb_importance_plot <- ggplot(importance_df, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 40 Variable Importance in XGBoost", x = "Variable", y = "Importance")

ggsave("views/XGB_var_importance.pdf", plot = xgb_importance_plot, width = 10, height = 8)

