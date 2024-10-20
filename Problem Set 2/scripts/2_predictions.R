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

name<- paste0("stores/submissions/Logit.csv") 
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
  mtry = c(4, 6, 8),            
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

name <- paste0("stores/submissions/06_n200_RF_mtry_8.csv") 
write.csv(predictSample, name, row.names = FALSE)



# 7) Model: XGBoost ============================================================

# Hyperparameter grid for XGBoost
tune_grid_xgb <- expand.grid(
  nrounds = 200,         
  max_depth = c(4, 6),            
  eta = c(0.05, 0.1),            
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

name <- paste0("stores/submissions/07_XGB_n200_eta_0.1_gamma_0.csv") 
write.csv(predictSample, name, row.names = FALSE)


