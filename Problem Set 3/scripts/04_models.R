# ============================================================================ #
# Problem Set 1                                                                #
# Big Data and Machine Learning - 202420                                       #
#                                                                              #
# Script: Predictive Models                                                    #
#                                                                              #
# Team 1: - Sergio Sandoval                                                    #
#         - María Fernanda Blanco                                              #
#         - Juan Fernando Barberi                                              #
#         - Juan Gutierrez                                                     #
# ============================================================================ #


# ENVIRONMENT SETUP AND DATA UPLOADING =========================================

# Set working directory
setwd("/Users/sergiosandovalcamargo/Desktop/Problem Set 3")

# Clean the environment
rm(list = ls())

# Seed
set.seed(202028276)

# Libraries
library(pacman)
p_load(tidyverse,
       caret,
       recipes,
       spatialsample,
       tidymodels,
       sf,
       dplyr,
       keras,
       torch,
       bonsai,
       tensorflow)    

# Data
data <-read.csv("stores/data/work/data_med.csv")
data <- data %>%
  select(-id_manz)

# Categorical variables
sapply(data, class)


# Function to convert variables to factors
convert_to_factors <- function(data, variables) {
  for (var in variables) {
    if (var %in% colnames(data)) {
      data[[var]] <- as.factor(data[[var]])
    } else {
      warning(paste("Variable", var, "not found in the DataFrame"))
    }
  }
  return(data)
}

# Apply function
categorical_vars <- c("id_UPZ", "id_local", "estrato", "property_type", "year_month")
data <- convert_to_factors(data, categorical_vars)
sapply(data, class)

# Split variables into categorical and numeric
categorical_vars <- c("id_UPZ", "id_local", "estrato", "property_type", "year_month")
data_categoricals <- data %>% select(all_of(categorical_vars))
data_numerics <- data %>% select(-all_of(categorical_vars))

# Dummies for categorical variables
dummy_model <- dummyVars(" ~ .", data = data_categoricals)
data_categoricals_dummies <- predict(dummy_model, newdata = data_categoricals) %>%
  as.data.frame()

# Final data frame with dummies
data_clean <- cbind(data_numerics, data_categoricals_dummies)

# Split into real train and test data
real_train <- data_clean[data_clean$is_test == 0, ] %>%
  select(-is_test,
         -property_id) %>%
  mutate(ln_price = log(price)) 

real_test <- data_clean[data_clean$is_test == 1, ] %>%
  select(-is_test,
         -price)

# Sub train and test (80%)
split <- createDataPartition(real_train$price, p = 0.8, list = FALSE)
train <- real_train[split, ]
test <- real_train[-split, ]

# Split 2
real_train_2 <- data[data$is_test == 0, ] %>%
  select(-is_test,
         -property_id) %>%
  mutate(ln_price = log(price)) 

real_test_2 <- data[data$is_test == 1, ] %>%
  select(-is_test,
         -price)

# Sub train and test (80%)
split <- createDataPartition(real_train_2$price, p = 0.8, list = FALSE)
train_2 <- real_train[split, ]
test_2 <- real_train[-split, ]



# RECIPES SETTINGS =============================================================

# Function to create spatial cross-validation folds
create_spatial_cv <- function(data, v = 5) {
  # Ensure the data is an sf object
  data_sf <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)
  
  # Add lat and lon columns before dropping geometry
  data_sf <- data_sf %>%
    mutate(lon = st_coordinates(.)[, 1],  
           lat = st_coordinates(.)[, 2])
  
  # Create spatial folds
  folds <- spatial_block_cv(data_sf, v = v)
  
  # Drop geometry after extracting coordinates
  data_clean <- st_drop_geometry(data_sf)
  
  return(list(data = data_clean, folds = folds))
}

# Prepare cross-validation folds
cv_sub <- create_spatial_cv(train)
train <- cv_sub$data
block_folds_sub <- cv_sub$folds

cv_real <- create_spatial_cv(real_train)
real_train <- cv_real$data
block_folds <- cv_real$folds

cv_sub_2 <- create_spatial_cv(train_2)
train_2 <- cv_sub$data
block_folds_sub_2 <- cv_sub$folds

cv_real_2 <- create_spatial_cv(real_train_2)
real_train_2 <- cv_real$data
block_folds_2 <- cv_real$folds

# Recipe 1: lnPrice = f(X). With dummies and normalized variables
rec_1_sub <- recipe(ln_price ~ ., data = train[, !names(train) %in% "price"]) %>%
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

rec_1 <- recipe(ln_price ~ ., data = real_train[, !names(real_train) %in% "price"]) %>%
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# Recipe 2: Price = f(X). With dummies and normalized variables
rec_2_sub <- recipe(price ~ ., data = train[, !names(train) %in% "ln_price"]) %>%
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

rec_2 <- recipe(price ~ ., data = real_train[, !names(real_train) %in% "ln_price"]) %>%
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# Recipe 3: lnPrice = f(X). 
rec_3_sub <- recipe(ln_price ~ ., data = train_2[, !names(train_2) %in% "price"]) %>%
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

rec_3 <- recipe(ln_price ~ ., data = real_train_2[, !names(real_train_2) %in% "price"]) %>%
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())


# ================================== MODELS ================================== #

# 1) Linear Regression Model ==================================================

# Linear Regression Model
lin_reg_model <- linear_reg() %>% 
  set_engine("lm")

# Workflow for Linear Regression (sub_train)
workflow_lin_reg_sub <- workflow() %>%
  add_recipe(rec_1_sub) %>%
  add_model(lin_reg_model)

# Cross-validation (CV) for sub_train
cv_results_lin_reg_sub <- fit_resamples(
  workflow_lin_reg_sub,
  resamples = block_folds_sub,
  metrics = metric_set(mae),
  control = control_resamples(save_pred = TRUE) 
)

# Save OOF predictions for superlearner
oof_predictions_lin_reg <- collect_predictions(cv_results_lin_reg_sub) %>%
  mutate(price_pred_oof_lin_reg = exp(.pred)) %>% 
  select(price_pred_oof_lin_reg)

# Print OOF predictions for inspection (optional)
print(head(oof_predictions_lin_reg))

# Select the best model (not critical for lm, but useful for structure)
best_lin_reg_sub <- select_best(cv_results_lin_reg_sub, metric = "mae")

# Finalize workflow for linear regression (sub_train)
final_workflow_lin_reg_sub <- finalize_workflow(workflow_lin_reg_sub, best_lin_reg_sub)

# Train the finalized workflow on the complete sub_train dataset
final_fit_lin_reg_sub <- fit(final_workflow_lin_reg_sub, data = train)

# Predict on the test set (sub_test)
test_predictions_lin_reg_sub <- predict(final_fit_lin_reg_sub, new_data = test) %>%
  bind_cols(test) %>%                
  mutate(price_pred_lin_reg = exp(.pred))  

# Calculate MAE for sub_test
mae_test_lin_reg_sub <- mae(test_predictions_lin_reg_sub, truth = price, estimate = price_pred_lin_reg)
print(mae_test_lin_reg_sub)

# Save sub_test predictions for superlearner
sub_test_predictions_lin_reg <- test_predictions_lin_reg_sub %>%
  select(price_pred_lin_reg)

#### MAE = 148

# Workflow for Linear Regression (real_train)
workflow_lin_reg <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(lin_reg_model)

# Cross-validation (CV) for real_train
cv_results_lin_reg <- fit_resamples(
  workflow_lin_reg,
  resamples = block_folds,
  metrics = metric_set(mae),
  control = control_resamples(save_pred = TRUE) 
)

# Save OOF predictions for real_train (optional for superlearner)
oof_predictions_real_train <- collect_predictions(cv_results_lin_reg) %>%
  mutate(price_pred_oof_real_train = exp(.pred)) %>%  # Convert ln_price to price
  select(price_pred_oof_real_train)

# Print OOF predictions for inspection (optional)
print(head(oof_predictions_real_train))

# Select the best model for real_train
best_lin_reg <- select_best(cv_results_lin_reg, metric = "mae")

# Finalize workflow for real_train
final_workflow_lin_reg <- finalize_workflow(workflow_lin_reg, best_lin_reg)

# Train the finalized workflow on the complete real_train dataset
final_fit_lin_reg <- fit(final_workflow_lin_reg, data = real_train)

# Predict on the real_test set
test_predictions_lin_reg <- predict(final_fit_lin_reg, new_data = real_test) %>%
  bind_cols(real_test) %>%                
  mutate(price_pred_lin_reg = exp(.pred))  

# Save predictions for real_test
real_test_predictions_lin_reg <- test_predictions_lin_reg %>%
  select(property_id, price_pred_lin_reg)

# Submission file for Kaggle
submission <- test_predictions_lin_reg %>%
  mutate(price = round(price_pred_lin_reg, 5)) %>%  
  select(property_id, price)

write.csv(submission, "stores/submissions/1_LN.csv", row.names = FALSE)

#### MAE Kaggle = 203


# 2) Elastic Net Model =======================================================

# Elastic Net Model
elastic_net_model <- linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")

# Workflow for Elastic Net (sub_train)
workflow_enet_sub <- workflow() %>%
  add_recipe(rec_2_sub) %>%
  add_model(elastic_net_model)

# Define hyperparameter grid for Elastic Net
enet_grid <- grid_regular(
  penalty(range = c(0, 1)),  
  mixture(range = c(0, 0.1)),   
  levels = 50                    
)

# Cross-validation tuning for Elastic Net (sub_train)
tune_results_enet_sub <- tune_grid(
  workflow_enet_sub,
  resamples = block_folds_sub,
  grid = enet_grid,
  metrics = metric_set(mae),
  control = control_grid(save_pred = TRUE)
)

# Save OOF predictions for superlearner
oof_predictions_enet <- collect_predictions(tune_results_enet_sub) %>%
  mutate(price_pred_oof_enet = .pred) %>%  # Convert ln_price to price
  select(price_pred_oof_enet)

# Print OOF predictions for inspection (optional)
print(head(oof_predictions_enet))

# Select the best hyperparameters for Elastic Net
best_enet_sub <- select_best(tune_results_enet_sub, metric = "mae")

# Finalize workflow for Elastic Net (sub_train)
final_workflow_enet_sub <- finalize_workflow(workflow_enet_sub, best_enet_sub)

# Train the finalized workflow on the complete sub_train dataset
final_fit_enet_sub <- fit(final_workflow_enet_sub, data = train)

# Predict on the test set (sub_test)
test_predictions_enet_sub <- predict(final_fit_enet_sub, new_data = test) %>%
  bind_cols(test) %>%
  mutate(price_pred_enet = .pred)  # Convert ln_price to price

# Calculate MAE for sub_test
mae_test_enet_sub <- mae(test_predictions_enet_sub, truth = price, estimate = price_pred_enet)
print(mae_test_enet_sub)

#### MAE = 156

# Save sub_test predictions for superlearner
sub_test_predictions_enet <- test_predictions_enet_sub %>%
  select(price_pred_enet)

# Workflow for Elastic Net (real_train)
workflow_enet <- workflow() %>%
  add_recipe(rec_2) %>%
  add_model(elastic_net_model)

# Cross-validation tuning for Elastic Net (real_train)
tune_results_enet <- tune_grid(
  workflow_enet,
  resamples = block_folds,
  grid = enet_grid,
  metrics = metric_set(mae),
  control = control_grid(save_pred = TRUE)
)

# Save OOF predictions for real_train (optional for superlearner)
oof_predictions_real_train_enet <- collect_predictions(tune_results_enet) %>%
  mutate(price_pred_oof_real_train_enet = .pred) %>%  # Convert ln_price to price
  select(price_pred_oof_real_train_enet)

# Print OOF predictions for inspection (optional)
print(head(oof_predictions_real_train_enet))

# Select the best hyperparameters for Elastic Net (real_train)
best_enet <- select_best(tune_results_enet, metric = "mae")

# Finalize workflow for Elastic Net (real_train)
final_workflow_enet <- finalize_workflow(workflow_enet, best_enet)

# Train the finalized workflow on the complete real_train dataset
final_fit_enet <- fit(final_workflow_enet, data = real_train)

# Predict on the real_test set
test_predictions_enet <- predict(final_fit_enet, new_data = real_test) %>%
  bind_cols(real_test) %>%
  mutate(price_pred_enet = .pred) 

# Save predictions for real_test
real_test_predictions_enet <- test_predictions_enet %>%
  select(property_id, price_pred_enet)

# Submission file for Kaggle
submission_enet <- test_predictions_enet %>%
  mutate(price = round(price_pred_enet, 5)) %>% 
  select(property_id, price)

# Inspect the best hyperparameters for Elastic Net (real_train)
best_hyperparameters_enet <- select_best(tune_results_enet, metric = "mae")
print(best_hyperparameters_enet)

write.csv(submission_enet, "stores/submissions/2_EN_lambda_1_alpha_0.csv", row.names = FALSE)

#### MAE Kaggle = 206

