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



# 3) LightGBM  =================================================================

# Define the LightGBM model for tuning
lightgbm_model <- boost_tree(
  trees = 500,            # Maximum number of trees
  tree_depth = tune(),    # Tune tree depth
  min_n = tune(),         # Tune minimum node size
  learn_rate = tune(),    # Tune learning rate
) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")

# Workflow for LightGBM (sub_train)
workflow_lightgbm_sub <- workflow() %>%
  add_recipe(rec_3_sub) %>%
  add_model(lightgbm_model)

# Define hyperparameter grid for LightGBM 
lightgbm_grid <- expand.grid(
  tree_depth = c(3, 5, 7),       
  min_n = c(10, 15, 20, 30),         
  learn_rate = c(0.05, 0.1, 0.2) 
)

# Cross-validation tuning for LightGBM (sub_train)
tune_results_lightgbm_sub <- tune_grid(
  workflow_lightgbm_sub,
  resamples = block_folds_sub_2,
  grid = lightgbm_grid,
  metrics = metric_set(mae),
  control = control_grid(save_pred = TRUE)  
)

# Save OOF predictions for superlearner
oof_predictions_lightgbm <- collect_predictions(tune_results_lightgbm_sub) %>%
  mutate(price_pred_oof_lightgbm = exp(.pred)) %>%  
  select(price_pred_oof_lightgbm)

# Print OOF predictions for inspection (optional)
print(head(oof_predictions_lightgbm))

# Select the best hyperparameters for LightGBM (sub_train)
best_lightgbm_sub <- select_best(tune_results_lightgbm_sub, metric = "mae")
print(best_lightgbm_sub)  # Display best hyperparameters (even if fixed)

# Finalize workflow for LightGBM (sub_train)
final_workflow_lightgbm_sub <- finalize_workflow(workflow_lightgbm_sub, best_lightgbm_sub)

# Train the finalized workflow on the complete sub_train dataset
final_fit_lightgbm_sub <- fit(final_workflow_lightgbm_sub, data = train_2)

# Predict on the test set (sub_test)
test_predictions_lightgbm_sub <- predict(final_fit_lightgbm_sub, new_data = test_2) %>%
  bind_cols(test_2) %>%
  mutate(price_pred_lightgbm = exp(.pred))  

# Calculate MAE for sub_test
mae_test_lightgbm_sub <- mae(test_predictions_lightgbm_sub, truth = price, estimate = price_pred_lightgbm)
print(mae_test_lightgbm_sub)

#### MAE = 82

# Save sub_test predictions for superlearner
sub_test_predictions_lightgbm <- test_predictions_lightgbm_sub %>%
  select(price_pred_lightgbm)

# Workflow for LightGBM (real_train)
workflow_lightgbm <- workflow() %>%
  add_recipe(rec_3) %>%
  add_model(lightgbm_model)

# Cross-validation (CV) for real_train
tune_results_lightgbm <- tune_grid(
  workflow_lightgbm,
  resamples = block_folds_2,
  grid = lightgbm_grid,
  metrics = metric_set(mae),
  control = control_grid(save_pred = TRUE) 
)

# Save OOF predictions for real_train (optional for superlearner)
oof_predictions_real_train_lightgbm <- collect_predictions(tune_results_lightgbm) %>%
  mutate(price_pred_oof_real_train_lightgbm = exp(.pred)) %>% 
  select(price_pred_oof_real_train_lightgbm)

# Print OOF predictions for inspection (optional)
print(head(oof_predictions_real_train_lightgbm))

# Select the best hyperparameters for LightGBM (real_train)
best_lightgbm <- select_best(tune_results_lightgbm, metric = "mae")
print(best_lightgbm)  # Display best hyperparameters (even if fixed)

# Finalize workflow for LightGBM (real_train)
final_workflow_lightgbm <- finalize_workflow(workflow_lightgbm, best_lightgbm)

# Train the finalized workflow on the complete real_train dataset
final_fit_lightgbm <- fit(final_workflow_lightgbm, data = real_train_2)

# Predict on the real_test set
test_predictions_lightgbm <- predict(final_fit_lightgbm, new_data = real_test) %>%
  bind_cols(real_test) %>%
  mutate(price_pred_lightgbm = exp(.pred))  

# Save predictions for real_test
real_test_predictions_lightgbm <- test_predictions_lightgbm %>%
  select(property_id, price_pred_lightgbm)

# Submission file for Kaggle
submission_lightgbm <- test_predictions_lightgbm %>%
  mutate(price = round(price_pred_lightgbm, 5)) %>%  
  select(property_id, price)

write.csv(submission_lightgbm, "stores/submissions/4_LightGBM_ntrees_500_minn_30_treedepth_5_lr_0.05.csv", row.names = FALSE)

#### MAE Kaggle = 188

# Graph variable importance

# Extraer el modelo LightGBM desde el `final_fit_lightgbm`
lightgbm_model_object <- extract_fit_engine(final_fit_lightgbm)

# Obtener la importancia de las variables
variable_importance <- lgb.importance(lightgbm_model_object)

# Convertir a un dataframe para ggplot2
variable_importance_df <- as_tibble(variable_importance)

# Graficar la importancia de las variables
ggplot(variable_importance_df, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Rotar el gráfico
  labs(
    title = "Variable Importance",
    x = "Features",
    y = "Gain",
    caption = "Source: LightGBM Model"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

# Filtrar las primeras 40 variables más importantes
top_30_variables <- variable_importance_df %>% 
  slice_max(order_by = Gain, n = 30)

top_30_variables <- subset(top_30_variables, !(Feature == "lat" | Feature == "lon"))

corrections <- c(
  "bathrooms" = "Bathrooms",
  "area" = "Area",
  "parkings" = "Parkings",
  "bedrooms" = "Bedrooms",
  "property_type.Apartamento" = "Property Type: Apartamento",
  "estrato.6" = "Estrato 6",
  "estrato.3" = "Estrato 3",
  "estrato.5" = "Estrato 5",
  "estrato.4" = "Estrato 4",
  "man_dens" = "Manzana's Density",
  "dist_station" = "Distance to Station",
  "dist_cycle" = "Distance to Cycle Path",
  "dist_mall" = "Distance to Mall",
  "dist_highway" = "Distance to Highway",
  "dist_school" = "Distance to School",
  "id_UPZ.16" = "UPZ ID 16",
  "val_cat" = "Valution Catastro"
)

# Corregir nombres con dplyr
top_30_variables <- top_30_variables %>%
  mutate(
    name = case_when(
      Feature %in% names(corrections) ~ corrections[Feature],
      grepl("^PC[0-9]+$", Feature) ~ Feature, # Mantener PC# como está
      TRUE ~ Feature # Para casos no especificados, mantener original
    )
  )

# Graficar la importancia de las variables
graph <- ggplot(top_30_variables, aes(x = reorder(name, Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +  # Rotar el gráfico
  labs(x = "Features", y = "Gain",
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

ggsave("views/figure0.pdf", graph, width = 10, height = 8, dpi = 300)


# 4) Neural Network  ===========================================================

# Define a Neural Network Model with `keras`
build_nn <- function(input_shape) {
  model <- keras_model_sequential() %>%
    layer_dense(units = 128, activation = "tanh", input_shape = input_shape) %>%
    layer_dropout(rate = 0.2) %>%  
    layer_dense(units = 64, activation = "relu") %>%
    layer_dropout(rate = 0.1) %>%  
    layer_dense(units = 1, activation = "linear") 
  model %>% compile(
    optimizer = optimizer_adam(learning_rate = 0.001),
    loss = "mean_squared_error",
    metrics = list("mean_absolute_error")
  )
  return(model)
}

# Define a parsnip model with keras
nn_model <- mlp(
  epochs = 50,           
  hidden_units = NULL,      
  penalty = NULL,         
  activation = "relu"     
) %>%
  set_engine("keras", build_fn = build_nn) %>%
  set_mode("regression")

# Workflow for Neural Network (sub_train)
workflow_nn_sub <- workflow() %>%
  add_recipe(rec_3_sub) %>%
  add_model(nn_model)

# Cross-validation (CV) for sub_train
cv_results_nn_sub <- fit_resamples(
  workflow_nn_sub,
  resamples = block_folds_sub,
  metrics = metric_set(mae),
  control = control_resamples(save_pred = TRUE)  
)

# Save OOF predictions for superlearner
oof_predictions_nn <- collect_predictions(cv_results_nn_sub) %>%
  mutate(price_pred_oof_nn = .pred) %>%  
  select(price_pred_oof_nn)

# Print OOF predictions for inspection (optional)
print(head(oof_predictions_nn))

# Select the best model (not needed as no tuning, but included for structure)
best_nn_sub <- select_best(cv_results_nn_sub, metric = "mae")

# Finalize workflow for Neural Network (sub_train)
final_workflow_nn_sub <- finalize_workflow(workflow_nn_sub, best_nn_sub)

# Train the finalized workflow on the complete sub_train dataset
final_fit_nn_sub <- fit(final_workflow_nn_sub, data = train)

# Predict on the test set (sub_test)
test_predictions_nn_sub <- predict(final_fit_nn_sub, new_data = test) %>%
  bind_cols(test) %>%
  mutate(price_pred_nn = exp(.pred))  

# Calculate MAE for sub_test
mae_test_nn_sub <- mae(test_predictions_nn_sub, truth = price, estimate = price_pred_nn)
print(mae_test_nn_sub)

#### MAE = 136

# Save sub_test predictions for superlearner
sub_test_predictions_nn <- test_predictions_nn_sub %>%
  select(price_pred_nn)

# Workflow for Neural Network (real_train)
workflow_nn <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(nn_model)

# Cross-validation (CV) for real_train
cv_results_nn <- fit_resamples(
  workflow_nn,
  resamples = block_folds,
  metrics = metric_set(mae),
  control = control_resamples(save_pred = TRUE)  
)

# Save OOF predictions for real_train (optional for superlearner)
oof_predictions_real_train_nn <- collect_predictions(cv_results_nn) %>%
  mutate(price_pred_oof_real_train_nn = exp(.pred)) %>%  
  select(price_pred_oof_real_train_nn)

# Print OOF predictions for inspection (optional)
print(head(oof_predictions_real_train_nn))

# Finalize workflow for Neural Network (real_train)
final_workflow_nn <- finalize_workflow(workflow_nn, best_nn_sub)

# Train the finalized workflow on the complete real_train dataset
final_fit_nn <- fit(final_workflow_nn, data = real_train)

# Predict on the real_test set
test_predictions_nn <- predict(final_fit_nn, new_data = real_test) %>%
  bind_cols(real_test) %>%
  mutate(price_pred_nn = exp(.pred)) 

# Save predictions for real_test
real_test_predictions_nn <- test_predictions_nn %>%
  select(property_id, price_pred_nn)

# Submission file for Kaggle
submission_nn <- test_predictions_nn %>%
  mutate(price = round(price_pred_nn, 5)) %>%  
  select(property_id, price)

write.csv(submission_nn, "stores/submissions/5_NN_layer_2_unit_128_64_dropout_0.2_0.1.csv", row.names = FALSE)



# 5) Super Learner  ============================================================

# Combine OOF predictions into a single data frame
oof_data <- data.frame(
  price = train$price,  # True price values from the training set
  lin_reg = oof_predictions_lin_reg$price_pred_oof_lin_reg,
  elastic_net = oof_predictions_enet$price_pred_oof_enet,
  lightgbm = oof_predictions_lightgbm$price_pred_oof_lightgbm,
  nn = oof_predictions_nn$price_pred_oof_nn
)

# Fit a linear regression model on the OOF predictions
super_learner_model <- lm(price ~ ., data = oof_data)

# Print summary of the super learner model (optional)
print(summary(super_learner_model))

# Combine real_test predictions into a single data frame
real_test_predictions <- data.frame(
  lin_reg = real_test_predictions_lin_reg$price_pred_lin_reg,
  elastic_net = real_test_predictions_enet$price_pred_enet,
  lightgbm = real_test_predictions_lightgbm$price_pred_lightgbm,
  nn = real_test_predictions_nn$price_pred_nn
)

# Use the super learner model to predict on real_test
super_learner_predictions <- predict(super_learner_model, newdata = real_test_predictions)

# Create submission file for Kaggle
submission_super_learner <- data.frame(
  property_id = real_test$property_id,
  price = round(super_learner_predictions, 5)  
)

write.csv(submission_super_learner, "stores/submissions/6_SuperLearner_LN_EN_LGB_NN.csv", row.names = FALSE)

#### MAE Kaggle = 183



# 6) XGBoost Model ============================================================

# XGBoost Model
xgboost_model <- boost_tree(
  trees = 500,            
  tree_depth = tune(),    
  min_n = tune(),         
  learn_rate = tune(),    
  loss_reduction = tune(), 
  sample_size = tune()     
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# Workflow for XGBoost (sub_train)
workflow_xgb_sub <- workflow() %>%
  add_recipe(rec_1_sub) %>%
  add_model(xgboost_model)

# Define hyperparameter grid for XGBoost
xgb_grid <- expand.grid(
  tree_depth = c(6, 8),    
  min_n = c(15, 30),          
  learn_rate = c(0.05, 0.1), 
  loss_reduction = 0.01,    
  sample_size = 0.8           
)

# Control settings for verbose output
control_verbose <- control_grid(
  verbose = TRUE,        
  save_pred = TRUE,     
  save_workflow = TRUE   
)

# Cross-validation tuning for XGBoost (sub_train)
tune_results_xgb_sub <- tune_grid(
  workflow_xgb_sub,
  resamples = block_folds_sub,
  grid = xgb_grid,
  metrics = metric_set(mae),
  control = control_verbose
)

# Save OOF predictions for superlearner
oof_predictions_xgb <- collect_predictions(tune_results_xgb_sub) %>%
  mutate(price_pred_oof_xgb = exp(.pred)) %>%  
  select(price_pred_oof_xgb)

# Print OOF predictions for inspection (optional)
print(head(oof_predictions_xgb))

# Select the best hyperparameters for XGBoost (sub_train)
best_xgb_sub <- select_best(tune_results_xgb_sub, metric = "mae")
print(best_xgb_sub)  

# Finalize workflow for XGBoost (sub_train)
final_workflow_xgb_sub <- finalize_workflow(workflow_xgb_sub, best_xgb_sub)

# Train the finalized workflow on the complete sub_train dataset
final_fit_xgb_sub <- fit(final_workflow_xgb_sub, data = train)

# Predict on the test set (sub_test)
test_predictions_xgb_sub <- predict(final_fit_xgb_sub, new_data = test) %>%
  bind_cols(test) %>%
  mutate(price_pred_xgb = exp(.pred))  

# Calculate MAE for sub_test
mae_test_xgb_sub <- mae(test_predictions_xgb_sub, truth = price, estimate = price_pred_xgb)
print(mae_test_xgb_sub)

#### MAE Kaggle = 92

# Save sub_test predictions for superlearner
sub_test_predictions_xgb <- test_predictions_xgb_sub %>%
  select(price_pred_xgb)

# Workflow for XGBoost (real_train)
workflow_xgb <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(xgboost_model)

# Cross-validation tuning for XGBoost (real_train)
tune_results_xgb <- tune_grid(
  workflow_xgb,
  resamples = block_folds,
  grid = xgb_grid,
  metrics = metric_set(mae),
  control = control_verbose
)

# Save OOF predictions for real_train (optional for superlearner)
oof_predictions_real_train_xgb <- collect_predictions(tune_results_xgb) %>%
  mutate(price_pred_oof_real_train_xgb = exp(.pred)) %>%  
  select(price_pred_oof_real_train_xgb)

# Print OOF predictions for inspection (optional)
print(head(oof_predictions_real_train_xgb))

# Select the best hyperparameters for XGBoost (real_train)
best_xgb <- select_best(tune_results_xgb, metric = "mae")
print(best_xgb) 

# Finalize workflow for XGBoost (real_train)
final_workflow_xgb <- finalize_workflow(workflow_xgb, best_xgb)

# Train the finalized workflow on the complete real_train dataset
final_fit_xgb <- fit(final_workflow_xgb, data = real_train)

# Predict on the real_test set
test_predictions_xgb <- predict(final_fit_xgb, new_data = real_test) %>%
  bind_cols(real_test) %>%
  mutate(price_pred_xgb = exp(.pred))  

# Save predictions for real_test
real_test_predictions_xgb <- test_predictions_xgb %>%
  select(property_id, price_pred_xgb)

# Submission file for Kaggle
submission_xgb <- test_predictions_xgb %>%
  mutate(price = round(price_pred_xgb, 5)) %>% 
  select(property_id, price)

write.csv(submission_xgb, "stores/submissions/3_XGB_ntrees_500_minn_30_treedepth_6.csv", row.names = FALSE)

#### MAE Kaggle = 200



