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


