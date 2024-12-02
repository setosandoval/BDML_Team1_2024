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

