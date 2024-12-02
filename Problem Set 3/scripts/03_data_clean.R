# ============================================================================ #
# Problem Set 1                                                                #
# Big Data and Machine Learning - 202420                                       #
#                                                                              #
# Script: Data Cleaning                                                        #
#                                                                              #
# Team 1: - Sergio Sandoval                                                    #
#         - María Fernanda Blanco                                              #
#         - Juan Fernando Barberi                                              #
#         - Juan Gutierrez                                                     #
# ============================================================================ #


# ENVIRONMENT SETUP AND DATA UPLOADING =========================================

# Set working directory
setwd("/Users/setosandoval/Documents/Problem Set 3")

# Clean the environment
rm(list = ls())

# Seed
set.seed(202028276)

# Load necessary libraries using pacman
library(pacman)
p_load(tidyverse)     # Tidy-data

# Data
data_location <-read.csv("stores/data/raw/train_test/data_location.csv")
data_text <-read.csv("stores/data/raw/train_test/data_text.csv")

# Combined data
data <- merge(data_location, data_text, by = "property_id", all = TRUE)


# MISSING VALUES AND OUTLIERS ==================================================

# Missing Values
missing_counts <- colSums(is.na(data))
missing_vars <- missing_counts[missing_counts > 0]
missing_vars

# Impute original data set missing values with values from text variables
data$rooms_imp <-  pmin(data$rooms, data$num_habitaciones, na.rm = TRUE)
data$bathrooms_imp <- pmin(data$bathrooms, data$num_banos, na.rm = TRUE)
data$area_imp <- pmin(data$surface_total, data$surface_covered, data$area_m2, na.rm = TRUE)

data_clean <- data %>%
  select(-rooms,
         -num_habitaciones,
         -bathrooms,
         -num_banos,
         -surface_total,
         -surface_covered,
         -area_m2)

# Too many missings year constructed
data_clean <- data_clean %>%
  select(-ano_construccion)

# Missing Values
missing_counts <- colSums(is.na(data_clean))
missing_vars <- missing_counts[missing_counts > 0]
missing_vars

# Outliers
table(data_clean$num_pisos)
data_clean$num_pisos <- ifelse(data_clean$num_pisos > 50, NA, data_clean$num_pisos)

table(data_clean$num_parqueaderos)
data_clean$num_parqueaderos <- ifelse(data_clean$num_parqueaderos > 5, NA, data_clean$num_parqueaderos)

table(data_clean$rooms_imp)
data_clean$rooms_imp <- ifelse(data_clean$rooms_imp > 15, NA, data_clean$rooms_imp)

table(data_clean$bathrooms_imp)
data_clean$bathrooms_imp <- ifelse(data_clean$bathrooms_imp > 15, NA, data_clean$bathrooms_imp)

table(data_clean$area_imp)
data_clean$area_imp <- ifelse(data_clean$area_imp > 1000, NA, data_clean$area_imp)

# Remaining missing Values
missing_counts <- colSums(is.na(data_clean))
missing_vars <- missing_counts[missing_counts > 0]
missing_vars

# IMPUTATION ===================================================================

# Function to impute missing values hierarchically
impute_hierarchical <- function(data, variable) {
  # Create the name for the new imputed variable
  variable_imp <- paste0(variable, "_imp")
  
  # 1. Impute by id_manz
  data <- data %>%
    group_by(id_manz) %>%
    mutate(!!variable_imp := ifelse(
      is.na(.data[[variable]]),
      median(.data[[variable]], na.rm = TRUE),  # Median by id_manz
      .data[[variable]]  # Keep original value if not NA
    )) %>%
    ungroup()
  
  # 2. Impute by id_UPZ
  data <- data %>%
    group_by(id_UPZ) %>%
    mutate(!!variable_imp := ifelse(
      is.na(.data[[variable_imp]]),
      median(.data[[variable_imp]], na.rm = TRUE),  # Median by id_UPZ
      .data[[variable_imp]]  # Keep the existing value
    )) %>%
    ungroup()
  
  # 3. Impute by id_local
  data <- data %>%
    group_by(id_local) %>%
    mutate(!!variable_imp := ifelse(
      is.na(.data[[variable_imp]]),
      median(.data[[variable_imp]], na.rm = TRUE),  # Median by id_local
      .data[[variable_imp]]  # Keep the existing value
    )) %>%
    ungroup()
  
  # 4. Impute with overall median
  data <- data %>%
    mutate(!!variable_imp := ifelse(
      is.na(.data[[variable_imp]]),
      median(.data[[variable_imp]], na.rm = TRUE),  # Overall median
      .data[[variable_imp]]  # Keep the existing value
    ))
  
  return(data)
}



