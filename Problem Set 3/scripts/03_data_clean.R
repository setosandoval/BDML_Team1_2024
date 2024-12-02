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
setwd("C:/Users/User/Documents/RepBDML2/BDML_Team1_2024/Problem Set 3/scripts")

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

# Apply function
data_clean <- impute_hierarchical(data_clean, "num_pisos")
data_clean <- impute_hierarchical(data_clean, "num_parqueaderos")
data_clean <- impute_hierarchical(data_clean, "rooms_imp")
data_clean <- impute_hierarchical(data_clean, "bathrooms_imp")
data_clean <- impute_hierarchical(data_clean, "area_imp")

# Remove and rename variables
data_clean <- data_clean %>%
  select(-num_pisos,
         -num_parqueaderos,
         -rooms_imp,
         -bathrooms_imp,
         -area_imp)  %>%
  rename(floors = num_pisos_imp,
         parkings = num_parqueaderos_imp,
         rooms = rooms_imp_imp,
         bathrooms = bathrooms_imp_imp,
         area = area_imp_imp)

# Remaining missing Values
missing_counts <- colSums(is.na(data_clean))
missing_vars <- missing_counts[missing_counts > 0]
missing_vars



# FINAL VARIABLES ==============================================================

# Drop variables already used
data_clean <- data_clean %>%
  select(-city,
         -title,
         -description,
         -cleaned_text,
         -operation_type)

# Variable indicating time period year_month
data_clean$year_month <- paste(data_clean$year, data_clean$month, sep = "_")
data_clean <- data_clean %>%
  select(-year,
         -month)


# FINAL MERGED DATA SET ========================================================

# PC data
PC_light <- read.csv("stores/data/raw/train_test/PC_light.csv")
PC_med <- read.csv("stores/data/raw/train_test/PC_med.csv")
PC_large <- read.csv("stores/data/raw/train_test/PC_large.csv")

# Merged data
data_light <- merge(data_clean, PC_light, by = "property_id", all = TRUE)
data_med <- merge(data_clean, PC_med, by = "property_id", all = TRUE)
data_large <- merge(data_clean, PC_large, by = "property_id", all = TRUE)

# Export data
write.csv(data_light, "stores/data/work/data_light.csv", row.names = F)
write.csv(data_med, "stores/data/work/data_med.csv", row.names = F)
write.csv(data_large, "stores/data/work/data_large.csv", row.names = F)





