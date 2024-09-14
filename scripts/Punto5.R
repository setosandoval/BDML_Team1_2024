# ENVIRONMENT SETUP AND PACKAGE LOADING ======================================

# Set working directory
setwd("")

# Clean the environment
rm(list = ls())

# Seed
set.seed(202028276)

# Load necessary libraries using pacman
library(pacman)
p_load(rio,        # For importing/exporting data
       tidyverse,  # For tidy-data operations
       skimr,      # For summarizing data
       visdat,     # For visualizing missing data
       stargazer,  # For creating tables/output to LaTeX
       boot,       # For bootstrap
       MASS)       # For regression calculations



# DATA LOADING AND CLEANING ==================================================

# Load the RDS file
df <- readRDS("stores/data_PS1.rds")

# Convert the dataframe to a tibble
db <- as_tibble(df)

# Filter the data according to the given conditions
db_clean <- db %>%
  filter(age > 18,               # Include individuals older than 18 years
         !is.na(age),            # Remove missing values in age
         y_salary_m > 0,         # Include only positive salaries
         !is.na(y_salary_m)) %>% # Remove missing values in salary
  mutate(log_wage = log(y_salary_m)) # Create a new column with the log of wages



# 5. PREDICTING EARNINGS ====================================================
