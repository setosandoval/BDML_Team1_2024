# ========================================================================== #
# Problem Set 1                                                              #
# Big Data and Machine Learning - 202402                                     #
#                                                                            #
# Script: Descriptive Statistics                                             #
#                                                                            #
# Team 1: - Sergio Sandoval                                                  #
#         - María Fernanda Blanco                                            #
#         - Juan Fernando Barberi                                            #
#         - Juan Gutierrez                                                   #
# ========================================================================== #


rm(list = ls())

setwd("C:/Users/User/Documents/RepBDML2/BDML_Team1_2024/Problem Set 2")


library(pacman)
p_load(tidyverse,       
       glmnet,          
       caret,           
       dplyr,           
       scales)          

# Load datasets
train_hogares <- read.csv("stores/data/raw/train_hogares.csv")
test_hogares <- read.csv("stores/data/raw/test_personas.csv")
train_personas <- read.csv("stores/data/raw/train_personas.csv")
test_personas <- read.csv("stores/data/raw/test_personas.csv")

# Check column names
colnames(train_hogares)
colnames(train_personas)

### Descriptive statistics:

# 1: Histogram of income based on poverty status


# Classify households as poor or non-poor based on income and poverty line (Lp)
train_combined <- train_hogares %>%
  mutate(pobreza_status = ifelse(Ingtotug < Lp, "Poor", "Not poor"))

# Filter extreme values (top 1%) for visualization clarity
train_filtered <- train_combined %>%
  filter(Ingtotug > 0 & Ingtotug < quantile(Ingtotug, 0.99, na.rm = TRUE))

# Plot: Income distribution relative to poverty line
ggplot(train_filtered, aes(x = Ingtotug, fill = pobreza_status)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(Lp, na.rm = TRUE)), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Total Income Distribution Relative to Poverty Line (pl)",
       subtitle = "Households classified as poor or not poor according to poverty threshold (pl)",
       x = "Total Income (Millions)", y = "Frequency", fill = "Poverty Status") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        plot.subtitle = element_text(hjust = 0.5)) +  # Center the subtitle
  scale_x_continuous(labels = comma) +  # Avoid scientific notation for x-axis
  annotate("text", x = max(train_filtered$Ingtotug) * 0.9, y = -500, 
           label = "*Red line = Poverty Line (pl)", color = "red", size = 3, hjust = 1)

# Calculate average poverty line
average_lp <- mean(train_hogares$Lp, na.rm = TRUE)
cat("The average poverty line value is:", average_lp, "\n")

##### NEW DATA PROCESSING #####

# Clear the workspace
rm(list = ls())

# Load necessary libraries
p_load(tidyverse, glmnet, caret)  # Data manipulation, regularization, machine learning

# Load cleaned datasets
test <- readRDS("stores/data/work/test_clean.rds")
train <- readRDS("stores/data/work//train_clean.rds")

# Check column names
colnames(train)
colnames(test)

### Descriptive statistics:

# A) Percentage of poor households
train$poor <- ifelse(train$poor == "Yes" | train$poor == "Pobre", 1, 0)

total_personas <- nrow(train)  
num_pobres <- sum(train$poor == 1)  

# Calculate and print percentage of poor households
percent_poor <- (num_pobres / total_personas) * 100
cat("The percentage of poor households is:", percent_poor, "%\n")

# B) Number of unemployed persons per household
summary(train$num_unemp)

# Additional statistics
mean_unemp <- mean(train$num_unemp, na.rm = TRUE)  # Mean
median_unemp <- median(train$num_unemp, na.rm = TRUE)  # Median
std_unemp <- sd(train$num_unemp, na.rm = TRUE)  # Standard deviation
min_unemp <- min(train$num_unemp, na.rm = TRUE)  # Minimum
max_unemp <- max(train$num_unemp, na.rm = TRUE)  # Maximum
percentiles_unemp <- quantile(train$num_unemp, probs = c(0.25, 0.75), na.rm = TRUE)  # Percentiles

# Display results
cat("Mean number of unemployed per household:", mean_unemp, "\n")
cat("Median number of unemployed per household:", median_unemp, "\n")
cat("Standard deviation of unemployed per household:", std_unemp, "\n")
cat("Minimum number of unemployed per household:", min_unemp, "\n")
cat("Maximum number of unemployed per household:", max_unemp, "\n")
cat("25th and 75th percentiles of unemployed per household:", percentiles_unemp, "\n")

# Frequency table for unemployed persons per household
table_unemp <- table(train$num_unemp)
percent_unemp <- prop.table(table_unemp) * 100

# Display frequency and percentage table
cat("Frequency of unemployed persons per household:\n")
print(table_unemp)
cat("Percentage of unemployed persons per household:\n")
print(round(percent_unemp, 2))

# C) Unemployment rate per household
summary(train$prop_unemp)

# Calculate additional unemployment statistics
mean_prop_unemp <- mean(train$prop_unemp, na.rm = TRUE)
median_prop_unemp <- median(train$prop_unemp, na.rm = TRUE)
std_prop_unemp <- sd(train$prop_unemp, na.rm = TRUE)
min_prop_unemp <- min(train$prop_unemp, na.rm = TRUE)
max_prop_unemp <- max(train$prop_unemp, na.rm = TRUE)
percentiles_prop_unemp <- quantile(train$prop_unemp, probs = c(0.25, 0.75), na.rm = TRUE)

# Display results
cat("Mean unemployment rate per household:", mean_prop_unemp, "\n")
cat("Median unemployment rate per household:", median_prop_unemp, "\n")
cat("Standard deviation of unemployment rate per household:", std_prop_unemp, "\n")
cat("Minimum unemployment rate per household:", min_prop_unemp, "\n")
cat("Maximum unemployment rate per household:", max_prop_unemp, "\n")
cat("25th and 75th percentiles of unemployment rate per household:", percentiles_prop_unemp, "\n")

# D) Maximum education level in the household
table_max_educ <- table(train$max_educ)
cat("Frequency of each education level (max_educ):\n")
print(table_max_educ)

# Calculate the mode
mode_max_educ <- names(which.max(table_max_educ))

# Display summary and mode
cat("\nSummary of max_educ:\n")
summary(train$max_educ)
cat("Most frequent education level:", mode_max_educ, "\n")

###### EXTRA STATISTICS ######

# 1: Histogram of rent prices categorized by poverty status
train_filtered <- train %>%
  filter(rent > 0 & rent < quantile(rent, 0.99, na.rm = TRUE))  # Filter rent outliers

ggplot(train_filtered, aes(x = rent, fill = factor(poor, labels = c("Not poor", "Poor")))) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(rent, na.rm = TRUE)), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Histogram of Rent Prices Categorized by Poverty Status",
       subtitle = "Household observations by rent price categorized as poor or not poor",
       x = "Rent Price (Millions)", y = "Frequency", fill = "Poverty Status") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotate("text", x = max(train_filtered$rent) * 0.9, y = -500, 
           label = "*Red line = Average rent price", color = "red", size = 3, hjust = 1)

# 2: Calculate the percentage of households below and above the poverty line
percent_below_poverty_line <- sum(train$rent < train$pl, na.rm = TRUE) / nrow(train) * 100
percent_above_poverty_line <- sum(train$rent >= train$pl, na.rm = TRUE) / nrow(train) * 100

cat("Percentage of households below the poverty line:", percent_below_poverty_line, "%\n")
cat("Percentage of households above the poverty line:", percent_above_poverty_line, "%\n")