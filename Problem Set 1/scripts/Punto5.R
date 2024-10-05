# ENVIRONMENT SETUP AND PACKAGE LOADING ======================================

# Set working directory
setwd("/Users/home/Library/Mobile Documents/com~apple~CloudDocs/Universidad/Semestre8/BigData/Problem_Sets/PS1/BDML_Team1_PS1")

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

# A) TRAINING-TEST ----------------------------------------------------------

# Create training and testing datasets (70%-30% split)
inTrain <- createDataPartition(
  y = db_clean$y_salary_m,  # Target variable for partition
  p = 0.70,  # Proportion of data for training set
  list = FALSE  # Return indices as a vector
)

# Create the training dataset
training <- db_clean %>%
  filter(row_number() %in% inTrain)

# Create the testing dataset
testing <- db_clean %>% 
  filter(!row_number() %in% inTrain)

# B) MODELS -----------------------------------------------------------------


# Model 1: Quadratic Age
model_1a <- lm(form_1, data = training)

# Out of sample Performance
predictions <- predict(model_1a, testing)
score_1a <- RMSE(predictions, testing$log_wage)


# Model 2: Sex
model_2a <- lm(form_2, data = training)

# Out of sample Performance
predictions <- predict(model_2a, testing)
score_2a <- RMSE(predictions, testing$log_wage)


# Model 3: Conditional Sex
model_3a <- lm(form_3, data = training)

# Out of sample Performance
predictions <- predict(model_3a, testing)
score_3a <- RMSE(predictions, testing$log_wage)


# Model 4: Age, age squared, and all variables
form_4 <- log_wage ~ age + I(age^2) + female + hoursWorkUsual + formal + relab  + maxEducLevel + estrato1 + sizeFirm
model_4a <- lm(form_4, data = training)

# Out of sample Performance
predictions <- predict(model_4a, testing)
score_4a <- RMSE(predictions, testing$log_wage)


# Model 5:  Logarithm of hours worked and firm size
form_5 <- log_wage ~ age + I(age^2) + female + log(hoursWorkUsual) + formal + relab + maxEducLevel + estrato1 + log(sizeFirm)
model_5a <- lm(form_5, data = training)

# Out of sample Performance
predictions <- predict(model_5a, testing)
score_5a <- RMSE(predictions, testing$log_wage)


# Model 6: Multiple interactions, no logs
form_6 <- log_wage ~ age + I(age^2) + female + hoursWorkUsual + formal + relab + maxEducLevel + estrato1 + sizeFirm +
  female:age + female:hoursWorkUsual + age:maxEducLevel + hoursWorkUsual:relab  
model_6a <- lm(form_6, data = training)

# Out of sample Performance
predictions <- predict(model_6a, testing)
score_6a <- RMSE(predictions, testing$log_wage)


# Model 7: Logs and interactions
form_7 <- log_wage ~ age + I(age^2) + female + log(hoursWorkUsual) + formal + relab + maxEducLevel + estrato1 + log(sizeFirm) +
  female:age + female:log(hoursWorkUsual) + age:maxEducLevel + log(hoursWorkUsual):relab  
model_7a <- lm(form_7, data = training)

# Out of sample Performance
predictions <- predict(model_7a, testing)
score_7a <- RMSE(predictions, testing$log_wage)


# Modelo 8: Complex specification with logs, polynomials, and interactions
form_8 <- log_wage ~ age + poly(age, 3, raw = TRUE) + female + log(hoursWorkUsual) + formal + relab + maxEducLevel + estrato1 + log(sizeFirm) +
  poly(age, 3, raw = TRUE):female + poly(age, 3, raw = TRUE):maxEducLevel +  poly(age, 3, raw = TRUE):relab
model_8a <- lm(form_8, data = training)

# Out of sample Performance
predictions <- predict(model_8a, testing)
score_8a <- RMSE(predictions, testing$log_wage)

# Create a data frame to store RMSE scores for all models
scores_test <- data.frame( Model= c(1, 2, 3, 4, 5, 6, 7, 8),
                           RMSE= c(score_1a, score_2a, score_3a, score_4a, score_5a, score_6a, score_7a, score_8a))
print(scores_test)

# Export the scores table to LaTeX
cat("
\\begin{table}[ht]
\\centering
\\begin{tabular}{lc}
\\hline
Model & RMSE \\\\
\\hline
1 & ", round(score_1a, 4), " \\\\
2 & ", round(score_2a, 4), " \\\\
3 & ", round(score_3a, 4), " \\\\
4 & ", round(score_4a, 4), " \\\\
5 & ", round(score_5a, 4), " \\\\
6 & ", round(score_6a, 4), " \\\\
7 & ", round(score_7a, 4), " \\\\
8 & ", round(score_8a, 4), " \\\\
\\hline
\\end{tabular}
\\caption{RMSE Scores for Different Models}
\\label{tab:rmse_scores}
\\end{table}
", file = "views/model_rmse_scores.tex")

# Model with smallest RSME: 8
# Obtain predictions from model 8 on the test set
predictions_8 <- predict(model_8a, testing)

# Calculate prediction errors (residuals)
prediction_errors <- testing$log_wage - predictions_8

# Visualize the distribution of prediction errors (histogram)
ggplot(data = testing, aes(x = prediction_errors)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Prediction Errors", x = "Prediction Error", y = "Frequency") +
  theme_minimal()

# Boxplot to visualize outliers
ggplot(data = testing, aes(y = prediction_errors)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Boxplot of Prediction Errors", y = "Prediction Error") +
  theme_minimal()



# C) LOOCV ------------------------------------------------------------------

# Models with smallest RSME: 5,7 AND 8

ctrl <- trainControl(
  method = "LOOCV") ## input the method Leave One Out Cross Validation


model_5b <- train(form_5,
                  data = db_clean,
                  method = 'lm', 
                  trControl= ctrl)

score_5b <- RMSE(model_5b$pred$pred, db$log_wage)


model_7b <- train(form_7,
                  data = db_clean,
                  method = 'lm', 
                  trControl= ctrl)

score_7b <- RMSE(model_5b$pred$pred, db$log_wage)


model_8b <- train(form_8,
                  data = db_clean,
                  method = 'lm', 
                  trControl= ctrl)

score_8b <- RMSE(model_5b$pred$pred, db$log_wage)


scores_test2 <- data.frame(Model= c(5, 7, 8),
                           RMSE = c(score_5a, score_7a, score_8a),
                           RMSE_LOOCV = c(score_5b, score_7b, score_8b))
print(scores_test2)




# C) LOOCV ------------------------------------------------------------------
# Perform Leave-One-Out Cross Validation (LOOCV) on models 5, 7, and 8

# Define the control method for LOOCV
ctrl <- trainControl(method = "LOOCV")  # Leave-One-Out Cross Validation

# Model 5 with LOOCV
model_5b <- train(form_5,  # Formula for model 5
                  data = db_clean,  # Full dataset
                  method = 'lm',  # Linear regression
                  trControl = ctrl)  # LOOCV control

# Calculate RMSE for model 5 using LOOCV predictions
score_5b <- RMSE(model_5b$pred$pred, db_clean$log_wage)


# Model 7 with LOOCV
model_7b <- train(form_7,  # Formula for model 7
                  data = db_clean,  # Full dataset
                  method = 'lm',  # Linear regression
                  trControl = ctrl)  # LOOCV control

# Calculate RMSE for model 7 using LOOCV predictions
score_7b <- RMSE(model_7b$pred$pred, db_clean$log_wage)


# Model 8 with LOOCV
model_8b <- train(form_8,  # Formula for model 8
                  data = db_clean,  # Full dataset
                  method = 'lm',  # Linear regression
                  trControl = ctrl)  # LOOCV control

# Calculate RMSE for model 8 using LOOCV predictions
score_8b <- RMSE(model_8b$pred$pred, db_clean$log_wage)


# Create a data frame to compare RMSE for models 5, 7, and 8 (with and without LOOCV)
scores_test2 <- data.frame(Model = c(5, 7, 8),
                           RMSE = c(score_5a, score_7a, score_8a),  # RMSE from the test sample
                           RMSE_LOOCV = c(score_5b, score_7b, score_8b))  # RMSE from LOOCV

# Print the scores table
print(scores_test2)

# Export the table to LaTeX format
cat("
\\begin{table}[ht]
\\centering
\\begin{tabular}{lcc}
\\hline
Model & RMSE (Test) & RMSE (LOOCV) \\\\
\\hline
5 & ", round(score_5a, 4), " & ", round(score_5b, 4), " \\\\
7 & ", round(score_7a, 4), " & ", round(score_7b, 4), " \\\\
8 & ", round(score_8a, 4), " & ", round(score_8b, 4), " \\\\
\\hline
\\end{tabular}
\\caption{Comparison of RMSE for Models 5, 7, and 8 (Test vs. LOOCV)}
\\label{tab:rmse_loocv}
\\end{table}
", file = "views/rmse_loocv_comparison.tex")
