# ========================================================================== #
# Problem Set 1                                                              #
# Big Data and Machine Learning - 202402                                     #
#                                                                            #
# Script: Work                                                               #
#                                                                            #
# Team 1: - Sergio Sandoval                                                  #
#         - María Fernanda Blanco                                            #
#         - Juan Fernando Barberi                                            #
#         - Juan Gutierrez                                                   #
# ========================================================================== #

# ENVIRONMENT SETUP AND PACKAGE LOADING ======================================

# Set working directory
setwd("/Users/setosandoval/Desktop/BDML/Problem Sets/PS1/BDML_Team1_PS1")

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
       MASS,       # For regression calculations
       dplyr)      



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
  mutate(log_wage = log(y_salary_m)) %>% #  # Create a new column with the log of wages
  mutate(y_salary_m_scale = y_salary_m/2e06) # Create a new column with wage in scale

# Variables to keep
db_clean <- db_clean[, c("y_salary_m", "age", "sex", "hoursWorkUsual", "formal", "relab", "maxEducLevel", "estrato1", "sizeFirm", "log_wage", "y_salary_m_scale")]

# Missings
colSums(is.na(db_clean[, c("log_wage", "age", "sex", "hoursWorkUsual", "formal", "relab", "maxEducLevel", "estrato1", "sizeFirm")]))
db_clean <- db_clean %>%
  filter(!is.na(maxEducLevel))  # Remove missing values             


# 2. DESCRIPTIVE STATISTICS ==================================================

# Complete



# 3. AGE-WAGE PROFILE ========================================================

# MODEL ----------------------------------------------------------------------

# Quadratic regression model: log(w) = β1 + β2*Age + β3*Age^2 + u
form_1 <- log_wage ~ age + I(age^2)
model_1 <- lm(form_1, data = db_clean) # Fit the linear model
summary(model_1) # Display the model summary


# PEAK AGE -------------------------------------------------------------------

# Calculate the peak age (the age at which wage maximizes) using the quadratic model
# The critical point is found where β2 + 2*β3*age = 0
beta2 <- coef(model_1)["age"]
beta3 <- coef(model_1)["I(age^2)"]
peak_age <- -beta2 / (2 * beta3)

# Print the peak age
print(paste("The peak age is approximately:", round(peak_age, 2)))


# Bootstrap Function for CI --------------------------------------------------

# Define a function to get model coefficients for bootstrapping
boot_fn <- function(data, index) {
  model <- lm(model_1, data = data[index, ])
  return(coef(model))
}

# Perform bootstrap with 1000 replications
bootstrap_results <- boot(db_clean, boot_fn, R = 1000)

# Calculate the peak age for each bootstrap sample
boot_peak_ages <- -bootstrap_results$t[, 2] / (2 * bootstrap_results$t[, 3])

# Calculate the 95% confidence interval for the peak age
ci_peak_age <- quantile(boot_peak_ages, probs = c(0.025, 0.975))

# Print the 95% confidence interval for the peak age
print(paste("The 95% confidence interval for the peak age is:",
            round(ci_peak_age[1], 3), "to", round(ci_peak_age[2], 3)))


# ESTIMATES ------------------------------------------------------------------

# Prepare the peak age and confidence intervals as separate rows
additional_rows <- list(
  c("Peak Age", round(peak_age, 1)), # Peak age
  c("95% CI for Peak Age (Bootstrap)", paste(round(ci_peak_age[1], 2), "-", round(ci_peak_age[2], 2))) # Confidence intervals
)

# Export to LaTeX
stargazer(model_1, type = "latex",
          omit.stat = c("f", "adj.rsq", "ser"), # Only include observations and R-squared
          add.lines = additional_rows, # Add peak age and confidence interval
          title = "Regression Results: Log Wage as a Function of Age",
          dep.var.labels = "Log Wage",
          covariate.labels = c("Age", "Age2"), # Label the coefficients
          digits = 3,
          out = "views/reg_model1.tex") # Save to LaTeX file


# PLOT ----------------------------------------------------------------------

# Generate a sequence of ages for predicting wage values
age_seq <- data.frame(age = seq(min(db_clean$age), max(db_clean$age), by = 1))

# Predict log wage with confidence intervals
predictions <- predict(model_1, newdata = age_seq, interval = "confidence")

# Add predicted log wage and confidence intervals to the dataframe
age_seq$predicted_log_wage <- predictions[, "fit"]
age_seq$lower_ci <- predictions[, "lwr"]
age_seq$upper_ci <- predictions[, "upr"]

# Convert predicted log wage back to regular wage in scale
age_seq$predicted_wage <- exp(age_seq$predicted_log_wage)/1e06
age_seq$lower_ci2 <- exp(age_seq$lower_ci)/1e06
age_seq$upper_ci2 <- exp(age_seq$upper_ci)/1e06

# Plot 1: Age-Log Wage Profile with Peak Age and Confidence Intervals
plot1 <- ggplot() + 
  geom_point(data = db_clean, aes(x = age, y = log_wage), alpha = 0.1, color = "blue") +  # Plot individual observations
  geom_line(data = age_seq, aes(x = age, y = predicted_log_wage), color = "red") +  # Predicted log wage curve
  geom_ribbon(data = age_seq, aes(x = age, ymin = lower_ci, ymax = upper_ci), alpha = 0.4, fill = "grey") +  # Confidence interval as a shaded region
  geom_vline(xintercept = peak_age, color = "green") +  # Peak age vertical line
  geom_vline(xintercept = ci_peak_age[1], color = "black", linetype = "dashed") +  # Lower bound of confidence interval
  geom_vline(xintercept = ci_peak_age[2], color = "black", linetype = "dashed") +  # Upper bound of confidence interval
  labs(x = "Age", y = "Log Wage",
       caption = "Green line: Peak Age\nDashed lines: 95% Confidence Interval (Bootstrap)") +  # Add explanation to the plot
  theme_minimal()
ggsave("views/P1_age_log_wage_profile.pdf", plot1, width = 8, height = 6) # Save plot 1

# Filter data to include only wages less than 2 million for better visualization
db_clean_filtered <- db_clean %>% 
  filter(y_salary_m_scale < 2)

# Plot 2: Age-Wage Profile with Observations, Peak Age, and Confidence Intervals
plot2 <- ggplot() + 
  geom_point(data = db_clean_filtered, aes(x = age, y = y_salary_m_scale), alpha = 0.1, color = "blue") +  # Plot individual observations
  geom_point(alpha = 0.1, color = "blue") +  # Plot individual observations
  geom_ribbon(data = age_seq, aes(x = age, ymin = lower_ci2, ymax = upper_ci2), alpha = 0.4, fill = "grey") +  # Confidence interval as a shaded region
  geom_line(data = age_seq, aes(x = age, y = predicted_wage), color = "red") +  # Predicted wage curve
  geom_vline(xintercept = peak_age, color = "green") +  # Peak age vertical line
  geom_vline(xintercept = ci_peak_age[1], color = "black", linetype = "dashed") +  # Lower bound of confidence interval
  geom_vline(xintercept = ci_peak_age[2], color = "black", linetype = "dashed") +  # Upper bound of confidence interval
  labs(x = "Age", y = "Wage",
       caption = "Green line: Peak Age\nDashed lines: 95% Confidence Interval (Bootstrap)\nFiltered for wages < 2 million") +  # Add explanation to the plot
  theme_minimal()
ggsave("views/P2_age_wage_profile.pdf", plot2, width = 8, height = 6) # Save plot 2

# Plot 3: Predicted Wage Profile by Age
plot3 <- ggplot() + 
  geom_line(data = age_seq, aes(x = age, y = predicted_wage), color = "blue", size = 1) +  # Predicted wage curve
  geom_ribbon(data = age_seq, aes(x = age, ymin = lower_ci2, ymax = upper_ci2), alpha = 0.4, fill = "grey") +  # Confidence interval as a shaded region
  labs(x = "Age", y = "Predicted Wage") +  # Add explanation to the plot
  theme_minimal()
ggsave("views/P3_age_wage_profile2.pdf", plot3, width = 8, height = 6) # Save plot 3




# 4. GENDER EARNINGS GAP ====================================================

# A) MODEL ------------------------------------------------------------------

# Create female = 1 if not male, otherwise 0
db_clean <- db_clean %>%
  mutate(female = if_else(sex == 1, 0, 1))  

# Model: log(w) = β1 + β2*Female + u
form_2 <- log_wage ~ female
model_2 <- lm(form_2, data = db_clean) # Fit the linear model
summary(model_2) # Display the model summary


# B) CONDITIONAL MODEL ------------------------------------------------------

# Model 
form_3 <- log_wage ~ female + relab + hoursWorkUsual + formal + sizeFirm
model_3 <- lm(form_3, data = db_clean)

# i) FWL
# Regress log wage on the control variables
resid_lw_controls <- lm(log_wage ~ relab + hoursWorkUsual + formal + sizeFirm, data = db_clean)$residuals

# Regress female on the control variables
resid_female_controls <- lm(female ~ relab + hoursWorkUsual + formal + sizeFirm, data = db_clean)$residuals

# Regress residuals of log wage on residuals of female
fwl_model <- lm(resid_lw_controls ~ resid_female_controls)
summary(fwl_model) # Display the model summary

# The coefficient and standard error from the FWL model
fwl_coef <- coef(fwl_model)[2]  
fwl_se <- summary(fwl_model)$coefficients[2, "Std. Error"]

# ii) FWL with Bootstrap
boot_fwl_fn <- function(data, index) {
  data_sample <- data[index, ]
  
  # Step 1: Regress log wage on controls
  resid_lw_controls <- lm(log_wage ~ relab + hoursWorkUsual + formal + sizeFirm, data = data_sample)$residuals
  
  # Step 2: Regress female on controls
  resid_female_controls <- lm(female ~ relab + hoursWorkUsual + formal + sizeFirm, data = data_sample)$residuals
  
  # Step 3: Regress residuals of log wage on residuals of female
  fwl_model <- lm(resid_lw_controls ~ resid_female_controls)
  
  return(coef(fwl_model))
}

# Perform bootstrap with 1000 replications
bootstrap_results <- boot(db_clean, boot_fwl_fn, R = 1000)

# Calculate standard errors from bootstrap
bootstrap_se <- apply(bootstrap_results$t, 2, sd)

# Prepare stargazer for LaTeX export
stargazer(model_2, model_3, fwl_model, 
          type = "text", 
          se = list(NULL, NULL, c(fwl_se, bootstrap_se)),  # Add SE for FWL with bootstrap
          title = "Gender Wage Gap: Simple, Conditional, and FWL Models", 
          dep.var.labels = "Log Wage",
          covariate.labels = c("Female"),  # Only show 'Female'
          omit = c("relab", "hoursWorkUsual", "formal", "sizeFirm"),  # Omit control variables
          omit.stat = c("f", "ser", "adj.rsq"),  # Omit F-statistic and standard error of the regression
          out = "views/gender_wage_gap_models.tex")

# Create the LaTeX table manually using cat() for custom formatting
cat("
\\begin{table}[ht]
\\centering
\\begin{tabular}{l c}
\\hline
 & Statistic \\\\
\\hline
Coefficient & ", round(fwl_coef, 5), " \\\\
Standard Error & ", round(fwl_se, 5), " \\\\
Standard Error (Bootstrap) & ", round(bootstrap_se, 5), " \\\\
\\hline
\\end{tabular}
\\caption{FWL Coefficient and Standard Errors Comparison}
\\label{tab:fwl_comparison}
\\end{table}
", file = "views/fwl_se_comparison.tex")

# C) PLOT -------------------------------------------------------------------

# Separate data by gender
db_clean_male <- db_clean %>% filter(female == 0)
db_clean_female <- db_clean %>% filter(female == 1)

# Model for males: log(w) = β1 + β2*Age + β3*Age^2 + u
form_male <- log_wage ~ age + I(age^2)
model_male <- lm(form_male, data = db_clean_male)

# Model for females: log(w) = β1 + β2*Age + β3*Age^2 + u
form_female <- log_wage ~ age + I(age^2)
model_female <- lm(form_female, data = db_clean_female)

# Calculate peak age for males and females
peak_age_male <- -coef(model_male)["age"] / (2 * coef(model_male)["I(age^2)"])
peak_age_female <- -coef(model_female)["age"] / (2 * coef(model_female)["I(age^2)"])

# Bootstrap for confidence intervals for males
boot_fn_male <- function(data, index) {
  model <- lm(form_male, data = data[index, ])
  return(coef(model))
}
bootstrap_results_male <- boot(db_clean_male, boot_fn_male, R = 1000)
boot_peak_ages_male <- -bootstrap_results_male$t[, 2] / (2 * bootstrap_results_male$t[, 3])
ci_peak_age_male <- quantile(boot_peak_ages_male, probs = c(0.025, 0.975))

# Bootstrap for confidence intervals for females
boot_fn_female <- function(data, index) {
  model <- lm(form_female, data = data[index, ])
  return(coef(model))
}
bootstrap_results_female <- boot(db_clean_female, boot_fn_female, R = 1000)
boot_peak_ages_female <- -bootstrap_results_female$t[, 2] / (2 * bootstrap_results_female$t[, 3])
ci_peak_age_female <- quantile(boot_peak_ages_female, probs = c(0.025, 0.975))

# Generate LaTeX table for Peak Ages and Bootstrap Confidence Intervals
cat("
\\begin{table}[ht]
\\centering
\\begin{tabular}{lccc}
\\hline
Sex & Peak Age & 95\\% CI Lower & 95\\% CI Upper \\\\
\\hline
Male & ", round(peak_age_male, 2), " & ", round(ci_peak_age_male[1], 2), " & ", round(ci_peak_age_male[2], 2), " \\\\
Female & ", round(peak_age_female, 2), " & ", round(ci_peak_age_female[1], 2), " & ", round(ci_peak_age_female[2], 2), " \\\\
\\hline
\\end{tabular}
\\caption{Peak Age and 95\\% Bootstrap Confidence Intervals by Sex}
\\label{tab:peak_age}
\\end{table}
", file = "views/peak_age_table.tex")

# Generate a sequence of ages for predicting wage values for males and females
age_seq_male <- data.frame(age = seq(min(db_clean_male$age), max(db_clean_male$age), by = 1))
age_seq_female <- data.frame(age = seq(min(db_clean_female$age), max(db_clean_female$age), by = 1))

# Predict log wage for males and females
predicted_male <- predict(model_male, newdata = age_seq_male, interval = "confidence")
predicted_female <- predict(model_female, newdata = age_seq_female, interval = "confidence")

# Add predicted log wage and confidence intervals to the dataframe
age_seq_male$predicted_log_wage <- predicted_male[, "fit"]
age_seq_male$lower_ci <- predicted_male[, "lwr"]
age_seq_male$upper_ci <- predicted_male[, "upr"]
age_seq_female$predicted_log_wage <- predicted_female[, "fit"]
age_seq_female$lower_ci <- predicted_female[, "lwr"]
age_seq_female$upper_ci <- predicted_female[, "upr"]

# Convert predicted log wage back to regular wage in scale
age_seq_male$predicted_wage <- exp(age_seq_male$predicted_log_wage)/1e06
age_seq_male$lower_ci2 <- exp(age_seq_male$lower_ci)/1e06
age_seq_male$upper_ci2 <- exp(age_seq_male$upper_ci)/1e06
age_seq_female$predicted_wage <- exp(age_seq_female$predicted_log_wage)/1e06
age_seq_female$lower_ci2 <- exp(age_seq_female$lower_ci)/1e06
age_seq_female$upper_ci2 <- exp(age_seq_female$upper_ci)/1e06

# Create a combined data frame for males and females with a gender identifier
db_clean_combined <- db_clean %>%
  mutate(gender = if_else(female == 1, "Female", "Male"))

# Generate a sequence of ages for predicting wage values for combined data
age_seq_male$gender <- "Male"
age_seq_female$gender <- "Female"
age_seq_combined <- bind_rows(age_seq_male, age_seq_female)

# Plot 4: Age-Log Wage Profile by sex
plot4 <-ggplot() + 
  geom_point(data = db_clean_combined, aes(x = age, y = log_wage, color = gender, shape = gender), alpha = 0.5, size = 0.5) +  # Plot individual observations
  geom_line(data = age_seq_combined, aes(x = age, y = predicted_log_wage, color = gender), size = 1) +  # Predicted log wage curve
  geom_ribbon(data = age_seq_combined, aes(x = age, ymin = lower_ci, ymax = upper_ci, color = gender), alpha = 0.4, fill = "grey") +  # Confidence interval as a shaded region
  geom_vline(xintercept = peak_age_male, color = "blue", linetype = "solid") +  # Peak age for males
  geom_vline(xintercept = ci_peak_age_male[1], color = "blue", linetype = "dashed") +  # Lower bound of CI for males
  geom_vline(xintercept = ci_peak_age_male[2], color = "blue", linetype = "dashed") +  # Upper bound of CI for males
  geom_vline(xintercept = peak_age_female, color = "red", linetype = "solid") +  # Peak age for females
  geom_vline(xintercept = ci_peak_age_female[1], color = "red", linetype = "dashed") +  # Lower bound of CI for females
  geom_vline(xintercept = ci_peak_age_female[2], color = "red", linetype = "dashed") +  # Upper bound of CI for females
  labs(x = "Age", y = "Log Wage", color = "Sex", shape = "Sex",
       caption = "Solid vertical lines: Peak Age\nDashed lines: 95% Confidence Interval (Bootstrap)") +
  theme_minimal()
ggsave("views/P4_age_logw_sex_profile.pdf", plot4, width = 8, height = 6) # Save plot 4

# Filter data to include only wages less than 2 million for better visualization
db_clean_combined_filtered <- db_clean_combined %>% 
  filter(y_salary_m_scale < 2)

# Plot 5: Age-Wage Profile by sex
plot5 <- ggplot() + 
  geom_point(data = db_clean_combined_filtered, aes(x = age, y = y_salary_m_scale, color = gender, shape = gender), alpha = 0.5, size = 0.5) +  # Plot individual observations
  geom_line(data = age_seq_combined, aes(x = age, y = predicted_wage, color = gender), size = 1) +  # Predicted log wage curve
  geom_ribbon(data = age_seq_combined, aes(x = age, ymin = lower_ci2, ymax = upper_ci2, color = gender), alpha = 0.4, fill = "grey") +  # Confidence interval as a shaded region
  geom_line(data = age_seq_combined, aes(x = age, y = predicted_wage, color = gender), size = 1) +  # Predicted wage curve
  geom_vline(xintercept = peak_age_male, color = "blue", linetype = "solid") +  # Peak age for males
  geom_vline(xintercept = ci_peak_age_male[1], color = "blue", linetype = "dashed") +  # Lower bound of CI for males
  geom_vline(xintercept = ci_peak_age_male[2], color = "blue", linetype = "dashed") +  # Upper bound of CI for males
  geom_vline(xintercept = peak_age_female, color = "red", linetype = "solid") +  # Peak age for females
  geom_vline(xintercept = ci_peak_age_female[1], color = "red", linetype = "dashed") +  # Lower bound of CI for females
  geom_vline(xintercept = ci_peak_age_female[2], color = "red", linetype = "dashed") +  # Upper bound of CI for females
  labs(x = "Age", y = "Wage", color = "Sex", shape = "Sex", 
       caption = "Solid vertical lines: Peak Age\nDashed lines: 95% Confidence Interval (Bootstrap)\nFiltered for wages < 2 million") +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas for better readability
  theme_minimal()
ggsave("views/P5_age_wage_sex_profile.pdf", plot5, width = 8, height = 6) # Save plot 5

# Plot 6: Combined Age-Wage Profile for Males and Females
plot6 <- ggplot() +
  geom_line(data = age_seq_male, aes(x = age, y = predicted_wage, color = "Male"), size = 1) +  # Males
  geom_line(data = age_seq_female, aes(x = age, y = predicted_wage, color = "Female"), size = 1) +  # Females
  geom_ribbon(data = age_seq_combined, aes(x = age, ymin = lower_ci2, ymax = upper_ci2, color = gender), alpha = 0.4, fill = "grey") +  # Confidence interval as a shaded region
  labs(x = "Age", y = "Predicted Wage", color = "Sex") +  # Add labels for the legend
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +  # Set colors for the legend
  scale_linetype_manual(values = c("Male" = "solid", "Female" = "dashed")) +  # Set line types for the legend
  theme_minimal()
ggsave("views/P6_age_wage_sex_profile.pdf", plot6, width = 8, height = 6) # Save plot 6



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

# Plot 7: Visualize the distribution of prediction errors (histogram)
plot7 <- ggplot(data = testing, aes(x = prediction_errors)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  labs(x = "Prediction Error", y = "Frequency") +
  theme_minimal()
ggsave("views/P7_prediction_errors_hist.pdf", plot7, width = 8, height = 6) # Save plot 6

# plot 8: Boxplot to visualize outliers
plot8 <- ggplot(data = testing, aes(y = prediction_errors)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(y = "Prediction Error") +
  theme_minimal()
ggsave("views/P8_prediction_errors_box.pdf", plot8, width = 8, height = 6) # Save plot 6


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
