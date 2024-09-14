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

# Predict log wage based on the age sequence
age_seq$predicted_log_wage <- predict(model_1, newdata = age_seq)

# Convert predicted log wage back to regular wage
age_seq$predicted_wage <- exp(age_seq$predicted_log_wage)

# Plot 1: Age-Log Wage Profile with Peak Age and Confidence Intervals
plot1 <- ggplot(db_clean, aes(x = age, y = log_wage)) +
  geom_point(alpha = 0.1, color = "blue") +  # Plot individual observations
  geom_line(data = age_seq, aes(x = age, y = predicted_log_wage), color = "red") +  # Predicted log wage curve
  geom_vline(xintercept = peak_age, color = "green") +  # Peak age vertical line
  geom_vline(xintercept = ci_peak_age[1], color = "black", linetype = "dashed") +  # Lower bound of confidence interval
  geom_vline(xintercept = ci_peak_age[2], color = "black", linetype = "dashed") +  # Upper bound of confidence interval
  labs(x = "Age", y = "Log Wage",
       caption = "Green line: Peak Age\nDashed lines: 95% Confidence Interval (Bootstrap)") +  # Add explanation to the plot
  theme_minimal()
ggsave("views/P1_age_log_wage_profile.pdf", plot1, width = 8, height = 6) # Save plot 1

# Filter data to include only wages less than 2 million for better visualization
db_clean_filtered <- db_clean %>% 
  filter(y_salary_m < 2e06)

# Plot 2: Age-Wage Profile with Observations, Peak Age, and Confidence Intervals
plot2 <- ggplot(db_clean_filtered, aes(x = age, y = y_salary_m)) +
  geom_point(alpha = 0.1, color = "blue") +  # Plot individual observations
  geom_line(data = age_seq, aes(x = age, y = predicted_wage), color = "red") +  # Predicted wage curve
  geom_vline(xintercept = peak_age, color = "green") +  # Peak age vertical line
  geom_vline(xintercept = ci_peak_age[1], color = "black", linetype = "dashed") +  # Lower bound of confidence interval
  geom_vline(xintercept = ci_peak_age[2], color = "black", linetype = "dashed") +  # Upper bound of confidence interval
  labs(x = "Age", y = "Wage",
       caption = "Green line: Peak Age\nDashed lines: 95% Confidence Interval (Bootstrap)\nFiltered for wages < 2 million") +  # Add explanation to the plot
  theme_minimal()
ggsave("views/P2_age_wage_profile.pdf", plot2, width = 8, height = 6) # Save plot 2

# Plot 3: Predicted Wage Profile by Age
plot3 <- ggplot(age_seq, aes(x = age, y = predicted_wage)) +
  geom_line(color = "blue", size = 1) +  # Plot predicted wage curve
  labs(x = "Age", y = "Predicted Wage", 
       caption = "Green line: Peak Age\nDashed lines: 95% Confidence Interval (Bootstrap)") +  # Add explanation to the plot
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

# Export to LaTeX
stargazer(model_2, type = "latex",
          omit.stat = c("f", "adj.rsq", "ser"), # Only include observations and R-squared
          title = "Regression Results: Log Wage as a Function of Gender",
          dep.var.labels = "Log Wage",
          covariate.labels = c("Female (1 = Female)"), # Label the coefficients
          digits = 3,
          out = "views/reg_model2.tex") # Save to LaTeX file


# B) CONDITIONAL MODEL ------------------------------------------------------

# i) FWL
# Regress log wage on the control variables
resid_lw_controls <- lm(log_wage ~ relab + hoursWorkUsual + formal, data = db_clean)$residuals

# Regress female on the control variables
resid_female_controls <- lm(female ~ relab + hoursWorkUsual + formal, data = db_clean)$residuals

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
  resid_lw_controls <- lm(log_wage ~ relab + hoursWorkUsual + clase + formal, data = data_sample)$residuals
  
  # Step 2: Regress female on controls
  resid_female_controls <- lm(female ~ relab + hoursWorkUsual + clase + formal, data = data_sample)$residuals
  
  # Step 3: Regress residuals of log wage on residuals of female
  fwl_model <- lm(resid_lw_controls ~ resid_female_controls)
  
  return(coef(fwl_model))
}

# Perform bootstrap with 1000 replications
bootstrap_results <- boot(db_clean, boot_fwl_fn, R = 1000)

# Calculate standard errors from bootstrap
bootstrap_se <- apply(bootstrap_results$t, 2, sd)

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
age_seq_male$predicted_log_wage <- predict(model_male, newdata = age_seq_male)
age_seq_female$predicted_log_wage <- predict(model_female, newdata = age_seq_female)

# Convert predicted log wage back to regular wage for males and females
age_seq_male$predicted_wage <- exp(age_seq_male$predicted_log_wage)
age_seq_female$predicted_wage <- exp(age_seq_female$predicted_log_wage)

# Create a combined data frame for males and females with a gender identifier
db_clean_combined <- db_clean %>%
  mutate(gender = if_else(female == 1, "Female", "Male"))

# Generate a sequence of ages for predicting wage values for combined data
age_seq_male$gender <- "Male"
age_seq_female$gender <- "Female"
age_seq_combined <- bind_rows(age_seq_male, age_seq_female)

# Plot 4: Age-Log Wage Profile by sex
plot4 <- ggplot(db_clean_combined, aes(x = age, y = log_wage, color = gender, shape = gender)) +
  geom_point(alpha = 0.5, size = 0.5) +  # Plot individual observations with different shapes by gender
  geom_line(data = age_seq_combined, aes(x = age, y = predicted_log_wage, color = gender), size = 1) +  # Predicted log wage curve
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
  filter(y_salary_m < 2e06)

# Plot 5: Age-Wage Profile by sex
plot5 <-ggplot(db_clean_combined_filtered, aes(x = age, y = y_salary_m, color = gender, shape = gender)) +
  geom_point(alpha = 0.5, size = 0.5) +  # Plot individual observations with different shapes by gender
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
plot6 <- ggplot(db_clean_filtered, aes(x = age, y = y_salary_m)) +
  geom_line(data = age_seq_male, aes(x = age, y = predicted_wage, color = "Male", linetype = "Male"), size = 1) +  # Males
  geom_line(data = age_seq_female, aes(x = age, y = predicted_wage, color = "Female", linetype = "Female"), size = 1) +  # Females
  labs(x = "Age", y = "Predicted Wage", color = "Sex", linetype = "Sex") +  # Add labels for the legend
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +  # Set colors for the legend
  scale_linetype_manual(values = c("Male" = "solid", "Female" = "dashed")) +  # Set line types for the legend
  theme_minimal()
ggsave("views/P6_age_wage_sex_profile.pdf", plot6, width = 8, height = 6) # Save plot 6
