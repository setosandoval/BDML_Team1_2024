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

# Load necessary libraries using pacman
library(pacman)
p_load(rio,        # For importing/exporting data
       tidyverse,  # For tidy-data operations
       skimr,      # For summarizing data
       visdat,     # For visualizing missing data
       stargazer,  # For creating tables/output to LaTeX
       boot,       # For bootstrap
       MASS)       # For regression calculations



# 2. DATA LOADING AND CLEANING ================================================

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

# Plot 2: Predicted Wage Profile by Age
plot2 <- ggplot(age_seq, aes(x = age, y = predicted_wage)) +
  geom_line(color = "blue", size = 1) +  # Plot predicted wage curve
  labs(x = "Age", y = "Wage", 
       caption = "Green line: Peak Age\nDashed lines: 95% Confidence Interval (Bootstrap)") +  # Add explanation to the plot
  theme_minimal()
ggsave("views/P2_age_wage_profile.pdf", plot2, width = 8, height = 6) # Save plot 2

# Filter data to include only wages less than 2 million for better visualization
db_clean_filtered <- db_clean %>% 
  filter(y_salary_m < 2e06)

# Plot 3: Age-Wage Profile with Observations, Peak Age, and Confidence Intervals
plot3 <- ggplot(db_clean_filtered, aes(x = age, y = y_salary_m)) +
  geom_point(alpha = 0.1, color = "blue") +  # Plot individual observations
  geom_line(data = age_seq, aes(x = age, y = predicted_wage), color = "red") +  # Predicted wage curve
  geom_vline(xintercept = peak_age, color = "green") +  # Peak age vertical line
  geom_vline(xintercept = ci_peak_age[1], color = "black", linetype = "dashed") +  # Lower bound of confidence interval
  geom_vline(xintercept = ci_peak_age[2], color = "black", linetype = "dashed") +  # Upper bound of confidence interval
  labs(x = "Age", y = "Wage",
       caption = "Green line: Peak Age\nDashed lines: 95% Confidence Interval (Bootstrap)") +  # Add explanation to the plot
  theme_minimal()
ggsave("views/P3_age_wage_profile2.pdf", plot3, width = 8, height = 6) # Save plot 3
