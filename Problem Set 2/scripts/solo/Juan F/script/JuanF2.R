
# Clean environment
rm(list = ls())

# Set working directory
setwd("C:/Users/User/Documents/RepBDML2/BDML_Team1_2024/Problem Set 2")

# Libraries
library(pacman)
p_load(tidyverse, # Tidy-data
       glmnet,     # Regularization algorithms. 
       caret)      # Predictive models

test <- readRDS("scripts/solo/Juan F/test_clean.rds")
train <- readRDS("scripts/solo/Juan F/train_clean.rds")

