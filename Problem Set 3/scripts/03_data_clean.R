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


