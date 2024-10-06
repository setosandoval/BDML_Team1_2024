# ========================================================================== #
# Problem Set 2                                                              #
# Big Data and Machine Learning - 202420                                     #
#                                                                            #
# Script: Sergio Sandoval                                                    #
# ========================================================================== #

# Clean environment
rm(list = ls())

# Set working directory
setwd("/Users/setosandoval/Desktop/BDML/Problem Sets/BDML_Team1_PS1/Problem Set 2")

# Libraries
library(pacman)
p_load(tidyverse, # Tidy-data
       glmnet,     # Regularization algorithms. 
       caret)      # Predictive models

# Data
train_hogares<-read.csv("stores/data/raw/train_hogares.csv")
test_hogares<-read.csv("stores/data/raw/test_hogares.csv")

train_personas<-read.csv("stores/data/raw/train_personas.csv")
test_personas<-read.csv("stores/data/raw/test_personas.csv")

# Find the intersection of the vars between the train and test 
common_hogares <- intersect(names(train_hogares), names(test_hogares))
common_personas <- intersect(names(train_personas), names(test_personas))

# Keep only vars for train that is also in test 
train_hogares <- train_hogares[, common_hogares]
train_personas<- train_personas[, common_personas]
