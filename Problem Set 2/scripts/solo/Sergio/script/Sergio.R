# ========================================================================== #
# Problem Set 2                                                              #
# Big Data and Machine Learning - 202420                                     #
#                                                                            #
# Script: Sergio Sandoval                                                    #
# ========================================================================== #

# Clean environment
rm(list = ls())

# Set working directory
setwd("/Users/sergiosandovalcamargo/Documents/BDML/Repositorio P. Sets/BDML_Team1_2024/Problem Set 2")

# Libraries
library(pacman)
p_load(tidyverse)  # Tidy-data

# Data 
train_hogares<-read.csv("stores/data/raw/train_hogares.csv")
test_hogares<-read.csv("stores/data/raw/test_hogares.csv")

train_personas<-read.csv("stores/data/raw/train_personas.csv")
test_personas<-read.csv("stores/data/raw/test_personas.csv")

# Find the intersection of the variables between the train and test 
common_hogares <- intersect(names(train_hogares), names(test_hogares))
common_hogares <- c(common_hogares, "Pobre")
common_personas <- intersect(names(train_personas), names(test_personas))

# Keep only the intersection of the variables
train_hogares <- train_hogares[, common_hogares]
train_personas<- train_personas[, common_personas]



# HOUSEHOLD 

hogares <- function(data_hogares,...){
  data_hogares <- data_hogares %>% 
    mutate(dom = factor(Dominio),
           p_room = Nper / P5010,
           kind = factor(P5090, levels=c(1:6), labels = c("Fully owned", "Owned being paid off", "Rented", "Usufruct", "Without title","Other")),
           kind = relevel(kind, ref = "Fully owned"),
           rent = ifelse(is.na(P5140), P5130, P5140)) %>% 
    rename(num_per = Nper,
           num_per_u = Npersug,
           pl = Lp,
           fex_c = Fex_c) %>% 
    select(id, dom, p_room, kind, rent, num_per, num_per_u, pl, fex_c)
}


# INDIVIDUAL DATA 

personas <- function(data_personas,...){
  data_personas <- data_personas %>%
    mutate(sex = ifelse(P6020 == 2, 1, 0), 
           minor = ifelse(P6040 < 15, 1, 0),
           old = ifelse(P6040 > 60, 1, 0),
           s_sec = ifelse(is.na(P6090) | P6090 != 1, 0, 1),
           educ = ifelse(is.na(P6210), 1, ifelse(P6210==9,1,P6210)),
           educ = factor(educ,levels=c(1:6), labels=c('None', 'Preschool', 'Primary', 'Secondary', 'High School', 'University')),
           educ = relevel(educ, ref = "None"),
           head = ifelse(P6050 == 1, 1, 0),
           subs = ifelse(P6585s1 == 1, 1, 0) + 
             ifelse(P6585s2 == 1, 1, 0) + 
             ifelse(P6585s3 == 1, 1, 0) + 
             ifelse(P6585s4 == 1, 1, 0),
           subs = ifelse(is.na(subs), 0, subs),
           unemp = ifelse(is.na(Oc),1,0),
           inact = ifelse(is.na(Ina),0,1)) %>%
    rename(age = P6040,
           fex_c_p = Fex_c)  %>%
    select(id, sex, age,  minor, old, s_sec, educ, head, subs, unemp, inact, fex_c_p)
}


# Household head

head <- function(data_personas,...){
  info_head <- data_personas %>% 
    filter(head==1) %>% 
    rename(head_female = sex,
           head_age = age,
           head_old = old,
           head_s_sec = s_sec,
           head_educ = educ,
           head_unemp = unemp,
           head_inact = inact) %>% 
    mutate(head_sub = ifelse(subs > 0, 1, 0)) %>% 
    select(id, head_female, head_age, head_old, head_s_sec, head_educ, head_unemp, head_inact, head_sub)
}


# MERGED 

personas_hogares <- function(data_personas,...){
  info_personas <- data_personas %>%
    group_by(id) %>%
    summarise(num_female = sum(sex),
              mean_age = mean(age),
              num_minor = sum(minor),
              num_old = sum(old),
              num_s_sec = sum(s_sec),
              max_educ = max(as.numeric(educ)),
              num_subs = sum(subs),
              num_unemp = sum(unemp),
              num_inact = sum(inact),
              sum_fex_c_p = sum (fex_c_p)) %>% 
    mutate(max_educ = factor(max_educ, levels = c(1:6), labels = c('None', 'Preschool', 'Primary', 'Secondary', 'High School', 'University')),
           max_educ = relevel(max_educ, ref = "None")) %>% 
    select(id,num_female, mean_age, num_minor, num_old, num_s_sec, max_educ, num_subs, num_unemp, num_inact, sum_fex_c_p)
}



# Final data:
final_data <- function(data_hogares, data_personas){
  hogares_d <- hogares(data_hogares)
  personas_d <- personas(data_personas)
  
  final <- hogares_d %>%
    left_join(head(personas_d), by = "id") %>%
    left_join(personas_hogares(personas_d), by = "id")
  
  return(final)
}

# Train data
train <- final_data(train_hogares, train_personas)

poor_var <- train_hogares %>% 
  mutate(poor = factor(Pobre,levels=c(0,1), labels=c("No","Yes"))) %>% 
  select(id, poor)

train <- train %>%
  left_join(poor_var, by = "id") %>%
  select(-id)

# Test Data
test <- final_data(test_hogares, test_personas)



