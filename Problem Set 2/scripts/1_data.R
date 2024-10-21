# ========================================================================== #
# Problem Set 1                                                              #
# Big Data and Machine Learning - 202402                                     #
#                                                                            #
# Script: Data                                                               #
#                                                                            #
# Team 1: - Sergio Sandoval                                                  #
#         - María Fernanda Blanco                                            #
#         - Juan Fernando Barberi                                            #
#         - Juan Gutierrez                                                   #
# ========================================================================== #

# ENVIRONMENT SETUP AND DATA UPLOADING =========================================

# Set working directory
setwd("/Users/sergiosandovalcamargo/Documents/BDML/Repositorio P. Sets/BDML_Team1_2024/Problem Set 2")

# Clean the environment
rm(list = ls())

# Seed
set.seed(202028276)

# Load necessary libraries using pacman
library(pacman)
p_load(tidyverse) # Tidy-data

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



# HOUSEHOLD DATA ===============================================================

# Columns and rows
length(train_hogares) # 17 vars 
nrow(train_hogares)   # 164,960 obs

length(test_hogares) # 16 vars 
nrow(test_hogares)   # 66,168 obs

# Missing values
colSums(is.na(train_hogares))
colSums(is.na(test_hogares))

# Variables
  # 0. id
  # 0. poor: 1[poor]
  # 1. dom: city or urban/rural classification
  # 2. p_room: persons per room
  # 3. kind: type of housing ownership (1-6)
  # 4. rent: rent price (if missing, it takes P5130 - hypothetical asked rent price)
  # 5. num_per: # people
  # 6. num_per_u: # people in the expenditure unit
  # 7. pl: poverty line
  # 8. fex_c: expansion factor
  # 9. fex_dpto: expansion factor (departamento)

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
           fex_c = Fex_c,
           fex_dpto = Fex_dpto) %>% 
    select(id, dom, p_room, kind, rent, num_per, num_per_u, pl, fex_c, fex_dpto)
}



# INDIVIDUAL DATA ==============================================================

# Columns and rows
length(train_personas) # 63 vars 
nrow(train_personas)   # 543,109 obs

length(test_personas) # 63 vars 
nrow(test_personas)   # 219,644 obs

# Missing values
colSums(is.na(train_personas))
colSums(is.na(test_personas))

# Variables
  # 0. id
  # 1. sex: 1[female]
  # 2. age
  # 3. minor: 1[age<15]
  # 4. old: 1[age>60]
  # 5. s_sec: 1[social security]
  # 6. educ: max ecuation level (1-6)
  # 7. head: 1[household head]
  # 8. subs: # subsidies
  # 9. unemp: 1[unemployed]
  # 10. inact: 1[inactive]
  # 11. fex_c_p: individual expansion factor

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



# INDIVIDUAL DATA: HOUSEHOLD HEAD ==============================================

# Variables
  # 0. id
  # 1. head_female: 1[female]
  # 2. head_age: age
  # 3. head_old: 1[age>60]
  # 4. head_s_sec: 1[social security]
  # 5. head_educ: max ecuation level (1-6)
  # 6. head_unemp: 1[unemployed]
  # 7. head_inact: 1[inactive]
  # 8. head_sub: 1[subsidies > 0]

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



# MERGED DATA ==================================================================

# Variables
  # 0. id
  # 1. num_female: # females
  # 2. mean_age: age's average
  # 3. num_minor: # minors
  # 4. num_old: # olds
  # 5. num_s_sec: # social security
  # 6. max_educ: max ecuation level (1-6)
  # 7. head: 1[household head]
  # 8. subs: # subsidies
  # 9. num_unemp: # unemployed
  # 10. inact: # inactives
  # 11. fex_c_p: individual expansion factor

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


# FINAL DATA ===================================================================

# Final Variables
  # 0. id
  # 0. poor: 1[poor]
  # 1. dom: city or urban/rural classification
  # 2. p_room: persons per room
  # 3. kind: type of housing ownership (1-6)
  # 4. rent: rent price (if missing, it takes P5130 - hypothetical asked rent price)
  # 5. num_per: # people
  # 6. num_per_u: # people in the expenditure unit
  # 7. pl: poverty line
  # 8. fex_c: expansion factor
  # 9. fex_dpto: expansion factor (departamento)
  # 10. num_female: # females
  # 11. mean_age: age's average
  # 12. num_minor: # minors
  # 13. num_old: # olds
  # 14. num_s_sec: # social security
  # 14. max_educ: max ecuation level (1-6)
  # 15. head: 1[household head]
  # 16. subs: # subsidies
  # 17. num_unemp: # unemployed
  # 18. num_inact: # inactives
  # 19. fex_c_p: individual expansion factor
  # 20. head_female: 1[female]
  # 21. head_age: age
  # 22. head_old: 1[age>60]
  # 23. head_s_sec: 1[social security]
  # 24. head_educ: max ecuation level (1-6)
  # 25. head_unemp: 1[unemployed]
  # 26. head_inact: 1[inactive]
  # 27. head_sub: 1[subsidies > 0]
  # 28. prop_female: # females as a proportion
  # 29. prop_minor: minors as a proportion
  # 30. prop_old: old as a proportion
  # 31. prop_s_sec: social security as a proportion
  # 32. prop_subs: 1[subsidy] as a proportion
  # 33. prop_unemp: unemployed as a proportion
  # 34. prop_inact: inactives as a proportion

# Final data: household data + merge head data + merged group individual data 
final_data <- function(data_hogares, data_personas){
  hogares_d <- hogares(data_hogares)
  personas_d <- personas(data_personas)
  
  final <- hogares_d %>%
    left_join(head(personas_d), by = "id") %>%
    left_join(personas_hogares(personas_d), by = "id")
  
  final <- final %>% 
    mutate(prop_female = num_female / num_per,
           prop_minor = num_minor / num_per,
           prop_old = num_old / num_per,
           prop_s_sec = num_s_sec / num_per,
           prop_subs = num_subs / num_per,
           prop_unemp = num_unemp / num_per,
           prop_inact = num_inact / num_per) 
  
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

# Columns and rows
length(train) # 27 vars 
nrow(train)   # 164,190 obs

length(test) # 27 vars 
nrow(test)   # 66,168 obs

# Missing values
colSums(is.na(train))
colSums(is.na(test))

# Save data
saveRDS(train, file = "stores/data/work/train_clean.rds")
saveRDS(test, file = "stores/data/work/test_clean.rds")
