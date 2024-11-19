# ========================================================================== #
# Problem Set 2                                                              #
# Big Data and Machine Learning - 202420                                     #
#                                                                            #
# Script: María Fernanda Blanco                                              #
# ========================================================================== #

# Clean environment
rm(list = ls())

# Set working directory
setwd("/Users/home/Library/Mobile Documents/com~apple~CloudDocs/Universidad/Semestre8/BigData/BDML_Team1_2024/Problem Set 2")

# Libraries
library(pacman)
p_load(tidyverse) # Tidy-data

# Data
train_hogares<-read.csv("stores/data/raw/train_hogares.csv")
test_hogares<-read.csv("stores/data/raw/test_hogares.csv")

train_personas<-read.csv("stores/data/raw/train_personas.csv")
test_personas<-read.csv("stores/data/raw/test_personas.csv")

# Find the intersection of the vars between the train and test 
common_hogares <- intersect(names(train_hogares), names(test_hogares))
common_hogares <- c(common_hogares, "Pobre")
common_personas <- intersect(names(train_personas), names(test_personas))

# Keep only vars between train and test
train_hogares <- train_hogares[, common_hogares]
train_personas<- train_personas[, common_personas]

colSums(is.na(test_hogares))
colSums(is.na(test_personas))


train_hogares<- train_hogares %>% 
  mutate(poor = factor(Pobre,levels=c(0,1),labels=c("No","Yes")),
         dom = factor(Dominio),
         urban = ifelse(Clase==1,1,0),
         urban = factor(urban,levels=c(0,1),labels=c("Rural","Urban")),
         room_p = ifelse(P5010 > 0, Nper / P5010, NA),
         kind = case_when(
           P5090 %in% c(1, 2) ~ 1,       # Group "Propia, totalmente pagada" and "Propia, la están pagando"
           P5090 %in% c(3, 4) ~ 2,       # Group "En arriendo o subarriendo" and "En usufructo"
           P5090 %in% c(5, 6) ~ 3 ),     # Group "Posesión sin título" and "Otra"
         kind = factor(kind, levels = c(1, 2, 3), labels = c("Owned", "Rented", "No Property"))) %>% 
  rename(num_per = Nper,
         num_per_u = Npersug,
         lp = Lp,
         fex_c = Fex_c) %>% 
  select(id, poor, urban, room_p, kind, num_per, num_per_u, lp, fex_c, dom)

colSums(is.na(train_hogares))


train_personas <- train_personas %>%
  mutate(sex = ifelse(P6020==2,1,0), 
         sex = factor(sex,levels=c(0,1),labels=c("Male","Female")),
         soc_sec = ifelse(is.na(P6090), 0, ifelse(P6090 == 1, 1, 0)),
         soc_sec = factor(soc_sec,levels=c(0,1),labels=c("No","Yes")),
         educ = ifelse(is.na(P6210), 1, ifelse(P6210==9,1,P6210)),
         educ = factor(educ,levels=c(1:6), labels=c('None', 'Preschool', 'Primary', 'Secondary', 'High School', 'University')),
         subs = ifelse(P6585s1 == 1, 1, 0) + 
           ifelse(P6585s2 == 1, 1, 0) + 
           ifelse(P6585s3 == 1, 1, 0) + 
           ifelse(P6585s4 == 1, 1, 0),
         subs = ifelse(is.na(subs), 0, subs),
         subs = ifelse(is.na(subs), 0, subs),
         unemp = ifelse(is.na(Oc),1,0),
         unact = ifelse(is.na(Ina),0,1)) %>% 
  rename(fex_c_per = Fex_c) %>% 
  select(id, sex, soc_sec, educ, subs, unemp, unact, fex_c_per)

colSums(is.na(train_personas))

# Agrupar por hogar y calcular la proporción de mujeres
info_personas <- train_personas %>%
  group_by(id) %>%
  summarise(num_female = sum(as.numeric(sex)),
            num_socsec = sum(as.numeric(soc_sec)),
            max_educ = max(as.numeric(educ)),
            subs = sum(subs),
            num_unemp = sum(unemp),
            num_unact = sum(unact),
            sum_fex_cper = sum (fex_c_per)) %>% 
  select(id, num_female, num_socsec, max_educ, subs, num_unemp, num_unact, sum_fex_cper)


# Unir con la base de datos de hogares
train_hogares <- train_hogares %>%
  left_join(info_personas, by = "id") %>%
  mutate(max_educ = factor(max_educ,levels=c(1:6), labels=c('None', 'Preschool', 'Primary', 'Secondary', 'High School', 'University')))

colSums(is.na(train_hogares))


########## TEST, luego hacer funcion que lo haga por ambos
test_hogares <- test_hogares %>% 
  mutate(dom = factor(Dominio),
         urban = ifelse(Clase==1,1,0),
         urban = factor(urban,levels=c(0,1),labels=c("Rural","Urban")),
         room_p = ifelse(P5010 > 0, Nper / P5010, NA),
         kind = case_when(
           P5090 %in% c(1, 2) ~ 1,       # Group "Propia, totalmente pagada" and "Propia, la están pagando"
           P5090 %in% c(3, 4) ~ 2,       # Group "En arriendo o subarriendo" and "En usufructo"
           P5090 %in% c(5, 6) ~ 3 ),     # Group "Posesión sin título" and "Otra"
         kind = factor(kind, levels = c(1, 2, 3), labels = c("Owned", "Rented", "No Property"))) %>% 
  rename(num_per = Nper,
         num_per_u = Npersug,
         lp = Lp,
         fex_c = Fex_c) %>% 
  select(id, urban, room_p, kind, num_per, num_per_u, lp, fex_c, dom)
#quitar dom

colSums(is.na(test_hogares))


test_personas <- test_personas %>%
  mutate(sex = ifelse(P6020==2,1,0), 
         sex = factor(sex,levels=c(0,1),labels=c("Male","Female")),
         soc_sec = ifelse(is.na(P6090), 0, ifelse(P6090 == 1, 1, 0)),
         soc_sec = factor(soc_sec,levels=c(0,1),labels=c("No","Yes")),
         educ = ifelse(is.na(P6210), 1, ifelse(P6210==9,1,P6210)),
         educ = factor(educ,levels=c(1:6), labels=c('None', 'Preschool', 'Primary', 'Secondary', 'High School', 'University')),
         subs = ifelse(P6585s1 == 1, 1, 0) + 
           ifelse(P6585s2 == 1, 1, 0) + 
           ifelse(P6585s3 == 1, 1, 0) + 
           ifelse(P6585s4 == 1, 1, 0),
         subs = ifelse(is.na(subs), 0, subs),
         subs = ifelse(is.na(subs), 0, subs),
         unemp = ifelse(is.na(Oc),1,0),
         unact = ifelse(is.na(Ina),0,1)) %>% 
  rename(fex_c_per = Fex_c) %>% 
  select(id, sex, soc_sec, educ, subs, unemp, unact, fex_c_per)
# Falto Fex_c persona

colSums(is.na(test_personas))

# Agrupar por hogar y calcular la proporción de mujeres
info_personas2 <- test_personas %>%
  group_by(id) %>%
  summarise(num_female = sum(as.numeric(sex)),
            num_socsec = sum(as.numeric(soc_sec)),
            max_educ = max(as.numeric(educ)),
            subs = sum(subs),
            num_unemp = sum(unemp),
            num_unact = sum(unact),
            sum_fex_cper = sum (fex_c_per)) %>% 
  select(id, num_female, num_socsec, max_educ, subs, num_unemp, num_unact, sum_fex_cper)
#max educ podría ser factor

# Unir con la base de datos de hogares y var categóricas
test_hogares <- test_hogares %>%
  left_join(info_personas2, by = "id") %>%
  mutate(max_educ = factor(max_educ,levels=c(1:6), labels=c('None', 'Preschool', 'Primary', 'Secondary', 'High School', 'University')))

colSums(is.na(test_hogares))



######## Guardar #############
# Guardar el dataframe en formato .rds
train_hogares <- train_hogares  %>%
  select(-id)

saveRDS(train_hogares, file = "scripts/solo/Mafe/train.rds")
saveRDS(test_hogares, file = "scripts/solo/Mafe/test.rds")





