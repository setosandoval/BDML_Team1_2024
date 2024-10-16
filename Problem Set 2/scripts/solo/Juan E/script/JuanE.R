# Clean environment
rm(list = ls())

# Set working directory
setwd("~/Desktop/Repositiorios/BDML_Team1_2024/Problem Set 2")

# Libraries
library(pacman)
p_load(tidyverse, glmnet, caret, rpart, rpart.plot, ranger, Metrics)

# Data
train_hogares <- read.csv("stores/data/raw/train_hogares.csv")
test_hogares <- read.csv("stores/data/raw/test_hogares.csv")

train_personas <- read.csv("stores/data/raw/train_personas.csv")
test_personas <- read.csv("stores/data/raw/test_personas.csv")

# Find the intersection of the vars between train and test
common_hogares <- intersect(names(train_hogares), names(test_hogares))
common_hogares <- c(common_hogares, "Pobre")
common_personas <- intersect(names(train_personas), names(test_personas))

# Keep only vars between train and test
train_hogares <- train_hogares[, common_hogares]
train_personas <- train_personas[, common_personas]

# Limpieza y transformación de los datos
train_hogares <- train_hogares %>%
  mutate(poor = factor(Pobre, levels = c(0, 1), labels = c("No", "Yes")),
         urban = ifelse(Clase == 1, 1, 0),
         urban = factor(urban, levels = c(0, 1), labels = c("Rural", "Urban")),
         room_p = ifelse(P5010 > 0, Nper / P5010, NA),
         kind = case_when(
           P5090 %in% c(1, 2) ~ 1, 
           P5090 %in% c(3, 4) ~ 2,
           P5090 %in% c(5, 6) ~ 3
         ),
         kind = factor(kind, levels = c(1, 2, 3), labels = c("Owned", "Rented", "No Property"))
  ) %>%
  rename(num_per = Nper, num_per_u = Npersug, lp = Lp, fex_c = Fex_c) %>%
  select(id, poor, urban, room_p, kind, num_per, num_per_u, lp, fex_c)

# Agrupación por hogar en base a train_personas
info_personas <- train_personas %>%
  group_by(id) %>%
  summarise(num_female = sum(as.numeric(P6020 == 2)),
            max_educ = max(as.numeric(P6210)),
            num_unemp = sum(as.numeric(Oc == 0))) %>%
  select(id, num_female, max_educ, num_unemp)

# Unir las bases de datos
train_hogares <- train_hogares %>%
  left_join(info_personas, by = "id") %>%
  mutate(max_educ = factor(max_educ, levels = c(1:6), labels = c('None', 'Preschool', 'Primary', 'Secondary', 'High School', 'University')))


# Limpieza y transformación de los datos
test_hogares <- test_hogares %>%
  mutate(urban = ifelse(Clase == 1, 1, 0),
         urban = factor(urban, levels = c(0, 1), labels = c("Rural", "Urban")),
         room_p = ifelse(P5010 > 0, Nper / P5010, NA),
         kind = case_when(
           P5090 %in% c(1, 2) ~ 1, 
           P5090 %in% c(3, 4) ~ 2,
           P5090 %in% c(5, 6) ~ 3
         ),
         kind = factor(kind, levels = c(1, 2, 3), labels = c("Owned", "Rented", "No Property"))
  ) %>%
  rename(num_per = Nper, num_per_u = Npersug, lp = Lp, fex_c = Fex_c) %>%
  select(id, urban, room_p, kind, num_per, num_per_u, lp, fex_c)

# Agrupación por hogar en base a train_personas
info_personas2 <- test_personas %>%
  group_by(id) %>%
  summarise(num_female = sum(as.numeric(P6020 == 2)),
            max_educ = max(as.numeric(P6210)),
            num_unemp = sum(as.numeric(Oc == 0))) %>%
  select(id, num_female, max_educ, num_unemp)

# Unir las bases de datos
test_hogares <- test_hogares %>%
  left_join(info_personas2, by = "id") %>%
  mutate(max_educ = factor(max_educ, levels = c(1:6), labels = c('None', 'Preschool', 'Primary', 'Secondary', 'High School', 'University')))

rm(info_personas, info_personas2, train_personas, test_personas)



####### MODELOS




# Definir control de entrenamiento con validación cruzada de 10 folds
train_control <- trainControl(method = "cv", number = 10)

# Ajustar modelo con pesos de clase para equilibrar las clases
tree_model <- 

  
#### EXPORTAR
predictSample <- test_hogares   %>% 
  mutate(pobre = predict(tree_model, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre=="Yes",1,0)) %>% 
  select(id,pobre)

head(predictSample)

name<- paste0("scripts/solo/Juan E/submissions/NOMBREcsv") 
write.csv(predictSample,name, row.names = FALSE)









