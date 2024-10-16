# Clean environment
rm(list = ls())

# Set working directory
setwd("~/Desktop/Big Data/PS2")

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

# Separamos la variable dependiente
y_train <- train_hogares$poor
X_train <- train_hogares %>% select(-poor, -id)

# Eliminar filas con valores faltantes
complete_cases <- complete.cases(X_train)
X_train_clean <- X_train[complete_cases, ]
y_train_clean <- y_train[complete_cases]
