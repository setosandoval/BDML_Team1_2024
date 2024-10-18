# Clean environment
gc()
rm(list = ls())
# Set working directory
setwd("~/Desktop/Repositiorios/BDML_Team1_2024/Problem Set 2")

# Libraries
library(pacman)
p_load(tidyverse, glmnet, caret, rpart, rpart.plot, ranger, Metrics, DMwR, MLmetrics)

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
  mutate(
    urban = ifelse(Clase == 1, 1, 0),
    urban = factor(urban, levels = c(0, 1), labels = c("Rural", "Urban")),
    room_p = ifelse(P5010 > 0, Nper / P5010, NA),
    rent = ifelse(!is.na(P5130),P5130,P5140),
    kind = case_when(
      P5090 %in% c(1, 2) ~ 1, 
      P5090 %in% c(3, 4) ~ 2,
      P5090 %in% c(5, 6) ~ 3
    ),
    kind = factor(kind, levels = c(1, 2, 3), labels = c("Owned", "Rented", "No Property"))
  ) %>%
  rename(num_per = Nper, num_per_u = Npersug, lp = Lp, fex_c = Fex_c) %>%
  select(Pobre, id, urban, room_p, num_per, lp, rent)

train_hogares <- subset(train_hogares,rent >1000)

# MODIFICACION DE VARIABLES

train_personas$P6090[is.na(train_personas$P6090)] <- 0
test_personas$P6090[is.na(test_personas$P6090)] <- 0

# Agrupación por hogar con base en train_personas
info_personas <- train_personas %>%
  group_by(id) %>%
  summarise(num_female = sum(as.numeric(P6020 == 2)),
            max_educ = max(as.numeric(P6210)),
            salud = mean(as.numeric(P6090)),
            num_emp = sum(as.numeric(P6240 == 1))) %>%
  select(id, num_female, max_educ, num_emp)

# Unir las bases de datos
train_hogares <- train_hogares %>%
  left_join(info_personas, by = "id") %>%
  mutate(max_educ = factor(max_educ, levels = c(1:6), labels = c('None', 'Preschool', 'Primary', 'Secondary', 'High School', 'University')))


# Limpieza y transformación de los datos

test_hogares <- test_hogares %>%
  mutate(urban = ifelse(Clase == 1, 1, 0),
         urban = factor(urban, levels = c(0, 1), labels = c("Rural", "Urban")),
         room_p = ifelse(P5010 > 0, Nper / P5010, NA),
         rent = ifelse(!is.na(P5130),P5130,P5140),
         kind = case_when(
           P5090 %in% c(1, 2) ~ 1, 
           P5090 %in% c(3, 4) ~ 2,
           P5090 %in% c(5, 6) ~ 3
         ),
         kind = factor(kind, levels = c(1, 2, 3), labels = c("Owned", "Rented", "No Property"))
  ) %>%
  rename(num_per = Nper, num_per_u = Npersug, lp = Lp, fex_c = Fex_c) %>%
  select(id, urban, room_p, num_per, lp, rent)

test_hogares <- subset(test_hogares,rent >1000)

# Agrupación por hogar con base en train_personas
info_personas2 <- test_personas %>%
  group_by(id) %>%
  summarise(num_female = sum(as.numeric(P6020 == 2)),
            max_educ = max(as.numeric(P6210)),
            salud = mean(as.numeric(P6090)),
            num_emp = sum(as.numeric(P6240 == 1))) %>%
  select(id, num_female, max_educ, num_emp)

# Unir las bases de datos
test_hogares <- test_hogares %>%
  left_join(info_personas2, by = "id") %>%
  mutate(max_educ = factor(max_educ, levels = c(1:6), labels = c('None', 'Preschool', 'Primary', 'Secondary', 'High School', 'University')))

rm(info_personas, info_personas2, train_personas, test_personas)

train_hogares$num_emp[is.na(train_hogares$num_emp)] <- 0
test_hogares$num_emp[is.na(test_hogares$num_emp)] <- 0

sapply(train_hogares, class)
train_hogares <- train_hogares %>%
  select(-id) %>%
  mutate(Pobre = factor(Pobre,levels=c(0,1), labels=c("No","Yes")))

train_hogares <- na.omit(train_hogares)
test_hogares <- na.omit(test_hogares)
####### MODELOS



# Configuración del control del modelo con validación cruzada y F1-score
ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = TRUE,  # Necesario para obtener las probabilidades
                     summaryFunction = prSummary,  # Utilizar prSummary para precisión, recall y F1
                     savePredictions = TRUE)

# Ajustar el modelo Elastic Net con F1-score
set.seed(123)

model1 <- train(Pobre ~ .,
                data = train_hogares,
                method = "glmnet",      # Elastic Net
                family = "binomial",    # Regresión logística
                metric = "F1",          # Usar F1 como métrica de evaluación
                trControl = ctrl,
                tuneGrid = expand.grid(
                  alpha = seq(0, 1, by = 0.25),  # Alpha values for Elastic Net
                  lambda = 10^seq(-1, -3, length = 10)  # Lambda values
                )
)

# Mostrar los resultados del modelo
model1

predictSample <- test_hogares   %>% 
  mutate(Pobre = predict(model1, newdata = test_hogares, type = "raw")    ## predicted class labels
  )  %>% select(id,Pobre)

head(predictSample)


predictSample<- predictSample %>% 
  mutate(Pobre=ifelse(Pobre=="Yes",1,0)) %>% 
  select(id,Pobre)
head(predictSample)  

name<- paste0("scripts/solo/Juan E/submissions/","EN_lambda_", "", "_alpha_" , "", ".csv") 

write.csv(predictSample,name, row.names = FALSE)
