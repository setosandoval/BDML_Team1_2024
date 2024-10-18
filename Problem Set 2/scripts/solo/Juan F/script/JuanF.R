# ========================================================================== #
# Problem Set 2                                                              #
# Big Data and Machine Learning - 202420                                     #
#                                                                            #
# Script: Juan Fernando Barberi                                              #
# ========================================================================== #


# Clean environment
rm(list = ls())

# Set working directory
setwd("C:/Users/User/Documents/RepBDML2/BDML_Team1_2024")

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



###### Revisión de datos ######

# Keep only vars for train that is also in test 

common_personas <- intersect(names(train_personas), names(test_personas))
train_personas<- train_personas[, common_personas]




###Imputación: 
#(training_personas)

#Pension:
library(dplyr)

get_mode <- function(v) {
  uniqv <- unique(v[!is.na(v)])  # Excluir NA
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

modo_P6920 <- get_mode(train_personas$P6920)
print(paste("El modo de P6920 es:", modo_P6920))

train_personas <- train_personas %>%
  mutate(
    P6920 = ifelse(is.na(P6920), modo_P6920, P6920)
  )

summary(train_personas$P6920)



#Tiempo de ocupacion:
library(dplyr)
get_mode <- function(v) {
  uniqv <- unique(v[!is.na(v)])  # Excluir NA
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

modo_P6240 <- get_mode(train_personas$P6240)
print(paste("El modo de P6240 es:", modo_P6240))

train_personas <- train_personas %>%
  mutate(
    P6240 = ifelse(is.na(P6240), modo_P6240, P6240)
  )

summary(train_personas$P6240)


#Salud:
library(dplyr)
get_mode <- function(v) {
  uniqv <- unique(v[!is.na(v)])  # Excluir NA
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

modo_P6090 <- get_mode(train_personas$P6090)
print(paste("El modo de P6090 es:", modo_P6090))

train_personas <- train_personas %>%
  mutate(
    P6090 = ifelse(is.na(P6090), modo_P6090, P6090)
  )

summary(train_personas$P6090)


#(test_personas)

#Pension:
library(dplyr)

get_mode <- function(v) {
  uniqv <- unique(v[!is.na(v)])  # Excluir NA
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

modo_P6920 <- get_mode(test_personas$P6920)
print(paste("El modo de P6920 es:", modo_P6920))

test_personas <- test_personas %>%
  mutate(
    P6920 = ifelse(is.na(P6920), modo_P6920, P6920)
  )

summary(test_personas$P6920)



#Tiempo de ocupacion:
library(dplyr)
get_mode <- function(v) {
  uniqv <- unique(v[!is.na(v)])  # Excluir NA
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

modo_P6240 <- get_mode(test_personas$P6240)
print(paste("El modo de P6240 es:", modo_P6240))

test_personas <- test_personas %>%
  mutate(
    P6240 = ifelse(is.na(P6240), modo_P6240, P6240)
  )

summary(test_personas$P6240)


#Salud:
library(dplyr)
get_mode <- function(v) {
  uniqv <- unique(v[!is.na(v)])  # Excluir NA
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

modo_P6090 <- get_mode(test_personas$P6090)
print(paste("El modo de P6090 es:", modo_P6090))

test_personas <- test_personas %>%
  mutate(
    P6090 = ifelse(is.na(P6090), modo_P6090, P6090)
  )

summary(test_personas$P6090)





#### Personas ####
### Creación usando personas (train)
train_personas <- train_personas %>% 
  mutate(
    mujer = ifelse(P6020 == 2, 1, 0), 
    H_Head = ifelse(P6050 == 1, 1, 0), #Household head
    menor = ifelse(P6040 <= 12, 1, 0), # Menores
    EducLevel = ifelse(P6210 == 9, 0, P6210), # Reemplazar 9 con 0
    ocupado = ifelse(is.na(Oc), 0, 1),
    
    # Pension
    pension = case_when(
      P6920 == 1 ~ 1,  # Pensionado actualmente
      P6920 == 2 ~ 0,  # No está pensionado
      P6920 == 3 ~ 2   # Ya se pensionó (probablemente jubilado)
    ),
    
    # Salud
    salud = case_when(
      P6090 == 1 ~ 1,          # Afiliado, cotizante o beneficiario
      P6090 == 2 ~ 0,          # No afiliado
      P6090 == 9 | is.na(P6090) ~ 2 # Desconocido = 2
    ),
    
    # Subsidios: Alimentación, Transporte, Familiar, Educación
    subsidio_alimentacion = case_when(
      P6585s1 == 1 ~ 1,  # Recibió subsidio de alimentación
      P6585s1 == 2 ~ 0,  # No recibió subsidio de alimentación
      P6585s1 == 9 | is.na(P6585s1) ~ 2  # No sabe o NA
    ),
    
    subsidio_transporte = case_when(
      P6585s2 == 1 ~ 1,  # Recibió subsidio de transporte
      P6585s2 == 2 ~ 0,  # No recibió subsidio de transporte
      P6585s2 == 9 | is.na(P6585s2) ~ 2  # No sabe o NA
    ),
    
    subsidio_familiar = case_when(
      P6585s3 == 1 ~ 1,  # Recibió subsidio familiar
      P6585s3 == 2 ~ 0,  # No recibió subsidio familiar
      P6585s3 == 9 | is.na(P6585s3) ~ 2  # No sabe o NA
    ),
    
    subsidio_educacion = case_when(
      P6585s4 == 1 ~ 1,  # Recibió subsidio de educación
      P6585s4 == 2 ~ 0,  # No recibió subsidio de educación
      P6585s4 == 9 | is.na(P6585s4) ~ 2  # No sabe o NA
    )
  ) %>%
  rename(tiempo = P6240) %>%
  select(id, Orden, mujer, H_Head, menor, EducLevel, ocupado, pension, salud, 
         subsidio_alimentacion, subsidio_transporte, subsidio_familiar, subsidio_educacion, tiempo)

# Ver las primeras filas del dataset modificado
head(train_personas)




###Creación usando personas (test)

test_personas <- test_personas %>%  
  mutate(
    mujer = ifelse(P6020 == 2, 1, 0), 
    H_Head = ifelse(P6050 == 1, 1, 0), #Household head
    menor = ifelse(P6040 <= 12, 1, 0), # Menores
    EducLevel = ifelse(P6210 == 9, 0, P6210), # Reemplazar 9 con 0
    ocupado = ifelse(is.na(Oc), 0, 1),
    
    # Pension
    pension = case_when(
      P6920 == 1 ~ 1,  # Pensionado actualmente
      P6920 == 2 ~ 0,  # No está pensionado
      P6920 == 3 ~ 2   # Ya se pensionó (probablemente jubilado)
    ),
    
    # Salud
    salud = case_when(
      P6090 == 1 ~ 1,          # Afiliado, cotizante o beneficiario
      P6090 == 2 ~ 0,          # No afiliado
      P6090 == 9 | is.na(P6090) ~ 2 # Desconocido = 2
    ),
    
    # Subsidios: Alimentación, Transporte, Familiar, Educación
    subsidio_alimentacion = case_when(
      P6585s1 == 1 ~ 1,  # Recibió subsidio de alimentación
      P6585s1 == 2 ~ 0,  # No recibió subsidio de alimentación
      P6585s1 == 9 | is.na(P6585s1) ~ 2  # No sabe o NA
    ),
    
    subsidio_transporte = case_when(
      P6585s2 == 1 ~ 1,  # Recibió subsidio de transporte
      P6585s2 == 2 ~ 0,  # No recibió subsidio de transporte
      P6585s2 == 9 | is.na(P6585s2) ~ 2  # No sabe o NA
    ),
    
    subsidio_familiar = case_when(
      P6585s3 == 1 ~ 1,  # Recibió subsidio familiar
      P6585s3 == 2 ~ 0,  # No recibió subsidio familiar
      P6585s3 == 9 | is.na(P6585s3) ~ 2  # No sabe o NA
    ),
    
    subsidio_educacion = case_when(
      P6585s4 == 1 ~ 1,  # Recibió subsidio de educación
      P6585s4 == 2 ~ 0,  # No recibió subsidio de educación
      P6585s4 == 9 | is.na(P6585s4) ~ 2  # No sabe o NA
    )
  ) %>%
  rename(tiempo = P6240) %>%
  select(id, Orden, mujer, H_Head, menor, EducLevel, ocupado, pension, salud, 
         subsidio_alimentacion, subsidio_transporte, subsidio_familiar, subsidio_educacion, tiempo)

# Ver las primeras filas del dataset modificado
head(test_personas)
#####################################################################################################################


#### Uso de variables de personas para hogares ####


## Crear variables a nivel hogar (train)
train_personas_nivel_hogar <- train_personas %>% 
  group_by(id) %>% 
  summarize(
    nmujeres = sum(mujer, na.rm = TRUE),              # Número de mujeres en el hogar
    nmenores = sum(menor, na.rm = TRUE),              # Número de menores en el hogar
    maxEducLevel = max(EducLevel, na.rm = TRUE),      # Nivel educativo máximo en el hogar
    nocupados = sum(ocupado, na.rm = TRUE),           # Número de ocupados en el hogar
    
    # Resumir pensiones y salud a nivel hogar
    n_pensionados = sum(pension == 1, na.rm = TRUE),   # Número de pensionados en el hogar
    n_no_pensionados = sum(pension == 0, na.rm = TRUE),# Número de no pensionados en el hogar
    n_salud_afiliado = sum(salud == 1, na.rm = TRUE),  # Número de personas afiliadas a salud
    n_salud_no_afiliado = sum(salud == 0, na.rm = TRUE),# Número de personas no afiliadas a salud
    
    # Resumir el tiempo de ocupación a nivel hogar (trabajo, estudio, etc.)
    n_trabajando = sum(tiempo == 1, na.rm = TRUE),    # Número de personas trabajando
    n_estudiando = sum(tiempo == 3, na.rm = TRUE),    # Número de personas estudiando
    
    # Resumir subsidios a nivel hogar
    n_subsidio_alimentacion = sum(subsidio_alimentacion == 1, na.rm = TRUE),  # Número de personas con subsidio de alimentación
    n_subsidio_transporte = sum(subsidio_transporte == 1, na.rm = TRUE),      # Número de personas con subsidio de transporte
    n_subsidio_familiar = sum(subsidio_familiar == 1, na.rm = TRUE),          # Número de personas con subsidio familiar
    n_subsidio_educacion = sum(subsidio_educacion == 1, na.rm = TRUE)         # Número de personas con subsidio de educación
  )

# Crear las variables del jefe del hogar y unir con las nuevas variables
train_personas_hogar <- train_personas %>% 
  filter(H_Head == 1) %>% 
  select(id, mujer, EducLevel, ocupado, pension, salud, tiempo) %>% 
  rename(
    H_Head_mujer = mujer,
    H_Head_Educ_level = EducLevel,
    H_Head_ocupado = ocupado,
    H_Head_pension = pension,      # Agregar variable pensión del jefe del hogar
    H_Head_salud = salud,          # Agregar estado de salud del jefe del hogar
    H_Head_tiempo = tiempo         # Agregar ocupación del jefe del hogar
  ) %>% 
  left_join(train_personas_nivel_hogar, by = "id")

# Ver las primeras filas del dataset final
head(train_personas_hogar)




## Crear variables a nivel hogar (test)

test_personas_nivel_hogar <- test_personas %>% 
  group_by(id) %>% 
  summarize(
    nmujeres = sum(mujer, na.rm = TRUE),              # Número de mujeres en el hogar
    nmenores = sum(menor, na.rm = TRUE),              # Número de menores en el hogar
    maxEducLevel = max(EducLevel, na.rm = TRUE),      # Nivel educativo máximo en el hogar
    nocupados = sum(ocupado, na.rm = TRUE),           # Número de ocupados en el hogar
    
    # Resumir pensiones y salud a nivel hogar
    n_pensionados = sum(pension == 1, na.rm = TRUE),   # Número de pensionados en el hogar
    n_no_pensionados = sum(pension == 0, na.rm = TRUE),# Número de no pensionados en el hogar
    n_salud_afiliado = sum(salud == 1, na.rm = TRUE),  # Número de personas afiliadas a salud
    n_salud_no_afiliado = sum(salud == 0, na.rm = TRUE),# Número de personas no afiliadas a salud
    
    # Resumir el tiempo de ocupación a nivel hogar (trabajo, estudio, etc.)
    n_trabajando = sum(tiempo == 1, na.rm = TRUE),    # Número de personas trabajando
    n_estudiando = sum(tiempo == 3, na.rm = TRUE),    # Número de personas estudiando
    
    # Resumir subsidios a nivel hogar
    n_subsidio_alimentacion = sum(subsidio_alimentacion == 1, na.rm = TRUE),  # Número de personas con subsidio de alimentación
    n_subsidio_transporte = sum(subsidio_transporte == 1, na.rm = TRUE),      # Número de personas con subsidio de transporte
    n_subsidio_familiar = sum(subsidio_familiar == 1, na.rm = TRUE),          # Número de personas con subsidio familiar
    n_subsidio_educacion = sum(subsidio_educacion == 1, na.rm = TRUE)         # Número de personas con subsidio de educación
  )

# Crear las variables del jefe del hogar y unir con las nuevas variables
test_personas_hogar <- test_personas %>% 
  filter(H_Head == 1) %>% 
  select(id, mujer, EducLevel, ocupado, pension, salud, tiempo) %>% 
  rename(
    H_Head_mujer = mujer,
    H_Head_Educ_level = EducLevel,
    H_Head_ocupado = ocupado,
    H_Head_pension = pension,      # Agregar variable pensión del jefe del hogar
    H_Head_salud = salud,          # Agregar estado de salud del jefe del hogar
    H_Head_tiempo = tiempo         # Agregar ocupación del jefe del hogar
  ) %>% 
  left_join(test_personas_nivel_hogar, by = "id")

# Ver las primeras filas del dataset final
head(test_personas_hogar)



#### HOGARES ####

#Hacinamiento:
# (para train)
train_hogares <- train_hogares %>% 
  mutate(
    arrienda = ifelse(P5090 == 3, 1, 0),  # Si arrienda
    hacinamiento = Nper / P5010           # Calcular hacinamiento
  ) %>% 
  select(id, Dominio, arrienda, hacinamiento, Pobre)  # Seleccionar las variables de interés

# Ver las primeras filas del dataset
head(train_hogares)


# (para test)
test_hogares <- test_hogares %>% 
  mutate(
    arrienda = ifelse(P5090 == 3, 1, 0),  # Si arrienda
    hacinamiento = Nper / P5010           # Calcular hacinamiento
  ) %>% 
  select(id, Dominio, arrienda, hacinamiento)  # Seleccionar las variables de interés

# Ver las primeras filas del dataset
head(test_hogares)



#### UNIÓN PERSONAS - HOGAR ####


train<- train_hogares %>% 
  left_join(train_personas_hogar) %>% 
  select(-id) #no longer need id

test<- test_hogares %>% 
  left_join(test_personas_hogar)


## Base final:
#(train)
train <- train %>% 
  mutate(
    Pobre = factor(Pobre, levels = c(0, 1), labels = c("No", "Yes")),   # Convertir Pobre a factor con niveles válidos
    Dominio = factor(Dominio),                                          # Convertir Dominio a factor
    
    # Convertir H_Head_Educ_level a factor con etiquetas válidas
    H_Head_Educ_level = factor(H_Head_Educ_level, levels = c(0:6), 
                               labels = c("Ns", "Ninguno", "Preescolar", "Primaria", "Secundaria", "Media", "Universitaria")),
    
    # Convertir maxEducLevel a factor con las mismas etiquetas que H_Head_Educ_level
    maxEducLevel = factor(maxEducLevel, levels = c(0:6), 
                          labels = c("Ns", "Ninguno", "Preescolar", "Primaria", "Secundaria", "Media", "Universitaria")),
    
    # Convertir la variable de arriendo (arrienda) en factor
    arrienda = factor(arrienda, levels = c(0, 1), labels = c("No", "Yes"))
  )

# Ver las primeras filas para revisar el resultado
head(train)


#(test)

test <- test %>% 
  mutate(Dominio = factor(Dominio),
         H_Head_Educ_level = factor(H_Head_Educ_level, levels = c(0:6), 
                                    labels = c("Ns", "Ninguno", "Preescolar", "Primaria", "Secundaria", "Media", "Universitaria")),
         maxEducLevel = factor(maxEducLevel, levels = c(0:6), 
                               labels = c("Ns", "Ninguno", "Preescolar", "Primaria", "Secundaria", "Media", "Universitaria")),
         arrienda = factor(arrienda, levels = c(0, 1), labels = c("No", "Yes"))
  )

# Ver las primeras filas para revisar el resultado
head(test)



# Cargar la librería necesaria para prSummary
library(caret)

# Configuración del control del modelo con validación cruzada y F1-score
ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = TRUE,  # Necesario para obtener las probabilidades
                     summaryFunction = prSummary,  # Utilizar prSummary para precisión, recall y F1
                     savePredictions = TRUE)

# Ajustar el modelo Elastic Net con F1-score
set.seed(098063)

model1 <- train(Pobre ~ .,
                data = train,
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


# Kaggle modelo 1:

predictSample <- test   %>% 
  mutate(pobre_lab = predict(model1, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

head(predictSample)


predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)  

name<- paste0("EN_lambda_", "0001", "_alpha_" , "025", ".csv") 

write.csv(predictSample,name, row.names = FALSE)





# Logit -SMOTE- (Modelo 2)
# Cargar la librería adicional para SMOTE
p_load(DMwR)  # Si no tienes esta función, usa install.packages("DMwR")

# Configuración del control del modelo con validación cruzada y SMOTE
ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = TRUE,  # Necesario para obtener las probabilidades
                     summaryFunction = prSummary,  # Usa precision, recall y F1-score
                     savePredictions = TRUE,
                     sampling = "smote")  # Aplicar SMOTE para balancear las clases

# Entrenar el modelo logit con SMOTE
set.seed(098063)

model2 <- train(Pobre ~ .,  # Usar la variable Pobre como objetivo
                data = train,  # Conjunto de datos de entrenamiento
                method = "glm",  # Regresión logística
                family = "binomial",  # Especificar la familia binomial
                metric = "F",  # F1-score como métrica
                trControl = ctrl)

# Mostrar los resultados del modelo
model2

# Kaggle modelo 2: ARREGLAR

predictSample <- test   %>% 
  mutate(pobre_lab = predict(model1, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

head(predictSample)


predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)  

name<- paste0("LO_lambda_", "0001", "_alpha_" , "025", ".csv") 

write.csv(predictSample,name, row.names = FALSE)




# Random Forest (Modelo 3):

library(caret)

# Configuración del control del modelo con validación cruzada y F1-score
ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = TRUE,  # Para obtener las probabilidades de clase
                     summaryFunction = prSummary,  # Usar prSummary para obtener Precision, Recall y F1
                     savePredictions = TRUE)

# Ajustar el modelo Random Forest con restricciones en profundidad
set.seed(098063)

model3 <- train(Pobre ~ .,                # Variable objetivo y variables predictoras
                data = train,             # Conjunto de datos de entrenamiento
                method = "rf",            # Random Forest
                metric = "F",            # Usar F1 como métrica de evaluación
                trControl = ctrl,         # Control del modelo (validación cruzada)
                tuneGrid = expand.grid(
                  mtry = c(2, 4, 6, 15)       # Ajustar diferentes valores de mtry (número de variables por split)
                ),
                ntree = 100,              # Reducir el número de árboles
                nodesize = 15             # Tamaño mínimo del nodo
)

# Mostrar los resultados del modelo
model3

#Kaggle modelo 3:

predictSample <- test   %>% 
  mutate(pobre_lab = predict(model1, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

head(predictSample)


predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)  

name<- paste0("RF_", "6_", "mtry", ".csv") 

write.csv(predictSample,name, row.names = FALSE)
