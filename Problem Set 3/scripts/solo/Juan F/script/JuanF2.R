# Limpiar el entorno
rm(list = ls())

# Establecer el directorio de trabajo
setwd("C:/Users/User/Documents/RepBDML2/BDML_Team1_2024/Problem Set 2")

# Cargar las librerías necesarias
library(pacman)
p_load(tidyverse, glmnet, caret)  # Cargar librerías necesarias con pacman

# Cargar los datos de entrenamiento y prueba
test <- readRDS("scripts/solo/Juan F/test_clean.rds")
train <- readRDS("scripts/solo/Juan F/train_clean.rds")

# Configuración del control del modelo con validación cruzada, SMOTE y F1-score
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,           # Para obtener probabilidades de clase
  summaryFunction = prSummary, # Usar prSummary para obtener Precision, Recall y F1
  savePredictions = TRUE,      # Guardar predicciones para análisis posterior
  sampling = "smote",          # Aplicar SMOTE para balancear las clases
  verboseIter = TRUE           # Mostrar progreso durante el entrenamiento
)

# Ajustar el modelo Random Forest con restricciones en profundidad y SMOTE
set.seed(098063)

model3 <- train(
  poor ~ .,                    # Variable objetivo y predictoras
  data = train,                 # Conjunto de datos de entrenamiento
  method = "rf",                # Random Forest
  metric = "F",                # Usar F1 como métrica de evaluación
  trControl = ctrl,             # Control del modelo (validación cruzada y SMOTE)
  tuneGrid = expand.grid(
    mtry = c(2, 4, 6, 15)       # Ajustar diferentes valores de mtry (número de variables por split)
  ),
  ntree = 100,                  # Reducir el número de árboles
  nodesize = 15                 # Tamaño mínimo del nodo
)

# Mostrar los resultados del modelo
print(model3)




# Kaggle modelo 3:

predictSample <- test   %>% 
  mutate(pobre_lab = predict(model3, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

head(predictSample)


predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)  

name<- paste0("scripts/solo/Juan F/submisison/","RF_SMOTE_mtry", "15", ".csv") 

write.csv(predictSample,name, row.names = FALSE)



###Modelo 5 CART:

# Cargar la librería necesaria
library(caret)

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,           
  summaryFunction = prSummary, 
  savePredictions = TRUE,      
  verboseIter = TRUE,          
  sampling = "smote"           
)


set.seed(098063)

model9 <- train(
  poor ~ .,                   # Variable objetivo y predictoras
  data = train,                # Conjunto de datos de entrenamiento
  method = "rpart",            # Usar CART (árboles de decisión)
  metric = "F",               # Usar F1 como métrica de evaluación
  trControl = ctrl,            # Control del modelo (validación cruzada con SMOTE)
  tuneLength = 10,             # Ajustar el número de hiperparámetros a explorar
  parms = list(split = "information") # Ajustar el criterio de división a "information gain"
)

# Mostrar los resultados del modelo
print(model9)




# Kaggle modelo 9:

predictSample <- test   %>% 
  mutate(pobre_lab = predict(model9, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

head(predictSample)


predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)  

name<- paste0("scripts/solo/Juan F/submisison/","CART_cp", "00013", ".csv") 

write.csv(predictSample,name, row.names = FALSE)









