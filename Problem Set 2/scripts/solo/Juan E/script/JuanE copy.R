# Clean environment
gc()
rm(list = ls())
# Set working directory
setwd("~/Desktop/Repositiorios/BDML_Team1_2024/Problem Set 2")

# Libraries
library(pacman)
p_load(tidyverse, glmnet, caret, rpart, rpart.plot, ranger, Metrics, DMwR, MLmetrics)

# Data
test <- readRDS("scripts/solo/Juan F/test_clean.rds")
train <- readRDS("scripts/solo/Juan F/train_clean.rds")

#MODEL

#LOGIT

# Configuración del control del modelo con validación cruzada y SMOTE
ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = TRUE,  # Necesario para obtener las probabilidades
                     summaryFunction = prSummary,  # Usa precision, recall y F
                     savePredictions = TRUE)  # 

# Entrenar el modelo logit
model_logit <- train(poor ~ ., 
                     data = train,
                     method = "glm", 
                     family = "binomial",
                     metric = "F",  # F1-score como métrica
                     trControl = ctrl)
model_logit

predictSample <- test   %>% 
  mutate(poor = predict(model_logit, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,poor)

head(predictSample)


predictSample<- predictSample %>% 
  mutate(poor=ifelse(poor=="Yes",1,0)) %>% 
  select(id,poor)
head(predictSample)  


name<- paste0("scripts/solo/Juan E/submissions/LOGIT.csv") 
write.csv(predictSample,name, row.names = FALSE)


#Random Forest

p_load(ranger)


ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = TRUE,  # Necesario para obtener las probabilidades
                     summaryFunction = prSummary,  # Usa precision, recall y F
                     savePredictions = TRUE,
                     sampling = "smote")  # Aplicar undersampling/smote

forest <- train(poor ~ ., 
                data = train, 
                method = "ranger",  
                trControl = ctrl,
                metric = "F",
                num.trees = 100,  
                tuneGrid = expand.grid(mtry = 4, splitrule = "gini", min.node.size = 30),
                importance = "impurity")

forest

predictSample2 <- test   %>% 
  mutate(poor = predict(forest, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,poor)

head(predictSample)


predictSample2<- predictSample2 %>% 
  mutate(poor=ifelse(poor=="Yes",1,0)) %>% 
  select(id,poor)
head(predictSample2)  


name<- paste0("scripts/solo/Juan E/submissions/RF_trees_100_mtry_4_minnode_30.csv") 
write.csv(predictSample2,name, row.names = FALSE)
