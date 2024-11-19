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
p_load(tidyverse, # Tidy-data
       glmnet,     # Regularization algorithms. 
       caret,      # Predictive models
       Metrics,
       ranger,
       xgboost,
       MLmetrics)

# Data
train <- readRDS("scripts/solo/Mafe/train.rds")
test <- readRDS("scripts/solo/Mafe/test.rds")


############### ELASTIC NET
ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = TRUE,  # Necesario para obtener las probabilidades
                     summaryFunction = prSummary,  # Usa precision, recall y F
                     savePredictions = TRUE,
                     verboseIter = TRUE)

model1 <- train(poor~.,
                data = train,
                metric = "F",
                method = "glmnet",
                trControl = ctrl,
                family = "binomial",
                tuneGrid = expand.grid(
                  alpha = 0.5,  # Valor específico de alpha
                  lambda = 0.01  # Valor específico de lambda
                )
)

model1

# SUBMISSION
predictSample <- test   %>% 
  mutate(pobre = predict(model1, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre=="Yes",1,0)) %>% 
  select(id,pobre)

head(predictSample)

name<- paste0("scripts/solo/Mafe/submissions/EN_alpha_0.5_lambda_0.01.csv") 
write.csv(predictSample,name, row.names = FALSE)


########### LOGIT
model_logit <- train(poor ~., 
                     data = train,
                     method = "glm", 
                     family = "binomial",
                     metric = "F",  # F1-score como métrica
                     trControl = ctrl)

model_logit


# SUBMISSION
predictSample <- test   %>% 
  mutate(pobre = predict(model_logit, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre=="Yes",1,0)) %>% 
  select(id,pobre)

head(predictSample)

name<- paste0("scripts/solo/Mafe/submissions/Logit.csv") 
write.csv(predictSample,name, row.names = FALSE)


### LOGIT UNDERSMAPLING

# Configuración del control del modelo con validación cruzada y undersampling
ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = TRUE,  # Necesario para obtener las probabilidades
                     summaryFunction = prSummary,  # Usa precision, recall y F
                     savePredictions = TRUE,
                     sampling = "down")  # Aplicar undersampling

# Entrenar el modelo logit con undersampling
model_logit_under <- train(poor ~ ., 
                           data = train,
                           method = "glm", 
                           family = "binomial",
                           metric = "F",  # F1-score como métrica
                           trControl = ctrl)

model_logit_under

# SUBMISSION
predictSample <- test   %>% 
  mutate(pobre = predict(model_logit_under, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre=="Yes",1,0)) %>% 
  select(id,pobre)

head(predictSample)

name<- paste0("scripts/solo/Mafe/submissions/Logit_under_s.csv") 
write.csv(predictSample,name, row.names = FALSE)



########################  LOGIT SMOTE
ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = TRUE,  # Necesario para obtener las probabilidades
                     summaryFunction = prSummary,  # Usa precision, recall y F
                     savePredictions = TRUE,
                     sampling = "smote",
                     verboseIter = TRUE)  # Aplicar SMOTE

# Entrenar el modelo logit con SMOTE
model_logit_smote <- train(poor ~ ., 
                           data = train,
                           method = "glm", 
                           family = "binomial",
                           metric = "F",  # F1-score como métrica
                           trControl = ctrl)
model_logit_smote

# SUBMISSION
predictSample <- test   %>% 
  mutate(pobre = predict(model_logit_smote, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre=="Yes",1,0)) %>% 
  select(id,pobre)

head(predictSample)

name<- paste0("scripts/solo/Mafe/submissions/Logit_SMOTE.csv") 
write.csv(predictSample,name, row.names = FALSE)




#### Random forest
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
                tuneGrid = expand.grid(mtry = 4, splitrule = "gini", min.node.size = 25),
                importance = "impurity")

forest

# SUBMISSION
predictSample4 <- test   %>% 
  mutate(pobre = predict(forest, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre)

predictSample4<- predictSample4 %>% 
  mutate(pobre=ifelse(pobre=="Yes",1,0)) %>% 
  select(id,pobre)

head(predictSample4)

name<- paste0("scripts/solo/Mafe/submissions/RF_mtry_4_minN_25.csv") 
write.csv(predictSample4,name, row.names = FALSE)



