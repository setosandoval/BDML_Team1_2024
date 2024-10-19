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
p_load(tidyverse, 
       glmnet,     
       caret,     
       Metrics,
       ranger,
       xgboost,
       MLmetrics)

# Data
train <- readRDS("scripts/solo/Sergio/data/train_Sergio.rds")
test <- readRDS("scripts/solo/Sergio/data/test_Sergio.rds")


######## LOGIT SMOTE
ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = TRUE, 
                     summaryFunction = prSummary, 
                     savePredictions = TRUE,
                     sampling = "smote",
                     verboseIter = TRUE)  

model_logit_smote <- train(poor ~ ., 
                           data = train,
                           method = "glm", 
                           family = "binomial",
                           metric = "F", 
                           trControl = ctrl)
model_logit_smote

# Predecir probabilidades en el conjunto de entrenamiento
pred_probs <- predict(model_logit_smote, newdata = train, type = "prob")[, "Yes"]

# Función para ajustar el threshold
adjust_threshold <- function(threshold, true_labels, predicted_probs) {
  preds <- ifelse(predicted_probs >= threshold, "Yes", "No")
  confusion <- confusionMatrix(as.factor(preds), as.factor(true_labels))
  f1 <- confusion$byClass["F1"]
  return(f1)
}

# Buscar el threshold óptimo
thresholds <- seq(0.05, 0.95, by = 0.01)
f1_scores <- sapply(thresholds, adjust_threshold, true_labels = train$poor, predicted_probs = pred_probs)

# Encontrar el mejor threshold
optimal_threshold <- thresholds[which.max(f1_scores)]
print(paste("Optimal Threshold:", optimal_threshold))

# Aplicar el threshold óptimo
final_preds <- ifelse(pred_probs >= 0.3, "Yes", "No")

# SUBMISSION
predictSample <- test   %>% 
  mutate(pobre = predict(model_logit_smote, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre=="Yes",1,0)) %>% 
  select(id,pobre)

head(predictSample)

name<- paste0("scripts/solo/Sergio/submissions/Logit_SMOTE.csv") 
write.csv(predictSample,name, row.names = FALSE)




