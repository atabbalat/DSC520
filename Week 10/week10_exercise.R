# Assignment: ASSIGNMENT Week 10
# Name: Tabbalat, Abed
# Date: 2021-05-21

library(corrplot)
library(caret)
library(plyr)
library(dplyr)
library(reshape2)
library(foreign)


setwd("C:/Users/abedt/OneDrive/Desktop/PyCharm Projects/DSC 520/Week 10")

thoraricSurgery_raw <- foreign::read.arff('ThoraricSurgery.arff')
thoraricSurgery <- thoraricSurgery_raw %>% 
  as.data.frame() %>% 
  mutate(id = row_number()) %>% 
  mutate_if(is.factor, ~ as.character(.)) %>% 
  mutate(DGN = mapvalues(x = DGN, from = c("DGN1", "DGN2", "DGN3", "DGN4", "DGN5", "DGN6", "DGN8"), to = c(1:6, 8)),
         PRE6 = mapvalues(x = PRE6, from = c('PRZ0', 'PRZ1', 'PRZ2'), to = c(0:2)),
         PRE14 = mapvalues(x = PRE14, from = c("OC11", "OC12", "OC13", "OC14"), to = c(11:14))
  )
thoraricSurgery <- dcast(mutate(
  melt(thoraricSurgery, id.var="id"),
  value = mapvalues(value, 
                    c("T","F"),
                    c(1, 0)
  )), id ~ variable) 
thoraricSurgery <- thoraricSurgery %>% 
  mutate_if(is.character, ~as.numeric(.))
#- Thoraric Surgery Column Definitions
{
  # 1. DGN: Diagnosis - specific combination of ICD-10 codes for primary and secondary as well multiple tumours if any (DGN3,DGN2,DGN4,DGN6,DGN5,DGN8,DGN1)
  # 2. PRE4: Forced vital capacity - FVC (numeric)
  # 3. PRE5: Volume that has been exhaled at the end of the first second of forced expiration - FEV1 (numeric)
  # 4. PRE6: Performance status - Zubrod scale (PRZ2,PRZ1,PRZ0)
  # 5. PRE7: Pain before surgery (T,F)
  # 6. PRE8: Haemoptysis before surgery (T,F)
  # 7. PRE9: Dyspnoea before surgery (T,F)
  # 8. PRE10: Cough before surgery (T,F)
  # 9. PRE11: Weakness before surgery (T,F)
  # 10. PRE14: T in clinical TNM - size of the original tumour, from OC11 (smallest) to OC14 (largest) (OC11,OC14,OC12,OC13)
  # 11. PRE17: Type 2 DM - diabetes mellitus (T,F)
  # 12. PRE19: MI up to 6 months (T,F)
  # 13. PRE25: PAD - peripheral arterial diseases (T,F)
  # 14. PRE30: Smoking (T,F)
  # 15. PRE32: Asthma (T,F)
  # 16. AGE: Age at surgery (numeric)
  # 17. Risk1Y: 1 year survival period - (T)rue value if died (T,F)
  }
# Assignment Instructions:
survival_glm_all <- glm(Risk1Yr ~ DGN + PRE4 + PRE5 + PRE6 + PRE8 + PRE9 + PRE10 + PRE11 + PRE14 + PRE17 + PRE19 + PRE25 + PRE30 + PRE32 + AGE, 
                        data = thoraricSurgery)   
summary(survival_glm_all)
# Fit a binary logistic regression model to the data set that predicts whether or not the patient survived for one year
# (the Risk1Y variable) after the surgery. Use the glm() function to perform the logistic regression.
# See Generalized Linear Models for an example. 
# Include a summary using the summary() function in your results.
survival_glm <- glm(Risk1Yr ~ DGN +  PRE9 + PRE14 + PRE17 + PRE30 , 
                    data = thoraricSurgery)   
summary(survival_glm)
focusThoraricSurgery <- thoraricSurgery %>% 
  select(c(Risk1Yr, DGN, PRE9, PRE14, PRE17, PRE30))


x <- focusThoraricSurgery[,2:6]
y <- focusThoraricSurgery[,1]
scales <- list(x = list(relation="free"),
               y=list(relation="free"))
featurePlot(x = x, 
            y = y,
            plot="pairs",
            scales=scales)


correlationThoraricSurgery <- cor(x)
corrplot(correlationThoraricSurgery, method="circle")
# According to the summary, which variables had the greatest effect on the survival rate?
#   To compute the accuracy of your model, use the dataset to predict the outcome variable.
#   The percent of correct predictions is the accuracy of your model.
#   What is the accuracy of your model?
thoraric_fit <- predict(survival_glm, type = "response")
summary(thoraric_fit)
thoraric_predictor <-ifelse(thoraric_fit > 0.5, 1, 0)

thoraric_predictor_data <- thoraric_predictor %>%   
  as.data.frame() %>% 
  mutate(id = row_number())

names(thoraric_predictor_data)[1] <- 'Prediction'

thoraric_class_data <- focusThoraricSurgery %>% 
  mutate(id = row_number(),
         Count = 1) %>% 
  left_join(thoraric_predictor_data) %>% 
  mutate(`Correct Prediction` = ifelse(Risk1Yr - Prediction == 0, 'Correct', 'Incorrect'), 
         Risk1Yr = ifelse(Risk1Yr == 1, "T", "F"))

totRecords_thor <- length(thoraric_class_data$Risk1Yr)

thoraric_class_data %>% 
  group_by(Risk1Yr, `Correct Prediction`) %>% 
  summarise(`Prediction Count` = sum(Count)) %>% 
  mutate(Accurracy = round(`Prediction Count` / totRecords_thor, 3))
# Fit a Logistic Regression Model
binaryClassData_Raw <- read.csv('binary-classifier-data.csv')
# Fit a logistic regression model to the binary-classifier-data.csv dataset
# The dataset (found in binary-classifier-data.csv) contains three variables; label, x, and y.
# The label variable is either 0 or 1 and is the output we want to predict using the x and y variables.
binary_lm <- glm(label ~ x + y, 
                 data = binaryClassData_Raw)
summary(binary_lm)
x2 <- binaryClassData_Raw[,2:3]
y2 <- binaryClassData_Raw[,1]
featurePlot(x = x2, 
            y = y2,
            plot="pairs",
            # scales=scales
)
binary_lm_fit <- predict(binary_lm, type = 'response')
binary_lm_predictor <- ifelse(binary_lm_fit > .5, 1, 0)

summary(binary_lm_fit)

binary_lm_predictionData <- binary_lm_predictor %>%   
  as.data.frame() %>% 
  mutate(id = row_number()) 

names(binary_lm_predictionData)[1] <- 'Prediction'

binaryClassData <- binaryClassData_Raw %>% 
  mutate(id = row_number(),
         Count = 1) %>% 
  left_join(binary_lm_predictionData) %>% 
  mutate(`Correct Prediction` = ifelse(label - Prediction == 0, 'Correct', 'Incorrect'))

totRecords <- length(binaryClassData$label)
# What is the accuracy of the logistic regression classifier?
binaryClassData %>% 
  group_by(label, `Correct Prediction`) %>% 
  summarise(`Prediction Count` = sum(Count)) %>% 
  mutate(Accurracy = round(`Prediction Count` / totRecords, 3))

# Keep this assignment handy, as you will be comparing your results from this week to next week.