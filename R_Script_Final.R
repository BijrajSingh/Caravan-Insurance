#Import data
training<- Traning_FieldNames
test<- Test_Data_FieldNames


#Resources for Correlations
install.packages("pacman")
library(pacman)

install.packages("ggcorrplot")
p_load(explore, ggplot2, tidyverse)
p_load(lares, GGally)
library(corrplot)
p_load(corrplot)



#To find individual correlations with the target
training_copy <-  training
training_copy %>% dplyr :: select(purchasing_power_class, caravan) %>% corr_var(caravan)
training_copy %>% dplyr :: select(contribution_trailer_policies, caravan) %>% corr_var(caravan)

#Correlation - two groups
training.sociodemographic = training[,c(1:43,86)]
training.ownership = training[,c(44:86)]
training.sociodemographic.cor = cor(training.sociodemographic)
training.ownership.cor = cor(training.ownership)

#Install package
library(dplyr)
library(rpart)
library(pacman); p_load(RMySQL)
library(dplyr)
library(MASS)
p_load(explore, ggplot2, tidyverse)
p_load(lares, GGally)
p_load(C50)
p_load(caret)
p_load(MASS)

#Train model - C5.0 - imbalanced data
p_load(C50)
library(lattice)
p_load(caret)
training.target <- factor(training$caravan) 
model.imbalanced <- C5.0(x = training, y = training.target, rules = TRUE)
summary(model.imbalanced)
varImp(model.imbalanced)

#Test Model - imbalanced data
test.model_target = predict(model.imbalanced, newdata = test)
test.target = as.factor(test$caravan)
mean(test.model_target==test.target)


#Confusion Matrix - imbalanced data
confusionMatrix(data = test.model_target, reference = test.target)
#Data Cleaning
#Balancing target data - ROSE - 50:50
install.packages("ROSE")
library(ROSE)
p_load(ROSE)
training.rose_50_50 <- ovun.sample(caravan~., data=training, "over", p = 0.50, seed = 222)$data
table(training.rose_50_50$caravan)

#Balancing target data - ROSE - 60:40
training.rose_60_40 <- ovun.sample(caravan~., data=training, "over", p = 0.4, seed = 222)$data
table(training.rose_60_40$caravan)

#Balancing target data - ROSE - 70:30
training.rose_70_30 <- ovun.sample(caravan~., data=training, "over", p = 0.3, seed = 222)$data
                               
table(training.rose_70_30$caravan)

#Data Pre-processing
# Both stepwise model - Feature selection after balancing
library(MASS)

# Fit the full model - 50:50
linear.model_5050<- lm(caravan ~., data = training.ownership)
# Stepwise regression model
step.model_5050 <- stepAIC(linear.model_5050, direction = "both",  trace = FALSE)
summary(step.model_5050)
featureselect.training_5050 <- names(step.model_5050$coefficients) #Obtain features from the model

featureselect.training_5050 <- featureselect.training_5050[!featureselect.training_5050 %in% "(Intercept)"]
print(featureselect.training_1)

# Fit the full model - 60:40
linear.model_6040 <- lm(caravan ~., data = training.rose_60_40)
# Stepwise regression model
step.model_6040 <- stepAIC(linear.model_6040, direction = "both",  trace = FALSE)
summary(step.model_6040)
featureselect.training_6040 <- names(step.model_6040$coefficients) #Obtain features from the model

featureselect.training_6040 <- featureselect.training_6040[!featureselect.training_6040 %in% "(Intercept)"]
print(featureselect.training_6040)

# Fit the full model - 70:30
full.model_7030 <- lm(caravan ~., data = training.rose_70_30)
# Stepwise regression model
step.model_7030 <- stepAIC(full.model_7030, direction = "both",  trace = FALSE)
summary(step.model_7030)
featureselect.training_7030 <- names(step.model_7030$coefficients) #Obtain features from the model

featureselect.training_7030 <- featureselect.training_7030[!featureselect.training_7030 %in% "(Intercept)"]
print(featureselect.training_7030)

#Data Mining + Evaluation
#Train Model - C5 - 50:50 - reduced features
library(C50)

Training_RF_1 <- dplyr :: select(training.rose_50_50, contribution_private_third_party_insurance_see_l4,number_of_third_party_insurance_.firms._...,contribution_car_policies,number_of_motorcycle.scooter_policies,contribution_trailer_policies,contribution_life_insurances,contribution_family_accidents_insurance_policies,number_of_disability_insurance_policies,contribution_fire_policies,number_of_property_insurance_policies,number_of_boat_policies,number_of_bicycle_policies,number_of_social_security_insurance_policies, caravan)
Y_1 <- factor(training.rose_50_50$caravan)
X_1 <- Training_RF_1 |> dplyr :: select(-c(caravan))
model.rules_1 <- C5.0(x =X_1, y = Y_1, rules = TRUE)
summary(model.rules_1) #Accuracy
varImp(model.rules_1)

#Test Model - 50:50 + reduced features
yh_1 = predict(model.rules_1, newdata = test)
y_1 = as.factor(test$caravan)
mean(y_1==yh_1)

#Confusion Matrix (50:50) + reduced features
confusionMatrix(data = yh_1, reference = y_1)

#Train model - C5.0 -  (60:40 + reduced features)
Training_RF_2 <- dplyr :: select(training.rose_60_40, contribution_private_third_party_insurance_see_l4,number_of_third_party_insurance_.firms._...,contribution_car_policies,number_of_motorcycle.scooter_policies,contribution_trailer_policies,contribution_life_insurances,contribution_family_accidents_insurance_policies,number_of_disability_insurance_policies,contribution_fire_policies,number_of_property_insurance_policies,number_of_boat_policies,number_of_bicycle_policies,number_of_social_security_insurance_policies, caravan)
Y_2 <- factor(training.rose_60_40$caravan) 
X_2 <- Training_RF_2 |> dplyr :: select(-c(caravan))
model.rules_2 <- C5.0(x = X_2, y = Y_2, rules = TRUE)
summary(model.rules_2) #Accuracy
varImp(model.rules_2)

#Test Model - C5 - 60:40
yh_2 = predict(model.rules_2, newdata = test)
y_2 = as.factor(test$caravan)
mean(y_2==yh_2)

#Confusion Matrix - C5 - (60:40)
confusionMatrix(data = yh_2, reference = y_2)

#Train Model - C5.0 (70:30 - reduced features)
Training_RF_3 <- dplyr :: select(training.rose_70_30, contribution_private_third_party_insurance_see_l4,number_of_third_party_insurance_.firms._...,contribution_car_policies,number_of_motorcycle.scooter_policies,contribution_trailer_policies,contribution_life_insurances,contribution_family_accidents_insurance_policies,number_of_disability_insurance_policies,contribution_fire_policies,number_of_property_insurance_policies,number_of_boat_policies,number_of_bicycle_policies,number_of_social_security_insurance_policies, caravan)
Y_3 <- factor(training.rose_70_30$caravan) 
X_3 <- Training_RF_3 |> dplyr :: select(-c(caravan))
model.rules_3 <- C5.0(x = X_3, y = Y_3, rules = TRUE)
summary(model.rules_3) #Accuracy
varImp(model.rules_3)

#Test Model - C5 - 70:30
yh_3 = predict(model.rules_3, newdata = test)
y_3 = as.factor(test$caravan)
mean(y_3==yh_3)

#Confusion Matrix - C5 - (70:30)
confusionMatrix(data = yh_3, reference = y_3) #Better (Compared with other C5 models)

#Train Model - LogR - (50:50)
library(pacman)
p_load(explore, ggplot2, tidyverse, lares, GGally, caret)
logregmodel_train_5050 <- glm(training.rose_50_50$caravan ~ contribution_private_third_party_insurance_see_l4+number_of_third_party_insurance_.firms._...+contribution_car_policies+number_of_motorcycle.scooter_policies+contribution_trailer_policies+contribution_life_insurances+contribution_family_accidents_insurance_policies+number_of_disability_insurance_policies+contribution_fire_policies+number_of_property_insurance_policies+number_of_boat_policies+number_of_bicycle_policies+number_of_social_security_insurance_policies,data= training.rose_50_50, family = "binomial")

#Test Model - LogR - (50:50)
p_1 <- predict(logregmodel_train_5050, type = "response", newdata = test)
yh_test_1 <- (p_1 > .5)*1
y_test_1 <- ifelse(test$caravan=="1",1,0)
mean(y_test_1==yh_test_1)

#Confusion Matrix  - LogR - (50:50)
library(caret)
yhf_1 <- as.factor(yh_test_1); yf_1 <- as.factor(y_test_1)
cm_1 <- confusionMatrix(data=yhf_1, reference = yf_1, positive = "1")
print(cm_1)

#Train Model - LogR - (40:60)
logregmodel_train_2 <- glm(training.rose_60_40$caravan ~ contribution_private_third_party_insurance_see_l4+number_of_third_party_insurance_.firms._...+contribution_car_policies+number_of_motorcycle.scooter_policies+contribution_trailer_policies+contribution_life_insurances+contribution_family_accidents_insurance_policies+number_of_disability_insurance_policies+contribution_fire_policies+number_of_property_insurance_policies+number_of_boat_policies+number_of_bicycle_policies+number_of_social_security_insurance_policies, data = training.rose_60_40, family = "binomial")

#Test Model - LogR - (40:60)
p_2 <- predict(logregmodel_train_2, type = "response", newdata = test)
yh_test_2 <- (p_2 > .5)*1
y_test_2 <- ifelse(test$caravan=="1",1,0)
mean(y_test_2==yh_test_2)

#Confusion Matrix - LogR - (40:60)
yhf_2 <- as.factor(yh_test_2); yf_2 <- as.factor(y_test_2)
cm_2 <- confusionMatrix(data=yhf_2, reference = yf_2, positive = "1")
print(cm_2) 

#Train Model - LogR - (30:70)
logregmodel_train_3 <- glm(training.rose_70_30$caravan ~ contribution_private_third_party_insurance_see_l4+number_of_third_party_insurance_.firms._...+contribution_car_policies+number_of_motorcycle.scooter_policies+contribution_trailer_policies+contribution_life_insurances+contribution_family_accidents_insurance_policies+number_of_disability_insurance_policies+contribution_fire_policies+number_of_property_insurance_policies+number_of_boat_policies+number_of_bicycle_policies+number_of_social_security_insurance_policies, data = training.rose_70_30, family = "binomial")

#Test Model - LogR - (30:70)
p_3 <- predict(logregmodel_train_3, type = "response", newdata = test)
yh_test_3 <- (p_3 >.5)*1
y_test_3 <- ifelse(test$caravan=="1",1,0)
mean(y_test_3==yh_test_3)

#Confusion Matrix - LogR - (30:70)
yhf_3 <- as.factor(yh_test_3); yf_3 <- as.factor(y_test_3)
cm_3 <- confusionMatrix(data=yhf_3, reference = yf_3, positive = "1")
print(cm_3) #Better (Compared to other LogR models and C5 models)


