#### Goal 3A: find the predictors associated with sleep disturbance for the liver_dfcleanCC data frame

#Loading dataset
liver_df <- read.csv("project_data.csv")

#Loading packages for removal of weak predictors

library(dplyr)
install.packages("carData")
library(carData)
install.packages("MASS")
library(MASS)


# Fit the full logistic regression model with all predictors
# logistic regression model was used instead of the linear model because we have binary variables.
PSQI_full_CC <- glm(Pittsburgh_binary~
                   Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis +
                   Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                   Renal.Failure + Depression + Corticoid, data = na.omit(liver_dfcleanCC), family = "binomial")
# using the glimpse function to view a concise overview of the data frame
glimpse(PSQI_full_CC)
# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
PSQI.step.back <- stepAIC(PSQI_full_CC)
summary(PSQI.step.back.CC)

# finding the predictors associated with sleep for the ESS measure
ESS_full_CC <- glm(Epworth_binary~
                  Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis +
                  Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                  Renal.Failure + Depression + Corticoid, data = na.omit(liver_dfcleanCC), family = "binomial")
# using the glimpse function to view a concise overview of the data frame
glimpse(ESS_full_CC)
# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
ESS.step.back.CC <- stepAIC(ESS_full_CC)

summary(ESS.step.back.CC)

# finding the predictors associated with sleep for the AIS measure
AIS_full_CC <- glm(Athens_binary~
                  Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis +
                  Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                  Renal.Failure + Depression + Corticoid, data = na.omit(liver_dfcleanCC), family = "binomial")
# using the glimpse function to view a concise overview of the data frame
glimpse(AIS_full_CC)
# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
AIS.step.back.CC <- stepAIC(AIS_full_CC)
summary(AIS.step.back.CC)


# finding the predictors associated with sleep for the BSS measure
BSS_full_CC <- glm(Berlin.Sleepiness.Scale~
                  Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis +
                  Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                  Renal.Failure + Depression + Corticoid, data = na.omit(liver_dfcleanCC), family = "binomial")
# using the glimpse function to view a concise overview of the data frame
glimpse(BSS_full_CC)
# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
BSS.step.back.CC <- stepAIC(BSS_full_CC)
summary(BSS.step.back.CC)



#### Goal 3B: find the predictors associated with sleep disturbance for the liver_dfcleanIMPrmNA data frame

#Loading dataset
liver_df <- read.csv("project_data.csv")

#Loading packages for removal of weak predictors

library(dplyr)
install.packages("carData")
library(carData)
install.packages("MASS")
library(MASS)


# Fit the full logistic regression model with all predictors
# logistic regression model was used instead of the linear model because we have binary variables.
PSQI_full_rmNA <- glm(Pittsburgh_binary~
                   Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis +
                   Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                   Renal.Failure + Depression + Corticoid, data = na.omit(liver_dfcleanIMPrmNA), family = "binomial")
# using the glimpse function to view a concise overview of the data frame
glimpse(PSQI_full_rmNA)
# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
PSQI.step.back.rmNA <- stepAIC(PSQI_full_rmNA)
summary(PSQI.step.back.rmNA)

# finding the predictors associated with sleep for the ESS measure
ESS_full_rmNA <- glm(Epworth_binary~
                  Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis +
                  Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                  Renal.Failure + Depression + Corticoid, data = na.omit(liver_dfcleanIMPrmNA), family = "binomial")
# using the glimpse function to view a concise overview of the data frame
glimpse(ESS_full_rmNA)
# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
ESS.step.back.rmNA <- stepAIC(ESS_full_rmNA)

summary(ESS.step.back.rmNA)

# finding the predictors associated with sleep for the AIS measure
AIS_full_rmNA <- glm(Athens_binary~
                  Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis +
                  Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                  Renal.Failure + Depression + Corticoid, data = na.omit(liver_dfcleanIMPrmNA), family = "binomial")
# using the glimpse function to view a concise overview of the data frame
glimpse(AIS_full_rmNA)
# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
AIS.step.back.rmNA <- stepAIC(AIS_full_rmNA)
summary(AIS.step.back.rmNA)


# finding the predictors associated with sleep for the BSS measure
BSS_full_rmNA <- glm(Berlin.Sleepiness.Scale~
                  Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis +
                  Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                  Renal.Failure + Depression + Corticoid, data = na.omit(liver_dfcleanIMPrmNA), family = "binomial")
# using the glimpse function to view a concise overview of the data frame
glimpse(BSS_full_rmNA)
# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
BSS.step.back.rmNA <- stepAIC(BSS_full_rmNA)
summary(BSS.step.back.rmNA)

