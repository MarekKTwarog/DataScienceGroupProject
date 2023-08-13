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



#### Goal 3B: find the predictors associated with sleep disturbance for the liver_dfcleanPMM_IMPrmNA data frame

# Fit the full logistic regression model with all predictors
# logistic regression model was used instead of the linear model because we have binary variables.
PSQI_full_IMPrmNA <- glm(Pittsburgh_binary~
                          Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis +
                          Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                          Renal.Failure + Depression + Corticoid, data = na.omit(liver_dfcleanPMM_IMPrmNA), family = "binomial")

# using the glimpse function to view a concise overview of the data frame
glimpse(PSQI_full_IMPrmNA)
# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
PSQI.step.back.IMPrmNA <- stepAIC(PSQI_full_IMPrmNA)
summary(PSQI.step.back.IMPrmNA)

# finding the predictors associated with sleep for the ESS measure
ESS_full_IMPrmNA <- glm(Epworth_binary~
                         Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis +
                         Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                         Renal.Failure + Depression + Corticoid, data = na.omit(liver_dfcleanPMM_IMPrmNA), family = "binomial")
# using the glimpse function to view a concise overview of the data frame
glimpse(ESS_full_IMPrmNA)
# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
ESS.step.back.IMPrmNA <- stepAIC(ESS_full_IMPrmNA)

summary(ESS.step.back.IMPrmNA)

# finding the predictors associated with sleep for the AIS measure
AIS_full_IMPrmNA <- glm(Athens_binary~
                         Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis +
                         Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                         Renal.Failure + Depression + Corticoid, data = na.omit(liver_dfcleanPMM_IMPrmNA), family = "binomial")
# using the glimpse function to view a concise overview of the data frame
glimpse(AIS_full_IMPrmNAI)
# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
AIS.step.back.IMPrmNA <- stepAIC(AIS_full_IMPrmNA)
summary(AIS.step.back.IMPrmNA)


# finding the predictors associated with sleep for the BSS measure
BSS_full_IMPrmNA <- glm(Berlin.Sleepiness.Scale~
                         Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis +
                         Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                         Renal.Failure + Depression + Corticoid, data = na.omit(liver_dfcleanPMM_IMPrmNA), family = "binomial")
# using the glimpse function to view a concise overview of the data frame
glimpse(BSS_full_IMPrmNA)
# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
BSS.step.back.IMPrmNA <- stepAIC(BSS_full_IMPrmNA)
summary(BSS.step.back.IMPrmNA)



#### Goal 3C: find the predictors associated with sleep disturbance for the liver_noPSQI_rmNA data frame

#Loading dataset
liver_df <- read.csv("project_data.csv")

#Loading packages for removal of weak predictors

# Fit the full logistic regression model with all predictors
# logistic regression model was used instead of the linear model because we have binary variables.
PSQI_full_noPSQI <- glm(Pittsburgh_binary~
                      Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis +
                      Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                      Renal.Failure + Depression + Corticoid, data = na.omit(liver_noPSQI_rmNA), family = "binomial")
#ERROR: Pittsburgh_binary not found since PSQI was removed from this dataframe

# finding the predictors associated with sleep for the ESS measure
ESS_full_noPSQI <- glm(Epworth_binary~
                     Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis +
                     Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                     Renal.Failure + Depression + Corticoid, data = na.omit(liver_noPSQI_rmNA), family = "binomial")
# using the glimpse function to view a concise overview of the data frame
glimpse(ESS_full_noPSQI)
# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
ESS.step.back.noPSQI <- stepAIC(ESS_full_noPSQI)

summary(ESS.step.back.noPSQI)

# finding the predictors associated with sleep for the AIS measure
AIS_full_noPSQI <- glm(Athens_binary~
                     Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis +
                     Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                     Renal.Failure + Depression + Corticoid, data = na.omit(liver_noPSQI_rmNA), family = "binomial")
# using the glimpse function to view a concise overview of the data frame
glimpse(AIS_full_noPSQI)
# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
AIS.step.back.noPSQI <- stepAIC(AIS_full_noPSQI)
summary(AIS.step.back.noPSQI)


# finding the predictors associated with sleep for the BSS measure
BSS_full_noPSQI <- glm(Berlin.Sleepiness.Scale~
                     Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis +
                     Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                     Renal.Failure + Depression + Corticoid, data = na.omit(liver_noPSQI_rmNA), family = "binomial")
# using the glimpse function to view a concise overview of the data frame
glimpse(BSS_full_noPSQI)
# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
BSS.step.back.noPSQI <- stepAIC(BSS_full_noPSQI)
summary(BSS.step.back.noPSQI)
