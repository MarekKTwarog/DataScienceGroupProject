#### Goal 3: find the predictors associated with sleep disturbance (updated)

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
PSQI_full <- glm(Pittsburgh_binary~
                 Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis +
                 Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                Renal.Failure + Depression + Corticoid, data = na.omit(liver_dfclean3), family = "binomial")
# using the glimpse function to view a concise overview of the data frame
glimpse(PSQI_full)
# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
PSQI.step.back <- stepAIC(PSQI_full)
summary(PSQI.step.back)

# finding the predictors associated with sleep for the ESS measure
ESS_full <- glm(Epworth_binary~
                   Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis +
                   Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                   Renal.Failure + Depression + Corticoid, data = na.omit(liver_dfclean3), family = "binomial")
# using the glimpse function to view a concise overview of the data frame
glimpse(ESS_full)
# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
ESS.step.back <- stepAIC(ESS_full)

summary(ESS.step.back)

# finding the predictors associated with sleep for the AIS measure
AIS_full <- glm(Athens_binary~
                  Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis +
                  Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                  Renal.Failure + Depression + Corticoid, data = na.omit(liver_dfclean3), family = "binomial")
# using the glimpse function to view a concise overview of the data frame
glimpse(AIS_full)
# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
AIS.step.back <- stepAIC(AIS_full)
summary(AIS.step.back)


# finding the predictors associated with sleep for the BSS measure
BSS_full <- glm(Berlin.Sleepiness.Scale~
                  Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis +
                  Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                  Renal.Failure + Depression + Corticoid, data = na.omit(liver_dfclean3), family = "binomial")
# using the glimpse function to view a concise overview of the data frame
glimpse(BSS_full)
# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
BSS.step.back <- stepAIC(BSS_full)
summary(BSS.step.back)
