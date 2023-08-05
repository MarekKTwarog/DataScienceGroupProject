# Goal 3: find the predictors associated with sleep disturbance
# will use the backwards stepwise method
library(dplyr)
install.packages("carData")
library(carData)
install.packages("MASS")
library(MASS)

# finding the predictors associated with sleep for the PSQ1 measure
# create a model with all predictors included
liver_PSQ1PP <- liver_dfclean3[,c("Gender", "Age", "BMI", "Time.from.transplant",
                                  "Liver.Diagnosis", "Recurrence.of.disease", "Rejection.graft.dysfunction",
                                  "Any.fibrosis", "Renal.Failure", "Depression", "Corticoid", "Pittsburgh.Sleep.Quality.Index.Score")]
# using the glimpse function to view a concise overview of the data frame
glimpse(liver_PSQ1P)
# Fit the full logistic regression model with all predictors
# logistic regression model was used instead of the linear model because we have binary variables.
logit_mod_full <- glm(Pittsburgh.Sleep.Quality.Index.Score~., data = liver_PSQ1PP)

# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
logit.step.back <- stepAIC(logit_mod_full)

summary(logit.step.back)
# Backwards selection using stepwise AIC selection suggests, "Gender","Time.from.transplant"
#'"Depression","Corticoid", and "Recurrence.of.disease" are largest predictors of 'patient "Pittsburgh.Sleep.Quality.Index.Score".

# finding the predictors associated with sleep for the ESS measure
liver_Epworth <- liver_dfclean3[,c("Gender", "Age", "BMI", "Time.from.transplant",
                                   "Liver.Diagnosis", "Recurrence.of.disease", "Rejection.graft.dysfunction",
                                   "Any.fibrosis", "Renal.Failure", "Depression", "Corticoid", "Pittsburgh.Sleep.Quality.Index.Score")]
# using the glimpse function to view a concise overview of the data frame
glimpse(liver_Epworth)
# creating a logistic regression model with all the predictors
logit_mod_full_ESS <- glm(Pittsburgh.Sleep.Quality.Index.Score~., data = liver_Epworth)

# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
logit.step.back.ESS <- stepAIC(logit_mod_full_ESS)

summary(logit.step.back.ESS)
# Backwards selection using stepwise AIC selection suggests
#"Gender", "Time.from.transplant", "Recurrence.of.disease", "Depression", "Corticoid" are the largest predictors of 'patient "Epworth.Sleepiness.Scale"

# finding the predictors associated with sleep for the AIS measure
liver_Athens <- liver_dfclean3[,c("Gender", "Age", "BMI", "Time.from.transplant",
                                  "Liver.Diagnosis", "Recurrence.of.disease", "Rejection.graft.dysfunction",
                                  "Any.fibrosis", "Renal.Failure", "Depression", "Corticoid", "Pittsburgh.Sleep.Quality.Index.Score")]
# using the glimpse function to view a concise overview of the data frame
glimpse(liver_Athens)
# creating a logistic regression model with all the predictors
logit_mod_full_AIS <- glm(Pittsburgh.Sleep.Quality.Index.Score~., data = liver_Athens)

# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
logit.step.back.AIS <- stepAIC(logit_mod_full_AIS)

summary(logit.step.back.AIS)
# Backwards selection using stepwise AIC selection suggests
#"Gender", "Time.from.transplant", "Recurrence.of.disease", "Depression", "Corticoid" are the largest predictors of 'patient "Epworth.Sleepiness.Scale"


# finding the predictors associated with sleep for the BSS measure
liver_Berlin <- liver_dfclean3[,c("Gender", "Age", "BMI", "Time.from.transplant",
                                  "Liver.Diagnosis", "Recurrence.of.disease", "Rejection.graft.dysfunction",
                                  "Any.fibrosis", "Renal.Failure", "Depression", "Corticoid", "Pittsburgh.Sleep.Quality.Index.Score")]
# using the glimpse function to view a concise overview of the data frame
glimpse(liver_Berlin)
# creating a logistic regression model with all the predictors
logit_mod_full_BSS <- glm(Pittsburgh.Sleep.Quality.Index.Score~., data = liver_Berlin)

# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
logit.step.back.BSS <- stepAIC(logit_mod_full_BSS)

summary(logit.step.back.BSS)
# Backwards selection using stepwise AIC selection suggests
#"Gender", "Time.from.transplant", "Recurrence.of.disease", "Depression", "Corticoid" are the largest predictors of 'patient "Epworth.Sleepiness.Scale"



