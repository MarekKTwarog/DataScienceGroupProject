#Removing "Epworth.Sleepiness.Scale","Pittsburgh.Sleep.Quality.Index.Score", "Athens.Insomnia.Scale",  "SF36.PCS", "SF36.MCS",
#and Predicted PSQ1 columns since these are not relavaent predictors of Pittsburgh.Sleep.Quality.Index.Score.
#as they are more response variables. We also dont use the Pittsburgh_binary variable because the response variable
#for a logistic regression model cannot be binary when performing full logistic regression, instead an error message is displayed.
liver_PSQ1PP <- liver_dfclean3[,c("Gender", "Age", "BMI", "Time.from.transplant",
                                  "Liver.Diagnosis", "Recurrence.of.disease", "Rejection.graft.dysfunction",
                                  "Any.fibrosis", "Renal.Failure", "Depression", "Corticoid", "Pittsburgh.Sleep.Quality.Index.Score")]
glimpse(liver_PSQ1PP)
# Fit the full logistic regression model with all predictors. The logistic regression
#model was used instead of the linear modeel used in class tutoriall example because
#we have binary variables.
logit_mod_full <- glm(Pittsburgh.Sleep.Quality.Index.Score~., data = liver_PSQ1PP)

logit.step.back <- stepAIC(logit_mod_full)
#Backwards seelection using stepwise AIC selection suggests, "Gender","Time.from.transplant"
#'"Depression","Corticoid" , and "Recurrence.of.disease" are largest predictors of
#'patient "Pittsburgh.Sleep.Quality.Index.Score".

summary(lm.step.back)
