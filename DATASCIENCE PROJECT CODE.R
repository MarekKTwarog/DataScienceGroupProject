#Loading packages for data cleaning / analysis
install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")
library(tidyverse)
library(funModeling)
library(Hmisc)

#Loading dataset
liver_df <- read.csv("project_data.csv")

#'Creating a new dataframe with only the variables that we need to conduct our
#'analysis of the dataset
liver_dfclean <- liver_df[,c("Gender", "Age", "BMI", "Time.from.transplant",
                             "Liver.Diagnosis", "Recurrence.of.disease", "Rejection.graft.dysfunction",
                             "Any.fibrosis", "Renal.Failure", "Depression", "Corticoid", "Epworth.Sleepiness.Scale",
                             "Pittsburgh.Sleep.Quality.Index.Score", "Athens.Insomnia.Scale", "Berlin.Sleepiness.Scale",
                             "SF36.PCS", "SF36.MCS")]

#'Using glimpse() to display column names, analyze structure of data, identify
#'missing values/zeros and to see what type each variable is coded as. Status()
#'was used to check p_na values for each variable, values with p_na <0.2 indicate
#'data for variable has less than 20% missing values. Status() also tells us how
#'many unique values are present per variable, this can help easily identify
#'which variables should be categorical.
glimpse(liver_dfclean)
status(liver_dfclean)# p_na values for PSQI variabee is 0.317164179 which is high, the NA values will need to be imputatd
dim(liver_dfclean)
#Installling corrplot package so I can generate a correlation plot so I can determine
#' which variable has highest correlation with PSQ1 so I can then create a linear
#' regression model between the two variables and then use that model to predict
#' NA values for PSQ1
install.packages("corrplot")
library(corrplot)

#' Here we use the cor() function to find the correlation between each variable
#' then we use the corrplot function to crease a plot that will help easily visualize
#' which variable has the strongest correlation with PSQ1. Based on the results,
#' wee find that the Athens.Insomnia.Scale has the strongest correlation with
#' PSQ1 and thus we use the lm() function to build a linear regression model between
#' the two variables so that it can then be used along with the predict() function
#' to predict values for NA in the PSQ1 variable column. The values predicted are
#' then used to replace the NA values in the PSQ1 column,

correlation <- cor(liver_dfclean, use = "pairwise")
corrplot(correlation, type = "lower", diag = FALSE)
lm_model <- lm(Pittsburgh.Sleep.Quality.Index.Score ~ Athens.Insomnia.Scale, data = liver_dfclean, na.action = na.exclude)
liver_dfclean$PredictedPSQ1 <- predict(lm_model, newdata = liver_dfclean)

# Replace NA values in 'PSQ1' with their respective 'y_predicted' values
liver_dfclean$Pittsburgh.Sleep.Quality.Index.Score[is.na(liver_dfclean$Pittsburgh.Sleep.Quality.Index.Score)] <- liver_dfclean$PredictedPSQ1[is.na(liver_dfclean$Pittsburgh.Sleep.Quality.Index.Score)]


#'Here we convert categorical variables into factors by first assigning all variables
#'needing conversion to "categorical_var", then we implement a for loop which will
#' move through each variable in the categorical_var list and convert them to factors
#' then reassign the converted variable back to the liver_defclean dataset. The
#' temporary 'col' variable aprt of the for loop is assigneed to each variable in
#' the given list one by one until all variables have been converted to factors.
categorical_var <- c("Gender", "Liver.Diagnosis", "Recurrence.of.disease", "Rejection.graft.dysfunction",
                     "Any.fibrosis", "Renal.Failure", "Depression", "Corticoid", "Berlin.Sleepiness.Scale")
for (col in categorical_var) {
  liver_dfclean[[col]] <- factor(liver_dfclean[[col]])
}

#Checking to see if variables were correctly transformed
glimpse(liver_dfclean)

#'Using the summary function to check for presence of any values that were not
#'encoded as NA's but look to be out of place
summary(liver_dfclean) #Looks good

#'Here we convert PSQI, ESS and AIS, to binary variables like BSS based on the
#'datasets description for values above a certain level. Epworth Sleepiness Scale
#'total score greater than 10, indicating at least mild excessive daytime sleepiness
#'(1 = yes, 0 = no), Pittsburgh Sleep Quality Index score greater than 4, indicating
#'poor sleep quality (1 = yes, 0 = no),Athens Insomnia Scale total score greater than
#'5, indicating the presence of insomnia, (1 = yes, 0 = no). The new variables are
#'added to the dataframe as new columns using the mutate function from the tidyverse
#'package.
liver_dfclean2 <- liver_dfclean %>%
  mutate(Epworth_binary = ifelse(Epworth.Sleepiness.Scale > 10, 1, 0),
         Pittsburgh_binary = ifelse(Pittsburgh.Sleep.Quality.Index.Score > 4, 1, 0),
         Athens_binary = ifelse(Athens.Insomnia.Scale > 5, 1, 0))

#Now we must convert the newly made binary variables to factors for further analysis
categorical_var2 <- c("Epworth_binary", "Pittsburgh_binary", "Athens_binary")
for (col in categorical_var2) {
  liver_dfclean2[[col]] <- factor(liver_dfclean2[[col]])
}

#checking to see if conversion to factors was successful
glimpse(liver_dfclean2)#variables successfully converted to factors

#'The final set requires the removal of NA values from the dataset, this is done
#'using the na.omit function

liver_dfclean3 <- na.omit(liver_dfclean2)
#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------
# Step 4. Identifying the relationship between sleep disturbance and quality of life (physical and mental).
# We need to create linear regression models and conduct correlation tests for single predictors
# to investigate whether there is a linear relationship between each measure and outcome (PCS and MCS)
# We begin with the continuous measures
# List of continuous predictors
continuous_predictors <- c("Epworth.Sleepiness.Scale", "Pittsburgh.Sleep.Quality.Index.Score", "Athens.Insomnia.Scale")
# List of binary predictors
binary_predictors <- c("Epworth_binary", "Pittsburgh_binary", "Athens_binary", "Berlin.Sleepiness.Scale")
# List of response variables
responses <- c("SF36.PCS", "SF36.MCS")

# Loop through each combination of predictor (continuous and binary) and response variable, and fit a linear regression model
for (predictor in c(continuous_predictors, binary_predictors)) {
  for (response in responses) {
    model_formula <- as.formula(paste(response, "~", predictor))
    model <- lm(model_formula, data = liver_dfclean3)
    print(paste("Model: ", response, " ~ ", predictor))
    print(summary(model))
    plot(fitted(model), resid(model))  # Checking for homoscedasticity
    print(cor.test(liver_dfclean3[[response]], liver_dfclean3[[predictor]]))
  }
}
#----------------------------------------------------------------------------------
# Now build up to multiple predictor models
# Load the MASS library for the stepAIC function
library(MASS)
# Continous variables only for PCS and MCS

# Define the PCS null model
PCS.cont.lm.mod.null <- lm(SF36.PCS ~ 1, data = liver_dfclean3)

# Define the full model
PCS.cont.lm.mod.full <- lm(SF36.PCS ~ Epworth.Sleepiness.Scale + Pittsburgh.Sleep.Quality.Index.Score + Athens.Insomnia.Scale, data = liver_dfclean3)

# Perform forward stepwise selection
PCS.cont.lm.step.forw <- stepAIC(PCS.cont.lm.mod.null, direction = "forward", trace = F, scope = list(upper = cont.lm.mod.full, lower = cont.lm.mod.null))

# View the final model
summary(PCS.cont.lm.step.forw)

# Define the MCS null model
MCS.cont.lm.mod.null <- lm(SF36.MCS ~ 1, data = liver_dfclean3)

# Define the full model
MCS.cont.lm.mod.full <- lm(SF36.MCS ~ Epworth.Sleepiness.Scale + Pittsburgh.Sleep.Quality.Index.Score + Athens.Insomnia.Scale, data = liver_dfclean3)

# Perform forward stepwise selection
MCS.cont.lm.step.forw <- stepAIC(MCS.cont.lm.mod.null, direction = "forward", trace = F, scope = list(upper = cont.lm.mod.full, lower = cont.lm.mod.null))

# View the final model
summary(MCS.cont.lm.step.forw)
#---------------------------------------------------------------------------------
# Binary variables only for PCS

# Define the PCS null model
PCS.binom.mod.null <- lm(SF36.PCS ~ 1, data = liver_dfclean3)

# Define the full model
PCS.binom.mod.full <- lm(SF36.PCS ~ Epworth_binary + Pittsburgh_binary + Athens_binary + Berlin.Sleepiness.Scale, data = liver_dfclean3)

# Perform forward stepwise selection
PCS.binom.step.forw <- stepAIC(PCS.binom.mod.null, direction = "forward", trace = F, scope = list(upper = PCS.binom.mod.full, lower = PCS.binom.mod.null))

# View the final model
summary(PCS.binom.step.forw)

# Binary variables only for MCS

# Define the MCS null model
MCS.binom.mod.null <- lm(SF36.MCS ~ 1, data = liver_dfclean3)

# Define the full model
MCS.binom.mod.full <- lm(SF36.MCS ~ Epworth_binary + Pittsburgh_binary + Athens_binary + Berlin.Sleepiness.Scale, data = liver_dfclean3)

# Perform forward stepwise selection
MCS.binom.step.forw <- stepAIC(MCS.binom.mod.null, direction = "forward", trace = F, scope = list(upper = MCS.binom.mod.full, lower = MCS.binom.mod.null))

# View the final model
summary(MCS.binom.step.forw)
# --------------------------------------------
# Mixed model
library(car) #loading vif function
# Define the PCS null model
PCS.mix.mod.null <- lm(SF36.PCS ~ 1, data = liver_dfclean3)

# Define the full model
PCS.mix.mod.full <- lm(SF36.PCS ~ Epworth.Sleepiness.Scale + Pittsburgh.Sleep.Quality.Index.Score + Athens.Insomnia.Scale + Berlin.Sleepiness.Scale, data = liver_dfclean3)

# Perform forward stepwise selection
PCS.mix.step.forw <- stepAIC(PCS.mix.mod.null, direction = "forward", trace = F, scope = list(upper = PCS.mix.mod.full, lower = PCS.mix.mod.null))

# View the final model
summary(PCS.mix.step.forw)
#Check for collinearity
vif(PCS.mix.step.forw)
#Comparing to full varaible inclusion
anova(PCS.mix.mod.full, PCS.mix.step.forw)
#We see there is not a stat. significant value for inclusion of PSQI
#Checking AICs to further investigate
AIC(PCS.mix.mod.full)
AIC(PCS.mix.step.forw)
# Define the MCS null model
MCS.mix.mod.null <- lm(SF36.MCS ~ 1, data = liver_dfclean3)

# Define the full model
MCS.mix.mod.full <- lm(SF36.MCS ~ Epworth.Sleepiness.Scale + Pittsburgh.Sleep.Quality.Index.Score + Athens.Insomnia.Scale + Berlin.Sleepiness.Scale, data = liver_dfclean3)

# Perform forward stepwise selection
MCS.mix.step.forw <- stepAIC(MCS.mix.mod.null, direction = "forward", trace = F, scope = list(upper = MCS.mix.mod.full, lower = MCS.mix.mod.null))

# View the final model
summary(MCS.mix.step.forw)
#Check for collinearity
vif(MCS.mix.step.forw)
#comparing to full variable inclusion
anova(MCS.mix.mod.full, MCS.mix.step.forw)
#We see there is not a stat. significant value for inclusion of PSQI
#Checking AICs to further investigate
AIC(MCS.mix.mod.full)
AIC(MCS.mix.step.forw)
