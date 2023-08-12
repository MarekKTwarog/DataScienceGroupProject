################################################################################
############# Team Project BTC 1859H: Data Science in Health I #################
######## Leen Madani, Maryaah Salyani, Owen Treleaven, Marek Twarog ############
####################### August 15, 2023 at 5PM #################################
################################################################################

################################################################################
################## Goal 1: Description of relevant data ########################
################################################################################

#Loading packages for data cleaning / analysis
install.packages("tidyverse")
library(tidyverse)
install.packages("funModeling")
library(funModeling)
install.packages("mice")
library(mice)
install.packages("corrplot")
library(corrplot)

################################################################################
############################ LOADING DATASET ###################################
################################################################################

liver_df <- read.csv("project_data.csv")
glimpse(liver_df)

################################################################################
################################################################################

################################################################################
###### CREATING DATAFRAM WITH VARIABLES TO BE USED FOR ANALYSIS ################
################################################################################

liver_dfclean <- liver_df[,c("Gender", "Age", "BMI", "Time.from.transplant",
                             "Liver.Diagnosis", "Recurrence.of.disease", "Rejection.graft.dysfunction",
                             "Any.fibrosis", "Renal.Failure", "Depression", "Corticoid", "Epworth.Sleepiness.Scale",
                             "Pittsburgh.Sleep.Quality.Index.Score", "Athens.Insomnia.Scale", "Berlin.Sleepiness.Scale",
                             "SF36.PCS", "SF36.MCS")]

################################################################################
################################################################################

################################################################################
###################### UNDERSTANDING THE NA VALUES #############################
################################################################################

#'Using glimpse() to display column names, analyze structure of data, identify
#'missing values/zeros and to see what type each variable is coded as. Status()
#'was used to check p_na values for each variable, values with p_na <0.3 indicate
#'data for variable has less than 30% missing values. Status() also tells us how
#'many unique values are present per variable, this can help easily identify
#'which variables should be categorical.

glimpse(liver_dfclean)
status(liver_dfclean)
dim(liver_dfclean)

#' p_na values for PSQI variable is 0.317164179 which is high and over the 0.3
#' threshold meaning the variable will have to bee removed from analysis to prevent
#' inaccurate results. Thus a new dataframe excluding PSQI will be made but also
#' a complete case analysis will be performeed which includes PSQI as a variable,
#' and another dataframe which involves imputation of PSQI values will be made.
#' An analysis will be done on these 3 dataframes which will encompass a sensitivity test.

################################################################################
####################### CREATING DATAFRAME WITHOUT PSQI #######################
################################################################################

liver_noPSQI <- liver_dfclean[, -which(names(liver_dfclean) == "Pittsburgh.Sleep.Quality.Index.Score")]

################################################################################
## SEARCHING FOR CORRELATION BETWEEN PSQI AND OTHER VARIABLE(S) USING CORRPLOT #
################################################################################

#' Here we use the cor() function to find the correlation between each variable
#' then we use the corrplot function to crease a plot that will help easily visualize
#' which variable has the strongest correlation with PSQ1. Based on the results,
#' we find that the Athens.Insomnia.Scale has the strongest correlation with
#' PSQ1 and thus we use the lm() function to build a linear regression model between
#' the two variables so that it can then be used along with the predict() function
#' to predict values for NA in the PSQ1 variable column. The values predicted are
#' then used to replace the NA values in the PSQ1 column, AIS is also used in
#'pairwise imputation to predict the missing values for PSQI

correlation2 <- cor(liver_dfclean, use = "pairwise")
corrplot(correlation2, type = "lower", diag = FALSE)

################################################################################
################################################################################

################################################################################
############## CHECKING TO SEE IF MISSING VALUES ARE MCAR ######################
################################################################################


#' To determine whether we can conduct pairwise regression imputation and complete 
#' case analysis to impute missing NA's we split the dataset into two data frames, one which
#' contains only complete case values with respect to PSQI value presence and another which only
#' contains rows that are found as NA's for the PSQI variable. We then take the
#' mean of each variabl for each of the dataframes (with and without PSQI NA's)
#' The means are then used to conduct a t-statistic which tests to see whether
#' there is a significant difference in the mean of each variable btween datasets 
#' which will let us know whether we should consider our missing PSQI data as MCAR or MAR.
#'  If the means are found to be significantly different from each other then this 
#'  tells us that the missing NA values are not MCAR and thus a complete case and
#'  pairwise imputation cannot be used to deal with thee missing values for PSQI.

# List of variables for which you want to compare means
variables_to_compare <- c("Gender", "Age", "BMI", "Time.from.transplant",
                          "Liver.Diagnosis", "Recurrence.of.disease", "Rejection.graft.dysfunction",
                          "Any.fibrosis", "Renal.Failure", "Depression", "Corticoid", "Epworth.Sleepiness.Scale",
                          "Athens.Insomnia.Scale", "Berlin.Sleepiness.Scale",
                          "SF36.PCS", "SF36.MCS")

# Create two data frames, one with NA values and one without NA values in Pittsburgh column
na_rows <- is.na(liver_dfclean$Pittsburgh.Sleep.Quality.Index.Score)
df_with_na <- liver_dfclean[na_rows, ]
df_without_na <- liver_dfclean[!na_rows, ]

# Initialize an empty list to store t-test results for each variable
t_test_results_list <- list()

# Loop through each variable and perform t-test
for (variable in variables_to_compare) {
  t_test_result <- t.test(df_with_na[[variable]], df_without_na[[variable]])
  t_test_results_list[[variable]] <- t_test_result
}

# Print the t-test results for each variable
for (variable in variables_to_compare) {
  cat("Variable:", variable, "\n")
  print(t_test_results_list[[variable]])
  cat("\n")
}

################################################################################
#### CREATING DATAFRAMES FOR EACH METHOD OF DEALING WITH NA'S ##################
################################################################################

#'Creating two datasets, liver_dfcleanCC will have only complete case values
#'and liver_dfcleanPW_IMP which will use pairwise imputation to
#'impute values for PSQI. This will result in now 3 dataframes that can be used 
#'for analysis, one with PSQI completely excluded, one with PSQI included but
#'using complete case approach, and the last with missing values for PSQI imputed
#'using pairwise imputation

liver_dfcleanCC <- liver_dfclean
liver_dfcleanPW_IMP <- liver_dfclean

################################################################################
################################################################################

################################################################################
#### PERFORMING SHAPIRO-WILK TEST ON PSQI TO CHECK IF NORMAL DISTRIBUTION ######
################################################################################

#Before we can conduct pairwise imputation it is crucial to make sure that PSQI
#does not follow a normal distribution

psqi_scores <- liver_dfclean$Pittsburgh.Sleep.Quality.Index.Score
shapiro.test(psqi_scores) #p-value of 4.004e-08 means data dos not follow normal distribution

################################################################################
################################################################################

################################################################################
##################### PERFORMING PAIRWISE IMPUTATION ###########################
################################################################################

# Selecting relevant columns for imputation
imputation_col <- liver_dfclean[c("Pittsburgh.Sleep.Quality.Index.Score", "Athens.Insomnia.Scale")]
# Perform pairwise imputation
PSQIimputations <- mice(imputation_col, method = "pmm", m = 1, maxit = 1, print = FALSE)
PSQI_imputed_values <- complete(PSQIimputations)
liver_dfcleanPW_IMP$Pittsburgh.Sleep.Quality.Index.Score <- PSQI_imputed_values$Pittsburgh.Sleep.Quality.Index.Score

################################################################################
################################################################################

################################################################################
######### SUMMARY OF IMPUTED VALUES AFTER PAIRWISE IMPUTATION ##################
################################################################################

summary(liver_dfcleanPW_IMP$Pittsburgh.Sleep.Quality.Index.Score)
# Summary of the PSQI column using the summary function
summary(liver_dfcleanPW_IMP$Pittsburgh.Sleep.Quality.Index.Score)
# Mean of the PSQI column
mean(liver_dfcleanPW_IMP$Pittsburgh.Sleep.Quality.Index.Score, na.rm = TRUE)
# Median of the PSQI column
median(liver_dfcleanPW_IMP$Pittsburgh.Sleep.Quality.Index.Score, na.rm = TRUE)
# Standard deviation of the PSQI column
sd(liver_dfcleanPW_IMP$Pittsburgh.Sleep.Quality.Index.Score, na.rm = TRUE)

################################################################################
################################################################################

################################################################################
##REMOVING NA VALUES FROM COMPLETE CASE DATAFRAME AND DATAFRAME EXCLUDING PSQI##
################################################################################

#Remove NA values from complete case dataframe
liver_dfcleanCC <- na.omit(liver_dfcleanCC)
liver_noPSQI_rmNA <- na.omit(liver_noPSQI)
################################################################################
################################################################################

################################################################################
################## CONVERTING VARIABLES TO CATEGORICAL #########################
################################################################################

#'Here we convert categorical variables into factors by first assigning all variables
#'needing conversion to "categorical_var", then we implement a for loop which will
#' move through each variable in the categorical_var list and convert them to factors
#' then reassign the converted variable back to the liver_defclean dataset. The
#' temporary 'col' variable aprt of the for loop is assigneed to each variable in
#' the given list one by one until all variables have been converted to factors.
#' This is done for both dataframes liver_dfcleanCC and liver_dfcleanIMP
categorical_var <- c("Gender", "Liver.Diagnosis", "Recurrence.of.disease", "Rejection.graft.dysfunction",
                     "Any.fibrosis", "Renal.Failure", "Depression", "Corticoid", "Berlin.Sleepiness.Scale")
for (col in categorical_var) {
  liver_dfcleanCC[[col]] <- factor(liver_dfcleanCC[[col]])
}

for (col in categorical_var) {
  liver_dfcleanPW_IMP[[col]] <- factor(liver_dfcleanPW_IMP[[col]])
}

for (col in categorical_var) {
  liver_noPSQI_rmNA[[col]] <- factor(liver_noPSQI_rmNA[[col]])
}

#Checking to see if variables were correctly transformed
glimpse(liver_dfcleanPW_IMP) #Looks good
glimpse(liver_dfcleanCC) #Looks good
glimpse(liver_noPSQI_rmNA) #Looks good
#'Using the summary function to check for presence of any values that were not
#'encoded as NA's but look to be out of place
summary(liver_dfcleanPW_IMP) #Looks good
summary(liver_dfcleanCC) #Looks good
summary(liver_noPSQI_rmNA) #Looks good
################################################################################
################################################################################

################################################################################
###### CREATING COLUMNS WITH SLEEP DISTURBANCE VARIABLES AS BINARY #############
################################################################################

#'Here we convert PSQI, ESS and AIS, to binary variables like BSS based on the
#'datasets description for values above a certain level. Epworth Sleepiness Scale
#'total score greater than 10, indicating at least mild excessive daytime sleepiness
#'(1 = yes, 0 = no), Pittsburgh Sleep Quality Index score greater than 4, indicating
#'poor sleep quality (1 = yes, 0 = no),Athens Insomnia Scale total score greater than
#'5, indicating the presence of insomnia, (1 = yes, 0 = no). The new variables are
#'added to the dataframe as new columns using the mutate function from the tidyverse
#'package.

liver_dfcleanCC <- liver_dfcleanCC %>%
  mutate(Epworth_binary = ifelse(Epworth.Sleepiness.Scale > 10, 1, 0),
         Pittsburgh_binary = ifelse(Pittsburgh.Sleep.Quality.Index.Score > 4, 1, 0),
         Athens_binary = ifelse(Athens.Insomnia.Scale > 5, 1, 0))

liver_dfcleanPW_IMP <- liver_dfcleanPW_IMP %>%
  mutate(Epworth_binary = ifelse(Epworth.Sleepiness.Scale > 10, 1, 0),
         Pittsburgh_binary = ifelse(Pittsburgh.Sleep.Quality.Index.Score > 4, 1, 0),
         Athens_binary = ifelse(Athens.Insomnia.Scale > 5, 1, 0))

liver_noPSQI_rmNA <- liver_noPSQI_rmNA %>%
  mutate(Epworth_binary = ifelse(Epworth.Sleepiness.Scale > 10, 1, 0),
         Athens_binary = ifelse(Athens.Insomnia.Scale > 5, 1, 0))

################################################################################
################################################################################

################################################################################
################## CONVERTING BINARY VARIABLES TO FACTORS ######################
################################################################################

#Now we must convert the newly made binary variables to factors for further analysis
categorical_var2 <- c("Epworth_binary", "Pittsburgh_binary", "Athens_binary")
categorical_var3 <- c("Epworth_binary", "Athens_binary")

for (col in categorical_var2) {
  liver_dfcleanCC[[col]] <- factor(liver_dfcleanCC[[col]])
}

for (col in categorical_var2) {
  liver_dfcleanPW_IMP[[col]] <- factor(liver_dfcleanPW_IMP[[col]])
}

for (col in categorical_var3) {
  liver_noPSQI_rmNA[[col]] <- factor(liver_noPSQI_rmNA[[col]])
}

#checking to see if conversion to factors was successful
glimpse(liver_dfcleanCC)#variables successfully converted to factors
glimpse(liver_dfcleanPW_IMP)#variables successfully converted to factors
glimpse(liver_noPSQI_rmNA) #variables successfully converted to factors
################################################################################
################################################################################

################################################################################
####################REMOVING REMAINING NA's#####################################
################################################################################

#Remove NA values from liver_dfcleanPW_IMP dataframe
liver_dfcleanPW_IMPrmNA <- na.omit(liver_dfcleanPW_IMP)

################################################################################
################################################################################

################################################################################
##################### FINAL LIST OF RELEVANT DATAFRAMES#########################
################################################################################

#---liver_dfcleanCC--- omplete case dataframe which includes PSQI variable

#---liver_dfcleanPW_IMP---Dataframe with imputed values for PSQI abut still has NA values present for other variables
#---liver_dfcleanPW_IMPrmNA--- Dataframe with imputed values for PSQI and remaining NA's removed

#---liver_noPSQI--- Dataframe excluding PSQI but still has NA values present for other variables
#---liver_noPSQI_rmNA--- Dataframe with PSQI compleetely removed as a variable and remaining NA's removed

#--------------------------------------------------------------------------------------------------------------
########################################################################################################################################
####################Identifying the relationship between sleep disturbance and quality of life (physical and mental)####################
########################################################################################################################################
# We need to create linear regression models and conduct correlation tests for single predictors
# to investigate whether there is a linear relationship between each measure and outcome (PCS and MCS)
# We begin with the continuous measures
# List of continuous predictors
continuous_predictors <- c("Epworth.Sleepiness.Scale", "Athens.Insomnia.Scale")
# List of binary predictors
binary_predictors <- c("Epworth_binary", "Athens_binary", "Berlin.Sleepiness.Scale")
# List of response variables
responses <- c("SF36.PCS", "SF36.MCS")
#For liver_noPSQI_rmNA
for (predictor in c(continuous_predictors, binary_predictors)) {
  for (response in responses) {
    model_formula <- as.formula(paste(response, "~", predictor))
    model <- lm(model_formula, data = liver_noPSQI_rmNA)
    print(paste("Model: ", response, " ~ ", predictor))
    print(summary(model))
    plot(fitted(model), resid(model), 
         main = paste("Residual Plot for Model:", response, "~", predictor),
         sub = "Dataset: PSQI Removed")    
    # Only run cor.test if both predictor and response are numeric
    if (is.numeric(liver_noPSQI_rmNA[[response]]) & is.numeric(liver_noPSQI_rmNA[[predictor]])) {
      print(cor.test(liver_noPSQI_rmNA[[response]], liver_noPSQI_rmNA[[predictor]]))
    }
  }
}
#For liver_dfcleanPW_IMPrmNA
#Need to now establish PSQI as a variable
continuous_predictors <- c("Epworth.Sleepiness.Scale", "Pittsburgh.Sleep.Quality.Index.Score", "Athens.Insomnia.Scale")
binary_predictors <- c("Epworth_binary", "Pittsburgh_binary", "Athens_binary", "Berlin.Sleepiness.Scale")

for (predictor in c(continuous_predictors, binary_predictors)) {
  for (response in responses) {
    model_formula <- as.formula(paste(response, "~", predictor))
    model <- lm(model_formula, data = liver_dfcleanPW_IMPrmNA)
    print(paste("Model: ", response, " ~ ", predictor))
    print(summary(model))
    plot(fitted(model), resid(model), 
         main = paste("Residual Plot for Model:", response, "~", predictor),
         sub = "Dataset: Pairwise Imputation")
    
    # Only run cor.test if both predictor and response are numeric
    if (is.numeric(liver_dfcleanPW_IMPrmNA[[response]]) & is.numeric(liver_dfcleanPW_IMPrmNA[[predictor]])) {
      print(cor.test(liver_dfcleanPW_IMPrmNA[[response]], liver_dfcleanPW_IMPrmNA[[predictor]]))
    }
  }
}
#For liver_dfcleanCC
for (predictor in c(continuous_predictors, binary_predictors)) {
  for (response in responses) {
    model_formula <- as.formula(paste(response, "~", predictor))
    model <- lm(model_formula, data = liver_dfcleanCC)
    print(paste("Model: ", response, " ~ ", predictor))
    print(summary(model))
    plot(fitted(model), resid(model), 
    main = paste("Residual Plot for Model:", response, "~", predictor),
    sub = "Dataset: Complete Case")    
    # Only run cor.test if both predictor and response are numeric
    if (is.numeric(liver_dfcleanCC[[response]]) & is.numeric(liver_dfcleanCC[[predictor]])) {
      print(cor.test(liver_dfcleanCC[[response]], liver_dfcleanCC[[predictor]]))
    }
  }
}

################################################################################
################## INVESTIGATING NON-PSQI MODEL OUTPUTS ##########################
################################################################################
# Load the MASS library for the stepAIC function
library(MASS)
# Mixed model
library(car) #loading vif function

# Define the PCS null model
noPSQI.PCS.mix.mod.null <- lm(SF36.PCS ~ 1, data = liver_noPSQI_rmNA)

# Define the full model
noPSQI.PCS.mix.mod.full <- lm(SF36.PCS ~ Epworth.Sleepiness.Scale + Athens.Insomnia.Scale + Berlin.Sleepiness.Scale, data = liver_noPSQI_rmNA)

# Perform forward stepwise selection
noPSQI.PCS.mix.step.forw <- stepAIC(noPSQI.PCS.mix.mod.null, direction = "forward", trace = F, scope = list(upper = noPSQI.PCS.mix.mod.full, lower = noPSQI.PCS.mix.mod.null))

# View the final model
summary(noPSQI.PCS.mix.step.forw)

# Check for collinearity
vif(noPSQI.PCS.mix.step.forw)

# Comparing to full variable inclusion
anova(noPSQI.PCS.mix.step.forw, noPSQI.PCS.mix.mod.full)

# Checking AICs to further investigate
AIC(noPSQI.PCS.mix.mod.full)
AIC(noPSQI.PCS.mix.step.forw)

# Define the MCS null model
noPSQI.MCS.mix.mod.null <- lm(SF36.MCS ~ 1, data = liver_noPSQI_rmNA)

# Define the full model
noPSQI.MCS.mix.mod.full <- lm(SF36.MCS ~ Epworth.Sleepiness.Scale  + Athens.Insomnia.Scale + Berlin.Sleepiness.Scale, data = liver_noPSQI_rmNA)

# Perform forward stepwise selection
noPSQI.MCS.mix.step.forw <- stepAIC(noPSQI.MCS.mix.mod.null, direction = "forward", trace = F, scope = list(upper = noPSQI.MCS.mix.mod.full, lower = noPSQI.MCS.mix.mod.null))

# View the final model
summary(noPSQI.MCS.mix.step.forw)

# Check for collinearity
vif(noPSQI.MCS.mix.step.forw)

# Comparing to full variable inclusion
anova(noPSQI.MCS.mix.step.forw, noPSQI.MCS.mix.mod.full)

# We see there is not a statistically significant value for inclusion of PSQI

# Checking AICs to further investigate
AIC(noPSQI.MCS.mix.mod.full)
AIC(noPSQI.MCS.mix.step.forw)

# Plotting residuals for the PCS stepwise model
plot(fitted(noPSQI.PCS.mix.step.forw), residuals(noPSQI.PCS.mix.step.forw),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values for Model SF36.PCS ~ AIS + ESS + BSS")
abline(h = 0, col = "red")  # Adds a horizontal line at y = 0

# Plotting residuals for the MCS stepwise model
plot(fitted(noPSQI.MCS.mix.step.forw), residuals(noPSQI.MCS.mix.step.forw),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values for Model SF36.MCS ~ AIS + ESS")
abline(h = 0, col = "red")  # Adds a horizontal line at y = 0

# Q-Q plot for the PCS stepwise model residuals
qqnorm(residuals(noPSQI.PCS.mix.step.forw))
qqline(residuals(noPSQI.PCS.mix.step.forw))

# Q-Q plot for the MCS stepwise model residuals
qqnorm(residuals(noPSQI.MCS.mix.step.forw))
qqline(residuals(noPSQI.MCS.mix.step.forw))


################################################################################
################## COMPARING IMP vs. CC MODEL OUTPUTS ##########################
################################################################################


################################################################################
###### IMP ANALYSIS #############
################################################################################

# Define the PCS null model
IMP.PCS.mix.mod.null <- lm(SF36.PCS ~ 1, data = liver_dfcleanPW_IMPrmNA)

# Define the full model
IMP.PCS.mix.mod.full <- lm(SF36.PCS ~ Epworth.Sleepiness.Scale + Pittsburgh.Sleep.Quality.Index.Score + Athens.Insomnia.Scale + Berlin.Sleepiness.Scale, data = liver_dfcleanPW_IMPrmNA)

# Perform forward stepwise selection
IMP.PCS.mix.step.forw <- stepAIC(IMP.PCS.mix.mod.null, direction = "forward", trace = F, scope = list(upper = IMP.PCS.mix.mod.full, lower = IMP.PCS.mix.mod.null))

# View the final model
summary(IMP.PCS.mix.step.forw)

# Check for collinearity
vif(IMP.PCS.mix.step.forw)

# Comparing to full variable inclusion
anova(IMP.PCS.mix.step.forw, IMP.PCS.mix.mod.full)

# We see there is not a statistically significant value for inclusion of PSQI

# Checking AICs to further investigate
AIC(IMP.PCS.mix.mod.full)
AIC(IMP.PCS.mix.step.forw)

# Define the MCS null model
IMP.MCS.mix.mod.null <- lm(SF36.MCS ~ 1, data = liver_dfcleanPW_IMPrmNA)

# Define the full model
IMP.MCS.mix.mod.full <- lm(SF36.MCS ~ Epworth.Sleepiness.Scale + Pittsburgh.Sleep.Quality.Index.Score + Athens.Insomnia.Scale + Berlin.Sleepiness.Scale, data = liver_dfcleanPW_IMPrmNA)

# Perform forward stepwise selection
IMP.MCS.mix.step.forw <- stepAIC(IMP.MCS.mix.mod.null, direction = "forward", trace = F, scope = list(upper = IMP.MCS.mix.mod.full, lower = IMP.MCS.mix.mod.null))

# View the final model
summary(IMP.MCS.mix.step.forw)

# Check for collinearity
vif(IMP.MCS.mix.step.forw)

# Comparing to full variable inclusion
anova(IMP.MCS.mix.step.forw, IMP.MCS.mix.mod.full)

# We see there is not a statistically significant value for inclusion of PSQI

# Checking AICs to further investigate
AIC(IMP.MCS.mix.mod.full)
AIC(IMP.MCS.mix.step.forw)

################################################################################
###### CC ANALYSIS #############
################################################################################


# Define the PCS null model
CC.PCS.mix.mod.null <- lm(SF36.PCS ~ 1, data = liver_dfcleanCC)

# Define the full model
CC.PCS.mix.mod.full <- lm(SF36.PCS ~ Epworth.Sleepiness.Scale + Pittsburgh.Sleep.Quality.Index.Score + Athens.Insomnia.Scale + Berlin.Sleepiness.Scale, data = liver_dfcleanCC)

# Perform forward stepwise selection
CC.PCS.mix.step.forw <- stepAIC(CC.PCS.mix.mod.null, direction = "forward", trace = F, scope = list(upper = CC.PCS.mix.mod.full, lower = CC.PCS.mix.mod.null))

# View the final model
summary(CC.PCS.mix.step.forw)

# Check for collinearity
vif(CC.PCS.mix.step.forw)
vif(CC.PCS.mix.mod.full)
# Comparing to full variable inclusion
anova(CC.PCS.mix.step.forw, CC.PCS.mix.mod.full)

# We see there is not a statistically significant value for inclusion of PSQI

# Checking AICs to further investigate
AIC(CC.PCS.mix.mod.full)
AIC(CC.PCS.mix.step.forw)

# Define the MCS null model
CC.MCS.mix.mod.null <- lm(SF36.MCS ~ 1, data = liver_dfcleanCC)

# Define the full model
CC.MCS.mix.mod.full <- lm(SF36.MCS ~ Epworth.Sleepiness.Scale + Pittsburgh.Sleep.Quality.Index.Score + Athens.Insomnia.Scale + Berlin.Sleepiness.Scale, data = liver_dfcleanCC)

# Perform forward stepwise selection
CC.MCS.mix.step.forw <- stepAIC(CC.MCS.mix.mod.null, direction = "forward", trace = F, scope = list(upper = CC.MCS.mix.mod.full, lower = CC.MCS.mix.mod.null))

# View the final model
summary(CC.MCS.mix.step.forw)

# Check for collinearity
vif(CC.MCS.mix.step.forw)

# Comparing to full variable inclusion
anova(CC.MCS.mix.step.forw, CC.MCS.mix.mod.full)

# We see there is not a statistically significant value for inclusion of PSQI

# Checking AICs to further investigate
AIC(CC.MCS.mix.mod.full)
AIC(CC.MCS.mix.step.forw)

