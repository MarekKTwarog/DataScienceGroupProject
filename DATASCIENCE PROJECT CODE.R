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

################################################################################
############################ LOADING DATASET ###################################
################################################################################

liver_df <- read.csv("project_data.csv")

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
#'was used to check p_na values for each variable, values with p_na <0.2 indicate
#'data for variable has less than 20% missing values. Status() also tells us how
#'many unique values are present per variable, this can help easily identify
#'which variables should be categorical.

glimpse(liver_dfclean)
status(liver_dfclean)# p_na values for PSQI variabee is 0.317164179 which is high, the NA values will need to be imputatd
dim(liver_dfclean)

################################################################################
########################## INSTALLING CORRPLOT PACKAGE #########################
################################################################################

#Installling corrplot package so I can generate a correlation plot so I can determine
#' which variable has the highest correlation with PSQ1 so I can then create a linear
#' regression model between the two variables as well as pairwise imputation to predict
#' NA values for PSQ1

install.packages("corrplot")
library(corrplot)

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

correlation <- cor(liver_dfclean, use = "pairwise")
corrplot(correlation, type = "lower", diag = FALSE)

################################################################################
################################################################################

################################################################################
############## CHECKING TO SEE IF MISSING VALUES ARE MCAR ######################
################################################################################

#************CHECK THIS SUMMARY OVER, SINCE IN SLIDES MCAR DOES NOT EQUAL REGRESSION?
#' To determine whether we can conduct a linear regression model to impute missing NA
#' values for the PSQI variable, we split the dataset into two data frames, one which
#' contains only complete case values with respect to PSQI and another which only
#' contains rows that are found as NA's for the PSQI variable. We then take the
#' mean of the variables "Athens.Insomnia.Scale", "Berlin.Sleepiness.Scale", and
#' "Epworth.Sleepiness.Scale" for each of the dataframes (with and without PSQI NA's)
#' The means are then used to conduct a t-statistic which tests to see whether
#' there is a significant difference in means which will let us know whether we should
#' consider our missing PSQI data as MCAR or MAR. If the means are found to be
#' significantly different from each other then this tells us that the missing NA
#' values are not MCAR and thus a linear regression model cannot be used to
#' impute the missing NA values for PSQ1.

# Create two data frames, one with NA values and one without NA values in Pittsburgh column
na_rows <- is.na(liver_dfclean$Pittsburgh.Sleep.Quality.Index.Score)
df_with_na <- liver_dfclean[na_rows, ]
df_without_na <- liver_dfclean[!na_rows, ]
# Calculate means for variables in df_with_na
mean_with_na <- colMeans(df_with_na[c("Athens.Insomnia.Scale", "Berlin.Sleepiness.Scale", "Epworth.Sleepiness.Scale")], na.rm = TRUE)
mean_with_na
# Calculate means for variables in df_without_na
mean_without_na <- colMeans(df_without_na[c("Athens.Insomnia.Scale", "Berlin.Sleepiness.Scale", "Epworth.Sleepiness.Scale")], na.rm = TRUE)
mean_without_na
# Perform t-test
t_test_result <- t.test(mean_with_na, mean_without_na)
# Print the t-test result
print(t_test_result)

#'we employed a Welch Two Sample t-test to investigate potential differences in
#'the means of three sleep-related variables, namely "Athens.Insomnia.Scale,"
#'"Berlin.Sleepiness.Scale," and "Epworth.Sleepiness.Scale," between two separate
#'dataframes, "mean_with_na" and "mean_without_na." The t-test was performed to
#'ascertain whether there is a statistically significant disparity in the means
#'of these variables across the two dataframes. The t-test results indicated a
#'small t-value of 0.12858, with degrees of freedom (df) calculated as 3.9558.
#'The corresponding p-value was found to be 0.904, suggesting no significant
#'difference in means. Additionally, the 95 percent confidence interval (-9.294151, 10.192629)
#'included 0, further supporting the null hypothesis of no significant difference.
#'Consequently, we conclude that there is no evidence to reject the null hypothesis,
#'indicating that the means of the three sleep-related variables are not significantly
#'different between the two dataframes. Therefore, because the means of variables
#'are not significantly different from each other they can be considered MCAR and so
#'it is safe to conduct a linear regression model to impute missing values for
#'Pittsburgh.Sleep.Quality.Index.Score.

################################################################################
#### CREATING DATAFRAMES FOR EACH METHOD OF DEALING WITH NA'S ##################
################################################################################

#'Creating three datasets, liver_dfcleanCC will have only complete case values
#'liver_dfcleanGLM_IMP will have imputed values for PSQI using linear regression
#'for imputation, and liver_dfcleanPW_IMP which will use pariwise imputation to
#'imput values for PSQI

liver_dfcleanCC <- liver_dfclean
liver_dfcleanGLM_IMP <- liver_dfclean
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
################ IMPUTATION VIA LINEAR REGRESSION MODEL ########################
################################################################################

lm_model <- lm(Pittsburgh.Sleep.Quality.Index.Score ~ Athens.Insomnia.Scale, data = liver_dfclean, na.action = na.exclude)
liver_dfcleanGLM_IMP$PredictedPSQ1 <- predict(lm_model, newdata = liver_dfclean)
# Replace NA values in 'PSQ1' with their respective 'y_predicted' values
liver_dfcleanGLM_IMP$Pittsburgh.Sleep.Quality.Index.Score[is.na(liver_dfcleanGLM_IMP$Pittsburgh.Sleep.Quality.Index.Score)] <- liver_dfcleanGLM_IMP$PredictedPSQ1[is.na(liver_dfcleanGLM_IMP$Pittsburgh.Sleep.Quality.Index.Score)]

################################################################################
################################################################################

################################################################################
######### SUMMARY OF PSQI VALUES AFTER LINEEAR REGRESSION IMPUTATION ###########
################################################################################

summary(liver_dfcleanGLM_IMP$Pittsburgh.Sleep.Quality.Index.Score)
# Summary of the PSQI column using the summary function
summary(liver_dfcleanGLM_IMP$Pittsburgh.Sleep.Quality.Index.Score)
# Mean of the PSQI column
mean(liver_dfcleanGLM_IMP$Pittsburgh.Sleep.Quality.Index.Score, na.rm = TRUE)
# Median of the PSQI column
median(liver_dfcleanGLM_IMP$Pittsburgh.Sleep.Quality.Index.Score, na.rm = TRUE)
# Standard deviation of the PSQI column
sd(liver_dfcleanGLM_IMP$Pittsburgh.Sleep.Quality.Index.Score, na.rm = TRUE)

################################################################################
################################################################################

################################################################################
############## REMOVING NA VALUES FROM COMPLETE CASE DATAFRAME #################
################################################################################

#Remove NA values from complete case dataframe
liver_dfcleanCC <- na.omit(liver_dfcleanCC)

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
  liver_dfcleanGLM_IMP[[col]] <- factor(liver_dfcleanGLM_IMP[[col]])
}

for (col in categorical_var) {
  liver_dfcleanPW_IMP[[col]] <- factor(liver_dfcleanPW_IMP[[col]])
}

#Checking to see if variables were correctly transformed
glimpse(liver_dfcleanGLM_IMP) #Looks good
glimpse(liver_dfcleanPW_IMP) #Looks good
glimpse(liver_dfcleanCC) #Looks good

#'Using the summary function to check for presence of any values that were not
#'encoded as NA's but look to be out of place
summary(liver_dfcleanGLM_IMP) #Looks good
summary(liver_dfcleanPW_IMP) #Looks good
summary(liver_dfcleanCC) #Looks good

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

liver_dfcleanGLM_IMP <- liver_dfcleanGLM_IMP %>%
  mutate(Epworth_binary = ifelse(Epworth.Sleepiness.Scale > 10, 1, 0),
         Pittsburgh_binary = ifelse(Pittsburgh.Sleep.Quality.Index.Score > 4, 1, 0),
         Athens_binary = ifelse(Athens.Insomnia.Scale > 5, 1, 0))

liver_dfcleanCC <- liver_dfcleanCC %>%
  mutate(Epworth_binary = ifelse(Epworth.Sleepiness.Scale > 10, 1, 0),
         Pittsburgh_binary = ifelse(Pittsburgh.Sleep.Quality.Index.Score > 4, 1, 0),
         Athens_binary = ifelse(Athens.Insomnia.Scale > 5, 1, 0))

liver_dfcleanPW_IMP <- liver_dfcleanPW_IMP %>%
  mutate(Epworth_binary = ifelse(Epworth.Sleepiness.Scale > 10, 1, 0),
         Pittsburgh_binary = ifelse(Pittsburgh.Sleep.Quality.Index.Score > 4, 1, 0),
         Athens_binary = ifelse(Athens.Insomnia.Scale > 5, 1, 0))

################################################################################
################################################################################

################################################################################
################## CONVERTING BINARY VARIABLES TO FACTORS ######################
################################################################################

#Now we must convert the newly made binary variables to factors for further analysis
categorical_var2 <- c("Epworth_binary", "Pittsburgh_binary", "Athens_binary")

for (col in categorical_var2) {
  liver_dfcleanGLM_IMP[[col]] <- factor(liver_dfcleanGLM_IMP[[col]])
}

for (col in categorical_var2) {
  liver_dfcleanCC[[col]] <- factor(liver_dfcleanCC[[col]])
}

for (col in categorical_var2) {
  liver_dfcleanPW_IMP[[col]] <- factor(liver_dfcleanPW_IMP[[col]])
}

#checking to see if conversion to factors was successful
glimpse(liver_dfcleanGLM_IMP)#variables successfully converted to factors
glimpse(liver_dfcleanCC)#variables successfully converted to factors
glimpse(liver_dfcleanPW_IMP)#variables successfully converted to factors

################################################################################
################################################################################

################################################################################
####################REMOVING REMAINING NA's#####################################
################################################################################

#Remove NA values from liver_dfcleanPW_IMP dataframe
liver_dfcleanGLM_IMPrmNA <- na.omit(liver_dfcleanGLM_IMP)

#Remove NA values from liver_dfcleanPW_IMP dataframe
liver_dfcleanPW_IMPrmNA <- na.omit(liver_dfcleanPW_IMP)

################################################################################
################################################################################

