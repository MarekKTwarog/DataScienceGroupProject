################################################################################
############# Team Project BTC 1859H: Data Science in Health I #################
######## Leen Madani, Maryaah Salyani, Owen Treleaven, Marek Twarog ############
####################### August 15, 2023 at 5PM #################################
################################################################################

################################################################################
################## Goal 1: Description of relevant data ########################
################################################################################

#Loading packages for data cleaning / analysis of data

install.packages("tidyverse")
library(tidyverse)
install.packages("funModeling")
library(funModeling)
install.packages("mice")
library(mice)
install.packages("corrplot")
library(corrplot)
install.packages("dplyr")
library(dplyr)
install.packages("carData")
library(carData)
install.packages("MASS")
library(MASS)
install.packages("gridExtra")
library(gridExtra)
install.packages("car")
library(car)

################################################################################
############################ LOADING DATASET ###################################
################################################################################

liver_df <- read.csv("project_data.csv")
glimpse(liver_df) #quick look at the data

################################################################################
################################################################################

################################################################################
###### CREATING DATAFRAME WITH VARIABLES TO BE USED FOR ANALYSIS ###############
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
#' threshold meaning the variable will have to be removed from analysis to prevent
#' inaccurate results. Thus a new data frame excluding PSQI will be made but also
#' a complete case analysis will be performed which includes PSQI as a variable,
#' and another data frame which involves imputation of PSQI values will be made.
#' An analysis will be done on these 3 data frames which will encompass a sensitivity test.

################################################################################
####################### CREATING DATA FRAME WITHOUT PSQI #######################
################################################################################

liver_noPSQI <- liver_dfclean[, -which(names(liver_dfclean) == "Pittsburgh.Sleep.Quality.Index.Score")]

################################################################################
## SEARCHING FOR CORRELATION BETWEEN PSQI AND OTHER VARIABLE(S) USING CORRPLOT #
################################################################################

#' Here we use the cor() function to find the correlation between each variable
#' then we use the corrplot function to create a plot that will help easily visualize
#' which variable has the strongest correlation with PSQ1. Based on the results,
#' we find that the Athens.Insomnia.Scale has the strongest correlation with
#' PSQ1 and thus we use multiple imputation via predictive mean matching method 
#' with AIS as the predictor to predict values for NA in the PSQ1 variable column. 
#'The values predicted are then used to replace the NA values in the PSQ1 column.

correlation2 <- cor(liver_dfclean, use = "pairwise")
corrplot(correlation2, type = "lower", diag = FALSE) #diag removes have the corr plot leaving only a diagonal half of the plot making it easier to read

################################################################################
################################################################################

################################################################################
############## CHECKING TO SEE IF MISSING VALUES ARE MCAR ######################
################################################################################


#' To determine whether we can conduct complete case analysis we must check to see 
#' if data is MCAR. We split the dataset into two data frames, one which contains 
#' only complete case values with respect to PSQI value presence and another which only
#' contains rows that are found as NA's for the PSQI variable. We then take the
#' mean of each variable for each of the data frames (with and without PSQI NA's)
#' The means are then used to conduct a t-statistic which tests to see whether
#' there is a significant difference in the mean of each variable btween datasets 
#' which will let us know whether we should consider our missing PSQI data as MCAR or MAR.
#'  If the means are found to be significantly different from each other then this 
#'  tells us that the missing NA values are not MCAR and thus a complete case approach 
#' cannot be used to deal with the missing values for PSQI.

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

# Creating a list that will store t-test results for each variable
t_test_results_list <- list()

# Using a for loop to iterate through each variable and perform t-test
for (variable in variables_to_compare) {
  t_test_result <- t.test(df_with_na[[variable]], df_without_na[[variable]])
  t_test_results_list[[variable]] <- t_test_result
}

# Printing the t-test results for each variable
for (variable in variables_to_compare) {
  cat("Variable:", variable, "\n")
  print(t_test_results_list[[variable]])
  cat("\n")
}

################################################################################
#### CREATING DATAFRAMES FOR EACH METHOD OF DEALING WITH NA'S ##################
################################################################################

#'Creating two datasets, liver_dfcleanCC will have only complete case values
#'and liver_dfcleanPMM_IMP which will contain imputed values for PSQI. 
#' This will result in now 3 data frames that can be used for analysis, one with PSQI 
#' completely excluded, one with PSQI included but using complete case approach, 
#' and the last with missing values for PSQI imputed using multiple imputation.

liver_dfcleanCC <- liver_dfclean
liver_dfcleanPMM_IMP <- liver_dfclean

################################################################################
################################################################################

################################################################################
###########MULTIPLE IMPUTATION VIA PREDICTIVE MEAN MATCHING METHOD #############
################################################################################

# Select columns for imputation
imputation_col <- liver_dfclean[c("Pittsburgh.Sleep.Quality.Index.Score", "Athens.Insomnia.Scale")]
# Performing PMM imputation
PSQIimputations <- mice(imputation_col, method = "pmm", m = 31, maxit = 10, print = FALSE)
# Using the complete function on the mids object to get imputed values
PSQI_imputed_values <- complete(PSQIimputations)
# Calculating the means of the imputed values
mean_imputed <- rowMeans(PSQI_imputed_values)
# Assigning the pooled imputed values to liver_dfcleanPMM_IMP dataframe
liver_dfcleanPMM_IMP$Pittsburgh.Sleep.Quality.Index.Score <- mean_imputed

#We select 31 as our value for m since as a rule of thumb the amount of imputed
#datasets you want should equal the percentage of missing data for the variable
#being imputed. The max iteration value is set to 10 since values from 10-20 are
#usually recommended. The newly imputed values in each of the 31 datasets created
#then have their means calculated and the mean of each imputed mssing value is assigned
#to the dataset liver_dfcleanPMM_IMP 

################################################################################
################################################################################

################################################################################
###############REMOVING REMAINING NA VALUES DATA FRAMES#########################
################################################################################

#Remove NA values from complete case dataframe
liver_dfcleanCC <- na.omit(liver_dfcleanCC)
liver_noPSQI_rmNA <- na.omit(liver_noPSQI)
liver_dfcleanPMM_IMPrmNA <- na.omit(liver_dfcleanPMM_IMP)
################################################################################
################################################################################

################################################################################
################## CONVERTING VARIABLES TO CATEGORICAL #########################
################################################################################

#'Here we convert categorical variables into factors by first assigning all variables
#'needing conversion to "categorical_var", then we implement a for loop which will
#' move through each variable in the categorical_var list and convert them to factors
#' then reassign the converted variable back to each data frame. The
#' temporary 'col' variable apart of the for loop is assigned to each variable in
#' the given list one by one until all variables have been converted to factors.
#' This is done for all of the data frames.
categorical_var <- c("Gender", "Liver.Diagnosis", "Recurrence.of.disease", "Rejection.graft.dysfunction",
                     "Any.fibrosis", "Renal.Failure", "Depression", "Corticoid", "Berlin.Sleepiness.Scale")
for (col in categorical_var) {
  liver_dfcleanCC[[col]] <- factor(liver_dfcleanCC[[col]])
}

for (col in categorical_var) {
  liver_dfcleanPMM_IMP[[col]] <- factor(liver_dfcleanPMM_IMP[[col]])
}

for (col in categorical_var) {
  liver_dfcleanPMM_IMPrmNA[[col]] <- factor(liver_dfcleanPMM_IMPrmNA[[col]])
}

for (col in categorical_var) {
  liver_noPSQI[[col]] <- factor(liver_noPSQI[[col]])
}

for (col in categorical_var) {
  liver_noPSQI_rmNA[[col]] <- factor(liver_noPSQI_rmNA[[col]])
}

#Checking to see if variables were correctly transformed
glimpse(liver_dfcleanPMM_IMP) #Looks good
glimpse(liver_dfcleanCC) #Looks good
glimpse(liver_noPSQI_rmNA) #Looks good
glimpse(liver_noPSQI) #Looks good
glimpse(liver_dfcleanPMM_IMPrmNA) #Looks good

#'Using the summary function to check for presence of any values that were not
#'encoded as NA's but look to be out of place
summary(liver_dfcleanPMM_IMP) #Looks good
summary(liver_dfcleanCC) #Looks good
summary(liver_noPSQI_rmNA) #Looks good
summary(liver_noPSQI) #Looks good
summary(liver_dfcleanPMM_IMPrmNA) #Looks good

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
#'added to the data frames as new columns using the mutate function from the tidyverse
#'package.

liver_dfcleanCC <- liver_dfcleanCC %>%
  mutate(Epworth_binary = ifelse(Epworth.Sleepiness.Scale > 10, 1, 0),
         Pittsburgh_binary = ifelse(Pittsburgh.Sleep.Quality.Index.Score > 4, 1, 0),
         Athens_binary = ifelse(Athens.Insomnia.Scale > 5, 1, 0))

liver_dfcleanPMM_IMP <- liver_dfcleanPMM_IMP %>%
  mutate(Epworth_binary = ifelse(Epworth.Sleepiness.Scale > 10, 1, 0),
         Pittsburgh_binary = ifelse(Pittsburgh.Sleep.Quality.Index.Score > 4, 1, 0),
         Athens_binary = ifelse(Athens.Insomnia.Scale > 5, 1, 0))

liver_dfcleanPMM_IMPrmNA <- liver_dfcleanPMM_IMPrmNA %>%
  mutate(Epworth_binary = ifelse(Epworth.Sleepiness.Scale > 10, 1, 0),
         Pittsburgh_binary = ifelse(Pittsburgh.Sleep.Quality.Index.Score > 4, 1, 0),
         Athens_binary = ifelse(Athens.Insomnia.Scale > 5, 1, 0))

liver_noPSQI_rmNA <- liver_noPSQI_rmNA %>%
  mutate(Epworth_binary = ifelse(Epworth.Sleepiness.Scale > 10, 1, 0),
         Athens_binary = ifelse(Athens.Insomnia.Scale > 5, 1, 0))

liver_noPSQI <- liver_noPSQI %>%
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
  liver_dfcleanPMM_IMP[[col]] <- factor(liver_dfcleanPMM_IMP[[col]])
}

for (col in categorical_var2) {
  liver_dfcleanPMM_IMPrmNA[[col]] <- factor(liver_dfcleanPMM_IMPrmNA[[col]])
}

for (col in categorical_var3) {
  liver_noPSQI_rmNA[[col]] <- factor(liver_noPSQI_rmNA[[col]])
}

for (col in categorical_var3) {
  liver_noPSQI[[col]] <- factor(liver_noPSQI[[col]])
}

#checking to see if conversion to factors was successful
glimpse(liver_dfcleanCC)#variables successfully converted to factors
glimpse(liver_dfcleanPMM_IMP)#variables successfully converted to factors
glimpse(liver_noPSQI_rmNA) #variables successfully converted to factors
glimpse(liver_noPSQI) #variables successfully converted to factors
glimpse(liver_dfcleanPMM_IMPrmNA) #variables successfully converted to factors

################################################################################
################################################################################

################################################################################
##################### FINAL LIST OF RELEVANT DATAFRAMES#########################
################################################################################

#---liver_dfcleanCC--- Complete case data frame which includes PSQI variable

#---liver_dfcleanPMM_IMP---Dataframe with imputed values for PSQI but still has NA values present for other variables
#---liver_dfcleanPMM_IMPrmNA--- Dataframe with imputed values for PSQI and remaining NA's removed

#---liver_noPSQI--- Dataframe excluding PSQI but still has NA values present for other variables
#---liver_noPSQI_rmNA--- Dataframe with PSQI completely removed as a variable and remaining NA's removed

################################################################################
##################### PREDICTORS ASSOCIATED WITH SLEEP DISTURBANCE##############
################################################################################

################################################################################
#### Goal 3A: find the predictors associated with sleep disturbance for the liver_dfcleanCC data frame
################################################################################

# Fit the full logistic regression model with all predictors
# logistic regression model was used instead of the linear model because we have binary variables.
PSQI_full_CC <- glm(Pittsburgh_binary~
                      Gender + Age + BMI + Time.from.transplant + Liver.Diagnosis +
                      Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis +
                      Renal.Failure + Depression + Corticoid, data = na.omit(liver_dfcleanCC), family = "binomial")
# using the glimpse function to view a concise overview of the data frame
glimpse(PSQI_full_CC)
# using the stepAIC function to perform the backwards stepwise selection process which eliminates predictors on the basis of AIC
PSQI.step.back.CC <- stepAIC(PSQI_full_CC)
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


################################################################################
#### Goal 3B: find the predictors associated with sleep disturbance for the liver_dfcleanPMM_IMPrmNA data frame
################################################################################

# Fit the full logistic regression model with all predictors
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
glimpse(AIS_full_IMPrmNA)
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


################################################################################
#### Goal 3C: find the predictors associated with sleep disturbance for the liver_noPSQI_rmNA data frame
################################################################################

#Loading dataset
liver_df <- read.csv("project_data.csv")

# finding the predictors associated with sleep for the ESS measure
# Fit the full logistic regression model with all predictors
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

################################################################################
############### Goal 2: Estimation of the prevalence of sleep disturbance ######
################################################################################

################################################################################
#### Approach 1#################################################################
################################################################################

# Attach dataframe with complete cases to avoid referencing it every time you use it
attach(liver_noPSQI_rmNA)

# Calculate the prevalence of sleep disturbance using the 4 measures using prop.table() function and view results in a table
prevalence1 <- prop.table(table(Berlin.Sleepiness.Scale, Epworth_binary, Athens_binary))

# Extract the table
write.csv(prevalence1, "prevalence1.csv")

# We want to plot the results we got for when we have sleep disturbance based on one scale and complete case dataset
prevalence1 <- data.frame(
  Scale = c("AIS", "BSS", "ESS"),
  Prevalence.Percent = c(23.79, 9, 6.15)
)

# Install and load ggplot2 for generating graphs
#install.packages("ggplot2")
library(ggplot2)
# Create the bar plot with color instead of fill
approach1 <- ggplot(data = prevalence1, aes(x = Scale, y = Prevalence.Percent, fill= Scale)) +
  geom_bar(stat = "identity") +
  theme_light() +                                                                   # theme_ light will select a white background for the graph
  geom_text(aes(label = paste(round(Prevalence.Percent, 2), "%")),                  #geom_text allows to put labels and text on the graph itself
            vjust = -1, size = 4, color = "black") +                                # adjust the vertical positioning using vjust = and set the size using size =
  scale_y_continuous(limits = c(0, 100), name = "Prevalence (%)") +                 # set the y-scale from 0 to 100, with 100 being the highest prevalance and name the y-axis using name =
  labs(title = "Prevalence of Sleep Disturbance Across Scales Using Complete Case", # give a title to the graph using title =
       x = "Sleep Disturbance Scale") +                                            # if x = is not included, the x-axis name will be just "Scale".
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +                        # axis.text.x = will rotate the x-axis labels and hjust will alter it position
  guides(fill = "none")                                                     # this removes the fill legend because its unnecessary

################################################################################
#### Approach 2#################################################################
################################################################################

#' The results of prevalence when doing it all together is not the most ideal way to do it because each scale has each scale has a different number of observations with missing values
#' Thus, we will try to calculate the prevalence by taking into consideration the observations without missing values for each scale seperately
#' Detach liver_noPSQI_rmNA as we need to use the previous dataframe where NA's where not omitted yet (available case analysis): liver_noPSQI.
detach(liver_noPSQI_rmNA)

attach(liver_noPSQI)

# Check the number of observations with missing values for each scale
missing_berlin <- sum(is.na(Berlin.Sleepiness.Scale))

# Calculate the number of cases (positive outcomes) for sleep disturbance for each scale
berlin_cases <- sum(Berlin.Sleepiness.Scale == 1, na.rm=TRUE)

# Calculate the total number of valid observations for each scale
berlin_pop <- sum(!is.na(Berlin.Sleepiness.Scale))  # equal to : nrow(liver_dfclean2) - missing_berlin & to this sum(complete.cases(Berlin.Sleepiness.Scale))

# Calculate the prevalence of sleep disturbance with confidence interval set at 0.95 for each scale using prop.test()
prev_berlin <- prop.test(x = berlin_cases, n = berlin_pop, conf.level = 0.95)

# Instead of doing it for every measure, i will use a loop that will do it for each sleep disturbance measure.
# Create a list of sleep disturbance variables using list()
sleep_vars <- list(
  BSS = Berlin.Sleepiness.Scale,
  ESS = Epworth_binary,
  AIS = Athens_binary
)

# Initialize an empty dataframe to store results
prevalence2 <- data.frame()

# Loop through sleep disturbance variables
for (scale_name in names(sleep_vars)) {
  scale_values <- sleep_vars[[scale_name]]
  
  # Calculate prevalence using prop.test
  cases <- sum(scale_values == 1, na.rm = TRUE)
  population <- sum(!is.na(scale_values))
  missing_values <- sum(is.na(scale_values))
  
  # Calculate prevalence and confidence intervals using prop.test
  prev_result <- prop.test(x = cases, n = population, conf.level = 0.95)
  
  # Create a row for the dataframe
  result <- data.frame(
    Scale = scale_name,
    Cases = cases,
    Population = population,
    Missing.Values = missing_values,
    Prevalence.Percent = prev_result$estimate * 100,
    CI.Lower = prev_result$conf.int[1] * 100,
    CI.Upper = prev_result$conf.int[2] * 100
  )
  
  # Append the scale result to the dataframe and use rbind to join multiple rows created above.
  prevalence2 <- rbind(prevalence2, result)
}

prevalence2<- prevalence2[sort.list(prevalence2$Prevalence.Percent),]
prevalence2$rank <- 1:nrow(prevalence2 )

approach2 <- ggplot(data = prevalence2, aes(x = Scale, y = Prevalence.Percent, fill = Scale)) +
  geom_bar(stat = "identity") +
  theme_light() +
  geom_errorbar(aes(ymin = CI.Lower , ymax = CI.Upper), width = 0.2) +
  geom_text(aes(label = paste(round(Prevalence.Percent, 2), "%")),    # round the prevalence to 2 decimal places
            vjust = -4, size = 4, color = "black") +
  scale_y_continuous(limits = c(0, 100), name = "Prevalence (%)") +
  labs(title = "Prevalence of Sleep Disturbance Across Scales Using Available Case",
       x = "Sleep Disturbance Scale",
       caption = "*Estimation procedure capture true prevalence in this range 95% of the time.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(size = 9, color = "gray", hjust = 0)) +
  guides(fill = "none") # Remove the fill legend because its unnecessary

detach(liver_noPSQI)

prev_plots <- grid.arrange(approach1, approach2, ncol = 2)
ggsave("prev_plots.png", prev_plots, width = 14, height = 6, dpi = 300)

################################################################################
###########################Experimenting: Supplementary#########################
################################################################################

#' We're interested to investigate the likelihood that an individual with sleep disturbance
#' ...(according to the sleep disturbance scale scores) will also have low quality of life (based on the SF-36 PCS = & MCS scores).
#' the exact threshold for defining "low" quality of life using the SF-36 PCS and MCS scores is subjective. Therefore, a pragmatic
#' approach was adopted: scores one standard deviation or more below the averages were taken to indicate low quality of life.
#' For PCS, scores of ~33 or below indicate low quality of life, and for MCS, a score of ~36 or below.
#' Event A: Low quality of life defined by a chosen threshold
#' Event B: Sleep disturbance based on specific scale
#' P(A|B) = = Number of individuals with both A and B / Number of individuals with B
#' Define the function to generate a contingency table
generate_contingency_table <- function(scale_column, sleep_column, sf36_threshold) {
  sleep_disturbance <- liver_noPSQI_rmNA[[scale_column]] == 1
  low_quality_sf36 <- liver_noPSQI_rmNA[[sf36_column]] <= sf36_threshold
  
  contingency_table <- table(sleep_disturbance, low_quality_sf36)
  return(contingency_table)
}

# Set the SF36 thresholds
sf36_pcs_threshold <- 33
sf36_mcs_threshold <- 36

# List of sleep disturbance scales and SF36 columns
sleep_scale_columns <- c("Epworth_binary", "Athens_binary", "Berlin.Sleepiness.Scale")
sf36_columns <- c("SF36.PCS", "SF36.MCS")


# Initialize chi-square results as a list
chi_square_results <- list()

# Generate and save contingency tables
for (scale_column in sleep_scale_columns) {
  for (sf36_column in sf36_columns) {
    if (sf36_column == "SF36.PCS") {
      sf36_threshold <- sf36_pcs_threshold
    } else {
      sf36_threshold <- sf36_mcs_threshold
    }
    
    contingency_table <- generate_contingency_table(scale_column, sf36_column, sf36_threshold)
    
    # Create the file name for the contingency table
    file_name <- paste(scale_column,  sf36_column,"contingency_table.csv", sep = "_")
    
    # Save the contingency table as a CSV file
    write.csv(contingency_table, file_name)
    
    # Perform chi-square test
    chi_square_result <- chisq.test(contingency_table)
    chi_square_results[[scale_column]][[sf36_column]] <- chi_square_result
  }
}

# Double check that the answers above are correct by checking number of participants with low quality of life and those with sleep disturbance seperately
table(liver_noPSQI_rmNA$SF36.MCS)
table(liver_noPSQI_rmNA$Athens_binary)

# Create a data frame with the chi-square results
chi_square_res <- data.frame(
  Scale = rep(c("BSS", "ESS", "AIS"), each = 2),
  Low_Quality_Measure = rep(c("SF36.PCS", "SF36.MCS"), times = 3),
  X_squared = c(11.116, 1.5749, 11.268, 10.58, 6.3135, 24.347),
  p_value = c(0.0008558, 0.2095, 0.0007884, 0.001143, 0.01198, 8.044e-07)
)

# Create a bar plot for the x^2 values
x_plot <- ggplot(chi_square_res, aes(x = Scale, y = X_squared, fill = Low_Quality_Measure)) +
  geom_bar(stat = "identity", position = "dodge") + # dodge is selected to have the MCS and PCS for each scale beside each other
  labs(title = " X^2 from Chi-squared Test Results",
       x = "Sleep Disturbance Scale",
       y = "X-squared Value",
       fill = "Low Quality of Life Measure") +
  theme_light()+
  guides(fill = "none") # Remove the fill legend because I will add it in the p-value plot

x_plot # not informative!

# Create a dot plot for p-values
p_value_plot <- ggplot(chi_square_res, aes(x = Scale, y = p_value, color = Low_Quality_Measure)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) + # adjusted the position of data points because ESS points overlap
  labs(title = "P-values from Chi-squared Test Results",
       x = "Sleep Disturbance Scale",
       y = "P-value",
       color = "Low Quality of Life Measure") +
  theme_light()

p_value_plot

ggsave("p_value_plot.png", p_value_plot, width = 8, height = 6, dpi = 300)

# Experimenting and creating a forest plot-like visualization
forest_plot <- ggplot(chi_square_res, aes(x = Scale, y = X_squared)) +
  geom_point(aes(color = Low_Quality_Measure), size = 3) +
  geom_text(aes(label = sprintf("p = %.4f", p_value)), nudge_y = 0.2, size = 3, hjust = -0.1) +
  labs(title = "Chi-squared Test Results",
       x = "Sleep Disturbance Scale",
       y = "X-squared Value",
       color = "Low Quality of Life Measure") +
  theme_light()
forest_plot

ggsave("forestplot_experimenting.png", forest_plot, width = 8, height = 6, dpi = 300)

################################################################################
################################################################################

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
#For liver_dfcleanPMM_IMPrmNA
#Need to now establish PSQI as a variable
continuous_predictors <- c("Epworth.Sleepiness.Scale", "Pittsburgh.Sleep.Quality.Index.Score", "Athens.Insomnia.Scale")
binary_predictors <- c("Epworth_binary", "Pittsburgh_binary", "Athens_binary", "Berlin.Sleepiness.Scale")

for (predictor in c(continuous_predictors, binary_predictors)) {
  for (response in responses) {
    model_formula <- as.formula(paste(response, "~", predictor))
    model <- lm(model_formula, data = liver_dfcleanPMM_IMPrmNA)
    print(paste("Model: ", response, " ~ ", predictor))
    print(summary(model))
    plot(fitted(model), resid(model), 
         main = paste("Residual Plot for Model:", response, "~", predictor),
         sub = "Dataset: Pairwise Imputation")
    
    # Only run cor.test if both predictor and response are numeric
    if (is.numeric(liver_dfcleanPMM_IMPrmNA[[response]]) & is.numeric(liver_dfcleanPMM_IMPrmNA[[predictor]])) {
      print(cor.test(liver_dfcleanPMM_IMPrmNA[[response]], liver_dfcleanPMM_IMPrmNA[[predictor]]))
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
IMP.PCS.mix.mod.null <- lm(SF36.PCS ~ 1, data = liver_dfcleanPMM_IMPrmNA)

# Define the full model
IMP.PCS.mix.mod.full <- lm(SF36.PCS ~ Epworth.Sleepiness.Scale + Pittsburgh.Sleep.Quality.Index.Score + Athens.Insomnia.Scale + Berlin.Sleepiness.Scale, data = liver_dfcleanPMM_IMPrmNA)

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
IMP.MCS.mix.mod.null <- lm(SF36.MCS ~ 1, data = liver_dfcleanPMM_IMPrmNA)

# Define the full model
IMP.MCS.mix.mod.full <- lm(SF36.MCS ~ Epworth.Sleepiness.Scale + Pittsburgh.Sleep.Quality.Index.Score + Athens.Insomnia.Scale + Berlin.Sleepiness.Scale, data = liver_dfcleanPMM_IMPrmNA)

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

# We see there is not a statistically significant value for inclusion of PSQI and BSS

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

# We see there is not a statistically significant value for inclusion of BSS and ESS

# Checking AICs to further investigate
AIC(CC.MCS.mix.mod.full)
AIC(CC.MCS.mix.step.forw)
