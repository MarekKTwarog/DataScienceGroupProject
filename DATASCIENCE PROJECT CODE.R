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
liver_dfclean <- liver_df[,c("Subject", "Gender", "Age", "BMI", "Time.from.transplant",
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
status(liver_dfclean)

#'Here we convert categorical variables into factors by first assigning all variables
#'needing conversion to "categorical_var", then we implement a for loop which will
#' move through each variable in the categorical_var list and convert them to factors
#' then reassign the converted variable back to the liver_defclean dataset. The
#' temporary 'col' variable aprt of the for loop is assigneed to each variable in
#' the given list one by one until all variables have been converted to factors.
categorical_var <- c("Gender", "Liver.Diagnosis", "Recurrence.of.disease", "Rejection.graft.dysfunction",
                     "Any.fibrosis", "Renal.Failure", "Depression", "Corticoid")
for (col in categorical_var) {
  liver_dfclean[[col]] <- factor(liver_dfclean[[col]])
}

#Checking to see if variables were correctly transformed
glimpse(liver_dfclean)

#'Using the summary function to check for presence of any values that were not
#'encoded as NA's but look to be out of place
summary(liver_dfclean) #Looks good

#'Here we convert PSQI, ESS and AIS, to binary variables like BSS based on the
#'datasets description for values above a certain level. Epworth Sleepiness scale
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




