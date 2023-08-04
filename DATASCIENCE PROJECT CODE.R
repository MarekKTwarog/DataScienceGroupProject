### Team Project BTC 1859H: Data Science in Health I
### Leen Madani, Maryaah Salyani, Owen Treleaven, Marek Twarog
### August 15, 2023 at 5PM


#### Goal 1: Description of relevant data

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
#' to predict values for NA in the PSQ1 variable column. The values predictred are
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

#'Here , we convert sleep disturbance measures (PSQI, ESS, and AIS) to binary variables
#' using the given clinically accepted threshold for each measure
#' Berlin.Sleepiness.Scale (BSS) is already a binary variable, so will not convert it
#' The new variables are added to the dataframe as new columns using the mutate() function from the tidyverse package
liver_dfclean2 <- liver_dfclean %>%
  mutate(Epworth_binary = ifelse(Epworth.Sleepiness.Scale > 10, 1, 0),                # for ESS, values above 10 indicate sleep disturbance and are changed to 1, else 0 (normal)
         Pittsburgh_binary = ifelse(Pittsburgh.Sleep.Quality.Index.Score > 4, 1, 0),  # for PSQI, values above 4 indicate disturbed sleep and are changed to 1, else 0 (normal)
         Athens_binary = ifelse(Athens.Insomnia.Scale > 5, 1, 0))                      # for AIS, values above 5 indicate sleep disturbance and are changed to 1, else 0 (normal)

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

#### Goal 2: Estimation of the prevalence of sleep disturbance

# Attach dataframe to avoid referencing it every time you use it
attach(liver_dfclean3)

# Calculate the prevalence of sleep disturbance using the 4 measures using prop.table() function and view results in a table
prevalence <- prop.table(table(Berlin.Sleepiness.Scale, Epworth_binary, Pittsburgh_binary, Athens_binary))

# Extract the table
write.csv(prevalence, "prevalence.csv")

# Calculate the prevalence of sleep disturbance for each scale
prev_berlin <- sum(Berlin.Sleepiness.Scale == 1) / nrow(liver_dfclean3)
prev_epworth <- sum(Epworth_binary == 1) / nrow(liver_dfclean3)
prev_pittsburgh <- sum(Pittsburgh_binary == 1) / nrow(liver_dfclean3)
prev_athens <- sum(Athens_binary == 1) / nrow(liver_dfclean3)

# Create a data frame with the prevalence values
prevalence_df <- data.frame(
  Scale = c("Berlin", "Epworth", "Pittsburgh", "Athens"),
  Prevalence = c(prev_berlin, prev_epworth, prev_pittsburgh, prev_athens)
)

#' Trying to calculate the prevalence by taking into consideration the observations without missing values for each scale
#' Each scale has different number of observations with missing values
#' Detach liver_dfclean3 as we need to use the previous dataframe where NA's where not omitted yet.
detach(liver_dfclean3)
attach(liver_dfclean2)

# Check the number of observations with missing values for each scale
missing_berlin <- sum(is.na(Berlin.Sleepiness.Scale))
missing_epworth <- sum(is.na(Epworth_binary))
missing_pittsburgh <- sum(is.na(Pittsburgh_binary))
missing_athens <- sum(is.na(Athens_binary))

sum(Berlin.Sleepiness.Scale == 1, na.rm=TRUE)
sum(Epworth_binary == 1, na.rm=TRUE)
sum(Pittsburgh_binary == 1, na.rm = TRUE)
# Calculate the prevalence of sleep disturbance for each scale
prev_berlin <- sum(Berlin.Sleepiness.Scale == 1, na.rm=TRUE) / (nrow(liver_dfclean2) - missing_berlin)
prev_epworth <- sum(Epworth_binary == 1, na.rm=TRUE) / (nrow(liver_dfclean2) - missing_epworth)
prev_pittsburgh <- sum(Pittsburgh_binary == 1, na.rm = TRUE) / (nrow(liver_dfclean2) - missing_pittsburgh)
prev_athens <- sum(Athens_binary == 1, na.rm = TRUE) / (nrow(liver_dfclean2) - missing_athens)

# Create a data frame with the prevalence values
prevalence_NAs <- data.frame(
  Scale = c("Berlin", "Epworth", "Pittsburgh", "Athens"),
  Prevalence = c(prev_berlin, prev_epworth, prev_pittsburgh, prev_athens)
)
