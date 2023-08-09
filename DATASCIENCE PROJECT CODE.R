### Team Project BTC 1859H: Data Science in Health I
### Leen Madani, Maryaah Salyani, Owen Treleaven, Marek Twarog
### August 15, 2023 at 5PM


#### Goal 1: Description of relevant data

#Loading packages for data cleaning / analysis
#install.packages("tidyverse")
#install.packages("funModeling")
#install.packages("Hmisc")
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
#install.packages("corrplot")
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


#' Trying to calculate the prevalence by taking into consideration the observations without missing values for each scale
#' Each scale has different number of observations with missing values
#' Detach liver_dfclean3 as we need to use the previous dataframe where NA's where not omitted yet.
detach(liver_dfclean3)
attach(liver_dfclean2)


# Learn more about the number of positive, negative, and missing test results for each scale using summary()
summary(liver_dfclean2)

# Store the number of observations with missing values for each scale as: missing_"scale name"
missing_berlin <- sum(is.na(Berlin.Sleepiness.Scale))
missing_epworth <- sum(is.na(Epworth_binary))
missing_pittsburgh <- sum(is.na(Pittsburgh_binary))
missing_athens <- sum(is.na(Athens_binary))

# Store the number of cases (positive outcomes) for sleep disturbance for each scale as: "scale name"_cases
berlin_cases <- sum(Berlin.Sleepiness.Scale == 1, na.rm=TRUE)
epworth_cases <- sum(Epworth_binary == 1, na.rm = TRUE)
pittsburgh_cases <- sum(Pittsburgh_binary == 1, na.rm = TRUE)
athens_cases <- sum(Athens_binary == 1, na.rm = TRUE)

# Calculate the total number of valid observations for each scale and store them as the variable: "scale name"_pop
berlin_pop <- sum(!is.na(Berlin.Sleepiness.Scale))  # equal to : nrow(liver_dfclean2) - missing_berlin & to this sum(complete.cases(Berlin.Sleepiness.Scale))
epworth_pop <- sum(!is.na(Epworth_binary))
pittsburgh_pop <- sum(!is.na(Pittsburgh_binary))
athens_pop <- sum(!is.na(Athens_binary))


# Calculate the prevalence of sleep disturbance with confidence interval set at 0.95 for each scale using prop.test()
prev_berlin <- prop.test(x = berlin_cases, n = berlin_pop, conf.level = 0.95)
prev_epworth <- prop.test(x = epworth_cases,  n = epworth_pop, conf.level = 0.95)
prev_pittsburgh <- prop.test(x = pittsburgh_cases, n = pittsburgh_pop, conf.level = 0.95)
prev_athens <- prop.test(x = athens_cases, n = athens_pop, conf.level = 0.95)


# Create a data frame that ultimately show prevalence values as percent with the confidence intervals
data_df <- data.frame(
  Scale = c("Berlin", "Epworth", "Pittsburgh", "Athens"),
  Cases = c(berlin_cases, epworth_cases, pittsburgh_cases, athens_cases),
  Population = c(berlin_pop, epworth_pop, pittsburgh_pop, athens_pop),
  Missing.Values = c(missing_berlin, missing_epworth, missing_pittsburgh, missing_athens),
  Prevalence.Percent = c(prev_berlin$estimate, prev_epworth$estimate, prev_pittsburgh$estimate, prev_athens$estimate) * 100,
  CI.Lower = c(prev_berlin$conf.int[1], prev_epworth$conf.int[1], prev_pittsburgh$conf.int[1], prev_athens$conf.int[1]) * 100,
  CI.Upper = c(prev_berlin$conf.int[2], prev_epworth$conf.int[2], prev_pittsburgh$conf.int[2], prev_athens$conf.int[2]) * 100
)



data_df <- data_df[sort.list(data_df$Prevalence.Percent),]
data_df$rank <- 1:nrow(data_df)

library(ggplot2)
ggplot(data = data_df, aes(x = rank, y = Prevalence.Percent)) +
  theme_bw() +
  geom_errorbar(aes(ymin = CI.Lower , ymax = CI.Upper), width = 0.1) +
  geom_point() +
  geom_text(aes(label = paste(round(Prevalence.Percent, 2), "%\n(", round(CI.Lower, 2), "-", round(CI.Upper, 2), ")")), #%\n( to create a line break
            vjust = -1, hjust = 0.5, size = 3, color = "black") +  # Labeling confidence intervals
  scale_x_continuous(breaks = data_df$rank, labels = data_df$Scale, name = "Sleep Disturbance Scales") +
  scale_y_continuous(limits = c(0, 100), name = "Prevalence (%)") +
  labs(caption = "Confidence intervals indicate uncertainty.") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.caption = element_text(size = 9, color = "gray", hjust = 0))


ggplot(data = data_df, aes(x = Scale, y = Prevalence.Percent, fill = Scale)) +
  theme_bw() +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = CI.Lower , ymax = CI.Upper), position = position_dodge(width = 0.9), width = 0.2) +
  geom_text(aes(label = paste(round(Prevalence.Percent, 2), "%")),
            vjust = -3, size = 3, position = position_dodge(width = 0.9), color = "black") +
  scale_y_continuous(limits = c(0, 100), name = "Prevalence (%)") +
  labs(title = "Prevalence of Sleep Disturbance Across Scales",
       caption = "Confidence intervals indicate uncertainty.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(size = 9, color = "gray", hjust = 0)) +
  guides(fill = "none") # Remove the fill legend because its unnecessary



# Conditional probability of low quality of life given that there is sleep disturbance as measured by that specific scale.
# Interested to investigate the likelihood that an individual with sleep disturbance
# ...(according to the sleep disturbance scale scores) will also have low quality of life (based on the SF-36 PCS = & MCS scores).
# Event A: Low quality of life defined by a chosen threshold
# Event B: Sleep disturbance based on specific scale
# P(A|B) = = Number of individuals with both A and B / Number of individuals with B

# Define the function to calculate conditional probability
calculate_conditional_prob <- function(column_name, sf36_pcs_threshold, sf36_mcs_threshold) {
  # Calculate the number of individuals with sleep disturbance and low quality of life for PCS and MCS
  low_quality_pcs <- sum(liver_dfclean3[[column_name]] == 1 & liver_dfclean3$SF36.PCS <= sf36_pcs_threshold, na.rm = TRUE)
  low_quality_mcs <- sum(liver_dfclean3[[column_name]] == 1 & liver_dfclean3$SF36.MCS <= sf36_mcs_threshold, na.rm = TRUE)

  # Calculate the number of individuals with sleep disturbance for the scale
  sleep_disturbance <- sum(liver_dfclean3[[column_name]] == 1, na.rm = TRUE)

  # Calculate conditional probabilities for PCS and MCS
  conditional_prob_pcs <- low_quality_pcs / sleep_disturbance
  conditional_prob_mcs <- low_quality_mcs / sleep_disturbance

  # Return the results
  return(list(
    scale_name = column_name,
    conditional_prob_pcs = conditional_prob_pcs,
    conditional_prob_mcs = conditional_prob_mcs
  ))
}

# Define the sleep disturbance scale column names
sleep_scale_columns <- c("Epworth_binary", "Pittsburgh_binary", "Athens_binary", "Berlin.Sleepiness.Scale")

# Thresholds for SF-36 scores
sf36_pcs_threshold <- 33
sf36_mcs_threshold <- 36

results <- sapply(sleep_scale_columns, function(scale_column) { # sapply will apply the function to each element in sleep_scale_columns
  calculate_conditional_prob(column_name = scale_column,
                             sf36_pcs_threshold = sf36_pcs_threshold,
                             sf36_mcs_threshold = sf36_mcs_threshold)
}, simplify = "data.frame")


# Chi-square test of independence to assess the association between sleep disturbance (presence/absence) based on different scales and low quality of life

## Create a function to perform chi-square test for a specific scale and low quality measure
perform_chi_square <- function(scale_column, low_quality_column, low_quality_threshold) {
  contingency_table <- table(liver_dfclean3[[scale_column]] == 1, liver_dfclean3[[low_quality_column]] <= low_quality_threshold)
  chi_square_result <- chisq.test(contingency_table)
  return(chi_square_result)
}

# List of scales and their corresponding low quality of life thresholds
scales <- c("Berlin.Sleepiness.Scale", "Epworth_binary", "Pittsburgh_binary", "Athens_binary")
low_quality_columns <- c("SF36.PCS", "SF36.MCS")
low_quality_threshold <- c(33, 36)  # Thresholds for SF36.PCS and SF36.MCS

# Perform chi-square tests for each scale and low quality measure combination
chi_square_results <- list()

for (scale in scales) {
  chi_square_results[[scale]] <- list()
  for (i in seq_along(low_quality_columns)) {
    result <- perform_chi_square(scale_column = scale, low_quality_column = low_quality_columns[i], low_quality_threshold = low_quality_threshold[i])
    chi_square_results[[scale]][[low_quality_columns[i]]] <- result
  }
}

# Print the results
for (scale in scales) {
  for (low_quality_col in low_quality_columns) {
    cat("Scale:", scale, ", Low Quality Measure:", low_quality_col, "\n")
    print(chi_square_results[[scale]][[low_quality_col]])
    cat("\n")
  }
}


library(ggplot2)

# Create a data frame with the results
results <- data.frame(
  Scale = c("Berlin.Sleepiness.Scale", "Berlin.Sleepiness.Scale", "Epworth_binary", "Epworth_binary", "Pittsburgh_binary", "Pittsburgh_binary", "Athens_binary", "Athens_binary"),
  Low_Quality_Measure = rep(c("SF36.PCS", "SF36.MCS"), each = 4),
  X_squared = c(11.116, 1.5749, 11.268, 10.58, 6.8435, 18.42, 6.3135, 24.347),
  p_value = c(0.0008558, 0.2095, 0.0007884, 0.001143, 0.008897, 1.772e-05, 0.01198, 8.044e-07)
)

# Create a bar plot
ggplot(results, aes(x = Scale, y = X_squared, fill = Low_Quality_Measure)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Chi-squared Test Results",
       x = "Scale and Low Quality Measure",
       y = "X-squared Value") +
  theme_minimal()

# Create a dot plot for p-values
ggplot(results, aes(x = p_value, y = reorder(Scale, p_value))) +
  geom_point(size = 3, aes(color = Low_Quality_Measure)) +
  labs(title = "Chi-squared Test Results",
       x = "p_value",
       y = "Scale",
       color = "Low Quality Measure") +
  theme_minimal() +
  scale_x_log10()  # Use log scale for better visualization of p-values

detach(liver_dfclean2)
