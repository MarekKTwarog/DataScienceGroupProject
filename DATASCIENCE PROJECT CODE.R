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

#'To determine whether we can conduct a linear reegression model to imput missing NA
#' values for the PSQI variable, we split the dataset into two dataframes, one which
#' contains only complete case values with respect to PSQI and another which only
#' contains rows that are found as NA's for thee PSQI variable. We then take the
#' mean of the variables "Athens.Insomnia.Scale", "Berlin.Sleepiness.Scale", and
#' "Epworth.Sleepiness.Scale" fir each of the dataframes (with and without PSQI NA's)
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

#' Here we use the cor() function to find the correlation between each variable
#' then we use the corrplot function to crease a plot that will help easily visualize
#' which variable has the strongest correlation with PSQ1. Based on the results,
#' we find that the Athens.Insomnia.Scale has the strongest correlation with
#' PSQ1 and thus we use the lm() function to build a linear regression model between
#' the two variables so that it can then be used along with the predict() function
#' to predict values for NA in the PSQ1 variable column. The values predicted are
#' then used to replace the NA values in the PSQ1 column,

correlation <- cor(liver_dfclean, use = "pairwise")
corrplot(correlation, type = "lower", diag = FALSE)
lm_model <- lm(Pittsburgh.Sleep.Quality.Index.Score ~ Athens.Insomnia.Scale, data = liver_dfclean, na.action = na.exclude)
liver_dfclean$PredictedPSQ1 <- predict(lm_model, newdata = liver_dfclean)

#'Creating two datasets, one where only complete case values will be included and
#'the other where missing values for PSQI will be imputed
liver_dfcleanCC <- liver_dfclean
liver_dfcleanIMP <- liver_dfclean

# Replace NA values in 'PSQ1' with their respective 'y_predicted' values
liver_dfcleanIMP$Pittsburgh.Sleep.Quality.Index.Score[is.na(liver_dfcleanIMP$Pittsburgh.Sleep.Quality.Index.Score)] <- liver_dfcleanIMP$PredictedPSQ1[is.na(liver_dfcleanIMP$Pittsburgh.Sleep.Quality.Index.Score)]

#Remove NA values from complete case dataframe
liver_dfcleanCC <- na.omit(liver_dfcleanCC)

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
  liver_dfcleanIMP[[col]] <- factor(liver_dfcleanIMP[[col]])
}

for (col in categorical_var) {
  liver_dfcleanCC[[col]] <- factor(liver_dfcleanCC[[col]])
}

#Checking to see if variables were correctly transformed
glimpse(liver_dfcleanIMP) #Looks good
glimpse(liver_dfcleanCC) #Looks good

#'Using the summary function to check for presence of any values that were not
#'encoded as NA's but look to be out of place
summary(liver_dfcleanIMP) #Looks good
summary(liver_dfcleanCC) #Looks good

#'Here we convert PSQI, ESS and AIS, to binary variables like BSS based on the
#'datasets description for values above a certain level. Epworth Sleepiness Scale
#'total score greater than 10, indicating at least mild excessive daytime sleepiness
#'(1 = yes, 0 = no), Pittsburgh Sleep Quality Index score greater than 4, indicating
#'poor sleep quality (1 = yes, 0 = no),Athens Insomnia Scale total score greater than
#'5, indicating the presence of insomnia, (1 = yes, 0 = no). The new variables are
#'added to the dataframe as new columns using the mutate function from the tidyverse
#'package.
liver_dfcleanIMP <- liver_dfcleanIMP %>%
  mutate(Epworth_binary = ifelse(Epworth.Sleepiness.Scale > 10, 1, 0),
         Pittsburgh_binary = ifelse(Pittsburgh.Sleep.Quality.Index.Score > 4, 1, 0),
         Athens_binary = ifelse(Athens.Insomnia.Scale > 5, 1, 0))

liver_dfcleanCC <- liver_dfcleanCC %>%
  mutate(Epworth_binary = ifelse(Epworth.Sleepiness.Scale > 10, 1, 0),
         Pittsburgh_binary = ifelse(Pittsburgh.Sleep.Quality.Index.Score > 4, 1, 0),
         Athens_binary = ifelse(Athens.Insomnia.Scale > 5, 1, 0))

#Now we must convert the newly made binary variables to factors for further analysis
categorical_var2 <- c("Epworth_binary", "Pittsburgh_binary", "Athens_binary")

for (col in categorical_var2) {
  liver_dfcleanIMP[[col]] <- factor(liver_dfcleanIMP[[col]])
}

for (col in categorical_var2) {
  liver_dfcleanCC[[col]] <- factor(liver_dfcleanCC[[col]])
}

#checking to see if conversion to factors was successful
glimpse(liver_dfcleanIMP)#variables successfully converted to factors
glimpse(liver_dfcleanCC)#variables successfully converted to factors

#'The final set requires the removal of NA values from the imputed dataset, this is done
#'using the na.omit function

liver_dfcleanIMPrmNA <- na.omit(liver_dfcleanIMP)

################################################################################

#### Goal 2: Estimation of the prevalence of sleep disturbance

# Attach dataframe with complete cases to avoid referencing it every time you use it
attach(liver_dfcleanCC)

# Calculate the prevalence of sleep disturbance using the 4 measures using prop.table() function and view results in a table
prevalence <- prop.table(table(Berlin.Sleepiness.Scale, Epworth_binary, Pittsburgh_binary, Athens_binary))

# Extract the table
write.csv(prevalence, "prevalence.csv")


#' The above calcu Trying to calculate the prevalence by taking into consideration the observations without missing values for each scale
#' Each scale has different number of observations with missing values
#' Detach liver_dfcleanIMPrmNA as we need to use the previous dataframe where NA's where not omitted yet (complete case).
detach(liver_dfcleanCC)
attach(liver_dfcleanIMP)

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
  PSQI = Pittsburgh_binary,
  AIS = Athens_binary
)

# Initialize an empty dataframe to store results
prevalence_df <- data.frame()

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

  # Append the scale result to the dataframe
  prevalence_df <- rbind(prevalence_df, result)
}

prevalence_df <- prevalence_df[sort.list(prevalence_df$Prevalence.Percent),]
prevalence_df$rank <- 1:nrow(prevalence_df )

library(ggplot2)
ggplot(data = prevalence_df, aes(x = rank, y = Prevalence.Percent)) +
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


ggplot(data = prevalence_df, aes(x = Scale, y = Prevalence.Percent, fill = Scale)) +
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

detach(liver_dfcleanIMP)

# We're interested to investigate the likelihood that an individual with sleep disturbance
# ...(according to the sleep disturbance scale scores) will also have low quality of life (based on the SF-36 PCS = & MCS scores).
# Event A: Low quality of life defined by a chosen threshold
# Event B: Sleep disturbance based on specific scale
# P(A|B) = = Number of individuals with both A and B / Number of individuals with B
# Define the function to generate a contingency table
generate_contingency_table <- function(scale_column, sleep_column, sf36_threshold) {
  sleep_disturbance <- liver_dfcleanIMPrmNA[[scale_column]] == 1
  low_quality_sf36 <- liver_dfcleanIMPrmNA[[sleep_column]] <= sf36_threshold

  contingency_table <- table(sleep_disturbance, low_quality_sf36)
  return(contingency_table)
}

# Set the SF36 thresholds
sf36_pcs_threshold <- 33
sf36_mcs_threshold <- 36

# List of sleep disturbance scales and SF36 columns
sleep_scale_columns <- c("Epworth_binary", "Pittsburgh_binary", "Athens_binary", "Berlin.Sleepiness.Scale")
sf36_columns <- c("SF36.PCS", "SF36.MCS")

# Generate and save contingency tables
for (scale_column in sleep_scale_columns) {
  for (sf36_column in sf36_columns) {
    if (sf36_column == "SF36.PCS") {
      sf36_threshold <- sf36_pcs_threshold
    } else {
      sf36_threshold <- sf36_mcs_threshold
    }

    contingency_table <- generate_contingency_table(scale_column, sf36_column, sf36_threshold)

    # Perform chi-square test
    chi_square_result <- chisq.test(contingency_table)
    chi_square_results[[scale_column]][[sf36_column]] <- chi_square_result
  }

  # Create the file name for the contingency table
  file_name <- paste(scale_column, "contingency_table.csv", sep = "_")

  # Save the contingency table as a CSV file
  write.csv(contingency_table, file_name)
}

# Define the function to calculate conditional probability
calculate_conditional_prob <- function(column_name, sf36_pcs_threshold, sf36_mcs_threshold) {
  # Calculate the number of individuals with sleep disturbance and low quality of life for PCS and MCS
  low_quality_pcs <- sum(liver_dfcleanIMPrmNA[[column_name]] == 1 & liver_dfcleanIMPrmNA$SF36.PCS <= sf36_pcs_threshold, na.rm = TRUE)
  low_quality_mcs <- sum(liver_dfcleanIMPrmNA[[column_name]] == 1 & liver_dfcleanIMPrmNA$SF36.MCS <= sf36_mcs_threshold, na.rm = TRUE)

  # Calculate conditional probabilities for PCS and MCS
  conditional_prob_pcs <- (low_quality_pcs / sleep_disturbance) * 100
  conditional_prob_mcs <- (low_quality_mcs / sleep_disturbance) * 100

  # Define the sleep disturbance scale column names
  sleep_scale_columns <- c("Epworth_binary", "Pittsburgh_binary", "Athens_binary", "Berlin.Sleepiness.Scale")

  # Return the results
  return(list(
    scale_name = column_name,
    conditional_prob_pcs = conditional_prob_pcs,
    conditional_prob_mcs = conditional_prob_mcs
  ))
}
cond.prob.results <- data.frame(sapply(sleep_scale_columns, function(scale_column) {
  calculate_conditional_prob(column_name = scale_column,
                             sf36_pcs_threshold = sf36_pcs_threshold,
                             sf36_mcs_threshold = sf36_mcs_threshold)}))


# Create a data frame with the chi-square results
chi_square_res <- data.frame(
  Scale = rep(c("BSS", "ESS", "PSQI", "AIS"), each = 2),
  Low_Quality_Measure = rep(c("SF36.PCS", "SF36.MCS"), times = 4),
  X_squared = c(11.116, 1.5749, 11.268, 10.58, 6.8435, 18.42, 6.3135, 24.347),
  p_value = c(0.0008558, 0.2095, 0.0007884, 0.001143, 0.008897, 1.772e-05, 0.01198, 8.044e-07)
)

library(ggplot2)
# Create a bar plot for the x^2 values
x_plot <- ggplot(chi_square_res, aes(x = Scale, y = X_squared, fill = Low_Quality_Measure)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Chi-squared Test Results",
       x = "Sleep Disturbance Scale",
       y = "X-squared Value",
       fill = "Low Quality of Life Measure") +
  theme_minimal()
x_plot
# Create a dot plot for p-values
p_value_plot <- ggplot(chi_square_res, aes(x = p_value, y = Scale, color = Low_Quality_Measure)) +
  geom_point(size = 3) +
  labs(title = "Chi-squared Test Results",
       x = "P-value",
       y = "Sleep Disturbance Scale") +
  theme_minimal()

p_value_plot
# Display the plots side by side
library(gridExtra)
grid.arrange(x_plot, p_value_plot, ncol = 2)

library(ggplot2)

# Create a forest plot-like visualization
forest_plot <- ggplot(chi_square_res, aes(x = Scale, y = X_squared)) +
  geom_point(aes(color = Low_Quality_Measure), size = 3) +
  geom_text(aes(label = sprintf("p = %.4f", p_value)), nudge_y = 0.2, size = 3, hjust = -0.1) +
  labs(title = "Chi-squared Test Results",
       x = "Sleep Disturbance Scale",
       y = "X-squared Value",
       color = "Low Quality of Life Measure") +
  theme_minimal()
forest_plot
#####################################################################################


