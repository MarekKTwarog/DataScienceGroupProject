################################################################################
############# Team Project BTC 1859H: Data Science in Health I #################
######## Leen Madani, Maryaah Salyani, Owen Treleaven, Marek Twarog ############
####################### August 15, 2023 at 5PM #################################
################################################################################

################################################################################
################## Goal 1: Description of relevant data ########################
################################################################################

#Loading packages for data cleaning / analysis
#install.packages("tidyverse")
library(tidyverse)
#install.packages("funModeling")
library(funModeling)
#install.packages("mice")
library(mice)
#install.packages("corrplot")
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
  liver_noPSQI[[col]] <- factor(liver_noPSQI[[col]])
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

liver_noPSQI <- liver_noPSQI %>%
  mutate(Epworth_binary = ifelse(Epworth.Sleepiness.Scale > 10, 1, 0),
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
  liver_noPSQI[[col]] <- factor(liver_noPSQI[[col]])
}

for (col in categorical_var3) {
  liver_noPSQI_rmNA[[col]] <- factor(liver_noPSQI_rmNA[[col]])
}

#checking to see if conversion to factors was successful
glimpse(liver_dfcleanCC)#variables successfully converted to factors
glimpse(liver_dfcleanPW_IMP)#variables successfully converted to factors
glimpse(liver_noPSQI) #variables successfully converted to factors
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

#---liver_dfcleanCC--- complete case dataframe which includes PSQI variable

#---liver_dfcleanPW_IMP---Dataframe with imputed values for PSQI abut still has NA values present for other variables
#---liver_dfcleanPW_IMPrmNA--- Dataframe with imputed values for PSQI and remaining NA's removed

#---liver_noPSQI--- Dataframe excluding PSQI but still has NA values present for other variables
#---liver_noPSQI_rmNA--- Dataframe with PSQI compleetely removed as a variable and remaining NA's removed



############### Goal 2: Estimation of the prevalence of sleep disturbance ########################

#### Approach 1
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


#### Approach 2

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


############Experimenting: Supplementary################
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

#####################################################################################
