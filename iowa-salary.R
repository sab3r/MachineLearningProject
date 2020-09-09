#### Cleaning the data in the Iowa State Salary dataset.

# Change the working directory to the data folder.
getwd()
setwd("C:/Users/Soham More/Documents/Datasets/v1_Data_Processing/state-of-iowa-salary-book/")


# Load the data into a dataframe salary_df. 
salary_df <- read.csv("State_of_Iowa_Salary_Book.csv", header = TRUE)

# Check for NA, NULL and NAN values
sapply(salary_df, function(x) sum(is.na(x)))
sapply(salary_df, function(x) sum(is.null(x)))
sapply(salary_df, function(x) sum(is.nan(x)))

head(salary_df)

# Replace all the NA values in 'Travel and Subsistance' with 0.
# Here I am assuming that if the value is NA the person was not offered 'Travel and Subsistance' money for that salary. Hence 0 USD.
salary_df$Travel...Subsistence[is.na(salary_df$Travel...Subsistence)] <- 0


salary_df_with_rates <- data.frame(do.call('rbind', strsplit(as.character(salary_df$Base.Salary), ' ', fixed = TRUE)))
head(salary_df_with_rates)
dataFreq <- table(salary_df_with_rates$X1)
dataFreq

library(dplyr)

str(dataFreq)
salary_df_with_rates$X1 %>%
  select(!starts_with("$"))
