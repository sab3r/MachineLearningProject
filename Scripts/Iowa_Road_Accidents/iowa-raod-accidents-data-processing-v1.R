# Set the appropriate directory for the data file.
getwd()
setwd("C:/Users/Soham More/Documents/Github/dmml1/Datasets/Iowa_Road_Accidents/v1_Data_Processing/")


accidents_df <- read.csv("Vehicle_Accidents_in_Iowa_by_Location__Last_Ten_Years_.csv", header = TRUE)

library(dplyr)

names(accidents_df)

# Change Date from factor to character so that data can be filtered for specific years.
class(accidents_df$Crash.Date...Time)
accidents_df$Crash.Date...Time <- as.character(accidents_df$Crash.Date...Time)

# Check variable names.
names(accidents_df)

# Install 'anytime' package to format the Date variables.
install.packages("anytime")
# Check if the date is formatted correctly before applying it to the dataframe.
anytime::anydate(head(accidents_df$Crash.Date...Time)) 

# Transform the Date values so that we can perform operations on it.
accidents_df$Crash.Date...Time <- anytime::anydate(accidents_df$Crash.Date...Time)


# Filter data for years 2015 and 2016.
accident_df_2015_2016 <- accidents_df %>%
  filter(accidents_df$Crash.Date...Time >= as.Date("2015-01-01") & accidents_df$Crash.Date...Time <= as.Date("2016-12-31"))

names(accident_df_2015_2016)
# Order the data using Date and Store Name
new_accident_df <- accident_df_2015_2016[order(as.Date(accident_df_2015_2016$Crash.Date...Time), accident_df_2015_2016$County),]

# Check head and tail if the data is correct.
head(new_accident_df)
tail(new_accident_df)

# Check for NA values
sapply(new_accident_df, function(x) sum(is.na(x))) 
# No NA values

# Check for NaN values
sapply(new_accident_df, function(x) sum(is.nan(x)))
# No NaN values

# Check for Blank values
sapply(new_accident_df, function(x) sum(grepl("^\\s*$", x)))
# DOT.Case.Number - 0
# Law.Enforcement.Case.Number - 3499
# Crash.Date...Time - 0
# District - 0
# County - 0
# City - 29902
# Route - 64525
# Crash.Month - 0
# Crash.Day - 0
# Crash.Time - 0
# Major.Cause - 0
# Crash.Manner - 0
# Crash.Severity - 0
# Surface.Conditions - 0
# Drug.Alcohol.Related - 0
# Environment - 0
# Roadway - 0
# Light.Conditions - 0
# Weather.Conditions - 0
# Work.Zone - 109199
# Paved.Road - 0
# Roadway.Junction - 0
# First.Harmful.Event - 0
# Harmful.Event.Location - 0
# Literal.Description - 404
# Vehicles - 0
# Occupants - 0
# Fatalities - 0
# Injuries - 0
# Major.Injuries - 0
# Minor.Injuries - 0
# Possible.Injuries - 0
# Unknown.Injuries - 0
# Property.Damage - 0
# Report.Type - 0
# Rest.Update - 0
# Crash.Location - 0


# Check unique values
sapply(new_accident_df, function(x) length(unique(x)))
# DOT.Case.Number - 110472
# Law.Enforcement.Case.Number - 99308
# Crash.Date...Time - 731
# District - 6
# County - 99
# City - 852
# Route - 127
# Crash.Month - 12
# Crash.Day - 7
# Crash.Time - 1440
# Major.Cause - 71
# Crash.Manner - 12
# Crash.Severity - 5
# Surface.Conditions - 13
# Drug.Alcohol.Related - 8
# Environment - 10
# Roadway - 17
# Light.Conditions - 8
# Weather.Conditions - 12
# Work.Zone - 2
# Paved.Road - 4
# Roadway.Junction - 26
# First.Harmful.Event - 65
# Harmful.Event.Location - 11
# Literal.Description - 29838
# Vehicles - 14
# Occupants - 57
# Fatalities - 6
# Injuries - 15
# Major.Injuries - 8
# Minor.Injuries - 13
# Possible.Injuries - 10
# Unknown.Injuries - 5
# Property.Damage - 2760
# Report.Type - 3
# Rest.Update - 1
# Crash.Location - 93861



# Trim White Spaces from all the columns
sapply(new_accident_df, function(x) x <- trimws(x))

# Write all the data to a new file.
write.csv(new_accident_df, "Vehicle_Accidents_in_Iowa_2015_2016.csv", row.names = FALSE)

new_accident_df$Crash.Time <- as.character(new_accident_df$Crash.Time)





################## Change Crash Time to ['Morning', 'Afternoon', 'Evening', 'Night']

head(new_accident_df$Crash.Time)
new_accident_df$Crash.Time[new_accident_df$Crash.Time > '12:00' & new_accident_df$Crash.Time <= '18:00'] <- 'Afternoon'

new_accident_df$Crash.Time[new_accident_df$Crash.Time > '06:00' & new_accident_df$Crash.Time <= '12:00'] <- 'Morning'

new_accident_df$Crash.Time[new_accident_df$Crash.Time > '18:00' & new_accident_df$Crash.Time <= '24:00'] <- 'Evening'
new_accident_df$Crash.Time[new_accident_df$Crash.Time == '00:00'] <- 'Evening'

new_accident_df$Crash.Time[new_accident_df$Crash.Time > '00:00' & new_accident_df$Crash.Time <= '06:00'] <- 'Night'

unique(new_accident_df$Crash.Time)
