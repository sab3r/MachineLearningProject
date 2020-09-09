road_accidents <- read.csv("C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Road_Accidents\\v1_Data_Processing\\Vehicle_Accidents_in_Iowa_by_Location__Last_Ten_Years_.csv", header = TRUE)
str(road_accidents)
road_accidents$Crash.Date...Time <- as.character(road_accidents$Crash.Date...Time)

road_accidents$Crash.Date...Time <- anytime::anydate(road_accidents$Crash.Date...Time)


# Filter data for years 2015 and 2016.
library(dplyr)
road_accidents_2015_2016 <- road_accidents %>%
  filter(road_accidents$Crash.Date...Time >= as.Date("2015-01-01") & road_accidents$Crash.Date...Time <= as.Date("2016-12-31"))


road_accidents_df <- road_accidents_2015_2016[order(as.Date(road_accidents_2015_2016$Crash.Date...Time), road_accidents_2015_2016$County),]

write.csv(road_accidents_df, "C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Road_Accidents\\v1_Data_Processing\\Road_Accidents_2015_2016.csv", row.names = FALSE)
