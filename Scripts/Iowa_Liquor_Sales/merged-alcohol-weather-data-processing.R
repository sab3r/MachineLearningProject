alcohol_df <- read.csv("C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Liquor_Sales\\v1_Data_Processing\\alcohol_df_aggregate.csv", header = TRUE)

library('reshape')
new_alcohol_df <- dcast(alcohol_df, Date+County~Category.Name, value.var = alcohol_df$Sale..Dollars.)
str(new_alcohol_df)

write.csv(new_alcohol_df, 'C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Liquor_Sales\\v1_Data_Processing\\alcohol_df_aggregate_with_categories.csv', row.names = FALSE)

library(tidyr)

alcohol_df_pivot <- alcohol_df %>%
  pivot_wider(names_from = Category.Name, values_from = Sale..Dollars.)

write.csv(alcohol_df_pivot, 'C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Liquor_Sales\\v1_Data_Processing\\alcohol_df_aggregate_pivot.csv', row.names = FALSE)

weather_df <- read.csv("C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Weather_Data\\NewDatasets\\weather_2015_2016_aggregate.csv", header = TRUE)
weather_df$AWND <- NULL
weather_df$WT11 <- NULL
weather_df$WT10 <- NULL
weather_df$WT09 <- NULL
weather_df$WT08 <- NULL
weather_df$WT07 <- NULL
weather_df$WT06 <- NULL
weather_df$WT05 <- NULL
weather_df$WT04 <- NULL
weather_df$WT03 <- NULL
weather_df$WT02 <- NULL
weather_df$WT01 <- NULL
weather_df$SN31 <- NULL
weather_df$SN32 <- NULL
weather_df$SN33 <- NULL
weather_df$SX31 <- NULL
weather_df$SX32 <- NULL
weather_df$SX33 <- NULL
weather_df$PSUN <- NULL
weather_df$TAVG <- NULL
weather_df$TSUN <- NULL
weather_df$WESD <- NULL
weather_df$WESF <- NULL
weather_df$NAME <- NULL




merged_data <- merge(x = alcohol_df_pivot, y = weather_df, by.x = c('Date', 'County'), by.y = c('DATE', 'County'))


str(alcohol_df)
alcohol_df$Category <- NULL
alcohol_df$State.Bottle.Retail <- NULL
alcohol_df$Bottles.Sold <- NULL
alcohol_df$State.Bottle.Cost <- NULL
alcohol_df$Pack <- NULL
alcohol_df$Bottle.Volume..ml. <- NULL
alcohol_df$Volume.Sold..Gallons. <- NULL
alcohol_df$County.Number <- NULL


alcohol_df_pivot <- alcohol_df %>%
  pivot_wider(names_from = Category.Name, values_from = Sale..Dollars., id_cols = Date)
write.csv(alcohol_df_pivot, 'C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Liquor_Sales\\v1_Data_Processing\\alcohol_df_aggregate_pivot.csv', row.names = FALSE)



alcohol_df_pivot <- spread(alcohol_df, alcohol_df$Category.Name, alcohol_df$Sale..Dollars.)


dcast(alcohol_df, Date+County~Category.Name)

alcohol_df_pivot <- reshape(alcohol_df, idvar = c('Date', 'County'), direction = 'wide', timevar = 'Category.Name')
write.csv(alcohol_df_pivot, 'C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Liquor_Sales\\v1_Data_Processing\\alcohol_df_aggregate_pivot.csv', row.names = FALSE)


sapply(alcohol_df_pivot, function(x) sum(is.na(x)))

alcohol_df_pivot[is.na(alcohol_df_pivot)] <- 0

str(weather_df)
str(alcohol_df_pivot)

weather_df$DATE <- as.Date(weather_df$DATE)
alcohol_df_pivot$Date <- as.Date(alcohol_df_pivot$Date)
alcohol_df_pivot$Date <- anytime::anydate(alcohol_df_pivot$Date)
weather_df$DATE <- anytime::anydate(weather_df$DATE)
head(weather_df)
merged_data <- merge(x = alcohol_df_pivot, y = weather_df, by.x = c('Date', 'County'), by.y = c('DATE', 'County'))

write.csv(merged_data, "C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\final_merged_alcohol_weather_pivot.csv", row.names = FALSE)

# Data processed in Excel

final_merged_pivot_alcohol_weather <- read.csv("C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\final_merged_alcohol_weather_pivot.csv", header = TRUE)
