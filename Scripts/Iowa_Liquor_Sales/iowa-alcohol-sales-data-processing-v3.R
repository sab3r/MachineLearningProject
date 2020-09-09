alcohol_df <- read.csv("C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Liquor_Sales\\v1_Data_Processing\\Iowa_Liquor_Sales_Final_2015_2016_v2.csv", header = TRUE)
count(alcohol_df$Category.Name)
library(plyr)
alcohol_df_aggregate <- aggregate(alcohol_df, by = list(alcohol_df$Category.Name, alcohol_df$County, alcohol_df$Date), FUN = sum)
summary(alcohol_df)
summarize(alcohol_df)
str(alcohol_df)
new_alcohol_df <- alcohol_df
new_alcohol_df$Invoice.Item.Number <- NULL
new_alcohol_df$Vendor.Name <- NULL
new_alcohol_df$Store.Number <- NULL
new_alcohol_df$Address <- NULL
new_alcohol_df$City <- NULL
new_alcohol_df$Zip.Code <- NULL
str(new_alcohol_df)
new_alcohol_df$County <- as.character(new_alcohol_df$County)
new_alcohol_df$Category.Name <- as.character(new_alcohol_df$Category.Name)
new_alcohol_df$Item.Number <- as.numeric(new_alcohol_df$Item.Number)
new_alcohol_df$Item.Description <- as.character(new_alcohol_df$Item.Description)
new_alcohol_df$Date <- as.Date(new_alcohol_df$Date)
new_alcohol_df$Store.Location <- NULL
new_alcohol_df$Vendor.Number <- NULL
new_alcohol_df$Date <- as.character(new_alcohol_df$Date)
new_alcohol_df$Item.Description <- NULL
new_alcohol_df$Item.Number <- NULL


alcohol_df_aggregate <- aggregate(. ~ Date+County+Category.Name,data=new_alcohol_df, FUN=sum, na.rm=TRUE)


head(alcohol_df_aggregate)
alcohol_df_aggregate$Date <- as.Date(alcohol_df_aggregate$Date)
alcohol_df_aggregate[order(alcohol_df_aggregate$Date, alcohol_df_aggregate$County, alcohol_df_aggregate$Category.Name)]


count(is.na(alcohol_df_aggregate$Category.Name))
sapply(alcohol_df_aggregate, function(x) sum(grepl("^\\s*$", x)))
count(alcohol_df_aggregate$Category.Name[alcohol_df_aggregate$Category.Name == ''])

final_alcohol_df <- alcohol_df_aggregate[!(alcohol_df_aggregate$Category.Name == ''),]

final_alcohol_df <- final_alcohol_df[order(final_alcohol_df$Date, final_alcohol_df$County, final_alcohol_df$Category.Name),]

write.csv(final_alcohol_df, 'C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Liquor_Sales\\v1_Data_Processing\\alcohol_df_aggregate.csv', row.names = FALSE)

str(alcohol_df_aggregate)
alcohol_df_aggregate$County.Number <- NULL
alcohol_df_aggregate$Category <- NULL
alcohol_df_aggregate$Bottle.Volume..ml. <- NULL
alcohol_df_aggregate$Pack <- NULL
alcohol_df_aggregate$Volume.Sold..Gallons. <- NULL
alcohol_df_aggregate$State.Bottle.Cost <- NULL
alcohol_df_aggregate$State.Bottle.Retail <- NULL
alcohol_df_aggregate$Bottles.Sold <- NULL

str(alcohol_df_aggregate)

alcohol_df_aggregate$County <- as.factor(alcohol_df_aggregate$County)
alcohol_df_aggregate$Date <- as.Date(alcohol_df_aggregate$Date)
alcohol_df_aggregate$Category.Name <- as.factor(alcohol_df_aggregate$Category.Name)
alcohol_df_aggregate$Sale..Dollars. <- as.numeric(alcohol_df_aggregate$Sale..Dollars.)
alcohol_df_aggregate$Volume.Sold..Liters. <- as.numeric(alcohol_df_aggregate$Volume.Sold..Liters.)

alcohol_df_for_joining <- aggregate(. ~ Date+County,data=alcohol_df_aggregate, FUN=sum, na.rm=TRUE)

