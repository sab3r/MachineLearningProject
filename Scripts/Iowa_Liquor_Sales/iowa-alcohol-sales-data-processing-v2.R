# Fix missing County values in the Iowa Liquor Sales dataset.


# Set the appropriate directory for the data file.
getwd()
setwd("C:/Users/Soham More/Documents/Github/dmml1/Datasets/Iowa_Liquor_Sales/v1_Data_Processing/")

rm(alcohol_df)
alcohol_df <- read.csv("Iowa_Liquor_Sales_Final_2015_2016.csv", header = TRUE)

  # Read the correct data
correctCountyData <- read.csv("Data_From_Maps_API.csv", header = TRUE)

alcohol_df[] <- lapply(alcohol_df, as.character)
correctCountyData[] <- lapply(correctCountyData, as.character)

for(i in 1:length(correctCountyData$County.Name)){
  alcohol_df$Address[alcohol_df$Store.Name == correctCountyData$StoreName[i]] <- correctCountyData$Address
  alcohol_df$Store.Location[alcohol_df$Store.Name == correctCountyData$StoreName[i]] <- paste('POINT (',correctCountyData$Lat, ' ', correctCountyData$Lng, ')')
  alcohol_df$County[alcohol_df$Store.Name == correctCountyData$StoreName[i]] <- correctCountyData$County.Name
}

# Check for Blank values
sapply(alcohol_df, function(x) sum(grepl("^\\s*$", x)))




# Write all the data to a new file.
write.csv(alcohol_df, "Iowa_Liquor_Sales_Final_2015_2016_v2.csv", row.names = FALSE)

# Write the data with blank County to a new file for review
write.csv(alcohol_df[alcohol_df$County =='', ], "Iowa_Liquor_Sales_Final_2015_2016_v2_blank_county_review.csv", row.names = FALSE)

alcohol_df[alcohol_df$County == '', ]

# Correct the Store name to make it consistent with other Yesway Store Numbers
alcohol_df$Store.Name[alcohol_df$County == ''] <- 'Yesway Store # 10029/ Clarion'

# Add County to the rows missing the data.
alcohol_df$County[alcohol_df$County == ''] <- 'Wright'

# Write all the data to a new file......AGAIN!
write.csv(alcohol_df, "Iowa_Liquor_Sales_Final_2015_2016_v2.csv", row.names = FALSE)
