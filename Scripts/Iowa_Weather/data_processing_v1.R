weather_2016 <- read.csv("C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Weather_Data\\NewDatasets\\2103697.csv", header = TRUE)
weather_2015 <- read.csv("C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Weather_Data\\NewDatasets\\2103695.csv", header = TRUE)
weather_station_location <- read.csv("C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Weather_Data\\NewDatasets\\Weather Station Location.csv", header = TRUE)


str(weather_2016)
str(weather_2015)

weather_2015$AWND_ATTRIBUTES <- NULL
weather_2015$PRCP_ATTRIBUTES <- NULL
weather_2015$PSUN_ATTRIBUTES <- NULL
weather_2015$SN31_ATTRIBUTES <- NULL
weather_2015$SN32_ATTRIBUTES <- NULL
weather_2015$SN33_ATTRIBUTES <- NULL
weather_2015$SNOW_ATTRIBUTES <- NULL
weather_2015$SNWD_ATTRIBUTES <- NULL
weather_2015$SX31_ATTRIBUTES <- NULL
weather_2015$SX32_ATTRIBUTES <- NULL
weather_2015$SX33_ATTRIBUTES <- NULL
weather_2015$TAVG_ATTRIBUTES <- NULL
weather_2015$TMAX_ATTRIBUTES <- NULL
weather_2015$TMIN_ATTRIBUTES <- NULL
weather_2015$TSUN_ATTRIBUTES <- NULL
weather_2015$WESD_ATTRIBUTES <- NULL
weather_2015$WESF_ATTRIBUTES <- NULL
weather_2015$WT01_ATTRIBUTES <- NULL
weather_2015$WT02_ATTRIBUTES <- NULL
weather_2015$WT03_ATTRIBUTES <- NULL
weather_2015$WT04_ATTRIBUTES <- NULL
weather_2015$WT05_ATTRIBUTES <- NULL
weather_2015$WT06_ATTRIBUTES <- NULL
weather_2015$WT07_ATTRIBUTES <- NULL
weather_2015$WT08_ATTRIBUTES <- NULL
weather_2015$WT09_ATTRIBUTES <- NULL
weather_2015$WT10_ATTRIBUTES <- NULL
weather_2015$WT11_ATTRIBUTES <- NULL

weather_2016$AWND_ATTRIBUTES <- NULL
weather_2016$PRCP_ATTRIBUTES <- NULL
weather_2016$PSUN_ATTRIBUTES <- NULL
weather_2016$SN31_ATTRIBUTES <- NULL
weather_2016$SN32_ATTRIBUTES <- NULL
weather_2016$SN33_ATTRIBUTES <- NULL
weather_2016$SNOW_ATTRIBUTES <- NULL
weather_2016$SNWD_ATTRIBUTES <- NULL
weather_2016$SX31_ATTRIBUTES <- NULL
weather_2016$SX32_ATTRIBUTES <- NULL
weather_2016$SX33_ATTRIBUTES <- NULL
weather_2016$TAVG_ATTRIBUTES <- NULL
weather_2016$TMAX_ATTRIBUTES <- NULL
weather_2016$TMIN_ATTRIBUTES <- NULL
weather_2016$TSUN_ATTRIBUTES <- NULL
weather_2016$WESD_ATTRIBUTES <- NULL
weather_2016$WESF_ATTRIBUTES <- NULL
weather_2016$WT01_ATTRIBUTES <- NULL
weather_2016$WT02_ATTRIBUTES <- NULL
weather_2016$WT03_ATTRIBUTES <- NULL
weather_2016$WT04_ATTRIBUTES <- NULL
weather_2016$WT05_ATTRIBUTES <- NULL
weather_2016$WT06_ATTRIBUTES <- NULL
weather_2016$WT07_ATTRIBUTES <- NULL
weather_2016$WT08_ATTRIBUTES <- NULL
weather_2016$WT09_ATTRIBUTES <- NULL
weather_2016$WT10_ATTRIBUTES <- NULL
weather_2016$WT11_ATTRIBUTES <- NULL

weather_2015$County <- weather_station_location$County[weather_2015$STATION]
head(weather_2015)
tail(weather_2015)

weather_2016$County <- weather_station_location$County[weather_2016$STATION]
head(weather_2016)
tail(weather_2016)


weather_2015$DATE <- as.Date(weather_2015$DATE)
weather_2016$DATE <- as.Date(weather_2016$DATE)


weather_2015 <- weather_2015[order(weather_2015$DATE, weather_2015$County),]
head(weather_2015)
count(weather_2015$County)

weather_2016 <- weather_2016[order(weather_2016$DATE, weather_2016$County),]
head(weather_2016)
count(weather_2016$County)

str(weather_2015)
str(weather_2016)




#weather_2015[is.na(weather_2015)] <- 0

#weather_2016[is.na(weather_2016)] <- 0

write.csv(weather_2015,"C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Weather_Data\\NewDatasets\\weather_2015.csv", row.names = FALSE)
write.csv(weather_2016,"C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Weather_Data\\NewDatasets\\weather_2016.csv", row.names = FALSE)

weather_2015_aggregate <- aggregate(. ~ DATE+County,data=weather_2015, FUN=mean, na.rm=TRUE, na.action=NULL)
weather_2016_aggregate <- aggregate(. ~ DATE+County,data=weather_2016, FUN=mean, na.rm=TRUE, na.action=NULL)

write.csv(weather_2015_aggregate,"C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Weather_Data\\NewDatasets\\weather_2015_aggregate.csv", row.names = FALSE)
write.csv(weather_2016_aggregate,"C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Weather_Data\\NewDatasets\\weather_2016_aggregate.csv", row.names = FALSE)


count(unique(weather_2015[,c('DATE','County','STATION')]))
count(unique(weather_2015[,c('DATE','County')]))
count(unique(weather_2015_aggregate[,c('DATE','County','STATION')]))
count(unique(weather_2015_aggregate[,c('DATE','County')]))

count(unique(weather_2016[,c('DATE','County','STATION')]))
count(unique(weather_2016[,c('DATE','County')]))
count(unique(weather_2016_aggregate[,c('DATE','County','STATION')]))
count(unique(weather_2016_aggregate[,c('DATE','County')]))


str(weather_2015_aggregate)
str(weather_2016_aggregate)


weather_2015_2016_aggregate <- rbind(weather_2015_aggregate, weather_2016_aggregate)

write.csv(weather_2015_2016_aggregate, "C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Weather_Data\\NewDatasets\\weather_2015_2016_aggregate.csv", row.names = FALSE)

sapply(weather_2015_2016_aggregate, function(x) sum(is.na(x)))

# Replace WT** column with the respective number and add them to a new column WT
#weather_2015_2016_aggregate$WT01[weather_2015_2016_aggregate$WT01 == 1] <- 1
#weather_2015_2016_aggregate$WT02[weather_2015_2016_aggregate$WT02 == 1] <- 2
#weather_2015_2016_aggregate$WT03[weather_2015_2016_aggregate$WT03 == 1] <- 3
#weather_2015_2016_aggregate$WT04[weather_2015_2016_aggregate$WT04 == 1] <- 4
#weather_2015_2016_aggregate$WT05[weather_2015_2016_aggregate$WT05 == 1] <- 5
#weather_2015_2016_aggregate$WT06[weather_2015_2016_aggregate$WT06 == 1] <- 6
#weather_2015_2016_aggregate$WT07[weather_2015_2016_aggregate$WT07 == 1] <- 7
#weather_2015_2016_aggregate$WT08[weather_2015_2016_aggregate$WT08 == 1] <- 8
#weather_2015_2016_aggregate$WT09[weather_2015_2016_aggregate$WT09 == 1] <- 9
#weather_2015_2016_aggregate$WT10[weather_2015_2016_aggregate$WT10 == 1] <- 10
#weather_2015_2016_aggregate$WT11[weather_2015_2016_aggregate$WT11 == 1] <- 11

#weather_2015_2016_aggregate$WT <- rowSums(weather_2015_2016_aggregate[,c("WT01", "WT02", "WT03", "WT04", "WT05", "WT06", "WT07", "WT08", "WT09", "WT10", "WT11")], na.rm=TRUE)
#table(weather_2015_2016_aggregate$WT)
str(weather_2015_2016_aggregate)




# Merge weather and alcohol sales

merged_data <- merge(x = alcohol_df_for_joining, y = weather_2015_2016_aggregate, by.x = c('Date', 'County'), by.y = c('DATE', 'County'))
head(merged_data)

write.csv(merged_data, "C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\final_merged_alcohol_weather.csv", row.names = FALSE)



str(merged_data)
my_data <- merged_data[, c(4,5,11:38)]
correlation_matrix <- cor(my_data)
correlation_matrix


final_weather_alcohol <- read.csv("C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\final_merged_alcohol_weather_with_0_prcp_temp.csv", header = TRUE)
my_data <- final_weather_alcohol[, c(4,5,11:38)]
correlation_matrix <- cor(my_data)
correlation_matrix
