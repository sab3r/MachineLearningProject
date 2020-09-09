library("ggplot2")
theme_set(theme_bw())
library("sf")


library("rnaturalearth")
library("rnaturalearthdata")


world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
head(world)

accidents_df_for_plotting <- read.csv('C:/Users/Soham More/Documents/Github/dmml1/Datasets/Iowa_Road_Accidents/v1_Data_Processing/Road_Accidents_2015_2016.csv', header = TRUE)

## Occupants boxplot

boxplot(accidents_df_for_plotting$Occupants[accidents_df_for_plotting$Occupants < 30], xlab = 'Nmber of occupants',
        ylab = 'Frequency')
p <- ggplot(accidents_df_for_plotting[accidents_df_for_plotting$Occupants < 30,], aes(x='', y = Occupants)) + 
  geom_boxplot(alpha=0.2, fill ="blue")
p

fatality_locations <- accidents_df_for_plotting$Crash.Location[accidents_df$Fatalities>0]
fatality_locations <- sub("\\).*", "", sub(".*\\(", "", fatality_locations)) 
fatality_locations <- data.frame(do.call('rbind', strsplit(as.character(fatality_locations),' ',fixed=TRUE)))
tail(fatality_locations)
colnames(fatality_locations) <- c('longitude', 'latitude')
str(fatality_locations)
fatality_locations$longitude <- as.numeric(as.character(fatality_locations$longitude))
fatality_locations$latitude <- as.numeric(as.character(fatality_locations$latitude))
(fatality_locations_sf <- st_as_sf(fatality_locations, coords = c("longitude", "latitude"), 
                                  crs = 4326, agr = "constant"))


accident_location <- accidents_df_for_plotting$Crash.Location
accident_location
accident_location <- sub("\\).*", "", sub(".*\\(", "", accident_location)) 
accident_location
accident_location_df <- data.frame(do.call('rbind', strsplit(as.character(accident_location),' ',fixed=TRUE)))
tail(accident_location_df)
colnames(accident_location_df) <- c('longitude', 'latitude')

str(accident_location_df)

head(accident_location_df$longitude)
accident_location_df$longitude <- as.numeric(as.character(accident_location_df$longitude))
accident_location_df$latitude <- as.numeric(as.character(accident_location_df$latitude))
head(accident_location_df)

#(accident_location_sf <- st_as_sf(accident_location_df, coords = c("longitude", "latitude"), 
#                   crs = 4326, agr = "constant"))

library("maps")
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)
states <- cbind(states, st_coordinates(st_centroid(states)))

library("tools")
states$ID <- as.character(states$ID)
states$ID <- toTitleCase(states$ID)
head(states)

str(states)

counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("iowa", counties$ID))
counties$area <- as.numeric(st_area(counties))
head(counties)





str(accident_location_df)
accident_location_df$longitude <- as.numeric(accident_location_df$longitude)
accident_location_df$latitude <- as.numeric(accident_location_df$latitude)

(accident_location_sf <- st_as_sf(accident_location_df, coords = c("longitude", "latitude"), 
                                  crs = 4326, agr = "constant"))

#View(counties)
library(ggspatial)

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) + 
  geom_sf(data = counties, fill = NA) +
  #geom_sf(data = counties, aes(fill = area)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = 1) +
  geom_sf(data = accident_location_sf, size = 1, shape = 23, fill = "red") +
  geom_sf(data = fatality_locations_sf, size = 2, shape = 21, fill = "yellow") +
#  geom_point(data = accident_location_df, aes(x = longitude, y = latitude), size = 0.25, shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-96.724, -90.141), ylim = c(40.410, 43.549), expand = TRUE)



# Plotting alcohol stores and weather stations

liquor_store_df <- read.csv('C:/Users/Soham More/Documents/GitHub/dmml1/Datasets/Iowa_Liquor_Sales/v1_Data_Processing/Iowa_Liquor_Sales_Final_2015_2016_v2.csv', header = TRUE)
weather_station_df <- read.csv('C:/Users/Soham More/Documents/GitHub/dmml1/Datasets/Iowa_Weather_Data/NewDatasets/Weather Station Location.csv', header = TRUE)
head(liquor_store_df)
head(weather_station_df)

(weather_station_sf <- st_as_sf(weather_station_df, coords = c("Longitude", "Latitude"), 
                                  crs = 4326, agr = "constant"))


liquor_store_location <- liquor_store_df$Store.Location
head(liquor_store_location)
liquor_store_location <- sub("\\).*", "", sub(".*\\(", "", liquor_store_location)) 
head(liquor_store_location)
liquor_store_location <- unique(liquor_store_location)
write.csv(liquor_store_location,'C:/Users/Soham More/Documents/GitHub/dmml1/Datasets/Iowa_Liquor_Sales/v1_Data_Processing/unique_liquor_store_locations.csv', row.names = FALSE)

sum(grepl("^\\s*$", liquor_store_location))
str(liquor_store_location)
liquor_store_location <- liquor_store_location[!liquor_store_location == '']
liquor_store_location <- trimws(liquor_store_location)
liquor_store_location <- gsub("\\s+"," ",liquor_store_location)  # Replace consecutive spaces with a single space
liquor_store_location <- data.frame(do.call('rbind', strsplit(as.character(liquor_store_location),' ',fixed=TRUE)))
tail(liquor_store_location)
colnames(liquor_store_location) <- c('longitude', 'latitude')


str(liquor_store_location)
liquor_store_location$longitude <- as.numeric(as.character(liquor_store_location$longitude))
liquor_store_location$latitude <- as.numeric(as.character(liquor_store_location$latitude))
head(liquor_store_location)

(liquor_store_sf <- st_as_sf(liquor_store_location,  coords = c("longitude", "latitude"),
                                crs = 4326, agr = "constant"))


library(ggspatial)
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) + 
  geom_sf(data = counties, fill = NA) +
  geom_sf(data = counties, aes(fill = area)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = 1) +
  geom_sf(data = weather_station_sf, size = 2, shape = 23, fill = "red") +
  geom_sf(data = liquor_store_sf, size = 2, shape = 21, fill = "yellow") +
  #geom_label(data = states, aes(X, Y, label = ID), size = 5, fontface = "bold", nudge_y = states$nudge_y) +
  #annotation_scale(location = "bl", width_hint = 0.2) +
  #annotation_north_arrow(location = "bl", which_north = "true", 
  #                       pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
  #                       style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-96.724, -90.141), ylim = c(40.376, 43.549), expand = TRUE)





#####
## Plot seasonality in alcohol sales
#####

alcohol_df_for_plotting <- read.csv("C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\final_merged_alcohol_weather_with_0_prcp_temp_cleaned.csv", header = TRUE)
str(alcohol_df_for_plotting)
#alcohol_df_for_plotting <- aggregate(. ~ Date,data=alcohol_df_for_plotting, FUN=sum, na.rm=TRUE, na.action=NULL)

library(ggplot2)

# Basic line plot
ggplot(data = alcohol_df_for_plotting, aes(x = Date, y = Sale..Dollars., group = 1))+
  geom_line(color = "#00AFBB", size = 1)

ggplot(data = alcohol_df_for_plotting, aes(x = Date, y = TMIN, group = 1))+
  geom_line(color = "#00AFBB", size = 1)


### Plotting Histogram of temperatures

hist(alcohol_df_for_plotting$TMAX)
hist(alcohol_df_for_plotting$TMIN)
boxplot(alcohol_df_for_plotting$PRCP)
hist(alcohol_df_for_plotting$SNWD)
boxplot(alcohol_df_for_plotting$AWND)



ggplot(data = alcohol_df) +
  scale_size_area() + 
  labs(x = "Temperature",
       y = "Frequency",
       title = "Histograms of TMAX and TMIN") +
  geom_histogram(aes(x = TMAX, y=(..count..), fill = TMAX), 
                 alpha=0.2, fill ="red",binwidth=2,position="dodge", size =0.5, color = "#FFFFFF") +
  geom_histogram(aes(x = TMIN, y=(..count..), fill = TMIN), 
                 alpha=0.2,, fill ="blue",binwidth=2,position="dodge", size =0.5, color = "#FFFFFF")
  
ggplot(data = alcohol_df_for_plotting) +
  scale_size_area() + 
  labs(x = "Precipitation",
       y = "Frequency",
       title = "Histograms of PRCP") +
  geom_histogram(aes(x = PRCP, y=(..count..), fill = PRCP), 
                 alpha=0.2, fill ="red",binwidth=2,position="dodge", size =0.5, color = "#FFFFFF")

ggplot(data = alcohol_df_for_plotting) +
  scale_size_area() + 
  labs(x = "Wind",
       y = "Frequency",
       title = "Histograms of AWND") +
  geom_histogram(aes(x = AWND, y=(..count..), fill = AWND), 
                 alpha=0.2,, fill ="blue",binwidth=2,position="dodge", size =0.5, color = "#FFFFFF")

# PLotting ROC

library(pROC)
var1 <- as.numeric(test1$Fatality)
summary(test1$Fatality)
table(fitted.results)
var2 <- as.numeric(fitted.results)
var3 <- as.numeric(weighted_fitted.results)
var4 <- as.numeric(naive_pred)
var5 <- as.numeric(y_pred_svm_linear)
rocobj_weighted_logit <- roc(var1, var3)
rocobj_logit <- roc(var1, var2)
rocobj_naive <- roc(var1, var4)
rocobj_svm <- roc(var1, var5)
#rocobj2 <- roc(aSAH$outcome, aSAH$wfns)

library(ggplot2)


g2 <- ggroc(list(NaiveBayes=rocobj_naive, LogisticRegresssion=rocobj_logit, WeightedLogisticRegression=rocobj_weighted_logit, SVM = rocobj_svm),  size = 1)
g2  + xlab("FPR") + ylab("TPR") + theme_minimal() + ggtitle("ROC Curve Comparison") + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="black", linetype="dashed")



#################################################
##### PLotting time series of alcohol sales #####
#################################################
library(ggplot2)
library(plotly)
library(dplyr)
library(hrbrthemes)
library(knitr)
library(printr)
library(plyr)
library(dplyr)
library(lubridate)
library(gridExtra)
library(reshape2)
library(TTR)
library(xts)

# Load dataset from file
liquor_store_df <- read.csv('C:/Users/Soham More/Documents/GitHub/dmml1/Datasets/Iowa_Liquor_Sales/v1_Data_Processing/Iowa_Liquor_Sales_Final_2015_2016_v2.csv', header = TRUE)
liquor_store_df$Date <- as.Date(liquor_store_df$Date)
ts_date <- as.Date(liquor_store_df$Date)


ts_aggregate_data <- aggregate(x = liquor_store_df[c("Sale..Dollars.")],
                     FUN = sum,
                     by = list(Group.date = liquor_store_df$Date))
head(ts_aggregate_data)

#table(ts_aggregate_data$Sale..Dollars.[ts_aggregate_data$Sale..Dollars. < 200000])


# Get 2015 data only
#ts_aggregate_above_200000 <- ts_aggregate_data[ts_aggregate_data$Sale..Dollars. > 200000]

ts_SMA3 <- SMA(ts_aggregate_data$Sale..Dollars., n=5)
# Usual area chart
p <- ts_aggregate_data %>%
  ggplot( aes(x=Group.date, y=ts_SMA3)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("Alcohol Sales ($)") +
  xlab("Date") +
  theme_ipsum()
# Turn it interactive with ggplotly
p <- ggplotly(p)
p

#ts_df <- xts(ts_aggregate_data[,-1], order.by = as.Date(ts_aggregate_data[,1], "%d/%m/%Y"), frequency = 360)
#summary(ts_df)
#length(ts_df)




## Boxplots

p10 <- ggplot(alcohol_df_for_plotting, aes(x= County, y = log(Sale..Dollars.))) +
  geom_boxplot()
p10


## Correlation matrix 

liquor_df_temp <- read.csv('C:/Users/Soham More/Documents/GitHub/dmml1/Datasets/Iowa_Liquor_Sales/v1_Data_Processing/alcohol_df_aggregate_with_categories.csv', header = TRUE)

str(liquor_df_temp)

install.packages('ggcorrplot')
library(ggcorrplot)
str(alcohol_df_for_plotting)
plot(log(alcohol_df_for_plotting$Sale..Dollars.), alcohol_df_for_plotting$TMAX)
alcohol_df_temp <- alcohol_df_for_plotting
length(alcohol_df_for_plotting$Date)
str(alcohol_df_for_plotting)
length(alcohol_df_temp$Date)
cor_matrix <- alcohol_df_temp[c('Sale..Dollars.', 'Volume.Sold..Liters.', 'TMAX', 'TMIN', 'PRCP', 'AWND', 'SNOW', 'SNWD')]
cor_matrix$Volume.Sold..Liters. <- log(alcohol_df_temp$Volume.Sold..Liters.)
cor_matrix$Sale..Dollars. <- sqrt(alcohol_df_temp$Sale..Dollars.)

head(cor_matrix)
corr <- cor(cor_matrix, use = "complete.obs")
ggcorrplot(corr, lab = TRUE)
head(cor_matrix)






#### Accidents fatalities by factors

accidents_df_fatality <- accidents_df[accidents_df$Fatality == 1,]

ggplot(data=accidents_df_fatality, aes(x=Surface.Conditions)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data=accidents_df_fatality, aes(x=Major.Cause)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data=accidents_df_fatality, aes(x=Light.Conditions)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme(axis.text.x = element_text(angle = 90))

