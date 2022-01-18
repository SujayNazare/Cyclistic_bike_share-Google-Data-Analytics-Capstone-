#Installing and loading all the neccesary packages
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("lubridate")
library(lubridate)
install.packages("ggplot2")
library(ggplot2)
install.packages("readr")
library(readr)
#Setting the directory for R to import the files
getwd()
#Importing neccesary .csv files into R
Apr_2020 <- read_csv("Apr_2020.csv")
May_2020 <- read_csv("May_2020.csv")
Jun_2020 <- read_csv("Jun_2020.csv")
Jul_2020 <- read_csv("Jul_2020.csv")
Aug_2020 <- read_csv("Aug_2020.csv")
Sep_2020 <- read_csv("Sep_2020.csv")
Oct_2020 <- read_csv("Oct_2020.csv")
Nov_2020 <- read_csv("Nov_2020.csv")
Dec_2020 <- read_csv("Dec_2020.csv")
Jan_2021 <- read_csv("Jan_2021.csv")
Feb_2021 <- read_csv("Feb_2021.csv")
Mar_2021 <- read_csv("Mar_2021.csv")
#There are variations in the data types of start_station_id and end_station_id, we need to fix this
Apr_2020 <- mutate(Apr_2020, start_station_id = as.double(start_station_id), end_station_id = as.double(end_station_id))
May_2020 <- mutate(May_2020, start_station_id = as.double(start_station_id), end_station_id = as.double(end_station_id)) 
Jun_2020 <- mutate(Jun_2020, start_station_id = as.double(start_station_id), end_station_id = as.double(end_station_id))
Jul_2020 <- mutate(Jul_2020, start_station_id = as.double(start_station_id), end_station_id = as.double(end_station_id))
Aug_2020 <- mutate(Aug_2020, start_station_id = as.double(start_station_id), end_station_id = as.double(end_station_id))
Sep_2020 <- mutate(Sep_2020, start_station_id = as.double(start_station_id), end_station_id = as.double(end_station_id))
Oct_2020 <- mutate(Oct_2020, start_station_id = as.double(start_station_id), end_station_id = as.double(end_station_id))
Nov_2020 <- mutate(Nov_2020, start_station_id = as.double(start_station_id), end_station_id = as.double(end_station_id))
Dec_2020 <- mutate(Dec_2020, start_station_id = as.double(start_station_id), end_station_id = as.double(end_station_id))
Jan_2021 <- mutate(Jan_2021, start_station_id = as.double(start_station_id), end_station_id = as.double(end_station_id))
Feb_2021 <- mutate(Feb_2021, start_station_id = as.double(start_station_id), end_station_id = as.double(end_station_id))
Mar_2021 <- mutate(Mar_2021, start_station_id = as.double(start_station_id), end_station_id = as.double(end_station_id))
#Converting the data type of started_at/ended_at from character to datetime format (Jan_2021, Feb_2021 & Mar_2021)
Jan_2021 <- mutate(Jan_2021, started_at = as_datetime(started_at), ended_at = as_datetime(ended_at))
Feb_2021 <- mutate(Feb_2021, started_at = as_datetime(started_at), ended_at = as_datetime(ended_at))
Mar_2021 <- mutate(Mar_2021, started_at = as_datetime(started_at), ended_at = as_datetime(ended_at))
#Merging all the tables into a single dataset
final_data <- bind_rows(Apr_2020, May_2020, Jun_2020, Jul_2020, Aug_2020, Sep_2020, Oct_2020, Nov_2020, Dec_2020, Jan_2021, Feb_2021, Mar_2021)
#Eliminating unwanted columnns/data from the dataset
final_data <- final_data %>% select(-c(start_lat, end_lat, start_lng, end_lng))
#Analyzing surface level data from the filtered/cleaned dataset
colnames(final_data)
nrow(final_data)
dim(final_data)
head(final_data)
str(final_data)
summary(final_data)
#Segregating Date, day, month and year into seperate columns
final_data$date <- as.Date(final_data$started_at)
final_data$day <- format(as.Date(final_data$date), "%d")
final_data$month <- format(as_date(final_data$date), "%m")
final_data$year <- format(as_date(final_data$date), "%Y")
final_data$day_of_week <- format(as_date(final_data$date), "%a")
#Calculating the ride time for each of the rides
final_data$ride_length <- difftime(final_data$ended_at, final_data$started_at)
#Eliminating unwanted data from the dataset to get meaningful insights
is.factor(final_data$ride_length) #To ensure if ride_length is a foactor or not. TRUE = yes and FALSE = no 
final_data$ride_length <- as.numeric(as.character(final_data$ride_length)) #To convert the ride_length from chr to num
is.numeric(final_data$ride_length) #To ensure if the above step has been executed. TRUE = yes and FALSE = no
final_data_v2 <- final_data[!(final_data$start_station_name == "" | final_data$ride_length<0),] #Created a new version by removing all the unwanted data from the dataset
#Analyzing the v2.0 of the dataset with respect to thr ride_length
summary(final_data_v2$ride_length)
#Sorting data based on rider type details (casual/member) 
aggregate(final_data_v2$ride_length ~ final_data_v2$member_casual, FUN = mean)
aggregate(final_data_v2$ride_length ~ final_data_v2$member_casual, FUN = median)
aggregate(final_data_v2$ride_length ~ final_data_v2$member_casual, FUN = max)
aggregate(final_data_v2$ride_length ~ final_data_v2$member_casual, FUN = min)
#Calculating average time by day of the week with respect to rider type
final_data_v2$day_of_week <- ordered(final_data_v2$day_of_week, levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
aggregate(final_data_v2$day_of_week ~ final_data_v2$member_casual + final_data_v2$day_of_week, FUN = mean)
#Further analyzing the rider data w.r.t. ride length and day of the week
final_data_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>% drop_na()
#Creating visualizations using ggplot
#Viz.1
final_data_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)   %>% drop_na() %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of rides v Member type", subtitle = "For each day of the week")
#Viz.2
final_data_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% drop_na() %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average ride duration v Day of the week", subtitle = "For each rider type")
#Viz.3
final_data_v2 %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual) %>% drop_na() %>% 
  ggplot(aes(x = rideable_type, y=number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of rides in each ride type by member types")
#Saving a new copy of file for further analysis
write.csv(final_data_v2, file = 'D:/Coursera/Google Data Analytics/M8 - Google Data Analytics Capstone Complete a Case Study/Capstone/Final_Capstone.csv')
#########################




#########################
[Trials/unnecessary]
##########################
str(final_data)
final_data
aggregate(final_data_v2.0$ride_length ~ final_data_v2.0$member_casual, FUN = mean)

View(final_data)
str(final_data)
#
str(Apr_2020)
str(May_2020)
str(Jun_2020)
str(Jul_2020)
str(Aug_2020)
str(Sep_2020)
str(Oct_2020)
str(Nov_2020)
str(Dec_2020)
str(Jan_2021)
str(Feb_2021)
str(Mar_2021)
#
Jan_2021 <- mutate(Jan_2021, started_at = as_datetime(started_at), ended_at = as_datetime(ended_at))
Feb_2021 <- mutate(Feb_2021, started_at = as_datetime(started_at), ended_at = as_datetime(ended_at))
Mar_2021 <- mutate(Mar_2021, started_at = as_datetime(started_at), ended_at = as_datetime(ended_at))

