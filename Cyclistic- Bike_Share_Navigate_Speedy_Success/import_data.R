install.packages('tidyverse')
install.packages('lubridate')
install.packages('janitor')


library(tidyverse)
library(lubridate)
library(janitor)



df1 <- read.csv("./Bike-Share-Navigate-Speedy-Success/Data/202004-divvy-tripdata.csv")
df2 <- read.csv("./Bike-Share-Navigate-Speedy-Success/Data/202005-divvy-tripdata.csv")
df3 <- read.csv("./Bike-Share-Navigate-Speedy-Success/Data/202006-divvy-tripdata.csv")
df4 <- read.csv("./Bike-Share-Navigate-Speedy-Success/Data/202007-divvy-tripdata.csv")
df5 <- read.csv("./Bike-Share-Navigate-Speedy-Success/Data/202008-divvy-tripdata.csv")
df6 <- read.csv("./Bike-Share-Navigate-Speedy-Success/Data/202009-divvy-tripdata.csv")
df7 <- read.csv("./Bike-Share-Navigate-Speedy-Success/Data/202010-divvy-tripdata.csv")
df8 <- read.csv("./Bike-Share-Navigate-Speedy-Success/Data/202011-divvy-tripdata.csv")
df9 <- read.csv("./Bike-Share-Navigate-Speedy-Success/Data/202012-divvy-tripdata.csv")
df10 <- read.csv("./Bike-Share-Navigate-Speedy-Success/Data/202101-divvy-tripdata.csv")

### Combine 10 data

bike_rides <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10)
bike_rides <- janitor::remove_empty(bike_rides, which = c("cols", "rows"))
bike_rides <- bike_rides %>%
  distinct()

##clean environment
rm(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10)


##Format date/time from string to time
bike_rides$started_at <- lubridate::ymd_hms(bike_rides$started_at)
bike_rides$ended_at <- lubridate::ymd_hms(bike_rides$ended_at)

###make a copy for further analysis
bike_rides_2 <-bike_rides

##Create ride length, hour and date field
bike_rides_2$start_hour <- lubridate::hour(bike_rides_2$started_at)
bike_rides_2$end_hour <- lubridate::hour(bike_rides_2$ended_at)
bike_rides_2$start_date <- lubridate::date(bike_rides_2$started_at)
bike_rides_2$end_date <- lubridate::date(bike_rides_2$ended_at)
bike_rides_2$ride_length <-difftime(bike_rides_2$ended_at,bike_rides_2$started_at, units="mins")

###The raw data also contains unneeded data such as ride IDs, station IDs, and
##latitude and longitude coordinates ,
#remove rows where ride_length is <= to zero
bike_rides_2 <- bike_rides_2[!(bike_rides_2$ride_length <=0),]
#remove unneeded columns
bike_rides_2 <- bike_rides_2 %>%  
  select(-c(ride_id, start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng))

##Rename column name "member_casual" since there is two possible values, casual and member.
bike_rides_2 <- bike_rides_2 %>% 
  rename(member_type = member_casual)

#convert ride_length to numeric
bike_rides_2$ride_length <- as.numeric(as.character(bike_rides_2$ride_length))


###PLOT - number of rides per hour
bike_rides_2 %>% count(start_hour, sort= TRUE) %>%
  ggplot()+
  geom_line((aes(x=start_hour,y=n)))+
  labs(title="Count of Bike Rides by Hour: Previous 10 months", x="Start Hours", y="Number of Rides")

#converts values from scientific notation 
options(scipen = 999)


##total rides
nrow(bike_rides_2)

##count by member type
bike_rides_2 %>% count(member_type)


#total rides by bike type
bike_rides_2 %>%
  group_by(rideable_type) %>% 
  count(rideable_type)

#PLOT rides by type of bycicle

bike_rides_2 %>%
  group_by(rideable_type, member_type) %>%
  dplyr::summarize(count_trips = n()) %>%  
  ggplot(aes(x=rideable_type, y=count_trips, fill=member_type, color=member_type)) +
  geom_bar(stat='identity', position='dodge') +
  theme_bw()+
  labs(title="Number of Trips by Bicycle Type", x="Bicycle Type", y="Number of Rides")


#min, max, median, mean length of ride
summary(bike_rides_2$ride_length)

#create column day_of_week in text
bike_rides_2$day_of_week <- wday(bike_rides_2$started_at)



#plot number of rides by day of week
bike_rides_2 %>% 
  group_by(member_type, day_of_week) %>%
  dplyr::summarize(count_trips = n()) %>%  
  ggplot(aes(x= day_of_week, y=count_trips, fill=member_type, color=member_type)) +
  geom_bar(stat='identity', position = 'dodge') +
  scale_x_continuous(breaks = 1:7, labels = c("1", "2", "3", "4", "5", "6", "7")) + 
  theme_bw()+
  labs(title ="Number of Rides by Day of Week", x = "Day of Week", y = "Number of Rides")


#Find popular start station for casual
bike_rides_2 %>%
  group_by(member_type,start_station_name) %>%
  dplyr::summarise(number_of_ride = n()) %>%
  filter(start_station_name != "", "casual" == member_type) %>%
  arrange(-number_of_ride) %>%
  head(n=5) %>%
  select(-member_type)
