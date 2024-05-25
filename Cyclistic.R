library(tidyverse)
library(dplyr)
library(conflicted)

q1_2019.df <- data.frame(Divvy_Trips_2019_Q1)
q1_2020.df <- data.frame(Divvy_Trips_2020_Q1)

#Going to join the two tables but need to make sure the colnames are the same
#Used the 2020 col names

colnames(q1_2019.df)
colnames(q1_2020.df)

q1_2019.df <- rename(q1_2019.df,
                     ride_id = trip_id,
                     rideable_type = bikeid,
                     started_at = start_time,
                     ended_at = end_time,
                     start_station_name = from_station_name,
                     end_station_name = to_station_name,
                     end_station_id = to_station_id,
                     member_casual = usertype)

#inspecting results 

colnames(q1_2019.df)
colnames(q1_2020.df)

#inspecting the dataframe

str(q1_2019.df)
str(q1_2020.df)

#rideable_type and ride_id do not have the same datatype. 
#Changing them to 2020 data type.

q1_2019.df <- mutate(q1_2019.df, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

all_trips <- bind_rows(q1_2019.df, q1_2020.df)

#removing the other fields 

all_trips <- all_trips %>%

  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "tripduration"))

summary(all_trips)
str(all_trips)
colnames(all_trips)
dim(all_trips)

#I am going to make it more consistent by assigning subscribers as members and customer as casual

all_trips = all_trips %>%
  mutate(member_casual = recode(member_casual,
                                "Subscriber" = "member",
                                "Customer" = "casual"))
view(all_trips$member_casual)

all_trips$date <- as.Date(all_trips$started_at) 
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

str(all_trips)

#to run calculation I need to change it to numeric

is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

clean_all_trips <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

#now we do not have data containing negative distances and the trips from HQ

#Descriptive Analysis

summary(clean_all_trips)

#day_of_week         ride_length      
#Length:788189      Min.   :       1  
#Class :character   1st Qu.:     331  
#Mode  :character   Median :     539  
#Mean   :    1189  
#3rd Qu.:     912  
#Max.   :10628422  

aggregate(clean_all_trips$ride_length ~ clean_all_trips$member_casual, FUN = mean)
aggregate(clean_all_trips$ride_length ~ clean_all_trips$member_casual, FUN = median)
aggregate(clean_all_trips$ride_length ~ clean_all_trips$member_casual, FUN = max)
aggregate(clean_all_trips$ride_length ~ clean_all_trips$member_casual, FUN = min)

aggregate(clean_all_trips$ride_length ~ clean_all_trips$member_casual + clean_all_trips$day_of_week, FUN = mean)

clean_all_trips$day_of_week <- ordered(clean_all_trips$day_of_week, levels=c("Sunday", "Monday",
                                                                       "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(clean_all_trips$ride_length ~ clean_all_trips$member_casual + clean_all_trips$day_of_week,
          FUN = mean)

#calculating number of rides and average duration for each member type and weekday.
#visualize the number of rides by rider type
clean_all_trips%>%
  mutate(weekday=wday(started_at,label=TRUE))%>%#
  group_by(member_casual, weekday)%>% 
  summarise(number_of_rides=n(),
            average_duration=mean(ride_length))%>%
  arrange(member_casual,weekday)%>%
ggplot(aes(x=weekday,y=number_of_rides,fill=member_casual)) +
  geom_col(position="dodge")

#graphing the average duration
clean_all_trips%>%
  mutate(weekday=wday(started_at,label=TRUE))%>%#
  group_by(member_casual, weekday)%>% 
  summarise(number_of_rides=n(),
            average_duration=mean(ride_length))%>%
  arrange(member_casual,weekday)%>%
ggplot(aes(x=weekday,y=average_duration,fill=member_casual)) +
  geom_col(position="dodge") 

counts <- aggregate(clean_all_trips$ride_length ~ clean_all_trips$member_casual +
                      clean_all_trips$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')