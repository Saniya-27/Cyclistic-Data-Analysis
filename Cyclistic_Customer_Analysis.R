#Installing and Loading Packages

install.packages("here")
library(here)
install.packages("dplyr") 
library(dplyr) 
install.packages("tidyverse") 
library(tidyverse) 
install.packages("purrr") 
library(purrr) 
install.packages("data.table")
library(data.table)
install.packages("tidyverse")
library(tidyverse)
install.packages("lattice")
library(lattice)
install.packages("lubridate")
library(lubridate)


#Reading the files 
df1 <- list.files(path = "Cyclistic/2022/", pattern = "*.csv")  
df2 <- paste0('Cyclistic/2022/',df1)  
df2022 <- map_df(df2,~fread(.))

df1 <- list.files(path = "Cyclistic/2023/", pattern = "*.csv")  
df2 <- paste0('Cyclistic/2023/',df1)  
df2023 <- map_df(df2,~fread(.))

df_Cyclistic <- merge(df2023,df2022, all = TRUE)
remove(df2023,df2022,df,df1,df2)


#Formatting the data as needed
df_Cyclistic$year <- format(df_Cyclistic$started_at,"%Y")
df_Cyclistic$month <- format(df_Cyclistic$started_at,"%h")
df_Cyclistic$started_hour <- format(df_Cyclistic$started_at,"%H")
df_Cyclistic$quarter <- quarter(df_Cyclistic$started_at)
df_Cyclistic$day_of_week <-wday(as.Date(df_Cyclistic$started_at),label=TRUE)
df_Cyclistic$length_of_ride <- as.numeric(difftime(df_Cyclistic$ended_at,df_Cyclistic$started_at, units='mins'))
df_Cyclistic$ride_length <- dplyr::case_when(df_Cyclistic$length_of_ride<1 ~ "<1min",
                                      df_Cyclistic$length_of_ride<10 ~ "<10min",
                                      df_Cyclistic$length_of_ride<30 ~ "<30min",
                                      df_Cyclistic$length_of_ride<60 ~ "<60min",
                                      df_Cyclistic$length_of_ride<1440 ~ "<1 day",
                                      TRUE ~ ">1 day")
df_Cyclistic <- rename(df_Cyclistic,member_type=member_casual)


View(df_Cyclistic)

#Cleaning the data

df_Cyclistic<-distinct(df_Cyclistic,ride_id, .keep_all=TRUE)
df_Cyclistic <-filter(df_Cyclistic,length_of_ride>0)
df_Cyclistic<- select (df_Cyclistic,-c(start_lng,end_lng,start_lat,end_lat))


#Counts
df_total_rides=as.numeric(nrow(df_Cyclistic))
df_total_member_rides=as.numeric(nrow(filter(df_Cyclistic,member_type=='member')))
df_total_casual_rides=as.numeric(nrow(filter(df_Cyclistic,member_type=='casual')))
df_total_classic_bike_rides=as.numeric(nrow(filter(df_Cyclistic,rideable_type=='classic_bike')))
df_total_electric_bike_rides=as.numeric(nrow(filter(df_Cyclistic,rideable_type=='electric_bike')))
df_total_docked_bike_rides=as.numeric(nrow(filter(df_Cyclistic,rideable_type=='docked_bike')))
#Avg Ride length by user type
df_avg_ride_length_member=round(mean(filter(df_Cyclistic,member_type=='member')$length_of_ride),1)
df_avg_ride_length_casual=round(mean(filter(df_Cyclistic,member_type=='casual')$length_of_ride),1)
df_avg_ride_length= round(mean(df_Cyclistic$length_of_ride),1)

##PLOTS

options(scipen = 999)


#Plots to show count of bike rides based on member_type and their preferred bike types
ggplot(df_Cyclistic,aes(x=rideable_type,fill=member_type)) + geom_bar(stat="count", position='dodge')+
  geom_text(aes(label = ..count..), stat="count", size =4,vjust = -0.3, position = position_dodge(.9))+
  labs(title='Total Number of Rides based on Bicycle Type', x='Bicycle Type', y='Number Of Rides', fill = 'Member Type')

#Plots to show count of bike rides based on member_type on a daily basis
ggplot(df_Cyclistic,aes(x=day_of_week, fill=member_type))+geom_bar(stat="count", position = 'dodge')+
  geom_text(aes(label = ..count..), stat="count", size =3,vjust = -0.3, position = position_dodge(.9))+
  labs(title='Total Number of Rides based on the Day Of The Week', x='Day Of The Week', y='Number Of Rides', fill = 'Member Type')

#Avg Ride length on a daily basis
ggplot(df_Cyclistic,aes(x=day_of_week, y=length_of_ride, fill=member_type))+
  geom_bar(stat="summary", position = 'dodge')+
  geom_text(aes(y = stage(length_of_ride, after_stat = y), label = round(after_stat(y),1)),stat="summary", size =3,vjust = -0.3, position = position_dodge(.9))+
  labs(title='Average Length of Rides based on the Day of The Week', x='Month', y='Number Of Rides', fill = 'Member Type')

#Plots to show count of bike rides based on member_type on a monthly basis
ggplot(df_Cyclistic,aes(x=factor(month, level=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')), fill=member_type))+
  geom_bar(stat="count", position = 'dodge')+
  geom_text(aes(label = ..count..), stat="count", size =3,vjust = -0.3, position = position_dodge(.9))+
  labs(title='Total Number of Rides based on the Month', x='Month', y='Number Of Rides', fill = 'Member Type')

#Avg Ride length on a monthly bases
ggplot(df_Cyclistic,aes(x=factor(month, level=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')), y=length_of_ride, fill=member_type))+
  geom_bar(stat="summary", position = 'dodge')+
  geom_text(aes(y = stage(length_of_ride, after_stat = y), label = round(after_stat(y),1)),stat="summary", size =4,vjust = -0.3, position = position_dodge(.9))+
  labs(title='Average Length of Rides based on the Month', x='Month', y='Number Of Rides', fill = 'Member Type')

#Plots to show count of bike rides based on member_type on a Yearly basis
ggplot(df_Cyclistic,aes(x=year, fill=member_type))+geom_bar(stat="count", position = 'dodge')+
  geom_text(aes(label = ..count..), stat="count", size =4,vjust = -0.3, position = position_dodge(.9))+
  labs(title='Total Number of Rides based on the Year', x='Year', y='Number Of Rides', fill = 'Member Type')

#Plots to show count of bike rides based on member_type on an hourly basis
ggplot(df_Cyclistic,aes(x=started_hour, fill=member_type))+geom_bar(stat="count", position = 'dodge')+
  geom_text(aes(label = ..count..), stat="count", size =2,vjust = -0.3, position = position_dodge(.9)  )+
  labs(title='Total Number of Rides based on the Start Of Ride Hour', x='Start Of Ride Hour', y='Number Of Rides', fill = 'Member Type')

#Plots to show long each type of member is renting the bike
ggplot(df_Cyclistic,aes(factor(ride_length,level=c('<1min','<10min','<30min','<60min','<1 day','>1 day')),fill=member_type))+geom_bar(stat='count',position='dodge')+ 
  geom_text(aes(label = ..count..), stat="count", size =4,vjust = -0.3, position = position_dodge(.9)  )+
  labs(title='Total Number of Rides based on the Length Of The Ride', x='Length Of Ride', y='Number Of Rides', fill = 'Member Type')

#popular start stations for casual and members
head(arrange(data.frame(table(filter(df_Cyclistic, start_station_name!='' & member_type=='member')$start_station_name)),-Freq),n=5) %>% 
  ggplot(aes(x=reorder(Var1, -Freq), y=Freq, fill=Var1))+geom_bar(stat='identity')+ 
  geom_text(aes(label = Freq), size = 4,vjust = -0.3, position = position_dodge(.9)  )+
  labs(title='Most Popular Start Stations For Members', x='Station Name', y='Number Of Rides', fill = 'Station Name')

head(arrange(data.frame(table(filter(df_Cyclistic, start_station_name!='' & member_type=='casual')$start_station_name)),-Freq),n=5) %>% 
  ggplot(aes(x=reorder(Var1, -Freq), y=Freq, fill=Var1))+geom_bar(stat='identity')+ 
  geom_text(aes(label = Freq), size = 4,vjust = -0.3, position = position_dodge(.9)  )+
  labs(title='Most Popular Start Stations For Casual Riders', x='Station Name', y='Number Of Rides', fill = 'Station Name')


