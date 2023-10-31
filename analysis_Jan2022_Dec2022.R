### Cyclistic_Exercise_Full_Year_Analysis ###

# This analysis is for case study 1 from the Google Data Analytics Certificate (Cyclistic).  It’s originally based on the case study "'Sophisticated, Clear, and Polished’: Divvy and Data Visualization" written by Kevin Hartman (found here: https://artscience.blog/home/divvy-dataviz-case-study). We will be using the Divvy dataset for the case study. The purpose of this script is to consolidate downloaded Divvy data into a single dataframe and then conduct simple analysis to help answer the key question: “In what ways do members and casual riders use Divvy bikes differently?”

# # # # # # # # # # # # # # # # # # # # # # # 
# Install and load required packages
# # # # # # # # # # # # # # # # # # # # # # #  
install.packages("tidyverse") #helps wrangle data
install.packages("lubridate") #helps wrangle date attributes
install.packages("ggplot2") #helps visualize data
install.packages("janitor") #helps compare dataframes
install.packages("dplyr") #helps for glimpsing data

library(tidyverse)  
library(lubridate)  
library(ggplot2)  
library(janitor)  
library(dplyr)  

getwd() #displays your working directory
setwd("../Documents/divvy-trip-data/") #set working directory to simplify calls to data

#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy datasets (csv files) here
jan22 <- read_csv("202201-divvy-tripdata.csv")
feb22 <- read_csv("202202-divvy-tripdata.csv")
mar22 <- read_csv("202203-divvy-tripdata.csv")
apr22 <- read_csv("202204-divvy-tripdata.csv")
may22 <- read_csv("202205-divvy-tripdata.csv")
jun22 <- read_csv("202206-divvy-tripdata.csv")
jul22 <- read_csv("202207-divvy-tripdata.csv")
aug22 <- read_csv("202208-divvy-tripdata.csv")
sep22 <- read_csv("202209-divvy-tripdata.csv")
oct22 <- read_csv("202210-divvy-tripdata.csv")
nov22 <- read_csv("202211-divvy-tripdata.csv")
dec22 <- read_csv("202212-divvy-tripdata.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare col names each of the files and returns true if we can join them into one file using rbind
# While col names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file
# Alternatively, we call the column name of each file and manually compare (e.g. colnames(jan22))
compare_df_cols_same(jan22, feb22, mar22, apr22, may22, jun22, jul22, aug22, sep22, oct22, nov22, dec22, bind_method = "rbind")

#inspect dataframes and look for incongruencies
str(jan22)
str(feb22)
str(mar22)
str(apr22)
str(may22)
str(jun22) #observed that it's missing start_station_name start_station_id, end_station_name, and end_station_id
str(jul22)
str(aug22) #also missing start/end name and id data
str(sep22) #also missing start/end name and id data
str(oct22)
str(nov22)
str(dec22)
# talk to supervisor on how to proceed regarding missing data

# combine 12 dataframes into a single dataframe 
all_trips <- rbind(jan22, feb22, mar22, apr22, may22, jun22, jul22, aug22, sep22, oct22, nov22, dec22)

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #Check number of rows
dim(all_trips)  #Dimensions of the dataframe
head(all_trips)  #See the first 6 rows of dataframe.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# There are a few problems we will need to fix:
# (1) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data (e.g. day, month, year) that provide additional opportunities to aggregate the data.
# (2) We will want to add a calculated field for length of ride as ride_length for helpful analysis.

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$hour <- format(as.POSIXct(all_trips$started_at),"%H")

# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Converting ride_length from seconds to minutes
all_trips$ride_minutes <- all_trips$ride_length/60
glimpse(all_trips)

# Remove "bad" data
# The dataframe includes a few hundred entries where bikes were missing start/end station id and/or names or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
all_trips_v2 <- all_trips[!(is.na(all_trips$start_station_name) | is.na(all_trips$end_station_name) | is.na(all_trips$start_station_id) | is.na(all_trips$end_station_id) | all_trips$ride_length<0),]


# Creating trip_type variable indicating one-way or round-trip
all_trips_v2 <- all_trips_v2 %>%
  mutate(trip_type = ifelse(start_station_name == end_station_name,"Round Trip","One Way"))

#-------------------------
# Descriptive Analysis
#-------------------------
#descriptive analysis on ride_length in minutes
summary(all_trips_v2$ride_minutes)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median) #difference between mean and median may indicate presence of outliers skewing the average
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# Compare average ride time by each HOUR for members vs casual users
aggregate(all_trips_v2$ride_minutes ~ all_trips_v2$member_casual + all_trips_v2$hour, FUN = mean)

# Sort the days of the week and see the average ride time by each DAY for members vs casual users
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# See the average ride time by MONTH for members vs casual users
aggregate(all_trips_v2$ride_minutes ~ all_trips_v2$member_casual +
            all_trips_v2$month, FUN = sum)
aggregate(all_trips_v2$ride_minutes ~ all_trips_v2$member_casual +
            all_trips_v2$month, FUN = mean)

# Analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()	#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 	#calculates the average duration
  arrange(member_casual, weekday)								#sorts

#-------------------------
# Visual Analysis
#-------------------------
#Day of the week vs. Number of Rides
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_minutes)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "stack")

# Visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_minutes)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_minutes)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

# Create a visualization for median duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,median_duration = median(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = median_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS 
#=================================================
#Export new dataframe that is a subset of the full data frame for a more manageable file
agg_trips <- all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% #creates weekday field
  mutate(month = month(started_at, label = TRUE)) %>% #creates month field
  group_by(date,hour, rideable_type, trip_type, member_casual, start_station_name) %>%
  summarise(total_rides = n(),
            total_duration = sum(ride_minutes)) %>%
  arrange(date)
write_csv(agg_trips, "agg_trips.csv")
