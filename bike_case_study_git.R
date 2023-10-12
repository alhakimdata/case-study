library("tidyverse") # Helps wrangle data
library("skimr") # Provide summary statistics about variables in data frames, tibbles, data tables and vectors.
library("lubridate") # Helps wrangle date attributes
library("janitor") # Examining and cleaning dirty data


## By the way, before you proceed on reading my data analysis, I am following a steps and instruction on what and how will the
## data analysis turns to a success in finding the insights of this analysis.


## Step 1: Collect the Data

df_trip1 <- read_csv("C:/Users/DEPED/Documents/Coursera Data Analytics Notes/Course 8 Google Data Analytics Capstone Case Study/Case Study Datasets/Bike-share Case Study/202201-divvy-tripdata.csv")
df_trip2 <- read_csv("C:/Users/DEPED/Documents/Coursera Data Analytics Notes/Course 8 Google Data Analytics Capstone Case Study/Case Study Datasets/Bike-share Case Study/202202-divvy-tripdata.csv")
df_trip3 <- read_csv("C:/Users/DEPED/Documents/Coursera Data Analytics Notes/Course 8 Google Data Analytics Capstone Case Study/Case Study Datasets/Bike-share Case Study/202203-divvy-tripdata.csv")
df_trip4 <- read_csv("C:/Users/DEPED/Documents/Coursera Data Analytics Notes/Course 8 Google Data Analytics Capstone Case Study/Case Study Datasets/Bike-share Case Study/202204-divvy-tripdata.csv")
df_trip5 <- read_csv("C:/Users/DEPED/Documents/Coursera Data Analytics Notes/Course 8 Google Data Analytics Capstone Case Study/Case Study Datasets/Bike-share Case Study/202205-divvy-tripdata.csv")
df_trip6 <- read_csv("C:/Users/DEPED/Documents/Coursera Data Analytics Notes/Course 8 Google Data Analytics Capstone Case Study/Case Study Datasets/Bike-share Case Study/202206-divvy-tripdata.csv")
df_trip7 <- read_csv("C:/Users/DEPED/Documents/Coursera Data Analytics Notes/Course 8 Google Data Analytics Capstone Case Study/Case Study Datasets/Bike-share Case Study/202207-divvy-tripdata.csv")
df_trip8 <- read_csv("C:/Users/DEPED/Documents/Coursera Data Analytics Notes/Course 8 Google Data Analytics Capstone Case Study/Case Study Datasets/Bike-share Case Study/202208-divvy-tripdata.csv")
df_trip9 <- read_csv("C:/Users/DEPED/Documents/Coursera Data Analytics Notes/Course 8 Google Data Analytics Capstone Case Study/Case Study Datasets/Bike-share Case Study/202209-divvy-publictripdata.csv")
df_trip10 <- read_csv("C:/Users/DEPED/Documents/Coursera Data Analytics Notes/Course 8 Google Data Analytics Capstone Case Study/Case Study Datasets/Bike-share Case Study/202210-divvy-tripdata.csv")
df_trip11 <- read_csv("C:/Users/DEPED/Documents/Coursera Data Analytics Notes/Course 8 Google Data Analytics Capstone Case Study/Case Study Datasets/Bike-share Case Study/202211-divvy-tripdata.csv")
df_trip12 <- read_csv("C:/Users/DEPED/Documents/Coursera Data Analytics Notes/Course 8 Google Data Analytics Capstone Case Study/Case Study Datasets/Bike-share Case Study/202212-divvy-tripdata.csv")


## Step 2: Wrangle Data and Combine Into a Single File
# Compare column names each of the files
# # While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file

colnames(df_trip1)
colnames(df_trip2)
colnames(df_trip3)
colnames(df_trip4)
colnames(df_trip5)
colnames(df_trip6)
colnames(df_trip7)
colnames(df_trip8)
colnames(df_trip9)
colnames(df_trip10)
colnames(df_trip11)
colnames(df_trip12)

# After we checked the column names of each files. I observed that all of the column names are perfectly match and we can skip on using the rename() Function
# to rename each file that will match every column names.
# Inspect the dataframes and look for incongruencies

str(df_trip1)
str(df_trip2)
str(df_trip3)
str(df_trip4)
str(df_trip5)
str(df_trip6)
str(df_trip7)
str(df_trip8)
str(df_trip9)
str(df_trip10)
str(df_trip11)
str(df_trip12)



# Stack individual quarter's data frames into one big data frame

alltrips <- bind_rows(df_trip1, df_trip2, df_trip3, df_trip4, df_trip5, df_trip6, df_trip7, df_trip8, df_trip9, df_trip10, df_trip11, df_trip12)

# Step 3: Clean Up and Add Data to Prepare for Analysis
# Inspect the new table that has been created


colnames(alltrips) # List of column names
nrow(alltrips) # How many rows are in data frame?
dim(alltrips) # Dimensions of the data frame?
head(alltrips) # Display the first 6 rows of the data frame
tail(alltrips) # Display the last 6 rows of the data frame
str(alltrips) # See the structure of column names along with it's data types
summary(alltrips) # Statistical summary of data mainly for numeric

# We can also use the skimr() function which is an alternative to summary() , quickly providing a broad overview of a data frame.
# skim_without_charts(alltrips)

# I have checked the data frame and it seems all values from each columns are perfectly match

table(alltrips$member_casual) # create a categorical representation of data with variable name and the frequency in the form of a table

# It seems we have the right values in the member_casual column as it should only have 2 levels with "casual" and "member"
# Now, we will add columns that list the date, month, day, and year of each ride
# this will allow us to aggregate ride data for each month, day, or year.

alltrips$date <- as.Date(alltrips$started_at) # The default format is yyyy-mm-dd
alltrips$month <- format(as.Date(alltrips$date), "%m")
alltrips$day <- format(as.Date(alltrips$date), "%d")
alltrips$year <- format(as.Date(alltrips$date), "%Y")
alltrips$day_of_week <- format(as.Date(alltrips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
alltrips$ride_length <- difftime(alltrips$ended_at, alltrips$started_at)

# Inspect the structure of the columns for newly added columns
str(alltrips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(alltrips$ride_length)

alltrips$ride_length <- as.numeric(as.character(alltrips$ride_length))
is.numeric(alltrips$ride_length)

# Remove bad data
# The dataframe includes a few hundred entries when bikes were taken out of docks and
# checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed

alltrips_2 <- alltrips[!(alltrips$start_station_name == "HQ QR" | alltrips$ride_length<0),] %>% 
  na.omit(alltrips$ride_length)

# Step 4: Conduct Descriptive Analysis
# Descriptive analysis on ride_length (all figures in seconds)

# Get the average of the column named ride_length
mean(alltrips_2$ride_length)

# Get the median of the column named ride_length
median(alltrips_2$ride_length)

# Longest ride
max(alltrips_2$ride_length)

# shortest ride
min(alltrips_2$ride_length)

# Compare members and casual users
# The approach using the base R which is difficult to read
# We will be using the tidyverse approach as it is much easy to read the code
aggregate(alltrips_2$ride_length ~ alltrips_2$member_casual, FUN = mean)

# The approach using dplyr package included in tidyverse
alltrips_2 %>% 
  group_by(member_casual) %>% 
  summarise(avg_ride = mean(ride_length))

alltrips_2 %>% 
  group_by(member_casual) %>% 
  summarise(median_ride = median(ride_length))

alltrips_2 %>% 
  group_by(member_casual) %>% 
  summarise(longride = max(ride_length))

alltrips_2 %>% 
  group_by(member_casual) %>% 
  summarise(shortride = min(ride_length))

# See the average ride time by each day for members vs casual users
alltrips_2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(avg_per_day = mean(ride_length))

# Notice that the days of the week are out of order. Let's fix that
alltrips_2$day_of_week <- factor(alltrips_2$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

alltrips_2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(avg_per_day = mean(ride_length))

# Analyze ridership data by type and weekday
alltrips_2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% # creates weekday field using wday()
  group_by(member_casual, weekday) %>% # groups by usertype and weekday
  summarise(number_of_rides = n(), avg_duration = mean(ride_length)) %>% #calculates the number of rides and average duration and calculates the average duration
  arrange(member_casual, weekday) # sorts result

# Let's visualize the number of rides by rider type
alltrips_2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), avg_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
alltrips_2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), avg_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = avg_duration, fill = member_casual)) +
  geom_col(position = "dodge")

# Check my Rpub account for the output of my exploratory data analysis case study