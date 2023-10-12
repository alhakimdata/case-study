library("tidyverse")
library("lubridate")
library("skimr")
library("janitor")

## Step 1: COLLECT THE DATA
# I don't need to bind the data as it is only one file
# Since our data is just one, which is included from the tidyverse or Rstudio dataset
df <- read_csv("C:/Users/DEPED/Documents/CSV Samples/USA - Real State/realtor-data.csv")

## Step 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
# Compare column names each of the files
# While the names don't have to be in the same order
# they DO need to match perfectly before we can use a command to join them into one file
# Since, I only have 1 file. There's no need to combine other files.
colnames(df)


# After we checked the column names of each files. I observed that all of the column names are perfectly match and we can skip on using the rename() Function
# to rename each file that will match every column names.
# Inspect the dataframes and look for incongruencies
str(df)

# Stack individual quarter's data frames into one big data frame
# However, we only have one data. So, no need to do it.


# Step 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
# Inspect the new table that has been created
nrow(df)
dim(df)
summary(df)
skim_without_charts(df)

# Clean the column's variable for consistency
clean_names(df)

## Step 4: WE CAN NOW DO AN EXPLORATORY DATA ANALYSIS
# We know now that there are NA values from the columns of data set
# Let's arrange the data set by house_size
df %>% 
  arrange(house_size)

# Let's find for the unique values of acre_lot
df %>% 
  distinct(acre_lot)

# I'll create a new column with a variable name meter_lot for it's metric measurement
# Then, I'll select the newly created column along with some of the columns
df_clean <- df %>% 
  na.omit() %>% 
  mutate(meter_lot = acre_lot * 4047)

# Let's check the new dataset after adding the new column variable meter_lot
df_clean

# I have 82,964 rows/observation after omitting the NA values
# Let's check for the unique values of the variable state
df_clean %>% 
  distinct(state)

# Alternatively, we can type the above code like this
distinct(df_clean, state)

# I know now that there are 9 unique values from the column variable state
# Let's check the unique values for the column variable city
distinct(df_clean, city)

# There are 695 rows/observations for the column variable city
# I will display the unique value of the column variable prev_sold_date
df_clean %>% 
  distinct(prev_sold_date)

# There are 4,632 rows/observations
# Let's check the minimum value of the price
min(df_clean$price)

# I got 10000 as the min price

# Let's get the max value
# Max value of the column variable price
max(df_clean$price)

# The max price is 6e+07
# I will check for the unique values of column variable status
distinct(df_clean, status)

# Now, I know that there is only one unique value the column variable status has.
# I can now dive deeper to what insights we can get from this dataset
# Let's filter for the state == Rhode Island and price >= 10000
df_clean %>% 
  filter(state == "Rhode Island" & price >= 10000)

# We got 12,628 rows/observations
# Take note: Rhode Island is the smallest state in size in the United States.
# Let's keep on going.
filter(df_clean, house_size <= 1000 | meter_lot <= 1000)

# We got a result of 39,682 rows/observations and it's quite interesting, as it has 7 beds and 3 bath
# for the first 10 rows/observations displayed.
# Let's get going to go dive deeper
filter(df_clean, prev_sold_date <= 2019-01-01) %>% 
  select(prev_sold_date, state, price, acre_lot, meter_lot)

# Let's get the mean of the dataset for the state = New York
filter(df_clean, state == "New York") %>% 
  group_by(acre_lot) %>% 
  summarise(nyc_average_price = mean(price))

# I now have 159 rows/observations for the average price grouped by acre_lot in New York
# Let's make a plot about the average prices of column variable "state"
# Let's get the average price for average price grouped by state
df_clean %>% 
  group_by(state) %>% 
  summarise(avg_price = mean(price)) # avg_price gets the values of the mean function grouped by state


df_clean %>% 
  group_by(state) %>% 
  summarise(median_price = median(price)) # median_price gets the values of the median function grouped by state

# Now that I have the average price by states and the median pprice by states
# Let's try to plot the above code by adding ggplot
df_clean %>% 
  group_by(state) %>% 
  summarise(avg_price = mean(price)) %>% 
  ggplot(mapping = aes(x = state, y = avg_price, fill = state))+
  geom_col(position = 'dodge')


# Let's plot the median price now
df_clean %>% 
  group_by(state) %>% 
  summarise(median_price = median(price)) %>% 
  ggplot(mapping = aes(x = state, y = median_price, fill = state))+
  geom_col(position = 'dodge')

## STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software

avg_price_df <- df_clean %>% 
  group_by(state) %>% 
  summarise(avg_price = mean(price))

median_price_df <- df_clean %>% 
  group_by(state) %>% 
  summarise(median_price = median(price))

write_csv(avg_price_df, "C:/Users/Documents/RStudio/Csv Data Analysis Results/avg_price_realstate.csv")
write_csv(median_price_df, "C:/Users/Documents/RStudio/Csv Data Analysis Results/median_price_realstate.csv")
