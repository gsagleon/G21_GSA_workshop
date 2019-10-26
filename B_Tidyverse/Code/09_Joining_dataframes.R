# Joining dataframes

# Load Libraries ----
# this is done each time you run a script
library(readxl) # read in excel files
library(tidyverse) # dplyr and piping and ggplot etc
library(lubridate) # dates and times
library(scales) # scales on ggplot ases
library(skimr) # quick summary stats
library(janitor) # clean up excel imports
library(patchwork) # multipanel graphs

# Types of Joins
# great resource is here - https://medium.com/@HollyEmblem/joining-data-with-dplyr-in-r-874698eb8898
# full_join - keeps records in both
# left_join - returns all in left and those mathcing in right
# right_join - returns all in right and those matching in left
# inner_join - return rows that exist in both a and b
# anti_join - returns all in first table where there are not matching values in right
# semi_join - returns all data in left where there is matching in right but no right data


# read in dataframe 1 from excel
join1.df <- read_csv("data/join_data_1.csv") %>%
  mutate(datetime = mdy_hms(datetime))

# read in dataframe 2 from excel
join2.df <- read_csv("data/join_data_2.csv") %>%
  mutate(datetime = mdy_hms(datetime))

# lets make a date time series to merge to as an example
datetime.df <-  data.frame(datetime = seq(ymd_hms("2019-06-30 20:00:00"),
                                          ymd_hms("2019-07-01 18:00:00"),  # now(tzone = "UTC"),
                                         by = "15 min"))


# Lets try merging dataset one to date time
try1.df <- full_join(datetime.df, join1.df, by="datetime") %>% 
  arrange(datetime)

# Lets try merging dataset one to date time
try2.df <- full_join(datetime.df, join2.df, by="datetime") %>% 
  arrange(datetime)


# What if you wanted the data to match in both and time is not absolutely critical
# you can round time to the nearest interval that you want to look at

# How can we modify the datetime to 
join1.df <- join1.df %>% 
  mutate(datetime = ymd_hms(format(
    strptime("1970-01-01", "%Y-%m-%d", tz = "UTC") +
      round(as.numeric(ymd_hms(datetime)) / 900) * 900)))

join2.df <- join2.df %>% 
  mutate(datetime = ymd_hms(format(
    strptime("1970-01-01", "%Y-%m-%d", tz = "UTC") +
      round(as.numeric(ymd_hms(datetime)) / 900) * 900)))


# So if time is in seconds and we want to round to .... we would use ....
# 5 minutes is 300 seconds
# 15 minutes is 900 seconds
# 1 hour is 3600 seconds


