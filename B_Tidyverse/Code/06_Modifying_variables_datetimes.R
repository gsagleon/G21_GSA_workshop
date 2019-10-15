# Leach et al reduced for GLEON Workshop

# Load Libraries ----
# this is done each time you run a script
library("readxl") # read in excel files
library("tidyverse") # dplyr and piping and ggplot etc
library("lubridate") # dates and times
library("scales") # scales on ggplot ases
library("skimr") # quick summary stats
library("janitor") # clean up excel imports
library("patchwork") # multipanel graphs

# So now we have seen how to look at the data
# What if we wanted to modify the data in terms of columns or rows

# lets read in a new file to add some complexity for fun
exo.df <- read_csv("data/lt_exo_2017_01_23_datetimes.csv")

# So when this comes in 
  # what type of variable is date?
  # what type of variable is time?
# What if we wanted to make a datetime column?

# Mutate and paste ----
# sep is the separator and you just list the variables you want to paste togeher
exo.df <- exo.df %>% 
  mutate(datetime = paste(date, time, sep=" "))

# what if you wanted to separate these varaibles?
exo.df <- exo.df %>% 
  separate(datetime, c("newdate", "newtime"), sep=" ", remove=FALSE)

# note if you wanted to separte newdate into "year", "month", "day" what would you do?
exo.df <- exo.df


# Dates and times -----
# Once you know how to mutate data you can now use lubridate to work with dates
# Sometimes dates and times come in as characters rather than date format
# So we have date and we have datetime but how do we make R understand
# that these are not characters and are POSIXct date times or Dates

# for datetime we do...
exo.df <- exo.df %>% 
  mutate(datetime = mdy_hms(datetime))

# What do you think we would do for the date column? 
# Modify the code below
exo.df <- exo.df %>% 
  mutate(date = (date))

# What is datetime really - When did Time begin?


# How can we modify the datetime to 
exo.df <- exo.df %>% 
          mutate(datetime = ymd_hms(format(
                 strptime("1970-01-01", "%Y-%m-%d", tz = "UTC") +
                 round(as.numeric(ymd_hms(datetime)) / 300) * 300)))

# So if time is in seconds and we want to round to .... we would use ....
# 5 minutes is 300 seconds
# 15 minutes is 900 seconds
# 1 hour is 3600 seconds

# why do this - if you have two datasets and you want them to join together 
# you would need to do this.

# I may or may not go into timezones here but it gets messy fast
# Personally I stick with UTC that has no daylight savings and no timezone


