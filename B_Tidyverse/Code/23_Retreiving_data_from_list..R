# This is great to be able to retreive bits of data based on date times


# Load Libraries ----
# this is done each time you run a script
library("readxl") # read in excel files
library("tidyverse") # dplyr and piping and ggplot etc
library("lubridate") # dates and times
library("scales") # scales on ggplot ases
library("skimr") # quick summary stats
library("janitor") # clean up excel imports
library("patchwork") # multipanel graphs


# I will use an example fake dataset for this one.
# These are the dates you want to extract the rangese form
# Read in range data -----
dates_toextract.df <- read_csv("data/datetimes_to_extract.csv") %>% clean_names()

# Read in the original data -----
data.df <- read_csv("data/datetimes_to_extract_from.csv") %>% clean_names()

# Convert Dates to dates -----
dates_toextract.df <- dates_toextract.df %>% 
  mutate(
    begin = mdy_hm(begin),
    end = mdy_hm(end)
  )

data.df <- data.df %>%
  mutate(datetime = mdy_hms(datetime))

# the old way
dates_toextract_long.df <- dates_toextract.df %>%
  # select(-peak) %>%
  gather(time, datetime, -id)

# the new way to make long
dates_toextract_long.df <- dates_toextract.df %>%
  pivot_longer(-id,
               names_to = "time",
               values_to = "datetime")

# Join the dataframes -----
final_subset.df <- full_join(data.df, dates_toextract_long.df, by = "datetime")

# Reorder the dataframes -----
final_subset.df <- final_subset.df %>% select(datetime, id, time, everything())

# Now to extract the data
final_subset.df <- final_subset.df %>% 
  fill(time) %>% # this fills time labels down
  filter(time == "begin") %>% # this selects out the ones called begin
  fill(id) # this fills the id down

