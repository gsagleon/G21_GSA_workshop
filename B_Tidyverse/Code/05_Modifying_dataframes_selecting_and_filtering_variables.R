# A lot of the data was modified from 
# Leach, TH,  LA Winslow,  FW Acker,  JA Bloomfield,  CW Boylen,  PA Bukaveckas,  
# DF Charles,  RA Daniels,  CT Driscoll,  LW Eichler,  JL Farrell,  CS Funk,  
# CA Goodrich,  TM Michelena,  SA Nierzwicki-Bauer,  KM Roy,  WH  Shaw, 
# JW  Sutherland, MW  Swinton, DA  Winkler, KC  Rose.
# Long-term dataset on aquatic responses to concurrent climate change 
# and recovery from acidification. 2018.  Scientific Data. online.  
# https://doi.org/10.1038/sdata.2018.59.  10.1038/sdata.2018.59


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
lakes.df <- read_csv("data/reduced_lake_long_genus_species.csv")


# first lets look at how to modify a variable

lakes_modified.df <- lakes.df %>%
  mutate(log_org_l = log10(org_l +1))

# we can do essentially any math we want and do it within groups


# the boolean operators are really important here
#' less than <
#' greater than >
#' less than or equal to <=
#' greater than or equal to >=
#' is equal to ==
#' is not equal to !=
#' inclusive of %in% c("x", "y", "z")
#' 
#' These can be combined with 
#'     &  and
#'     | or



# We can remove/retain/or reorder columns using select
# Reorder columns ----
lakes.df <- lakes.df %>%
  select(date, lake_name, permanent_id, group, genus_species, org_l)

# we can also reorder one column to the front
lakes.df <- lakes.df %>%
  select(lake_name, everything())

# we could remove columns
lakes.df <- lakes.df %>%
  select(-permanent_id)

# you can also select columns with pattern matching
# starts_with or ends_with
lakes.df <- lakes.df %>%
  select(starts_with("g"), date, lake_name, everything()  )


# Filtering data and counting data ----
# there are several boolean operators that are useful for filtering data
# we can use these to just see the data or we can use to 

# lets say we wanted to look at only one lake
lakes.df %>% filter(org_l >5) %>%  filter(lake_name == "Willis")


# lets look at some of the data using some simple methods
# how many lakes are there and how many 
lakes.df %>% count(lake_name)

# lets see how many genus species there are
lakes.df %>% count(genus_species)

# now this is odd all have the same N 
# lets look at what the data is a bit more
lakes.df %>% 
  group_by(genus_species) %>%
  filter(org_l==0) %>%
  count(genus_species)

# So there are a lot of 0s - what if we removed that.
lakes.df %>% 
  filter(org_l != 0) %>%
  count(genus_species)


# Conditional flagging of outliers
# if else ----
# what if we wanted to flag all 0  values

lakes.df <- lakes.df %>%
  mutate(flag = ifelse(org_l==0, "ZERO", "NOT ZERO"))


# case when
# we can do the same thing with case_when
lakes.df <- lakes.df %>%
  mutate(flag = case_when(org_l == 0 ~ "ZERO",
                          org_l >0 & org_l < 10  ~ "1 to 10",
                          org_l >=10 & org_l <100 ~ "10 to 100",
                          TRUE ~ "something else"))


