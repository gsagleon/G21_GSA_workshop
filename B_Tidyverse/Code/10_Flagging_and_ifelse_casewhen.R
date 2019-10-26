# Flagging of outliers and conditional if else commands

# Load Libraries ----
# this is done each time you run a script
library(readxl) # read in excel files
library(tidyverse) # dplyr and piping and ggplot etc
library(lubridate) # dates and times
library(scales) # scales on ggplot ases
library(skimr) # quick summary stats
library(janitor) # clean up excel imports
library(patchwork) # multipanel graphs
library(plotly)

# read in file ----
lakes.df <- read_csv("data/reduced_lake_long_genus_species.csv")

# Lets say we want to look at the data and figure out where outliers are

# I personally like to see what I am doing first
# lets look at only Cladocerans first
lakes.df %>% 
  filter(group=='Cladoceran') %>%
  ggplot(aes(lake_name, org_l, color=group)) +
  geom_point(
  ) 

# Points overlap and you might want to spread them out a bit
lakes.df %>% 
  filter(group=='Cladoceran') %>%
  ggplot(aes(lake_name, org_l, color=group)) +
  geom_point(
    position= position_jitterdodge(jitter.width = 0.3)
  ) 

# There are some high values - how can we figure out what they are...
# there are a few aproaches
clad.plot <- lakes.df %>% 
  filter(group=='Cladoceran') %>%
  ggplot(aes(lake_name, org_l, color=group)) +
  geom_point(
    position= position_jitterdodge(jitter.width = 0.3)
  ) 
clad.plot

# I personally like the plotly package and GGPlotly
clad.plot <- lakes.df %>% 
  filter(group=='Cladoceran') %>%
  ggplot(aes(lake_name, org_l, color=group, date=date, lake_name = lake_name, genus_species= genus_species)) +
  geom_point(
    position= position_jitterdodge(jitter.width = 0.1)
  ) 
clad.plot

ggplotly(clad.plot, tooltip = c("date", "genus_species"))



# We could also do this by looking at the values that are above some threshold
# Display values greater than a value by group ------

lakes.df %>% group_by(lake_name) %>%
  filter(org_l > 40)

# Using multiple conditions
lakes.df %>% group_by(lake_name) %>%
  filter(org_l > 40 & lake_name == "Willis")

# or yes you can use or as |
lakes.df %>% group_by(lake_name) %>%
  filter(org_l > 40 & lake_name == "Willis" |
         org_l > 40 & lake_name == "South" )


# Flagging data -----
# First the ifelse statement

lakes.df <- lakes.df %>%
  mutate(flag = ifelse(org_l >40, "high", "in_range"))

# Look at data...
lakes.df %>% 
  filter(group=='Cladoceran') %>%
  ggplot(aes(lake_name, org_l, color=flag)) +
  geom_point(
    position= position_jitterdodge(jitter.width = 0.1)
  ) 

# Case_when ------
# you can nest if_else statements can be a mess
# this is where case_when comes into play
# this is the same but you can add more conditions easier
lakes.df <- lakes.df %>%
  mutate(flag = case_when(org_l > 40 ~ "high",
                          TRUE ~ "in_range"))

lakes.df <- lakes.df %>%
  mutate(flag = case_when(org_l > 40 ~ "high",
                          org_l < 40 & org_l >=30 ~ "medium",
                          org_l < 30 & org_l >=20 ~ "lower",
                          TRUE ~ "in_range"))
