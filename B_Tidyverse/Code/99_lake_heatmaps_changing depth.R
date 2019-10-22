# load the libraries each time you restart R
#load packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(colorRamps)
library(akima)

#use readr package to load mendoata data
mendota.df <- read_csv("data/mendota_temp.csv")


test.df <- mendota.df %>%
  filter(year4==2014) %>%
  filter(month != 6 | depth < 15)

#clean up bad data and out of range data
mendota_clean.df <- mendota.df %>% # use this df for rest of commands
  filter(wtemp>=0) %>% # select data that is greater or equal to 0
  filter(!is.na(wtemp)) %>% # remove all NA values
  select(sampledate, depth, wtemp) # this uses only the variables that are needed

# interolated watertemp with depth
# mendota_interp.df <- mendota_clean.df %>% rename(x=sampledate, y=depth, z=wtemp)
# jsut a note here that the x interpretation step of 1 works with day data as it is using the 
# number of days. The issue comes up when you want to use time. The thing to remember here is 
# time in R is the number of seconds since 1970-01-01 00:00:00 so if you do hours you would use
# 3600 seconds rather than 1
mendota_interp.df <- interp(x = mendota_clean.df$sampledate, 
                            y = mendota_clean.df$depth,
                            z = mendota_clean.df$wtemp,
                            xo = seq(min(mendota_clean.df$sampledate), max(mendota_clean.df$sampledate), by = 1),
                            yo = seq(min(mendota_clean.df$depth), max(mendota_clean.df$depth), by = 0.2),
                            extrap=FALSE,
                            linear=TRUE)


