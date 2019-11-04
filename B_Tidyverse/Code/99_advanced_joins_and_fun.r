# Leach et al data


# Load Libraries ----
# this is done each time you run a script
library(readxl) # read in excel files
library(tidyverse) # dplyr and piping and ggplot etc
library(lubridate) # dates and times
library(scales) # scales on ggplot ases
library(skimr) # quick summary stats
library(janitor) # clean up excel imports
library(patchwork) # multipanel graphs

# read in files ----
secchi.df <- read_csv("data/leach_data/secchi.csv") %>% 
  clean_names() %>%
  mutate(date = as.character(date))

dotemp.df <- read_csv("data/leach_data/temp_do_profiles.csv") %>% 
  clean_names() %>%
  mutate(date = as_date(date))%>%
  mutate(date = as.character(date))

chem.df <- read_csv("data/leach_data/waterchem.csv") %>% 
  clean_names() %>%
  mutate(date = as.character(date))


# join basic
leach.df <- full_join(secchi.df, dotemp.df, by = c("lake_name", "date"))
leach.df <- full_join(leach.df, chem.df, by = c("lake_name", "date"))

# join advanced
leach2.df <- full_join(secchi.df, dotemp.df, by = c("lake_name", "date")) %>%
           full_join(., chem.df, by = c("lake_name", "date")) 




