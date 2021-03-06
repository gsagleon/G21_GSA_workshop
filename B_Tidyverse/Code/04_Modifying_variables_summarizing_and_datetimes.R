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
library(readxl) # read in excel files
library(tidyverse) # dplyr and piping and ggplot etc
library(lubridate) # dates and times
library(scales) # scales on ggplot ases
library(skimr) # quick summary stats
library(janitor) # clean up excel imports
library(patchwork) # multipanel graphs

# So now we have seen how to look at the data
# What if we wanted to modify the data in terms of columns or rows

# lets read in a new file to add some complexity for fun
lakes.df <- read_csv("data/reduced_lake_long_genus_species.csv")

# Mutate -----
# If you want to modify variables you can change them with MUTATE

# Mutate - log
lakes_modified.df <- lakes.df %>%
  mutate(log_org_l = log10(org_l + 1))

# Mutate and mean ----
lakes_modified.df <- lakes.df %>%
  mutate(mean_org_l = mean(org_l, na.rm=TRUE))

# Mean by group ------
lakes_modified.df <- lakes.df %>%
  group_by(group, lake_name) %>%
  mutate(mean_org_l = mean(org_l, na.rm=TRUE))

# how would you modify this to do the mean by group and lake?
lakes_modified.df <- lakes.df %>%
  group_by(group) %>%
  mutate(mean_org_l = mean(org_l, na.rm=TRUE))

# Mean and Standard Error -----
# there is no na.rm=TRUE for sum so we have to do some special math
lakes_modified.df <- lakes.df %>%
  group_by(group) %>%
  mutate(mean_org_l = mean(org_l, na.rm=TRUE),
         se_org_l = sd(org_l, na.rm = T) / sqrt(sum(!is.na(org_l))))


# So mutate is a key thing we will use a lot in the future
# but this just adds a new column



# Summarize data ----
# What if we wanted a summary dataset rather than adding a new column

# there are two ways...
# the first is do all of the math manually
lakes_summary.df <- lakes.df %>%
  group_by(lake_name, group) %>%
  summarize(mean_org_l = mean(org_l, na.rm=TRUE),
         se_org_l = sd(org_l, na.rm = T) / sqrt(sum(!is.na(org_l))))

write_excel_csv(lakes_summary.df, "finalized_data/summary.csv")

lakes.df %>%
  group_by(lake_name, group) %>%
  summarize(mean_org_l = mean(org_l, na.rm=TRUE)) %>%
  ggplot(aes(lake_name, mean_org_l, color=group)) +
  geom_point() 


# the other way to do this is using skimr to look at summary data

lakes.df %>% filter(org_l>0) %>% group_by(lake_name, group) %>% skim(org_l)

# this can be saved to a dataframe as well
lakes.df %>% dplyr::group_by(group) %>% skim(org_l)


# there are a lot of things we can do with mutate and the possibilities are 
# endless. What would you like to see done?

