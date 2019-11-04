# Modifying data frame structure

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
# What if we wanted to modify the data in terms of overall structure

# If you were typing in data this might be how it looks
# Read in wide dataframe ----
lakes.df <- read_csv("data/reduced_lake_long_genus_species_wide.csv")


# Convert wide to long format ----
# this is an older method that is simple
lakes_long.df <- lakes.df %>% 
  gather(genus_species, # this will make a new column group with the column names 
         org_l,  # this will make a column of data for the counts per liter
         -lake_name, -date, -permanent_id, -year # "-" means leave alone
  )

lakes_long.df <-  lakes_long.df %>%
  separate(genus_species, c("group", "genus", "species"), 
           sep="_")


# this is the newer way that might be better
lakes_long.df <- lakes.df %>%
  pivot_longer(
    -c(lake_name, date, permanent_id, year),
    names_to = "genus_species", 
    values_to = "org_l")



# Long to Wide format ----
# the older method 
lakes_wide.df <- lakes_long.df %>%
  spread(
    genus_species, org_l
  )

# now the newer method
lakes_wide.df <- lakes_long.df %>%
  pivot_wider(
    id_cols = c( "lake_name", "date" ),
    names_from = genus_species,
    values_from = org_l)


# now the new version of modification can also do cool stuff like 
# summarizing data 
# lets read in a new file to add some complexity for fun
lakes.df <- read_csv("data/reduced_lake_long_genus_species.csv")

# lets simplify the data
lakes.df <- lakes.df %>% select(-genus_species)

# make wide with column headers of group and summarize by lake and date
group_wide.df <- lakes.df %>%
  pivot_wider(names_from = group, 
              values_from = org_l,
              values_fn = list(org_l = mean, na.rm=TRUE))

