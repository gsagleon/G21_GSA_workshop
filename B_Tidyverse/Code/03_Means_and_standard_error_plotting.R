# 03 Special Plotting 
# So that was the most simple version of plotting and will get you started
# This will take you the rest of the way to publication ready plotting
# And data exploration

# Load Libraries ----
# this is done each time you run a script
library("readxl") # read in excel files
library("tidyverse") # dplyr and piping and ggplot etc
library("lubridate") # dates and times
library("scales") # scales on ggplot ases
library("skimr") # quick summary stats
library("janitor") # clean up excel imports
library("patchwork") # multipanel graphs

# 

# lets read in a new file to add some complexity for fun
lakes.df <- read_csv("data/reduced_lake_long_genus_species.csv")


# Special mean and standard error plotting-----
# GGplot has a lot of aspects that make it super fast.
ggplot(lakes.df, aes(year, color=group)) + 
  stat_summary(aes(y = org_l),
               fun.y = mean, na.rm = TRUE,
               geom = "point",
               size = 3) + 
  stat_summary(aes(y = org_l),
               fun.y = mean, na.rm = TRUE,
               geom = "line",
               size = 1) +
  stat_summary(aes(y = org_l),
               fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               width = 0.2) +
  labs(x = "Date", y = "Animals (Number per Liter)") +
  scale_color_manual(name = "Group", 
                     values = c("blue", "red"),
                     labels = c("Cladoceran", "Copepod"))

# we can offset the points so they dont overlap
ggplot(lakes.df, aes(year, color=group)) + 
  stat_summary(aes(y = org_l),
               fun.y = mean, na.rm = TRUE,
               geom = "point",
               size = 3, 
               position = position_dodge(0.2)) +
  stat_summary(aes(y = org_l),
               fun.y = mean, na.rm = TRUE,
               geom = "line",
               size = 1, 
               position = position_dodge(0.2)) +
  stat_summary(aes(y = org_l),
               fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               width = 0.2, 
               position = position_dodge(0.2)) +
  labs(x = "Date", y = "Animals (Number per Liter)") +
  scale_color_manual(name = "Group", 
                     values = c("blue", "red"),
                     labels = c("Cladoceran", "Copepod"))

# Mean and Standard error by lake
#Note we could look at this in each lake
ggplot(lakes.df, aes(year, color=group)) + 
  stat_summary(aes(y = org_l),
               fun.y = mean, na.rm = TRUE,
               geom = "point",
               size = 3, 
               position = position_dodge(0.2)) +
  stat_summary(aes(y = org_l),
               fun.y = mean, na.rm = TRUE,
               geom = "line",
               size = 1, 
               position = position_dodge(0.2)) +
  stat_summary(aes(y = org_l),
               fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               width = 0.2, 
               position = position_dodge(0.2)) +
  labs(x = "Date", y = "Animals (Number per Liter)") +
  scale_color_manual(name = "Group", 
                     values = c("blue", "red"),
                     labels = c("Cladoceran", "Copepod")) 
# WHAT DO YOU NEED TO ADD
# WHAT DO YOU NEED TO ADD TO MAKE SCALES Y Free?



