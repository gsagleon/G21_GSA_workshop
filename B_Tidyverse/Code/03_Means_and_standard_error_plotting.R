# 03 Special Plotting 
# So that was the most simple version of plotting and will get you started
# This will take you the rest of the way to publication ready plotting
# And data exploration

# Load Libraries ----
# this is done each time you run a script
library(readxl) # read in excel files
library(tidyverse) # dplyr and piping and ggplot etc
library(lubridate) # dates and times
library(scales) # scales on ggplot ases
library(skimr) # quick summary stats
library(janitor) # clean up excel imports
library(patchwork) # multipanel graphs

# Statistical Plots ----- 
# Long plot file is the best way to work with data
south_long.df <- read_csv("data/south_lake_long.csv")

# Now lets look at some statistical plot
# try adding in geom_boxplot()
# be careful where you add it
plot1.plot <- ggplot(south_long.df, aes(date, org_l, color=group)) + 
  geom_point() 
plot1.plot

install.packages("plotly")
library(plotly)

ggplotly(plot1.plot)






# to fix overlying points
ggplot(south_long.df, aes(group, org_l, color=group)) + 
  geom_boxplot() +
  geom_jitter(size = 3)
  
  
  geom_point(position= position_jitterdodge(jitter.width = 0.3))


# Histograms------
# look for other geoms 
# example of histogram one group at a time
south_long.df %>%
  filter(group=="cladoceran") %>%
  ggplot( aes(org_l, fille=group)) + 
  geom_histogram(binwidth = 10) 

# Area plots -----
south_long.df %>%
  filter(group=="cladoceran") %>%
  ggplot( aes(org_l, fill=group)) + 
  geom_area(stat="bin")

# look at the ggplot cheat sheet and there are many many more graphs 



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


ggplot(lakes.df, aes(x= year, y = org_l, color=group)) + 
  stat_summary(
               fun.y = mean, na.rm = TRUE,
               geom = "point",
               size = 3) +
  stat_summary(fun.y = mean,
             fun.ymin = function(x) mean(x) - sd(x),
             fun.ymax = function(x) mean(x) + sd(x),
             geom = "errorbar")





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
# Note we could look at this in each lake
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


# the above takes the mean and standard error of all lakes
# what if you wanted to see this for each lake separately?
  # WHAT DO YOU NEED TO ADD
# note that when you do this the y scales are all the same
# what can you do to fix this
  # add 
  # , scales = "free_x" 
  # , scales = "free_y" 
  # , scales = "free" 




