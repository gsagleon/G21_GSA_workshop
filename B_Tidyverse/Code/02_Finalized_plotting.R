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

# So now we have seen how to look at the data
# What if we wanted to modify the data in terms of columns or rows

# Making graphs this way can get a bit cumbersome as you might imagine. 
# This is because the data is in what we call wide format 
# The long format is the format often used for Anovas and other stats
# We will go over how to do this later but for now lets just look at the file

south_long.df <- read_csv("data/south_lake_long.csv")

# Plotting long format data ------
# notice that it plots all the data and is sort of a mess...
# there are no groupings of cladocerans or copepods

ggplot(south_long.df, aes(date, org_l)) + # sometimes necessary is , group = group
  geom_point()+
  geom_line()

# Mapping a color to data groups ----
# If you add ", color=group" inside of the aes statement it will map a color to
# each group and it is sometimes necessary to add ", group = group"
ggplot(south_long.df, aes(date, org_l, color=group)) + 
  geom_point()+
  geom_line()

# Mapping shape, size, and linetype------
# this also applies to shape, linestyle and sizes (you won't want to do size)
# , shape = group
# , linetype = group
# , size = group

ggplot(south_long.df, aes(date, org_l, color=group)) + 
  geom_point()+
  geom_line()


# Now lets look at some statistical plot
# try adding in geom_boxplot()

ggplot(south_long.df, aes(group, org_l, color=group)) + 
  geom_point() 
  
# to fix overlying points
ggplot(south_long.df, aes(group, org_l, color=group)) + 
  geom_boxplot() +
  geom_point(position= position_jitterdodge(jitter.width = 0.3))


# look for other plot types like violin or others adn see what you get


# Final publication quality graph-----
# now we can add axes labels and custom colors
ggplot(south_long.df, aes(date, org_l, color=group)) + # sometimes necessary is , group = group
  geom_point()+
  geom_line() +
  labs(x = "Date", y = expression(bold("Animals (No. L"^-1*")"))) +
  scale_x_date(date_breaks = "3 month",
               limits = as_date(c('1994-06-01', '2006-12-31')),
               labels=date_format("%Y-%m-%d"), expand=c(0,0))  +
  theme(axis.text.x = element_text(size=12, face="bold", angle=45, hjust=1)) +
  scale_color_manual(name = "Group", 
                     values = c("blue", "red"),
                     labels = c("Cladoceran", "Copepod"))

# Note here the labels on the X Axis are too frequent - try changing this below....
# try 6 months, 12 months, 1 years, 52 weeks 
ggplot(south_long.df, aes(date, org_l, color=group)) + # sometimes necessary is , group = group
  geom_point()+
  geom_line() +
  labs(x = "Date", y = expression(bold("Animals (No. L"^-1*")"))) +
  scale_x_date(date_breaks = "52 weeks",
               limits = as_date(c('1994-06-01', '2006-12-31')),
               labels=date_format("%Y-%m-%d"), expand=c(0,0))  +
  theme(axis.text.x = element_text(size=12, face="bold", angle=45, hjust=1)) +
  scale_color_manual(name = "Group", 
                     values = c("blue", "red"),
                     labels = c("Cladoceran", "Copepod"))



# Facetting graphs -----
# So lets begin to break it up a bit more... this is a busy graph... what
# if we wanted to see two separate graphs?

# Facet wrap ---- 
# sort of flows graphs around by a variable
ggplot(south_long.df, aes(date, org_l, color=group)) + # sometimes necessary is , group = group
  geom_point()+
  geom_line() +
  labs(x = "Date", y = expression(bold("Animals (No. L"^-1*")"))) +
  scale_x_date(date_breaks = "12 month",
               limits = as_date(c('1994-06-01', '2006-12-31')),
               labels=date_format("%Y-%m-%d"), expand=c(0,0))  +
  theme(axis.text.x = element_text(size=12, face="bold", angle=45, hjust=1)) +
  scale_color_manual(name = "Group", 
                     values = c("blue", "red"),
                     labels = c("Cladoceran", "Copepod")) +
  facet_wrap(~group)


# Facet_grid -----
# lets you make a grid of one or two variables in a grid

ggplot(south_long.df, aes(date, org_l, color=group)) + # sometimes necessary is , group = group
  geom_point()+
  geom_line() +
  labs(x = "Date", y = expression(bold("Animals (No. L"^-1*")"))) +
  scale_x_date(date_breaks = "12 month",
               limits = as_date(c('1994-06-01', '2006-12-31')),
               labels=date_format("%Y-%m-%d"), expand=c(0,0))  +
  theme(axis.text.x = element_text(size=12, face="bold", angle=45, hjust=1)) +
  scale_color_manual(name = "Group", 
                     values = c("blue", "red"),
                     labels = c("Cladoceran", "Copepod")) +
  facet_grid(.~group) # rows by columns
  

# Facet_grid vertical ----
ggplot(south_long.df, aes(date, org_l, color=group)) + # sometimes necessary is , group = group
  geom_point()+
  geom_line() +
  labs(x = "Date", y = expression(bold("Animals (No. L"^-1*")"))) +
  scale_x_date(date_breaks = "12 month",
               limits = as_date(c('1994-06-01', '2006-12-31')),
               labels=date_format("%Y-%m-%d"), expand=c(0,0))  +
  theme(axis.text.x = element_text(size=12, face="bold", angle=45, hjust=1)) +
  scale_color_manual(name = "Group", 
                     values = c("blue", "red"),
                     labels = c("Cladoceran", "Copepod")) +
  facet_grid(group~.) # rows by columns


# Relabelling facet titles -----
# now if we wanted to relabel the graphs we can do that as well

ggplot(south_long.df, aes(date, org_l, color=group)) + # sometimes necessary is , group = group
  geom_point()+
  geom_line() +
  labs(x = "Date", y = expression(bold("Animals (No. L"^-1*")"))) +
  scale_x_date(date_breaks = "12 month",
               limits = as_date(c('1994-06-01', '2006-12-31')),
               labels=date_format("%Y-%m-%d"), expand=c(0,0))  +
  theme(axis.text.x = element_text(size=12, face="bold", angle=45, hjust=1),
        strip.background = element_rect(fill = NA),
        strip.text = element_text(size = 14, face = "bold", hjust = 0)) +
  scale_color_manual(name = "Group", 
                     values = c("blue", "red"),
                     labels = c("Cladoceran", "Copepod")) +
  facet_grid(.~group,
             labeller = labeller(group = c("cladoceran" = "a) Cladoceran",
                                             "copepod" = "b) Copepod"))) # rows by columns





# lets read in a new file to add some complexity for fun
lakes.df <- read_csv("data/Reduced_Lake_Long_Genus_Species.csv")


# Special plotting-----


# now we can do some more fun stuff
# what if we wanted to just look at the mean density by year?
# we can do this in ggplot

# lets make a year column
lakes.df <- lakes.df %>%
  mutate( year = year(date))

# now for the plot
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



