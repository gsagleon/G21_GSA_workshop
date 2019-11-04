# Finalized  Plotting 
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

ggplot(south_long.df, aes(date, org_l, 
                          shape=group, linetype=group, color=group)) + # sometimes necessary is , group = group
  geom_point()+
  geom_line()

# We need to tell ggplot to organize the data by some grouping variable
# in this case it is the varaible group
# Inside the aes statement you can add `group =` to specify a grouping term
# try copying the text above and adding `group = group` in the aes statement

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
# try changing the line style or symbol shape
ggplot(south_long.df, aes(date, org_l, color=group)) + 
  geom_point()+
  geom_line()


# Final publication quality graph-----
# Modifying custom colors ------
ggplot(south_long.df, aes(date, org_l, shape=group, color=group)) + # sometimes necessary is , group = group
  geom_point()+
  geom_line() +
  labs(x = "Date", y = expression(bold("Animals (No. L"^-1*")"))) +
  scale_x_date(date_breaks = "6 month",
               limits = as_date(c('1994-06-01', '2006-12-31')),
               labels=date_format("%Y-%m-%d"))  +
  theme(axis.text.x = element_text(size=12, face="bold", angle=45, hjust=1)) +
  scale_color_manual(name = "Taxa", 
                     values = c("blue", "red"),
                     labels = c("Cladoceran", "Copepod")) +
  scale_shape_manual(name = "Group", 
                     values = c(21, 23),
                     labels = c("Cladoceran", "Copepod"))

# If you add in a shape, linetype, or fill above you need to add a corresponding scale command
# you can also add custom fill, shape, linetype and so on by adding in 
# scale_fill_manual
# scale_shape_manual
# scale_linetype_manual # "blank", "solid", "dashed", "dotted", "dotdash", "longdash", and "twodash"
# you can also add size = X to both lines and symbols to change sizes

# Try copying the code above and adding a custom symbol shape
# We can also change sybol size by adding size=3 say in the geom_poit command
# symbols are - 
      # shape = 0, square
      # shape = 1, circle
      # shape = 2, triangle point up
      # shape = 3, plus
      # shape = 4, cross
      # shape = 5, diamond
      # shape = 6, triangle point down
      # shape = 7, square cross
      # shape = 8, star
      # shape = 9, diamond plus
      # shape = 10, circle plus
      # shape = 11, triangles up and down
      # shape = 12, square plus
      # shape = 13, circle cross
      # shape = 14, square and triangle down
      # shape = 15, filled square
      # shape = 16, filled circle
      # shape = 17, filled triangle point-up
      # shape = 18, filled diamond
      # shape = 19, solid circle
      # shape = 20, bullet (smaller circle)
      # shape = 21, filled circle blue
      # shape = 22, filled square blue
      # shape = 23, filled diamond blue
      # shape = 24, filled triangle point-up blue
      # shape = 25, filled triangle point down blue








# Adjusting date scales increments -----
# Note here the labels on the X Axis are too frequent - try changing this below....
# try 6 months, 12 months, 1 years, 52 weeks 
ggplot(south_long.df, aes(date, org_l, color=group)) + # sometimes necessary is , group = group
  geom_point()+
  geom_line() +
  labs(x = "Date", y = expression(bold("Animals (No. L"^-1*")"))) +
  scale_x_date(date_breaks = "52 weeks",
               limits = as_date(c('1994-06-01', '2006-12-31')),
               labels=date_format("%Y-%m-%d"))  +
  theme(axis.text.x = element_text(size=12, face="bold", angle=45, hjust=1)) +
  scale_color_manual(name = "Group", 
                     values = c("blue", "red"),
                     labels = c("Cladoceran", "Copepod"))

# Adjusting numerical scales -----
ggplot(south_long.df, aes(date, org_l, color=group)) + # sometimes necessary is , group = group
  geom_point()+
  geom_line() +
  labs(x = "Date", y = expression(bold("Animals (No. L"^-1*")"))) +
  scale_x_date(date_breaks = "52 weeks",
               limits = as_date(c('1994-06-01', '2006-12-31')),
               labels=date_format("%Y-%m-%d"))  +
  scale_y_continuous(limits = c(0,65), breaks = seq(0, 65, by = 5)) +
  theme(axis.text.x = element_text(size=12, face="bold", angle=45, hjust=1)) +
  scale_color_manual(name = "Group", 
                     values = c("blue", "red"),
                     labels = c("Cladoceran", "Copepod"))

# removing buffer around axes with expand=c(0,0)----
ggplot(south_long.df, aes(date, org_l, color=group)) + # sometimes necessary is , group = group
  geom_point()+
  geom_line() +
  labs(x = "Date", y = expression(bold("Animals (No. L"^-1*")"))) +
  scale_x_date(date_breaks = "52 weeks",
               limits = as_date(c('1994-06-01', '2006-12-31')),
               labels=date_format("%Y-%m-%d"), expand=c(0,0))  +
  scale_y_continuous(limits = c(0,65), breaks = seq(0, 65, by = 5), expand=c(0,0)) +
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
  facet_grid(group~.) # rows by columns
  

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
  facet_grid(group ~ .) # rows by columns


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
             labeller = 
               labeller(group = c("cladoceran" = "a) ",
                           "copepod" = "b) "))) # rows by columns







