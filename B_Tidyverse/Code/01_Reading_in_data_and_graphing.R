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

# install.packages("hms")

# Read in file point and click ----
# read in the file south_lake.csv using the Import Dataset in the Environment tab

# click and point uses either
# south_lake <- read_csv("finalized data/south_lake.csv")

# or if you want to specify columns 
# south_lake <- read_csv("finalized data/south_lake.csv", 
#                         col_types = cols(date = col_date(format = "%Y-%m-%d")))

# Read in file using tidyverse code-----
south.df <- read_csv("data/south_lake.csv")


# south.df <- read_csv("data/south_lake_long.csv")

# there are a few tricks here - 
# , guess_max = XXX - this will extend the range readr tries to guess the forma of the variable
# you can also add in a pipe %>% and read in the file and pass that to further prcessing scripts

# Review data structure -----
# Use blue triangle

# data Structure
str(south.df)
# or
glimpse(south.df)

# Saving files -----
# We can save the file we just read in using 
# Saving dataframes -----
# lets say you have made a lot of changes and its now time to save the dataframe

write_csv(south.df, "finalized_data/output_file.csv")


# Note you can read in excel files just as easy
south_excel.df <- read_excel("data/south_lake.xlsx", sheet = "south_lake")

# Graphing data-----
# The key to all data cleaning and analyses is graphing

# GGplot layering ----
# GGplot uses layers to build a graph 
# the data =, x=, and y= is optional in most cases
ggplot(data=south.df, aes(x=date, y=cladoceran))  # this sets up data - note there are no symbols      

# GGplot requires you to add how you want the data presentedo using geoms
# note that the aes statement can all go in the geom_ lines
ggplot(data=south.df, aes(x=date, y=cladoceran)) + # this sets up data 
  geom_line() # this adds a geometry to present the data from the setup

# Add geom_point() -----
# Add points to the graph below using geom_point()
# you can also try other geoms
ggplot(south.df, aes(x=date, y=cladoceran)) +
  
  geom_point(size=3, color="khaki1")+
geom_line(color="darkgreen", linetype="dashed") 
# Add color ----
# What if you wanted red points?
# Add color="red" into the geoms
ggplot(south.df, aes(x=date, y=cladoceran)) +
  geom_line() +
  geom_point() 

# What colors are available? ----
# google --> ggplot colors

# Adding simple axes labels ----
# in the simple labels you can add \n to make a line break... try it after running this code
ggplot(south.df, aes(x=date, y=cladoceran)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = "Animals (Number per Liter)") +
  theme_classic()


# Label expressions -----
# Adding special formatting to labels
# this can get very sophisticated
ggplot(south.df, aes(x=date, y=cladoceran)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", 
       y = expression(bold("Animals (No. L"[-1]*mu*")")))

# the symbol ^ adds a space
# the symbol * adds no space

# Expressions with 2 lines -----
# here you need to add in the statemetn atop
ggplot(south.df, aes(x=date, y=cladoceran)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = expression(bold(atop("Animals (No. L"^-1*")", paste("more stuff on the second line")))))

# Dates on the X-Axis -----
# So now you might want to change the axes scales
# uses the scales pacakges
ggplot(south.df, aes(x=date, y=cladoceran)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = expression(bold("Animals (No. L"^-1*")"))) +
  scale_x_date(date_breaks = "24 weeks",
                   # limits = as_datetime(c('2017-06-01 00:00:00','2017-08-01 00:00:00')),
                   labels=date_format("%Y-%m-%d"), expand=c(0,0))

# what are the dteas and time represented as
# CODE	MEANING
# # # %S	second (00-59)
# # # %M	minute (00-59)
# # # %l	hour, in 12-hour clock (1-12)
# # # %I	hour, in 12-hour clock (01-12)
# # # %H	hour, in 24-hour clock (01-24)
# # # %a	day of the week, abbreviated (Mon-Sun)
# # # %A	day of the week, full (Monday-Sunday)
# # # %e	day of the month (1-31)
# # # %d	day of the month (01-31)
# # # %m	month, numeric (01-12)
# # # %b	month, abbreviated (Jan-Dec)
# # # %B	month, full (January-December)
# # # %y	year, without century (00-99)
# # # %Y	year, with century (0000-9999)

# Themes for graphs -----
# Here we need to start setting the theme for the graph or appearance
# There are built in theme 
ggplot(south.df, aes(x=date, y=cladoceran)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = expression(bold("Animals (No. L"^-1*")"))) +
  scale_x_date(date_breaks = "6 month",
               # limits = as_datetime(c('2017-06-01 00:00:00','2017-08-01 00:00:00')),
               labels=date_format("%Y-%m-%d"), expand=c(0,0)) +
  theme_void()

# NOW - try a few different themes  by modifying the above statement
# themes that are available:
      #' theme_minimal()
      #' theme_gray()
      #' theme_bw()
      #' theme_linedraw()
      #' theme_light()
      #' theme_dark()
      #' theme_minimal()
      #' theme_void()


# Adjusting graph themes ----
# so now there is a formatted axis but we need to change the rotation of the font
# note you can add a base theme and then mondify it with the theme statements
ggplot(south.df, aes(x=date, y=cladoceran)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = expression(bold("Animals (No. L"^-1*")"))) +
  scale_x_date(date_breaks = "6 month",
               limits = as_date(c('1994-06-01', '2006-12-31')),
               labels=date_format("%Y-%m-%d"), expand=c(0,0)) + 
  theme (
    axis.text.x = element_text(size=12, face="bold", angle=45, hjust=1)
     ) 

# GGTHEME ASSISTANT -----
#' So you could ask Dr. Google every theme aspect and spend hours making graphs
#' Use ggtheme assistant - highlight code of graph and go to Addins
#' Select `ggplot Theme Assistant`
#' 
#' This will open a dialog box and then the code will appear below
#' This is where we use the addin ggThemeAssistant
#' 
#' highlight the code below and click addins and ggThemeAssistant

ggplot(south.df, aes(x=date, y=cladoceran)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = expression(bold("Animals (No. L"^-1*")"))) +
  scale_x_date(date_breaks = "6 month",
               limits = as_date(c('1994-06-01', '2006-12-31')),
               labels=date_format("%Y-%m-%d"), expand=c(0,0))


# Commonly used theme settings ------
# This is the set of theme settings used in my code
ggplot(south.df, aes(x=date, y=cladoceran)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = expression(bold("Animals (No. L"^-1*")"))) +
  scale_x_date(date_breaks = "6 month",
               limits = as_date(c('1994-06-01', '2006-12-31')),
               labels=date_format("%Y-%m-%d"), expand=c(0,0))+
  theme(
    axis.ticks.length=unit(-0.25, "cm"),
    # LABLES APPEARANCE
    axis.title.x=element_text(size=14, face="bold"),
    axis.title.y=element_text(size=14, face="bold"),
    axis.text.x = element_text(size=14, face="bold", angle=45, hjust=1.1),
    axis.text.y = element_text(size=14, face="bold", hjust=-.7),
    # plot.title = element_text(hjust = 0.5, colour="black", size=22, face="bold"),
    # LEGEND
    legend.position="none",
    # LEGEND TEXT
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    # LEGEND TITLE
    legend.title = element_text(colour="black", size=16, face="bold"),
    # LEGEND POSITION AND JUSTIFICATION 
    # legend.justification=c(0.1,1),
    # legend.position=c(0.02,.99),
    # REMOVE LEGEND BOX
    # legend.key = element_rect(fill = "transparent", colour = "transparent"), 
    # REMOVE LEGEND BOX
    # legend.background = element_rect(fill = "transparent", colour = "transparent"), 
    # #REMOVE PLOT FILL AND GRIDS
    # panel.background=element_rect(fill = "transparent", colour = "transparent"), 
    # # removes the window background
    # plot.background=element_rect(fill="transparent",colour=NA),
    # # removes the grid lines
    # panel.grid.major = element_blank(), 
    # panel.grid.minor = element_blank(),
    # ADD AXES LINES AND SIZE
    axis.line.x = element_line(color="black", size = 0.3),
    axis.line.y = element_line(color="black", size = 0.3),
    # ADD PLOT BOX
    panel.border = element_rect(colour = "black", fill=NA, size=0.3))


# Creating your own theme -----
# that is a lot of code to add each time so it is better to make your own theme
# you can also make your own theme to avoid this every time
# Theme for Graphs
# Make a new default theme
# Run this and it will store it as an object in the environemtn tab for use later 
theme_gleon <- function(base_size = 14, base_family = "Times")
{
  theme(
    # LABLES APPEARANCE
    axis.title.x=element_text(size=14, face="bold"),
    axis.title.y=element_text(size=14, face="bold"),
    axis.text.x = element_text(size=14, face="bold", angle=45, hjust=1),
    axis.text.y = element_text(size=14, face="bold"),
    # plot.title = element_text(hjust = 0.5, colour="black", size=22, face="bold"),
    # LEGEND
    legend.position="none",
    # LEGEND TEXT
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    # LEGEND TITLE
    legend.title = element_text(colour="black", size=16, face="bold"),
    # LEGEND POSITION AND JUSTIFICATION 
    # legend.justification=c(0.1,1),
    # legend.position=c(0.02,.99),
    # REMOVE LEGEND BOX
    # legend.key = element_rect(fill = "transparent", colour = "transparent"), 
    # REMOVE LEGEND BOX
    # legend.background = element_rect(fill = "transparent", colour = "transparent"), 
    # #REMOVE PLOT FILL AND GRIDS
    # panel.background=element_rect(fill = "transparent", colour = "transparent"), 
    # # removes the window background
    # plot.background=element_rect(fill="transparent",colour=NA),
    # # removes the grid lines
    # panel.grid.major = element_blank(), 
    # panel.grid.minor = element_blank(),
    # ADD AXES LINES AND SIZE
    axis.line.x = element_line(color="black", size = 0.3),
    axis.line.y = element_line(color="black", size = 0.3),
    # ADD PLOT BOX
    panel.border = element_rect(colour = "black", fill=NA, size=0.3))
  }

# Using custom theme -----
# note it cuts down on the code repitition
ggplot(south.df, aes(x=date, y=cladoceran)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = expression(bold("Animals (No. L"^-1*")"))) +
  scale_x_date(date_breaks = "6 month",
               limits = as_date(c('1994-06-01', '2006-12-31')),
               labels=date_format("%Y-%m-%d"), expand=c(0,0)) +
  theme_gleon()

# say this is all you wanted for your advisor and you wanted to save it...
# manual point click method is to use export --> pdf or image
# the code method to save the last plot made is ---
ggsave(last_plot(), 
       file="plot1.jpeg",
       width = 10, height = 8, 
       device="jpeg",
       units="in",
       dpi=300)

# Saving named plots-----
# you can also save the plot and save by name----
plot1.plot <-  ggplot(south.df, aes(x=date, y=cladoceran)) +
  geom_line() +
  geom_point() 
plot1.plot

ggsave(plot1.plot, 
       file="figures/plot1.pdf",
       width = 10, height = 8, 
       units="in",
       dpi=300)



