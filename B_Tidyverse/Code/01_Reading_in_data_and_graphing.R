# Leach et al Zoplakton reduced for GLEON Workshop

# Load Libraries ----
# this is done each time you run a script
library("readxl") # read in excel files
library("tidyverse") # dplyr and piping and ggplot etc
library("lubridate") # dates and times
library("scales") # scales on ggplot ases
library("skimr") # quick summary stats
library("janitor") # clean up excel imports
library("patchwork") # multipanel graphs

# Read in file point and click ----
# read in the file south_lake.csv

# click and point uses either
# south_lake <- read_csv("finalized data/south_lake.csv")

# or if you want to specify columns 
# south_lake <- read_csv("finalized data/south_lake.csv", 
#                         col_types = cols(date = col_date(format = "%m/%d/%Y")))

# Read in file using tidyverse code-----
south.df <- read_csv("data/South_Lake.csv")

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
ggplot(data=south.df, aes(x=date, y=cladoceran)) + # this sets up data 
  geom_line() # this adds a geometry to present the data from above

# Add geom_point() -----
# Add points to the graph below using geom_point()
ggplot(south.df, aes(x=date, y=cladoceran)) +
  geom_line() 


# Add color ----
# What if you wanted red points?
# Add color="red" into the geoms
ggplot(south.df, aes(x=date, y=cladoceran)) +
  geom_line() +
  geom_point() 


# Adding axes labels ----
ggplot(south.df, aes(x=date, y=cladoceran)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = "Animals (Number per Liter)")


# Label expressions -----
# Adding special formatting to labels
ggplot(south.df, aes(x=date, y=cladoceran)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = expression(bold("Animals (No. L"^-1*")")))


# Dates on the X-Axis -----
# So now you might want to change the axes scales
# uses the scales pacakges
ggplot(south.df, aes(x=date, y=cladoceran)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = expression(bold("Animals (No. L"^-1*")"))) +
  scale_x_date(date_breaks = "6 month",
                   # limits = as_datetime(c('2017-06-01 00:00:00','2017-08-01 00:00:00')),
                   labels=date_format("%Y-%m-%d"), expand=c(0,0))


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
  theme_light()

# NOW - try a few different themes
ggplot(south.df, aes(x=date, y=cladoceran)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = expression(bold("Animals (No. L"^-1*")"))) +
  scale_x_date(date_breaks = "6 month",
               # limits = as_datetime(c('2017-06-01 00:00:00','2017-08-01 00:00:00')),
               labels=date_format("%Y-%m-%d"), expand=c(0,0))

# Adjusting graph themes ----
# so now there is a formatted axis but we need to change the rotation of the font
ggplot(south.df, aes(x=date, y=cladoceran)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = expression(bold("Animals (No. L"^-1*")"))) +
  scale_x_date(date_breaks = "6 month",
               limits = as_date(c('1994-06-01', '2006-12-31')),
               labels=date_format("%Y-%m-%d"), expand=c(0,0)) + 
  theme(
    axis.text.x = element_text(size=12, face="bold", angle=45, hjust=1)
    )

# GGTHEME ASSISTANT -----
#' So you could as Dr. Google every theme aspect and spend hours making grpahs
#' Use ggtheme assistant - highlight code of graph and go to Addins
#' Select ggplot Theme Assistant
#' 
#' This will open a dialog box and then the code will appear below


ggplot(south.df, aes(x=date, y=cladoceran)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = expression(bold("Animals (No. L"^-1*")"))) +
  scale_x_date(date_breaks = "6 month",
               limits = as_date(c('1994-06-01', '2006-12-31')),
               labels=date_format("%Y-%m-%d"), expand=c(0,0))

# Commonly used theme settings ------
# This is the set of theme settings I use

ggplot(south.df, aes(x=date, y=cladoceran)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = expression(bold("Animals (No. L"^-1*")"))) +
  scale_x_date(date_breaks = "6 month",
               limits = as_date(c('1994-06-01', '2006-12-31')),
               labels=date_format("%Y-%m-%d"), expand=c(0,0))+
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



# Creating your own theme -----
# you can also make your own theme to avoid this every time
# Theme for Graphs
# Make a new default theme
# Run this and it will store it as an object for use later
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

# using the new theme
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


