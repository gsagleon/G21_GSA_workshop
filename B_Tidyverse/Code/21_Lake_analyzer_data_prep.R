# R Lake Analyzer data preparation

# load Libraries ----
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
library(plotly)

# read in file-----
underwater_hourly.df <- read_csv("data/underwater hourly lt buoy data.csv")

# lets look at the data to see what it looks like
temp.plot <- underwater_hourly.df %>% 
  filter(datehour > ymd_hms("2017-01-01 00:00:00") & 
           datehour < ymd_hms("2017-01-02 24:00:00")) %>% 
  ggplot(aes(x=datehour, y=value, color=as.factor(depth))) +
  geom_point()+
  geom_line()
temp.plot  

# if we wanted to make the plot interactive
ggplotly(temp.plot)


# so lets look at making the plot of higher quality
temp.plot <- underwater_hourly.df %>% 
  filter(datehour > ymd_hms("2017-01-01 00:00:00") & 
           datehour < ymd_hms("2017-01-02 24:00:00")) %>% 
  ggplot(aes(x=datehour, y=value, color=as.factor(depth))) +
  geom_point()+
  geom_line() +
  labs(x= "Date time", y = expression(bold("Temperature ("^"o"*"C)" ))) + 
  scale_color_manual(
    name="Depth (M)",
    labels = c("10 m", "20 m", "30 m", "40 m", "50 m"),
    values = c("red", "orange", "yellow", "green", "blue" )) +
  scale_x_datetime(date_breaks = "12 hours", 
               limits = ymd_hms(c('2017-01-01 00:00:00','2017-01-03 00:00:00')),
               labels=date_format("%Y-%b-%d"), expand=c(0,0)) +
  theme(
    # LABLES APPEARANCE
    axis.title.x=element_text(size=14, face="bold"),
    axis.title.y=element_text(size=14, face="bold"),
    axis.text.x = element_text(size=13, face="bold", angle=0, vjust = .5, hjust=.5),
    axis.text.y = element_text(size=13, face="bold"),
    # plot.title = element_text(hjust = 0.5, colour="black", size=22, face="bold"),
    # LEGEND
    legend.position="right",
    # LEGEND TEXT
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    # LEGEND TITLE
    legend.title = element_text(colour="black", size=16, face="bold"),
    # LEGEND POSITION AND JUSTIFICATION 
    # legend.justification=c(0.1,1),
    # legend.position=c(0.02,.99),
    # PLOT COLORS
    # REMOVE BOX BEHIND LEGEND SYMBOLS
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

temp.plot





# filter out sets of data ----
# if you wanted to filter out certain types of data
temp_hourly.df <- underwater_hourly.df %>% 
  filter(variablelong == "water_temp")  %>% 
  filter(datehour > ymd_hms("2017-01-01 00:00:00") & 
           datehour < ymd_hms("2017-01-02 24:00:00")) %>%
  filter(depth < 60)

# lets put wtr_ in front of depth to get ready for lake analyzer
temp_hourly.df <- temp_hourly.df %>%
  mutate(depth = paste("wtr_", depth, sep=""))

# remove variables and spread to wide format
temp_hourly.df <- temp_hourly.df %>% 
  select(-variablelong, -day_night, -hour, - dates) %>%
  spread(depth, value) 

# Prep for lake analyzer----
temp_hourly.df <- temp_hourly.df %>%
  mutate(datetime = datehour) %>%
  select(-datehour) %>%
  select(datetime, everything()) %>%
  mutate(
    datetime=as.character(datetime)
  )

# # Paste wtr_ in front of the depths----
# colnames(hourly_temp.df)[-1] = paste0('wtr_',colnames(hourly_temp.df)[-1])


