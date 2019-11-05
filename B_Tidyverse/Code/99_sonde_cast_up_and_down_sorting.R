# This file will help isolate the down cast and will also separate
# casts at differen locations

# Load Libraries ----
# this is done each time you run a script
library(readxl) # read in excel files
library(tidyverse) # dplyr and piping and ggplot etc
library(lubridate) # dates and times
library(scales) # scales on ggplot ases
library(skimr) # quick summary stats
library(janitor) # clean up excel imports
library(patchwork) # multipanel graphs

# turn off sci notation-----
options(scipen=999)

# read in file ------
sonde.df <- read_excel("data/063570_20190530_1236_Couchiching.xlsx", 
                    sheet="Data", skip=1,
                    guess_max = 1000) %>%
  clean_names()

# rename and reorder columns ------
sonde.df <- sonde.df %>%
  select(datetime = time, 
         pressure, 
         sea_pressure, 
         pressure,
         odo_mgl = dissolved_o_concentration,
         odo_pct = dissolved_o_saturation,
         temperature,
         turbidity,
         depth_m = depth)  

# make depth difference column ------
sonde.df <- sonde.df %>% 
  mutate(depth_diff = depth_m - lag(depth_m, 
                      default = 0, 
                      n = 1L, order_by = datetime))

#add a difference column -
### Svens code -----
# Teurlincx, Sven <S.Teurlincx@nioo.knaw.nl>
# sonde.df <- sonde.df %>%  mutate( diff_depth=c(NA,diff(depth_m))) %>%
#   filter(depth_m >=0)

# remove above water data-----
sonde.df <- sonde.df %>%  filter(depth_m >=0)

# make a time differrence column ----
sonde.df <- sonde.df %>%
  mutate(time_diff = datetime - lag(datetime,
                                     default = min(datetime),
                                     n = 1L, order_by = datetime))

# ceate and index column -----
sonde.df <- sonde.df %>% 
  mutate(index = cumsum(time_diff != dplyr::lag(time_diff, 
                         n=1L, 
                         default = 3))) # note this is the time reading and can try to make esier

# identify the odd and even index values and this will help sort out the 
# data tht needs to be converted to the same group
# https://stackoverflow.com/questions/30756112/how-to-subset-rows-with-only-odd-numbers-in-1-column-in-r
sonde.df <- sonde.df %>%
  mutate(odd = ifelse(index %% 2 == 1, "odd", "even"))

# this makes a new index that is fixed
sonde.df <- sonde.df %>% 
  mutate(new_index = ifelse(odd == "odd", lead(index, 
                                               n=1L, 
                                               order_by = datetime), index))

# here we get rid of all of the upcast data
upcast.df <- sonde.df %>% 
  filter(depth_diff >= 0)



# Lets plot the rate of casts
sonde.df %>% filter(new_index ==4) %>% 
  ggplot(aes(datetime, depth_m)) + geom_line() +
  scale_y_reverse() +
  scale_x_datetime(position = "top") 






