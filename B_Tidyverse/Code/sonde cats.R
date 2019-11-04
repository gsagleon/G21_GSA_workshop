
# Load Libraries ----
# this is done each time you run a script
library(readxl) # read in excel files
library(tidyverse) # dplyr and piping and ggplot etc
library(lubridate) # dates and times
library(scales) # scales on ggplot ases
library(skimr) # quick summary stats
library(janitor) # clean up excel imports
library(patchwork) # multipanel graphs

options(scipen=999)

sonde <- read_excel("data/063570_20190530_1236_Couchiching.xlsx", 
                    sheet="Data", skip=1,
                    guess_max = 1000) %>%
  clean_names()



sonde <- sonde %>% 
  mutate(depth_diff = depth - lag(depth, 
                                       default = 0, 
                                       n = 3L, order_by = time))

sonde <- sonde %>% 
  mutate(time_diff = time - lag(time, 
                                  default = min(time), 
                                  n = 3L, order_by = time))


test <- sonde %>%  filter(depth >=0) 