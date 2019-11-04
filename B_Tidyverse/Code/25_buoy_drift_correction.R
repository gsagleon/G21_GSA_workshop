# Processing Exosonde Data

# Load Libraries ----
library(tidyverse)
library(readxl)
library(lubridate)
library(patchwork)
library(janitor)
library(scales)
library(colorRamps)
library(akima)

# install package for drift called driftR
# from here: https://cran.r-project.org/web/packages/driftR/vignettes/driftR.html
# install.packages("driftR")
library(driftR)

# Import file ------
# from here https://stackoverflow.com/questions/44273235/how-to-skip-second-line-is-csv-file-while-maintaining-first-line-as-column-names

## Import header ------
df_names <- read_csv("data/2019 10 21 oneota data.csv", 
                     skip=1,  n_max = 0)  %>%
  clean_names() %>% names()

df_names

## Import data -----
oneota.df <- read_csv("data/2019 10 21 oneota data.csv", 
                      col_names = df_names, 
                      skip = 3) %>%
  rename(datetime = time_america_new_york_utc_04_00)%>%
  mutate(datetime = mdy_hms(datetime))

rm(df_names)

# get rid of unused variables
noquote(names(oneota.df))

oneota.df <- oneota.df %>%
  select(-processor_power, -rtc_power, -primary_power, -secondary_power, -sensor_power,
         -total_system_current, -sensor_current, -internal_pressure, -internal_temperature,
         -internal_humidity, -cell_signal_strength, -cell_status )


# driftR drift corrections
oneota.df %>%
  ggplot(aes(datetime, odo_sat)) + geom_line()

odo_sat.df <- oneota.df %>%
  select(datetime, odo_sat, temp_0m) %>%
  mutate(date=as_date(datetime),
         time= hms::hms(second(datetime), minute(datetime), hour(datetime)))

corrected_odo_sat <- dr_factor(odo_sat.df, 
                               corrFactor = corfac, 
                               dateVar= date,
                               timeVar = time, 
                               keepDateTime = TRUE)

corrected_odo_sat_2 <- dr_correctOne(corrected_odo_sat, sourceVar = odo_sat, cleanVar = odo_sat_corr,
                             calVal = 68.7, calStd = 71.9, factorVar = corfac)


corrected_odo_sat_2 %>%
  ggplot(aes(datetime)) + 
  geom_line(aes(y=odo_sat), color="red") +
  geom_line(aes(y=odo_sat_corr), color="darkgreen")



# 




# work with just the temperature data ---
temp.df <- oneota.df %>%
  select(datetime, starts_with("temp_"))

temp_long.df <- temp.df %>%
  pivot_longer(-datetime, 
               names_to = c("junk", "depth"),
               names_sep = "_",
               values_to = "wtemp_c",
               ) %>%
  select(-junk) 

temp_long.df <- temp_long.df %>%
  mutate(depth = as.numeric(str_remove(depth, "[m]")))

# temp_long.df <- temp_long.df %>% 
#   mutate(depth = paste("wtr_", depth, sep="")) 

temp_long.df <- temp_long.df  %>%
  filter(wtemp_c>=0) %>% # select data that is greater or equal to 0
  filter(!is.na(wtemp_c)) # remove all NA values


rlake.df <- temp_long.df %>%
  pivot_wider(names_from = depth,
              values_from = wtemp_c)


# heatmap
# interolated watertemp with depth
# just a note here that the x interpretation step of 1 works with day data as it is using the 
# number of days. The issue comes up when you want to use time. The thing to remember here is 
# time in R is the number of seconds since 1970-01-01 00:00:00 so if you do hours you would use
# 3600 seconds rather than 1
oneota_interp.df <- interp(x = temp_long.df$datetime, 
                            y = temp_long.df$depth,
                            z = temp_long.df$wtemp_c,
                            xo = seq(min(temp_long.df$datetime), max(temp_long.df$datetime), by = 900),
                            yo = seq(min(temp_long.df$depth), max(temp_long.df$depth), by = 0.1),
                            extrap=FALSE,
                           linear=TRUE)





# this converts the interpolated data into a dataframe
oneota_interp.df <- interp2xyz(oneota_interp.df, data.frame = TRUE)

# clean up dates using dplyr
oneota_interp.df <- oneota_interp.df %>%
  mutate(datetime = as_datetime(x)) %>% # interp turned dates into integers and this converts back
  mutate(day = day(datetime)) %>% # create day varaible for plotting
  mutate(year = year(datetime)) %>% # create a four digit year column
  select(-x) %>% #remove x column
  rename(depth=y, wtemp=z) #rename variables



# plot our interpolated data
ggplot(oneota_interp.df, aes(x = datetime, y = depth, z = wtemp, fill = wtemp)) +
  geom_tile() + # used to use geom_raster
  scale_y_reverse(expand=c(0,0)) +
  scale_fill_gradientn(colours=matlab.like(10), na.value = 'gray', name="Water\nTemp \nºC") + 
  scale_x_datetime(date_breaks = "1 week", 
               # limits = as_date(c('2016-12-06','2017-02-25')),
               labels=date_format("%b-%d"), expand=c(0,0)) + 
  ylab("Depth (m)") +
  xlab("") 






