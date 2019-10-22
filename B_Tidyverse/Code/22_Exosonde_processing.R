# Processing Exosonde Data

# Load Libraries ----
library(tidyverse)
library(readxl)
library(lubridate)
library(patchwork)
library(janitor)

# Import file ------
ltk.df <- read_csv("data/lt_exo_2017_01_23.csv", skip = 23) %>%
  clean_names() %>%
  remove_empty(which=c("rows", "cols"))

# Remove special characters ------
# ltk.df <- ltk.df %>% rename_all( ~ gsub('[^a-zA-Z0-9]', '', .))


# Select and rename variables -----
ltk.df <- ltk.df %>%
  select(
    date = "date_mm_dd_yyyy",
    time = "time_hh_mm_ss",
    site = "site_name",
    ph = "p_h",
    wtemp_c = "temp_c",
    spcond_uscm = "sp_cond_µ_s_cm",
    odo_sat_pct = "odo_percent_sat",
    odo_conc_mgl = "odo_mg_l",
    press_psi = "press_psi_a",
    depth_m = "depth_m"
  )

# Change column type and make datetime and dates------
ltk.df <- ltk.df %>% 
  mutate(datetime = paste(date, time, sep=" "),
         datetime = mdy_hms(datetime))
  
# Make meter depth bins -----
ltk.df <- ltk.df %>% mutate(depth_bin_m = round(depth_m,  1))


# What if you wanted a simple grpah of temp and other things with depth
ltk.df %>% 
  ggplot(aes(y=depth_m, x=wtemp_c)) +
  geom_point() +
  geom_path() +
  scale_y_reverse() +
  scale_x_continuous(position = "top")+
  labs(x="Water Temperature (C)", y = "Depth (m)")
  
# Make the dataframe long
ltk_long.df <- ltk.df %>%
  select(-date, -time, -depth_bin_m) %>%
  pivot_longer(-c(site, datetime, depth_m), 
               names_to = "variable",
               values_to = "measurement")

ltk_long.df %>% 
  filter(variable =="wtemp_c" | variable=="odo_conc_mgl") %>%
  ggplot(aes(y=depth_m, x=measurement, color = variable)) +
  geom_point() +
  geom_path() +
  scale_y_reverse() +
  scale_x_continuous(position = "top")+
  labs(x="", y = "Depth (m)") +
  facet_grid(~variable, scales = "free_x")


