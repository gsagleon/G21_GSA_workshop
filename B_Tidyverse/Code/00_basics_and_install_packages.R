# This script will help install packages we will use in class -----

# Install packages -----
<<<<<<< HEAD
# install.packages("devtools") # installs new things not on CRAN
# install.packages("tidyverse") # dplyr and piping and ggplot etc
# install.packages("lubridate") # dates and times
# install.packages("scales") # scales on ggplot ases
# install.packages("readxl") # read in excel files
# install.packages("skimr") # quick summary stats
# install.packages("janitor") # clean up excel imports
# install.packages("plotly") # great for plots

# special install of packages using devtools -----
# devtools::install_github("thomasp85/patchwork") # multiple plots

# Other packages I really like ------
# install.packages("styler") # style your code - nice
# devtools::install_github("calligross/ggThemeAssist") # ggplot GUI
=======
install.packages("devtools") # installs new things not on CRAN
install.packages("tidyverse") # dplyr and piping and ggplot etc
install.packages("lubridate") # dates and times
install.packages("scales") # scales on ggplot ases
install.packages("readxl") # read in excel files
install.packages("skimr") # quick summary stats
install.packages("janitor") # clean up excel imports
<<<<<<< HEAD
install.packages("plotly") # great for interactive plots
=======
install.packages("plotly") # great for plots
>>>>>>> 03d5b841fb9290b5da3a127dde8cbee0199aab29

# special install of packages using devtools -----
devtools::install_github("thomasp85/patchwork") # multiple plots

# Other packages I really like ------
install.packages("styler") # style your code - nice
devtools::install_github("calligross/ggThemeAssist") # ggplot GUI
>>>>>>> 342e763033d69a1005e1be69dfd4fcb0b0ffd545

# Load Libraries -----
# these are the commands to load libraries that we will use in class
# there may colored text saying commads are masked but you can test these if you like

# run these only one time----
<<<<<<< HEAD
library(ggThemeAssist)
library(styler)

# run these each time you run a script -----
=======
<<<<<<< HEAD
library("ggthemeassist")
library(styler)

# run these each time you run a script -----
library("readxl") # read in excel files
library("tidyverse") # dplyr and piping and ggplot etc
library("lubridate") # dates and times
library("scales") # scales on ggplot ases
library("skimr") # quick summary stats
library("janitor") # clean up excel imports
library("patchwork") # multipanel graphs
=======
library(ggThemeAssist)
library(styler)

# run these each time you run a script -----
>>>>>>> 342e763033d69a1005e1be69dfd4fcb0b0ffd545
library(readxl) # read in excel files
library(tidyverse) # dplyr and piping and ggplot etc
library(lubridate) # dates and times
library(scales) # scales on ggplot ases
library(skimr) # quick summary stats
library(janitor) # clean up excel imports
library(patchwork) # multipanel graphs
<<<<<<< HEAD
=======
>>>>>>> 03d5b841fb9290b5da3a127dde8cbee0199aab29
>>>>>>> 342e763033d69a1005e1be69dfd4fcb0b0ffd545
library(plotly) # great for interactive plots

# the <- is the assingment operator -----
#' This puts anything on the right and stores it in an object in the environment

x <- 7

x
 
# hello #####

# this will overwrite the previous assingment
x <- "Hello there"

x 


#' The other operator that we will use is the pipe or %>%
#' This passes information from the left to the right.
#' You could think of this in your daily morning routine
#' 
#' Person %>% wakes up %>% shower %>% caffeine %>% functional being
#' 
#' This all could then be stored in the functional human object
#' 
#' functional being <- Person %>% wakes up %>% shower %>% caffeine*10


