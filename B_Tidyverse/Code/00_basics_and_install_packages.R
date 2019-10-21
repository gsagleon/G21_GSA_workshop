# This script file will help install packages we will use in class-----

# Install packages -----
install.packages("devtools") # installs new things not on CRAN
install.packages("tidyverse") # dplyr and piping and ggplot etc
install.packages("lubridate") # dates and times
install.packages("scales") # scales on ggplot ases
install.packages("readxl") # read in excel files
install.packages("skimr") # quick summary stats
install.packages("janitor") # clean up excel imports

# special install of packages using devtools -----
devtools::install_github("thomasp85/patchwork") # multiple plots


# Other packages I really like ------
install.packages("styler") # style your code - nice
devtools::install_github("calligross/ggthemeassist") # ggplot GUI



# Load Libraries -----
# these are the commands to load libraries that we will use in class
# there may colored text saying commads are masked but you can test these if you like

# run these only one time----
library("ggthemeassist")

# run these each time you run a script -----
library("readxl") # read in excel files
library("tidyverse") # dplyr and piping and ggplot etc
library("lubridate") # dates and times
library("scales") # scales on ggplot ases
library("skimr") # quick summary stats
library("janitor") # clean up excel imports
library("patchwork") # multipanel graphs
=
# the <- is the assingment operator -----
#' This puts anything on the right and stores it in an object in the environment

x <- 7

x

# this will overwrite.
x <- "Hello there"


#' The other operator that we will use is the pipe or %>%
#' This passes information from the left to the right.
#' You could think of this in your daily morning routine
#' 
#' Person %>% wakes up %>% shower %>% caffeine %>% functional being
#' 
#' This all could then be stored in the functional human object
#' 
#' functional being <- Person %>% wakes up %>% shower %>% caffeine %>% functional being