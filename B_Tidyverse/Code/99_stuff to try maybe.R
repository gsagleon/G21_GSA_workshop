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


# Reading in many csv files and mergin
# find all file names ending in .csv 
data_path <- "data/csv_files/"
files <- dir(data_path,
             pattern = "*.csv")

# reads in many files 
# into on dataframe with all columns and a column of filena.e
temp.df <-  tibble(filename = files) %>% # create a data frame holding the file names
  mutate(file_contents = map(filename,         # read files into
                             ~ read_csv(file.path(data_path, .))) # a new data column
  )   
temp.df

# this unnests the list file that is a different way of working with data
temp.df <- unnest(temp.df) %>% clean_names() %>% remove_empty(which = c("cols", "rows"))


### How to use different color settings

temp.df %>% 
  ggplot(aes(diameter, mass, color = center)) +
  geom_point(size=3)

# what about colors and shape
temp.df %>% 
  ggplot(aes(diameter, mass, color = interaction(color, center))) +
  geom_point(size=3)




