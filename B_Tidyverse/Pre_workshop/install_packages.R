# This scirpt file is in place to install the packages that we will use
# in this workshop

# Install packages -----
install.packages("devtools") # installs new things not on CRAN
install.packages("tidyverse") # dplyr and piping and ggplot etc
install.packages("lubridate") # dates and times
install.packages("scales") # scales on ggplot ases
install.packages("readxl") # read in excel files
install.packages("skimr") # quick summary stats
install.packages("janitor") # clean up excel imports

devtools::install_github("thomasp85/patchwork") # multiple plots


# Other packages I really like ------
# these you only have to run one time and appear in the Addins drop down
install.packages("styler") # style your code - nice

devtools::install_github("calligross/ggthemeassist") # ggplot GUI
