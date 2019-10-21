rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Download the newest packages
# In come cases you'll need to deinstall old package versions first,
# e.g. remove.packages()
install.packages('devtools')
install.packages('adagio')
install.packages('hydroGOF')
library(devtools)
devtools::install_github("GLEON/GLM3r")
devtools::install_github('hdugan/glmtools')


# Load packages
library(glmtools)
library(GLM3r)
library(ggplot2)

# this calibration routine works with two GLM nml-files:
# glm4.nml refers to your model setup and is the default template, this is the file which you should adapt with your
# specific lake information (morphometry, initial conditions, boundary conditions, time discretization, etc.)
# glm3.nml is the work horse of this approach and will be overwritten multiple times by the calibration routine.

# first, what's the name of your GLM setup nml-file? Default is 'glm3.nml'
nml.file = 'glm3.nml' 

# Run GLM
file.copy('glm4.nml', nml.file, overwrite = TRUE)
sim_folder <- getwd()
run_glm()

# the next line will read in your desired time discretization and will divide the data into a calibration and a validation period
# ratio refers to how you want to split the data, as we have only two years here, we will divide it by 1:1
# e.g. a ratio of 2 means that it's calibration : validation of 2:1
calib_periods <- get_calib_periods(nml = nml.file, ratio = 1)
# the list contains three sub-lists, 1 is the total time period, 2 is calibration, 3 is validation

# Compare the simulated data with our observed field data.
out_file <- file.path(sim_folder, 'output/output.nc')
plot_var_nc(out_file, var_name = 'temp')
plot_var_compare(nc_file = out_file, field_file = 'bcs/ME_observed.csv',var_name = 'temp', precision = 'hours')

temp_rmse <- compare_to_field(out_file, field_file = 'bcs/ME_observed.csv', 
                              metric = 'water.temperature', as_value = FALSE, precision= 'hours')
print(paste('total time period:',round(temp_rmse,2),'deg C RMSE'))

# generic setup for a GLM calibration
# calib_setup <- get_calib_setup()
calib_setup <-   setup <- data.frame('pars' = as.character(c('wind_factor','lw_factor','ch','sed_temp_mean','sed_temp_mean',
                                                             'coef_mix_hyp','Kw')),
                                     'lb' = c(0.7,0.7,5e-4,3,8,0.6,0.05),
                                     'ub' = c(2,2,0.002,8,20,0.4,0.5),
                                     'x0' = c(1,1,0.0013,5,15,0.5,0.1))
print(calib_setup)

# Input from user is required:
# variable to which we apply the calibration procedure
var = 'temp'
# simulation path/folder
path = getwd() 
# observed field data
obs = read_field_obs('bcs/ME_observed.csv') 
#"glm" # command to be used, default applies GLM3r
glmcmd = NULL
# Optional variables
# if TRUE, deletes all local csv-files that stores the outcome of previous calibration runs
first.attempt = TRUE 
# define a period for the calibration, this helps you to check the performance of the model on a second time period (= validation)
period = calib_periods[[2]] # list('start' = '2011-01-01 12:00:00', 'stop' = '2011-12-31 12:00:00')
# scaling should be TRUE for CMA-ES
scaling = TRUE 
# optimization method, choose between Covariance Matrix Adaption - Evolution Strategy (CMA-ES) or Nelder-Mead
method = 'CMA-ES' # 'Nelder-Mead'#'CMA-ES'
# objective function, which will be minimized, here the root-mean square error
metric = 'RMSE' 
# refers to a target fit of 1.5 degrees Celsius
target.fit = 1.5 
# refers to a maximum run of 150 calibration iterations
target.iter = 150

# main calibration function
calibrate_sim(var, path, obs, nml.file, calib_setup, glmcmd, first.attempt, period, scaling, method, metric, target.fit, target.iter)

# loads all iterations
results <- read.csv('calib_results_RMSE_temp.csv')
results$DateTime <- as.POSIXct(results$DateTime)
ggplot(results, aes(DateTime, RMSE)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x)) +
  theme_bw() +
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_datetime()

# compares simulated with observed data
plot_var_compare(nc_file = out_file, field_file = 'bcs/ME_observed.csv',var_name = 'temp', precision = 'hours')
temp_rmse <- compare_to_field(out_file, field_file = 'bcs/ME_observed.csv', 
                              metric = 'water.temperature', as_value = FALSE, precision= 'hours')
print(paste('calibration:',round(temp_rmse,2),'deg C RMSE'))

# check the model fit during the validation period
nml <- read_nml(nml.file)
nml <- set_nml(nml, arg_list = calib_periods[[3]])
write_nml(nml,nml.file)

run_glm()
plot_var_compare(nc_file = out_file, field_file = 'bcs/ME_observed.csv',var_name = 'temp', precision = 'hours')
temp_rmse <- compare_to_field(out_file, field_file = 'bcs/ME_observed.csv', 
                              metric = 'water.temperature', as_value = FALSE, precision= 'hours')
print(paste('validation:',round(temp_rmse,2),'deg C RMSE'))

# check the model fit during the whole time period
nml <- read_nml(nml.file)
nml <- set_nml(nml, arg_list =calib_periods[[1]])
write_nml(nml,nml.file)

run_glm()
plot_var_compare(nc_file = out_file, field_file = 'bcs/ME_observed.csv',var_name = 'temp', precision = 'hours')
temp_rmse <- compare_to_field(out_file, field_file = 'bcs/ME_observed.csv', 
                              metric = 'water.temperature', as_value = FALSE, precision= 'hours')
print(paste('total time period:',round(temp_rmse,2),'deg C RMSE'))

# print a matrix of our constrained variable space, the initial value and the calibrated value
calibrated_results <- cbind(calib_setup, 'calibrated' =round(c(results$wind_factor[1], 
                   results$wind_factor[1],
                   results$ch[1],
                   results$sed_temp_mean[1],
                   results$sed_temp_mean.1[1],
                   results$coef_mix_hyp[1],
                   results$Kw[1]),4))
print(calibrated_results)
