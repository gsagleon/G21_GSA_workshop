
library(dplyr) 

source('A_EcoForecast/Code/EnKF_functions.R')
source('A_EcoForecast/Code/lake_doc_model.R')

# get model start, stop, full dates, and n_steps
n_en = 100
start = as.Date('2014-06-04')
stop = as.Date('2014-09-20')
time_step = 'days' 
dates = get_model_dates(model_start = start, model_stop = stop, time_step = time_step)
n_step = length(dates)

# get observation matrix
obs_df = readRDS('A_EcoForecast/Data/lake_c_data.rds') %>% 
  select(datetime, doc_lake) 

n_states_est = 1 # number of states we're estimating 

n_params_est = 1 # number of parameters we're calibrating

n_params_obs = 0 # number of parameters for which we have observations

# temp for now, add as input later *****************************
state_sd = rep(.5, n_states_est)

param_sd = rep(.2, n_params_obs)

# setting up matrices
# observations as matrix
obs = get_obs_matrix(obs_df = obs_df,
                     model_dates = dates,
                     n_step = n_step,
                     n_states = n_states_est)


# Y vector for storing state / param estimates and updates
Y = get_Y_vector(n_states = n_states_est,
                 n_params_est = n_params_est,
                 n_step = n_step,
                 n_en = n_en)

# observation error matrix
R = get_obs_error_matrix(n_states = n_states_est,
                         n_params_obs = n_params_obs,
                         n_step = n_step,
                         state_sd = state_sd,
                         param_sd = param_sd)

# observation identity matrix
H = get_obs_id_matrix(n_states = n_states_est,
                      n_params_obs = n_params_obs,
                      n_params_est = n_params_est,
                      n_step = n_step,
                      obs = obs)

# use this later for organizing obs error matrix *******************************
n_states_obs = 1 # number of states we're updating; will be dependent on obs

# initialize Y vector
Y = initialize_Y(Y = Y, obs = obs, n_states_est = n_states_est,
                 n_params_est = n_params_est, n_params_obs = n_params_obs,
                 n_step = n_step, n_en = n_en, state_sd = state_sd, param_sd = param_sd)

drivers = data2[, ]

# start modeling
for(t in 2:n_step){
  for(n in 1:n_en){
    
    # run model; 
    model_output = predict_lake_doc(doc_load = data2$doc_load[t-1], 
                                    doc_lake = Y[1, t-1, n], 
                                    lake_vol = data2$lake_vol[t-1], 
                                    water_out = data2$water_out[t-1],
                                    decay = Y[2, t-1, n])
    
    Y[1 , t, n] = model_output$doc_predict # store in Y vector
    Y[2 , t, n] = model_output$decay
  }
  if(any(!is.na(obs[ , , t]))){
    Y = kalman_filter(Y = Y,
                      R = R,
                      obs = obs,
                      H = H,
                      n_en = n_en,
                      cur_step = t) # updating params / states if obs available
  }
  # update states / params for model config
}

out = list(Y = Y, dates = dates, obs = obs, R = R, model_locations = model_locations)









