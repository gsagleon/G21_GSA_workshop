
#' retreive the model time steps based on start and stop dates and time step
#'
#' @param model_start model start date in date class
#' @param model_stop model stop date in date class
#' @param time_step model time step, defaults to daily timestep
get_model_dates = function(model_start, model_stop, time_step = 'days'){
  
  model_dates = seq.Date(from = as.Date(model_start), to = as.Date(model_stop), by = time_step)
  
  return(model_dates)
}

#' vector for holding states and parameters for updating
#'
#' @param n_states number of states we're updating in data assimilation routine
#' @param n_params_est number of parameters we're calibrating
#' @param n_step number of model timesteps
#' @param n_en number of ensembles
get_Y_vector = function(n_states, n_params_est, n_step, n_en){
  
  Y = array(dim = c(n_states + n_params_est, n_step, n_en))
  
  return(Y)
}

#' observation error matrix, should be a square matrix where
#'   col & row = the number of states and params for which you have observations
#'
#' @param n_states number of states we're updating in data assimilation routine
#' @param n_param_obs number of parameters for which we have observations
#' @param n_step number of model timesteps
#' @param state_sd vector of state observation standard deviation; assuming sd is constant through time
#' @param param_sd vector of parmaeter observation standard deviation; assuming sd is constant through time
get_obs_error_matrix = function(n_states, n_params_obs, n_step, state_sd, param_sd){
  
  R = array(0, dim = c(n_states + n_params_obs, n_states + n_params_obs, n_step))
  
  state_var = state_sd^2 #variance of temperature observations
  
  param_var = param_sd^2
  
  if(n_params_obs > 0){
    all_var = c(state_var, param_var)
  }else{
    all_var = state_var
  }
  
  for(i in 1:n_step){
    # variance is the same for each depth and time step; could make dynamic or varying by time step if we have good reason to do so
    R[,,i] = diag(all_var, n_states + n_params_obs, n_states + n_params_obs)
  }
  
  return(R)
}

#' Measurement operator matrix saying 1 if there is observation data available, 0 otherwise
#'
#' @param n_states number of states we're updating in data assimilation routine
#' @param n_param_obs number of parameters for which we have observations
#' @param n_params_est number of parameters we're calibrating
#' @param n_step number of model timesteps
#' @param obs observation matrix created with get_obs_matrix function
get_obs_id_matrix = function(n_states, n_params_obs, n_params_est, n_step, obs){
  
  H = array(0, dim=c(n_states + n_params_obs, n_states + n_params_est, n_step))
  
  # order goes 1) states, 2)params for which we have obs, 3) params for which we're estimating but don't have obs
  
  for(t in 1:n_step){
    H[1:(n_states + n_params_obs), 1:(n_states + n_params_obs), t] = diag(ifelse(is.na(obs[,,t]),0, 1), n_states + n_params_obs, n_states + n_params_obs)
  }
  
  return(H)
}


#' turn observation dataframe into matrix
#'
#' @param obs_df observation data frame
#' @param model_dates dates over which you're modeling
#' @param n_step number of model time steps
#' @param n_states number of states we're updating in data assimilation routine
get_obs_matrix = function(obs_df, model_dates, n_step, n_states){
  
  # need to know location and time of observation
  
  obs_df_filtered = obs_df %>%
    dplyr::filter(as.Date(datetime) %in% model_dates) %>%
    mutate(date = as.Date(datetime)) %>%
    select(date, doc_lake) %>%
    mutate(date_step = which(model_dates %in% date))

  obs_matrix = array(NA, dim = c(n_states, 1, n_step))

  for(j in obs_df_filtered$date_step){
    obs_matrix[1, 1, j] = dplyr::filter(obs_df_filtered,
                                        date_step == j) %>%
      pull(doc_lake)
  }
  
  return(obs_matrix)
}



##' @param Y vector for holding states and parameters you're estimating
##' @param R observation error matrix
##' @param obs observations at current timestep
##' @param H observation identity matrix
##' @param n_en number of ensembles
##' @param cur_step current model timestep
kalman_filter = function(Y, R, obs, H, n_en, cur_step){
  
  cur_obs = obs[ , , cur_step]
  
  cur_obs = ifelse(is.na(cur_obs), 0, cur_obs) # setting NA's to zero so there is no 'error' when compared to estimated states
  
  ###### estimate the spread of your ensembles #####
  Y_mean = matrix(apply(Y[ , cur_step, ], MARGIN = 1, FUN = mean), nrow = length(Y[ , 1, 1])) # calculating the mean of each temp and parameter estimate
  delta_Y = Y[ , cur_step, ] - matrix(rep(Y_mean, n_en), nrow = length(Y[ , 1, 1])) # difference in ensemble state/parameter and mean of all ensemble states/parameters
  
  ###### estimate Kalman gain #########
  K = ((1 / (n_en - 1)) * delta_Y %*% t(delta_Y) %*% matrix(t(H[, , cur_step]))) %*%
    qr.solve(((1 / (n_en - 1)) * H[, , cur_step] %*% delta_Y %*% t(delta_Y) %*% matrix(t(H[, , cur_step])) + R[, , cur_step]))
  
  ###### update Y vector ######
  for(q in 1:n_en){
    Y[, cur_step, q] = Y[, cur_step, q] + K %*% (cur_obs - H[, , cur_step] %*% Y[, cur_step, q]) # adjusting each ensemble using kalman gain and observations
  }
  return(Y)
}



#' initialize Y vector with draws from distribution of obs
#'
#' @param Y Y vector
#' @param obs observation matrix
initialize_Y = function(Y, obs, init_params, n_states_est, n_params_est, n_params_obs, n_step, n_en, state_sd, param_sd){
  
  # initializing states with earliest observations and parameters
  first_obs = coalesce(!!!lapply(seq_len(dim(obs)[3]), function(i){obs[,,i]})) %>% # turning array into list, then using coalesce to find first obs in each position.
    ifelse(is.na(.), mean(., na.rm = T), .) # setting initial temp state to mean of earliest temp obs from other sites if no obs
  
  if(n_params_est > 0){
    ## update this later ***********************
    first_params = init_params
  }else{
    first_params = NULL
  }
  
  Y[ , 1, ] = array(abs(rnorm(n = n_en * (n_states_est + n_params_est),
                              mean = c(first_obs, first_params),
                              sd = c(state_sd, param_sd))),
                    dim = c(c(n_states_est + n_params_est), n_en))
  
  return(Y)
}


#' matrix for holding driver data
#'
#' @param drivers_df dataframe which holds all the driver data 
#' @param model_dates dates for model run 
#' @param n_drivers number of model drivers 
#' @param driver_colnames column names of the drivers in the driver dataframe 
#' @param driver_cv coefficient of variation for each driver data 
#' @param n_step number of model timesteps
#' @param n_en number of ensembles
get_drivers = function(drivers_df, model_dates, n_drivers, driver_colnames, driver_cv, n_step, n_en){
  
  drivers_filtered = drivers_df %>% 
    dplyr::filter(as.Date(datetime) %in% model_dates)

  drivers_out = array(NA, dim = c(n_step, n_drivers, n_en))
  
  for(i in 1:n_drivers){
    for(j in 1:n_step){
      drivers_out[j,i,] = rnorm(n = n_en, 
                               mean = as.numeric(drivers_filtered[j, driver_colnames[i]]),
                               sd = as.numeric(driver_cv[i] * drivers_filtered[j, driver_colnames[i]]))
    }
  }
  
  return(drivers_out) 
}


#' wrapper for running EnKF 
#' 
#' @param n_en number of model ensembles 
#' @param start start date of model run 
#' @param stop date of model run
#' @param time_step model time step, defaults to days 
#' @param obs_file observation file 
#' @param driver_file driver data file 
#' @param n_states_est number of states we're estimating 
#' @param n_params_est number of parameters we're estimating 
#' @param n_params_obs number of parameters for which we have observations 
#' @param decay_init initial decay rate of DOC 
#' @param obs_cv coefficient of variation of observations 
#' @param param_cv coefficient of variation of parameters 
#' @param driver_cv coefficient of variation of driver data for DOC Load, Discharge out, and Lake volume, respectively 
#' @param init_cond_cv initial condition CV (what we're )
EnKF = function(n_en = 100, 
                start,
                stop, 
                time_step = 'days', 
                obs_file = 'A_EcoForecast/Data/lake_c_data.rds',
                driver_file = 'A_EcoForecast/Data/lake_c_data.rds',
                n_states_est = 1, 
                n_params_est = 1,
                n_params_obs = 0, 
                decay_init = 0.005, 
                obs_cv = 0.1,
                param_cv = 0.25,
                driver_cv = c(0.2, 0.2, 0.2),
                init_cond_cv = 0.1){
  
  
  n_en = n_en
  start = as.Date(start)
  stop = as.Date(stop)
  time_step = 'days' 
  dates = get_model_dates(model_start = start, model_stop = stop, time_step = time_step)
  n_step = length(dates)
  
  # get observation matrix
  obs_df = readRDS(obs_file) %>% 
    select(datetime, doc_lake) 
  
  drivers_df = readRDS(driver_file) %>% 
    select(datetime, doc_load, water_out, lake_vol) 
  
  n_states_est = n_states_est # number of states we're estimating 
  
  n_params_est = n_params_est # number of parameters we're calibrating
  
  n_params_obs = n_params_obs # number of parameters for which we have observations
  
  decay_init = decay_init # Initial estimate of DOC decay rate day^-1 
  
  doc_init = obs_df$doc_lake[min(which(!is.na(obs_df$doc_lake)))]
  
  state_cv = obs_cv #coefficient of variation of DOC observations 
  state_sd = state_cv * doc_init
  init_cond_sd = init_cond_cv * doc_init
  
  param_cv = param_cv #coefficient of variation of DOC decay 
  param_sd = param_cv * decay_init
  
  # driver data coefficient of variation for DOC Load, Discharge out, and Lake volume, respectively 
  driver_cv = driver_cv 
  
  
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
  
  # initialize Y vector
  Y = initialize_Y(Y = Y, obs = obs, init_params = decay_init, n_states_est = n_states_est,
                   n_params_est = n_params_est, n_params_obs = n_params_obs,
                   n_step = n_step, n_en = n_en, state_sd = init_cond_sd, param_sd = param_sd)
  
  # get driver data with uncertainty - dim = c(n_step, driver, n_en) 
  drivers = get_drivers(drivers_df = drivers_df, 
                        model_dates = dates,
                        n_drivers = 3, 
                        driver_colnames = c('doc_load', 'water_out', 'lake_vol'), 
                        driver_cv = driver_cv, 
                        n_step = n_step, 
                        n_en = n_en) 
  
  # start modeling
  for(t in 2:n_step){
    for(n in 1:n_en){
      
      # run model; 
      model_output = predict_lake_doc(doc_load = drivers[t-1, 1, n], 
                                      doc_lake = Y[1, t-1, n], 
                                      lake_vol = drivers[t-1, 3, n], 
                                      water_out = drivers[t-1, 2, n],
                                      decay = Y[2, t-1, n])
      
      Y[1 , t, n] = model_output$doc_predict # store in Y vector
      Y[2 , t, n] = model_output$decay
    }
    # check if there are any observations to assimilate 
    if(any(!is.na(obs[ , , t]))){
      Y = kalman_filter(Y = Y,
                        R = R,
                        obs = obs,
                        H = H,
                        n_en = n_en,
                        cur_step = t) # updating params / states if obs available
    }
  }
  out = list(Y = Y, dates = dates, drivers_df = drivers_df, R = R, obs = obs, state_sd = state_sd)
  
  return(out)
}


