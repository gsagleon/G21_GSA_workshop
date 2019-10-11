
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
    obs_matrix[i, 1, j] = dplyr::filter(obs_df_filtered,
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
  K = ((1 / (n_en - 1)) * delta_Y %*% t(delta_Y) %*% t(H[, , cur_step])) %*%
    qr.solve(((1 / (n_en - 1)) * H[, , cur_step] %*% delta_Y %*% t(delta_Y) %*% t(H[, , cur_step]) + R[, , cur_step]))
  
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
initialize_Y = function(Y, obs, n_states_est, n_params_est, n_params_obs, n_step, n_en, state_sd, param_sd){
  
  # initializing states with earliest observations and parameters
  first_obs = coalesce(!!!lapply(seq_len(dim(obs)[3]), function(i){obs[,,i]})) %>% # turning array into list, then using coalesce to find first obs in each position.
    ifelse(is.na(.), mean(., na.rm = T), .) # setting initial temp state to mean of earliest temp obs from other sites if no obs
  
  if(n_params_est > 0){
    ## update this later ***********************
    first_params = rep(.5, n_params_est)
  }else{
    first_params = NULL
  }
  
  Y[ , 1, ] = array(rnorm(n = n_en * (n_states_est + n_params_est),
                          mean = c(first_obs, first_params),
                          sd = c(state_sd, param_sd)),
                    dim = c(c(n_states_est + n_params_est), n_en))
  
  return(Y)
}



