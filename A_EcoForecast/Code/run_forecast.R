
library(dplyr) 

source('A_EcoForecast/Code/EnKF_functions.R')
source('A_EcoForecast/Code/lake_doc_model.R')

n_en = 100 # how many ensembles 

est_out = EnKF(n_en = n_en, 
           start = '2014-06-04', # 2014-06-04 is earliest date 
           stop = '2014-09-20', # 2014-09-20 is latest date
           obs_file = 'A_EcoForecast/Data/lake_c_data.rds',
           driver_file = 'A_EcoForecast/Data/lake_c_data.rds',
           decay_init = 0.005, 
           obs_cv = 0.1,
           param_cv = 0.2,
           driver_cv = c(0.2, 0.2, 0.0), # CV of driver data for DOC Load, Discharge out, and Lake volume, respectively 
           init_cond_cv = .1)

# plotting 
mean_doc_est = apply(est_out$Y[1,,] / est_out$drivers[,3,] * 12, 1, FUN = mean)
plot(mean_doc_est ~ est_out$dates, type ='l', 
     ylim = range(c(est_out$Y[1,,] / est_out$drivers[,3,] * 12, 
                    est_out$obs[1,,] / apply(est_out$drivers[,3,], 1, FUN = mean) * 12), na.rm = T),
     col = 'grey', ylab = 'DOC (mg L-1)', xlab = '')
for(i in 2:n_en){
  lines(est_out$Y[1,,i] / est_out$drivers[,3,i] * 12 ~ est_out$dates, 
        col = 'grey')
}
lines(mean_doc_est ~ est_out$dates, col = 'black', lwd =2 )
points(est_out$obs[1,,] / apply(est_out$drivers[,3,], 1, FUN = mean) * 12 ~ est_out$dates, pch = 16, col = 'red')
arrows(est_out$dates, est_out$obs[1,,] / apply(est_out$drivers[,3,], 1, FUN = mean) * 12 - 
         est_out$state_sd / apply(est_out$drivers[,3,], 1, FUN = mean) * 12, 
       est_out$dates, est_out$obs[1,,] / apply(est_out$drivers[,3,], 1, FUN = mean) * 12 +
         est_out$state_sd / apply(est_out$drivers[,3,], 1, FUN = mean) * 12, 
       code = 3, length = 0.1, angle = 90, col = 'red')

mean_decay_est = apply(est_out$Y[2,,], 1, FUN = mean)
plot(mean_decay_est ~ est_out$dates, type ='l', 
     ylim = range(est_out$Y[2,,]),
     col = 'grey', ylab = 'DOC Decay (day^-1)', xlab ='')
for(i in 2:n_en){
  lines(est_out$Y[2,,i] ~ est_out$dates, col = 'grey')
}
lines(mean_decay_est ~ est_out$dates, col = 'black', lwd = 2)




