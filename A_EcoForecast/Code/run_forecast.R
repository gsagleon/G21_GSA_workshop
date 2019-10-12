
library(dplyr) 

source('A_EcoForecast/Code/EnKF_functions.R')
source('A_EcoForecast/Code/lake_doc_model.R')

n_en = 100 # how many ensembles 

est_out = EnKF(n_en = n_en, 
           start = '2014-06-04', 
           stop = '2014-09-20')

# plotting 
mean_doc_est = apply(est_out$Y[1,,], 1, FUN = mean)
plot(mean_doc_est / est_out$drivers_df$lake_vol * 12 ~ est_out$dates, type ='l', 
     ylim = range(est_out$Y[1,,] / est_out$drivers_df$lake_vol * 12),
     col = 'grey', ylab = 'DOC (mg L-1)', xlab = '')
for(i in 2:n_en){
  lines(est_out$Y[1,,i] / est_out$drivers_df$lake_vol * 12 ~ est_out$dates, 
        col = 'grey')
}
lines(mean_doc_est / est_out$drivers_df$lake_vol * 12 ~ est_out$dates, col = 'black', lwd =2 )
points(est_out$obs[1,,] / est_out$drivers_df$lake_vol * 12 ~ est_out$dates, pch = 16, col = 'red')
arrows(est_out$dates, est_out$obs[1,,] / est_out$drivers_df$lake_vol * 12 - est_out$state_sd / est_out$drivers_df$lake_vol * 12, 
       est_out$dates, est_out$obs[1,,] / est_out$drivers_df$lake_vol * 12 + est_out$state_sd / est_out$drivers_df$lake_vol * 12, 
       code = 3, length = 0.1, angle = 90, col = 'red')

mean_decay_est = apply(est_out$Y[2,,], 1, FUN = mean)
plot(mean_decay_est ~ est_out$dates, type ='l', 
     ylim = range(est_out$Y[2,,]),
     col = 'grey', ylab = 'DOC Decay (day^-1)', xlab ='')
for(i in 2:n_en){
  lines(est_out$Y[2,,i] ~ est_out$dates, col = 'grey')
}
lines(mean_decay_est ~ est_out$dates, col = 'black', lwd = 2)




