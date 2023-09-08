library(R6)
set.seed(42)

norep_osESD_Detector <- function(data, time=1:length(data), dwins, rwins, train_size, alpha, maxr) {
  train_data <- data[1:train_size]
  online_data <- data[(train_size+1):length(data)]
  train_time <- time[1:train_size]
  online_time <- time[(train_size+1):length(data)]
  r_ins <- norep_osESD_TRES$new(data=train_data, time=train_time, wins=rwins)
  c_ins <- norep_osESD_TCHA$new(data=train_data, time=train_time, wins=dwins)
  SESD_TRES <- norep_osESD_vector$new(data=r_ins$tres, alpha=alpha, maxr=maxr)
  SESD_TCHA <- norep_osESD_vector$new(data=c_ins$tcha, alpha=alpha, maxr=maxr)
  anomal_index <- c()
  for(i in 1:length(online_data)) {
    ranom <- SESD_TRES$test(r_ins$update(online_data[i], online_time[i]))
    canom <- SESD_TCHA$test(c_ins$update(online_data[i], online_time[i]))
    if(canom || ranom){
      anomal_index <- c(anomal_index, (i+train_size))
      # data_ <- r_ins$data[-rwins]
      # time_ <- r_ins$time[-rwins]
      # x_bar <- ((rwins * r_ins$x_bar) - r_ins$time[rwins])/(rwins-1)
      # y_bar <- ((rwins * r_ins$y_bar) - r_ins$data[rwins])/(rwins-1)
      # beta_ <- sum((time_-x_bar)*(data_-y_bar))/sum((time_-x_bar)^2)
      # alpha_ <- y_bar - beta_ * x_bar
      # rep <- alpha_ + beta_ * time_[rwins-1]
      # c_ins$replace(rep)
      # r_ins$replace(rep)
    }
  }
  return(anomal_index)
}
