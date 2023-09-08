library(R6)

sosESD_Detector <- function(data, wins, alpha, maxr) {
  current_data = data[1:wins]
  SESD_TRES = sosESD_vector$new(data=current_data,alpha=alpha,maxr=maxr)
  anomal_index <- c()
  for(i in (wins+1):length(data)) {
    anom <- SESD_TRES$test(data[i])
    if (anom){
      anomal_index <- c(anomal_index, (i))
    }
  }
  # print(length(data))
  return(anomal_index)
}

