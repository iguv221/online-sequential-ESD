library(R6)
library(stats)
set.seed(42)
osESD_vector <- R6Class("osESD_vector", 
                        public = list(
                          mean = 0,
                          sqsum = 0,
                          alpha = 0,
                          maxr = 0,
                          data = NULL,
                          size = 0,
                          
                          initialize = function(data = NA, alpha = 0.01, maxr = 10) {
                            self$data <- data
                            self$size <- length(data)
                            self$mean <- mean(data)
                            self$sqsum <- sum(data^2)
                            self$alpha <- alpha
                            self$maxr <- maxr
                          },
                          
                          test = function(on) {
                            out <- self$data[1]
                            self$data <- c(self$data[-1], on)
                            self$mean <- self$mean - (out - on) / self$size
                            self$sqsum <- self$sqsum - out^2 + on^2
                            mean_ <- self$mean
                            sqsum_ <- self$sqsum
                            size_ <- self$size
                            data_ <- self$data
                            sd_ <- sqrt((sqsum_ - size_ * mean_^2 + 1e-8)/(size_-1))
                            ares <- abs((data_-mean_)/sd_)
                            esd_index <- which.max(ares)
                            esd <- ares[esd_index]
                            tryCatch({
                              if(esd > self$getLambda(self$alpha, size_)) {
                                if(esd_index == size_) {
                                  return(TRUE)
                                }
                              } else {
                                return(FALSE)
                              }
                            }, error = function(e) {
                              return (FALSE)
                            })
                            for(i in 2:self$maxr) {
                              size_ <- size_ - 1
                              mean_ <- ((size_+1)*mean_ - data_[esd_index])/size_
                              sqsum_ <- sqsum_ - data_[esd_index]^2
                              sd_ <- sqrt((sqsum_ - size_ * mean_^2 + 1e-8)/(size_-1))
                              data_ <- data_[-esd_index]
                              ares <- abs((data_-mean_)/sd_)
                              esd_index <- which.max(ares)
                              esd <- ares[esd_index]
                              tryCatch({
                                if(esd > self$getLambda(self$alpha, size_)) {
                                  if(esd_index == size_) {
                                    return(TRUE)
                                  }
                                } else {
                                  return(FALSE)
                                }
                              }, error = function(e) {
                                return (FALSE)
                              })
                            }
                            return(FALSE)
                          },
                          
                          getLambda = function(alpha, size) {
                            if (size<=2){
                              return (0)
                            }
                            p = 1 - alpha/(2*(size))
                            t_ = qt(p,(size-2))
                            return(t_ * (size-1) / sqrt((size + t_^2) * size))
                          }
                        )
)

sosESD_vector <- R6Class("sosESD_vector", 
                         public = list(
                           mean = 0,
                           sqsum = 0,
                           alpha = 0,
                           maxr = 0,
                           data = NULL,
                           size = 0,
                           
                           initialize = function(data = NA, alpha = 0.01, maxr = 10) {
                             self$data <- data
                             self$size <- length(data)
                             self$mean <- mean(data)
                             self$sqsum <- sum(data^2)
                             self$alpha <- alpha
                             self$maxr <- maxr
                           },
                           
                           test = function(on) {
                             out <- self$data[1]
                             self$data <- c(self$data[-1], on)
                             self$mean <- self$mean - (out - on) / self$size
                             self$sqsum <- self$sqsum - out^2 + on^2
                             mean_ <- self$mean
                             sqsum_ <- self$sqsum
                             size_ <- self$size
                             data_ <- self$data
                             sd_ <- sqrt((sqsum_ - size_ * mean_^2 + 1e-8)/(size_-1))
                             ares <- abs((data_-mean_)/sd_)
                             esd_index <- which.max(ares)
                             esd <- ares[esd_index]
                             tryCatch({
                               if(esd > self$getLambda(self$alpha, size_)) {
                                 if(esd_index == size_) {
                                   return(TRUE)
                                 }
                               } else {
                                 return(FALSE)
                               }
                             }, error = function(e) {
                               return (FALSE)
                             })
                             for(i in 2:self$maxr) {
                               size_ <- size_ - 1
                               mean_ <- ((size_+1)*mean_ - data_[esd_index])/size_
                               sqsum_ <- sqsum_ - data_[esd_index]^2
                               sd_ <- sqrt((sqsum_ - size_ * mean_^2 + 1e-8)/(size_-1))
                               data_ <- data_[-esd_index]
                               ares <- abs((data_-mean_)/sd_)
                               esd_index <- which.max(ares)
                               esd <- ares[esd_index]
                               tryCatch({
                                 if(esd > self$getLambda(self$alpha, size_)) {
                                   if(esd_index == size_) {
                                     return(TRUE)
                                   }
                                 } else {
                                   return(FALSE)
                                 }
                               }, error = function(e) {
                                 return (FALSE)
                               })
                             }
                             return(FALSE)
                           },
                           
                           getLambda = function(alpha, size) {
                             p = 1 - alpha/(2*(size))
                             t_ = qt(p,(size-2))
                             return(t_ * (size-1) / sqrt((size + t_^2) * size))
                           }
                         )
)


oGESD_vector <- R6Class("oGESD_vector", 
                        public = list(
                          mean = 0,
                          sqsum = 0,
                          alpha = 0,
                          maxr = 0,
                          data = NULL,
                          size = 0,
                          
                          initialize = function(data = NA, alpha = 0.01, maxr = 10) {
                            self$data <- data
                            self$size <- length(data)
                            self$mean <- mean(data)
                            self$sqsum <- sum(data^2)
                            self$alpha <- alpha
                            self$maxr <- maxr
                          },
                          
                          test = function(on) {
                            self$data <- c(self$data[-1], on)
                            GESD_ <- GESD(data=self$data, alpha=self$alpha,max_outliers=self$maxr)
                            Anoms <- GESD_$ESDtest()
                            if (length(Anoms)!=0){
                              if (Anoms[length(Anoms)]==length(self$data)){
                                return (TRUE)
                              }
                              return (FALSE)
                            }
                            return (FALSE)
                          }
                        )
)


GESD <- function(data=NULL, alpha=0.01, max_outliers=NULL) {
  
  if (!is.null(data)) {
    self <- list(data=data, alpha=alpha)
  } else {
    stop("Data must be provided.")
  }
  
  if (is.null(max_outliers)) {
    self$max_outliers <- as.integer(length(self$data) * 0.08)
  } else {
    self$max_outliers <- max_outliers
  }
  
  self$ANOMALIES <- NULL
  
  self$ESDtest <- function() {
    for (iterations in seq_len(self$max_outliers)) {
      stat_max_index <- self$Ri_stat(self$data)
      stat <- stat_max_index$stat
      
      max_index <- stat_max_index$max_index
      critical <- self$Critical_value(length(self$data))
      if (stat > critical) {
        self$ANOMALIES <- c(self$ANOMALIES, max_index)
      } else {
        break
      }
      self$data <- self$data[-max_index]
    }
    return(self$ANOMALIES)
  }
  
  self$Ri_stat <- function(y) {
    std_dev <- sd(y)
    avg_y <- mean(y)
    abs_val_minus_avg <- abs(y - avg_y)
    max_of_deviations <- max(abs_val_minus_avg)
    max_ind <- which(abs_val_minus_avg == max_of_deviations)[1]
    cal <- max_of_deviations / std_dev
    return(list(stat=cal, max_index=max_ind))
  }
  
  self$Critical_value <- function(S) {
    t_dist <- qt(1 - self$alpha / (2 * S), S - 2)
    num <- (S - 1) * sqrt(t_dist^2)
    den <- sqrt(S) * sqrt(S - 2 + t_dist^2)
    critical_value <- num / den
    return(critical_value)
  }
  
  return(self)
}


norep_osESD_vector <- R6Class("norep_osESD_vector", 
                              public = list(
                                mean = 0,
                                sqsum = 0,
                                alpha = 0,
                                maxr = 0,
                                data = NULL,
                                size = 0,
                                
                                initialize = function(data = NA, alpha = 0.01, maxr = 10) {
                                  self$data <- data
                                  self$size <- length(data)
                                  self$mean <- mean(data)
                                  self$sqsum <- sum(data^2)
                                  self$alpha <- alpha
                                  self$maxr <- maxr
                                },
                                
                                test = function(on) {
                                  out <- self$data[1]
                                  self$data <- c(self$data[-1], on)
                                  self$mean <- self$mean - (out - on) / self$size
                                  self$sqsum <- self$sqsum - out^2 + on^2
                                  mean_ <- self$mean
                                  sqsum_ <- self$sqsum
                                  size_ <- self$size
                                  data_ <- self$data
                                  sd_ <- sqrt((sqsum_ - size_ * mean_^2 + 1e-8)/(size_-1))
                                  ares <- abs((data_-mean_)/sd_)
                                  esd_index <- which.max(ares)
                                  esd <- ares[esd_index]
                                  tryCatch({
                                    if(esd > self$getLambda(self$alpha, size_)) {
                                      if(esd_index == size_) {
                                        return(TRUE)
                                      }
                                    } else {
                                      return(FALSE)
                                    }
                                  }, error = function(e) {
                                    return (FALSE)
                                  })
                                  for(i in 2:self$maxr) {
                                    size_ <- size_ - 1
                                    mean_ <- ((size_+1)*mean_ - data_[esd_index])/size_
                                    sqsum_ <- sqsum_ - data_[esd_index]^2
                                    sd_ <- sqrt((sqsum_ - size_ * mean_^2 + 1e-8)/(size_-1))
                                    data_ <- data_[-esd_index]
                                    ares <- abs((data_-mean_)/sd_)
                                    esd_index <- which.max(ares)
                                    esd <- ares[esd_index]
                                    tryCatch({
                                      if(esd > self$getLambda(self$alpha, size_)) {
                                        if(esd_index == size_) {
                                          return(TRUE)
                                        }
                                      } else {
                                        return(FALSE)
                                      }
                                    }, error = function(e) {
                                      return (FALSE)
                                    })
                                  }
                                  return(FALSE)
                                },
                                
                                getLambda = function(alpha, size) {
                                  p = 1 - alpha/(2*(size))
                                  t_ = qt(p,(size-2))
                                  return(t_ * (size-1) / sqrt((size + t_^2) * size))
                                }
                              )
)
