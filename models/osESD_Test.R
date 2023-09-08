library(R6)
set.seed(42)
#Sequential ESD test
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
                              # print(size_)
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



# SESD <- R6Class("SESD", 
#                 public = list(
#                   mean = 0,
#                   sqsum = 0,
#                   alpha = 0,
#                   maxr = 0,
#                   data = NULL,
#                   size = 0,
#                   
#                   initialize = function(data = NA, alpha = 0.01, maxr = 10) {
#                     self$data <- data
#                     self$size <- length(data)
#                     self$mean <- mean(data)
#                     self$sqsum <- sum(data^2)
#                     self$alpha <- alpha
#                     self$maxr <- maxr
#                   },
#                   
#                   test = function(on) {
#                     out <- self$data[1]
#                     self$data <- c(self$data[-1], on)
#                     self$mean <- self$mean - (out - on) / self$size
#                     self$sqsum <- self$sqsum - out^2 + on^2
#                     mean_ <- self$mean
#                     sqsum_ <- self$sqsum
#                     size_ <- self$size
#                     data_ <- self$data
#                     
#                     
#                     
#                     sd_ <- sqrt((sqsum_ - size_ * mean_^2 + 1e-8)/(size_-1))
#                     ares <- abs((data_-mean_)/sd_)
#                     esd_index <- which.max(ares)
#                     esd <- ares[esd_index]
#                     # print(esd)
#                     # print(self$getLambda(self$alpha, size_))
#                     tryCatch({
#                       if(esd > self$getLambda(self$alpha, size_)) {
#                         if(esd_index == size_) {
#                           return(TRUE)
#                         }
#                       } else {
#                         return(FALSE)
#                       }
#                     }, error = function(e) {
#                       return (FALSE)
#                     })
#                     
#                     for(i in 2:self$maxr) {
#                       size_ <- size_ - 1
#                       # print(size_)
#                       mean_ <- ((size_+1)*mean_ - data_[esd_index])/size_
#                       sqsum_ <- sqsum_ - data_[esd_index]^2
#                       
#                       
#                       
#                       sd_ <- sqrt((sqsum_ - size_ * mean_^2 + 1e-8)/(size_-1))
#                       data_ <- data_[-esd_index]
#                       ares <- abs((data_-mean_)/sd_)
#                       esd_index <- which.max(ares)
#                       esd <- ares[esd_index]
#                       
#                       
#                       # if(esd > self$getLambda(self$alpha, size_)) {
#                       #   if(esd_index == size_) {
#                       #     return(TRUE)
#                       #   }
#                       # } else {
#                       #   return(FALSE)
#                       # }
#                       
#                       
#                       tryCatch({
#                         if(esd > self$getLambda(self$alpha, size_)) {
#                           if(esd_index == size_) {
#                             return(TRUE)
#                           }
#                         } else {
#                           return(FALSE)
#                         }
#                       }, error = function(e) {
#                         return (FALSE)
#                       })
#                     }
#                     
#                     
#                     return(FALSE)
#                   },
#                   
#                   getLambda = function(alpha, size) {
#                     if (size<=2){
#                       return (0)
#                     }
#                     p = 1 - alpha/(2*(size))
#                     t_ = qt(p,(size-2))
#                     return(t_ * (size-1) / sqrt((size + t_^2) * size))
#                   }
#                   )
#                 )
