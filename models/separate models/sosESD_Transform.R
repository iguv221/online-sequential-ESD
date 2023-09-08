# library(R6)
# 
# #transform data to residual with update
# Window <- R6Class("Window", 
#                 public = list(
#                   data = NULL,
#                   time = NULL,
#                   original_data = NULL,
#                   original_time = NULL,
#                   x_bar = 0,
#                   y_bar = 0,
#                   wins = 0,
#                   
#                   initialize = function(data, time=1:length(data), wins) {
#                     self$wins <- wins
#                     self$data <- data[1:wins]
#                     self$time <- time[1:wins]
#                     self$original_data <- data
#                     self$original_time <- time
#                     self$x_bar <- mean(self$time)
#                     self$y_bar <- mean(self$data)
#                     beta <- sum((self$time-self$x_bar)*(self$data-self$y_bar))/sum((self$time-self$x_bar)^2)
#                     alpha <- self$y_bar - beta * self$x_bar
#                     print(self$x_bar)
#                     # self$tres <- self$data[wins] - (alpha + beta * self$time[wins])
#                     for(i in (wins+1):length(self$original_data)) {
#                       print(self$original_data)
#                       adadasdasd
#                       self$data <- c(self$data[-1], self$original_data[i])
#                       self$time <- c(self$time[-1], self$original_time[i])
#                       
#                       print((self$original_time[i-wins] - self$original_time[i])/wins)
#                       self$x_bar <- self$x_bar - (self$original_time[i-wins] - self$original_time[i])/wins
#                       self$y_bar <- self$y_bar - (self$original_data[i-wins] - self$original_data[i])/wins
#                       beta <- sum((self$time-self$x_bar)*(self$data-self$y_bar))/sum((self$time-self$x_bar)^2)
#                       alpha <- self$y_bar - beta * self$x_bar
#                       # self$tres[i-wins+1] <- data[i] - (alpha + beta * time[i])
#                     }
#                     print(self$x_bar)
#                     adsasdasdsad
#                   },
#                   
#                   update = function(ond, ont=(self$time[self$wins]+1)) {
#                     
#                     first_data <- self$data[1]
#                     first_time <- self$time[1]
#                     self$data <- c(self$data[-1], ond)
#                     self$time <- c(self$time[-1], ont)
#                     self$x_bar = self$x_bar - (first_time - ont)/self$wins
#                     self$y_bar = self$y_bar - (first_data - ond)/self$wins
#                     beta <- sum((self$time-self$x_bar)*(self$data-self$y_bar))/sum((self$time-self$x_bar)^2)
#                     alpha <- self$y_bar - beta * self$x_bar
#                     tres_ <- ond - (alpha + beta * ont)
#                     # self$tres <- c(self$tres[-1], tres_)
#                     # return(tres_)
#                   },
# 
#                   replace = function(rep) {
#                     prev <- self$data[self$wins]
#                     self$data[self$wins] <- rep
#                     self$y_bar = self$y_bar - (prev - rep)/self$wins
#                   }
#                 )
# )
# 
# # #transform data to change rate with update
# # TCHA <- R6Class("TCHA", 
# #                 public = list(
# #                   data = NULL,
# #                   time = NULL,
# #                   tcha = NULL,
# #                   wins = 0,
# #                   initialize = function(data, time=1:length(data), wins) {
# #                     size <- length(data)
# #                     self$wins <- wins
# #                     self$tcha <- (data[(wins):size]-data[1:(size-wins+1)])/(time[wins:size]-time[1:(size-wins+1)])
# #                     self$data <- data[(length(data)-wins+1):length(data)]
# #                     self$time <- time[(length(time)-wins+1):length(time)]
# #                   },
# #                   
# #                   update = function(ond, ont=self$time[self$wins]+1) {
# #                     self$data <- c(self$data[-1], ond)
# #                     self$time <- c(self$time[-1], ont)
# #                     tcha_ <- (self$data[self$wins]-self$data[1])/(self$time[self$wins]-self$time[1])
# #                     self$tcha <- c(self$tcha[-1], tcha_)
# #                     return(tcha_)
# #                   },
# #                   
# #                   replace = function(rep) {
# #                     self$data[self$wins] <- rep
# #                   }
# #                 )
# # )
