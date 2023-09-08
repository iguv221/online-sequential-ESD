library(R6)
set.seed(42)

#transform data to residual with update
osESD_TRES <- R6Class("osESD_TRES", 
                      public = list(
                        data = NULL,
                        time = NULL,
                        tres = NULL,
                        x_bar = 0,
                        y_bar = 0,
                        wins = 0,
                        
                        initialize = function(data, time=1:length(data), wins) {
                          self$wins <- wins
                          self$data <- data[1:wins]
                          self$time <- time[1:wins]
                          self$x_bar <- mean(self$time)
                          self$y_bar <- mean(self$data)
                          beta <- sum((self$time-self$x_bar)*(self$data-self$y_bar))/sum((self$time-self$x_bar)^2)
                          alpha <- self$y_bar - beta * self$x_bar
                          self$tres <- self$data[wins] - (alpha + beta * self$time[wins])
                          
                          for(i in (wins+1):length(data)) {
                            self$data <- c(self$data[-1], data[i])
                            self$time <- c(self$time[-1], time[i])
                            self$x_bar <- self$x_bar - (time[i-wins] - time[i])/wins
                            self$y_bar <- self$y_bar - (data[i-wins] - data[i])/wins
                            beta <- sum((self$time-self$x_bar)*(self$data-self$y_bar))/sum((self$time-self$x_bar)^2)
                            alpha <- self$y_bar - beta * self$x_bar
                            self$tres[i-wins+1] <- data[i] - (alpha + beta * time[i])
                          }
                        },
                        
                        update = function(ond, ont=(self$time[self$wins]+1)) {
                          first_data <- self$data[1]  
                          first_time <- self$time[1]
                          self$data <- c(self$data[-1], ond)
                          self$time <- c(self$time[-1], ont)
                          self$x_bar = self$x_bar - (first_time - ont)/self$wins
                          self$y_bar = self$y_bar - (first_data - ond)/self$wins
                          beta <- sum((self$time-self$x_bar)*(self$data-self$y_bar))/sum((self$time-self$x_bar)^2)
                          alpha <- self$y_bar - beta * self$x_bar
                          tres_ <- ond - (alpha + beta * ont)
                          self$tres <- c(self$tres[-1], tres_)
                          return(tres_)
                        },
                        
                        replace = function(rep) {
                          prev <- self$data[self$wins]
                          self$data[self$wins] <- rep
                          self$y_bar = self$y_bar - (prev - rep)/self$wins
                        }
                      )
)

#transform data to change rate with update
osESD_TCHA <- R6Class("osESD_TCHA", 
                      public = list(
                        data = NULL,
                        time = NULL,
                        tcha = NULL,
                        wins = 0,
                        initialize = function(data, time=1:length(data), wins) {
                          size <- length(data)
                          self$wins <- wins
                          self$tcha <- (data[(wins):size]-data[1:(size-wins+1)])/(time[wins:size]-time[1:(size-wins+1)])
                          self$data <- data[(length(data)-wins+1):length(data)]
                          self$time <- time[(length(time)-wins+1):length(time)]
                        },
                        
                        update = function(ond, ont=self$time[self$wins]+1) {
                          self$data <- c(self$data[-1], ond)
                          self$time <- c(self$time[-1], ont)
                          tcha_ <- (self$data[self$wins]-self$data[1])/(self$time[self$wins]-self$time[1])
                          self$tcha <- c(self$tcha[-1], tcha_)
                          return(tcha_)
                        },
                        
                        replace = function(rep) {
                          self$data[self$wins] <- rep
                        }
                      )
)

#transform data to residual with update
oGESD_TRES <- R6Class("oGESD_TRES", 
                      public = list(
                        data = NULL,
                        time = NULL,
                        tres = NULL,
                        x_bar = 0,
                        y_bar = 0,
                        wins = 0,
                        
                        initialize = function(data, time=1:length(data), wins) {
                          self$wins <- wins
                          self$data <- data[1:wins]
                          self$time <- time[1:wins]
                          self$x_bar <- mean(self$time)
                          self$y_bar <- mean(self$data)
                          beta <- sum((self$time-self$x_bar)*(self$data-self$y_bar))/sum((self$time-self$x_bar)^2)
                          alpha <- self$y_bar - beta * self$x_bar
                          self$tres <- self$data[wins] - (alpha + beta * self$time[wins])
                          for(i in (wins+1):length(data)) {
                            self$data <- c(self$data[-1], data[i])
                            self$time <- c(self$time[-1], time[i])
                            self$x_bar <- self$x_bar - (time[i-wins] - time[i])/wins
                            self$y_bar <- self$y_bar - (data[i-wins] - data[i])/wins
                            beta <- sum((self$time-self$x_bar)*(self$data-self$y_bar))/sum((self$time-self$x_bar)^2)
                            alpha <- self$y_bar - beta * self$x_bar
                            self$tres[i-wins+1] <- data[i] - (alpha + beta * time[i])
                          }
                        },
                        
                        update = function(ond, ont=(self$time[self$wins]+1)) {
                          first_data <- self$data[1]
                          first_time <- self$time[1]
                          self$data <- c(self$data[-1], ond)
                          self$time <- c(self$time[-1], ont)
                          self$x_bar = self$x_bar - (first_time - ont)/self$wins
                          self$y_bar = self$y_bar - (first_data - ond)/self$wins
                          beta <- sum((self$time-self$x_bar)*(self$data-self$y_bar))/sum((self$time-self$x_bar)^2)
                          alpha <- self$y_bar - beta * self$x_bar
                          tres_ <- ond - (alpha + beta * ont)
                          self$tres <- c(self$tres[-1], tres_)
                          return(tres_)
                        },
                        
                        replace = function(rep) {
                          prev <- self$data[self$wins]
                          self$data[self$wins] <- rep
                          self$y_bar = self$y_bar - (prev - rep)/self$wins
                        }
                      )
)

#transform data to change rate with update
oGESD_TCHA <- R6Class("oGESD_TCHA", 
                      public = list(
                        data = NULL,
                        time = NULL,
                        tcha = NULL,
                        wins = 0,
                        initialize = function(data, time=1:length(data), wins) {
                          size <- length(data)
                          self$wins <- wins
                          self$tcha <- (data[(wins):size]-data[1:(size-wins+1)])/(time[wins:size]-time[1:(size-wins+1)])
                          self$data <- data[(length(data)-wins+1):length(data)]
                          self$time <- time[(length(time)-wins+1):length(time)]
                        },
                        
                        update = function(ond, ont=self$time[self$wins]+1) {
                          self$data <- c(self$data[-1], ond)
                          self$time <- c(self$time[-1], ont)
                          tcha_ <- (self$data[self$wins]-self$data[1])/(self$time[self$wins]-self$time[1])
                          self$tcha <- c(self$tcha[-1], tcha_)
                          return(tcha_)
                        },
                        
                        replace = function(rep) {
                          self$data[self$wins] <- rep
                        }
                      )
)

#transform data to residual with update
norep_osESD_TRES <- R6Class("norep_osESD_TRES", 
                            public = list(
                              data = NULL,
                              time = NULL,
                              tres = NULL,
                              x_bar = 0,
                              y_bar = 0,
                              wins = 0,
                              
                              initialize = function(data, time=1:length(data), wins) {
                                self$wins <- wins
                                self$data <- data[1:wins]
                                self$time <- time[1:wins]
                                self$x_bar <- mean(self$time)
                                self$y_bar <- mean(self$data)
                                beta <- sum((self$time-self$x_bar)*(self$data-self$y_bar))/sum((self$time-self$x_bar)^2)
                                alpha <- self$y_bar - beta * self$x_bar
                                self$tres <- self$data[wins] - (alpha + beta * self$time[wins])
                                for(i in (wins+1):length(data)) {
                                  self$data <- c(self$data[-1], data[i])
                                  self$time <- c(self$time[-1], time[i])
                                  self$x_bar <- self$x_bar - (time[i-wins] - time[i])/wins
                                  self$y_bar <- self$y_bar - (data[i-wins] - data[i])/wins
                                  beta <- sum((self$time-self$x_bar)*(self$data-self$y_bar))/sum((self$time-self$x_bar)^2)
                                  alpha <- self$y_bar - beta * self$x_bar
                                  self$tres[i-wins+1] <- data[i] - (alpha + beta * time[i])
                                }
                              },
                              
                              update = function(ond, ont=(self$time[self$wins]+1)) {
                                
                                first_data <- self$data[1]
                                first_time <- self$time[1]
                                self$data <- c(self$data[-1], ond)
                                self$time <- c(self$time[-1], ont)
                                self$x_bar = self$x_bar - (first_time - ont)/self$wins
                                self$y_bar = self$y_bar - (first_data - ond)/self$wins
                                beta <- sum((self$time-self$x_bar)*(self$data-self$y_bar))/sum((self$time-self$x_bar)^2)
                                alpha <- self$y_bar - beta * self$x_bar
                                tres_ <- ond - (alpha + beta * ont)
                                self$tres <- c(self$tres[-1], tres_)
                                return(tres_)
                              },
                              
                              replace = function(rep) {
                                prev <- self$data[self$wins]
                                self$data[self$wins] <- rep
                                self$y_bar = self$y_bar - (prev - rep)/self$wins
                              }
                            )
)

#transform data to change rate with update
norep_osESD_TCHA <- R6Class("norep_osESD_TCHA", 
                            public = list(
                              data = NULL,
                              time = NULL,
                              tcha = NULL,
                              wins = 0,
                              initialize = function(data, time=1:length(data), wins) {
                                size <- length(data)
                                self$wins <- wins
                                self$tcha <- (data[(wins):size]-data[1:(size-wins+1)])/(time[wins:size]-time[1:(size-wins+1)])
                                self$data <- data[(length(data)-wins+1):length(data)]
                                self$time <- time[(length(time)-wins+1):length(time)]
                              },
                              
                              update = function(ond, ont=self$time[self$wins]+1) {
                                self$data <- c(self$data[-1], ond)
                                self$time <- c(self$time[-1], ont)
                                tcha_ <- (self$data[self$wins]-self$data[1])/(self$time[self$wins]-self$time[1])
                                self$tcha <- c(self$tcha[-1], tcha_)
                                return(tcha_)
                              },
                              
                              replace = function(rep) {
                                self$data[self$wins] <- rep
                              }
                            )
)
