
#### This code is for checking ARIMA and state-space datasets.
#### Plots will be made in the designated directory.


############################################################
############################################################
############################################################
############################################################
############################################################


rm(list=ls())
set.seed(42)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()
Starting_point <- getwd()
source('utils//data_functions.R')

dir_path <- "plots"
if (!file.exists(dir_path)) {
  dir.create(dir_path)
  cat("Directory created:", dir_path, "\n")
} else {
  cat("Directory already exists:", dir_path, "\n")
}
setwd("plots")

############################################################
############################################################
############################################################
############################################################
############################################################

### For checking the distribution of all six ARIMA scenarios

datalen<-10000
noise<-0.3
anom_percent <- 0.02
alpha<-0.05

ARIMA1<-RUN_DATA_1(data_length=datalen,noise_value=noise)
ARIMA1 <- diff(ARIMA1)
png("ARIMA_scenario_1_d1.png",width = 800, height = 600)
plot.ts(ARIMA1)
dev.off()

ARIMA2<-RUN_DATA_2(data_length=datalen,noise_value=noise)
ARIMA2 <- diff(ARIMA2)
png("ARIMA_scenario_2_d1.png",width = 800, height = 600)
plot.ts(ARIMA2)
dev.off()

ARIMA3<-RUN_DATA_3(data_length=datalen,noise_value=noise)
ARIMA3 <- diff(ARIMA3)
png("ARIMA_scenario_3_d1.png",width = 800, height = 600)
plot.ts(ARIMA3)
dev.off()

ARIMA4<-RUN_DATA_4(data_length=datalen,noise_value=noise)
ARIMA4 <- diff(ARIMA4)
png("ARIMA_scenario_4_d1.png",width = 800, height = 600)
plot.ts(ARIMA4)
dev.off()

ARIMA5<-RUN_DATA_5(data_length=datalen,noise_value=noise)
ARIMA5 <- diff(ARIMA5)
png("ARIMA_scenario_5_d1.png",width = 800, height = 600)
plot.ts(ARIMA5)
dev.off()

ARIMA6<-RUN_DATA_6(data_length=datalen,noise_value=noise)
ARIMA6 <- diff(ARIMA6)
png("ARIMA_scenario_6_d1.png",width = 800, height = 600)
plot.ts(ARIMA6)
dev.off()



############################################################
############################################################
############################################################
############################################################
############################################################


### For checking the four distributions of anomaly multipliers
# par(mfrow = c(2, 2))
bernoulli <- function() {
  return(sample(c(-1, 1), 10000, prob = c(0.5, 0.5),replace=TRUE))
}

uniform <- function(){
  return (runif(10000,-1,1))}

linear <- function(){
  L<-10000
  x<-1:L
  probs <- seq(0, 1, length.out = L)
  probs <- probs/(sum(probs))
  samples<-sample(x/L, size = L, replace = TRUE, prob = probs)
  signs<-bernoulli()
  return (samples*signs)
}

quadratic <- function(){
  L<-10000
  x <- 1:L
  probs <- seq(0, 1, length.out = L)
  probs <- probs^2/sum(probs)
  samples<-sample(x/L, size = L, replace = TRUE, prob = probs)
  signs<-bernoulli()
  return (samples*signs)
}

png(file = "Bernoulli_distribution.png", width = 800, height = 600)
hist(bernoulli(), breaks = 100,main = "", xlab = "")
dev.off()

png(file = "Uniform_distribution.png", width = 800, height = 600)
hist(uniform(), breaks = 100,main = "", xlab = "")
dev.off()

png(file = "Linear_distribution.png", width = 800, height = 600)
hist(linear(), breaks = 100, main = "",xlab = "")
dev.off()

png(file = "Quadratic_distribution.png", width = 800, height = 600)
hist(quadratic(), breaks = 100, main = "", xlab = "")
dev.off()


############################################################
############################################################
############################################################
############################################################
############################################################

### For checking the state space models
datalen<-10000
eta <- 0.1
Xi <- 0.02
anom_percent <- 0.02

epsilon <- 1
SS1 <- State_space_data(len = datalen, wp = datalen, sd_epsilon = epsilon, sd_eta = eta, sd_Xi = Xi, seasonality=TRUE)
png("State_space_epsilon_1.png",width = 800, height = 600)
plot.ts(SS1)
dev.off()

epsilon <- 10
SS10 <- State_space_data(len = datalen, wp = datalen, sd_epsilon = epsilon, sd_eta = eta, sd_Xi = Xi, seasonality=TRUE)
png("State_space_epsilon_10.png",width = 800, height = 600)
plot.ts(SS10)
dev.off()

epsilon <- 100
SS100 <- State_space_data(len = datalen, wp = datalen, sd_epsilon = epsilon, sd_eta = eta, sd_Xi = Xi, seasonality=TRUE)
png("State_space_epsilon_100.png",width = 800, height = 600)
plot.ts(SS100)
dev.off()

epsilon <- 300
SS300 <- State_space_data(len = datalen, wp = datalen, sd_epsilon = epsilon, sd_eta = eta, sd_Xi = Xi, seasonality=TRUE)
png("State_space_epsilon_300.png",width = 800, height = 600)
plot.ts(SS300)
dev.off()

setwd(Starting_point)
getwd()



