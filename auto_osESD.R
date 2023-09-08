
#### This code is for running auto_osESD.
#### Codes and a running example is provided below.


################################################
################################################
################################################
################################################
################################################


### Set working directory to current file location and set seed for consistent results.
rm(list=ls())
set.seed(42)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


### Import needed libraries from CRAM.
library(optparse);
library(tcltk);
suppressMessages(library(caret));


### Import source codes written in directory.

source("models//osESD_Transform.R")
source("models//osESD_Test.R")
source("models//osESD_Detector_auto.R")


### Set plot layout to 1 by 1 for each plot.

par(mfrow=c(1,1)) 

################################################
################################################
################################################
################################################
################################################

#### Preparing parameters for auto_osESD.

### If Min_Max_Grid_Switch is set to FALSE, then numbers in each list will be used when grid searching.
### If set to TRUE, then only minimum and maximum values of each parameter is needed, and these will be split
### in equal lengths then tested.

run_auto_osESD<-function(data,labeled=FALSE,parameter_learning_length=0.2,parameters=parse_args(OptionParser(option_list = list())),
                         weights=c(0, 0, 1, 0),min_max_switch=FALSE){
  
  tuning_results <- osESD_Detector_auto(database=data,data_label=labeled,weights=weights,par_len=parameter_learning_length, 
                                        parameters = parameters, min_max_switch=min_max_switch)
  
  pred_anoms <- grid_search_osESD(data=data$value,time=1:length(data$value),
                                  full_size=length(data$value),init_size=tuning_results$params[2], params=tuning_results$params)
  
  pred_anoms_index <- which(pred_anoms == 1)
  pred_anoms_index <- pred_anoms_index[pred_anoms_index > tuning_results$params[2]]
  
  return (pred_anoms_index)
}





################################################
################################################
################################################
################################################
################################################

#### auto_osESD running example.
#### First one is without any explicit parameters suggested,
#### Second one is one with minimal parameters and switch turned on.
#### Third one is using an unlabeled dataset.


set.seed(42)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()
Starting_point <- getwd()
source("utils//functions.R")

dir_path <- "auto_osESD_results//"
if (!file.exists(dir_path)) {
  dir.create(dir_path)
  cat("Directory created:", dir_path, "\n")
} else {
  cat("Directory already exists:", dir_path, "\n")
}


data_name <- "ARIMA6_quad_3.csv"
ex_data <- read.csv(paste0('Datasets//synthetic//',data_name), header=TRUE)
labeled<-TRUE

t1<-Sys.time()
ex_auto_osESD_anoms_1<-run_auto_osESD(data=ex_data,labeled=labeled)
t2<-Sys.time()

if (TRUE){
  png(file=paste0(Starting_point,"//",dir_path,gsub("//.csv", "", data_name),"_auto_osESD_result",".png"))
  plot.ts(ex_data$value, main=sprintf(paste0("Detected Anomalies on ", gsub("//.csv", "", data_name)," with auto_osESD")))
  points(ex_auto_osESD_anoms_1, ex_data$value[ex_auto_osESD_anoms_1], col="red")
  dev.off()
}

if (labeled){
  true_outlier<-as.factor(ex_data$anomaly[(size+1):length(ex_data$value)])
  pred_outlier <- rep(0, length(true_outlier))
  for (anom_index in ex_auto_osESD_anoms_1) {
    pred_outlier[anom_index-size] <- 1
  }
  pred_outlier <- as.factor(pred_outlier)
  values <- PRFTcalculator(true_outlier, pred_outlier, t1,t2)
  
  ### Write output text file if labeled.
  Output_1<-paste0("\nPrecision : ",values$precision,", Recall : ",values$recall,", F1-score : ",values$F1Score,", Time : ",values$Run_Time," (sec)\n\n",
                   "Real : [ ", paste(as.character(which(ex_data$anomaly==1)),collapse=", "), " ]\n\n",
                   "Anomalies : [ ", paste(as.character(ex_auto_osESD_anoms_1),collapse=", "), " ]")
  file_path <- paste0(Starting_point,"//",dir_path,gsub("//.csv", "", data_name),"_auto_osESD_result",".txt")
  writeLines(Output_1, file_path)
} else{
  ### Write output text file if unlabeled.
  Output_1<-paste0("\nAnomalies : [ ", paste(as.character(ex_auto_osESD_anoms_1),collapse=", "), " ]")
  file_path <- paste0(Starting_point,"//",dir_path,gsub("//.csv", "", data_name),"_auto_osESD_result",".txt")
  writeLines(Output_1, file_path)
}



### Draw plot with anomalies.
if (TRUE){
  png(file=paste0(paste0(Starting_point,"//",dir_path,gsub("//.csv", "", data_name),"_auto_osESD_result",".png")))
  plot.ts(ex_data$value, main=sprintf(paste0("Detected Anomalies on ", gsub("//.csv", "", data_name)," with osESD")))
  points(ex_auto_osESD_anoms_1, ex_data$value[ex_auto_osESD_anoms_1], col="red")
  dev.off()
}

cat(Output_1)






dir_path <- "auto_osESD_results//"
if (!file.exists(dir_path)) {
  dir.create(dir_path)
  cat("Directory created:", dir_path, "\n")
} else {
  cat("Directory already exists:", dir_path, "\n")
}

ex_opt_list <- list(
  make_option(c("--WindowSizes"), type="double", default=c(50,200)),
  make_option(c("--AndOr"), type="double", default=c(1,0)),
  make_option(c("--MaxRs"), type="double", default=c(3,20)),
  make_option(c("--Dwins"), type="double", default=c(2,30)),
  make_option(c("--Rwins"), type="double", default=c(4,30)),
  make_option(c("--Alphas"), type="double", default=c(0.001,0.05)))

ex_opt <- parse_args(OptionParser(option_list = ex_opt_list))

labeled<-TRUE
min_max_switch<-TRUE
ex_weights<-c(0,0,1,0.1)

t1<-Sys.time()
ex_auto_osESD_anoms_2<-run_auto_osESD(data=ex_data,labeled=labeled,parameters=ex_opt,weights=ex_weights,min_max_switch = min_max_switch)
t2<-Sys.time()

if (TRUE){
  png(file=paste0(Starting_point,"//",dir_path,gsub("//.csv", "", data_name),"_auto_osESD_result",".png"))
  plot.ts(ex_data$value, main=sprintf(paste0("Detected Anomalies on ", gsub("//.csv", "", data_name)," with osESD")))
  points(ex_auto_osESD_anoms_2, ex_data$value[ex_auto_osESD_anoms_2], col="red")
  dev.off()
}

if (labeled){
  true_outlier<-as.factor(ex_data$anomaly[(size+1):length(ex_data$value)])
  pred_outlier <- rep(0, length(true_outlier))
  for (anom_index in ex_auto_osESD_anoms_2) {
    pred_outlier[anom_index-size] <- 1
  }
  pred_outlier <- as.factor(pred_outlier)
  values <- PRFTcalculator(true_outlier, pred_outlier, t1,t2)
  
  ### Write output text file if labeled.
  Output_2<-paste0("\nPrecision : ",values$precision,", Recall : ",values$recall,", F1-score : ",values$F1Score,", Time : ",values$Run_Time," (sec)\n\n",
                   "Real : [ ", paste(as.character(which(ex_data$anomaly==1)),collapse=", "), " ]\n\n",
                   "Anomalies : [ ", paste(as.character(ex_auto_osESD_anoms_2),collapse=", "), " ]")
  file_path <- paste0(Starting_point,"//",dir_path,gsub("//.csv", "", data_name),"_auto_osESD_result",".txt")
  writeLines(Output_2, file_path)
} else{
  ### Write output text file if unlabeled.
  Output_2<-paste0("\nAnomalies : [ ", paste(as.character(ex_auto_osESD_anoms_2),collapse=", "), " ]")
  file_path <- paste0(Starting_point,"//",dir_path,gsub("//.csv", "", data_name),"_auto_osESD_result",".txt")
  writeLines(Output_2, file_path)
}


### Draw plot with anomalies.
if (TRUE){
  png(file=paste0(paste0(Starting_point,"//",dir_path,gsub("//.csv", "", data_name),"_auto_osESD_result",".png")))
  plot.ts(ex_data$value, main=sprintf(paste0("Detected Anomalies on ", gsub("//.csv", "", data_name)," with auto_osESD")))
  points(ex_auto_osESD_anoms_2, ex_data$value[ex_auto_osESD_anoms_2], col="red")
  dev.off()
}

cat(Output_2)







dir_path <- "auto_osESD_results//"
if (!file.exists(dir_path)) {
  dir.create(dir_path)
  cat("Directory created:", dir_path, "\n")
} else {
  cat("Directory already exists:", dir_path, "\n")
}

labeled<-FALSE
data_name <- "APPL.csv"
ex_data <- read.csv(paste0('Datasets//auto_osESD_tests//unlabeled//',data_name), header=TRUE)


t1<-Sys.time()
ex_auto_osESD_anoms_3<-run_auto_osESD(data=ex_data,labeled=labeled)
t2<-Sys.time()

if (TRUE){
  png(file=paste0(Starting_point,"//",dir_path,gsub("//.csv", "", data_name),"_auto_osESD_result",".png"))
  plot.ts(ex_data$value, main=sprintf(paste0("Detected Anomalies on ", gsub("//.csv", "", data_name)," with auto_osESD")))
  points(ex_auto_osESD_anoms_3, ex_data$value[ex_auto_osESD_anoms_3], col="red")
  dev.off()
}

if (labeled){
  true_outlier<-as.factor(ex_data$anomaly[(size+1):length(ex_data$value)])
  pred_outlier <- rep(0, length(true_outlier))
  for (anom_index in ex_auto_osESD_anoms_3) {
    pred_outlier[anom_index-size] <- 3
  }
  pred_outlier <- as.factor(pred_outlier)
  values <- PRFTcalculator(true_outlier, pred_outlier, t1,t2)
  
  ### Write output text file if labeled.
  Output_3<-paste0("\nPrecision : ",values$precision,", Recall : ",values$recall,", F1-score : ",values$F1Score,", Time : ",values$Run_Time," (sec)\n\n",
                   "Real : [ ", paste(as.character(which(ex_data$anomaly==1)),collapse=", "), " ]\n\n",
                   "Anomalies : [ ", paste(as.character(ex_auto_osESD_anoms_3),collapse=", "), " ]")
  file_path <- paste0(Starting_point,"//",dir_path,gsub("//.csv", "", data_name),"_auto_osESD_result",".txt")
  writeLines(Output_3, file_path)
} else{
  ### Write output text file if unlabeled.
  Output_3<-paste0("\nAnomalies : [ ", paste(as.character(ex_auto_osESD_anoms_3),collapse=", "), " ]")
  file_path <- paste0(Starting_point,"//",dir_path,gsub("//.csv", "", data_name),"_auto_osESD_result",".txt")
  writeLines(Output_3, file_path)
}

gsub("//.csv", "", data_name)
paste0(Starting_point,"//",dir_path,gsub("//.csv", "", data_name),"_auto_osESD_result",".txt")




### Draw plot with anomalies.
if (TRUE){
  png(file=paste0(paste0(Starting_point,"//",dir_path,gsub("//.csv", "", data_name),"_auto_osESD_result",".png")))
  plot.ts(ex_data$value, main=sprintf(paste0("Detected Anomalies on ", gsub("//.csv", "", data_name)," with osESD")))
  points(ex_auto_osESD_anoms_3, ex_data$value[ex_auto_osESD_anoms_3], col="red")
  dev.off()
}

cat(Output_3)

