
#### This code is for replicating results of auto_osESD tests.
#### For use of auto_osESD, check 'auto_osESD.R' in the same directory.


################################################
################################################
################################################
################################################
################################################

### Delete all current objects and set working directory to current file location and set seed for consistent results.
rm(list=ls())
set.seed(42)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()
Starting_point <- getwd()

### Import needed libraries from CRAM.
library(optparse);
library(tcltk);
suppressMessages(library(caret));

### Import source codes written in directory.
source("models//osESD_Transform.R")
source("models//osESD_Test.R")
source("models//osESD_Detector_auto.R")

dir_path <- "test_results//auto_osESD_test//plots"
if (!file.exists(dir_path)) {
  dir.create(dir_path,recursive=TRUE)
  cat("Directory created:", dir_path, "\n")
} else {
  cat("Directory already exists:", dir_path, "\n")
}

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

null_opt_list <-parse_args(OptionParser(option_list = list()))
null_opt_list

Min_Max_Grid_Switch<-FALSE
if (Min_Max_Grid_Switch==FALSE){
  #### FULL
  opt_list <- list(
    make_option(c("--WindowSizes"), type="double", default=c(50,100,150,200)),
    make_option(c("--AndOr"), type="double", default=c(1,0)),
    make_option(c("--MaxRs"), type="double", default=c(3,5,7,10,20)),
    make_option(c("--Dwins"), type="double", default=c(2,5,10,20,30)),
    make_option(c("--Rwins"), type="double", default=c(4,5,10,20,30)),
    make_option(c("--Alphas"), type="double", default=c(0.001,0.005,0.01,0.05)))
# 
  # ### MINI
  # opt_list <- list(
  #   make_option(c("--WindowSizes"), type="double", default=c(150)),
  #   make_option(c("--AndOr"), type="double", default=c(1,0)),
  #   make_option(c("--MaxRs"), type="double", default=c(5,10)),
  #   make_option(c("--Dwins"), type="double", default=c(2,5)),
  #   make_option(c("--Rwins"), type="double", default=c(3,5)),
  #   make_option(c("--Alphas"), type="double", default=c(0.01)))

} else {
  opt_list <- list(
    make_option(c("--WindowSizes"), type="double", default=c(50,200)),
    make_option(c("--AndOr"), type="double", default=c(1,0)),
    make_option(c("--MaxRs"), type="double", default=c(3,20)),
    make_option(c("--Dwins"), type="double", default=c(2,30)),
    make_option(c("--Rwins"), type="double", default=c(4,30)),
    make_option(c("--Alphas"), type="double", default=c(0.001,0.05)))
}

# PRECISION, RECALL, F1SCORE, TTIME
Weights<-c(0, 0, 1, 0)

opt_parser <- OptionParser(option_list = opt_list)
opt <- parse_args(opt_parser)
opt


################################################
################################################
################################################
################################################
################################################

#### RUNNING FINAL auto_osESD testing results.

### Set directory and parameter learning length amount. Currently set to 0.2, meaning
### the initial 20% of the dataset will be used when grid searching.

setwd('Datasets//auto_osESD_tests')
parameter_learning_length <- 0.2

IDX<-1
RESULTS<-c()
PARAMS<-c()
dir()

### If unlabeled, then pseudo y values will be inserted into the beginning part of the dataset
### for f1-score comparions. After finding the best results osESD will be run on the original dataset
### and indices deemed as anomalies will be returned.

for (data_type in dir()){
  if (data_type=='labeled'){
    labeled<-TRUE
  }
  else if (data_type=='unlabeled'){
    labeled<-FALSE
  }
  setwd(data_type)
  for (data_name in dir()){
    if (data_name=="plot_results"){
      next
    }
    final_parameters<-c(data_name)
    data_results<-c(data_name)
    print("-----------------------------------")
    print(IDX)
    print(data_name)

    data <- read.csv(data_name , header=TRUE)
    full_t1<-Sys.time()
    
    ### Part there osESD_Detector_auto is run with grid search and pseudo y value insertions.
    
    tuning_results <- osESD_Detector_auto(database=data,data_label=labeled,weights=Weights,par_len=parameter_learning_length, 
                                     parameters = opt, min_max_switch=FALSE)
    
    pred_anoms <- grid_search_osESD(data=data$value,time=1:length(data$value),
                            full_size=length(data$value),init_size=tuning_results$params[2], params=tuning_results$params)
    full_t2<-Sys.time()
    
    pred_anoms_index <- which(pred_anoms == 1)
    pred_anoms_index <- pred_anoms_index[pred_anoms_index > tuning_results$params[2]]
    
    if (labeled==TRUE){
      true_outlier<- as.factor(data$anomaly[(tuning_results$params[2]+1):length(data$value)])
      pred_anoms <- as.factor(pred_anoms)
      osESD_auto_Values<-PRFTcalculator(true_outlier, pred_anoms[(tuning_results$params[2]+1):length(pred_anoms)],full_t1,full_t2)
      data_results<-c(data_results,osESD_auto_Values$precision,osESD_auto_Values$recall,
                      osESD_auto_Values$F1Score,osESD_auto_Values$Run_Time)
      RESULTS<-rbind(RESULTS,data_results)
    }
    
    if (TRUE){
      png(file=paste0(paste0(Starting_point,"//test_results//auto_osESD_test//plots//auto_osESD_",gsub("//.csv", "", data_name),".png")))
      plot.ts(data$value, main=sprintf(paste0("Detected Anomalies on ", gsub("//.csv", "", data_name))))
      points(pred_anoms_index, data$value[pred_anoms_index], col="red")
      dev.off()
    }
    final_parameters<-c(final_parameters,tuning_results$params)
    PARAMS<-rbind(PARAMS,final_parameters)
    IDX<-IDX+1
  }
  setwd('..')
}

### Export final results with precision, recall, f1-scores and run times in Results_R.csv and
### export parameters in Params_r.csv.

result_directory_path <- "test_results//auto_osESD_test//plots"

if (!file.exists(result_directory_path)) {
  dir.create(result_directory_path, recursive = TRUE)
  cat("Directory created:", result_directory_path, "\n")
} else {
  cat("Directory already exists:", result_directory_path, "\n")
}


setwd(Starting_point)
result_cols <- c('Data Name',"auto_osESD__recall","auto_osESD__precision","auto_osESD__f1score","auto_osESD__time")
param_cols  <- c('Data Name',"Function","Window Size","Max R","D window","R window","Alpha")
colnames(RESULTS)<-result_cols
colnames(PARAMS)<-param_cols
write.csv(RESULTS,file=("test_results//auto_osESD_test//auto_osESD_Results_R.csv"))
write.csv(PARAMS,file=("test_results//auto_osESD_test//auto_osESD_Params_R.csv"))
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

