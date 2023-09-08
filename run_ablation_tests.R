
#### This code is for replicating results of ablation tests.


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
source("models//all_detectors.R")
source("models//all_tests.R")
source("models//all_transformers.R")
source("utils//functions.R")

dir_path <- "test_results//ablation_test//plots"

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
################################################### Set parameters for ablation tests. In case of shorter datasets (Yahoo!) then smaller values of
### window size, dwin, rwin, and maxr is recommended. ARIMA and seasonal state-space datasets are longer
### thus larger values are used. 
################################################

### Set parameters for ablation tests. In case of shorter datasets (Yahoo!) then smaller values of
### window size, dwin, rwin, and maxr is recommended. ARIMA and seasonal state-space datasets are longer
### thus larger values are used. 
opt_list_big <- list(
    make_option(c("-d", "--data"), type="character", default="67"),
    make_option(c("--size"), type="double", default=100),
    make_option(c("--dwin"), type="double", default=10),
    make_option(c("--rwin"), type="double", default=10),
    make_option(c("--maxr"), type="double", default=10),
    make_option(c("--alpha"), type="double", default=0.01),
    make_option(c("--condition"), type="logical", default=FALSE),
    make_option(c("--visualize"), default=FALSE, action="store_true")
)
opt_parser_big <- OptionParser(option_list = opt_list_big)
opt_big <- parse_args(opt_parser_big)


opt_list_small <- list(
    make_option(c("-d", "--data"), type="character", default="67"),
    make_option(c("--size"), type="double", default=100),
    make_option(c("--dwin"), type="double", default=2),
    make_option(c("--rwin"), type="double", default=4),
    make_option(c("--maxr"), type="double", default=10),
    make_option(c("--alpha"), type="double", default=0.01),
    make_option(c("--condition"), type="logical", default=FALSE),
    make_option(c("--visualize"), default=FALSE, action="store_true")
)
opt_parser_small <- OptionParser(option_list = opt_list_small)
opt_small <- parse_args(opt_parser_small)

### Setting used models. By switching between '0' and '1' below in 
### 'Model_Switch', the user can control which model to run. 
### Currently, all four models will be executed when running this code.
Models <- c('osESD', 'oGESD', 'sosESD', 'norep_osESD')
Model_Switch<-c(1,1,1,1)

Big<-c('ARIMA','Seasonal')
Small<-c('A2Benchmark','A3Benchmark','A4Benchmark')
Testing_Datasets<-c('real','synthetic')

setwd('Datasets')

### 'RESULTS' is where all values will be saved and exported to csv when all datasets and models have been run.
### Each dataset uses 'data_results' to save its scores and this 'data_results' is appended to 'RESULTS'.
RESULTS<-c()
IDX<-1
for (dataset in Testing_Datasets){
    setwd(dataset)
    for (data_name in dir()){
        if (grepl("Benchmark", data_name)) {
            opt<-opt_small
        } else {
            opt<-opt_big
        }
        
        # if ('A2Benchmark_synthetic_44.csv'!=data_name){
        #     next
        # }
        
        data_results<-c(data_name)
        print("-----------------------------------")
        print(IDX)
        print(data_name)
        data<-read.csv(data_name, header=TRUE)
        data<-timestamp_creator(data)
        true_outlier <- as.factor(data$anomaly[(opt$size+1):length(data$value)])

        ### Running osESD. Adds scores to list 'data_results' and also draws a plot in 'test_results//ablation_test//plots'.
        if (Model_Switch[1]==1){
            t1<-Sys.time()
            osESD_anoms <- osESD_Detector(data=data$value, time=data$timestamps, dwin=opt$dwin, rwin=opt$rwin, 
                                                                        train_size=opt$size, alpha=opt$alpha, maxr=opt$maxr, condition=opt$condition)
            t2<-Sys.time()
            pred_outlier <- rep(0, length(true_outlier))
            for (anom_index in osESD_anoms) {
                pred_outlier[anom_index-opt$size] <- 1
            }
            pred_outlier <- as.factor(pred_outlier)
            osESD_Values<-PRFTcalculator(true_outlier, pred_outlier, t1,t2)
            data_results<-c(data_results,osESD_Values$precision,osESD_Values$recall,osESD_Values$F1Score,osESD_Values$Run_Time)
            if (TRUE){
                png(file=paste0(paste0(Starting_point,"//test_results//ablation_test//plots//",Models[1],"_",gsub("//.csv", "", data_name),".png")))
                plot.ts(data$value, main=sprintf(paste0("Detected Anomalies on ", gsub("//.csv", "", data_name)," with ",Models[1])))
                points(osESD_anoms, data$value[osESD_anoms], col="red")
                dev.off()
            }
        }
        
        ### Running oGESD. Adds scores to list 'data_results' and also draws a plot in 'test_results//ablation_test//plots'.
        if (Model_Switch[2]==1){
            t1<-Sys.time()
            oGESD_anoms <- oGESD_Detector(data=data$value, dwin=opt$dwin, rwin=opt$rwin, 
                                                                        train_size=opt$size, alpha=opt$alpha, maxr=opt$maxr)
            t2<-Sys.time()
            pred_outlier <- rep(0, length(true_outlier))
            for (anom_index in oGESD_anoms) {
                pred_outlier[anom_index-opt$size] <- 1
            }
            pred_outlier <- as.factor(pred_outlier)
            oGESD_Values<-PRFTcalculator(true_outlier, pred_outlier, t1,t2)
            data_results<-c(data_results,oGESD_Values$precision,oGESD_Values$recall,oGESD_Values$F1Score,oGESD_Values$Run_Time)
            if (TRUE){
                png(file=paste0(paste0(Starting_point,"//test_results//ablation_test//plots//",Models[2],"_",gsub("//.csv", "", data_name),".png")))
                plot.ts(data$value, main=sprintf(paste0("Detected Anomalies on ", gsub("//.csv", "", data_name)," with ",Models[2])))
                points(oGESD_anoms, data$value[oGESD_anoms], col="red")
                dev.off()
            }
        }
        
        ### Running sosESD. Adds scores to list 'data_results' and also draws a plot in 'test_results//ablation_test//plots'.
        if (Model_Switch[3]==1){
            t1<-Sys.time()
            sosESD_anoms <- sosESD_Detector(data=data$value, wins=opt$size, alpha=opt$alpha, maxr=opt$maxr)
            t2<-Sys.time()
            pred_outlier <- rep(0, length(true_outlier))
            for (anom_index in sosESD_anoms) {
                pred_outlier[anom_index-opt$size] <- 1
            }
            pred_outlier <- as.factor(pred_outlier)
            sosESD_Values<-PRFTcalculator(true_outlier, pred_outlier, t1,t2)
            data_results<-c(data_results,sosESD_Values$precision,sosESD_Values$recall,sosESD_Values$F1Score,sosESD_Values$Run_Time)
            if (TRUE){
                png(file=paste0(paste0(Starting_point,"//test_results//ablation_test//plots//",Models[3],"_",gsub("//.csv", "", data_name),".png")))
                plot.ts(data$value, main=sprintf(paste0("Detected Anomalies on ", gsub("//.csv", "", data_name)," with ",Models[3])))
                points(sosESD_anoms, data$value[sosESD_anoms], col="red")
                dev.off()
            }
        }
        
        ### Running norep_osESD. Adds scores to list 'data_results' and also draws a plot in 'test_results//ablation_test//plots'.
        if (Model_Switch[4]==1){
            t1<-Sys.time()
            norep_osESD_anoms <- norep_osESD_Detector(data=data$value, time=data$timestamps, dwin=opt$dwin, rwin=opt$rwin, 
                                                                                                train_size=opt$size, alpha=opt$alpha, maxr=opt$maxr)
            t2<-Sys.time()
            pred_outlier <- rep(0, length(true_outlier))
            for (anom_index in norep_osESD_anoms) {
                pred_outlier[anom_index-opt$size] <- 1
            }
            pred_outlier <- as.factor(pred_outlier)
            norep_osESD_Values<-PRFTcalculator(true_outlier, pred_outlier, t1,t2)
            data_results<-c(data_results,norep_osESD_Values$precision,norep_osESD_Values$recall,norep_osESD_Values$F1Score,norep_osESD_Values$Run_Time)
            if (TRUE){
                png(file=paste0(paste0(Starting_point,"//test_results//ablation_test//plots//",Models[4],"_",gsub("//.csv", "", data_name),".png")))
                plot.ts(data$value, main=sprintf(paste0("Detected Anomalies on ", gsub("//.csv", "", data_name)," with ",Models[4])))
                points(norep_osESD_anoms, data$value[norep_osESD_anoms], col="red")
                dev.off()
            }
        }
        RESULTS<-rbind(RESULTS,data_results)
        IDX<-IDX+1
    }
    setwd('..')
}

setwd(Starting_point)

RESULTS

### Creating final csv file with all results.
Cols <- c('Data Name')
for (idx in 1:length(Model_Switch)) {
    switch <- Model_Switch[idx]
    if (switch == 1) {
        Cols <- c(Cols, paste0(Models[idx], "__recall"))
        Cols <- c(Cols, paste0(Models[idx], "__precision"))
        Cols <- c(Cols, paste0(Models[idx], "__f1score"))
        Cols <- c(Cols, paste0(Models[idx], "__time (sec)"))
    }
}

colnames(RESULTS)<-Cols
data_results
write.csv(RESULTS,file=(Starting_point+"test_results//ablation_test//Ablation_Tests_results_R.csv"))

setwd(dirname(rstudioapi::getSourceEditorContext()$path))




