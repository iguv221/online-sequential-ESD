

#### This code is for running osESD.
#### Codes and a running example is provided below.

################################################
################################################
################################################
################################################
################################################



### Code needed for executing osESD.

rm(list=ls())
library(optparse);
library(tcltk);
suppressMessages(library(caret));
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("models//osESD_detectors.R")
source("models//osESD_Test.R")
source("models//osESD_Transform.R")
source("utils//functions.R")

run_osESD <- function(data,size=100,dwin=5,rwin=5,maxr=10,alpha=0.01,condition=TRUE,visualize=FALSE){
  data<-timestamp_creator(data)
  osESD_anoms <- osESD_Detector(data=data$value, time=data$timestamps, dwin=dwin, rwin=rwin,
                                train_size=size, alpha=alpha, maxr=maxr, condition=condition)
  return (osESD_anoms)
}


################################################
################################################
################################################
################################################
################################################

### Example use in R.

# set.seed(42)
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# getwd()
# Starting_point <- getwd()
# source("utils//functions.R")


# dir_path <- "osESD_results//"
# if (!file.exists(dir_path)) {
#   dir.create(dir_path)
#   cat("Directory created:", dir_path, "\n")
# } else {
#   cat("Directory already exists:", dir_path, "\n")
# }

# data_name<-'A2_44'
# labeled <- TRUE
# size<-100
# ex_data<-read.csv('Datasets//synthetic//A2Benchmark_synthetic_44.csv')
# ts.plot(ex_data$value)

# t1 <- Sys.time()
# ### In this example basic parameters are used. These can be adjusted accordingly to the explanations provided in our paper.
# ex_osESD_anoms<-run_osESD(data=ex_data,size=size)
# t2 <- Sys.time()
# ex_osESD_anoms

# if (labeled){
#   true_outlier<-as.factor(ex_data$anomaly[(size+1):length(ex_data$value)])
#   pred_outlier <- rep(0, length(true_outlier))
#   for (anom_index in ex_osESD_anoms) {
#     pred_outlier[anom_index-size] <- 1
#   }
#   pred_outlier <- as.factor(pred_outlier)
#   values <- PRFTcalculator(true_outlier, pred_outlier, t1,t2)
# }

# ### Write output text file.
# Output<-paste0("\nPrecision : ",values$precision,", Recall : ",values$recall,", F1-score : ",values$F1Score,", Time : ",values$Run_Time," (sec)\n\n",
#                "Real : [ ", paste(as.character(which(ex_data$anomaly==1)),collapse=", "), " ]\n\n",
#                "Anomalies : [ ", paste(as.character(ex_osESD_anoms),collapse=", "), " ]")
# file_path <- paste0(dir_path,data_name,"_osESD_result.txt")
# writeLines(Output, file_path)

# ### Draw plot with anomalies.
# if (TRUE){
#   png(file=paste0(paste0(Starting_point,"//",dir_path,gsub("//.csv", "", data_name),"_osESD_result",".png")))
#   plot.ts(ex_data$value, main=sprintf(paste0("Detected Anomalies on ", gsub("//.csv", "", data_name)," with osESD")))
#   points(ex_osESD_anoms, ex_data$value[ex_osESD_anoms], col="red")
#   dev.off()
# }

# cat(Output)

