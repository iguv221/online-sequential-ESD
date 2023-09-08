rm(list=ls())
set.seed(42)
# install.packages('optparse')
# install.packages('caret')
library(optparse);
library(tcltk);
suppressMessages(library(caret));

source("oGESD_Detector.R")
source("oGESD_Transform.R")
source("oGESD_Test.R")

# opt_list <- list(
#   make_option(c("-d", "--data"), type="character", default="67"),
#   make_option(c("--size"), type="double", default=100),
#   make_option(c("--dwin"), type="double", default=2),
#   make_option(c("--rwin"), type="double", default=4),
#   make_option(c("--maxr"), type="double", default=10),
#   make_option(c("--alpha"), type="double", default=0.01),
#   make_option(c("--visualize"), default=FALSE, action="store_true")
# )

# opt_parser <- OptionParser(option_list = opt_list)
# opt <- parse_args(opt_parser)
# 
# opt$visualize = TRUE
# opt$data = 1


################################################

# file_path <- paste('C:\\Users\\iguv\\Desktop\\osESD to python\\osESD_AllCodes\\data\\A3Benchmark-TS', opt$data, '.csv', sep='')
# data <- read.csv(file_path, header=TRUE)
# 
t1 <- Sys.time()
print(data)
anoms <- oGESD_Detector(data=data$value, time=data$timestamps, opt$size,
                        dwin=opt$dwin, rwin=opt$rwin, alpha=opt$alpha, maxr=opt$maxr)

t2 <- Sys.time()
outliers <- which(data$anomaly == 1)
outliers <- outliers[outliers > opt$size]

true_outlier <- as.factor(data$anomaly[(opt$size+1):length(data$anomaly)])
pred_outlier <- rep(0, length(true_outlier))
for (anom_index in anoms) {
  pred_outlier[anom_index-opt$size] <- 1
}
pred_outlier <- as.factor(pred_outlier)

recall <- sensitivity(pred_outlier, true_outlier, positive="1")
precision <- posPredValue(pred_outlier, true_outlier, positive="1")
F1 <- (2 * precision * recall) / (precision + recall)
collapsed <- as.numeric(difftime(t2, t1, units="secs"))

cat(sprintf("[A3 TS-%s] recall: %.3f, precision: %.3f, F1-score: %.3f, time: %.3f (sec)\n",
            opt$data, recall, precision, F1, collapsed))

# print(anoms)
# print(outliers)
# print(length(anoms))
# print(length(outliers))
# 
# 
# 
# if (opt$visualize) {
#   # windows(title="TRUE Anomalies", xpos=200, ypos=100)
#   # plot.ts(data$value, main=sprintf("TRUE Anomalies on dataset A3 TS-%s", opt$data))
#   # points(outliers, data$value[outliers], col="red")
#   windows(title="Detected Anomalies", xpos=900, ypos=100)
#   plot.ts(data$value, main=sprintf("Detected Anomalies on dataset A3 TS-%s", opt$data))
#   points(anoms, data$value[anoms], col="red")
#   # prompt  <- "Click button to close plots"
#   # capture <- tk_messageBox(message = prompt)
# }








#### RUN ALL TESTS CODE

RESULTS<-data.frame()
for (DATA_NUM in 1:100){
  opt$data = DATA_NUM
  file_path <- paste('C:\\Users\\iguv\\Desktop\\osESD to python\\osESD_AllCodes\\data\\A3Benchmark-TS', opt$data, '.csv', sep='')
  data <- read.csv(file_path, header=TRUE)
  
  t1 <- Sys.time()
  anoms <- oGESD_Detector(data=data$value, time=data$timestamps, train_size=opt$size,
                                dwin=opt$dwin, rwin=opt$rwin, alpha=opt$alpha, maxr=opt$maxr)
  
  t2 <- Sys.time()
  outliers <- which(data$anomaly == 1)
  outliers <- outliers[outliers > opt$size]
  
  true_outlier <- as.factor(data$anomaly[(opt$size+1):length(data$anomaly)])
  pred_outlier <- rep(0, length(true_outlier))
  for (anom_index in anoms) {
    pred_outlier[anom_index-opt$size] <- 1
  }
  pred_outlier <- as.factor(pred_outlier)
  
  recall <- sensitivity(pred_outlier, true_outlier, positive="1")
  precision <- posPredValue(pred_outlier, true_outlier, positive="1")
  F1 <- (2 * precision * recall) / (precision + recall)
  collapsed <- as.numeric(difftime(t2, t1, units="secs"))
  
  cat(sprintf("[A3 TS-%s] recall: %.3f, precision: %.3f, F1-score: %.3f, time: %.3f (sec)\n",
              opt$data, recall, precision, F1, collapsed))
  Row_Values<-c(DATA_NUM,length(outliers),length(anoms),recall,precision,F1,collapsed)
  RESULTS<-rbind(RESULTS,Row_Values)
}

colnames(RESULTS)<-c('Data Num','Reals','Preds','Recall','Precision','F1Score','Time')
RESULTS
write.csv(RESULTS,file=("oGESD_Results_R.csv"))




