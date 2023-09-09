
# Automated Online Sequential ESD (R)

This package includes R codes for online sequential ESD(osESD), a variation of GESD tests.  
It is a statistical testing method for anomaly detection in univariate time series datasets.  
We provide osESD and an automated grid search method auto-osESD.  
Auto-osESD can be used to find the best parameters for a specific dataset,  
using parameters either provided explicitly or basic parameters if not provided.  
Original paper can be found in [LINK].  

## Installation
### 1. Clone repository.
Clone or download zip. file of our repository into local device.

### 2. Download dependencies
Download dependencies written in main.R . 
This can be easily done by running 'install.packages' of each library
```
install.packages('optparse')
install.packages('tcltk')
install.packages('caret')
```

Also, import sources needed in running code. 
This can be done with 'source'.
```
source("models//osESD_detectors.R")
source("models//osESD_Test.R")
source("models//osESD_Transform.R")
source("utils//functions.R")

```
### 3. Datasets.
URL link to google drive with datasets used in testing and replication.

[Dataset Link](https://drive.google.com/drive/folders/1ng4eqciexoEOJp_T5D4nwXVN7OVQfBp7?usp=sharing)



## Versions
R = 4.3.1  
Rstudio = 2022.12.0+353




## Example Usage

After cloning this repository and installing all dependencies, one can run our osESD method with the below code in R,  
with data_name being directory to dataset and result_directory being directory to where indices of anomalies be exported.

```
ex_osESD_anoms<-run_osESD(data=ex_data,size=size)
```

To run auto-osESD, the below code should be run.  

```
auto_osESD_anoms<-run_auto_osESD(data=ex_data,labeled=labeled,parameters=ex_opt,weights=ex_weights,min_max_switch = min_max_switch)
```

To change parameters and provide new ones, the below code should be modified and run.  

```
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
auto_osESD_anoms<-run_auto_osESD(data=ex_data,labeled=labeled,parameters=ex_opt,weights=ex_weights,min_max_switch = min_max_switch)
```

Finally, if the dataset is unlabeled, then one should set '--labeled' to False.  
```
labeled<-FALSE
ex_auto_osESD_anoms_3<-run_auto_osESD(data=ex_data,labeled=labeled)
```
