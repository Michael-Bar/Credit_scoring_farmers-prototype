# Author: Michael Barber
# Date: 2017-06-01
# Modified: 2017-10-02
# Description: Master Script to call other scripts locally
# Packages Used: "plyr", "dplyr","mice",  "reshape","caTools","ROCR","corrplot","car", "plotly","randomForest", "ggplot2","sp","cowplot","car","forcats","readr
# Blog Reference: Not published


list.of.packages <- c("rstudioapi","tcltk")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(rstudioapi)
require(tcltk)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

print(getwd())
Sys.sleep(3)
rm(list=ls())

#Ordered Run
#set up files

default_path = gsub("/repayment_model","", getwd())
merged_path = paste0(default_path,"/AUG21_model_out")


#sink(file=paste0(merged_path,"/output_console.txt"), type="output")


sciny14 <- choose.files(default= paste0(default_path,"/SC/2014"), caption="Select Seasons Clients (detailed) from 2014" )
vriny14 <- choose.files(default= paste0(default_path,"/VR/2014") , caption="Select Vertical Repayment (detailed) from 2014")

sciny15 <- choose.files(default= paste0(default_path,"/SC/2015"), caption="Select Seasons Clients (detailed) from 2015" )
vriny15 <- choose.files(default= paste0(default_path,"/VR/2015") , caption="Select Vertical Repayment (detailed) from 2015")

sciny16 <- choose.files(default= paste0(default_path,"/SC/2016"), caption="Select Seasons Clients (detailed) from 2016" )
vriny16 <- choose.files(default= paste0(default_path,"/VR/2016") , caption="Select Vertical Repayment (detailed) from 2016")

sciny17 <- choose.files(default= paste0(default_path,"/SC/2017"), caption="Select Seasons Clients (detailed) from 2017" )
vriny17 <- choose.files(default= paste0(default_path,"/VR/2017") , caption="Select Vertical Repayment (detailed) from2017")


av = Sys.time()


###### Code 1 - clean data -------------------
source("Data_cleaning.R")


process_data(sciny14, vriny14, merged_path , plot_out = FALSE)
process_data(sciny15, vriny15, merged_path , plot_out = FALSE)
process_data(sciny16, vriny16, merged_path , plot_out = FALSE)
process_data(sciny17, vriny17, merged_path , plot_out = FALSE)



###### Code 2 - train model -------------------

inpath2014 <- paste(merged_path,"/2014/groupdat_merged2014.csv",sep="")
inpath2016 <- paste(merged_path,"/2016/groupdat_merged2016.csv",sep="")
inpath2015 <- paste(merged_path,"/2015/groupdat_merged2015.csv",sep="")
inpath2017 <- paste(merged_path,"/2017/groupdat_merged2017.csv",sep="")
rfoutpath <- paste(merged_path,"/",sep="")


source("train_model.R")
train_RF_on_data(rfoutpath, inpath2016, inpath2015, inpath2014, full_mode=FALSE,3)


###### Code 3 - predict new data -------------------
RF_inpath <-  paste(rfoutpath,"/RANDOMFOREST/",sep="")
data_out_path <- rfoutpath

source("predict_default.R")
RF_predictions(inpath2017, inpath2016, RF_inpath, data_out_path)

###########3
### IGNORE BELOW #######

if(1==0){
  d1 <- read.csv(paste0(data_out_path, "predictions/PREDICTIONS_2017-05-31.csv"))
  
  d1 <- read.csv(paste0(data_out_path, "predictions/Enhanced_PREDICTIONS_2017-08-22.csv"))
  
  d2 <- read.csv(inpath2017)
  colnames(d1)[1] <- "group.uniq"
  
  aud <- merge(d1,d2, by="group.uniq")
  mean(aud$default)
  
  
  aud_ <- subset(aud, default == 1)
  
  
  sum(aud_$will_default)
  sum(aud_$default)
  
  table(aud_$will_default)
  
  aud__ <- subset(aud, default==0)
  
  table(aud__$will_default)
  #table(aud__$default)
}




