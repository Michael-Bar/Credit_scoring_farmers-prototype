#script to predict new default probs based on 2016 and 2015 data
#needs merged 2017 data (from function "process_data" in data_funcs.R)
#needs merged 2016 data (for looking at new district names)
#needs RF object, default name is SUPERFOREST.Rdata

#script 3 of 3
options(stringsAsFactors = FALSE)


list.of.packages <- c("plyr", "dplyr","mice",  "reshape","caTools","ROCR","corrplot","car", "plotly","randomForest", "ggplot2","sp","cowplot","car","forcats","readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(plyr, quietly = T)  #data manipulation
library(dplyr, quietly = T, warn.conflicts = F, verbose = F)
library(reshape, quietly = T)

library(lattice)
library(sp, quietly = T)
library(ggplot2, quietly = T)
library(cowplot)
library(mice)

library(readr)
library(randomForest)

library(ROCR)
library(car)

library(forcats, quietly = T) #data manipulation
library(readr, quietly = T) #faster FI
library(plotly)

find_name <- function(d, nom){
  dx = tolower(d)
  nom = tolower(nom)
  d[grepl(nom, dx)]  }


RF_predictions <- function(dat17in, oldtraindat,RF_inpath, data_out_path){
  if(0==1){ #dev mode
    dat17in <- inpath2017
    oldtraindat <- inpath2016
    RF_inpath <- RF_inpath
    #oldtraindat <- trainpath
    
    rm(dat17in)
    rm(oldtraindat)
    rm(RF_inpath)
    rm(latest)
    rm(train)
    rm(grpdat)
    rm(latest_data_in)
    rm(newdis)
    rm(pred)
    rm(pred_prob)
    rm(predictions_out)

    
    colnames(latest)
    colnames(newdis)
    
    for(i in colnames(newdis)){
      print(paste(i,mean(unlist(latest[i]), na.rm=TRUE)))
      print(paste(i,mean(unlist(newdis[i]), na.rm=TRUE)))
      print("###############")
    }
    #sanity
    
    
    }
  
  #read data    
  oldtraindat <-  load(paste0(RF_inpath,"train.Rdata"))
  oldtrain<- unique(train$District)
  grpdat = read.csv(dat17in)
  


  grpdat = Filter(function(x)!all(is.na(x)), grpdat)

  

  #### Read in RF object made by script 2 (training.R)--------
  #make_predictions_from_RF <- function(latestSCin, latestVRin){
  print("loading RF object")
  load(paste0(RF_inpath,"SUPERFOREST.Rdata"))
  load(paste0(RF_inpath,"NEW_DISTRICTFOREST.RData"))
  
  
  
  #take out decimals from names to prevent conflict
  names(grpdat) <- gsub('\\.', "_", names(grpdat))
  grpdat$sumda <- NULL
  #find missing districts
  grpdat$District <- as.factor(grpdat$District)
  oldtrain <- as.factor(oldtrain)
  #xdiff <- setdiff(levels(grpdat$District) , levels(oldtrain)  )

  #reset names to NEW
  #for(ixc in xdiff){
  #  levels(grpdat$District)[levels(grpdat$District)==ixc] <- "NEW" 
  #}
  

 
  #make default flag
  grpdat$default = grpdat$Final_repaid
  grpdat$default[grpdat$default < 100] = 1
  grpdat$default[grpdat$default >= 100] = 0
  
  
  #clean up a few last cols
  #replace infs with 0 
  grpdat$repcut_rate[grpdat$repcut_rate == Inf] = 0
  
  #set classes
  numnames = sapply(grpdat, is.numeric)
  datenames = which(lapply(grpdat, class) =="Date")
  dnames = which(lapply(grpdat, class) =="difftime")
  datenames = c(datenames,dnames)
  charnames = sapply(grpdat, is.character)
  intnames = sapply(grpdat, is.integer)
  
  numnames = colnames( grpdat[,numnames])
  datenames = colnames(grpdat[,datenames])
  charnames = colnames(grpdat[,charnames])
  intnames = colnames(grpdat[,intnames])
  grpdat[intnames] = sapply(grpdat[intnames],as.numeric)
  
  
  grpdat$last_date = as.numeric(grpdat$last_date)
  grpdat$first_date = as.numeric(grpdat$first_date)
  
  grpdat$default <- NULL
  
  #recode data for RF
  for(i in charnames){
    grpdat[[i]] = as.factor(grpdat[[i]])  }
  
  for(i in charnames){
    grpdat[[i]] = as.factor(grpdat[[i]])  }
  
  for(i in datenames){
    grpdat[[i]] = as.numeric(grpdat[[i]])  }
  
  grpdat <- as.data.frame(grpdat)
  
  
  
  #split names to get district, site, group
  temp_name <- data.frame(do.call('rbind', strsplit(as.character(grpdat$group_uniq),'-',fixed=TRUE)))
  grpdat$Site <- temp_name$X2
  grpdat$Group <- temp_name$X3
  
  nanvars <- c()
  for(i in colnames(grpdat)){
    if(sum(is.na(grpdat[i])) > 0){
      print(paste("found", sum(is.na(grpdat[i])),  "NANs in...",i))
      nanvars <- c(nanvars, i)}}
  
  
  
  
  print(paste(round(sum(is.na(grpdat))/dim(grpdat)[1]*100,5), "% missing data before attempted fix"))
  Sys.sleep(15)
  
  #clean up any vars missing due to math (e.g. division by 0)
  grpdat$sd_amount[is.na(grpdat$sd_amount) ] <- 0 # anyone with <= 1 repayment will have NA SD
  grpdat$average_amount[is.na(grpdat$average_amount) ] <- 0 # likewise with average repayment
  grpdat$repcut_rate[is.na(grpdat$repcut_rate) ] <- 0 #divide by 0 = NAN
  grpdat$cut_total_repaid[is.na(grpdat$cut_total_repaid) ] <- 0 # total of NA is NA
  grpdat$sum_rep_cut[is.na(grpdat$sum_rep_cut) ] <- 0 # total of NA is NA
  
  
  outp <- paste(data_out_path,"/predictions",sep="")
  dir.create(outp, showWarnings = FALSE, recursive = TRUE)
  
  if(sum(is.na(grpdat)) > dim(grpdat)[1]*0.05){
    print("attempting large missing data imputation, prediction quality may suffer.")
    Sys.sleep(10)
    library(VIM)
    png(paste(outp,"MICE_plots.png",sep=""), width=1080, height=1080, res=180)
    aggr_plot <- aggr(grpdat, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
    dev.off()
    subdat <- grpdat[c(nanvars, rownames(as.data.frame( sort(rf.tuned$importance[,3],decreasing = TRUE)  ))[1:4])]
    tempData <- mice(subdat,m=5,maxit=5,method="fastpmm",printFlag = TRUE)
    grpdat <- complete(tempData,1)
    
  }

  if(sum(is.na(grpdat)) <= dim(grpdat)[1]*0.05 & sum(is.na(grpdat)) >0){
    print("attempting missing data imputation...")
    Sys.sleep(10)
    library(VIM)
    png(paste(outp,"MICE_plots.png",sep=""), width=1080, height=1080, res=180)
    aggr_plot <- aggr(grpdat, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
    dev.off()
    
    subdat <- grpdat[c(nanvars, rownames(as.data.frame( sort(rf.tuned$importance[,3],decreasing = TRUE)  ))[1:7])]
    tempData <- mice(subdat,m=50,maxit=20,method="pmm",printFlag = FALSE)
    grpdat <- complete(tempData,1)
  }
  
  
  print(paste(round(sum(is.na(grpdat))/dim(grpdat)[1]*100,5), "% missing data after attempted fix"))
  Sys.sleep(5)  
  
  #rename
  latest_data_in <-  grpdat[complete.cases(grpdat),]
  

  #check for missing cols
  if(length(setdiff(rownames(rf.tuned$importance),colnames(latest_data_in) )) > 0){
    print("Error in dataframes, missing columns. STOPPING")
    stop()
  }
  
 
  print("Scaling data...")
  latest_data_in$sum_rep_cut <- scale(latest_data_in$sum_rep_cut, center=TRUE, scale=FALSE)
  latest_data_in$average_amount <- scale(latest_data_in$average_amount, center=TRUE, scale=TRUE)
  latest_data_in$sd_amount <- scale(latest_data_in$sd_amount, center=TRUE, scale=TRUE)
  latest_data_in$TotalCredit <- scale(latest_data_in$TotalCredit, center=TRUE, scale=TRUE)
  

  
  if(length(find_name(colnames(latest_data_in), "cyclecred")) >1  ){
    print("Found cycle credit metrics...")
    latest_data_in$X20XXA_CycleCredit <- scale(latest_data_in$X20XXA_CycleCredit, center=TRUE, scale=TRUE)
    latest_data_in$X20XXB_CycleCredit <- scale(latest_data_in$X20XXB_CycleCredit, center=TRUE, scale=TRUE)
    latest_data_in$changeAB <- latest_data_in$X20XXA_CycleCredit / latest_data_in$X20XXB_CycleCredit
    latest_data_in$changeAB[is.na(latest_data_in$changeAB)] = 0
    
  }
  

  
  
  #NEWLY ADDED
  ## split data into new and old districts, each to have its own model runon them
  #subset data
  #table(latest_data_in$District)
  #newdis <- latest_data_in[!latest_data_in$District %in% xdiff,]
  


  print("split data")
  latest = latest_data_in[latest_data_in$District %in% oldtrain,]
  newdis = latest_data_in[!latest_data_in$District %in% oldtrain,]

 
  match(unique(latest$District), unique(newdis$District))
  
  table(newdis$District)
  
  #newdis <- subset(latest_data_in, District=="NEW")
  #newdis$District <- NULL
  #newdis$TotalEnrolledSeasons <- NULL
  #newdis$NewMember <- NULL
  #latest <- subset(latest_data_in, District!="NEW")
  #table(latest$District)

  
  
  #setdiff(levels(train$District),levels(latest$District) )
  #setdiff(levels(latest$District),levels(train$District) )
  
  latest$District = as.character(latest$District)
  latest$District = as.factor(latest$District)
  
  newdis$District = as.character(newdis$District)
  newdis$District = as.factor(newdis$District)
  
  #print("2")
  levels(latest$District) <- c(levels(latest$District),setdiff(levels(train$District),levels(latest$District) ) )

  
  table(latest$District)
  
  pred_prob = predict(rf.tuned, latest, type = "prob") #probability of default
  pred_resp = predict(rf.tuned, latest, type = "response") # predicted class (1= default) 
  pred = data.frame("P defaulting" = pred_prob[,2], "will_default"=pred_resp)  # store as DF
  
  predictions_out <- cbind(latest[c("group_uniq","District","Site","Group","GL_Name","best_date_in_group","cut_total_repaid")], pred)
  
  table(predictions_out$District)
  #order by site and prob. 
  #predictions_out <- predictions_out[ with(predictions_out, order(Site, -P.defaulting)),]
  #write out
  
  #write.csv(predictions_out, file=paste(outp,"/PREDICTIONS_", Sys.Date(),".csv",sep=""),row.names = FALSE)
  #print(paste("Wrote CSV at",paste(outp,"/PREDICTIONS_", Sys.Date(),".csv",sep="")))
  
  #print("3")
  
  #calculate predictions for NEW districts
  
  
  if( dim(newdis)[1] >0 ){
    print("new districts found")
  pred_prob = predict(rf.newD, newdis, type = "prob") #probability of default
  pred_resp = predict(rf.newD, newdis, type = "response") # predicted class (1= default) 
  pred = data.frame("P defaulting" = pred_prob[,2], "will_default"=pred_resp)  # store as DF
  #newdis$District
  predictions_outnd <- cbind(newdis[c("group_uniq","District", "Site","Group","GL_Name","best_date_in_group","cut_total_repaid")], pred)
  
  #predictions_outnd$District <- data.frame(do.call('rbind', strsplit(as.character(predictions_outnd$group_uniq),'-',fixed=TRUE)))[1]
  #class(predictions_outnd$District)
  #predictions_outnd["District"] <- as.character(predictions_outnd$District) 

  #print(4)
  #predictions_out$District  <- "New district"
  #predictions_out$District <- as.character(predictions_out$District) 
  table(predictions_outnd$District)
  }
  
  if( dim(newdis)[1] >0 ){
    pred_out <- rbind(predictions_out, predictions_outnd)
  }
  
  if( dim(newdis)[1] ==0 ){
    pred_out <- predictions_out
  }
  
  
  #print(5)
  
  #pred_out <- pred_out[ with(pred_out, order(Site, -P.defaulting)),]
  #write out
    #write_csv(pred_out, path=paste(outp,"/Enhanced_PREDICTIONS_", Sys.Date(),".csv",sep=""))


    #force set any group with 99% +  repayment to be unflagged as a sanity check
    pred_out$will_default[pred_out$cut_total_repaid > 99.5] <- 0
    pred_out$P.defaulting[pred_out$cut_total_repaid > 99.5] <- pred_out$P.defaulting[pred_out$cut_total_repaid > 99.5] * 0.5

  write.csv(pred_out, file=paste(outp,"/Enhanced_PREDICTIONS_", Sys.Date(),".csv",sep=""),row.names = FALSE)
  #print(6)

   


  if( dim(newdis)[1] >0 ){
    print(paste("Wrote CSV at",paste(outp,"/PREDICTIONS_", Sys.Date(),".csv",sep="")))
    print(paste("Fraction of groups expected to default in new districts",  round(mean(as.numeric(predictions_outnd$will_default)-1, na.rm=TRUE),3 ) ))
    print(paste("Fraction of groups expected to default in established districts",   round(mean(as.numeric(predictions_out$will_default)-1, na.rm=TRUE),3) ))
    
  }
  
  if( dim(newdis)[1] ==0 ){
    print(paste("Wrote CSV at",paste(outp,"/PREDICTIONS_", Sys.Date(),".csv",sep="")))
    #print(paste("Fraction of groups expected to default in new districts",  round(mean(as.numeric(predictions_outnd$will_default)-1, na.rm=TRUE),3 ) ))
    print(paste("Fraction of groups expected to default in established districts",   round(mean(as.numeric(predictions_out$will_default)-1, na.rm=TRUE),3) ))
    
  }
  
  
  
  

  
  
}

#end