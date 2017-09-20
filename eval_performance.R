#eval


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




