# Author: Michael Barber
# Date: 2017-06-01
# Modified: 2017-10-02
# Description: Cleans and summarizes data for subsequent steps
# Packages Used: "plyr", "dplyr","mice",  "reshape","caTools","ROCR","corrplot","car", "plotly","randomForest", "ggplot2","sp","cowplot","car","forcats","readr
# Blog Reference: Not published




list.of.packages <- c("plyr", "dplyr", "ggplot2","sp","cowplot","car","forcats","readr","caret")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)



library(plyr, quietly = T)  #data manipulation
library(dplyr, quietly = T, warn.conflicts = F, verbose = F)
library(sp, quietly = T)
library(ggplot2, quietly = T)
library(cowplot)
library(car)
library(forcats, quietly = T) #data manipulation
library(readr, quietly = T) #faster FI





#### define helpful functions
# define function that turns decimal percentages into pretty formats
format_pct <- function(num) {round(num*100, digits = 2)}

#calc SEM of list
sems <- function(x) {sqrt(var(x)/length(x)) }


#calc non-parametric power using Monte Carlo methods
#Below based on code from Dr. Loladze http://elifesciences.org/content/3/e02245 
power <- function(sample1, sample2, reps=500, size=10) {
  results  = sapply(1:reps, function(r) {
    resample1 = sample(sample1, size=size, replace=TRUE) 
    resample2 = sample(sample2, size=size, replace=TRUE) 
    test = wilcox.test(resample1, resample2, paired=FALSE, correct=TRUE, exact=FALSE)
    test$p.value
  })
  sum(results<0.05)/reps
}

#for manually setting width of troublesome kable tables
html_table_width <- function(kable_output, width){
  width_html = paste0(paste0('<col width="', width, '">'), collapse = "\n")
  sub("<table>", paste0("<table>\n", width_html), kable_output) }


#will return unique val or NA, useful in summarising data
uniqueorna <- function(x) {
  if (length(unique(x)) == 1) {
    unique(x)[1]
  } else {
    NA
  }
}


#calc Cohen's D
cohen_d <- function(m1,m2,s1,s2){  
  spo = sqrt((s1**2 + s2**2)/2)
  d = (m1 - m2)/spo
  effsi = d / sqrt((d**2)+4)
  ret = list("d" = d, "effectsi" = effsi)
  return(ret) }

#given two list, find members NOT in BOTH  
excluded <- function(l1,l2){
  exc = c()
  exc = setdiff(l1,l2)
  exc = c(exc, setdiff(l2,l1))
  exc = unique(exc)
  return(exc)
}



summ.lm <- function (x, digits = max(3L, getOption("digits") - 3L), symbolic.cor = x$symbolic.cor, 
                     signif.stars = getOption("show.signif.stars"), ...) 
{
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  resid = x$residuals
  df = x$df
  rdf = df[2L]
  cat(if (!is.null(x$weights) && diff(range(x$weights))) 
    "Weighted ", "Residuals:\n", sep = "")
  if (rdf > 5L) {
    nam = c("Min", "1Q", "Median", "3Q", "Max")
    rq = if (length(dim(resid)) == 2L) 
      structure(apply(t(resid), 1L, quantile), dimnames = list(nam, 
                                                               dimnames(resid)[[2L]]))
    else {
      zz = zapsmall(quantile(resid), digits + 1L)
      structure(zz, names = nam)
    }
    print(rq, digits = digits, ...)
  }
  else if (rdf > 0L) {
    print(resid, digits = digits, ...)
  }
  else {
    cat("ALL", df[1L], "residuals are 0: no residual degrees of freedom!")
    cat("\n")
  }
  if (length(x$aliased) == 0L) {
    cat("\nNo Coefficients\n")
  }
  else {
    if (nsingular <- df[3L] - df[1L]) 
      cat("\nCoefficients: (", nsingular, " not defined because of singularities)\n", 
          sep = "")
    else cat("\nCoefficients:\n")
    coefs = x$coefficients
    if (!is.null(aliased = x$aliased) && any(aliased)) {
      cn = names(aliased)
      coefs = matrix(NA, length(aliased), 4, dimnames = list(cn, 
                                                              colnames(coefs)))
      coefs[!aliased, ] = x$coefficients
    }
    printCoefmat(coefs, digits = digits, signif.stars = signif.stars, 
                 na.print = "NA", ...)
  }
  cat("\nResidual standard error:", format(signif(x$sigma, 
                                                  digits)), "on", rdf, "degrees of freedom")
  cat("\n")
  if (nzchar(mess = naprint(x$na.action))) 
    cat("  (", mess, ")\n", sep = "")
  if (!is.null(x$fstatistic)) {
    cat("Multiple R-squared: ", formatC(x$r.squared, digits = digits))
    cat(",\tAdjusted R-squared: ", formatC(x$adj.r.squared, 
                                           digits = digits), "\nF-statistic:", formatC(x$fstatistic[1L], 
                                                                                       digits = digits), "on", x$fstatistic[2L], "and", 
        x$fstatistic[3L], "DF,  p-value:", format.pval(pf(x$fstatistic[1L], 
                                                          x$fstatistic[2L], x$fstatistic[3L], lower.tail = FALSE), 
                                                       digits = digits))
    cat("\n")
  }
  correl = x$correlation
  if (!is.null(correl)) {
    p = NCOL(correl)
    if (p > 1L) {
      cat("\nCorrelation of Coefficients:\n")
      if (is.logical(symbolic.cor) && symbolic.cor) {
        print(symnum(correl, abbr.colnames = NULL))
      }
      else {
        correl = format(round(correl, 2), nsmall = 2, 
                         digits = digits)
        correl[!lower.tri(correl)] = ""
        print(correl[-1, -p, drop = FALSE], quote = FALSE)
      }
    }
  }
  cat("\n")
  invisible(x)
}


get_recursive_RF <- function(datf, targetname, CVdata, cv_y,type,SD=FALSE){
  pb = txtProgressBar(min = 0, max = 5, style = 3)
  if(SD==TRUE){
    
    setTxtProgressBar(pb, 0.2) 
    yf1 = recursive_RF(datf, targetname, CVdata, cv_y, type)
    setTxtProgressBar(pb, 2) 
    yf2 = recursive_RF(datf, targetname, CVdata, cv_y, type)
    setTxtProgressBar(pb, 3) 
    yf3 = recursive_RF(datf, targetname, CVdata, cv_y, type)
    setTxtProgressBar(pb, 4) 
    
    
    yfeat = (yf1$exc + yf2$exc + yf3$exc) / 3
    yfeatsd = c()
    for(i in seq(1:length(yf1$exc))){
      s = c(yf1$exc[i],yf2$exc[i],yf3$exc[i])
      xc = sd(s)
      yfeatsd = c(yfeatsd, xc)
      
      
    }
    setTxtProgressBar(pb, 5) 
    
    xdf = data.frame("average_CVscore" = yfeat, "SD_CVscore" = yfeatsd)
    x = seq(1:dim(xdf)[1])/ dim(xdf)[1] *100
    xdf$x = x
    xdf$no = seq(1:dim(xdf)[1])
    xdf$feats = yf1$feats
    
    return(xdf)}
  
  
  
  if(SD==FALSE){
    setTxtProgressBar(pb, 3) 
    cp = recursive_RF(dataf, targetname, CVdata, cv_y, type)
    setTxtProgressBar(pb, 5) 
    return(cp)
    
    
  }
}


recursive_RF <- function(dataf, targetname, CVdata, cv_y,type){
  #this will recursively eliminate features from our RF by cross val
  corr_pred = c()
  featnam = c()
  if(type=="Classification"){
    xl = dim(dataf)[2]
    newtrain = dataf
    
    for(i in seq(1:xl)) {
      cl = colnames(newtrain)
      
      ix = xl-i
      if(ix > 1){
        loc = which(colnames(newtrain)==targetname)
        xn = names(newtrain)[-loc]
        xnames = paste(names(newtrain)[-loc], collapse=" + ")
        Xj = as.formula(paste("default ~ ",xnames,sep = ""))
        rf.rec = randomForest(Xj , ntree=50, replace=TRUE, importance = TRUE, norm.votes = TRUE, data=newtrain)
        varimp = as.data.frame( sort(rf.rec$importance[,3],decreasing = TRUE)  )
        pred = predict(rf.rec, newdata = CVdata , type="response", norm.votes=TRUE) 
        x = as.numeric(pred) -1
        cv = x + (as.numeric(cv_y)-1)
        correct_predictions = (sum(cv==0) + sum(cv==2)) /  length(cv) *100
        #print(paste("Total correct prediction % ==", round(correct_predictions,2 )))
        newtrain = newtrain[,1:ix]
        newtrain$default = dataf$default
        corr_pred = c(corr_pred,correct_predictions )
        cl2 = colnames(newtrain)
        xcd = setdiff(cl,cl2)
        featnam = c(featnam, xcd)
      }}
    
    if(type!= "Classification"){
      print("Currently only rigged for classification")
    }
    featnam = c("NONE",featnam)
    xd = data.frame("exc"=corr_pred, "feats"=featnam)
    
    return(xd)
  } }

redo_types <- function(dataframess){
  #####redo classes -----------
  numnames = c(which(lapply(dataframess, class) =="numeric"), which(lapply(dataframess, class) =="integer")) 
  datenames = c(which(lapply(dataframess, class) =="Date"), which(lapply(dataframess, class) =="difftime")) 
  charnames = c(which(lapply(dataframess, class) =="character"), which(lapply(dataframess, class) =="factor")) 

  
  #numnames = colnames( dataframess[,numnames])
  #datenames = colnames(dataframess[,datenames])
  #charnames = colnames(dataframess[,charnames])
  #intnames = colnames(dataframess[,intnames])
  #dataframess[intnames] = sapply(dataframess[intnames],as.numeric)
  
  #recode data for RF
  for(i in numnames){
    dataframess[i] = as.numeric(dataframess[[i]])  }
  
  for(i in charnames){
    dataframess[i] = as.factor(dataframess[[i]])  }
  
  for(i in datenames){
    dataframess[i] = as.numeric(unlist(dataframess[[i]]))  }
  
  return(dataframess)   }


#pretty plots on same page
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots = c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout = matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx = as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


find_name <- function(d, nom){
  dx = tolower(d)
  nom = tolower(nom)
  d[grepl(nom, dx)]  }


clear_NA <- function(d, threshhold){
  for(i in colnames(d)){
    xxx = sum(is.na(unlist(d[i])))
    if(xxx/dim(d)[1] >threshhold){
      d[i] = NULL
    }   }
  return(d) }

#SUPER function to process data

process_data <- function(scin, vrin, path_outy, plot_out =FALSE , fakedate=Sys.Date(), devmode=0){
pb = txtProgressBar(min = 0, max = 10, style = 3)
setTxtProgressBar(pb, 1) 
axxxx = Sys.time()



#year_in = 2015
if(1==0){
  #dev mode
  scin = "C:/Users/Michael/OneDrive/OAF/MB/Projects/Roster_data/Rwanda/SC/2014/Season Clients Detailed_20170522-102845.csv"
  vrin = "C:/Users/Michael/OneDrive/OAF/MB/Projects/Roster_data/Rwanda/VR/2014/Detailed_20170522-102805.csv"
  path_out = merged_path
  scin = sciny16
  vrin = vriny16
  path_outy = merged_path
  fakedate=Sys.Date()
  plot_out=FALSE
  devmode=0
  
  rm(devmode)
  rm(path_out)
  rm(scin)
  rm(vrin)
  rm(fakedate)
  
}

if(devmode==1){
  print("    DEVELOPER mode is ON. WARNING - this will not produce an accurate model, please make devmode=0")
  Sys.sleep(5)
}

#check args
print("Check args...")
print(scin)
print(vrin)
print(path_outy)
print(fakedate)
#####Read in data
#read in few lines for good titles and classes

titles = read.csv(scin, nrows = 100)
#read in actual data with the faster read_csv 
scdat = suppressMessages(read_csv(scin, trim_ws = TRUE, progress=FALSE))
colnames(scdat) = colnames(titles)


titles = read.csv(vrin, nrows = 100)
#read in actual data with the faster read_csv 
vrdat = suppressMessages(read_csv(vrin, trim_ws = TRUE,progress=FALSE))

colnames(vrdat) = colnames(titles)
titles = NULL

vrdat_slim = data.frame( "GlobalClientID" = vrdat$GlobalClientID, "Date" = vrdat$RepaymentDate, "Value" = vrdat$Amount, "Type"=vrdat$Type )



# full DF merged with VR --------------------------------------------------


scdat$SeasonName = gsub("[^0-9]", "", scdat$SeasonName) 


#check year
year_in = as.numeric(unique(scdat$SeasonName))
print(paste("identified data from....", year_in))


#ADD REAL DATES HERE


if(year_in > 2014& year_in <2017){
  print("proceeding...")
}


if(year_in < 2014| year_in >2017){
  print("year_in is out of bounds, please check and run again. STOPPPING")
  stop()
  
}

setTxtProgressBar(pb, 2)
#####extract useful bits from VR

#use real deadline, not extended deadline

vrdat_slim$Date =as.Date(vrdat_slim$Date, origin="1970-01-01")

summary(vrdat_slim$Date)

if(year_in==2014){
  #vr_sim = subset(vrdat_slim, Date<= "2015-05-01")
  xx = subset(vrdat_slim, Date > "2014-09-14") 
  vrdat_slim = subset(vrdat_slim, Date <= "2018-11-14") #uncomment for real use
  #vrdat_slim = subset(vrdat_slim, Date<="2014-09-08") #uncomment for real use

}



if(year_in==2015){
  #vr_sim = subset(vrdat_slim, Date<= "2015-05-01")
  vrdat_slim = subset(vrdat_slim, Date <= "2018-11-14") #uncomment for real use
  #vrdat_slim = subset(vrdat_slim, Date <= "2015-09-28") #uncomment for real use
  #vrdat_slim = subset(vrdat_slim, Date <= "2015-09-20") #uncomment for real use
  #vrdat_slim = subset(vrdat_slim, Date <= "2015-09-06") #uncomment for real use

}

if(year_in==2016){
  #vr_sim = subset(vrdat_slim, Date<= "2016-05-01")
  vrdat_slim = subset(vrdat_slim, Date <= "2018-11-14") #uncomment for real use
  #vrdat_slim = subset(vrdat_slim, Date<="2016-10-24") #uncomment for real use
  #vrdat_slim = subset(vrdat_slim, Date<="2016-09-04") #uncomment for real use
  #vrdat_slim = subset(vrdat_slim, Date<="2016-09-18") #uncomment for real use

}

if(year_in==2017){
  vrdat_slim = subset(vrdat_slim, Date<="2017-10-01" )#remove any weird dates

}


findat= summarise(group_by(vrdat_slim,GlobalClientID),  fin_am= sum(Value)     )   


scdat <- merge(scdat, findat, by="GlobalClientID")


#remove old definitions and use new instead
scdat$temp = scdat$X..Repaid
scdat$X..Repaid <- NULL
scdat$TotalRepaid <- NULL

scdat$TotalRepaid <- scdat$fin_am
scdat$X..Repaid <- (scdat$TotalRepaid / scdat$TotalCredit)*100.0
scdat$X..Repaid[scdat$X..Repaid == Inf] = 0
scdat$X..Repaid[is.na(scdat$X..Repaid)] = 0
scdat$TotalRepaid[scdat$TotalRepaid ==Inf]=0

print(paste("difference between new and old repayment %=", mean(scdat$temp - scdat$X..Repaid)))


print(mean(scdat$temp))
print(mean(scdat$X..Repaid, na.rm=TRUE))
print(sum(is.na(scdat$X..Repaid)))
#print(head(scdat[c("temp","X..Repaid")],20))
#print(tail(scdat[c("temp","X..Repaid")],20))

#merge with SC data
#scdat2 = merge(sumdat, scdat, by="GlobalClientID")

datenow = as.Date(fakedate)
if(0==1){
  #devmode
  datenow = Sys.Date()
  
}
print(paste("   selected cut date....", datenow))


# Cutoff DF and export -----------------------------------------------
####now lets make a sister data.frame which ends May 1st - to predict performance based on data we would have available

if(year_in==2012){
  #vr_sim = subset(vrdat_slim, Date<= "2015-05-01")
  vr_sim = subset(vrdat_slim, Date<(datenow-1826)) #uncomment for real use
  fdate= datenow-1826
}

if(year_in==2013){
  #vr_sim = subset(vrdat_slim, Date<= "2015-05-01")
  vr_sim = subset(vrdat_slim, Date<(datenow-1461)) #uncomment for real use
  fdate= datenow-1461
}

if(year_in==2014){
  #vr_sim = subset(vrdat_slim, Date<= "2015-05-01")
  vr_sim = subset(vrdat_slim, Date<(datenow-1096)) #uncomment for real use
  fdate= datenow-1096
}


if(year_in==2015){
  #vr_sim = subset(vrdat_slim, Date<= "2015-05-01")
  vr_sim = subset(vrdat_slim, Date<(datenow-731)) #uncomment for real use
  fdate= datenow-731
}

if(year_in==2016){
  #vr_sim = subset(vrdat_slim, Date<= "2016-05-01")
  vr_sim = subset(vrdat_slim, Date<(datenow-365)) #uncomment for real use
  fdate= datenow-365
}

if(year_in==2017){
  vr_sim = subset(vrdat_slim, Date<=datenow) #remove any weird dates
  fdate= datenow-0
}

print(paste("date used:", fdate))


#only keep receipts from repayment
vr_sim = subset(vr_sim, Type == "Receipt" | Type == "Auditor" | Type =="Transfer")

dim(vr_sim)

if( dim(subset(vr_sim, Type=="Transfer"))[1] > 1   ){
  vr_sim$Transfer = vr_sim$Type
  vr_sim$Transfer = as.character(vr_sim$Transfer)
vr_sim$Transfer[vr_sim$Transfer !="Transfer"] = 0
vr_sim$Transfer[vr_sim$Transfer =="Transfer"] = 1
vr_sim$Transfer = as.numeric(vr_sim$Transfer)
  }

if( dim(subset(vr_sim, Type=="Transfer"))[1] <1   ){
  vr_sim$Transfer = 0
}




if(devmode==1){
  #shrink data for dev mode
  x=dim(vr_sim)[1]
  library(caTools)
  vr_sim = sample_n(vr_sim ,size=20000, replace=FALSE)
  print(paste("DEVMODE: reduce data from", x, "to", dim(vr_sim)[1]))
  Sys.sleep(3)
  
}

library(dplyr)
setTxtProgressBar(pb, 3)
#sumda =  ddply(vr_sim, c("GlobalClientID"), summarise,  last_date= max(Date), first_date = min(Date), sum_rep_cut=sum(Value), N_repayments=length(Date), average_amount=mean(Value), sd_amount=sd(Value) ,Transfer=mean(Transfer ) )

s1 = group_by(vr_sim,GlobalClientID)

sumda= summarise(s1,  last_date= max(Date), first_date = min(Date), sum_rep_cut=sum(Value), N_repayments=length(Date), average_amount=mean(Value), sd_amount=sd(Value) ,Transfer=mean(Transfer )     )   
rm(s1)

sumda = as.data.frame(sumda)

sumda$first.last.date = sumda$last_date - sumda$first_date
sumda$sumda[is.na(sumda$sd_amount) ] = 0 # do this for people with <= 1 repayment
sumda$average_amount[is.na(sumda$average_amount) ] = 0

sumda =  transform(sumda, repcut.rate = sum_rep_cut/as.numeric(first.last.date))



#merge with SC 

scdat$Phone_number = scdat$ClientPhone


scdat_sim = merge(sumda,scdat, by="GlobalClientID")






# ###binarise all the other columns??
# for(ix in colnames(scdat_sim)[75:length(colnames(scdat_sim))]){
#   try(scdat_sim[ix][scdat_sim[ix] > 0] <- 1) }

#replace char TRUE with bool
for(ix in colnames(scdat_sim)){
  if(is.character(scdat_sim[,ix]) == TRUE){
    if(isTRUE(unique(scdat_sim[ix])[1,]=="False" | unique(scdat_sim[ix])[1,]=="True")==TRUE) {
      #print(ix)
      scdat_sim[ix][scdat_sim[ix] =="False"] = 0
      scdat_sim[ix][scdat_sim[ix] =="True"] = 1  
      scdat_sim[,ix] = as.numeric(scdat_sim[,ix])  }}}




##write ---------




#drop any all na cols
scdat_sim = Filter(function(x)!all(is.na(x)), scdat_sim)
#scdat2 = Filter(function(x)!all(is.na(x)), scdat2)




#drop any remaining character cols (for now - we may add these back in)
scdat_simdrop = scdat_sim[, !sapply(scdat_sim, is.character)]


#add phones back in
scdat_simdrop$Phone_number = scdat_sim$Phone_number



#add back district and site
scdat_simdrop$District = scdat_sim$DistrictName
scdat_simdrop$Site = scdat_sim$SiteName
scdat_simdrop$GroupName = scdat_sim$GroupName
scdat_simdrop$FirstName = scdat_sim$FirstName
scdat_simdrop$LastName = scdat_sim$LastName



#paste("dropping:",setdiff(colnames(scdat_sim),colnames(scdat_simdrop)))

setTxtProgressBar(pb, 4)

###### PART 2 ------ 

#feel free to just run from here if making changes later (the above is very slow)

#development tool: switch if just wanting to redo later steps
if(1==0){
  scdat2 = read.csv( file="C:/Users/Michael/OneDrive/OAF/MB/Projects/Roster_data/Rwanda/SC/2016/SCVR_merge2016.csv")

  scdat_simdrop =read.csv( file="C:/Users/Michael/OneDrive/OAF/MB/Projects/Roster_data/Rwanda/SC/2016/SCVR_merge_fakecutoff_may16.csv")
}


###further cleaning, feature building, and summarise to group level

cutdat = scdat_simdrop


#features
colnames(cutdat) = gsub("20..","20XX",colnames(cutdat))



cutdat = Filter(function(x)!all(is.na(x)), cutdat)


if(length(find_name(colnames(cutdat), "CycleCred")) >0 ) {
  #change in AB credit
  cutdat$changeAB = cutdat$X20XXA_CycleCredit/cutdat$X20XXB_CycleCredit
  cutdat$changeAB[cutdat$changeAB == Inf] = 0
  cutdat$changeAB[is.na(cutdat$changeAB)] = 0
}





#rename
names(cutdat)[names(cutdat)=="X..Repaid"] = "Final.repaid"


#not important, leaving in for hypothesis reporting, but remove from final training
if(length(find_name(colnames(cutdat), "maiz")) > 0){
#key maize choices
  xl = find_name(colnames(cutdat), "maiz")
  cutdat$maize_general = 0
  
  for(i in xl){
    cutdat$temp = unlist(cutdat[i])
    cutdat$maize_general = rowSums(cutdat[,c("temp","maize_general")], na.rm=TRUE)
  }  }

if(length(find_name(colnames(cutdat), "maiz")) == 0){
  cutdat$maize_general = 0
}

  


if(  length(find_name( find_name(colnames(cutdat), "fertil") , "maize"  ) )  > 0){  
  xl = find_name( find_name(colnames(cutdat), "fertil") , "acres"  )
  cutdat$maize_fert = 0
  for(i in xl){
    i
    cutdat$temp = unlist(cutdat[i])
    cutdat$maize_fert = rowSums(cutdat[,c("temp","maize_fert")], na.rm=TRUE)
  }  }

if(  length(find_name( find_name(colnames(cutdat), "fertil") , "maize"  ) )  == 0){
  cutdat$maize_fert = 0
}


#get total repaid by cutoff date (see script 1)
cutdat$cut.total.repaid = cutdat$sum_rep_cut/cutdat$TotalCredit * 100


#clean up math issues
cutdat$sd_amount[is.na(cutdat$sd_amount) ] = -1 # anyone with <= 1 repayment will have NA SD
cutdat$average_amount[is.na(cutdat$average_amount) ] = -1 # likewise with average repayment
cutdat$repcut.rate[is.na(cutdat$repcut.rate) ] = 0
cutdat$cut.total.repaid[is.na(cutdat$cut.total.repaid) ] = -1
cutdat$sum_rep_cut[is.na(cutdat$sum_rep_cut) ] = -1
library(caret)



print("  Calculating solar adoption...")
print("looking for keywords: Solar, energy, SKP, sun, ishy")
solnames = c(find_name(colnames(cutdat),"solar"), find_name(colnames(cutdat),"energy"), find_name(colnames(cutdat),"skp") , find_name(colnames(cutdat),"sun"), find_name(colnames(cutdat),"ishyi")  )
print(paste("Found solar col \n", solnames))
cutdat$solar = rowSums(cutdat[solnames], na.rm=TRUE)
cutdat$solar[cutdat$solar > 0] = 1
print(paste("Estimating mean solar adoptions to be....", round(mean(cutdat$solar, na.rm=TRUE)*100),"% in year",year_in))


setTxtProgressBar(pb, 5)
#get diff date rather than raw date
#lets use max date - date to get how many days ago (roughly) they last paid




#make default flag - this differes from default in that it will end up as a fraction of defaulters in group
cutdat$default_av = cutdat$Final.repaid
cutdat$default_av[cutdat$default_av < 100] = 1
cutdat$default_av[cutdat$default_av >= 100] = 0

# #quick feature selec
# cutdat$group.uniq = paste(cutdat$District, cutdat$Site, cutdat$GroupName, sep="-")
# 
# gd = cutdat %>% group_by(group.uniq) %>% summarise_each(funs(mean))
# 
# 
# #new feature sel step 1
# mem_cols = c()
# for(i in colnames(gd)){
#   x = sum(is.na(gd[i]))
#   if(x==0){
#     mem_cols = c(mem_cols, i)  }  }
# 
# 
# 
# #quick feature selection
# ftrsel = gd[,mem_cols]
# ftrsel$def = ftrsel$default_av
# 
# if(dim(ftrsel)[1]>=2001){
#   ftrsel = ftrsel[sample(nrow(ftrsel), 2000), ]
#  
# }
# 
# ftrsel = as.data.frame(ftrsel)
# ftrsel$def = ftrsel$Final.repaid
# ftrsel$def[ftrsel$def< 100] =0
# ftrsel$def[ftrsel$def>=100] =1
# ftrsel$def = as.factor(ftrsel$def)
# 
# 
# ftrsel$Final.repaid = NULL
# 
# 
# ftrsel= Filter(function(x)!all(is.na(x)), ftrsel)
# ftrsel$group.uniq = NULL
# 
# ftrsel = redo_types(ftrsel)
# ftr = ftrsel[complete.cases(ftrsel),]
# colnames(ftr)
# ftr$default_av = NULL
# ftr$RemainingCredit = NULL
# 
# ftr$OAFID = NULL
# 
# 
# ftr = as.data.frame(ftr)
# 
# for(i in colnames(ftr)){
#   x = sum(is.infinite(unlist(ftr[i])))
#   if(x >0 ){
#     ftr$temp = unlist(ftr[i])
#     #print(i)
#     ftr$temp[ftr$temp==Inf] = 0
#     ftr[i] = ftr$temp
#     
#   }
# }
# ftr$temp = NULL
# 
# fs = rfcv(ftr[-which(colnames(ftr)=="def")], ftr$def, cv.fold=3, step=1.2, recursive = TRUE)
# 
# nvars = names(fs$error.cv[which(fs$error.cv==min(fs$error.cv))][length(fs$error.cv[which(fs$error.cv==min(fs$error.cv))])])
# nvars = as.numeric(nvars)
# 
# xn = names(ftr)
# xn = xn[-c(which(xn=="def"))]
# xnames = paste(xn, collapse=" + ")
# Xj = as.formula(paste("def ~ ",xnames,sep = ""))
# simp = randomForest(Xj , ntree=200, replace=TRUE, importance = TRUE, norm.votes = TRUE, data=ftr, keep.forest=TRUE)
# varImpPlot(simp)
# v = as.data.frame(varImp(simp))
# v$`0` = abs(v$`0`)
# v$`1` = abs(v$`1`)
# varimpo = v[order(-v$`0`),] 
# rownames(varimpo)[1:nvars]



#slim columns for first pass
#keye vars
cutdat$temp = NULL


#find_name(colnames(scdat),"phone")

iny = c("last_date", "Site", "cut.total.repaid", "GroupName" ,"first_date", "Final.repaid" , "sum_rep_cut" ,"N_repayments","average_amount", "sd_amount", "Transfer",   "NewMember",   "TotalEnrolledSeasons" ,   "Facilitator", "TotalCredit", "X20XXA_CycleCredit","X20XXB_CycleCredit" ,  "District",    "changeAB", "repcut.rate"  ,"solar","last_date",	"first_date",	"sum_rep_cut",	"N_repayments",	"average_amount",	"sd_amount",	"Transfer",	"first.last.date",	"repcut.rate",	"NewMember",	"TotalEnrolledSeasons",	"Facilitator",	"TotalCredit",	"X20XXA_CycleCredit",	"X20XXB_CycleCredit",	"District",	"changeAB")
iny = c(iny, "solar","maize_fert","maize_general","default_av","LastName", "FirstName","Phone_number")
iny = unique(iny)

#match(colnames(cutdat), iny)


#inc cols
shortdat = as.data.frame(cutdat$District)
colnames(shortdat) = "District"
#safeguard to re intro key columns
for(i in iny){
  try( shortdat[i] <- unlist(cutdat[i]) )    }

dim(shortdat)
colnames(shortdat)
setdiff(colnames(cutdat),colnames(shortdat))


# if(year_in==2015){
#   #shortdat = cutdat[-53:-310] #2015 # estimated useless cols
#   #iny = c("last_date", "first_date",  "Site", "sum_rep_cut" ,"N_repayments","average_amount", "sd_amount", "Transfer",  "NewMember",   "TotalEnrolledSeasons" ,   "Facilitator", "TotalCredit", "X20XXA_CycleCredit","X20XXB_CycleCredit" ,  "District",    "changeAB", "solar_A")
#   for(i in iny){  #safeguard to re intro key columns
#     
#     try(shortdat[i] = cutdat[i])
#   }
# }
# 
# if(year_in==2014){
#   #shortdat = cutdat[1:2] #2015 # estimated useless cols
#   #iny = c("last_date", "first_date",  "Site", "sum_rep_cut" ,"N_repayments","average_amount", "sd_amount", "Transfer",  "NewMember",   "TotalEnrolledSeasons" ,   "Facilitator", "TotalCredit", "X20XXA_CycleCredit","X20XXB_CycleCredit" ,  "District",    "changeAB", "solar_A")
#   for(i in iny){
#     
#     try(shortdat[i] = cutdat[i])
#   }
# }
# colnames(shortdat)
# 
# if(year_in==2016){
#   #shortdat = cutdat[-53:-581] #2016
#   #iny = c("last_date", "first_date",  "Site", "sum_rep_cut" ,"N_repayments","average_amount", "sd_amount", "Transfer",    "NewMember",   "TotalEnrolledSeasons" ,   "Facilitator", "TotalCredit", "X20XXA_CycleCredit","X20XXB_CycleCredit" ,  "District",    "changeAB"  ,"solar_A")
#   for(i in iny){
#     try(shortdat[i] = cutdat[i])
#   }
# }
# 
# if(year_in==2017){
#   print(paste("2017 feature: ", colnames(cutdat)))
#   shortdat = cutdat[1:30]
#   ####iny = c("last_date", "Site", "cut.total.repaid", "first_date", "Final.repaid" , "sum_rep_cut" ,"N_repayments","average_amount", "sd_amount", "Transfer",   "NewMember",   "TotalEnrolledSeasons" ,   "Facilitator", "TotalCredit", "X20XXA_CycleCredit","X20XXB_CycleCredit" ,  "District",    "changeAB"  ,"solar_A")
#   for(i in iny){
#     
#   try(shortdat[i] = cutdat[i])
#   }
#  
# }


setTxtProgressBar(pb, 6)
if(plot_out == TRUE){
  ##explore data-------------
  h1 = ggplot(cutdat, aes(x=cut.total.repaid)) + geom_density(bw=5) +ggtitle("Total repaid by now") + xlim(0,100)
  h2 = ggplot(cutdat, aes(x=Final.repaid)) + geom_density(bw=5) +ggtitle("Final repaid") + xlim(0,100)
  multiplot(h1,h2,  cols=2)
  
  h3 = ggplot(cutdat) + geom_point(aes(x=cut.total.repaid, y=Final.repaid)) + ggtitle("Repayment vs repayment") + xlab("by now % repaid") + ylab("End of season % repaid")
  h4 = ggplot(cutdat, aes(x=last_date)) + geom_density(bw=0.05) +ggtitle("Last repayment (by now)")
  h5 = ggplot(cutdat) + geom_point(aes(x=first.last.date,y=Final.repaid)) + xlab("Last - first date") +ggtitle("Length of repayment vs %") + ylab("EOS repaid %")
  
  multiplot(h3,h4,h5, cols=3)
}




#lets use an easer DF first-------
shortdat = as.data.frame(shortdat)


#drop names
shortdat$group.uniq = paste(shortdat$District, shortdat$Site, shortdat$GroupName, sep="-")

colnames(shortdat)

#add in GL names
xtemp = subset(shortdat, Facilitator==1)


while(length(rownames(xtemp[duplicated(xtemp$group.uniq),] )) ) {
  print(" Duplicated District-site-group concats, fixing...")
  rn = rownames(xtemp[duplicated(xtemp$group.uniq),] )
  
  xtemp[duplicated(xtemp$group.uniq),]$group.uniq = paste0(xtemp[duplicated(xtemp$group.uniq),]$group.uniq,"2_")
  
}




xtemp$FirstName[is.na(xtemp$FirstName)] = "x"
xtemp$LastName[is.na(xtemp$LastName)] = "x"

xtemp$GL_Name = paste(xtemp$FirstName, xtemp$LastName, "--", xtemp$Phone_number, "leads", xtemp$group.uniq  ,sep=" ")

print(paste("Sample GL name/number:", xtemp$GL_Name[1:2]))

length(unique(xtemp$group.uniq))
length(unique(xtemp$GL_Name))




shortdat = merge(shortdat, xtemp[c("group.uniq","GL_Name")], by="group.uniq", all=TRUE)



shortdat$LastName = NULL
shortdat$FirstName = NULL
shortdat$GL_Name = as.character(shortdat$GL_Name)

xx = unique(shortdat$group.uniq[is.na(shortdat$GL_Name)])
#xx



for(i in seq(1:length(xx) ) ) {
  xnam = xx[i]
  shortdat$GL_Name[shortdat$group.uniq==xnam] = paste0("Missing Name", i)
}


  


#shortdat$GL_Name[is.na(shortdat$GL_Name)] = paste("MISSING NAME", shortdat$group.uniq, sep=" ")


#print("   Getting GL names...")


#shortdat$group.uniq

#drop anyone with negative repayment (bug?)
shortdat = shortdat[which(shortdat$Final.repaid >= 0),]

setTxtProgressBar(pb, 7)
#drop any dull columns
shortdat$SeasonName = NULL
shortdat$NationalID = NULL
shortdat$OAFID = NULL
shortdat$FieldOfficerPayrollID = NULL
shortdat$Dropped = NULL
shortdat$SeasonID = NULL
#shortdat$GroupID = NULL
shortdat$X2016B_CycleFeePortion = NULL
shortdat$X2016A_Enrollment.Fee.adjustment = NULL
shortdat$X2016B_Enrollment.Fee.adjustment = NULL
shortdat$Refund_Overpaid.Refund_Total = NULL

#print("   Normalising data")

shortdat$last_date_ori = shortdat$last_date

#added to test AUC
if(1==0){
shortdat$first_date = scale(as.numeric(shortdat$first_date))
shortdat$last_date =  scale(as.numeric(shortdat$last_date))
shortdat$first.last.date = scale(as.numeric(shortdat$first.last.date))
}




#spin off target - pythonic
target = data.frame("Final repaid" = shortdat$Final.repaid)

shortdat = redo_types(shortdat)

setTxtProgressBar(pb, 8)


##




#get group size
grp = as.data.frame(table(shortdat$group.uniq) )
colnames(grp) = c("group.uniq","grp.size")
#head(grp)

temp = as.data.frame(shortdat)


temp$GroupName = NULL
temp$GlobalClientID = NULL
temp$GL_Name = NULL


x = dim(temp)[1]

length(unique(temp$GL_Name))
length(unique(temp$group.uniq))

df = data.frame("GL_Name"=unique(shortdat$GL_Name), "group.uniq"=unique(shortdat$group.uniq))

xglname =unique(temp$GL_Name)


#summarise remaining into groups
#numerics

temp$GL_Name = NULL
temp$Phone_number = NULL
print(colnames(temp))
groupdat = temp %>% group_by(group.uniq) %>% summarise_each(funs(mean))
#gdname = tem %>% group_by(group.uniq) %>% summarise_each(funs(uniqueorna(.)))

groupdat$GL_Name = unique(shortdat$GL_Name)
groupdat = clear_NA(groupdat, 0.95)


#errors with dplyr
temp$GL_Name = NULL


dat2 = temp %>% group_by(group.uniq) %>% summarise_each(funs(uniqueorna(.)))
dat2 = clear_NA(dat2, 0.95)




#get group level metrics for indication/communication
#this one is the last date anyone in the group made a payment
reportmetrics = temp[c("group.uniq","last_date_ori")] %>% group_by(group.uniq) %>% summarise_each(funs(max))
reportmetrics2 = temp[c("group.uniq","last_date_ori")] %>% group_by(group.uniq) %>% summarise_each(funs(min))

groupdat = merge(groupdat, grp, by="group.uniq")
groupdat = Filter(function(x)!all(is.na(x)), groupdat)

#groupdat = merge(groupdat, dat2, by="group.uniq")
groupdat$District = dat2$District
groupdat$Site = dat2$Site
groupdat$best_date_in_group = reportmetrics$last_date_ori
groupdat$worst_date_in_group = reportmetrics2$last_date_ori

groupdat$last_date_ori = NULL

#this is for ML so needs to be standardised
groupdat$best_date = reportmetrics$last_date_ori
groupdat$worst_date= reportmetrics2$last_date_ori

#this is for reporting so needs to be human readable
groupdat$best_date_in_group = as.Date(groupdat$best_date_in_group, origin="1970-01-01")
groupdat$worst_date_in_group = as.Date(groupdat$worst_date_in_group , origin="1970-01-01")


#scale
groupdat$best_date = scale(as.numeric(groupdat$best_date))
groupdat$worst_date= scale(as.numeric(groupdat$worst_date))
groupdat$best_worst_dates = groupdat$best_date - groupdat$worst_date


if(max(groupdat$Final.repaid ) < 5){
  groupdat$Final.repaid = groupdat$Final.repaid  * 100.
}


groupdat$default  = groupdat$Final.repaid 
groupdat$default[groupdat$default < 100] = 1
groupdat$default[groupdat$default >= 100] = 0


print(paste("===Completion rate RATE FOR", year_in, "=", 1-mean(as.numeric(groupdat$default))))
#print(summary(groupdat$Final.repaid))


#groupdat$default = NULL

# print("   Normalising data")
# 
# groupdat$first_date = scale(as.numeric(groupdat$first_date))
# groupdat$last_date =  scale(as.numeric(groupdat$last_date))
# groupdat$first.last.date = scale(as.numeric(groupdat$first.last.date))
# groupdat$best_date = scale(as.numeric(groupdat$best_date))
# groupdat$worst_date= scale(as.numeric(groupdat$worst_date))
# groupdat$best_worst_dates = groupdat$best_date - groupdat$worst_date
# 
# colnames(groupdat)



rm(grp)
rm(dat2)
rm(shortdat)
rm(cutdat)

groupdat = as.data.frame(groupdat)

print(paste(x, "Individuals found"))
print(paste(dim(groupdat)[1], "groups found"))
#print(summary(groupdat$grp.size))
Sys.sleep(5)



#clean up group level data
groupdat$cut.total.repaid[groupdat$cut.total.repaid == Inf] = NA

setTxtProgressBar(pb, 9)
#path_outy = "C:/Users/Michael/OneDrive/OAF/MB/Projects/Roster_data/Rwanda"

colnames(groupdat)
if(1==0){
  #devmode
  path_outy = merged_path
  }


pathy_outy = paste(path_outy,year_in,sep="/")
dir.create(pathy_outy, showWarnings = FALSE, recursive = TRUE)

#final write
temp = Filter(function(x)!all(is.na(x)), temp)
groupdat = Filter(function(x)!all(is.na(x)), groupdat)
indout = paste(pathy_outy,"/indivdat_merged",year_in,".csv",sep="") # data on individuals
grpout = paste(pathy_outy, "/groupdat_merged",year_in,".csv",sep="") # data on groups

print("writing...")
print(indout)
print(grpout)


temp$year = year_in
groupdat$year = year_in

write.csv(temp, file=indout,row.names = FALSE)

write.csv(groupdat, grpout,row.names = FALSE)


print(indout)
print(grpout)

setTxtProgressBar(pb, 10)
print(paste("Remaining feature:",colnames(groupdat) ) )
print(paste("Cleaning done", year_in))
#paste("time taken=", round(Sys.time()-axxxx),"sec")

}


