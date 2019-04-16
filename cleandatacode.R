## read in data ##
#select
library(dplyr)
library(plyr)

require("RPostgreSQL")
library(RPostgreSQL)
require(ggplot2)
library(anchors)
require(caret)
library(caret)
library(corrplot)
library(MASS)
library(car)
library(leaps)
library(bestglm)
library(compare)
library(dplyr)
library("R.utils")

#good values are integer's, of 2, 3, 5 (5% training sample size, anda 5% holdout sample size per analysis)
#1% passes result in too low of a pass and give overfitted coefficient terms which result in too large of a sample for the 2nd holdout iteration.
#therefore a minimum of 1.25% is recommended, but to hard code that here... would be wonky.  So sticking to simply integer 

widthDiviser = 2

sub_returnCVNames <- function(data_sent){
  holderOfData <- cbind(data.frame(data_sent[,-1 , drop = FALSE]),data.frame(data_sent[,1 , drop = FALSE]))
  
  if ( widthDiviser == 1 )  B <- suppressMessages(bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=2, REP=widthDiviser, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive"))
  if (!(widthDiviser == 1 )) B <- suppressMessages(bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=widthDiviser, REP=widthDiviser, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive"))
  
  set<-round(colSums(B$Subsets))[-1]
  
  if(!is.null(B$Subsets))
  {
    cverrs = B$Subsets[, "CV"]
    sdCV = B$Subsets[, "sdCV"]
    CVLo = cverrs - sdCV
    CVHi = cverrs + sdCV
    ymax = max(CVHi)
    ymin = min(CVLo)
    k = 0:(length(cverrs) - 1)
    if(!(ymax=="Inf" || ymax=="-Inf")) plot(k, cverrs, ylim = c(ymin, ymax), type = "n", yaxt = "n")
    points(k,cverrs,cex = 2,col="red",pch=16)
    lines(k, cverrs, col = "red", lwd = 2)
    axis(2, yaxp = c(0.6, 1.8, 6))
    segments(k, CVLo, k, CVHi,col="blue", lwd = 2)
    eps = 0.15
    segments(k-eps, CVLo, k+eps, CVLo, col = "blue", lwd = 2)
    segments(k-eps, CVHi, k+eps, CVHi, col = "blue", lwd = 2)
    indMin = which.min(cverrs)
    fmin = sdCV[indMin]
    cutOff = fmin + cverrs[indMin]
    abline(h = cutOff, lty = 2)
    indMin = which.min(cverrs)
    fmin = sdCV[indMin]
    cutOff = fmin + cverrs[indMin]
    min(which(cverrs < cutOff))
  }
  
  left=length(set)-3
  result <- set[1:left]
  
  #aboveMedianCV <- as.character(rownames(data.frame(which(result >= median(result)))))
  return(as.character(rownames(data.frame(which(result >= median(result))))))
}

sub_returnCVNamesExclMin <- function(data_sent){
  holderOfData <- cbind(data.frame(data_sent[,-1 , drop = FALSE]),data.frame(data_sent[,1 , drop = FALSE]))
  
  if ( widthDiviser == 1 )  B <- suppressMessages(bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=2, REP=widthDiviser, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive"))
  if (! (widthDiviser == 1 )) B <- suppressMessages(bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=widthDiviser, REP=widthDiviser, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive"))
  
  set<-round(colSums(B$Subsets))[-1]
  
  if(!is.null(B$Subsets))
  {
    cverrs = B$Subsets[, "CV"]
    sdCV = B$Subsets[, "sdCV"]
    CVLo = cverrs - sdCV
    CVHi = cverrs + sdCV
    ymax = max(CVHi)
    ymin = min(CVLo)
    k = 0:(length(cverrs) - 1)
    if(!(ymax=="Inf" || ymax=="-Inf")) plot(k, cverrs, ylim = c(ymin, ymax), type = "n", yaxt = "n")
    points(k,cverrs,cex = 2,col="red",pch=16)
    lines(k, cverrs, col = "red", lwd = 2)
    axis(2, yaxp = c(0.6, 1.8, 6))
    segments(k, CVLo, k, CVHi,col="blue", lwd = 2)
    eps = 0.15
    segments(k-eps, CVLo, k+eps, CVLo, col = "blue", lwd = 2)
    segments(k-eps, CVHi, k+eps, CVHi, col = "blue", lwd = 2)
    indMin = which.min(cverrs)
    fmin = sdCV[indMin]
    cutOff = fmin + cverrs[indMin]
    abline(h = cutOff, lty = 2)
    indMin = which.min(cverrs)
    fmin = sdCV[indMin]
    cutOff = fmin + cverrs[indMin]
    min(which(cverrs < cutOff))
  }
  
  left=length(set)-3
  result <- set[1:left]
  
  #aboveMedianCV <- as.character(rownames(data.frame(which(result >= median(result)))))
  return(as.character(rownames(data.frame(which(result > min(result))))))
}

pw <- {"Read1234"}

#sourceDir="/home/rstudio/577/Capstone-577/"
sourceDir="C:/Users/user/Documents/School/CSUF/ISDS577/projects/Capstone-577/"
source(paste0(sourceDir,"bestglm.R"))
# Read CSV into R

d_2012 <- read.csv(paste0(sourceDir,"34574-0001-Data.csv"), header=TRUE, sep=",")
d_2013 <- read.csv(paste0(sourceDir,"34574-0001-Data.csv"), header=TRUE, sep=",")
d_2014 <- read.csv(paste0(sourceDir,"36149-0001-Data.csv"), header=TRUE, sep=",")
d_2015 <- read.csv(paste0(sourceDir,"36407-0001-Data.csv"), header=TRUE, sep=",")
d_2016 <- read.csv(paste0(sourceDir,"36799-0001-Data.csv"), header=TRUE, sep=",")
d_2017 <- read.csv(paste0(sourceDir,"37183-0001-Data.csv"), header=TRUE, sep=",")

d_combined <- rbind.fill(d_2012,d_2013,d_2014,d_2015,d_2016,d_2017)

na_count <-function (x) sapply(x, function(y) sum(is.na(y)))

#View(na_count(d_combined))

#summary(d_combined)

#write.csv(d_combined, file = "MyData.csv")
#data<-read.csv(paste0(sourceDir,"MyData.csv"),sep=",",quote="\"")

#write.csv(d_combined,paste0(sourceDir,"combined.csv"))
#data <- read.csv(paste0(sourceDir,"combined.csv"), header=TRUE, sep=,)

data <- d_combined

suppressWarnings(system(paste0('rm -f ',sourceDir,'/output/*.csv'), intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE, wait = TRUE, input = NULL, show.output.on.console = TRUE, minimized = FALSE, invisible = TRUE, timeout = 0))

#lister=1
for(lister in 1:1)
{
  #7221 gpa
  if (lister==1) list<-read.csv(paste0(sourceDir,"altList.txt"), header=FALSE, sep=,)
  
  #8517 gang
  if (lister==2) list<-read.csv(paste0(sourceDir,"gangfight.txt"), header=FALSE, sep=,)
  
  #7118 (psychadelics)
  if (lister==3) list<-read.csv(paste0(sourceDir,"reducedFilterList.txt"), header=FALSE, sep=,)
  
  # dim(data)
  # check missing with for loop
  # The below code gives the number of missing values for each variables
  
  #expensive, descriptive only
  #for (ii in 1:ncol(data)) {
  #  print( colnames(data)[ii] )
  #  print( table(is.na(data[,ii])) )
  #}
  ## select the columns with no 
  
  #drops columns with na values
  cleandata<-data[,colSums(is.na(data)) == 0] # dat[A, B] takes the A rows and B columns; A and B are indices; 
  # if A or B is not specified, all rows or columns will be retained
  
  #expensive, descriptive function only
  #table(is.na(cleandata))# table(is.na(cleandata)) gives the number of missing values of data
  #Since there are no missing values we export the data
  #write.csv(cleandata, "C:\\Users\\CampusUser\\Desktop\\MyData.csv")
  
  colnames(data)
  
  #https://stackoverflow.com/questions/27556353/subset-columns-based-on-list-of-column-names-and-bring-the-column-before-it
  
  #load from filterlist.txt
  col.num <- which(colnames(data) %in% as.character(list[,1]))
  #col2.num <- which(colnames(cleandata) %in% as.character("V7105D"))
  
  # loads the PostgreSQL driver
  #pg <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  # note that "con" will be used later in each connection to the database
  #conn = dbConnect(drv=pg
  #             ,user="postgres"
  #              ,password="Read1234"
  #               ,host="localhost"
  #                ,port=5432
  #                 ,dbname="analyticplatform"
  #)
  
  #dbExistsTable(conn, "temp_table_data")
  
  NewDF <- data[,(c(col.num))]
  
  #V7589 empty
  
  #https://stat.ethz.ch/R-manual/R-devel/library/base/html/system.html
  #https://stackoverflow.com/questions/32015333/executing-a-batch-file-in-an-r-script
  #shell.exec("\\\\network\\path\\file.bat")
  #db_drop_table(conn, "temp_table_data", force = TRUE)
  
  #https://stackoverflow.com/questions/12797909/creating-temp-table-from-a-data-frame-in-r-using-rpostgresql
  #dbWriteTable(conn, "temp_table_data", NewDF, temp.table=TRUE)
  
  #https://www.r-bloggers.com/getting-started-with-postgresql-in-r/
  #df_postgres <- dbGetQuery(conn, "SELECT * from temp_table_data")
  
  #identical(NewDF, df_postgres)
  
  #v508 was dropped or v8528
  
  #boxplot(NewDF)
  #summary(NewDF)
  #View(na_count(NewDF))
  
  #table(is.na(NewDF))
  #write.csv(NewDF,paste0(sourceDir,"filtered.csv"))
  
  #colnames(NewDF)
  
  summary(NewDF)
  nrow(NewDF)
  
  #list[,2]
  
  NewDF <- data[,(c(col.num))]
  
  library(dplyr) 
  
  length(colnames(NewDF))
  
  #transformations
  #https://stackoverflow.com/questions/8214303/conditional-replacement-of-values-in-a-data-frame
  #index <- df$b == 0
  #df$est[index] <- (df$a[index] - 5)/2.533 
  convert1Index <- list[,2] == 1
  convert2Index <- list[,2] == 2
  convert3Index <- list[,2] == 3
  #list[,1][convert1Index]
  
  #male to female
  
  NewDF <- replace.value( NewDF, colnames(NewDF), from=as.integer(-9), to=as.double(0), verbose = FALSE)
  NewDF <- replace.value( NewDF, colnames(NewDF), from=as.integer(-8), to=as.double(0), verbose = FALSE)
  NewDF <- replace.value( NewDF, as.character(list[,1][convert1Index]), from=as.integer(1), to=as.double(-1), verbose = FALSE)
  NewDF <- replace.value( NewDF, as.character(list[,1][convert1Index]), from=as.integer(2), to=as.double(1), verbose = FALSE)
  NewDF <- replace.value( NewDF, as.character(list[,1][convert1Index]), from=as.integer(3), to=as.double(1), verbose = FALSE)
  NewDF <- replace.value( NewDF, as.character(list[,1][convert1Index]), from=as.integer(4), to=as.double(1), verbose = FALSE)
  NewDF <- replace.value( NewDF, as.character(list[,1][convert1Index]), from=as.integer(5), to=as.double(1), verbose = FALSE)
  NewDF <- replace.value( NewDF, as.character(list[,1][convert1Index]), from=as.integer(6), to=as.double(1), verbose = FALSE)
  NewDF <- replace.value( NewDF, as.character(list[,1][convert1Index]), from=as.integer(7), to=as.double(1), verbose = FALSE)
  
  #https://stackoverflow.com/questions/24237801/calculate-mean-median-by-excluding-any-given-number
  #https://stackoverflow.com/questions/5824173/replace-a-value-in-a-data-frame-based-on-a-conditional-if-statement?rq=1
  
  NewDF <- replace.value( NewDF, "V7202", from=as.integer(1), to=as.double(-1), verbose = FALSE)
  NewDF <- replace.value( NewDF, "V7202", from=as.integer(2), to=as.double(1), verbose = FALSE)
  
  #https://www.ucl.ac.uk/child-health/short-courses-events/about-statistical-courses/research-methods-and-statistics/chapter-8-content-8
  #95% confidence
  #7: B+
  #95% conf confirmed
  V7221_Index <- NewDF[,"V7221"] >= median(NewDF[,"V7221"][NewDF[,"V7221"]>0])
  centerpoint = (length(NewDF[,"V7221"][NewDF[,"V7221"]>0]))/2
  width = round(1.96*sqrt((length(NewDF[,"V7221"][NewDF[,"V7221"]>0])))/2)
  lower = (length(NewDF[,"V7221"][NewDF[,"V7221"]>0]))/2 - width
  upper = (length(NewDF[,"V7221"][NewDF[,"V7221"]>0]))/2 + width
  sort(((NewDF[,"V7221"][NewDF[,"V7221"]>0])))[lower]
  sort(((NewDF[,"V7221"][NewDF[,"V7221"]>0])))[upper]
  
  NewDF[V7221_Index,"V7221"] <- 1
  V7221_Index <- NewDF[,"V7221"] > 1
  NewDF[V7221_Index,"V7221"] <- 0
  
  #College graduate
  #5: for college grad father, 5% conf confirmed
  V7215_Index <- NewDF[,"V7215"] >= median(NewDF[,"V7215"][NewDF[,"V7215"]>0])
  centerpoint = (length(NewDF[,"V7215"][NewDF[,"V7215"]>0]))/2
  width = round(1.96*sqrt((length(NewDF[,"V7215"][NewDF[,"V7215"]>0])))/2)
  lower = (length(NewDF[,"V7215"][NewDF[,"V7215"]>0]))/2 - width
  upper = (length(NewDF[,"V7215"][NewDF[,"V7215"]>0]))/2 + width
  sort(((NewDF[,"V7215"][NewDF[,"V7215"]>0])))[lower]
  sort(((NewDF[,"V7215"][NewDF[,"V7215"]>0])))[upper]
  
  NewDF[V7215_Index,"V7215"] <- 1
  V7215_Index <- NewDF[,"V7215"] > 1
  NewDF[V7215_Index,"V7215"] <- 0
  
  #4: 3-5 Hours Internet
  #4 #hours for computer use for internet leisure
  #95 conf confirmed
  
  V7551_Index <- NewDF[,"V7551"] >= median(NewDF[,"V7551"][NewDF[,"V7551"]>0])
  centerpoint = (length(NewDF[,"V7551"][NewDF[,"V7551"]>0]))/2
  width = round(1.96*sqrt((length(NewDF[,"V7551"][NewDF[,"V7551"]>0])))/2)
  lower = (length(NewDF[,"V7551"][NewDF[,"V7551"]>0]))/2 - width
  upper = (length(NewDF[,"V7551"][NewDF[,"V7551"]>0]))/2 + width
  sort(((NewDF[,"V7551"][NewDF[,"V7551"]>0])))[lower]
  sort(((NewDF[,"V7551"][NewDF[,"V7551"]>0])))[upper]
  
  unique(NewDF[,"V7551"])
  NewDF[V7551_Index,"V7551"] <- 1
  V7551_Index <- NewDF[,"V7551"] > 1
  NewDF[V7551_Index,"V7551"] <- 0
  
  #5: 6-9 Hours Facebook
  # 95% conf confirmed
  V7552_Index <- NewDF[,"V7552"] >= median(NewDF[,"V7552"][NewDF[,"V7552"]>0])
  centerpoint = (length(NewDF[,"V7552"][NewDF[,"V7552"]>0]))/2
  width = round(1.96*sqrt((length(NewDF[,"V7552"][NewDF[,"V7552"]>0])))/2)
  lower = (length(NewDF[,"V7552"][NewDF[,"V7552"]>0]))/2 - width
  upper = (length(NewDF[,"V7551"][NewDF[,"V7552"]>0]))/2 + width
  sort(((NewDF[,"V7552"][NewDF[,"V7552"]>0])))[lower]
  sort(((NewDF[,"V7552"][NewDF[,"V7552"]>0])))[upper]
  
  NewDF[V7552_Index,"V7552"] <- 1
  V7552_Index <- NewDF[,"V7552"] > 1
  NewDF[V7552_Index,"V7552"] <- 0
  
  #4 3-5 Hours Gaming
  # 95% conf confirmed
  V7553_Index <- NewDF[,"V7553"] >= median(NewDF[,"V7553"][NewDF[,"V7553"]>0])
  centerpoint = (length(NewDF[,"V7553"][NewDF[,"V7553"]>0]))/2
  width = round(1.96*sqrt((length(NewDF[,"V7553"][NewDF[,"V7553"]>0])))/2)
  lower = (length(NewDF[,"V7553"][NewDF[,"V7553"]>0]))/2 - width
  upper = (length(NewDF[,"V7553"][NewDF[,"V7553"]>0]))/2 + width
  sort(((NewDF[,"V7553"][NewDF[,"V7553"]>0])))[lower]
  sort(((NewDF[,"V7553"][NewDF[,"V7553"]>0])))[upper]
  
  NewDF[V7553_Index,"V7553"] <- 1
  V7553_Index <- NewDF[,"V7553"] > 1
  NewDF[V7553_Index,"V7553"] <- 0
  
  #4 3-5 Hours Texting
  # 95% conf confirmed
  V7562_Index <- NewDF[,"V7562"] >= median(NewDF[,"V7562"][NewDF[,"V7562"]>0])
  centerpoint = (length(NewDF[,"V7562"][NewDF[,"V7562"]>0]))/2
  width = round(1.96*sqrt((length(NewDF[,"V7562"][NewDF[,"V7562"]>0])))/2)
  lower = (length(NewDF[,"V7562"][NewDF[,"V7562"]>0]))/2 - width
  upper = (length(NewDF[,"V7562"][NewDF[,"V7562"]>0]))/2 + width
  sort(((NewDF[,"V7562"][NewDF[,"V7562"]>0])))[lower]
  sort(((NewDF[,"V7562"][NewDF[,"V7562"]>0])))[upper]
  
  NewDF[V7562_Index,"V7562"] <- 1
  V7562_Index <- NewDF[,"V7562"] > 1
  NewDF[V7562_Index,"V7562"] <- 0
  
  #2: <1 Hour talking on cell phone
  # 95% conf confirmed
  V7563_Index <- NewDF[,"V7563"] >= median(NewDF[,"V7563"][NewDF[,"V7563"]>0])
  centerpoint = (length(NewDF[,"V7563"][NewDF[,"V7563"]>0]))/2
  width = round(1.96*sqrt((length(NewDF[,"V7563"][NewDF[,"V7563"]>0])))/2)
  lower = (length(NewDF[,"V7562"][NewDF[,"V7563"]>0]))/2 - width
  upper = (length(NewDF[,"V7563"][NewDF[,"V7563"]>0]))/2 + width
  sort(((NewDF[,"V7563"][NewDF[,"V7563"]>0])))[lower]
  sort(((NewDF[,"V7563"][NewDF[,"V7563"]>0])))[upper]
  
  NewDF[V7563_Index,"V7563"] <- 1
  V7563_Index <- NewDF[,"V7563"] > 1
  NewDF[V7563_Index,"V7563"] <- 0
  
  NewDF <- replace.value( NewDF, as.character(list[,1][convert3Index]), from=as.integer(1), to=as.double(0), verbose = FALSE)
  NewDF <- replace.value( NewDF, as.character(list[,1][convert3Index]), from=as.integer(2), to=as.double(0), verbose = FALSE)
  NewDF <- replace.value( NewDF, as.character(list[,1][convert3Index]), from=as.integer(3), to=as.double(0), verbose = FALSE)
  NewDF <- replace.value( NewDF, as.character(list[,1][convert3Index]), from=as.integer(4), to=as.double(1), verbose = FALSE)
  NewDF <- replace.value( NewDF, as.character(list[,1][convert3Index]), from=as.integer(5), to=as.double(1), verbose = FALSE)
  NewDF <- replace.value( NewDF, as.character(list[,1][convert3Index]), from=as.integer(6), to=as.double(1), verbose = FALSE)
  
  #https://stackoverflow.com/questions/11036989/replace-all-0-values-to-na
  #kills the analysis
  NewDF[NewDF == -1] <- -2
  NewDF[NewDF == 0] <- -1
  NewDF[NewDF == -2] <- 0
  
  #0 = na
  #-1 = negative
  #1 = positve
  
  start=5
  
  #sets holdout resampling, monte carlo subset resampling, CV Passes, K Folds
  
  #resets each new file
  finalList <- c()
  
  #after lister, before holdoutReset
  
  if (widthDiviser == 1) end = (start+1)
  if (!(widthDiviser == 1)) end = (start+(widthDiviser-1))
  #seeder=start
  for (seeder in start:end)
  {
    
    seedbase=seeder
    print(paste("seed",seedbase))
    
    #holdoutReset=1    
    for (holdoutReset in 1:widthDiviser)
    {
      set.seed(seedbase)
      #setup holdout
      
      #static holdout
      holdoutSetSize = widthDiviser/100
      #holdoutSetSize = 1.25/100
      
      underOverSampleFactor=1
      
      #% to resample from resampled static hold out set
      holdoutSize = underOverSampleFactor/widthDiviser #(of set) #(never fully iterates over subsample)
      
      #proportion of nonHoldout (i.e. nonholdout: 1-holdoutSize) to use for model building, i.e. sample size.  Holdout can be tuned independently kind of.
      #preNonHoldOutSize = (1.25/100)/(1-holdoutSetSize) #forces it to be 5%, opposite is used for nonholdout
      preNonHoldOutSize = (widthDiviser/100)/(1-holdoutSetSize) #forces it to be 5%, opposite is used for nonholdout
      
      #% of training resamples from static nonholdout
      preTrainSize = underOverSampleFactor/widthDiviser # <1 = (never fully iterates over subsample)
      
      #static (outside of monte carlo/resampling, if desire resampling, simply move above set.seed(base))
      holdoutSet <- c()
      holdoutSet <- sample(nrow(NewDF), round(holdoutSetSize*nrow(NewDF)))
      
      NewDF.holdoutSet <- c()
      NewDF.holdoutSet <- NewDF[holdoutSet,]
      
      #static for monte carlo training 
      preNonHoldoutSet <- c()
      preNonHoldoutSet <- sample(nrow(NewDF[-holdoutSet,]), round(preNonHoldOutSize*nrow(NewDF[-holdoutSet,])))
      
      NewDF.preNonHoldoutSet <- c()
      NewDF.preNonHoldoutSet <- NewDF[-holdoutSet,][preNonHoldoutSet,]
      
      #monte carlo resample from static sets
      
      if (widthDiviser == 1) resample = 2
      if ((!widthDiviser == 1)) resample = widthDiviser
      
      #resample=1      
      for (resample in 1:widthDiviser)
      {
        base = resample
        #print is inside inner loop
        
        ##before reseed
        #https://adv-r.hadley.nz/subsetting.html
        
        #monte carlo resample from pre separated holdout (this means new holdout each subsample)
        holdout <- c()
        holdout <- sample(nrow(NewDF.holdoutSet), round(holdoutSize*nrow(NewDF.holdoutSet)))
        
        NewDF.holdout <- c()
        NewDF.holdout <- NewDF.holdoutSet[holdout, ]
        
        #taken from a "static" nonHoldoutSet (i.e. excluded from monte carlo)
        #monte carlo resamples from a static holdout
        #used for resampling monte carlo training set from non holdout partitions!
        preTrain <- c()
        preTrain <- sample(nrow(NewDF.preNonHoldoutSet), round(preTrainSize*nrow(NewDF.preNonHoldoutSet)))
        
        NewDF.preTrain <- c()
        NewDF.preTrain <- NewDF.preNonHoldoutSet[preTrain,]
        
        yIndex <- list[,4] == 0
        lGeographyIndex <- list[,4] == 1
        lGenderIndex <- list[,4] == 2
        lGPAIndex <- list[,4] == 3
        lViolenceIndex <- list[,4] == 4
        lFather1Index <- list[,4] == 5
        lFather2Index <- list[,4] == 6
        lHabitsIndex <- list[,4] == 7
        lHealthIndex <- list[,4] == 8
        lPsycheIndex <- list[,4] == 9
        
        if (widthDiviser == 1) train.control <- trainControl(method = "repeatedcv", number = 2, repeats = widthDiviser)
        if (!(widthDiviser == 1)) train.control <- trainControl(method = "repeatedcv", number = 2, repeats = widthDiviser)
        
        y <- c()
        yname <- c()
        #y iterator's
        #iterator=1
        
        for (iterator in 1:sum(yIndex))
        {
          
          #I know I have lister, but at one time I had multiple y's before I was utilizing lister...  so that's why there is a y iterator here
          yname <- c()
          yname <- as.character(list[yIndex,][iterator,][,1])
          
          y <- c()
          y <- list[yIndex,][iterator,]
          
          alty <- c()
          alty <- list[yIndex,][-iterator,]
          #y
          #yname <- as.character(list[yIndex,][iterator,][,1])
          #rather than move to end of file
          if (iterator==1 && resample==1 && holdoutReset==1 && seeder==start) print(paste("Y:",as.character(list[yIndex,][iterator,][,1])))
          
          #aggregated after categories loop
          namesTV <- c()
          namesH <- c()
          
          print(paste("holdoutReset: ",holdoutReset,"resample: ",base))
          
          #doesn't resample unless I [re-]sample (function) an index... unsure if CV has an internal index.  I'm sure it is random each pass.
          #My assumption is the first CV is always a specific seed.  My hope is to have different seeds.
          
          #categories 
          #val=7
          for (val in 2:9)
          {
            #used in category, rolled into names
            datalist1 <- c()
            datalist2 <- c()
            
            #end up with no records due to na's, and so any variables.  Inverse relationship.
            colList <- c()
            if (val == 2) colList <- list[lGenderIndex,]
            if (val == 3) colList <- list[lGPAIndex,]
            if (val == 4) colList <- list[lViolenceIndex,]
            if (val == 5) colList <- list[lFather1Index,]
            if (val == 6) colList <- list[lFather2Index,]
            if (val == 7) colList <- list[lHabitsIndex,]
            if (val == 8) colList <- list[lHealthIndex,]
            if (val == 9) colList <- list[lPsycheIndex,]
            
            if (is.null(nrow(data.frame(alty)))) break
            
            #colList <- rbind(list[yIndex,],colList)
            
            colList <- rbind(y,colList)
            
            #https://stackoverflow.com/questions/17878048/merge-two-data-frames-while-keeping-the-original-row-order
            #https://stackoverflow.com/questions/28311293/how-to-make-join-operations-in-dplyr-silent
            colListNames <- c()
            colListNames <- suppressMessages(paste(join(colList,list)[,1],join(colList,list)[,3]))
            
            newList <- c()        
            newList <-  suppressMessages(as.character(join(colList,list[,c(1,3)])[,1, drop=TRUE]))
            
            #https://stat.ethz.ch/R-manual/R-devel/library/base/html/droplevels.html
            #droplevels(newList)
            #https://stackoverflow.com/questions/34469178/r-convert-factor-to-numeric-and-remove-levels
            
            #needs to be inside category when newList is generated
            #don't re-use for csv's... csv's... are already cleaned
            #repurpose instead
            #replaces 0 with na's (so it assumes data is already precleaned to just a NewDF level)
            source(paste0(sourceDir,"/resampleMC.R"))
            
            merged <- c()
            holderOfData <- c()
            merged <- rbind(data.train,data.test)
            
            #subcategory specific
            
            tryCase <- tryCatch((datalist1 <- suppressWarnings(sub_returnCVNames(merged))), 
                                error=function(e) datalist1 <- suppressWarnings(sub_returnCVNames(merged)))
            
            #https://www.r-bloggers.com/careful-with-trycatch/
            
            #print(table(is.na(data.test)))
            #datalist2 <- sub_returnCVNames(data.test)
            
            #only have to iterate here because the function sub_returCVNames aggregates, I'm merely aggregateing the list.
            #print(c(datalist1))
            #print(length(datalist1))
            if(length(datalist1)>1)
              for (i in 1:length(datalist1))
              {
                namesTV <- rbind(namesTV,datalist1[i])
              }
            if(length(datalist1)==1)
            {
              namesTV <- rbind(namesTV,datalist1)
            }
            
            #modified code: https://rdrr.io/cran/bestglm/src/R/bestglm.R to ignore p <15
            #https://rdrr.io/cran/bestglm/man/bestglm.html
            #http://ropatics.com/machine-learning/ml_-_Logistic_regression.html
            #https://rstudio-pubs-static.s3.amazonaws.com/2897_9220b21cfc0c43a396ff9abf122bb351.html
            #https://rdrr.io/cran/bestglm/man/bestglm-package.html
            
            #if(length(names)>0) for(h in 1:length(names)) {cv.names[k,names[h]]=names[h]}
            
            #summary(step.model.test) 
            
            #Calculating MSE for training data
            #mse.train<- mean(residuals(step.model.train)^2)
            #mse.train
            
            #Calculating RMSE for training data
            #rmse.train <- sqrt(mse.train)
            #rmse.train
            
            #Calculating MSE for testing data
            #mse.test <- mean(residuals(step.model.test)^2)
            #mse.test
            
            #Calculating RMSE for testing data
            #rmse.test <- sqrt(mse.test)
            #rmse.test
            
            #end of category iterator
          }
          #print("category pass")  
          #Taggregated <- c()
          Hfiltered <- c()
          extract <- c()
          
          #data.trainAggregate <- c()
          #data.trainAggregate <- NewDF.preTrain[,as.character(c(yname,namesTV)), drop=FALSE] %>% filter_all(all_vars(!is.na(.)))
          #data.trainAggregate[data.trainAggregate == 0] <- NA
          #data.trainAggregate <- data.trainAggregate %>% filter_all(all_vars(!is.na(.)))
          #data.trainAggregate[data.trainAggregate == -1] <- 0
          #print(table(is.na(data.trainAggregate)))
          
          #pass to test/holdout partition to filter and refine on another pass
          #Taggregated <- sub_returnCVNames(data.trainAggregate)
          print(c("1: ", namesTV))
          
          names <- c(yname,namesTV)
          #resample before drawing from data.test
          source(paste0(sourceDir,"/resampleMC.R"))
          
          merged <- c()
          holderOfData <- c()
          merged <- rbind(data.train,data.test)
          holderOfData <- cbind(data.frame(merged[,-1 , drop = FALSE]),data.frame(merged[,1 , drop = FALSE]))
          
          if ( widthDiviser == 1 )  testCase <- tryCatch((Hfiltered <- suppressMessages(bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=2, REP=widthDiviser, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive"))), 
                                                         error=function(e) Hfiltered <- suppressMessages(bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=2, REP=widthDiviser, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive")))
          
          if (!(widthDiviser == 1 )) testCase <- tryCatch((Hfiltered <- suppressMessages(bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=widthDiviser, REP=widthDiviser, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive"))), 
                                                          error=function(e) suppressMessages(bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=widthDiviser, REP=widthDiviser, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive")))
          
          extract <- row.names(data.frame(Hfiltered$BestModel[1]))[-1]
          
          print(c("2: ", extract))
          
          #if((iterator==1 && resample==1 && holdoutReset==1 && seeder==start)) finalList <- Hfiltered
          
          # #3 finalList
          #if(!(iterator==1 && resample==1 && holdoutReset==1 && seeder==start)) finalList <- Hfiltered[Hfiltered %in% finalList]
          
          #going to use table to tabulate final results
          
          #https://stackoverflow.com/questions/34324008/in-r-select-rows-that-have-one-column-that-exists-in-another-list
          #p5[p5$id %in% current, ]
          
          #end of yPass 
          
          if(length(extract)>1)
            for (i in 1:length(extract))
            {
              finalList <- rbind(finalList,extract[i])
            }
          if(length(extract)==1)
          {
            finalList <- rbind(finalList,extract)
          }          
          
          print(c("2a: ", table(finalList)))
        }
        
        #end of resample MC pass
      }
      #write.csv(filtered,paste0(sourceDir,yname,"hR-",holdoutReset,"rS-",resample,"filteredv1.csv"))
      #write.csv(filteredv2,paste0(sourceDir,yname,"hR-",holdoutReset,"rS-",resample,"filteredv2.csv"))
      #end outermost loop
      
      #end of holdoutReset 
    }
    
    #end seed
  }
  
  
  #spacer
  finalListReduced <- c()
  tabled <- table(finalList)
  print(tabled)
  finalListReduced <- c(as.character(row.names(data.frame(tabled[tabled >= quantile(tabled)[4]]))))
  print(c("3: ", finalListReduced))
  hist((data.frame(table(finalList)))[,2])
  
  #validate against population    
  #population
  
  filtered <- c()
  filtered <- NewDF[,as.character(c(yname,finalListReduced)), drop=FALSE] %>% filter_all(all_vars(!is.na(.)))
  filtered[filtered == 0] <- NA
  temp <- filtered[] %>% filter_all(all_vars(!is.na(.)))
  filtered <- temp
  filtered[filtered == -1] <- 0    
  trainModel <- suppressMessages(train(filtered[-1], as.factor(filtered[,1]),method = "glm",trControl = train.control))
  testModel <- suppressMessages(train(filtered[-1], as.factor(filtered[,1]), method = "glm",trControl = train.control))
  
  print("population")
  print(summary(trainModel$finalModel))
  
  write.csv(filtered,(paste0(sourceDir,"/output/",yname,"-",widthDiviser,"-","filtered.csv")))  
  
  #end of lister
}

source(paste0(sourceDir,"4thpass.R"))
