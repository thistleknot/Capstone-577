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
library(tidyr)


#good values are integer's, of 2, 3, 5 (5% training sample size, anda 5% holdout sample size per analysis)
#1% passes result in too low of a pass and give overfitted coefficient terms which result in too large of a sample for the 2nd holdout iteration.
#therefore a minimum of 1.25% is recommended, but to hard code that here... would be wonky.  So sticking to simply integer 

sub_returnCVNames <- function(data_sent){
  #data_sent=data.train
  holderOfData <- cbind(data.frame(data_sent[,-1 , drop = FALSE]),data.frame(data_sent[,1 , drop = FALSE]))
  #table(NewDF[,"V7202"])
  
  info <- which(colSums(holderOfData)==nrow(holderOfData))
  name <- rownames(data.frame(info))
  if(!length(info)==0) holderOfData <- holderOfData[, -which(names(holderOfData) == name)]
  
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

pw <- {"Read1234"}

#sourceDir="/home/rstudio/577/Capstone-577/"
sourceDir="C:/Users/user/Documents/School/CSUF/ISDS577/projects/Capstone-577/"
source(paste0(sourceDir,"bestglm.R"))
source(paste0(sourceDir,"pairedLists.R"))
# Read CSV into R

d_2012 <- read.csv(paste0(sourceDir,"34574-0001-Data.csv"), header=TRUE, sep=",")
d_2013 <- read.csv(paste0(sourceDir,"34574-0001-Data.csv"), header=TRUE, sep=",")
d_2014 <- read.csv(paste0(sourceDir,"36149-0001-Data.csv"), header=TRUE, sep=",")
d_2015 <- read.csv(paste0(sourceDir,"36407-0001-Data.csv"), header=TRUE, sep=",")
d_2016 <- read.csv(paste0(sourceDir,"36799-0001-Data.csv"), header=TRUE, sep=",")
d_2017 <- read.csv(paste0(sourceDir,"37183-0001-Data.csv"), header=TRUE, sep=",")

d_combined <- rbind.fill(d_2012,d_2013,d_2014,d_2015,d_2016,d_2017)

for (interests in c("V7221","V7215","V7551","V7552","V7553","V7562","V7563"))
{
  print(paste("interest:",interests))
  for(year in c("d_2012","d_2013","d_2014","d_2015","d_2016","d_2017","d_combined"))
  {
    print(paste("year:",year))
    print(paste("count:",sum (count(df[df>0]))))
    
    #https://stackoverflow.com/questions/28802652/access-variable-dataframe-in-r-loop
    df <- (get(year)[,interests])
    
    centerpoint = (length(df[df>0]))/2
    
    #print(centerpoint)
    width = round(1.96*sqrt((length(df[df>0])))/2)
    
    lower = (length(df[df>0]))/2 - width
    upper = (length(df[df>0]))/2 + width
    print(paste("lower:", sort(((df[df>0])))[lower]))
    print(paste("median:",median(df[df>0])))
    print(paste("upper:",sort(((df[df>0])))[upper]))
    
    print(round(table ( df[(df>0)] ) / sum (count(df[df>0])) ,4))
    
    #https://stackoverflow.com/questions/9317830/r-do-i-need-to-add-explicit-new-line-character-with-print
    writeLines("\n")
  } 
  writeLines("\n")
}

na_count <-function (x) sapply(x, function(y) sum(is.na(y)))

data <- d_combined
ncol(data)
#drops columns with na values

cleandata<-data[,colSums(is.na(data)) >= round(nrow(data)*.25,0)] # dat[A, B] takes the A rows and B columns; A and B are indices; 
ncol(cleandata)

colnames(cleandata)
# if A or B is not specified, all rows or columns will be retained

#expensive, descriptive function only
#table(is.na(cleandata))# table(is.na(cleandata)) gives the number of missing values of data
#Since there are no missing values we export the data
#write.csv(cleandata, "C:\\Users\\CampusUser\\Desktop\\MyData.csv")


suppressWarnings(system(paste0('rm -f ',sourceDir,'/output/*.csv'), intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE, wait = TRUE, input = NULL, show.output.on.console = TRUE, minimized = FALSE, invisible = TRUE, timeout = 0))

#for (medianDirection in c("greaterEqual","greater"))
#medianDirection = "greaterEqual"
for (medianDirection in c("greaterEqual"))
{
  
  #will error on 3 for V7118
  #widthLoop=3
  for(widthLoop in c(3))
  {
    widthDiviser = widthLoop
    print(paste0("widthLoopSize: ",widthLoop))
    
    #so if 3, has to exist in > 1.5 subsamples
    
    CVRuns_pct_threshold = 1/widthDiviser
    #this needs to be set in 4thpass as well
    
    #CVRuns_pct_threshold = .25
    #has to appear in half the samples of 1 width?
    #dangerously overfits
    #should be more than 1/widthDviser
    #CVRuns_pct_threshold = (1/widthDiviser)2
    
    #lister=1
    for(flister in 1:3)
    {
      numRuns = 1
      #7221 gpa
      if (flister==1) list<-read.csv(paste0(sourceDir,"altList.txt"), header=FALSE, sep=,)
      
      #8517 gang
      if (flister==2) list<-read.csv(paste0(sourceDir,"gangfight.txt"), header=FALSE, sep=,)
      
      #7118 (psychadelics)
      if (flister==3) list<-read.csv(paste0(sourceDir,"reducedFilterList.txt"), header=FALSE, sep=,)
      
      colnames(data)
      col.num <- which(colnames(data) %in% as.character(list[,1]))
      NewDF <- data[,(c(col.num))]
      
      summary(NewDF)
      nrow(NewDF)
      
      NewDF <- data[,(c(col.num))]
      
      length(colnames(NewDF))
      
      #transformations
      #https://stackoverflow.com/questions/8214303/conditional-replacement-of-values-in-a-data-frame
      #index <- df$b == 0
      #df$est[index] <- (df$a[index] - 5)/2.533 
      #conversion profile
      #anything 2+ = positive
      convert1Index <- list[,2] == 1
      #median (no profile applied)
      convert2Index <- list[,2] == 2
      #4 = mostly
      convert3Index <- list[,2] == 3
      #list[,1][convert1Index]
      
      #male to female
      #View(list[,1][convert2Index])
      
      NewDF <- replace.value( NewDF, colnames(NewDF), from=as.integer(-9), to=as.double(0), verbose = FALSE)
      NewDF <- replace.value( NewDF, colnames(NewDF), from=as.integer(-8), to=as.double(0), verbose = FALSE)
      NewDF <- replace.value( NewDF, as.character(list[,1][convert1Index]), from=as.integer(1), to=as.double(-1), verbose = FALSE)
      NewDF <- replace.value( NewDF, as.character(list[,1][convert1Index]), from=as.integer(2), to=as.double(1), verbose = FALSE)
      NewDF <- replace.value( NewDF, as.character(list[,1][convert1Index]), from=as.integer(3), to=as.double(1), verbose = FALSE)
      NewDF <- replace.value( NewDF, as.character(list[,1][convert1Index]), from=as.integer(4), to=as.double(1), verbose = FALSE)
      NewDF <- replace.value( NewDF, as.character(list[,1][convert1Index]), from=as.integer(5), to=as.double(1), verbose = FALSE)
      NewDF <- replace.value( NewDF, as.character(list[,1][convert1Index]), from=as.integer(6), to=as.double(1), verbose = FALSE)
      NewDF <- replace.value( NewDF, as.character(list[,1][convert1Index]), from=as.integer(7), to=as.double(1), verbose = FALSE)
      
      NewDF <- replace.value( NewDF, "V7202", from=as.integer(1), to=as.double(-1), verbose = FALSE)
      NewDF <- replace.value( NewDF, "V7202", from=as.integer(2), to=as.double(1), verbose = FALSE)
      
      #https://stackoverflow.com/questions/24237801/calculate-mean-median-by-excluding-any-given-number
      #https://stackoverflow.com/questions/5824173/replace-a-value-in-a-data-frame-based-on-a-conditional-if-statement?rq=1
      
      
      #https://www.ucl.ac.uk/child-health/short-courses-events/about-statistical-courses/research-methods-and-statistics/chapter-8-content-8
      #95% confidence
      #7: B+
      #95% conf confirmed
      if (medianDirection=="greaterEqual") V7221_Index <- NewDF[,"V7221"] >= median(NewDF[,"V7221"][NewDF[,"V7221"]>0])
      if (medianDirection=="greater") V7221_Index <- NewDF[,"V7221"] > median(NewDF[,"V7221"][NewDF[,"V7221"]>0])
      centerpoint = (length(NewDF[,"V7221"][NewDF[,"V7221"]>0]))/2
      width = round(1.96*sqrt((length(NewDF[,"V7221"][NewDF[,"V7221"]>0])))/2)
      lower = (length(NewDF[,"V7221"][NewDF[,"V7221"]>0]))/2 - width
      upper = (length(NewDF[,"V7221"][NewDF[,"V7221"]>0]))/2 + width
      sort(((NewDF[,"V7221"][NewDF[,"V7221"]>0])))[lower]
      sort(((NewDF[,"V7221"][NewDF[,"V7221"]>0])))[upper]
      
      NewDF[V7221_Index,"V7221"] <- 1
      V7221_Index <- NewDF[,"V7221"] > 1
      NewDF[V7221_Index,"V7221"] <- -1
      
      #College graduate
      #5: for college grad father, 95% conf confirmed
      if (medianDirection=="greaterEqual") V7215_Index <- NewDF[,"V7215"] >= median(NewDF[,"V7215"][NewDF[,"V7215"]>0])
      if (medianDirection=="greater") V7215_Index <- NewDF[,"V7215"] > median(NewDF[,"V7215"][NewDF[,"V7215"]>0])
      centerpoint = (length(NewDF[,"V7215"][NewDF[,"V7215"]>0]))/2
      width = round(1.96*sqrt((length(NewDF[,"V7215"][NewDF[,"V7215"]>0])))/2)
      lower = (length(NewDF[,"V7215"][NewDF[,"V7215"]>0]))/2 - width
      upper = (length(NewDF[,"V7215"][NewDF[,"V7215"]>0]))/2 + width
      sort(((NewDF[,"V7215"][NewDF[,"V7215"]>0])))[lower]
      sort(((NewDF[,"V7215"][NewDF[,"V7215"]>0])))[upper]
      
      NewDF[V7215_Index,"V7215"] <- 1
      V7215_Index <- NewDF[,"V7215"] > 1
      NewDF[V7215_Index,"V7215"] <- -1
      
      #4: 3-5 Hours Internet #95% conf confirmed
      #4 #hours for computer use for internet leisure 
      
      if (medianDirection=="greaterEqual") V7551_Index <- NewDF[,"V7551"] >= median(NewDF[,"V7551"][NewDF[,"V7551"]>0])
      if (medianDirection=="greater") V7551_Index <- NewDF[,"V7551"] > median(NewDF[,"V7551"][NewDF[,"V7551"]>0])
      centerpoint = (length(NewDF[,"V7551"][NewDF[,"V7551"]>0]))/2
      width = round(1.96*sqrt((length(NewDF[,"V7551"][NewDF[,"V7551"]>0])))/2)
      lower = (length(NewDF[,"V7551"][NewDF[,"V7551"]>0]))/2 - width
      upper = (length(NewDF[,"V7551"][NewDF[,"V7551"]>0]))/2 + width
      sort(((NewDF[,"V7551"][NewDF[,"V7551"]>0])))[lower]
      sort(((NewDF[,"V7551"][NewDF[,"V7551"]>0])))[upper]
      
      unique(NewDF[,"V7551"])
      NewDF[V7551_Index,"V7551"] <- 1
      V7551_Index <- NewDF[,"V7551"] > 1
      NewDF[V7551_Index,"V7551"] <- -1
      
      #5: 6-9 Hours Facebook # 95% conf confirmed
      if (medianDirection=="greaterEqual") V7552_Index <- NewDF[,"V7552"] >= median(NewDF[,"V7552"][NewDF[,"V7552"]>0])
      if (medianDirection=="greater") V7552_Index <- NewDF[,"V7552"] > median(NewDF[,"V7552"][NewDF[,"V7552"]>0])
      centerpoint = (length(NewDF[,"V7552"][NewDF[,"V7552"]>0]))/2
      width = round(1.96*sqrt((length(NewDF[,"V7552"][NewDF[,"V7552"]>0])))/2)
      lower = (length(NewDF[,"V7552"][NewDF[,"V7552"]>0]))/2 - width
      upper = (length(NewDF[,"V7551"][NewDF[,"V7552"]>0]))/2 + width
      sort(((NewDF[,"V7552"][NewDF[,"V7552"]>0])))[lower]
      sort(((NewDF[,"V7552"][NewDF[,"V7552"]>0])))[upper]
      
      NewDF[V7552_Index,"V7552"] <- 1
      V7552_Index <- NewDF[,"V7552"] > 1
      NewDF[V7552_Index,"V7552"] <- -1
      
      #4 3-5 Hours Gaming # 95% conf confirmed
      if (medianDirection=="greaterEqual") V7553_Index <- NewDF[,"V7553"] >= median(NewDF[,"V7553"][NewDF[,"V7553"]>0])
      if (medianDirection=="greater") V7553_Index <- NewDF[,"V7553"] > median(NewDF[,"V7553"][NewDF[,"V7553"]>0])
      centerpoint = (length(NewDF[,"V7553"][NewDF[,"V7553"]>0]))/2
      width = round(1.96*sqrt((length(NewDF[,"V7553"][NewDF[,"V7553"]>0])))/2)
      lower = (length(NewDF[,"V7553"][NewDF[,"V7553"]>0]))/2 - width
      upper = (length(NewDF[,"V7553"][NewDF[,"V7553"]>0]))/2 + width
      sort(((NewDF[,"V7553"][NewDF[,"V7553"]>0])))[lower]
      sort(((NewDF[,"V7553"][NewDF[,"V7553"]>0])))[upper]
      
      NewDF[V7553_Index,"V7553"] <- 1
      V7553_Index <- NewDF[,"V7553"] > 1
      NewDF[V7553_Index,"V7553"] <- -1
      
      #4 3-5 Hours Texting # 95% conf confirmed
      if (medianDirection=="greaterEqual") V7562_Index <- NewDF[,"V7562"] >= median(NewDF[,"V7562"][NewDF[,"V7562"]>0])
      if (medianDirection=="greater") V7562_Index <- NewDF[,"V7562"] > median(NewDF[,"V7562"][NewDF[,"V7562"]>0])
      centerpoint = (length(NewDF[,"V7562"][NewDF[,"V7562"]>0]))/2
      width = round(1.96*sqrt((length(NewDF[,"V7562"][NewDF[,"V7562"]>0])))/2)
      lower = (length(NewDF[,"V7562"][NewDF[,"V7562"]>0]))/2 - width
      upper = (length(NewDF[,"V7562"][NewDF[,"V7562"]>0]))/2 + width
      sort(((NewDF[,"V7562"][NewDF[,"V7562"]>0])))[lower]
      sort(((NewDF[,"V7562"][NewDF[,"V7562"]>0])))[upper]
      
      NewDF[V7562_Index,"V7562"] <- 1
      V7562_Index <- NewDF[,"V7562"] > 1
      NewDF[V7562_Index,"V7562"] <- -1
      
      #2: <1 Hour talking on cell phone # 95% conf confirmed
      if (medianDirection=="greaterEqual") V7563_Index <- NewDF[,"V7563"] >= median(NewDF[,"V7563"][NewDF[,"V7563"]>0])
      if (medianDirection=="greater") V7563_Index <- NewDF[,"V7563"] > median(NewDF[,"V7563"][NewDF[,"V7563"]>0])
      centerpoint = (length(NewDF[,"V7563"][NewDF[,"V7563"]>0]))/2
      width = round(1.96*sqrt((length(NewDF[,"V7563"][NewDF[,"V7563"]>0])))/2)
      lower = (length(NewDF[,"V7562"][NewDF[,"V7563"]>0]))/2 - width
      upper = (length(NewDF[,"V7563"][NewDF[,"V7563"]>0]))/2 + width
      sort(((NewDF[,"V7563"][NewDF[,"V7563"]>0])))[lower]
      sort(((NewDF[,"V7563"][NewDF[,"V7563"]>0])))[upper]
      
      NewDF[V7563_Index,"V7563"] <- 1
      V7563_Index <- NewDF[,"V7563"] > 1
      NewDF[V7563_Index,"V7563"] <- -1
      
      NewDF <- replace.value( NewDF, as.character(list[,1][convert3Index]), from=as.integer(1), to=as.double(-1), verbose = FALSE)
      NewDF <- replace.value( NewDF, as.character(list[,1][convert3Index]), from=as.integer(2), to=as.double(-1), verbose = FALSE)
      NewDF <- replace.value( NewDF, as.character(list[,1][convert3Index]), from=as.integer(3), to=as.double(-1), verbose = FALSE)
      NewDF <- replace.value( NewDF, as.character(list[,1][convert3Index]), from=as.integer(4), to=as.double(1), verbose = FALSE)
      NewDF <- replace.value( NewDF, as.character(list[,1][convert3Index]), from=as.integer(5), to=as.double(1), verbose = FALSE)
      NewDF <- replace.value( NewDF, as.character(list[,1][convert3Index]), from=as.integer(6), to=as.double(1), verbose = FALSE)
    
      NewDF[NewDF == 0] <- -8
      
      NewDF <- replace.value( NewDF, colnames(NewDF), from=as.integer(-9), to=as.double(0), verbose = FALSE)
      NewDF <- replace.value( NewDF, colnames(NewDF), from=as.integer(-8), to=as.double(0), verbose = FALSE)
      
      {
        NewDF[NewDF == 0] <- NA
        
        #cleandata<-NewDF[,colSums(is.na(NewDF)) >= round(nrow(NewDF)*.50,0)] # dat[A, B] takes the A rows and B columns; A and B are indices;
        
        #remove problematic columns (>75% na's)
        filterList <- c()
        rows = nrow(NewDF)
        #lister=4
        for (lister in 1:ncol(NewDF))
        {
          name <- colnames(NewDF)[lister]
          print(name)
          temp <- table(NewDF[lister], useNA = "ifany")
          percent <- temp[length(temp)]/rows
          print(percent)
          if(percent>.75)
          {
            removedName <- c()
            removedName <- name
            filterList <- rbind(filterList,removedName)
          }
        }
        print(c(filterList))
        NewDF[is.na(NewDF)] <- 0
        
        oldDF <- c()
        
        oldDF <- NewDF
        
        NewDF <- c()
        #store <- 
        #table(oldDF["V7501"], useNA = "ifany")
        
        #https://stackoverflow.com/questions/5234117/how-to-drop-columns-by-name-in-a-data-frame
        #https://www.listendata.com/2015/06/r-keep-drop-columns-from-data-frame.html
        
        #NewDF = subset(oldDF, select = -c(noquote(c(as.character(filterList)))) )
        
      }
      
      #https://stackoverflow.com/questions/18562680/replacing-nas-with-0s-in-r-dataframe
 
      #http://www.datasciencemadesimple.com/drop-variables-columns-r-using-dplyr/
      NewDF <- dplyr::select(oldDF,-c(as.character(filterList)))
      
      start=5
      
      #sets holdout resampling, monte carlo subset resampling, CV Passes, K Folds
      
      #resets each new file
      finalList <- c()
 
      if (widthDiviser == 1) end = (start+1)
      if ( (widthDiviser > 1) && (widthDiviser < 3) ) end = (start+(widthDiviser-1))
      if ( (widthDiviser > 2) ) end = start
      #seeder=start
      for (seeder in start:end)
      {
        
        set.seed(seeder)
        #seedbase=seeder
        print(paste("seed: ",seeder))
        
        holdoutResetEnd  <- c()
        
        if (widthDiviser == 1) holdoutResetEnd = 2
        if ( !(widthDiviser == 1) ) holdoutResetEnd = widthDiviser
        
        #holdoutReset=2   
        for (holdoutReset in 1:holdoutResetEnd)
        {
          print(paste0("holdoutReset: ",holdoutReset))
          #setup holdout
          
          #static holdout
          holdoutSetSize = widthDiviser^2/100
          #holdoutSetSize = 1.25/100
          
          underOverSampleFactor=1
          
          #% to resample from resampled static hold out set
          holdoutSize = underOverSampleFactor/widthDiviser #(of set) #(never fully iterates over subsample)
          
          #proportion of nonHoldout (i.e. nonholdout: 1-holdoutSize) to use for model building, i.e. sample size.  Holdout can be tuned independently kind of.
          #preNonHoldOutSize = (1.25/100)/(1-holdoutSetSize) #forces it to be 5%, opposite is used for nonholdout
          preNonHoldOutSize = (widthDiviser^2/100)/(1-holdoutSetSize) #forces it to be 5%, opposite is used for nonholdout
          
          #% of training resamples from static nonholdout
          preTrainSize = underOverSampleFactor/widthDiviser # <1 = (never fully iterates over subsample)
          
          #taken from a "static" nonHoldoutSet (i.e. excluded from monte carlo)
          #monte carlo resamples from a static holdout
          #used for resampling monte carlo training set from non holdout partitions!
          
          source(paste0(sourceDir,"reseedTest.R"))
          
          #static for monte carlo training 
          source(paste0(sourceDir,"reseedTrain.R"))
          
          #monte carlo resample from static sets
          #if widthDiviser = 1, keep as 1
          #resample=2      
          for (resample in 1:widthDiviser)
          {
            #base = resample
            source(paste0(sourceDir,"MCResampleTrain.R"))
            source(paste0(sourceDir,"MCResampleTest.R"))
            #print is inside inner loop
            
            ##before reseed
            #https://adv-r.hadley.nz/subsetting.html
            
            yIndex <- list[,4] == 0
            lGeographyIndex <- list[,4] == 1
            
            lGenderGPAViolenceFatherIndex <- list[,4] == 2
            #lGenderIndex <- list[,4] == 2
            #lGPAIndex <- list[,4] == 3
            #lViolenceIndex <- list[,4] == 4
            #lFather1Index <- list[,4] == 5
            #lFather2Index <- list[,4] == 6
            
            lHabitsIndex1 <- list[,4] == 3
            lHealthIndex <- list[,4] == 4
            lPsycheIndex1 <- list[,4] == 5
            lPsycheIndex2 <- list[,4] == 6
            lHabitsIndex2 <- list[,4] == 7
            
            if (widthDiviser == 1) train.control <- trainControl(method = "repeatedcv", number = 2, repeats = widthDiviser)
            if (!(widthDiviser == 1)) train.control <- trainControl(method = "repeatedcv", number = widthDiviser, repeats = widthDiviser)
            
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
              if (iterator==1 && resample==1 && holdoutReset==1 && seeder==start) 
              {
                print(paste("Y:",as.character(list[yIndex,][iterator,][,1])))
                
              }
              
              if(resample==1 && holdoutReset==1 && seeder==start)
              {
                numRuns = 1
              }
              
              if (!(iterator==1 && resample==1 && holdoutReset==1 && seeder==start))
              {
                numRunsold <- c()
                numRunsold = numRuns
                numRuns <- c()
                numRuns = numRunsold + 1
              }
              
              #aggregated after categories loop
              namesTV <- c()
              namesH <- c()
              
              #print(paste("loop: ", numRuns, "holdoutReset: ",holdoutReset,"resample: ",resample))
              
              #doesn't resample unless I [re-]sample (function) an index... unsure if CV has an internal index.  I'm sure it is random each pass.
              #My assumption is the first CV is always a specific seed.  My hope is to have different seeds.
              
              #categories 
              #val=3
              for (val in 2:6)
              {
                #used in category, rolled into names
                datalist1 <- c()
                datalist2 <- c()
                
                #end up with no records due to na's, and so any variables.  Inverse relationship.
                colList <- c()
                if (val == 2) colList <- list[lGenderGPAViolenceFatherIndex,]
                #if (val == 3) colList <- list[lGPAIndex,]
                #if (val == 4) colList <- list[lViolenceIndex,]
                #if (val == 5) colList <- list[lFather1Index,]
                #if (val == 6) colList <- list[lFather2Index,]
                if (val == 3) colList <- list[lHabitsIndex1,]
                if (val == 4) colList <- list[lHealthIndex,]
                if (val == 5) colList <- list[lPsycheIndex1,]
                if (val == 6) colList <- list[lPsycheIndex2,]
                if (val == 7) colList <- list[lHabitsIndex2,]
                
                if (is.null(nrow(data.frame(alty)))) break
                
                #colList <- rbind(list[yIndex,],colList)
                
                colList <- rbind(y,colList)
                
                #https://stackoverflow.com/questions/17878048/merge-two-data-frames-while-keeping-the-original-row-order
                #https://stackoverflow.com/questions/28311293/how-to-make-join-operations-in-dplyr-silent
                colListNames <- c()
                colListNames <- suppressMessages(paste(join(colList,list)[,1],join(colList,list)[,3]))
                
                newList <- c()        
                newList <-  suppressMessages(as.character(join(colList,list[,c(1,3)])[,1, drop=TRUE]))
                oldList <- newList[-1]
                #https://stat.ethz.ch/R-manual/R-devel/library/base/html/droplevels.html
                #droplevels(newList)
                #https://stackoverflow.com/questions/34469178/r-convert-factor-to-numeric-and-remove-levels
                
                #needs to be inside category when newList is generated
                #don't re-use for csv's... csv's... are already cleaned
                #repurpose instead
                #replaces 0 with na's (so it assumes data is already precleaned to just a NewDF level)
               
                numOfVars <- c()
                numOfVars <- length(oldList)
                
                pairs <- c()
                pairs <- pairedLists(numOfVars)
                
                #runs=3
                for(runs in 1:nrow(pairs))
                {
                  #kind of hackey
                  #left
                  #pairs[runs,][1]
                  #right
                  #pairs[runs,][2]
                  oldList[as.integer(pairs[runs,][1])]
                  
                  ypair <- newList[1]
                  xpair <- cbind(oldList[as.integer(pairs[runs,][1])],oldList[as.integer(pairs[runs,][2])])
                  
                  newList <- c()
                  newList <- cbind(ypair,xpair)
                  
                  skipFlag=0
                  #subcategory specific
                  #just point to resample script and use data.train
                  tryCase <- tryCatch(source(paste0(sourceDir,"redrawTrain.R")), error=function(e) skipFlag=1)
                  
                  #I don't want it to reseed here'
                  if(tryCase!=1)
                  {
                    tryCase <- tryCatch((datalist1 <- suppressWarnings(sub_returnCVNames(data.train))), 
                                        error=function(e) datalist1 <- suppressWarnings(sub_returnCVNames(data.train)))
                    
                    
                    
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
                    
                    #end of pairs
                  }
                  
                  
                }
                
                
                #end of category iterator
              }
              #print("category pass")  
              #Taggregated <- c()
              Hfiltered <- c()
              extract <- c()
              
              #print(c("1: ", namesTV))
              
              tvList <- c()
              tvList <- c(yname,namesTV)
              #resample before drawing from data.test
              
              for (iterator in 1:length(tvList))
              {
                
                #merge(newList,list)
                l1 <- data.frame(tvList)
                colnames(l1) <- "V1"
                #merge(noquote(newList), list)
                recreated <- suppressMessages(join(l1,list))

                yIndex <- recreated[,4] == 0
                lGeographyIndex <- recreated[,4] == 1
                
                lGenderGPAViolenceFatherIndex <- recreated[,4] == 2
                lHabitsIndex1 <- recreated[,4] == 3
                lHealthIndex <- recreated[,4] == 4
                lPsycheIndex1 <- recreated[,4] == 5
                lPsycheIndex2 <- recreated[,4] == 6
                lHabitsIndex2 <- recreated[,4] == 7
                                
                Hfiltered <- c() 
                
                #V4, skip category 0 which is 1st position, i.e. y
                for (val in c(unique(recreated[,4])[-1]))
                {
                  #used in category, rolled into names
                  datalist1 <- c()
                  datalist2 <- c()
                  
                  #end up with no records due to na's, and so any variables.  Inverse relationship.
                  colList <- c()
                  if (val == 2) colList <- recreated[lGenderGPAViolenceFatherIndex,]
                  if (val == 3) colList <- recreated[lHabitsIndex1,]
                  if (val == 4) colList <- recreated[lHealthIndex,]
                  if (val == 5) colList <- recreated[lPsycheIndex1,]
                  if (val == 6) colList <- recreated[lPsycheIndex2,]
                  if (val == 7) colList <- recreated[lHabitsIndex2,]
                  
                  if (is.null(nrow(data.frame(alty)))) break
                  
                  #colList <- rbind(list[yIndex,],colList)
                  newList <- c()
                  newList <- as.character(rbind(y,colList)[,1])
                  
                  source(paste0(sourceDir,"redrawTest.R"))
                  
                  holderOfData <- c()
                  
                  holderOfData <- cbind(data.frame(data.test[,-1 , drop = FALSE]),data.frame(data.test[,1 , drop = FALSE]))
                  
                  if ( widthDiviser == 1 )  Hfiltered <- suppressMessages(bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=2, REP=widthDiviser, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive"))
                  if ( !widthDiviser == 1 )  Hfiltered <- suppressMessages(bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=widthDiviser, REP=widthDiviser, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive"))
                  
                  extract <- c()
                  extract <- row.names(data.frame(Hfiltered$BestModel[1]))[-1]
                  
                  #if errors, which I've seen with no resulting variables and throw no error... then I report nothing, tabulate nothing.  Simply a missed iteration, but numRuns will increase.
                  
                  #print(c("2: ", extract))
                  
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
                  if (length(extract)==0) extract <- c()
                  
                }
                #t1 <- data.frame(noquote(newList))
                
                #t1 %>% inner_join(list)
                #table(data.test[newList], useNA = "ifany")
                
                #just point to resample script and use data.train, isn't technically resampled except when
                #I don't need to resample here because I'm using holdout... data.test
                #I do need to resmaple, because I have a newList... this doesn't generate new samples, merely draws
                
                
              }
              
              print(c(numRuns,"2a: ", round(table(finalList)/numRuns,2)))
              
              #end of holdout analysis
              
              
              #end of resample MC pass 
            }
            #write.csv(filtered,paste0(sourceDir,yname,"hR-",holdoutReset,"rS-",resample,"filteredv1.csv"))
            #write.csv(filteredv2,paste0(sourceDir,yname,"hR-",holdoutReset,"rS-",resample,"filteredv2.csv"))
            #end outermost loop
            
            #would be a good place if one desired to see it iterate only every so often
            #print(c("2a: ", round(table(finalList)/numRuns,3)))
            #end of holdoutReset 
          }
          
          #end holdoutReset
        }
        
        
        #end of seeder
      }
      
      #spacer
      finalListReduced <- c()
      tabled <- table(finalList[,,drop=FALSE])/numRuns
      print(tabled)
      table(finalList)
      #if(length(tabled)==1) finalListReduced <- row.names(data.frame(tabled[tabled >= quantile(tabled)[3]]))
      #if(!length(tabled)==1) finalListReduced <- c(as.character(data.frame(table(finalList)[table(finalList) >= quantile(table(finalList))[3]])[,1]))
      
      #if (widthDiviser==1)
      {
        #if(length(tabled)==1) finalListReduced <- row.names(data.frame(tabled[tabled >= 1/numRuns]))
        #if(!length(tabled)==1) finalListReduced <- c(as.character(data.frame(table(finalList)[table(finalList) >= 1/numRuns])[,1]))
      }
      #if (!widthDiviser==1)
      if(length(tabled)==1) finalListReduced <- row.names(data.frame(tabled[tabled > CVRuns_pct_threshold]))
      #this is not tabled, which is based on table(finalList), hence I do the /numRuns, as tabled already has that done.
      
      if(!length(tabled)==1) 
      {
        if(sum(table(finalList)/numRuns > CVRuns_pct_threshold)==0)      
        {
          finalListReduced <- c
        }
        if(!(sum(table(finalList)/numRuns > CVRuns_pct_threshold)==0))
        {
          if(!length(table(finalList)[table(finalList)/numRuns > CVRuns_pct_threshold])==1)
          {
            finalListReduced <- c(as.character(row.names(table(finalList)[table(finalList)/numRuns > CVRuns_pct_threshold])))
          }
          if(length(table(finalList)[table(finalList)/numRuns > CVRuns_pct_threshold])==1)
          {
            finalListReduced <- c(as.character(row.names(data.frame(table(finalList)[table(finalList)/numRuns > CVRuns_pct_threshold]))))
          }
        }
      }
      
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
      
      write.csv(filtered,(paste0(sourceDir,"/output/",yname,"-",medianDirection,"-",widthDiviser,"-","filtered.csv")))  
      
      #end of lister
    }
    #end width
    #readline(prompt="Press [enter] to continue")
  }
  
  #end medianDirection  
}

source(paste0(sourceDir,"4thpass.R"))
