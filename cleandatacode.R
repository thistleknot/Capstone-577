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
library(stringr)

#good values are integer's, of 2, 3, 5 (5% training sample size, anda 5% holdout sample size per analysis)
#1% passes result in too low of a pass and give overfitted coefficient terms which result in too large of a sample for the 2nd holdout iteration.
#therefore a minimum of 1.25% is recommended, but to hard code that here... would be wonky.  So sticking to simply integer 

#used for resample r scripts to round/up down to sample sizes
#max precision is # of records
#precisionSize=182338*4

#the way I have this setup, it only returns one var
sub_returnCV <- function(data_sent){
  #data_sent=data.train
  holderOfData <- cbind(data.frame(data_sent[,-1 , drop = FALSE]),data.frame(data_sent[,1 , drop = FALSE]))
  #table(NewDF[,"V7202"])
  
  #info <- which(colSums(holderOfData)==nrow(holderOfData))
  #name <- rownames(data.frame(info))
  #if(!length(info)==0) holderOfData <- holderOfData[, -which(names(holderOfData) == name)]
  
  if ( widthDiviser == 1 )  B <- suppressMessages(bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=2, REP=widthDiviser, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive"))
  if (!(widthDiviser == 1 )) B <- suppressMessages(bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=widthDiviser, REP=widthDiviser, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive"))
  
  result <- c()
  #return all coefficients
  result <- data.frame(as.character(rownames(data.frame(B$BestModel$coefficients[]))[-1]))
  
  set<-round(colSums(B$Subsets))[-1]
  
  if(!is.null(result)) result <- NA
  
  #if(nrow(result)==2)
  #{
    #result <- 
  #}
  #aboveMedianCV <- as.character(rownames(data.frame(which(result >= median(result)))))
  return(result)
}

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

#write.csv(d_combined,(paste0(sourceDir,"d_combined.csv")))

for (interests in c("V7221","V7215","V7551","V7552","V7553","V7562","V7563"))
{
  #median information
  print(paste("interest:",interests))
  for(year in c("d_2012","d_2013","d_2014","d_2015","d_2016","d_2017","d_combined"))
  {
    #https://stackoverflow.com/questions/28802652/access-variable-dataframe-in-r-loop
    df <- (get(year)[,interests])
    print(paste("year:",year))
    print(paste("count:",sum (count(df[df>0]))))
    
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

#data <- read.csv(paste0(sourceDir,"d_combined.csv"), header=TRUE, sep=",")

data <- d_combined
ncol(data)
#drops columns with na values

#not actually used, was used initially
cleandata<-data[,colSums(is.na(data)) >= round(nrow(data)*.25,0)] # dat[A, B] takes the A rows and B columns; A and B are indices; 
ncol(cleandata)

colnames(cleandata)
# if A or B is not specified, all rows or columns will be retained

#expensive, descriptive function only
#table(is.na(cleandata))# table(is.na(cleandata)) gives the number of missing values of data
#Since there are no missing values we export the data
#write.csv(cleandata, "C:\\Users\\CampusUser\\Desktop\\MyData.csv")

suppressWarnings(system(paste0('rm -f ',sourceDir,'/output/*.csv'), intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE, wait = TRUE, input = NULL, show.output.on.console = TRUE, minimized = FALSE, invisible = TRUE, timeout = 0))

#medianDirection = "greaterEqual"
#for (medianDirection in c("greaterEqual"))
for (medianDirection in c("greaterEqual","greater"))
{
  #will error on 3 for V7118
  #widthDiviser=3
  #for(widthDiviser in c(3))
  for(widthDiviser in c(3))
  {
    print(paste0("widthDiviser: ",widthDiviser))
    
    if (widthDiviser == 1) train.control <- trainControl(method = "repeatedcv", number = 2, repeats = widthDiviser)
    if (!(widthDiviser == 1)) train.control <- trainControl(method = "repeatedcv", number = widthDiviser, repeats = widthDiviser)
    
    #so if 3, has to exist in > 1.5 subsamples
    #hard coded
    CVRuns_pct_threshold = .5
    #this needs to be set in 4thpass as well
    
    #CVRuns_pct_threshold = .25
    #has to appear in half the samples of 1 width?
    #dangerously overfits
    #should be more than 1/widthDviser
    #CVRuns_pct_threshold = (1/widthDiviser)2
    
    #flister=1
    for(flister in 1:3)
    {
      #y is handled in holdout
      #data manipulation (x's) is handled in resample loop
      numRuns = 1
      #7221 gpa
      if (flister==1) list<-read.csv(paste0(sourceDir,"gpalist.txt"), header=FALSE, sep=,)
      
      #8517 gang
      if (flister==2) list<-read.csv(paste0(sourceDir,"gangfight.txt"), header=FALSE, sep=,)
      
      #7118 (psychadelics)
      if (flister==3) list<-read.csv(paste0(sourceDir,"psyDFilterList.txt"), header=FALSE, sep=,)
      
      colnames(data)
      col.num <- which(colnames(data) %in% as.character(list[,1]))
      #need to include GPA and psyD and gangfight in all 3!
      #done, created 8th category and excluded it specifically from analysis, meaning I don't have to rerun my #'s :)
      NewDF <- data[,(c(col.num))]
      
      #reset each file
      tabulatedCrossValidated <- c()
      nullpairs <- c()
      
      length(colnames(NewDF))
      
      #this resets each file
      Hfiltered <- c()
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

      source(paste0(sourceDir,"NewDF.R"))
      
      #here I should drop from NewDF non important terms (i.e. GPA, gangfight and PsyD from equations where not being tested.)
      
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
      lExcludedIndex <- list[,4] == 8
      
      colListNames <- c()
      colListNames <- rbind(list[lGenderGPAViolenceFatherIndex,],list[lHabitsIndex1,],list[lHealthIndex,],list[lPsycheIndex1,],list[lPsycheIndex2,],list[lHabitsIndex2,])
  
      #resets each new file
      finalList <- c()
 
      start = 5
      if ( widthDiviser == 1) end = (start+1)
      if ( (widthDiviser > 1) && (widthDiviser < 3) ) end = (start+(widthDiviser-1))
      if ( (widthDiviser > 2) ) end = start
      #seeder=start
      for (seeder in start:end)
      {
        set.seed(seeder)
        #seedbase=seeder
        #I don't actually have to call the reseed function, but I need to check when seeder = start
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
          holdoutSetSize = widthDiviser/100
          #holdoutSetSize = 1.25/100
          
          #% to resample from resampled static hold out set
          holdoutSize = 1/widthDiviser #(of set) #(never fully iterates over subsample)
          
          #proportion of nonHoldout (i.e. nonholdout: 1-holdoutSize) to use for model building, i.e. sample size.  Holdout can be tuned independently kind of.
          #preNonHoldOutSize = (1.25/100)/(1-holdoutSetSize) #forces it to be 5%, opposite is used for nonholdout
          preNonHoldOutSize = (widthDiviser/100)/(1-holdoutSetSize) #forces it to be 5%, opposite is used for nonholdout
          
          #% of training resamples from static nonholdout
          preTrainSize = 1/widthDiviser # <1 = (never fully iterates over subsample)
          
          #taken from a "static" nonHoldoutSet (i.e. excluded from monte carlo)
          #monte carlo resamples from a static holdout
          #used for resampling monte carlo training set from non holdout partitions!
          
          #y and iterator info
          iterator = sum(yIndex)
          
          #I know I have lister, but at one time I had multiple y's before I was utilizing lister...  so that's why there is a y iterator here
          yname <- c()
          yname <- as.character(list[yIndex,][iterator,][,1])
          
          y <- c()
          y <- list[yIndex,][iterator,]
          
          alty <- c()
          alty <- list[yIndex,][-iterator,]
          #y
          #yname <- as.character(list[yIndex,][iterator,][,1])          
          
          #only place this is assigned
          newList <- c()        
          newList <- c(as.character(y[,1]),as.character(colListNames[,1]))
          oldList <- as.character(newList[-1])
          numOfVars <- c()
          numOfVars <- length(oldList)
          
          #static for monte carlo training 
          #monte carlo resample from static sets
          #if widthDiviser = 1, keep as 1
          #resample=2   
          #generates dynamic lists
          #run through training
          #run through testing
          #tabulate common terms
          for (resample in 1:widthDiviser)
          {
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
            
            pairs <- c()
            #this randomization is controlled by the resample iterator 
            #Hence necessary to have more than one resample iterator
            pairs <- pairedLists(numOfVars)
            
            pairedname_List <- c()
            pairsForLater <- c()
            #generate list of names 1st
            for(runs in 1:nrow(pairs))
            {
              ypair <- newList[1]
              xpair <- c(oldList[as.integer(pairs[runs,][1])],oldList[as.integer(pairs[runs,][2])])
              #newList <- c()
              #newList <- cbind(ypair,xpair)
              #print(newList)
              pairedname <- c()
              #https://stackoverflow.com/questions/7201341/how-can-two-strings-be-concatenated
              pairedname <- capture.output(cat(c(ypair,xpair), sep = ""))  
              #print(pairedname)
              
              #combinedOutside <- NewDF[,as.character(c(newList)),drop=FALSE] 
              #combined[combinedOutside == 0] <- NA
              #temp <- combinedOutside[] %>% filter_all(all_vars(!is.na(.)))
              
              #newList <- c()
              #newList <- pairedname
              #resample draws new partitions from new randomize columns.  This randomization is independent of the columns randomization.
              
              #check to see if list has records
              combinedOutside <- NewDF[,as.character(c(ypair,xpair)),drop=FALSE] 
              combinedOutside[combinedOutside == 0] <- NA
              temp <- combinedOutside[] %>% filter_all(all_vars(!is.na(.)))
              if(nrow(temp)!=0)
              {
                #at this juncture because it's confirmed rows are not 0
                source(paste0(sourceDir,"reseedBoth.R"))
                pairedname_List <- rbind(pairedname,pairedname_List)
                pairsForLater <- rbind(c(ypair,xpair),pairsForLater)
                
              }
              
              if(nrow(temp)==0) 
                {
                  tempholder <- c()
                  tempholder <- c(ypair,xpair)
                  nullpairs <- rbind(nullpairs,tempholder)
                  #print(c("null:",c(ypair,xpair)))
                }
              }
            #pairedname_List
            #base = resample
            #print is inside inner loop

            #runs1=1
            #iterate through list of names and set seeds
            
            #initiate outer dataset and inner index loops
            for(runs1 in 1:nrow(pairedname_List))
            {
              #checking in here because I need access to pairedNames...
              #don't reset the index PairsForLater here...
              #pairsForLater <- c()
              pairedName <- c()
              pairedname <- pairedname_List[runs1]
              
              #pairsForLater is created at the same time as pairedname_List, so they are paired.  I can assume they index the same.
              ypair <- c()
              ypair <- pairsForLater[runs1,][1]
              #ypair <- newList[1]
              #xpair <- c(oldList[as.integer(pairs[runs,][1])],oldList[as.integer(pairs[runs,][2])])
              xpair <- c()
              xpair <- pairsForLater[runs1,][-1]
              #newList <- c()
              #use c(ypair,xpair) for inside loops 
              #newList <- c(ypair,xpair)               
            
              #check if list is empty  
              combinedOutside <- c()
              combinedOutside <- NewDF[,as.character(c(ypair,xpair)),drop=FALSE] 
              combinedOutside[combinedOutside == 0] <- NA
              temp <- c()
              temp <- combinedOutside[] %>% filter_all(all_vars(!is.na(.)))
              if(nrow(temp)!=0)
              {
                #since I'm working with index's now, 
                #I think I can move reseed out of the inner most loop, 
                #yes, outside in an initiator loop that has it's own nrow check
                #these are deendent upon pariedName variable
                #which means don't do anything above this
                source(paste0(sourceDir,"reseedBoth.R"))
                source(paste0(sourceDir,"reseedTest.R"))
                source(paste0(sourceDir,"reseedTrain.R"))
                source(paste0(sourceDir,"MCResampleTest.R"))
                source(paste0(sourceDir,"MCResampleTrain.R"))
              }
             
            }

            #iterates over lists and generates namesTV
            #question is when do I want it reset?  Each file?  I want the holdoutnames to be reset each holdout
            #but namesTV is a lower level holdout cached/filtered set of names
            #they are filtered through holdout
            #so they are reset each list of pairedname_List on a single set of data.train seeded data
            #holdoutreset resets the holdout schema of data and that is where holdout data is aggregated.
            #best way to deal with unavoidance of dealing with possibility of sampling same data twice with the way dynamic lists are generated
            #tradoff between monte carlo and true hold out is by resampling the holdout indexes
            #and hence a new holdout analysis is done on a different 
            #set of column pairs generated within the MC resample 
            #based on new holdout partitions.  There is new column sets regardless of what I do and virtual index's didn't work.  
            #The only thing I have control over are outer indexes but these indexes are based on the size of the dynamic column sets that are generated
            #at sample time which are based on the holdout... so to get a new true holdout... I figured I'd rely on monte carlo to gen a new partition.
            #without virtual index's, I can't story original index's because the index's are based on these dynamic columns from these pairedlists based on the names
            #passed to pairedLists (well, based on the indexs from pairedList thrown at oldList)

            #print("single pair passes")
            #aggregated after categories loop
            namesTV <- c()
            
            #generates dynamics sets of records, working with initiated arrays and combined dataset from above loop.
            #runs2=1
            for(runs2 in 1:nrow(pairedname_List))
            {
              #checking in here because I need access to pairedNames...
              pairedname <- c()
              pairedname <- pairedname_List[runs2]
              ypair <- c()
              ypair <- pairsForLater[runs2,][1]
              #ypair <- newList[1]
              #xpair <- c(oldList[as.integer(pairs[runs,][1])],oldList[as.integer(pairs[runs,][2])])
              xpair <- c()
              xpair <- pairsForLater[runs2,][-1]
              #newList <- c()
              #newList <- c(ypair,xpair)
              #this quickly checks NewDF for the combination pair,
              #this assumes all other transformations have been done to NewDF to leave it in the same state that reseedBoth.R recieves it in.
              combinedOutside <- c()
              combinedOutside <- NewDF[,as.character(c(ypair,xpair)),drop=FALSE] 
              combinedOutside[combinedOutside == 0] <- NA
              temp <- c()
              temp <- combinedOutside[] %>% filter_all(all_vars(!is.na(.)))
              if(nrow(temp)!=0)
              {
                source(paste0(sourceDir,"redrawTrain.R"))
                #source(paste0(sourceDir,"redrawTest.R"))

                #could use d_combined and do conversion of -9 and -8 to na
                #would still have to do median after loading files, less payoff by doing that at this juncture
                # noticed V7562 and V8531 result in no records together when dropping na's... go figure
                
                pairedname <- c()
                #https://stackoverflow.com/questions/7201341/how-can-two-strings-be-concatenated
                pairedname <- capture.output(cat(c(ypair,xpair), sep = ""))
                
                #pairedname <- stringr::str_trim(prename)
                #print(pairedname)
                
                #going to be before I even do reseed's?  No, because column pair randomizations are dependent upon reseed... ughhh.
                #otherwise this would go above seed.  Which means it's going to be expensive, but I'm not testing every combination.
                #I'm using simulation to do the combinations, but I am ensuring I test every value twice (hopefully)
                #this means I need to put the rest of the MC loops inside here...
                
                #finalSet <- finalSetPre[!(finalSetPre %in% NA)]
                #print(c("Hfiltered:", Hfiltered))
                #print(c(numRuns,"2a: ", round(table(unique(Hfiltered))/numRuns,2)))
 
                #1st pass
                
                #runs=1
                #for(runs in 1:nrow(pairs))
   
                result <- sub_returnCVNames(data.train)
                
                for (i in 1:length(result))
                {
                  namesTV <- rbind(namesTV,result[i])
                }
                 
              #end if nrow != 0
              }
              
              if(nrow(temp)==0)
              {
                print(c("exclude",c(ypair,xpair)))
              }
              
              #end reseed-pairs (used for memory structures)
            }
            namesTV
            #print(c("namesTV:", namesTV))
            
            #holdout
            namesH <- c()
            #print("holdout pass")
            
            #generates dynamics sets of records, working with initiated arrays and combined dataset from above loop.
            #runs2=1
            for(runs3 in 1:nrow(pairedname_List))
            {
              #checking in here because I need access to pairedNames...
              pairedname <- c()
              pairedname <- pairedname_List[runs3]
              ypair <- c()
              ypair <- pairsForLater[runs3,][1]
              xpair <- c()
              xpair <- pairsForLater[runs3,][-1]
              #assumes nothing about 0/1 split
              combinedOutside <- c()
              combinedOutside <- NewDF[,as.character(c(ypair,xpair)),drop=FALSE] 
              combinedOutside[combinedOutside == 0] <- NA
              temp <- c()
              temp <- combinedOutside[] %>% filter_all(all_vars(!is.na(.)))
              if(nrow(temp)!=0)
              {
                #source(paste0(sourceDir,"redrawTrain.R"))
                source(paste0(sourceDir,"redrawTest.R"))
                
                pairedname <- c()
                #https://stackoverflow.com/questions/7201341/how-can-two-strings-be-concatenated
                pairedname <- capture.output(cat(c(ypair,xpair), sep = ""))
                
                holderOfData <- c()
                holderOfData <- cbind(data.test[,-1,drop=FALSE],data.test[,1,drop=FALSE])
                
                result <- sub_returnCVNames(data.test)
                
                #redundant check now!
                #if(nrow(data.train)!=0)
              
                #data.train[,xpair]
                #result <- sub_returnCVNames((data.test))
                
                for (i in 1:length(result))
                {
                  namesH <- rbind(namesH,result)
                }                
              
                if(is.na(result))
                {
                  namesH <- rbind(namesH,NA)  
                }

                #end if nrow != 0
              }
              
              if(nrow(temp)==0)
              {
                print(c("exclude",c(ypair,xpair)))
              }
              
              #end reseed-pairs (used for memory structures)
            }
            namesH

            #compare two lists/tests
            crossValidated <- c()
            if(nrow(namesTV)==nrow(namesH))
            {
              for(counter in 1:nrow(namesTV))
              {
                #if either na divert
                if(is.na(namesTV[counter])||is.na(namesH[counter]))
                {
                  crossValidated <- rbind (crossValidated,NA)
                }
                #IF NOT NA, DON'T DIVER
                if(!(is.na(namesTV[counter])||is.na(namesH[counter])))
                {
                  if(namesTV[counter]==namesH[counter])
                  {
                    crossValidated <- rbind (crossValidated,namesTV[counter])
                  }
                  if(namesTV[counter]!=namesH[counter])
                  {
                    crossValidated <- rbind (crossValidated,NA)
                  }
                }
              }
              #end namesTV nameH for loop
            }
            #crossValidated
            #print(c(length(na.omit(crossValidated)),"/",nrow(pairedname_List),":",crossValidated))
            
            #write.csv(filtered,paste0(sourceDir,yname,"hR-",holdoutReset,"rS-",resample,"filteredv1.csv"))
            #write.csv(filteredv2,paste0(sourceDir,yname,"hR-",holdoutReset,"rS-",resample,"filteredv2.csv"))
            #end outermost loop
            
            #would be a good place if one desired to see it iterate only every so often
            tabulatedCrossValidated <- rbind(tabulatedCrossValidated,crossValidated)
            
            print_tabled <-c()
            #due to the chance of no results on both sides two passes from na's, /8
            #*2 for 2 pairs per x2 columns x 2 passes (ond holdout and training)
            print_tabled <- round(table(tabulatedCrossValidated, useNA = "ifany")/numRuns/2,3)
            #print(print_tabled)
            #end if nrow !=0            
            
            #end of MC
          }
          print(c("tabCV: ",print_tabled))
          #end holdoutReset
        }
        
        
        #end of seeder
        
      }
      
      print(c("final: ",print_tabled))

      write.csv(unique(nullpairs),(paste0(sourceDir,"/output/",yname,"-",medianDirection,"-",widthDiviser,"-","nullpairs.csv")))
      write.csv(data.frame(print_tabled),(paste0(sourceDir,"/output/",yname,"-",medianDirection,"-",widthDiviser,"-","final.csv")))  
      
      #end of lister
    }
    #end width
    #readline(prompt="Press [enter] to continue")
  }
  
  #end medianDirection  
}
#unfortunately this relies on NewDF at the moment.  Either I need to reduce NewDF to it's own file/function or write it out to a .csv
source(paste0(sourceDir,"saveCSVs.R"))
#source(paste0(sourceDir,"4thpass.R"))
