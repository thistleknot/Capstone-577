## read in data ##
#select

#Written by Joshua Laferriere
#for Capstone 577
#CSUF
#2019 month of April
#over 400 commits
#https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
list.of.packages <- c("dplyr", "plyr","RPostgreSQL","ggplot2","anchors","caret","corrplot","MASS","car","leaps","bestglm","compare","R.utils","tidyr","stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(dplyr)
library(plyr)

#require("RPostgreSQL")
#library(RPostgreSQL)
#require(ggplot2)
library(anchors)
require(caret)
library(caret)
library(corrplot)
#library(MASS)
library(car)
library(leaps)
library(bestglm)
library(compare)
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

linux=0
if(linux)
{
  zipF <- "/home/rstudio/577/Capstone-577/Capstone-577.zip"
  outDir <- "/home/rstudio/577/Capstone-577/"
  sourceDir="/home/rstudio/577/Capstone-577/"
  
}
if(!linux)
{
  zipF<- "C:\\Users\\User\\Documents\\School\\CSUF\\ISDS577\\projects\\Capstone-577\\Capstone-577.zip"
  outDir<-"C:\\Users\\User\\Documents\\School\\CSUF\\ISDS577\\projects\\Capstone-577"
  sourceDir="C:/Users/user/Documents/School/CSUF/ISDS577/projects/Capstone-577/"
}

#sourceDir="/home/rstudio/577/Capstone-577/"
#sourceDir="C:/Users/user/Documents/School/CSUF/ISDS577/projects/Capstone-577/"
source(paste0(sourceDir,"bestglm.R"))
source(paste0(sourceDir,"sub_returnCVNames.R"))
source(paste0(sourceDir,"pairedLists.R"))
colListNames <- c()
source(paste0(sourceDir,"vars.R"))
source(paste0(sourceDir,"NewDF.R"))
# Read CSV into R

suppressWarnings(system(paste0('rm -f ',sourceDir,'/output/*.csv'), intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE, wait = TRUE, input = NULL, show.output.on.console = TRUE, minimized = FALSE, invisible = TRUE, timeout = 0))

for (medianDirection in medianDirectionSet)
{
  #call newDF, check if medianDirection is set, if not assume the 1st in medianDirectionSet
  
  #will error on 3 for V7118
  #widthDiviser=3
  #for(widthDiviser in c(3))
  #due to mcresampletest's class balancing.  I don't have error checking for when there is gross class imbalance.  So widthSize of 10 does
  for(widthDiviser in widthDiviserSet)
  {
    print(paste0("widthDiviser: ",widthDiviser))
    
    if (widthDiviser == 1) train.control <- trainControl(method = "repeatedcv", number = 2, repeats = widthDiviser)
    if (!(widthDiviser == 1)) train.control <- trainControl(method = "repeatedcv", number = 3, repeats = 1)
    
    #so if 3, has to exist in > 1.5 subsamples
    #hard coded
    #CVRuns_pct_threshold = .5
    #this needs to be set in 4thpass as well
    
    #CVRuns_pct_threshold = .25
    #has to appear in half the samples of 1 width?
    #dangerously overfits
    #should be more than 1/widthDviser
    #CVRuns_pct_threshold = (1/widthDiviser)2
    
    #flister=3
    for(flister in 1:3)
    {
      #y is handled in holdout
      #data manipulation (x's) is handled in resample loop
      numRuns = 1
      
      #reset each file
      tabulatedCrossValidated <- c()
      nullpairs <- c()
      errorpairs <- c()
      
      #7221 gpa
      if (flister==1) ilist<-read.csv(paste0(sourceDir,"gpalist.txt"), header=FALSE, sep=,)
      
      #8517 gang
      if (flister==2) ilist<-read.csv(paste0(sourceDir,"gangfight.txt"), header=FALSE, sep=,)
      
      #7118 (psychadelics)
      if (flister==3) ilist<-read.csv(paste0(sourceDir,"psyDFilterList.txt"), header=FALSE, sep=,)
  
      #this resets each file
      Hfiltered <- c()
      
      ##before reseed
      #https://adv-r.hadley.nz/subsetting.html
      
      yIndex <- ilist[,4] == 0

      #resets each new file
      finalList <- c()
      
      start = 5
      if ( widthDiviser == 1) end = (start+1)
      if ( (widthDiviser > 1) && (widthDiviser < 3) ) end = (start+(widthDiviser-1))
      if ( (widthDiviser > 2) ) end = start
      #seeder=start
      for (seeder in start:start)
      {
        set.seed(seeder)
        #seedbase=seeder
        #I don't actually have to call the reseed function, but I need to check when seeder = start
        print(paste("seed: ",seeder))
        
        holdoutResetEnd  <- c()
        
        if (widthDiviser == 1) holdoutResetEnd = 2
        if ( !(widthDiviser == 1) ) holdoutResetEnd = widthDiviser
        
        #holdoutReset=1
        for (holdoutReset in 1:widthDiviser)
        {
          print(paste0("holdoutReset: ",holdoutReset))
          #setup holdout
          
          #static holdout
          holdoutSetSize = pre_percent
          #holdoutSetSize = 1.25/100
          
          #% to resample from resampled static hold out set
          holdoutSize=1/3
          #holdoutSize = 1/3 #(of set) #(never fully iterates over subsample)
          
          #proportion of nonHoldout (i.e. nonholdout: 1-holdoutSize) to use for model building, i.e. sample size.  Holdout can be tuned independently kind of.
          #preNonHoldOutSize = (1.25/100)/(1-holdoutSetSize) #forces it to be 5%, opposite is used for nonholdout
          #same size as holdout
          preNonHoldOutSize = (pre_percent)/(1-holdoutSetSize) #forces it to be 5%, opposite is used for nonholdout
          
          #was using an underOverCoefficient which meant <1 = (never fully iterates over subsample)
          #% of training resamples from static nonholdout
          preTrainSize = 1/3
          #preTrainSize = 1/3 #
          
          #taken from a "static" nonHoldoutSet (i.e. excluded from monte carlo)
          #monte carlo resamples from a static holdout
          #used for resampling monte carlo training set from non holdout partitions!
          
          #y and iterator info
          iterator = sum(yIndex)
          
          #I know I have lister, but at one time I had multiple y's before I was utilizing lister...  so that's why there is a y iterator here
          yname <- c()
          yname <- as.character(ilist[yIndex,][iterator,][,1])
          
          y <- c()
          y <- ilist[yIndex,][iterator,]
          
          alty <- c()
          alty <- ilist[yIndex,][-iterator,]
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
          #generates dynamic lists
          #run through training
          #run through testing
          #tabulate common terms
          #resample=1
          #have to have more than 1 resample due to resampling from both 1's and 0's from one of the lower files (either MCResample or Redraw)
          for (resample in 1:3)
          {
            #rather than move to end of file
            if (iterator==1 && resample==1 && holdoutReset==1 && seeder==start) 
            {
              print(paste("Y:",as.character(ilist[yIndex,][iterator,][,1])))
              
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
              summary(temp)
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
                
                result <- c()
                nametemp <- c()
                nametemp <- eval(as.character(paste0("train",pairedname,str_replace_all(str_replace_all(string=Sys.time(), pattern=" ", repl=""), pattern=":", repl=""))))
                
                #https://rsangole.netlify.com/post/try-catch/
                tryCatch(
                  expr = {
                    result <- sub_returnCVNames(data.train)
                  },
                  error = function(e) {
                    write.csv(c("train",pairedname),paste0(sourceDir,"/output/",yname,"-",medianDirection,"-",widthDiviser,"-",nametemp,".csv"))
                  }
                  ,
                  warning = function(w){
                    # (Optional)
                    # Do this if an warning is caught...
                  },
                  finally = {
                    # (Optional)
                    # Do this at the end before quitting the tryCatch structure...
                  }
                )
                if(length(result)==0) result <- NA
                
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
            
            #generates dynamics sets of records, working with initiated arrays and combined dataset from above loop.
            #runs3=1
            for(runs3 in 1:nrow(pairedname_List))
            {
              #checking in here because I need access to pairedNames...
              pairedname <- c()
              pairedname <- pairedname_List[runs3]
              ypair <- c()
              ypair <- pairsForLater[runs3,][1]
              #ypair <- newList[1]
              #xpair <- c(oldList[as.integer(pairs[runs,][1])],oldList[as.integer(pairs[runs,][2])])
              xpair <- c()
              xpair <- pairsForLater[runs3,][-1]
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
                source(paste0(sourceDir,"redrawTest.R"))
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
                
                result <- c()
                nametemp <- c()
                nametemp <- eval(as.character(paste0("Test",pairedname,str_replace_all(str_replace_all(string=Sys.time(), pattern=" ", repl=""), pattern=":", repl=""))))
                
                #https://rsangole.netlify.com/post/try-catch/
                tryCatch(
                  expr = {
                    result <- sub_returnCVNames(data.test)
                  },
                  error = function(e) {
                    write.csv(c("Test",pairedname),paste0(sourceDir,"/output/",yname,"-",medianDirection,"-",widthDiviser,"-",nametemp,".csv"))
                  }
                  ,
                  warning = function(w){
                    # (Optional)
                    # Do this if an warning is caught...
                  },
                  finally = {
                    # (Optional)
                    # Do this at the end before quitting the tryCatch structure...
                  }
                )
                if(length(result)==0) result <- NA
                
                for (i in 1:length(result))
                {
                  namesH <- rbind(namesH,result[i])
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
                  #if sub_returnCVNames returns two... will only save two if those same two pass namesH, else it will drop both even if NamesH kept one.
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
            crossValidated
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
            print(print_tabled)
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
      write.csv(data.frame(print_tabled)[order(-data.frame(print_tabled)$Freq),],(paste0(sourceDir,"/output/",yname,"-",medianDirection,"-",widthDiviser,"-","final.csv")))  
      
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