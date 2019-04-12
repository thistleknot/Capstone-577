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

predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

pw <- {"Read1234"}

sourceDir="C:/Users/user/Documents/School/CSUF/ISDS577/projects/Capstone-577/"
source(paste0(sourceDir,"/bestglm.R"))
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

for(lister in 1:3)
{
  #7221 gpa
  if (lister==1) list<-read.csv(paste0(sourceDir,"altList.txt"), header=FALSE, sep=,)
  
  #8517 gang
  if (lister==2) list<-read.csv(paste0(sourceDir,"gangfight.txt"), header=FALSE, sep=,)
  
  #7118 (psychadelics)
  if (lister==3) list<-read.csv(paste0(sourceDir,"reducedfilterlist.txt"), header=FALSE, sep=,)
  
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
  
  table(is.na(NewDF))
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
  
  #7: B+
  V7221_Index <- NewDF[,"V7221"] >= median(NewDF[,"V7221"][NewDF[,"V7221"]>0])
  NewDF[V7221_Index,"V7221"] <- 1
  V7221_Index <- NewDF[,"V7221"] > 1
  NewDF[V7221_Index,"V7221"] <- 0
  
  #College graduate
  #5: for college grad father
  V7215_Index <- NewDF[,"V7215"] >= median(NewDF[,"V7215"][NewDF[,"V7215"]>0])
  NewDF[V7215_Index,"V7215"] <- 1
  V7215_Index <- NewDF[,"V7215"] > 1
  NewDF[V7215_Index,"V7215"] <- 0
  
  #4: 3-5 Hours Internet
  #4 #hours for computer use for internet leisure
  
  V7551_Index <- NewDF[,"V7551"] >= median(NewDF[,"V7551"][NewDF[,"V7551"]>0])
  unique(NewDF[,"V7551"])
  NewDF[V7551_Index,"V7551"] <- 1
  V7551_Index <- NewDF[,"V7551"] > 1
  #NewDF[V7551_Index,"V7551"] <- -1
  
  #5: 6-9 Hours Facebook
  V7552_Index <- NewDF[,"V7552"] >= median(NewDF[,"V7552"][NewDF[,"V7552"]>0])
  NewDF[V7552_Index,"V7552"] <- 1
  V7552_Index <- NewDF[,"V7552"] > 1
  NewDF[V7552_Index,"V7552"] <- 0
  
  #4 3-5 Hours Gaming
  V7553_Index <- NewDF[,"V7553"] >= median(NewDF[,"V7553"][NewDF[,"V7553"]>0])
  NewDF[V7553_Index,"V7553"] <- 1
  V7553_Index <- NewDF[,"V7553"] > 1
  NewDF[V7553_Index,"V7553"] <- 0
  
  #4 3-5 Hours Texting
  V7562_Index <- NewDF[,"V7562"] >= median(NewDF[,"V7562"][NewDF[,"V7562"]>0])
  NewDF[V7562_Index,"V7562"] <- 1
  V7562_Index <- NewDF[,"V7562"] > 1
  NewDF[V7562_Index,"V7562"] <- 0
  
  #2: <1 Hour talking on cell phone
  V7563_Index <- NewDF[,"V7563"] >= median(NewDF[,"V7563"][NewDF[,"V7563"]>0])
  NewDF[V7563_Index,"V7563"] <- 1
  V7563_Index <- NewDF[,"V7563"] > 1
  NewDF[V7563_Index,"V7563"] <- 0
  
  #NewDF[convert3Index] >= 4
  
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
  
  #after lister, before holdoutReset
  seedbase=7
  print(paste("seed",seedbase))
  
  widthDiviser=2
  #sets holdout resampling, monte carlo subset resampling, CV Passes, K Folds
  
  for (holdoutReset in 1:widthDiviser)
  {
    set.seed(seedbase)
    #setup holdout
    
    #static holdout
    holdoutSetSize = .05
    
    underOverSampleFactor=1
    
    #% to resample from resampled static hold out set
    holdoutSize = underOverSampleFactor/widthDiviser #(of set) #(never fully iterates over subsample)
    
    #proportion of nonHoldout (i.e. nonholdout: 1-holdoutSize) to use for model building, i.e. sample size.  Holdout can be tuned independently kind of.
    preHoldOutSize = .05
    
    #% of training resamples from static nonholdout
    preTrainSize = underOverSampleFactor/widthDiviser #(never fully iterates over subsample)
    
    #static (outside of monte carlo/resampling, if desire resampling, simply move above set.seed(base))
    holdoutSet <- c()
    holdoutSet <- sample(nrow(NewDF), round(holdoutSetSize*nrow(NewDF)))
    
    NewDF.holdoutSet <- c()
    NewDF.holdoutSet <- NewDF[holdoutSet,]
  
    #static for monte carlo training 
    preNonHoldoutSet <- c()
    preNonHoldoutSet <- sample(nrow(NewDF[-holdoutSet,]), round(preHoldOutSize*nrow(NewDF[-holdoutSet,])))
    
    NewDF.preNonHoldoutSet <- c()
    NewDF.preNonHoldoutSet <- NewDF[-holdoutSet,][preNonHoldoutSet,]
    
    #monte carlo sample size that samples from the preTrain.
    #considering that we are doing at least 10 outer loops, 
    #it doesn't make sense to oversaturate by having a large sample size since (i.e. 25% x 10 = 250% coverage) we're already cross validating at the lower level.  
    #trainMCSize = .10
    
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
      
      #train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
      
      y <- c()
      yname <- c()
      #y iterator's
      #iterator=2
      
      for (iterator in 1:sum(yIndex))
      {
      
        yname <- c()
        yname <- as.character(list[yIndex,][iterator,][,1])
        
        y <- c()
        y <- list[yIndex,][iterator,]
        
        alty <- c()
        alty <- list[yIndex,][-iterator,]
        #y
        #yname <- as.character(list[yIndex,][iterator,][,1])
        print(paste("Y:",as.character(list[yIndex,][iterator,][,1])))
        
        names <- c()
        
        print(paste("holdoutReset: ",holdoutReset))
        print(paste("resample: ",base))
  
        #doesn't resample unless I [re-]sample (function) an index... unsure if CV has an internal index.  I'm sure it is random each pass.
        #My assumption is the first CV is always a specific seed.  My hope is to have different seeds.
  
        #categories #val=4
        for (val in 2:9)
        {
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
          
          data.train <- c()
          data.train <- NewDF.preTrain[,as.character(newList), drop=FALSE] %>% filter_all(all_vars(!is.na(.)))
          data.train[data.train == 0] <- NA
          data.train <- data.train %>% filter_all(all_vars(!is.na(.)))
          data.train[data.train == -1] <- 0
          
          data.test <- c()
          data.test <- NewDF.holdout[,as.character(newList), drop=FALSE] %>% filter_all(all_vars(!is.na(.)))
          data.test[data.test == 0] <- NA
          data.test <- data.test %>% filter_all(all_vars(!is.na(.)))
          data.test[data.test == -1] <- 0
        
          #modified code: https://rdrr.io/cran/bestglm/src/R/bestglm.R to ignore p <15
          #https://rdrr.io/cran/bestglm/man/bestglm.html
          #http://ropatics.com/machine-learning/ml_-_Logistic_regression.html
          #https://rstudio-pubs-static.s3.amazonaws.com/2897_9220b21cfc0c43a396ff9abf122bb351.html
          #https://rdrr.io/cran/bestglm/man/bestglm-package.html
          holderOfData <- cbind(data.frame(data.train[,-1 , drop = FALSE]),data.frame(data.train[,1 , drop = FALSE]))
          B <- suppressMessages(bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=widthDiviser, REP=widthDiviser), family=binomial))
          
            {
              #cverrs = B$Subsets[, "CV"]
              #sdCV = B$Subsets[, "sdCV"]
              #CVLo = cverrs - sdCV
              #CVHi = cverrs + sdCV
              #ymax = max(CVHi)
              #ymin = min(CVLo)
              #k = 0:(length(cverrs) - 1)
              #plot(k, cverrs, ylim = c(ymin, ymax), type = "n", yaxt = "n")
              #points(k,cverrs,cex = 2,col="red",pch=16)
              #lines(k, cverrs, col = "red", lwd = 2)
              #axis(2, yaxp = c(0.6, 1.8, 6))
              #segments(k, CVLo, k, CVHi,col="blue", lwd = 2)
              #eps = 0.15
              #segments(k-eps, CVLo, k+eps, CVLo,  col = "blue", lwd = 2)
              #segments(k-eps, CVHi, k+eps, CVHi,  col = "blue", lwd = 2)
              #indMin = which.min(cverrs)
              #fmin = sdCV[indMin]
              #cutOff = fmin + cverrs[indMin]
              #abline(h = cutOff, lty = 2)
              #indMin = which.min(cverrs)
              #fmin = sdCV[indMin]
              #cutOff = fmin + cverrs[indMin]
              #min(which(cverrs < cutOff))
            }
            #(B$Subsets$CV-mean(B$Subsets$CV))/sd(B$Subsets$CV)
            
            #within one standard deviation from the min error
            
            #https://stackoverflow.com/questions/51107901/how-do-i-filter-a-range-of-numbers-in-r
            
            #B$Subsets%>% filter(CV %in% min(B$Subsets$CV):(min(B$Subsets$CV)+sd(B$Subsets$CV)))
            
            #B$Subsets$[B$Subsets$CV >= min(B$Subsets$CV) & B$Subsets$CV <= (min(B$Subsets$CV)+sd(B$Subsets$CV)) ]
            #don't reset names here, reset outside of categories
            datalist <- c()
            datalist <- as.character(rownames(data.frame(B$BestModel$coefficients)))[-1]
            if(length(datalist)==1)
            {
              
              names <- rbind(names,as.character(rownames(data.frame(B$BestModel$coefficients)))[-1])
            }
            
            if(length(as.character(rownames(data.frame(B$BestModel$coefficients)))[-1])>1)
              for (i in 1:length(datalist))
            {
              
              names <- rbind(names,datalist[i])
            }
            
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
            
           
        }
        print("1st pass")  
        print(c(names))
        
      #end of category iterator  
      }
      
      #second pass through 
      if(length(names)!=0)
      {
        
        #print("2nd pass")
        B2Names <- c()
        profile <- c()
        profile <- c(yname,c(names))
        
        filtered <- c()
        filtered <- NewDF.preTrain[,as.character(profile), drop=FALSE] %>% filter_all(all_vars(!is.na(.)))
        filtered[filtered == 0] <- NA
        filtered <- filtered %>% filter_all(all_vars(!is.na(.)))
        filtered[filtered == -1] <- 0
        
        filtered.train <- c()
        filtered.train <- NewDF.preTrain[,as.character(profile),drop=FALSE] %>% filter_all(all_vars(!is.na(.)))
        
        filtered.train[filtered.train == 0] <- NA
        filtered.train <- filtered %>% filter_all(all_vars(!is.na(.)))
        filtered.train[filtered.train == -1] <- 0
        
        filteredholdout <- c()
        filteredholdout <- NewDF.holdout[,as.character(profile),drop=FALSE] %>% filter_all(all_vars(!is.na(.)))
        filteredholdout[filteredholdout == 0] <- NA
        filteredholdout <- filteredholdout %>% filter_all(all_vars(!is.na(.)))
        filteredholdout[filteredholdout == -1] <- 0
        B2 <- suppressMessages(bestglm(Xy = cbind(data.frame(filteredholdout[,-1 , drop = FALSE]),data.frame(filteredholdout[,1 , drop = FALSE])), IC="CV", CVArgs=list(Method="HTF", K=widthDiviser, REP=widthDiviser,TopModels = 1), family=binomial))
        
        #B2Names <- as.character(rownames(data.frame(B2$BestModel$coefficients)))[-1]
        
        datalist2 <- c()
        datalist2 <- as.character(rownames(data.frame(B2$BestModel$coefficients)))[-1]
        
        if(length(datalist2)==1)
        {
          B2Names <- rbind(B2Names,as.character(rownames(data.frame(B2$BestModel$coefficients)))[-1])
        }
        
        if(length(as.character(rownames(data.frame(B2$BestModel$coefficients)))[-1])>1)
          for (i in 1:length(datalist2))
          {
            B2Names <- rbind(B2Names,datalist2[i])
          }
        
        if(length(B2Names)!=0) 
          {
            print("holdout pass: ")
            print(c(B2Names))
            
            #HoldoutModel <- glm(filteredholdout[colnames(filtered)])
            #HoldoutCVModel <- train(filteredholdout[colnames(filtered)][-1], as.factor(filteredholdout[colnames(filtered)][,1]), method = "glm",trControl = train.control)
            
            filteredv2 <- c()
            filteredv2 <- NewDF[,c(yname,as.character(B2Names)), drop = FALSE] %>% filter_all(all_vars(!is.na(.)))
            filteredv2[filteredv2 == 0] <- NA
            filteredv2 <- filteredv2 %>% filter_all(all_vars(!is.na(.)))
            filteredv2[filteredv2 == -1] <- 0
            
            #includes before variables are dropped.  Use for hypothesis tests.  
            
            #summary(HoldoutCVModel)
            #summary(HoldoutModel)        
          }
        
      }
      if(length(names)==0) B2Names <- c()
      write.csv(filteredholdout,paste0(sourceDir,yname,"hR-",holdoutReset,"rS-",resample,"filteredholdout.csv"))
    
      #end of resample
    }
    write.csv(filtered,paste0(sourceDir,yname,"hR-",holdoutReset,"rS-",resample,"filteredv1.csv"))
    write.csv(filteredv2,paste0(sourceDir,yname,"hR-",holdoutReset,"rS-",resample,"filteredv2.csv"))
    #end outermost loop
  }
#end of lister
}

#summary(full.model.train)
full.model.train <- glm(filteredv2.train[,1]~., data=filtered.train)
full.model.test <- glm(filteredv2holdout[,1]~., data=filteredv2holdout)

#give best model based on some metric
#step.model.train <- stepAIC(full.model.train, direction = "both", trace = FALSE)
#step.model.test <- stepAIC(full.model.test, direction = "both", trace = FALSE)
#as.character(rownames(data.frame(step.model.train$coefficients)))[-1:-2]
#as.character(rownames(data.frame(step.model.test$coefficients)))[-1:-2]

#V8517profile <- c("V8517","V7553","V7562","V7563","V7501","V7507")
V8517profile <- c("V8517","V7551","V8530","V8531","V8514","V8505")
#final after holdout is #8530 and 8531
filteredv8517 <- NewDF[,as.character(V8517profile)] %>% filter_all(all_vars(!is.na(.)))
filteredv8517[filteredv8517 == 0] <- NA
filteredv8517 <- filteredv8517 %>% filter_all(all_vars(!is.na(.)))
filteredv8517[filteredv8517 == -1] <- 0
filteredv8517.train <- filteredv8517
filteredv8517holdout <- NewDF.holdout[,as.character(V8517profile)] %>% filter_all(all_vars(!is.na(.)))
filteredv8517holdout[filteredv8517holdout == 0] <- NA
filteredv8517holdout <- filteredv8517holdout %>% filter_all(all_vars(!is.na(.)))
filteredv8517holdout[filteredv8517holdout == -1] <- 0
B28517 <- bestglm(Xy = cbind(data.frame(filteredv8517holdout[,-1]),data.frame(filteredv8517holdout[,1])), IC="CV", CVArgs=list(Method="HTF", K=3, REP=3), family=binomial)
B28517Names <- c("V8517",as.character(rownames(data.frame(B28517$BestModel$coefficients)))[-1])

filteredv8517v2 <- NewDF[,as.character(B28517Names)] %>% filter_all(all_vars(!is.na(.)))
filteredv8517v2[B28517Names == 0] <- NA
filteredv8517v2 <- filteredv8517v2 %>% filter_all(all_vars(!is.na(.)))
filteredv8517v2[filteredv8517v2 == -1] <- 0

write.csv(filteredv8517v2,paste0(sourceDir,"filteredv8517.csv"))

#seed 5
V7221profile <- c("V7221","V7202","V8517","V7206","V7215","V7552","V7562","V7563","V8527","V8512")
filteredv7221 <- NewDF[,as.character(V7221profile)] %>% filter_all(all_vars(!is.na(.)))
filteredv7221[filteredv7221 == 0] <- NA
filteredv7221 <- filteredv7221 %>% filter_all(all_vars(!is.na(.)))
filteredv7221[filteredv7221 == -1] <- 0
filteredv7221.train <- filteredv7221
filteredv7221holdout <- NewDF.holdout[,as.character(V7221profile)] %>% filter_all(all_vars(!is.na(.)))
filteredv7221holdout[filteredv7221holdout == 0] <- NA
filteredv7221holdout <- filteredv7221holdout %>% filter_all(all_vars(!is.na(.)))
filteredv7221holdout[filteredv7221holdout == -1] <- 0
B27221 <- bestglm(Xy = cbind(data.frame(filteredv7221holdout[,-1]),data.frame(filteredv7221holdout[,1])), IC="CV", CVArgs=list(Method="HTF", K=10, REP=1), family=binomial)
B27221Names <- c("V7221",as.character(rownames(data.frame(B27221$BestModel$coefficients)))[-1])
print(B27221Names)

filteredv7221v2 <- NewDF[,as.character(B27221Names)] %>% filter_all(all_vars(!is.na(.)))
filteredv7221v2[B27221Names == 0] <- NA
filteredv7221v2 <- filteredv7221v2 %>% filter_all(all_vars(!is.na(.)))
filteredv7221v2[filteredv7221v2 == -1] <- 0

write.csv(filteredv7221v2,paste0(sourceDir,base,"filteredv7221v2.csv"))
write.csv(filteredv7221,paste0(sourceDir,base,"filteredv7221v1.csv"))

#V7118
{
 
  resv7118 <- cor(filteredv7118.train)
  corrplot(resv7118)
  
  x=filteredv7118.train[,-1]
  y=filteredv7118.train[,1]
  
  pc <- prcomp(filteredv7118.train[,-1], center=TRUE, scale=TRUE)
  
  #includes proportion of variance
  summary(prcomp(filteredv7118.train[,-1], center=TRUE, scale=TRUE))
  te <- summary(prcomp(filteredv7118.train[,-1], center=TRUE, scale=TRUE))$importance
  #pc plot
  plot(te[3,1:ncol(te)])
  
  corrplot(cor(cbind(filteredv7118.train[,1],prcomp(filteredv7118[,-1], center=TRUE, scale=TRUE)$x)))
  
  #include data in new model for inclusion in a linear model
  #https://stats.stackexchange.com/questions/72839/how-to-use-r-prcomp-results-for-prediction
  
  pcaModel<- glm(y~pc$x[,1:length(data.frame(pc$x))])
  
  #predict using pca, just re-applying to training data.
  
  #applied PCA to holdout
  
  x <- filteredv7118holdout[-1]
  
  y <- data.frame(filteredv7118holdout[1])

  pred <- data.frame(predict(pc,x))
  pcaPred <- glm(cbind(y,pred))
  
  #predict(pcaPred,)
  
  #predict(pcaPred,filteredv7133holdout[-1])
  
  summary(pcaPred)
  hist(pcaPred$residuals)
  
  summary(pcaModel)
  summary(pcaPred)
  
  regularTrainModel <- glm(filteredv7118.train)
  regularTestModel <- glm(filteredv7118holdout)
  
  # Define training control
  
  #http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/#k-fold-cross-validation
  
  trainModel <- train(filteredv7118.train[-1], as.factor(filteredv7118.train[,1]), method = "glm",trControl = train.control)
  testModel <- train(filteredv7118holdout[-1], as.factor(filteredv7118holdout[,1]), method = "glm",trControl = train.control)
  
  testPredCV <- predict.train(model,newdata=filteredv7118holdout[,-1])
  
  testPred <- predict.glm(regularModel,filteredv7118holdout[,-1])
  cor(testPred,filteredv7118holdout[,1])
  
  #predict(regulardModel,)
  testPredResid <- (testPredCV-filteredv7118holdout[,1])
  
  count(abs(testPredResid)>.25)
  
  testModel <- glm(filteredv7118holdout)
  summary(trainModel)
  summary(testModel)
  
  summary(regularTrainModel)
  summary(regularTestModel)
  
  #%incorrect
  count(abs(testModel$residuals)>.25)$freq[2]/length(testModel$residuals)
  
 
}

#http://rstudio-pubs-static.s3.amazonaws.com/413041_9289a50ccb0e4f4ab84b22b6b1f4ac4f.html
holdoutmodelcv <- train(filteredv7118holdout[-1], filteredv7118holdout[,1], method = "glm", trControl = train.control)
holdoutmodelcv$results
summary(holdoutmodelcv$finalModel)
vif(holdoutmodelcv$finalModel)
#plot(holdoutmodelcv$finalModel)

#check errors
pcv <- predict(holdoutmodelcv, filteredv7118holdout[-1])
errorcv <- (pcv- filteredv7118holdout[,1])
RMSE_NewDatacv <- sqrt(mean(errorcv^2))

#check errors against training
pct <- predict(holdoutmodelcv, filteredv7118.train[-1])
errorcv <- (pct- filteredv7118.train[,1])
RMSE_NewDatacv <- sqrt(mean(errorcv^2))

#8517
{
  resv8517 <- cor(filteredv8517.train)
  corrplot(resv8517)
  
  x=filteredv8517.train[,-1]
  y=filteredv8517.train[,1]
  
  pc <- prcomp(filteredv8517.train[,-1], center=TRUE, scale=TRUE)
  
  #includes proportion of variance
  summary(prcomp(filteredv8517.train[,-1], center=TRUE, scale=TRUE))
  te <- summary(prcomp(filteredv8517.train[,-1], center=TRUE, scale=TRUE))$importance
  #pc plot
  plot(te[3,1:ncol(te)])
  
  corrplot(cor(cbind(filteredv8517.train[,1],prcomp(filteredv8517[,-1], center=TRUE, scale=TRUE)$x)))
  
  #include data in new model for inclusion in a linear model
  #https://stats.stackexchange.com/questions/72839/how-to-use-r-prcomp-results-for-prediction
  
  pcaModel<- glm(y~pc$x[,1:length(data.frame(pc$x))])
  
  #predict using pca, just re-applying to training data.
  
  #applied PCA to holdout
  
  x <- filteredv8517holdout[-1]
  #View(x)
  y <- data.frame(filteredv8517holdout[1])
  
  pred <- data.frame(predict(pc,x))
  pcaPred <- glm(cbind(y,pred))
  
  #predict(pcaPred,)
  
  #predict(pcaPred,filteredv7133holdout[-1])
  
  summary(pcaPred)
  hist(pcaPred$residuals)
  
  summary(pcaModel)
  summary(pcaPred)
  
  regularModel <- glm(filteredv8517.train)
  
  testPred <- predict.glm(regularModel,filteredv8517holdout[,-1])
  
  #cor(testPred,filteredv8517holdout[,1])
  #predict(regulardModel,)
  testPredResid <- (testPred-filteredv8517holdout[,1])
  
  count(abs(testPredResid)>.25)
  
  testModel <- glm(filteredv8517holdout)
  summary(regularModel)
  summary(testModel)
  hist(testModel$residuals)
  
  testModel
  
  summary(regularModel)
  
  #%incorrect
  count(abs(testModel$residuals)>.25)$freq[2]/length(testModel$residuals)
}

#V7221
{
  resv7221 <- cor(filteredv7221.train)
  corrplot(resv7221)
  
  x=filteredv7221.train[,-1]
  y=filteredv7221.train[,1]
  
  pc <- prcomp(filteredv7221.train[,-1], center=TRUE, scale=TRUE)
  
  #includes proportion of variance
  summary(prcomp(filteredv7221.train[,-1], center=TRUE, scale=TRUE))
  te <- summary(prcomp(filteredv7221.train[,-1], center=TRUE, scale=TRUE))$importance
  #pc plot
  plot(te[3,1:ncol(te)])
  
  corrplot(cor(cbind(filteredv7221.train[,1],prcomp(filteredv7221[,-1], center=TRUE, scale=TRUE)$x)))
  
  #include data in new model for inclusion in a linear model
  #https://stats.stackexchange.com/questions/72839/how-to-use-r-prcomp-results-for-prediction
  
  pcaModel<- glm(y~pc$x[,1:length(data.frame(pc$x))])
  
  #predict using pca, just re-applying to training data.
  
  #applied PCA to holdout
  
  x <- filteredv7221holdout[-1]
  #View(x)
  y <- data.frame(filteredv7221holdout[1])
  
  pred <- data.frame(predict(pc,x))
  pcaPred <- glm(cbind(y,pred))
  
  #predict(pcaPred,)
  
  #predict(pcaPred,filteredv7133holdout[-1])
  
  summary(pcaPred)
  hist(pcaPred$residuals)
  
  summary(pcaModel)
  summary(pcaPred)
  
  regularModel <- glm(filteredv7221.train)
  
  testPred <- predict.glm(regularModel,filteredv7221holdout[,-1])
  #predict(regulardModel,)
  testPredResid <- (testPred-filteredv7221holdout[,1])
  
  count(abs(testPredResid)>.25)
  
  testModel <- glm(filteredv7221holdout)
  summary(regularModel)
  summary(testModel)
  hist(testModel$residuals)
  
  testModel
  
  summary(regularModel)
  
  #%incorrect
  count(abs(testModel$residuals)>.25)$freq[2]/length(testModel$residuals)
}
