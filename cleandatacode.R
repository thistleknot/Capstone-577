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

pw <- {"Read1234"}

sourceDir="C:/Users/user/Documents/School/CSUF/ISDS577/projects/Capstone-577/"

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

#list<-read.csv(paste0(sourceDir,"altList.txt"), header=FALSE, sep=,)
#list<-read.csv(paste0(sourceDir,"gangfight.txt"), header=FALSE, sep=,)
list<-read.csv(paste0(sourceDir,"reducedfilterlist.txt"), header=FALSE, sep=,)

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
pg <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
conn = dbConnect(drv=pg
                 ,user="postgres"
                 ,password="Read1234"
                 ,host="localhost"
                 ,port=5432
                 ,dbname="analyticplatform"
)

dbExistsTable(conn, "temp_table_data")

NewDF <- data[,(c(col.num))]

#V7589 empty

#https://stat.ethz.ch/R-manual/R-devel/library/base/html/system.html
#https://stackoverflow.com/questions/32015333/executing-a-batch-file-in-an-r-script
#shell.exec("\\\\network\\path\\file.bat")
#db_drop_table(conn, "temp_table_data", force = TRUE)

#https://stackoverflow.com/questions/12797909/creating-temp-table-from-a-data-frame-in-r-using-rpostgresql
dbWriteTable(conn, "temp_table_data", NewDF, temp.table=TRUE)

#https://www.r-bloggers.com/getting-started-with-postgresql-in-r/
df_postgres <- dbGetQuery(conn, "SELECT * from temp_table_data")

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

#setup holdout

#static holdout
holdoutSize = .05

#proportion of nonHoldout (i.e. nonholdout: 1-holdoutSize) to use for model building, i.e. sample size.  Holdout can be tuned independently kind of.
preTrainSize = .05

#monte carlo sample size that samples from the preTrain.
#considering that we are doing at least 10 outer loops, 
#it doesn't make sense to oversaturate by having a large sample size since (i.e. 25% x 10 = 250% coverage) we're already cross validating at the lower level.  
#In other words we want to exchaust small samples.
trainMCSize = .25

base = 5
set.seed(base)

#https://adv-r.hadley.nz/subsetting.html
holdout <- sample(nrow(NewDF), round(holdoutSize*nrow(NewDF)))

NewDF.holdout <- NewDF[holdout, ]
NewDf.nonHoldout <-NewDF[-holdout, ]

preTrain <- sample(nrow(NewDf.nonHoldout), round(preTrainSize*nrow(NewDf.nonHoldout)))

NewDF.preTrain <- NewDf.nonHoldout[preTrain,]

#filtered <- NewDF[complete.cases(NewDF), ]

#https://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-inner-outer-left-right
#merge(colList, list, by = "V1")[,2]

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

#outer=8
#still iterates over a subset of the data that is not the holdout, which is important
for (outer in 1:20)
{
  #Monte Carlo sample from non holdout, preTrain is !holdout
  print(outer)
  
  set.seed(base+outer)
  
  NewDF.train <- NewDF.preTrain[sample(nrow(NewDF.preTrain), round(trainMCSize*nrow(NewDF.preTrain))),]

    y <- c()
    #y iterator's
    #iterator=2
    for (iterator in 1:sum(yIndex))
    {
      y <- list[yIndex,][iterator,]
      alty <- list[yIndex,][-iterator,]
      #y
      #yname <- as.character(list[yIndex,][iterator,][,1])
      print(as.character(list[yIndex,][iterator,][,1]))
      #val = 10
      
      #categories
      for (val in 1:10)
        #val=8
      {
        #val = 2
        if (val == 1) colList <- list[lGeographyIndex,]
        if (val == 2) colList <- list[lGenderIndex,]
        if (val == 3) colList <- list[lGPAIndex,]
        if (val == 4) colList <- list[lViolenceIndex,]
        if (val == 5) colList <- list[lFather1Index,]
        if (val == 6) colList <- list[lFather2Index,]
        if (val == 7) colList <- list[lHabitsIndex,]
        if (val == 8) colList <- list[lHealthIndex,]
        if (val == 9) colList <- list[lPsycheIndex,]
        if (val == 10) colList <- alty
        
        
        if (is.null(nrow(data.frame(alty)))) break
        
        #colList <- rbind(list[yIndex,],colList)
        colList <- rbind(y,colList)
        
        #https://stackoverflow.com/questions/17878048/merge-two-data-frames-while-keeping-the-original-row-order
        #https://stackoverflow.com/questions/28311293/how-to-make-join-operations-in-dplyr-silent
        colListNames <- suppressMessages(paste(join(colList,list)[,1],join(colList,list)[,3]))
        
        #colList <- c()
        #colList <- join(colList,list)
    
        #c(join(colList,list)[,1])
        #join,then only use 1st column
        newList <-  suppressMessages(as.character(join(colList,list[,c(1,3)])[,1, drop=TRUE]))
        
        #https://stat.ethz.ch/R-manual/R-devel/library/base/html/droplevels.html
        #droplevels(newList)
        #https://stackoverflow.com/questions/34469178/r-convert-factor-to-numeric-and-remove-levels
        
        #colnames(NewDF.train)
        temp <- NewDF.train[,newList]
        #colnames(temp) <- paste(newList[,1],newList[,3])
        temp[temp == 0] <- NA
        trows <- nrow(data.frame(temp))
        #% na's
        colSums(is.na(data.frame(temp)))/trows
    
        #drop na's
        #https://stackoverflow.com/questions/4862178/remove-rows-with-all-or-some-nas-missing-values-in-data-frame
        templist <- data.frame(temp) %>% filter_all(all_vars(!is.na(.)))
        #nrow(tempList)
        #NewDF[,"V7101"]
        #colnames(NewDF)
        
        if (val == 1)
        {
          colnames(templist)
          templist[,"V507NE"] <- templist[,"V507"] == 1
          templist[,"V507NE"][templist[,"V507NE"] == 0] <- -1
          templist[,"V507NC"] <- templist[,"V507"] == 2
          templist[,"V507NC"][templist[,"V507NC"] == 0] <- -1
          templist[,"V507W"] <- templist[,"V507"] == 4
          templist[,"V507W"][templist[,"V507W"] == 0] <- -1
    
          table(is.na(templist))
          
          drop <- c("V507")
          templistNoGeo = templist[,!(names(templist) %in% drop)]
          colnames(templistNoGeo)
          templist <- c()
          templist <- templistNoGeo
          #colnames(templist) <- suppressMessages(paste(join(templist,list)[,1],join(templist,list)[,3]))
        }
    
        #partition data before PCA 
        #https://stats.stackexchange.com/questions/61090/how-to-split-a-data-set-to-do-10-fold-cross-validation
        
        #templist[,as.character(y[,1])]
        #use filtered categorical data
    
        data <- templist
        
        data[data == -1] <- 0
        #data[data == 1] <- 1
        
        nrFolds <- 10
        
        #https://github.com/thistleknot/FredAPIR/blob/master/regression_analysis.R
        #cv.errors=matrix(NA,nrFolds,k, dimnames=list(NULL, paste(1:length(data))))
        if((length(data)-1)==0) break
        cv.names=matrix(NA,nrFolds,(length(data)-1), dimnames=list(NULL, paste(1:(length(data)-1))))
        cvagg.names=c()
        colnames(cv.names) <- colnames(data[-1])
        
        # generate array containing fold-number for each sample (row)
        folds <- rep_len(1:nrFolds, nrow(data))
        folds <- sample(folds, nrow(data))
        
        #https://swcarpentry.github.io/r-novice-inflammation/15-supp-loops-in-depth/
        reduced <- c()
        reduced <- c(numeric(length(data[,-1])),numeric(length = nrFolds))
        
        names <- c()
        namest <- data[0,-1]
        
        #empty column set
        #https://stackoverflow.com/questions/6764756/how-to-copy-an-objects-structure-but-not-the-data
        colnames(namest) <- colnames(data[0,-1])
        
        widthSize <- ncol(data.frame(data[,-1]))
        width <- numeric(length = widthSize)
        #width <- numeric(length = ncol(data.frame(data.train[,-1])))
        #klist <- array(c(0,0,0),dim=c(nrFolds,widthSize))
        klist <- array(numeric(length = ncol(data.frame(data[,-1]))),dim=c(nrFolds,ncol(data.frame(data[,-1]))))
        #array(width,dim=c(nrFolds,widthSize))
        
        #print(colnamesy)
        # actual cross validation
        #k=7
        #11th fold is holdout set used for testing models
        for(k in 1:nrFolds) {
          #colnames(namest) <- 
          #namest <- data.frame(rbind(namest,names))[,,drop=FALSE]            
    
          # actual split of the data
          fold <- which(folds == k)
          
          if(k!=nrFolds) trainfold <- which(folds == k)
          if(k==nrFolds) trainfold <- which(folds == 1)
        
          data.train <- data[-fold,]
          #data.train <- data[trainfold,]
          
          data.test <- data[fold,]
          
          full.model <- lm(data.train)
          
          #full.model <- lm(y=data.train[,1], x=data.train[,-1])
          
          #significance
          #https://stat.ethz.ch/pipermail/r-help/2005-December/084308.html
          
          #http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/
      
          # Fit the full model 
    
          # Stepwise regression model
          
          #stepwise regression
          #step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
          
          summary(full.model)
          lm.res <- summary(full.model)
          coef(lm.res)[,4]
          #View(full.model)
          
          #lm(y~pc$sdev[1:3]
          
          #data.validation <- data.test[1:as.integer(nrow(data.test)/2),]
          #data.test <- data.pretest[as.integer(nrow(data.test)/2)+1:,]
          
          res <- cor(data.train)
          corrplot(res)
          
          # train and test your model with data.train and data.test
          
          #Regression model
          #ncol(data.train)
          #colnames(data.train)
          #View(data.train)
          full.model.train <- glm(data.train[,1]~., data=data.train)
          #summary(full.model.train)
          full.model.test <- glm(data.test[,1]~., data=data.test)
          
          #basically, if both of these are significant, keep the coefficient
          coef(summary(full.model.train))[,4][-1:-2]
          
          #https://stackoverflow.com/questions/14205583/filtering-data-in-a-dataframe-based-on-criteria
          #test2sig <- coef(summary(full.model.test))[,4][-1:-2]
          #
          #Due to NaN in coefficient terms for p, along with -AIC, disabling this check for test2sig.  Else test2sig needs to check if the terms are Na or Nil or NAN
          #test2sigNames <- row.names(data.frame(test2sig[test2sig < .05]))
          
          #full.model.train$
        
          #full.model <- lm(y=data.train[,1], x=data.train[,-1])
          
          #significance()
          #https://stat.ethz.ch/pipermail/r-help/2005-December/084308.html
          
          #http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/
          
          # Fit the full model 
          
          # Stepwise regression model
          
          #stepwise regression
          #step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
          
          #try statement is expensive, but an if statement similar to what I do for the rownames just below... might not be
    
          testCaseTrain <- tryCatch(step.model.train <- stepAIC(full.model.train, direction = "both", trace = FALSE), error = function(e) e)
          
          if(!is.null(testCaseTrain$message))
          {
            
            if(testCaseTrain$message=="AIC is -infinity for this model, so 'stepAIC' cannot proceed"||testCaseTrain$message=="<text>:1:5: unexpected numeric constant\n1: ~ . NA\n        ^"||testCaseTrain$message=="undefined columns selected") {
                #names <- rownames(data.frame(step.model.train$coefficients[-1:-2]))
                
                #http://combine-australia.github.io/r-novice-gapminder/05-data-structures-part2.html
                #https://community.rstudio.com/t/type-error-after-rbind/16866/2
                
                #names <- test2sigNames
                names <- as.character(colnames(templist)[-1])
                
                #names <- as.character(data.frame(c1 = as.factor(rownames(data.frame(step.model.train$coefficients[-1:-2]))))$c1)
                #rbind(names,namest)
                
                #print(colnames(y))
                
                tempy <- as.character(colnames(templist)[1]) 
                tempxs <- names
                
                parse.model.test <- glm(data.test[,c(tempy,names)])
                
                #names2 <- as.character(data.frame(c1 = as.factor(rownames(data.frame(parse.model.test$coefficients[-1:-2]))))$c1)
                names2 <- tempxs
                
                #colnames(y[1])
                
                #subset(tempxs, select=-c(colnames(y)))
                
                #questionable fix
                #if(length(names2)==0){cv.names[k,names2[h]]=tempxs[h]}
                #if(!length(names2)==0){
                if(!length(names2)==0)
                {
                  for(h in 1:length(names2)) {
                    
                    if(!is.na(names2[h])) cv.names[k,names2[h]]=names2[h]
                    #cv.names[k,names2[h]]=tempxs[h]
                    
                  }
                  
                  
                }
                
                #}
                
                namest <- data.frame(rbind(namest,names2))[,,drop=FALSE]  
                
                #if(length(names2)>0) for(h in 1:length(names2)) {cv.names[k,names2[h]]=names2[h]}
                
                #if(length(names2) > 0) print(names2)
                #print("breaking")
                #break
              }
          }
          
          #using best subset model selection
          #http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/
          
          #sometimes will skip (i.e. all na) due to large # of same values I suppose, so I added (step.model.train$aic<10000) to check
          if(is.null(testCaseTrain$message)) step.model.train <- stepAIC(full.model.train, direction = "both", trace = FALSE)
    
          testCase <- tryCatch(stepAIC(full.model.test, direction = "both", trace = FALSE), error = function(e) e)
          
          #undefined columns selected happens when AIC hits -INF, we want low AIC
          
          if(!is.null(testCase$message))
          {
            if(testCase$message=="AIC is -infinity for this model, so 'stepAIC' cannot proceed"||testCase$message=="undefined columns selected"||step.model.train$aic<10000||testCase$message=="<text>:1:5: unexpected numeric constant\n1: ~ . NA\n        ^")
            {
              #names <- as.character(rownames(data.frame(step.model.train$coefficients[-1:-2])))
              names <- as.character(colnames(templist)[-1])
    
              tempy <- as.character(colnames(templist)[1])
              tempxs <- names
              
              parse.model.test <- glm(data.test[,c(tempy,names)])
              
              testCase <- tryCatch(stepAIC(full.model.test, direction = "both", trace = FALSE), error = function(e) e)
              
              names2 <- as.character(data.frame(c1 = as.factor(rownames(data.frame(parse.model.test$coefficients[-1:-2]))))$c1)
              namest <- data.frame(rbind(namest,names2))[,,drop=FALSE]
              
              #if(length(names2)>0) for(h in 1:length(names2)) {cv.names[k,names2[h]]=names2[h]}
              
              #if(length(names2) > 0) print(names2)
              
              #print("breaking")
              #break
            }
          }
    
          if(is.null(testCase$message)) step.model.test <- stepAIC(full.model.test, direction = "both", trace = FALSE)
    
          if(is.null(testCase$message)&&is.null(testCaseTrain$message))
          {
            names <- as.character(rownames(data.frame(step.model.train$coefficients[-1:-2])))
            
            if(length(names) > 0) {
    
              tempy <- as.character(colnames(templist)[1])
              tempxs <- names
              
              parse.model.test <- glm(data.test[,c(tempy,names)])
              
              names2 <- as.character(data.frame(c1 = as.factor(rownames(data.frame(parse.model.test$coefficients[-1:-2]))))$c1)
              namest <- data.frame(rbind(namest,names2))[,,drop=FALSE]
              
              #if(length(names2)>0) for(h in 1:length(names2)) {cv.names[k,names2[h]]=names2[h]}
              
              #if(length(names2) > 0) print(names2)
              
            }
            if(length(names)==0)
            {
              names <- c()
              tempxs <- c()
              
              tempy <- as.character(colnames(templist)[1])
              
              names2 <- names
              namest <- data.frame(rbind(namest,names2))[,,drop=FALSE]
              
              #colnames(data)[-1]
            }
          }
          
          if(length(names2)>0) for(h in 1:length(names2)) {cv.names[k,names2[h]]=names2[h]}
    
          summary(step.model.test) 
    
          #Calculating MSE for training data
          mse.train<- mean(residuals(step.model.train)^2)
          #mse.train
          
          #Calculating RMSE for training data
          rmse.train <- sqrt(mse.train)
          #rmse.train
          
          #Calculating MSE for testing data
          mse.test <- mean(residuals(step.model.test)^2)
          #mse.test
          
          #Calculating RMSE for testing data
          rmse.test <- sqrt(mse.test)
          #rmse.test
          
        }
        
        #outputting results
        #write.csv(cv.names,paste0(sourceDir,as.character(y[,1]),as.character(cv.names[1,1]),".csv"))
        
        #http://r.789695.n4.nabble.com/How-to-delete-only-those-rows-in-a-dataframe-in-which-all-records-are-missing-td3990418.html
        
        if(!sum(rowSums(is.na(data.frame(cv.names))))==(NCOL(data.frame(cv.names))*NROW(data.frame(cv.names)))) 
          {
            #https://stackoverflow.com/questions/4986101/counting-non-nas-in-a-data-frame-getting-answer-as-a-vector
            #print(data.frame(cv.names)[!(rowSums(is.na(data.frame(cv.names)))==NCOL(data.frame(cv.names))),])
            if(k!=1){
              #cvagg.names <- cbind(cvagg.names,cv.names)
              print(colSums(!is.na(data.frame(cv.names))))  
            }
          if(k==1) print(colSums(!is.na(data.frame(cv.names))))
            
          
          }
    
        #https://stackoverflow.com/questions/18958948/counting-zeros-in-columns-in-data-frame-in-r-and-express-as-percentage
        #lapply(klist, function(x){ length(which(x==0))/length(x)})    
        #print(klist)
        
        #not aggregating correctly
        #colnames(namest) <- colnames(data.train[0,-1])
        #print(namest)
        #View(namest)
        #print(result)
        
        #reduced[1]
        #result
        #summary(NewDF)
      
      }
      
      
    }
}

V7118profile <- c("V7118","V7552","V7553","V7562","V8527","V8528","V8529","V8530","V8531","V8509","V8512","V8514","V8565")
filteredv7118 <- NewDF[,as.character(V7118profile)] %>% filter_all(all_vars(!is.na(.)))
filteredv7118[filteredv7118 == 0] <- NA
filteredv7118 <- filteredv7118 %>% filter_all(all_vars(!is.na(.)))
filteredv7118[filteredv7118 == -1] <- 0
filteredv7118.train <- filteredv7118
filteredv7118holdout <- NewDF.holdout[,as.character(V7118profile)] %>% filter_all(all_vars(!is.na(.)))
filteredv7118holdout[filteredv7118holdout == 0] <- NA
filteredv7118holdout <- filteredv7118holdout %>% filter_all(all_vars(!is.na(.)))
filteredv7118holdout[filteredv7118holdout == -1] <- 0
write.csv(filteredv7118,paste0(sourceDir,"filteredv7118.csv"))

V8517profile <- c("V8517","V7553","V7562","V7563","V7501","V7507")
filteredv8517 <- NewDF[,as.character(V8517profile)] %>% filter_all(all_vars(!is.na(.)))
filteredv8517[filteredv8517 == 0] <- NA
filteredv8517 <- filteredv8517 %>% filter_all(all_vars(!is.na(.)))
filteredv8517[filteredv8517 == -1] <- 0
filteredv8517.train <- filteredv8517
filteredv8517holdout <- NewDF.holdout[,as.character(V8517profile)] %>% filter_all(all_vars(!is.na(.)))
filteredv8517holdout[filteredv8517holdout == 0] <- NA
filteredv8517holdout <- filteredv8517holdout %>% filter_all(all_vars(!is.na(.)))
filteredv8517holdout[filteredv8517holdout == -1] <- 0
write.csv(filteredv8517,paste0(sourceDir,"filteredv8517.csv"))

V7221profile <- c("V7221","V7553","V7562","V8530","V8531","V7501","V8565")
filteredv7221 <- NewDF[,as.character(V7221profile)] %>% filter_all(all_vars(!is.na(.)))
filteredv7221[filteredv7221 == 0] <- NA
filteredv7221 <- filteredv7221 %>% filter_all(all_vars(!is.na(.)))
filteredv7221[filteredv7221 == -1] <- 0
filteredv7221.train <- filteredv7221
filteredv7221holdout <- NewDF.holdout[,as.character(V7221profile)] %>% filter_all(all_vars(!is.na(.)))
filteredv7221holdout[filteredv7221holdout == 0] <- NA
filteredv7221holdout <- filteredv7221holdout %>% filter_all(all_vars(!is.na(.)))
filteredv7221holdout[filteredv7221holdout == -1] <- 0
write.csv(filteredv7221,paste0(sourceDir,"filteredv7221.csv"))

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
  #View(x)
  y <- data.frame(filteredv7118holdout[1])
  
  pred <- data.frame(predict(pc,x))
  pcaPred <- glm(cbind(y,pred))
  
  #predict(pcaPred,)
  
  #predict(pcaPred,filteredv7133holdout[-1])
  
  summary(pcaPred)
  hist(pcaPred$residuals)
  
  summary(pcaModel)
  summary(pcaPred)
  
  regularModel <- glm(filteredv7118.train)
  
  # Define training control
  
  #http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/#k-fold-cross-validation
  train.control <- trainControl(method = "repeatedcv", 
                                number = 10, repeats = 3)
  # Train the model
  
  model <- train(filteredv7118.train[-1], as.factor(filteredv7118.train[,1]), method = "glm",
                 trControl = train.control)
  # Summarize the results
  #print(model)
  #summary(model)  
  
  testPredCV <- predict.train(model,newdata=filteredv7118holdout[,-1])
  
  testPred <- predict.glm(regularModel,filteredv7118holdout[,-1])
  cor(testPred,filteredv7118holdout[,1])
  
  #predict(regulardModel,)
  testPredResid <- (testPredCV-filteredv7118holdout[,1])
  
  count(abs(testPredResid)>.25)
  
  testModel <- glm(filteredv7118holdout)
  summary(regularModel)
  summary(testModel)
  hist(testModel$residuals)
  
  testModel
  
  summary(regularModel)
  
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
