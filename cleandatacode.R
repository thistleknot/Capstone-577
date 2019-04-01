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

library(MASS)

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

write.csv(d_combined,paste0(sourceDir,"combined.csv"))

data <- read.csv(paste0(sourceDir,"combined.csv"), header=TRUE, sep=,)

list<-read.csv(paste0(sourceDir,"filterList.txt"), header=FALSE, sep=,)

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
View(na_count(NewDF))

table(is.na(NewDF))
write.csv(NewDF,paste0(sourceDir,"filtered.csv"))

library(corrplot)
#colnames(NewDF)

summary(NewDF)

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

set.seed(5)

y <- c()
#y iterator's
#iterator=1
  for (iterator in 1:sum(yIndex))
{
  y <- list[yIndex,][iterator,]
  #y
  #yname <- as.character(list[yIndex,][iterator,][,1])
  print(as.character(list[yIndex,][iterator,][,1]))
  #val = 1
  
  #categories
  for (val in 1:9)
    #val=8
  {
    #val = 3
    if (val == 1) colList <- list[lGeographyIndex,]
    if (val == 2) colList <- list[lGenderIndex,]
    if (val == 3) colList <- list[lGPAIndex,]
    if (val == 4) colList <- list[lViolenceIndex,]
    if (val == 5) colList <- list[lFather1Index,]
    if (val == 6) colList <- list[lFather2Index,]
    if (val == 7) colList <- list[lHabitsIndex,]
    if (val == 8) colList <- list[lHealthIndex,]
    if (val == 9) colList <- list[lPsycheIndex,]
    y <- list[yIndex,][iterator,]
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
    
    temp <- NewDF[,newList]
    #colnames(temp) <- paste(newList[,1],newList[,3])
    temp[temp == 0] <- NA
    trows <- nrow(temp)
    #% na's
    colSums(is.na(temp))/trows

    #drop na's
    #https://stackoverflow.com/questions/4862178/remove-rows-with-all-or-some-nas-missing-values-in-data-frame
    templist <- temp %>% filter_all(all_vars(!is.na(.)))
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

    data <- templist
    nrFolds <- 10
    
    # generate array containing fold-number for each sample (row)
    folds <- rep_len(1:nrFolds, nrow(data))
    
    folds <- sample(folds, nrow(data))
    
    #https://swcarpentry.github.io/r-novice-inflammation/15-supp-loops-in-depth/
    reduced <- c()
    reduced <- c(numeric(length(data.train[,-1])),numeric(length = nrFolds))
    
    #https://www.tutorialspoint.com/r/r_arrays.htm
    vector1 <- length(data.train[,-1])
    vector2 <- numeric(length = nrFolds)
    
    #result <- array(c(vector1,vector2),dim = c(nrFolds,length(data.train[,-1]),1))
    #result <- array(c(vector1,vector2),dim = c(nrFolds,length(data.train[,-1]),1))
    #result[1:9]
    #result <- c()

    names <- c()
    namest <- c()
    
    widthSize <- ncol(data.frame(data.train[,-1]))
    width <- numeric(length = widthSize)
    #width <- numeric(length = ncol(data.frame(data.train[,-1])))
    #klist <- array(c(0,0,0),dim=c(nrFolds,widthSize))
    klist <- array(numeric(length = ncol(data.frame(data.train[,-1]))),dim=c(nrFolds,ncol(data.frame(data.train[,-1]))))
    #array(width,dim=c(nrFolds,widthSize))
    
    #print(colnamesy)
    # actual cross validation
    #k=1
    for(k in 1:nrFolds) {

      # actual split of the data
      fold <- which(folds == k)
      
      data.train <- data[-fold,]
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
      full.model.train <- lm(data.train[,1]~., data=data.train)
      full.model.test <- lm(data.test[,1]~., data=data.test)
      
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
        if(testCaseTrain$message=="AIC is -infinity for this model, so 'stepAIC' cannot proceed") {
            #names <- rownames(data.frame(step.model.train$coefficients[-1:-2]))
            
            #http://combine-australia.github.io/r-novice-gapminder/05-data-structures-part2.html
            #https://community.rstudio.com/t/type-error-after-rbind/16866/2
            names <- as.character(data.frame(c1 = as.factor(rownames(data.frame(step.model.train$coefficients[-1:-2]))))$c1)
            namest <- data.frame(rbind(namest,names))[,,drop=FALSE]            
            #print(k)
            #print(names)
            print("breaking")
            break
          }
      }
      
      step.model.train <- step.model.train <- stepAIC(full.model.train, direction = "both", trace = FALSE)

      testCase <- tryCatch(stepAIC(full.model.test, direction = "both", trace = FALSE), error = function(e) e)
      if(!is.null(testCase$message))
      {
        if(testCase$message=="AIC is -infinity for this model, so 'stepAIC' cannot proceed") 
        {
          names <- as.character(rownames(data.frame(step.model.train$coefficients[-1:-2])))
          namest <- data.frame(rbind(namest,names))[,,drop=FALSE]           
          print("breaking")
          break
        }
      }

      step.model.test <- stepAIC(full.model.test, direction = "both", trace = FALSE)
      #class(testCase)
      
      #if(testCase$message!="AIC is -infinity for this model, so 'stepAIC' cannot proceed")

      #reduced[,k] <- data.frame((step.model.train$coefficients))
      
      #drop 1st intercept and y
      #names <- rownames(data.frame(step.model.train$coefficients[-1:-2]))
      #trying to populate a list.

      #if (length(names)==0) names <- width
      
      if(is.null(testCaseTrain$message)&&is.null(testCaseTrain$message))
      {
        names <- as.character(rownames(data.frame(step.model.train$coefficients[-1:-2])))
        namest <- data.frame(rbind(namest,names))[,,drop=FALSE]
      
        if(length(names) > 0) {
          print(names)
        }
      }

      #length(names)
      #klist[k,][1:length(names)] <- ifelse(is.na(klist[k,][1:length(names)]), names, klist[k,][1:length(names)])
      
      #type conversion tricks
      #castNamesDF <- data.frame(klist[k,][1:length(names)])
      #castNamesDF2 <- data.frame(names[1:length(names)])
      
      #castNamesDF <- castNamesDF2
      
      #names <- as.character(castNamesDF[,1])
      #klist[k,][1:length(names)] <- names)
      #print(k)
      #print(names)
      #if(k==1) namest <- names
      #if(!k==1) 

      #testCase <- tryCatch(klist[k,][1:length(names)] <- names), error = function(e) e)
      
      #if(!is.null(testCase$message))
      #{
        #if(testCase$message=="Error in klist[k, ] <- `*vtmp*` : 
      #number of items to replace is not a multiple of replacement length") {
          #k=1
          #klist <- array(numeric(length = ncol(data.frame(data.train[,-1]))),dim=c(nrFolds,ncol(data.frame(data.train[,-1]))))
          #break
        #}
      #}
      
      #library(zoo)
      
      #print(names)
      #len(names)
      
      #result[k] <- names

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

      #PCA
      #head(iris)
      x<-data.train[,-1]
      y<-data.train[,1]
      #head(d)
      pc <- prcomp(x)
      #pc <- princomp (x, cor=TRUE, score=TRUE)
      #pc$x[,1]
  
      pcaModel<- lm(y~pc$x[,1:length(data.frame(x))])
      
    }
    
    #https://stackoverflow.com/questions/18958948/counting-zeros-in-columns-in-data-frame-in-r-and-express-as-percentage
    #lapply(klist, function(x){ length(which(x==0))/length(x)})    
    #print(klist)
    
    #not aggregating correctly
    #print(namest)
    #View(namest)
    #print(result)
    
    #reduced[1]
    #result
    #summary(NewDF)
  
  }
  
}

filteredSubset <- rbind(list[lHabitsIndex,],list[lHealthIndex,],list[lPsycheIndex,],list[lGPAIndex,],list[lGenderIndex,])
filtered <- NewDF[,as.character(filteredSubset[,1])] %>% filter_all(all_vars(!is.na(.)))
colnames(filtered) <- suppressMessages(paste(as.character(join(filteredSubset,list[,c(1,3)])[,3, drop=TRUE]),as.character(join(filteredSubset,list[,c(1,3)])[,1, drop=TRUE])))
res2 <- cor(filtered)
corrplot(res2)
#special subset of Habits, Health, and Psyche

