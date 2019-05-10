#rbind.fill
library(plyr)
#replace.value
library(anchors)

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

unzip(zipF,exdir=outDir)

d_2012 <- read.csv(paste0(sourceDir,"34574-0001-Data.csv"), header=TRUE, sep=",")
d_2013 <- read.csv(paste0(sourceDir,"34574-0001-Data.csv"), header=TRUE, sep=",")
d_2014 <- read.csv(paste0(sourceDir,"36149-0001-Data.csv"), header=TRUE, sep=",")
d_2015 <- read.csv(paste0(sourceDir,"36407-0001-Data.csv"), header=TRUE, sep=",")
d_2016 <- read.csv(paste0(sourceDir,"36799-0001-Data.csv"), header=TRUE, sep=",")
d_2017 <- read.csv(paste0(sourceDir,"37183-0001-Data.csv"), header=TRUE, sep=",")

suppressWarnings(system(paste0('rm -f ',sourceDir,'/*-0001-Data.csv'), intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE, wait = TRUE, input = NULL, show.output.on.console = TRUE, minimized = FALSE, invisible = TRUE, timeout = 0))

d_combined <- rbind.fill(d_2012,d_2013,d_2014,d_2015,d_2016,d_2017)

for (interests in c("V7221","V7215","V7551","V7552","V7553","V7562","V7563"))
{
  #median information
  print(paste("interest:",interests))
  for(year in c("d_2012","d_2013","d_2014","d_2015","d_2016","d_2017","d_combined"))
  {
    #https://stackoverflow.com/questions/28802652/access-variable-dataframe-in-r-loop
    df <- (get(year)[,interests])
    print(paste("year:",year))
    #https://stackoverflow.com/questions/45986155/r-error-in-usemethodgroups-no-applicable-method-for-groups-applied-to
    print(paste("count:",sum (plyr::count(df[df>0]))))
    
    centerpoint = (length(df[df>0]))/2
    
    #print(centerpoint)
    width = round(1.96*sqrt((length(df[df>0])))/2)
    
    lower = (length(df[df>0]))/2 - width
    upper = (length(df[df>0]))/2 + width
    print(paste("lower:", sort(((df[df>0])))[lower]))
    print(paste("median:",median(df[df>0])))
    print(paste("upper:",sort(((df[df>0])))[upper]))
    
    print(round(table ( df[(df>0)] ) / sum (plyr::count(df[df>0])) ,4))
    
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

#https://stackoverflow.com/questions/9368900/how-to-check-if-object-variable-is-defined-in-r
if(!exists("medianDirection"))
{
  medianDirection <- medianDirectionSet[1]
}

#defaults, this works because they all convert the same.  I merely use y as a placeholder inside cleandatacode.R
if(!exists("flister"))
{
  flister <- c()
  flister <- 1
}


if(!exists("readList"))
{
  readList <- c()
  readList <- read.csv(paste0(sourceDir,"gpalist.txt"), header=FALSE, sep=,)
}

colnames(data)
col.num <- which(colnames(data) %in% as.character(readList[,1]))
#need to include GPA and psyD and gangfight in all 3!
#done, created 8th category and excluded it specifically from analysis, meaning I don't have to rerun my #'s :)

#length(colnames(NewDF))
#works with data DF

#transformations
#https://stackoverflow.com/questions/8214303/conditional-replacement-of-values-in-a-data-frame
#index <- df$b == 0
#df$est[index] <- (df$a[index] - 5)/2.533 
#conversion profile
#anything 2+ = positive
convert1Index <- readList[,2] == 1
#median (no profile applied)
convert2Index <- readList[,2] == 2
#4 = mostly
convert3Index <- readList[,2] == 3
#list[,1][convert1Index]

#male to female
#View(list[,1][convert2Index])

#here I should drop from NewDF non important terms (i.e. GPA, gangfight and PsyD from equations where not being tested.)

NewDF <- data[,(c(col.num))]

print(paste("medianDirection:",medianDirection))

NewDF <- replace.value( NewDF, colnames(NewDF), from=as.integer(-9), to=as.double(0), verbose = FALSE)
NewDF <- replace.value( NewDF, colnames(NewDF), from=as.integer(-8), to=as.double(0), verbose = FALSE)
NewDF <- replace.value( NewDF, as.character(readList[,1][convert1Index]), from=as.integer(1), to=as.double(-1), verbose = FALSE)
NewDF <- replace.value( NewDF, as.character(readList[,1][convert1Index]), from=as.integer(2), to=as.double(1), verbose = FALSE)
NewDF <- replace.value( NewDF, as.character(readList[,1][convert1Index]), from=as.integer(3), to=as.double(1), verbose = FALSE)
NewDF <- replace.value( NewDF, as.character(readList[,1][convert1Index]), from=as.integer(4), to=as.double(1), verbose = FALSE)
NewDF <- replace.value( NewDF, as.character(readList[,1][convert1Index]), from=as.integer(5), to=as.double(1), verbose = FALSE)
NewDF <- replace.value( NewDF, as.character(readList[,1][convert1Index]), from=as.integer(6), to=as.double(1), verbose = FALSE)
NewDF <- replace.value( NewDF, as.character(readList[,1][convert1Index]), from=as.integer(7), to=as.double(1), verbose = FALSE)

#gender
NewDF <- replace.value( NewDF, "V7202", from=as.integer(1), to=as.double(-1), verbose = FALSE)
#father household status
NewDF <- replace.value( NewDF, "V7206", from=as.integer(0), to=as.double(-1), verbose = FALSE)
NewDF <- replace.value( NewDF, "V7202", from=as.integer(2), to=as.double(1), verbose = FALSE)

#https://stackoverflow.com/questions/24237801/calculate-mean-median-by-excluding-any-given-number
#https://stackoverflow.com/questions/5824173/replace-a-value-in-a-data-frame-based-on-a-conditional-if-statement?rq=1

#https://www.ucl.ac.uk/child-health/short-courses-events/about-statistical-courses/research-methods-and-statistics/chapter-8-content-8
#95% confidence
#7: B+
#95% conf confirmed
#if flister==1(gpa)
#NewDF is referenced by saveCSV's, so by disabling this, I prevent referencing it
#if(flister==1)
#NewDF[,"V7221"]
V7221_Index <- c()
if (medianDirection=="greaterEqual") V7221_Index <- NewDF[,"V7221"] >= median(NewDF[,"V7221"][NewDF[,"V7221"]>0])
if (medianDirection=="greater") V7221_Index <- NewDF[,"V7221"] > median(NewDF[,"V7221"][NewDF[,"V7221"]>0])
centerpoint = (length(NewDF[,"V7221"][NewDF[,"V7221"]>0]))/2
width = round(1.96*sqrt((length(NewDF[,"V7221"][NewDF[,"V7221"]>0])))/2)
lower = (length(NewDF[,"V7221"][NewDF[,"V7221"]>0]))/2 - width
upper = (length(NewDF[,"V7221"][NewDF[,"V7221"]>0]))/2 + width
sort(((NewDF[,"V7221"][NewDF[,"V7221"]>0])))[lower]
sort(((NewDF[,"V7221"][NewDF[,"V7221"]>0])))[upper]

NewDF[V7221_Index,"V7221"] <- 21
V7221_Index <- c()
V7221_IndexNotAbove <- NewDF[,"V7221"] != 21
V7221_IndexNotAboveNotZero <- c()
V7221_IndexNotAboveNotZero <- NewDF[V7221_IndexNotAbove,"V7221"] != 0
NewDF[V7221_IndexNotAbove,"V7221"][V7221_IndexNotAboveNotZero] <- -1
tempIndex <- c()
tempIndex <- NewDF["V7221"]==21
NewDF[tempIndex,"V7221"] <- 1
summary(NewDF[,"V7221"])

#College graduate
#5: for college grad father, 95% conf confirmed
V7215_Index <- c()
if (medianDirection=="greaterEqual") V7215_Index <- NewDF[,"V7215"] >= median(NewDF[,"V7215"][NewDF[,"V7215"]>0])
if (medianDirection=="greater") V7215_Index <- NewDF[,"V7215"] > median(NewDF[,"V7215"][NewDF[,"V7215"]>0])
centerpoint = (length(NewDF[,"V7215"][NewDF[,"V7215"]>0]))/2
width = round(1.96*sqrt((length(NewDF[,"V7215"][NewDF[,"V7215"]>0])))/2)
lower = (length(NewDF[,"V7215"][NewDF[,"V7215"]>0]))/2 - width
upper = (length(NewDF[,"V7215"][NewDF[,"V7215"]>0]))/2 + width
sort(((NewDF[,"V7215"][NewDF[,"V7215"]>0])))[lower]
sort(((NewDF[,"V7215"][NewDF[,"V7215"]>0])))[upper]

NewDF[V7215_Index,"V7215"] <- 21
V7215_Index <- c()
V7215_IndexNotAbove <- NewDF[,"V7215"] != 21
V7215_IndexNotAboveNotZero <- c()
V7215_IndexNotAboveNotZero <- NewDF[V7215_IndexNotAbove,"V7215"] != 0
NewDF[V7215_IndexNotAbove,"V7215"][V7215_IndexNotAboveNotZero] <- -1
tempIndex <- c()
tempIndex <- NewDF["V7215"]==21
NewDF[tempIndex,"V7215"] <- 1

#4: 3-5 Hours Internet #95% conf confirmed
#4 #hours for computer use for internet leisure 

V7551_Index <- c()
if (medianDirection=="greaterEqual") V7551_Index <- NewDF[,"V7551"] >= median(NewDF[,"V7551"][NewDF[,"V7551"]>0])
if (medianDirection=="greater") V7551_Index <- NewDF[,"V7551"] > median(NewDF[,"V7551"][NewDF[,"V7551"]>0])
centerpoint = (length(NewDF[,"V7551"][NewDF[,"V7551"]>0]))/2
width = round(1.96*sqrt((length(NewDF[,"V7551"][NewDF[,"V7551"]>0])))/2)
lower = (length(NewDF[,"V7551"][NewDF[,"V7551"]>0]))/2 - width
upper = (length(NewDF[,"V7551"][NewDF[,"V7551"]>0]))/2 + width
sort(((NewDF[,"V7551"][NewDF[,"V7551"]>0])))[lower]
sort(((NewDF[,"V7551"][NewDF[,"V7551"]>0])))[upper]

NewDF[V7551_Index,"V7551"] <- 21
V7551_Index <- c()
V7551_IndexNotAbove <- NewDF[,"V7551"] != 21
V7551_IndexNotAboveNotZero <- c()
V7551_IndexNotAboveNotZero <- NewDF[V7551_IndexNotAbove,"V7551"] != 0
NewDF[V7551_IndexNotAbove,"V7551"][V7551_IndexNotAboveNotZero] <- -1
tempIndex <- c()
tempIndex <- NewDF["V7551"]==21
NewDF[tempIndex,"V7551"] <- 1

#5: 6-9 Hours Facebook # 95% conf confirmed
V7552_Index <- c()
if (medianDirection=="greaterEqual") V7552_Index <- NewDF[,"V7552"] >= median(NewDF[,"V7552"][NewDF[,"V7552"]>0])
if (medianDirection=="greater") V7552_Index <- NewDF[,"V7552"] > median(NewDF[,"V7552"][NewDF[,"V7552"]>0])
centerpoint = (length(NewDF[,"V7552"][NewDF[,"V7552"]>0]))/2
width = round(1.96*sqrt((length(NewDF[,"V7552"][NewDF[,"V7552"]>0])))/2)
lower = (length(NewDF[,"V7552"][NewDF[,"V7552"]>0]))/2 - width
upper = (length(NewDF[,"V7552"][NewDF[,"V7552"]>0]))/2 + width
sort(((NewDF[,"V7552"][NewDF[,"V7552"]>0])))[lower]
sort(((NewDF[,"V7552"][NewDF[,"V7552"]>0])))[upper]

NewDF[V7552_Index,"V7552"] <- 21
V7552_Index <- c()
V7552_IndexNotAbove <- NewDF[,"V7552"] != 21
V7552_IndexNotAboveNotZero <- c()
V7552_IndexNotAboveNotZero <- NewDF[V7552_IndexNotAbove,"V7552"] != 0
NewDF[V7552_IndexNotAbove,"V7552"][V7552_IndexNotAboveNotZero] <- -1
tempIndex <- c()
tempIndex <- NewDF["V7552"]==21
NewDF[tempIndex,"V7552"] <- 1

#4 3-5 Hours Gaming # 95% conf confirmed
V7553_Index <- c()
if (medianDirection=="greaterEqual") V7553_Index <- NewDF[,"V7553"] >= median(NewDF[,"V7553"][NewDF[,"V7553"]>0])
if (medianDirection=="greater") V7553_Index <- NewDF[,"V7553"] > median(NewDF[,"V7553"][NewDF[,"V7553"]>0])
centerpoint = (length(NewDF[,"V7553"][NewDF[,"V7553"]>0]))/2
width = round(1.96*sqrt((length(NewDF[,"V7553"][NewDF[,"V7553"]>0])))/2)
lower = (length(NewDF[,"V7553"][NewDF[,"V7553"]>0]))/2 - width
upper = (length(NewDF[,"V7553"][NewDF[,"V7553"]>0]))/2 + width
sort(((NewDF[,"V7553"][NewDF[,"V7553"]>0])))[lower]
sort(((NewDF[,"V7553"][NewDF[,"V7553"]>0])))[upper]

NewDF[V7553_Index,"V7553"] <- 21
V7553_Index <- c()
V7553_IndexNotAbove <- NewDF[,"V7553"] != 21
V7553_IndexNotAboveNotZero <- c()
V7553_IndexNotAboveNotZero <- NewDF[V7553_IndexNotAbove,"V7553"] != 0
NewDF[V7553_IndexNotAbove,"V7553"][V7553_IndexNotAboveNotZero] <- -1
tempIndex <- c()
tempIndex <- NewDF["V7553"]==21
NewDF[tempIndex,"V7553"] <- 1

#4 3-5 Hours Texting # 95% conf confirmed
V7562_Index <- c()
if (medianDirection=="greaterEqual") V7562_Index <- NewDF[,"V7562"] >= median(NewDF[,"V7562"][NewDF[,"V7562"]>0])
if (medianDirection=="greater") V7562_Index <- NewDF[,"V7562"] > median(NewDF[,"V7562"][NewDF[,"V7562"]>0])
centerpoint = (length(NewDF[,"V7562"][NewDF[,"V7562"]>0]))/2
width = round(1.96*sqrt((length(NewDF[,"V7562"][NewDF[,"V7562"]>0])))/2)
lower = (length(NewDF[,"V7562"][NewDF[,"V7562"]>0]))/2 - width
upper = (length(NewDF[,"V7562"][NewDF[,"V7562"]>0]))/2 + width
sort(((NewDF[,"V7562"][NewDF[,"V7562"]>0])))[lower]
sort(((NewDF[,"V7562"][NewDF[,"V7562"]>0])))[upper]

NewDF[V7562_Index,"V7562"] <- 21
V7562_Index <- c()
V7562_IndexNotAbove <- NewDF[,"V7562"] != 21
V7562_IndexNotAboveNotZero <- c()
V7562_IndexNotAboveNotZero <- NewDF[V7562_IndexNotAbove,"V7562"] != 0
NewDF[V7562_IndexNotAbove,"V7562"][V7562_IndexNotAboveNotZero] <- -1
tempIndex <- c()
tempIndex <- NewDF["V7562"]==21
NewDF[tempIndex,"V7562"] <- 1

#2: <1 Hour talking on cell phone # 95% conf confirmed
V7563_Index <- c()
if (medianDirection=="greaterEqual") V7563_Index <- NewDF[,"V7563"] >= median(NewDF[,"V7563"][NewDF[,"V7563"]>0])
#cbind(NewDF["V7563"],V7563_Index)
if (medianDirection=="greater") V7563_Index <- NewDF[,"V7563"] > median(NewDF[,"V7563"][NewDF[,"V7563"]>0])
centerpoint = (length(NewDF[,"V7563"][NewDF[,"V7563"]>0]))/2
width = round(1.96*sqrt((length(NewDF[,"V7563"][NewDF[,"V7563"]>0])))/2)
lower = (length(NewDF[,"V7563"][NewDF[,"V7563"]>0]))/2 - width
upper = (length(NewDF[,"V7563"][NewDF[,"V7563"]>0]))/2 + width
sort(((NewDF[,"V7563"][NewDF[,"V7563"]>0])))[lower]
sort(((NewDF[,"V7563"][NewDF[,"V7563"]>0])))[upper]

NewDF[(NewDF["V7563"] == 1),"V7563"]
NewDF[V7563_Index,"V7563"] <- 21
V7563_Index <- c()
V7563_IndexNotAbove <- NewDF[,"V7563"] != 21
V7563_IndexNotAboveNotZero <- c()
V7563_IndexNotAboveNotZero <- NewDF[V7563_IndexNotAbove,"V7563"] != 0
NewDF[V7563_IndexNotAbove,"V7563"][V7563_IndexNotAboveNotZero] <- -1
tempIndex <- c()
tempIndex <- NewDF["V7563"]==21
NewDF[tempIndex,"V7563"] <- 1

NewDF <- replace.value( NewDF, as.character(readList[,1][convert3Index]), from=as.integer(1), to=as.double(-1), verbose = FALSE)
NewDF <- replace.value( NewDF, as.character(readList[,1][convert3Index]), from=as.integer(2), to=as.double(-1), verbose = FALSE)
NewDF <- replace.value( NewDF, as.character(readList[,1][convert3Index]), from=as.integer(3), to=as.double(-1), verbose = FALSE)
NewDF <- replace.value( NewDF, as.character(readList[,1][convert3Index]), from=as.integer(4), to=as.double(1), verbose = FALSE)
NewDF <- replace.value( NewDF, as.character(readList[,1][convert3Index]), from=as.integer(5), to=as.double(1), verbose = FALSE)
NewDF <- replace.value( NewDF, as.character(readList[,1][convert3Index]), from=as.integer(6), to=as.double(1), verbose = FALSE)

NewDF[NewDF == 0] <- -8

NewDF <- replace.value( NewDF, colnames(NewDF), from=as.integer(-9), to=as.double(0), verbose = FALSE)
NewDF <- replace.value( NewDF, colnames(NewDF), from=as.integer(-8), to=as.double(0), verbose = FALSE)

#NewDF <- oldDF
#shows # of na's
{
  #NewDF[NewDF == 0] <- NA
  
  #cleandata<-NewDF[,colSums(is.na(NewDF)) >= round(nrow(NewDF)*.50,0)] # dat[A, B] takes the A rows and B columns; A and B are indices;
  
  #remove problematic columns (>75% na's)
  filterList <- c()
  rows = nrow(NewDF)
  #lister=4
  #skip 1st
  minone <- c()
  minzero <- c()
  #colnames(NewDF)
  percentTableList <- c()
  for (lister in 1:ncol(NewDF))
  {
    name <-c()
    name <- colnames(NewDF[lister])
    if (flister==1) print(name)
    percentTable <- table(NewDF[lister])/rows
    if (flister==1) print(percentTable)
    rowsL <- c()
    rowsL <- rownames(percentTable)
    leftrow <- c()
    middlerow <- c()
    if(length(rowsL)==3) middlerow <- rowsL[2]
    if(length(rowsL)==2) middlerow <- NA
    rightrow <- c()
    leftrow <- rowsL[1]
    rightrow <- rowsL[length(rowsL)]
    
    combinedRows <- c(leftrow,middlerow,rightrow)
    #print(combinedRows)
    
    minone <- min(minone,percentTable[length(percentTable)])
    minzero <- min(minzero,percentTable[1])
  }
  print(c("min one:",round(minone,3)))
  print(c("min zero:",round(minzero,3)))
  
  #write.csv(percentTableList,paste0(sourceDir,flister,"percentTableList.csv"))
  
  #print(c(filterList))
  
  oldDF <- c()
  
  oldDF <- NewDF
  
  NewDF <- c()
  #store <- 
  #table(oldDF["V7501"], useNA = "ifany")
  
  #https://stackoverflow.com/questions/5234117/how-to-drop-columns-by-name-in-a-data-frame
  #https://www.listendata.com/2015/06/r-keep-drop-columns-from-data-frame.html
  
  #NewDF = subset(oldDF, select = -c(noquote(c(as.character(filterList)))) )
  
  lGeographyIndex <- readList[,4] == 1
  
  lGenderGPAViolenceFatherIndex <- readList[,4] == 2
  #lGenderIndex <- list[,4] == 2
  #lGPAIndex <- list[,4] == 3
  #lViolenceIndex <- list[,4] == 4
  #lFather1Index <- list[,4] == 5
  #lFather2Index <- list[,4] == 6
  
  lHabitsIndex1 <- readList[,4] == 3
  lHealthIndex <- readList[,4] == 4
  lPsycheIndex1 <- readList[,4] == 5
  lPsycheIndex2 <- readList[,4] == 6
  lHabitsIndex2 <- readList[,4] == 7
  lExcludedIndex <- readList[,4] == 8
  
  colListNames <- rbind(readList[lGenderGPAViolenceFatherIndex,],readList[lHabitsIndex1,],readList[lHealthIndex,],readList[lPsycheIndex1,],readList[lPsycheIndex2,],readList[lHabitsIndex2,])
  
  #end na's 
}

#https://stackoverflow.com/questions/18562680/replacing-nas-with-0s-in-r-dataframe

#http://www.datasciencemadesimple.com/drop-variables-columns-r-using-dplyr/
#deselect filtered, 7501 and 7507 were removed as being over 75% na's after this was ran.
NewDF <- dplyr::select(oldDF,-c(as.character(filterList)))
