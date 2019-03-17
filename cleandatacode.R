## read in data ##
#select
library(dplyr)

sourceDir="C:/Users/user/Documents/School/CSUF/ISDS577/projects/Capstone-577/"

data<-read.csv(paste0(sourceDir,"MyData.csv"),sep=",",quote="\"")
list<-read.csv(paste0(sourceDir,"filterList.txt"), header=FALSE, sep=,)

#dim(data)
# check missing with for loop
# The below code gives the number of missing values for each variables

#expensive, descriptive only
#for (ii in 1:ncol(data)) {
#  print( colnames(data)[ii] )
#  print( table(is.na(data[,ii])) )
#}
## select the columns with no missings

cleandata<-data[,colSums(is.na(data)) == 0] # dat[A, B] takes the A rows and B columns; A and B are indices; 
# if A or B is not specified, all rows or columns will be retained

#expensive, descriptive function only
#table(is.na(cleandata))# table(is.na(cleandata)) gives the number of missing values of data
#Since there are no missing values we export the data
#write.csv(cleandata, "C:\\Users\\CampusUser\\Desktop\\MyData.csv")

colnames(data)

#https://stackoverflow.com/questions/27556353/subset-columns-based-on-list-of-column-names-and-bring-the-column-before-it

col.num <- which(colnames(data) %in% as.character(list[,1]))

NewDF <- data[,(c(col.num))]

