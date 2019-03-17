## read in data ##

sourceDir="C:/Users/user/Documents/School/CSUF/ISDS577/projects/Capstone-577/"

data<-read.csv(paste0(sourceDir,"MyData.csv"))
list<-data.frame(read.csv(paste0(sourceDir,"filterList.txt"), header=FALSE))

dim(data)
# check missing with for loop
# The below code gives the number of missing values for each variables
for (ii in 1:ncol(data)) {
  print( colnames(data)[ii] )
  print( table(is.na(data[,ii])) )
}
## select the columns with no missings

cleandata<-data[,colSums(is.na(data)) == 0] # dat[A, B] takes the A rows and B columns; A and B are indices; 
# if A or B is not specified, all rows or columns will be retained
table(is.na(cleandata))# table(is.na(cleandata)) gives the number of missing values of data
#Since there are no missing values we export the data
#write.csv(cleandata, "C:\\Users\\CampusUser\\Desktop\\MyData.csv")

colnames(data)


