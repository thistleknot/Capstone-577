## read in data ##
data<-read.csv("C:\\Users\\CampusUser\\Desktop\\MyData.csv")
dim(dat)
# check missing with for loop
# The below code gives the number of missing values for each variables
for (ii in 1:ncol(data)) {
  print( colnames(data)[ii] )
  print( table(is.na(data[,ii])) )
}
cleandata<-data[,colSums(is.na(data)) == 0]# if A or B is not specified, all rows or columns will be retained
table(is.na(cleandata))# table(is.na(cleandata)) gives the number of missing values of data
#Since there are no missing values we export the data
write.csv(cleandata, "C:\\Users\\CampusUser\\Desktop\\MyData.csv")
