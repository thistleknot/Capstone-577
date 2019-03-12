## read in data ##
data<-read.csv("C:\\Users\\CampusUser\\Desktop\\MyData.csv")
dim(dat)
# check missing with for loop
# The below code gives the number of missing values for each variables
for (ii in 1:ncol(data)) {
  print( colnames(data)[ii] )
  print( table(is.na(data[,ii])) )
}
## select the columns with no missings
id.col = which(colnames(data) %in% c('X', 'ï..CASEID', 'V1','V3','V4','V5','v7101', 'v7104', 'v7105', 'V7112', 'V7115', 'V7118', 'V7127', 'V7097', 'V7133', 'V7139', 
                                     'V7142', 'V8451','V7426', 'V7121', 'V7124', 'V7164', 'V7145', 'V7109', 'V7152','v7155','V7158','V7161','V7601','V8480','V7106',
                                     'V7113','V7116')) 
# 'which' gives the index when the condition is TRUE
# colnames(data) are column names; 

head(id.col) # just print out the head of id.col to see what it is;
cleandata = data[,id.col] # dat[A, B] takes the A rows and B columns; A and B are indices; 
# if A or B is not specified, all rows or columns will be retained
table(is.na(cleandata))# table(is.na(cleandata)) gives the number of missing values of data
#Since there are no missing values we export the data
write.csv(cleandata, "C:\\Users\\CampusUser\\Desktop\\MyData.csv")