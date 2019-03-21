## read in data ##
#select
library(dplyr)
library(plyr)

require("RPostgreSQL")
library(RPostgreSQL)
require(ggplot2)
library(anchors)

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

#dim(data)
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

#drop na's
#https://stackoverflow.com/questions/4862178/remove-rows-with-all-or-some-nas-missing-values-in-data-frame
NewDF <- NewDF %>% filter_all(all_vars(!is.na(.)))

length(colnames(NewDF))

#transformations
#https://stackoverflow.com/questions/8214303/conditional-replacement-of-values-in-a-data-frame
#index <- df$b == 0
#df$est[index] <- (df$a[index] - 5)/2.533 

convert1Index <- list[,2] == 1
list[,1][convert1Index]

NewDF <- replace.value( NewDF, list[,1][convert1Index], from=as.integer(-9), to=as.double(-1), verbose = FALSE)
NewDF <- replace.value( NewDF, list[,1][convert1Index], from=as.integer(-8), to=as.double(-1), verbose = FALSE)
NewDF <- replace.value( NewDF, list[,1][convert1Index], from=as.integer(1), to=as.double(0), verbose = FALSE)
NewDF <- replace.value( NewDF, list[,1][convert1Index], from=as.integer(2), to=as.double(1), verbose = FALSE)
NewDF <- replace.value( NewDF, list[,1][convert1Index], from=as.integer(3), to=as.double(1), verbose = FALSE)
NewDF <- replace.value( NewDF, list[,1][convert1Index], from=as.integer(4), to=as.double(1), verbose = FALSE)
NewDF <- replace.value( NewDF, list[,1][convert1Index], from=as.integer(5), to=as.double(1), verbose = FALSE)
NewDF <- replace.value( NewDF, list[,1][convert1Index], from=as.integer(6), to=as.double(1), verbose = FALSE)
NewDF <- replace.value( NewDF, list[,1][convert1Index], from=as.integer(7), to=as.double(1), verbose = FALSE)

#converted 1 index
index <- NewDF[,"V7112"] == -9
#table(is.na(data[,"V7101"]))
NewDF[,"V7112"]
NewDF[,"V7112D"] <- NewDF[,"V7112"]
NewDF[,"V7112D"][index] <- 0
NewDF[,"V7112D"][!index] <- NewDF[,"V7112"][!index]

#NewDF <- replace.value( NewDF, colnames(NewDF), from=as.integer(-9), to=as.double(0), verbose = FALSE)
#NewDF <- replace.value( NewDF, colnames(NewDF), from=as.integer(-8), to=as.double(0), verbose = FALSE)
#NewDF <- replace.value( NewDF, colnames(NewDF), from=as.integer(1), to=as.double(.5), verbose = FALSE)
#NewDF <- replace.value( NewDF, colnames(NewDF), from=as.integer(1), to=as.double(.5), verbose = FALSE)

#correlation matrix
res <- cor(NewDF)
colnames(NewDF)
#View(res)
#colnames(NewDF)

#ecdf(NewDF)
#plot(ecdf(NewDF[,1]))
#View(ecdf(NewDF[,2]))

colnames(NewDF)

colnames(res)

#https://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-inner-outer-left-right
#merge(colList, list, by = "V1")[,2]
colList <- data.frame(colnames(NewDF))
colnames(colList) <- "V1"

#corrplot(res, method = "square")

colnames(NewDF)


boxplot(NewDF)
summary(NewDF)



colListNames <- paste(merge(list, colList, by = "V1")[,1],merge(colList, list, by = "V1")[,3])
  
#https://stackoverflow.com/questions/17878048/merge-two-data-frames-while-keeping-the-original-row-order
join(colList,list)
colListNames <- paste(join(colList,list)[,1],join(colList,list)[,2])

colnames(res)<-colListNames
corrplot(res, method = "square")
write.csv(res,paste0(sourceDir,"correlationMatrix.csv"))
