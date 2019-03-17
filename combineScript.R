# Read CSV into R
library(plyr)
d_2012 <- read.csv(file="C:/Users/user/Documents/School/CSUF/ISDS577/projects/34574-0001-Data.csv", header=TRUE, sep=",")
d_2013 <- read.csv(file="C:/Users/user/Documents/School/CSUF/ISDS577/projects/35166-0001-Data.csv", header=TRUE, sep=",")
d_2014 <- read.csv(file="C:/Users/user/Documents/School/CSUF/ISDS577/projects/36149-0001-Data.csv", header=TRUE, sep=",")
d_2015 <- read.csv(file="C:/Users/user/Documents/School/CSUF/ISDS577/projects/36407-0001-Data.csv", header=TRUE, sep=",")
d_2016 <- read.csv(file="C:/Users/user/Documents/School/CSUF/ISDS577/projects/36799-0001-Data.csv", header=TRUE, sep=",")
d_2017 <- read.csv(file="C:/Users/user/Documents/School/CSUF/ISDS577/projects/37183-0001-Data.csv", header=TRUE, sep=",")

d_combined <- rbind.fill(d_2012,d_2013,d_2014,d_2015,d_2016,d_2017)


na_count <-function (x) sapply(x, function(y) sum(is.na(y)))

View(na_count(d_combined))

#summary(d_combined)

write.csv(d_combined, file = "MyData.csv")