set <- c()
setIndex <- c()
set <- eval(parse(text=paste("combined.",pairedname, sep = "")))
setIndex <- eval(parse(text=paste("combined.preTrain.",pairedname, sep = "")))
#set <- NewDF[,newList]
#set[set == 0] <- NA
#temp <- set[] %>% filter_all(all_vars(!is.na(.)))
#set <- c()
#set <- temp
#set[set == -1] <- 0

totalRows <- nrow(set)
#setIndex2 <- unique(round(setIndex/precisionSize*totalRows,0))

data.train <- c()
data.train <- set[setIndex,]
#data.train[data.train == 0] <- NA
#temp <- data.train[] %>% filter_all(all_vars(!is.na(.)))
#data.train <- c()
#data.train <- temp
#does conversion is done at the reseedBoth level.
#data.train[data.train == -1] <- 0
