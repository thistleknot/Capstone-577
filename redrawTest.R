set <- eval(parse(text=paste("combined.",pairedname, sep = "")))
setIndex <- eval(parse(text=paste("combined.holdout.",pairedname, sep = "")))
#set <- NewDF[,newList]

#set[set == 0] <- NA
#temp <- set[] %>% filter_all(all_vars(!is.na(.)))
#set <- c()
#set <- temp
#set[set == -1] <- 0

totalRows <- nrow(set)
setIndex2 <- unique(round(setIndex/precisionSize*totalRows,0))

#length(setIndex)

data.test <- c()
data.test <- set[setIndex2,]
#data.test[data.test == 0] <- NA
#temp <- data.test[] %>% filter_all(all_vars(!is.na(.)))
#data.test <- c()
#data.test <- temp
#data.test[data.test == -1] <- 0
