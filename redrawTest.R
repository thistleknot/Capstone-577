ones <- c()
zeros <- c()
set <- c()
set.ones <- c()
set.zeros <- c()
setIndex <- c()
setIndex.ones <- c()
setIndex.zeros <- c()
data.test <- c()

#set <- eval(parse(text=paste("combined.",pairedname, sep = "")))
set.ones <- eval(parse(text=paste("combined.ones.",pairedname, sep = "")))
set.zeros <- eval(parse(text=paste("combined.zeros.",pairedname, sep = "")))

#setIndex <- eval(parse(text=paste("combined.holdout.",pairedname, sep = "")))
setIndex.ones <- eval(parse(text=paste("combined.holdout.ones.",pairedname, sep = "")))
setIndex.zeros <- eval(parse(text=paste("combined.holdout.zeros.",pairedname, sep = "")))

ones <- set.ones[setIndex.ones,]
zeros <- set.zeros[setIndex.zeros,]

#data.test <- set[setIndex,]
data.test <- rbind(ones,zeros)
#summary(data.test)
#data.test[data.test == 0] <- NA
#temp <- data.test[] %>% filter_all(all_vars(!is.na(.)))
#data.test <- c()
#data.test <- temp
#data.test[data.test == -1] <- 0

