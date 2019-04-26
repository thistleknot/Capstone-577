set <- eval(parse(text=paste("combined.",pairedname, sep = "")))
nrow(set)
length(setIndex)
setIndex <- eval(parse(text=paste("combined.holdout.",pairedname, sep = "")))

data.test <- c()
data.test <- set[setIndex,]
#data.test[data.test == 0] <- NA
#temp <- data.test[] %>% filter_all(all_vars(!is.na(.)))
#data.test <- c()
#data.test <- temp
#data.test[data.test == -1] <- 0
