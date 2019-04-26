set <- eval(parse(text=paste("combined.",pairedname, sep = "")))
setIndex <- eval(parse(text=paste("combined.preTrain.",pairedname, sep = "")))

data.train <- c()
data.train <- set[setIndex,]
#data.train[data.train == 0] <- NA
#temp <- data.train[] %>% filter_all(all_vars(!is.na(.)))
#data.train <- c()
#data.train <- temp
#does conversion is done at the reseedBoth level.
#data.train[data.train == -1] <- 0
