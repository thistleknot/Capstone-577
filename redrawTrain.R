ones <- c()
zeros <- c()
set <- c()
set.ones <- c()
set.zeros <- c()
setIndex <- c()
setIndex.ones <- c()
setIndex.zeros <- c()
data.train <- c()

#set <- eval(parse(text=paste("combined.",pairedname, sep = "")))
set.ones <- eval(parse(text=paste("combined.ones.",pairedname, sep = "")))
set.zeros <- eval(parse(text=paste("combined.zeros.",pairedname, sep = "")))

#setIndex <- eval(parse(text=paste("combined.holdout.",pairedname, sep = "")))
setIndex.ones <- eval(parse(text=paste("combined.preTrain.ones.",pairedname, sep = "")))
setIndex.zeros <- eval(parse(text=paste("combined.preTrain.zeros.",pairedname, sep = "")))

ones <- set.ones[setIndex.ones,]
zeros <- set.zeros[setIndex.zeros,]

data.train <- c()
data.train <- rbind(ones,zeros)
