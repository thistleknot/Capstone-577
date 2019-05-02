set <- c()
setIndex <- c()
setIndex.ones <- c()
setIndex.zeros <- c()
#set.ones <- c()
#set.zeros <- c()

#set <- c()
set <- eval(parse(text=paste("combined.",pairedname, sep = "")))
#set.ones <- eval(parse(text=paste("combined.ones.",pairedname, sep = "")))
#set.zeros <- eval(parse(text=paste("combined.zeros.",pairedname, sep = "")))

#setIndex <- eval(parse(text=paste("combined.holdoutSet.",pairedname, sep = "")))
setIndex.ones <- eval(parse(text=paste("combined.holdoutSet.ones.",pairedname, sep = "")))
setIndex.zeros <- eval(parse(text=paste("combined.holdoutSet.zeros.",pairedname, sep = "")))
#nrow(set.ones)
#max(setIndex.ones)
#monte carlo resample from pre separated holdout (this means new holdout each subsample)
#holdout <- c()
holdout.ones <- c()
holdout.zeros <- c()
#holdout <- sample(setIndex, round(holdoutSize*length(setIndex)))
holdout.onesA <- c()
holdout.onesB <- c()

#MC resample
holdout.ones <- sample(c(rownames(setIndex.ones)), round(holdoutSize*length(setIndex.ones)))
#nrow(set.ones)
#View(set.ones)
#set.ones[32124,]
#needs to be same size as one's
#see http://r-statistics.co/Logistic-Regression-With-R.html
## 0's for training. Pick as many 0's as 1's
#specific undersampling/oversampling
holdout.zeros <- sample(c(rownames(setIndex.zeros)), round(holdoutSize*length(setIndex.ones)))
#length(holdout.zeros)
#length(holdout.ones)

#NewDF.holdout <- c()
#NewDF.holdout <- set[holdout, ]
#not holdoutset, holdout are derived FROM holdoutset, confusing because
#I make references to dataframes as set's aside from the naming convention of holdout and holdoutSet
#assign(paste("combined.holdout.",pairedname, sep = ""), holdout) 
assign(paste("combined.holdout.ones.",pairedname, sep = ""), setIndex.ones[holdout.ones,]) 
assign(paste("combined.holdout.zeros.",pairedname, sep = ""), setIndex.zeros[holdout.zeros,]) 
#length(holdout)
