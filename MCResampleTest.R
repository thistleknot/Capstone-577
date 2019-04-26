set <- c()
setIndex <- c()

#set <- c()
setIndex <- eval(parse(text=paste("combined.holdoutSet.",pairedname, sep = "")))

#monte carlo resample from pre separated holdout (this means new holdout each subsample)
#holdout <- c()
holdout <- sample(setIndex, round(holdoutSize*length(setIndex)))

#NewDF.holdout <- c()
#NewDF.holdout <- set[holdout, ]
assign(paste("combined.holdout.",pairedname, sep = ""), holdout) 
#length(holdout)