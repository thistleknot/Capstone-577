set <- c()
set <- eval(parse(text=paste("combined.holdoutSet.",pairedname, sep = "")))

#monte carlo resample from pre separated holdout (this means new holdout each subsample)
holdout <- c()
holdout <- sample(nrow(set), round(holdoutSize*nrow(set)))

#NewDF.holdout <- c()
#NewDF.holdout <- set[holdout, ]
assign(paste("combined.holdout.",pairedname, sep = ""), set[holdout, ]) 

