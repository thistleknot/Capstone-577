
#monte carlo resample from pre separated holdout (this means new holdout each subsample)
holdout <- c()
holdout <- sample(nrow(NewDF.holdoutSet), round(holdoutSize*nrow(NewDF.holdoutSet)))

NewDF.holdout <- c()
NewDF.holdout <- NewDF.holdoutSet[holdout, ]