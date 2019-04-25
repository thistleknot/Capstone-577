#reseed
#MC Reseed
holdoutSet <- c()
#will always have at least 1 var.
holdoutSet <- sample(nrow(NewDF[,,drop=FALSE]), round(holdoutSetSize*nrow(NewDF)))
NewDF.holdoutSet <- c()
NewDF.holdoutSet <- NewDF[holdoutSet,,drop=FALSE]
preNonHoldoutSet <- c()
preNonHoldoutSet <- sample(nrow(NewDF[-holdoutSet,,drop=FALSE]), round(preNonHoldOutSize*nrow(NewDF[-holdoutSet,,drop=FALSE])))
NewDF.preNonHoldoutSet <- c()
NewDF.preNonHoldoutSet <- NewDF[-holdoutSet,,drop=FALSE][preNonHoldoutSet,,drop=FALSE]

holdout <- c()
holdout <- sample(nrow(NewDF.holdoutSet[,,drop=FALSE]), round(holdoutSize*nrow(NewDF.holdoutSet[,,drop=FALSE])))
NewDF.holdout <- c()
NewDF.holdout <- NewDF.holdoutSet[holdout,,drop=FALSE]

source(paste0(sourceDir,"reseedSample.R"))
