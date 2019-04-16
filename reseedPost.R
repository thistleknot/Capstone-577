#reseed
#MC Reseed
holdoutSet <- c()
holdoutSet <- sample(nrow(PostDF), round(holdoutSetSize*nrow(PostDF)))
PostDF.holdoutSet <- c()
PostDF.holdoutSet <- PostDF[holdoutSet,,drop=FALSE]
preNonHoldoutSet <- c()
preNonHoldoutSet <- sample(nrow(PostDF[-holdoutSet,,drop=FALSE]), round(preNonHoldOutSize*nrow(PostDF[-holdoutSet,,drop=FALSE])))
PostDF.preNonHoldoutSet <- c()
PostDF.preNonHoldoutSet <- PostDF[-holdoutSet,,drop=FALSE]

holdout <- c()
holdout <- sample(nrow(PostDF.holdoutSet[,,drop=FALSE]), round(holdoutSize*nrow(PostDF.holdoutSet)))
PostDF.holdout <- c()
PostDF.holdout <- PostDF.holdoutSet[holdout,,drop=FALSE]

preTrain <- c()
preTrain <- sample(nrow(PostDF.preNonHoldoutSet[,,drop=FALSE]), round(preTrainSize*nrow(PostDF.preNonHoldoutSet)))
PostDF.preTrain <- c()
PostDF.preTrain <- PostDF.preNonHoldoutSet[preTrain,,drop=FALSE]

#don't call resampleMC.R from within this, you don't have the nameList yet, this is only index