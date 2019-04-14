#reseed
#MC Reseed
holdoutSet <- c()
holdoutSet <- sample(nrow(PostDF), round(holdoutSetSize*nrow(PostDF)))
PostDF.holdoutSet <- c()
PostDF.holdoutSet <- PostDF[holdoutSet,]
preNonHoldoutSet <- c()
preNonHoldoutSet <- sample(nrow(PostDF[-holdoutSet,]), round(preNonHoldOutSize*nrow(PostDF[-holdoutSet,])))
PostDF.preNonHoldoutSet <- c()
PostDF.preNonHoldoutSet <- PostDF[-holdoutSet,][preNonHoldoutSet,]

holdout <- c()
holdout <- sample(nrow(PostDF.holdoutSet), round(holdoutSize*nrow(PostDF.holdoutSet)))
PostDF.holdout <- c()
PostDF.holdout <- PostDF.holdoutSet[holdout, ]

preTrain <- c()
preTrain <- sample(nrow(PostDF.preNonHoldoutSet), round(preTrainSize*nrow(PostDF.preNonHoldoutSet)))
PostDF.preTrain <- c()
PostDF.preTrain <- PostDF.preNonHoldoutSet[preTrain,]

#don't call resampleMC.R from within this, you don't have the nameList yet, this is only index