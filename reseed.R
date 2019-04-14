#reseed
#MC Reseed
holdoutSet <- c()
holdoutSet <- sample(nrow(NewDF), round(holdoutSetSize*nrow(NewDF)))
NewDF.holdoutSet <- c()
NewDF.holdoutSet <- NewDF[holdoutSet,]
preNonHoldoutSet <- c()
preNonHoldoutSet <- sample(nrow(NewDF[-holdoutSet,]), round(preNonHoldOutSize*nrow(NewDF[-holdoutSet,])))
NewDF.preNonHoldoutSet <- c()
NewDF.preNonHoldoutSet <- NewDF[-holdoutSet,][preNonHoldoutSet,]
holdout <- c()
holdout <- sample(nrow(NewDF.holdoutSet), round(holdoutSize*nrow(NewDF.holdoutSet)))
NewDF.holdout <- c()
NewDF.holdout <- NewDF.holdoutSet[holdout, ]

preTrain <- c()
preTrain <- sample(nrow(NewDF.preNonHoldoutSet), round(preTrainSize*nrow(NewDF.preNonHoldoutSet)))
NewDF.preTrain <- c()
NewDF.preTrain <- NewDF.preNonHoldoutSet[preTrain,]

source(paste0(sourceDir,"/resampleMC.R"))
