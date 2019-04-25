preTrain <- c()
preTrain <- sample(nrow(NewDF.preNonHoldoutSet[,,drop=FALSE]), round(preTrainSize*nrow(NewDF.preNonHoldoutSet[,,drop=FALSE])))
NewDF.preTrain <- c()
NewDF.preTrain <- NewDF.preNonHoldoutSet[preTrain,,drop=FALSE]

#don't call resampleMC.R from within this, you don't have the nameList yet, this is only index