set <- c()
#setIndex <- eval(parse(text=paste("combined.holdoutSet.",pairedname, sep = "")))
setIndex <- eval(parse(text=paste("combined.preNonHoldoutSet.",pairedname, sep = "")))

#preTrain <- c()
preTrain <- sample(setIndex, round(preTrainSize*length(setIndex)))
#NewDF.preTrain <- c()
#NewDF.preTrain <- NewDF.preNonHoldoutSet[preTrain,,drop=FALSE]

assign(paste("combined.preTrain.",pairedname, sep = ""), preTrain) 

#don't call resampleMC.R from within this, you don't have the nameList yet, this is only index