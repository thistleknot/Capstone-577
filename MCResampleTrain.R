set <- c()
set <- eval(parse(text=paste("combined.preNonHoldoutSet.",pairedname, sep = "")))

#preTrain <- c()
preTrain <- sample(nrow(set[,,drop=FALSE]), round(preTrainSize*nrow(set[,,drop=FALSE])))
#NewDF.preTrain <- c()
#NewDF.preTrain <- NewDF.preNonHoldoutSet[preTrain,,drop=FALSE]

assign(paste("combined.preNonHoldoutSet.",pairedname, sep = ""), set[preTrain,,drop=FALSE]) 

#don't call resampleMC.R from within this, you don't have the nameList yet, this is only index