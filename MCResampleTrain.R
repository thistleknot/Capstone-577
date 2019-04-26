#set <- c()
#set <- eval(parse(text=paste("combined.",pairedname, sep = "")))
#set.ones <- eval(parse(text=paste("combined.ones.",pairedname, sep = "")))
#set.zeros <- eval(parse(text=paste("combined.zeros.",pairedname, sep = "")))
setIndex <- c()
setIndex.ones <- c()
setIndex.zeros <- c()
preTrain.ones <- c()
preTrain.zeros <- c()

#setIndex <- eval(parse(text=paste("combined.holdoutSet.",pairedname, sep = "")))
setIndex.ones <- eval(parse(text=paste("combined.preNonHoldoutSet.ones.",pairedname, sep = "")))
setIndex.zeros <- eval(parse(text=paste("combined.preNonHoldoutSet.zeros.",pairedname, sep = "")))
#nrow(set.ones)
#preTrain <- c()
preTrain.ones <- sample(setIndex.ones, round(preTrainSize*length(setIndex.ones)))
preTrain.zeros <- sample(setIndex.zeros, round(preTrainSize*length(setIndex.zeros)))
#NewDF.preTrain <- c()
#NewDF.preTrain <- NewDF.preNonHoldoutSet[preTrain,,drop=FALSE]

assign(paste("combined.preTrain.ones.",pairedname, sep = ""), preTrain.ones)
assign(paste("combined.preTrain.zeros.",pairedname, sep = ""), preTrain.zeros) 

#don't call resampleMC.R from within this, you don't have the nameList yet, this is only index