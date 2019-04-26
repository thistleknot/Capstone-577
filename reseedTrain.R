#don't call resampleMC.R from within this, you don't have the nameList yet, this is only index

set <- eval(parse(text=paste("combined.",pairedname, sep = "")))

preNonHoldoutSet <- c()
preNonHoldoutSet <- sample(nrow(set[-holdoutSet,]), round(preNonHoldOutSize*nrow(set[-holdoutSet,])))

#NewDF.preNonHoldoutSet <- c()
#NewDF.preNonHoldoutSet <- NewDF[-holdoutSet,][preNonHoldoutSet,]
assign(paste("combined.preNonHoldoutSet.",pairedname, sep = ""), set[-holdoutSet,][preNonHoldoutSet,]) 
