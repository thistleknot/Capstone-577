#to be ran after reseedBoth.R
#static (outside of monte carlo/resampling, if desire resampling, simply move above set.seed(base))
holdoutSet <- c()

holdoutSet <- sample(nrow(combined), round(holdoutSetSize*nrow(combined)))

combined.holdoutSet <- c()
combined.holdoutSet <- NewDF[holdoutSet,]

#don't call resampleMC.R from within this, you don't have the nameList yet, this is only index

preNonHoldoutSet <- c()
preNonHoldoutSet <- sample(nrow(NewDF[-holdoutSet,]), round(preNonHoldOutSize*nrow(NewDF[-holdoutSet,])))

NewDF.preNonHoldoutSet <- c()
NewDF.preNonHoldoutSet <- NewDF[-holdoutSet,][preNonHoldoutSet,]
