#static (outside of monte carlo/resampling, if desire resampling, simply move above set.seed(base))
holdoutSet <- c()
holdoutSet <- sample(nrow(NewDF), round(holdoutSetSize*nrow(NewDF)))

NewDF.holdoutSet <- c()
NewDF.holdoutSet <- NewDF[holdoutSet,]

#don't call resampleMC.R from within this, you don't have the nameList yet, this is only index