preNonHoldoutSet <- c()
preNonHoldoutSet <- sample(nrow(NewDF[-holdoutSet,]), round(preNonHoldOutSize*nrow(NewDF[-holdoutSet,])))

NewDF.preNonHoldoutSet <- c()
NewDF.preNonHoldoutSet <- NewDF[-holdoutSet,][preNonHoldoutSet,]
