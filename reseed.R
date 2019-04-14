#reseed
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
filtered.train <- c()
filtered.train <- NewDF.preTrain[,as.character(c(yname,finalList)),drop=FALSE] %>% filter_all(all_vars(!is.na(.)))
filtered.train[filtered.train == 0] <- NA
temp <- filtered.train[] %>% filter_all(all_vars(!is.na(.)))
filtered.train <- c()
filtered.train <- temp
filtered.train[filtered.train == -1] <- 0

filtered.test <- c()
filtered.test <- NewDF.holdout[,as.character(c(yname,finalList)),drop=FALSE] %>% filter_all(all_vars(!is.na(.)))
filtered.test[filtered.test == 0] <- NA
temp <- filtered.test[] %>% filter_all(all_vars(!is.na(.)))
filtered.test <- c()
filtered.test <- temp
filtered.test[filtered.test == -1] <- 0     
#end reseed
