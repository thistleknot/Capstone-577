#to be ran after reseedBoth.R
set <- c()
set.ones <- c()
set.zeros <- c()

set <- eval(parse(text=paste("combined.",pairedname, sep = "")))
set.ones <- eval(parse(text=paste("combined.ones.",pairedname, sep = "")))
set.zeros <- eval(parse(text=paste("combined.zeros.",pairedname, sep = "")))

#set.holdoutSet <- c()
set.holdoutSet.ones <- c()
set.holdoutSet.zeros <- c()
#index's
set.holdoutSet.ones <- eval(parse(text=paste("combined.holdoutSet.ones.",pairedname, sep = "")))
set.holdoutSet.zeros <- eval(parse(text=paste("combined.holdoutSet.zeros.",pairedname, sep = "")))

set.prenonholdout.ones <- c()
set.prenonholdout.zeros <- c()
#set.ones <- eval(parse(text=paste("combined.ones.",pairedname, sep = "")))

#don't use range <- 1:nrow(set), because if you do, you don't pass the original index
#set[-holdoutSet,]
rangeOnes <- c()
rangeZeros <- c()
rangeOnes <- 1:nrow(set.ones)
rangeZeros <- 1:nrow(set.zeros)

#index's
availableset.prenonholdout.ones <- c()
availableset.prenonholdout.zeros <- c()
availableset.prenonholdout.ones <- rangeOnes[-c(set.holdoutSet.ones)]
availableset.prenonholdout.zeros <- rangeZeros[-c(set.holdoutSet.ones)]

preNonHoldoutSet.ones <- c()
preNonHoldoutSet.zeros <- c()

avgCount <- c()
avgCount <- mean(length(availableset.prenonholdout.ones),length(availableset.prenonholdout.zeros))

reloopFactor <- c()
minFactor <- c()
minFactor <- min(round(preNonHoldOutSize*length(availableset.prenonholdout.ones)),round(preNonHoldOutSize*nrow(availableset.prenonholdout.zeros)))
reloopFactor <- min(round(preNonHoldOutSize*length(availableset.prenonholdout.ones)),round(preNonHoldOutSize*length(availableset.prenonholdout.zeros)))/round(preNonHoldOutSize*avgCount)
remainder <- c()
remainder = reloopFactor-floor(reloopFactor)

if(floor(reloopFactor)>0)
{
  for (loops in 1:floor(reloopFactor))
  {
    preNonHoldoutSet.ones <- cbind(preNonHoldoutSet.ones,sample(availableset.prenonholdout.ones, minFactor))
    preNonHoldoutSet.zeros <- cbind(preNonHoldoutSet.zeros,sample(availableset.prenonholdout.zeros, minFactor))
    
    #holdoutSet.ones <- cbind(holdoutSet.ones,sample(1:nrow(set.ones), minFactor))  # 1's for training
    #holdoutSet.zeros <- cbind(holdoutSet.zeros,sample(1:nrow(set.zeros), minFactor))  # 0's for training. Pick as many 0's as 1's
  }
}
preNonHoldoutSet.ones <- cbind(preNonHoldoutSet.ones,sample(1:nrow(availableset.prenonholdout.ones), round(availableset.prenonholdout.ones*remainder)))  # 1's for training
preNonHoldoutSet.zeros <- cbind(preNonHoldoutSet.zeros,sample(1:nrow(availableset.prenonholdout.zeros), round(availableset.prenonholdout.zeros*remainder)))  # 0's for training. Pick as many 0's as 1's


#preNonHoldoutSet.ones <- sample(availableset.prenonholdout.ones, round(preNonHoldOutSize*length(availableset.prenonholdout.ones)))
#preNonHoldoutSet.zeros <- sample(availableset.prenonholdout.zeros, round(preNonHoldOutSize*length(availableset.prenonholdout.ones)))

#NewDF.preNonHoldoutSet <- c()
#NewDF.preNonHoldoutSet <- NewDF[-holdoutSet,][preNonHoldoutSet,]
#this is the total data, I need to deselect the -holdoutSet
#where does this holdoutSet value derive from? reseedTest.  Questionable if this file sees it (encapsulation)
#But if it doesn't, it should throw an error on a fresh run.
#assign(paste("combined.preNonHoldoutSet.",pairedname, sep = ""), set[preNonHoldoutSet,])
assign(paste("combined.preNonHoldoutSet.ones.",pairedname, sep = ""), preNonHoldoutSet.ones)
assign(paste("combined.preNonHoldoutSet.zeros.",pairedname, sep = ""), preNonHoldoutSet.zeros) 
