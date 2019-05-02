#to be ran after reseedTest.R
set <- c()
set.ones <- c()
set.zeros <- c()

set <- eval(parse(text=paste("combined.",pairedname, sep = "")))
set.ones <- eval(parse(text=paste("combined.ones.",pairedname, sep = "")))
nrow(set.ones)
max(as.numeric(rownames(holdoutSet.ones)))
set.zeros <- eval(parse(text=paste("combined.zeros.",pairedname, sep = "")))

#not working with an index
#set.holdoutSet <- c()
set.holdoutSet.ones <- c()
#nrow(set.holdoutSet.ones)
set.holdoutSet.zeros <- c()
#index's
set.holdoutSet.ones <- eval(parse(text=paste("combined.holdoutSet.ones.",pairedname, sep = "")))
nrow(set.holdoutSet.ones)
#max(rownames(set.holdoutSet.ones))
set.holdoutSet.zeros <- eval(parse(text=paste("combined.holdoutSet.zeros.",pairedname, sep = "")))

holdoutSetIDs <- unique(c(as.numeric(rownames(set.holdoutSet.ones),rownames(set.holdoutSet.zeros))))

set.prenonholdoutSet <- c()
set.prenonholdoutSet <- set[!holdoutSetIDs %in% set,]

#View(set.holdoutSet.ones)
set.prenonholdout.ones <- c()
set.prenonholdout.zeros <- c()

set.prenonholdout.ones <- set.prenonholdoutSet[which(set.prenonholdoutSet[,1,drop=FALSE] == 1), ]  # all 1's
set.prenonholdout.zeros <- set.prenonholdoutSet[which(set.prenonholdoutSet[,1,drop=FALSE] == 0), ]  # all 0's

#set.ones <- eval(parse(text=paste("combined.ones.",pairedname, sep = "")))

preNonHoldoutSet.ones <- c()
preNonHoldoutSet.zeros <- c()

avgCountHalved <- c()
avgCountHalved <- mean(nrow(set.prenonholdout.ones),nrow(set.prenonholdout.zeros))/2

reloopFactor <- c()
minFactor <- c()
minFactor <- min(round(preNonHoldOutSize*nrow(set.prenonholdout.ones)),round(preNonHoldOutSize*nrow(set.prenonholdout.ones)))
reloopFactor <- min(round(preNonHoldOutSize*nrow(set.prenonholdout.zeros)),round(preNonHoldOutSize*nrow(set.prenonholdout.zeros)))/round(preNonHoldOutSize*avgCountHalved)
remainder <- c()
remainder = reloopFactor-floor(reloopFactor)

if(floor(reloopFactor)>0)
{
  for (loops in 1:floor(reloopFactor))
  {
    #generates index and samples in place.  I have to do this, else repeat index's get stored as .1's and .2' respectively
    preNonHoldoutSet.ones <- rbind(preNonHoldoutSet.ones,set[sample(c(as.numeric(rownames((set.prenonholdout.ones)))), minFactor),])  # 1's for training
    preNonHoldoutSet.zeros <- rbind(preNonHoldoutSet.zeros,set[sample(c(as.numeric(rownames((set.prenonholdout.zeros)))), minFactor),])  # 0's for training. Pick as many 0's as 1's
    #ones.index <- rbind(ones.index,input_ones[sample(c(rownames(input_ones)), minFactor),])  # 1's for training
    #zeros.index <- rbind(zeros.index,input_zeros[sample(c(rownames(input_zeros)), minFactor),])  # 0's for training. Pick as many 0's as 1's
  }
}
#generates index and samples in place.  I have to do this, else repeat index's get stored as .1's and .2' respectively
#because different length than above, I have to switch to c
preNonHoldoutSet.ones <- rbind(preNonHoldoutSet.ones,set[sample(c(as.numeric(rownames((set.prenonholdout.ones)))), minFactor*remainder),])  # 1's for training
preNonHoldoutSet.zeros <- rbind(preNonHoldoutSet.zeros,set[sample(c(as.numeric(rownames((set.prenonholdout.zeros)))), minFactor*remainder),])  # 0's for training. Pick as many 0's as 1's

#NewDF.preNonHoldoutSet <- c()
#NewDF.preNonHoldoutSet <- NewDF[-holdoutSet,][preNonHoldoutSet,]
#this is the total data, I need to deselect the -holdoutSet
#where does this holdoutSet value derive from? reseedTest.  Questionable if this file sees it (encapsulation)
#But if it doesn't, it should throw an error on a fresh run.
#assign(paste("combined.preNonHoldoutSet.",pairedname, sep = ""), set[preNonHoldoutSet,])
assign(paste("combined.preNonHoldoutSet.ones.",pairedname, sep = ""), preNonHoldoutSet.ones)
assign(paste("combined.preNonHoldoutSet.zeros.",pairedname, sep = ""), preNonHoldoutSet.zeros) 