#to be ran after reseedBoth.R
holdoutSet <- c()
set <- c()
set.ones <- c()
set.zeros <- c()

set <- eval(parse(text=paste("combined.",pairedname, sep = "")))
set.ones <- eval(parse(text=paste("combined.ones.",pairedname, sep = "")))
set.zeros <- eval(parse(text=paste("combined.zeros.",pairedname, sep = "")))

#pairedname_List

#static (outside of monte carlo/resampling, if desire resampling, simply move above set.seed(base))

#could use d_combined and do conversion of -9 and -8 to na
# noticed V7562 and V8531 result in no records together when dropping na's... go figure
#https://stackoverflow.com/questions/5510966/create-a-variable-name-with-paste-in-r
#nrow(set.ones)
holdoutSet <- sample(nrow(set), round(holdoutSetSize*nrow(set)))
holdoutSet.ones <- sample(1:nrow(set.ones), holdoutSetSize*nrow(set.ones))  # 1's for training
holdoutSet.zeros <- sample(1:nrow(set.zeros), holdoutSetSize*nrow(set.zeros))  # 0's for training. Pick as many 0's as 1's

#holdoutSet <- sample(precisionSize, round(holdoutSetSize*precisionSize))

combined.holdoutSet <- c()
combined.holdoutSet <- set[holdoutSet,]
#holdoutSet
#assign(paste("combined.holdoutSet.",pairedname, sep = ""), set[holdoutSet,]) 
assign(paste("combined.holdoutSet.",pairedname, sep = ""), holdoutSet)
assign(paste("combined.holdoutSet.ones.",pairedname, sep = ""), holdoutSet.ones) 
assign(paste("combined.holdoutSet.zeros.",pairedname, sep = ""), holdoutSet.zeros) 
#nrow(set)
length(holdoutSet)
#don't call resampleMC.R from within this, you don't have the nameList yet, this is only index