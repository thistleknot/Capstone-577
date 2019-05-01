#to be ran after reseedBoth.R
holdoutSet <- c()
set <- c()
set.ones <- c()
set.zeros <- c()
#summary(data[,"V8517"])
set <- eval(parse(text=paste("combined.",pairedname, sep = "")))
set.ones <- eval(parse(text=paste("combined.ones.",pairedname, sep = "")))
set.zeros <- eval(parse(text=paste("combined.zeros.",pairedname, sep = "")))

#pairedname_List

#static (outside of monte carlo/resampling, if desire resampling, simply move above set.seed(base))

#could use d_combined and do conversion of -9 and -8 to na
# noticed V7562 and V8531 result in no records together when dropping na's... go figure
#https://stackoverflow.com/questions/5510966/create-a-variable-name-with-paste-in-r
#nrow(set.ones)
#have to use nrow as upper limit at reseedtest because I'm starting here and derivin an upper limit. and reseedtrain.r will use this as an index (invert)
holdoutSet <- c()
holdoutSet.ones<- c()
holdoutSet.zeros <- c()

#holdoutSet <- sample(nrow(set), round(holdoutSetSize*nrow(set)))
#this resamples
#I see now why class balancing is limited to just instances of 1.  
#Because it most likely is the factor of interest and generally 0's dominate non presence of factor
#I guess we're forcing our data into an equal 50/50 split to find out characteristics that determine that binary equation
#so the concern of undersampling 0's is kind of irrelevant.  Although the elements of the 0 population would be undersampled.
#leading to a bias in itself.

#stratified resampling
#this ends up bootstrapping the amounts found in the population vs focusingg on a clean set of 1's then grabbing 0's
avgCount <- c()
avgCount <- mean(nrow(set.ones),nrow(set.zeros))

reloopFactor <- c()
minFactor <- c()
minFactor <- min(round(holdoutSetSize*nrow(set.ones)),round(holdoutSetSize*nrow(set.zeros)))
reloopFactor <- min(round(holdoutSetSize*nrow(set.ones)),round(holdoutSetSize*nrow(set.zeros)))/round(holdoutSetSize*avgCount)
remainder <- c()
remainder = reloopFactor-floor(reloopFactor)

if(floor(reloopFactor)>0)
{
  for (loops in 1:floor(reloopFactor))
  {
    holdoutSet.ones <- cbind(holdoutSet.ones,sample(1:nrow(set.ones), minFactor))  # 1's for training
    holdoutSet.zeros <- cbind(holdoutSet.zeros,sample(1:nrow(set.zeros), minFactor))  # 0's for training. Pick as many 0's as 1's
  }
}
holdoutSet.ones <- cbind(holdoutSet.ones,sample(1:nrow(set.ones), round(minFactor*remainder)))  # 1's for training
holdoutSet.zeros <- cbind(holdoutSet.zeros,sample(1:nrow(set.zeros), round(minFactor*remainder)))  # 0's for training. Pick as many 0's as 1's

#focus on a clean set of ones, then zeros
#holdoutSet.ones <- sample(1:nrow(set.ones), round(holdoutSetSize*nrow(set.ones)))  # 1's for training
#holdoutSet.zeros <- sample(1:nrow(set.zeros), round(holdoutSetSize*nrow(set.ones)))  # 0's for training. Pick as many 0's as 1's

#holdoutSet <- sample(precisionSize, round(holdoutSetSize*precisionSize))

#combined.holdoutSet <- c()
#combined.holdoutSet <- set[holdoutSet,]
#holdoutSet
#assign(paste("combined.holdoutSet.",pairedname, sep = ""), set[holdoutSet,]) 
#assign(paste("combined.holdoutSet.",pairedname, sep = ""), holdoutSet)
assign(paste("combined.holdoutSet.ones.",pairedname, sep = ""), holdoutSet.ones) 
assign(paste("combined.holdoutSet.zeros.",pairedname, sep = ""), holdoutSet.zeros) 
#nrow(set)
#length(holdoutSet)
#don't call resampleMC.R from within this, you don't have the nameList yet, this is only index