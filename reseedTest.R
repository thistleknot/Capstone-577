#to be ran after reseedBoth.R

#static (outside of monte carlo/resampling, if desire resampling, simply move above set.seed(base))
holdoutSet <- c()

#could use d_combined and do conversion of -9 and -8 to na
# noticed V7562 and V8531 result in no records together when dropping na's... go figure
#https://stackoverflow.com/questions/5510966/create-a-variable-name-with-paste-in-r
set <- c()
set <- eval(parse(text=paste("combined.",pairedname, sep = "")))

holdoutSet <- sample(nrow(set), round(holdoutSetSize*nrow(set)))

#combined.holdoutSet <- c()
#combined.holdoutSet <- set[holdoutSet,]
#holdoutSet
#assign(paste("combined.holdoutSet.",pairedname, sep = ""), set[holdoutSet,]) 
assign(paste("combined.holdoutSet.",pairedname, sep = ""), holdoutSet) 
nrow(set)
length(holdoutSet)
#don't call resampleMC.R from within this, you don't have the nameList yet, this is only index