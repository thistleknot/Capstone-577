#don't call resampleMC.R from within this, you don't have the nameList yet, this is only index
set <- c()
set <- eval(parse(text=paste("combined.",pairedname, sep = "")))[-holdoutSet,]

preNonHoldoutSet <- c()
#these are new #'s
#set[-holdoutSet,]
#View(id(set[-holdoutSet,]))

#don't use range <- 1:nrow(set), because if you do, you don't pass the original index
#set[-holdoutSet,]
range <- 1:nrow(set)

available <- range[-c(holdoutSet)]
#length(available)
#length(holdoutSet)
#nrow(set)
preNonHoldoutSet <- sample(available, round(preNonHoldOutSize*length(available)))

#NewDF.preNonHoldoutSet <- c()
#NewDF.preNonHoldoutSet <- NewDF[-holdoutSet,][preNonHoldoutSet,]
#this is the total data, I need to deselect the -holdoutSet
#where does this holdoutSet value derive from? reseedTest.  Questionable if this file sees it (encapsulation)
#But if it doesn't, it should throw an error on a fresh run.
assign(paste("combined.preNonHoldoutSet.",pairedname, sep = ""), set[preNonHoldoutSet,])
#assign(paste("combined.preNonHoldoutSet.",pairedname, sep = ""), preNonHoldoutSet) 
