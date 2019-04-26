#to be ran after reseedBoth.R

#static (outside of monte carlo/resampling, if desire resampling, simply move above set.seed(base))
holdoutSet <- c()

#could use d_combined and do conversion of -9 and -8 to na
# noticed V7562 and V8531 result in no records together when dropping na's... go figure

combined <- NewDF[,as.character(c(newList)),drop=FALSE] 
combined[combined == 0] <- NA
temp <- combined[] %>% filter_all(all_vars(!is.na(.)))
combined <- c()
combined <- temp
combined[combined == -1] <- 0

holdoutSet <- sample(nrow(combined), round(holdoutSetSize*nrow(combined)))

combined.holdoutSet <- c()
combined.holdoutSet <- NewDF[holdoutSet,]

#don't call resampleMC.R from within this, you don't have the nameList yet, this is only index