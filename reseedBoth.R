combined <- NewDF[,as.character(c(ypair,xpair)),drop=FALSE] 
#summary(combined)
combined[combined == 0] <- NA
temp <- combined[] %>% filter_all(all_vars(!is.na(.)))
combined <- c()
combined <- temp
combined[combined == -1] <- 0
#print(pairedname)
combined.ones <- c()
combined.zeros <- c()
combined.ones <- combined[which(combined[,1,drop=FALSE] == 1), ]  # all 1's
combined.zeros <- combined[which(combined[,1,drop=FALSE] == 0), ]  # all 0's
assign(paste("combined.",pairedname, sep = ""), combined)
assign(paste("combined.ones.",pairedname, sep = ""), combined.ones) 
assign(paste("combined.zeros.",pairedname, sep = ""), combined.zeros) 