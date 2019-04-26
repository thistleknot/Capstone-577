combined <- NewDF[,as.character(c(ypair,xpair)),drop=FALSE] 
combined[combined == 0] <- NA
temp <- combined[] %>% filter_all(all_vars(!is.na(.)))
combined <- c()
combined <- temp
combined[combined == -1] <- 0
#print(pairedname)
assign(paste("combined.",pairedname, sep = ""), combined) 
