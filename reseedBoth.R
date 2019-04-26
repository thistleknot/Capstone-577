combined <- NewDF[,as.character(c(newList)),drop=FALSE] 
combined[combined == 0] <- NA
temp <- combined[] %>% filter_all(all_vars(!is.na(.)))
combined <- c()
combined <- temp
combined[combined == -1] <- 0
#print(pairedname)
assign(paste("data",pairedname, sep = ""), combined) 
#assign(paste(pairdatasetname),combined)

#assign(paste(pairdatasetname),5)

#ls()