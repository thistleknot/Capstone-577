data.test <- c()
data.test <- NewDF.holdout[,as.character(c(newList)),drop=FALSE]
data.test[data.test == 0] <- NA
temp <- data.test[] %>% filter_all(all_vars(!is.na(.)))
data.test <- c()
data.test <- temp
data.test[data.test == -1] <- 0
