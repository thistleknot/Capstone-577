data.train <- c()
data.train <- NewDF.preTrain[,as.character(c(newList)),drop=FALSE] %>% filter_all(all_vars(!is.na(.)))
data.train[data.train == 0] <- NA
temp <- data.train[] %>% filter_all(all_vars(!is.na(.)))
data.train <- c()
data.train <- temp
data.train[data.train == -1] <- 0

data.test <- c()
data.test <- NewDF.holdout[,as.character(newList), drop=FALSE] %>% filter_all(all_vars(!is.na(.)))
data.test[data.test == 0] <- NA
temp <- data.test[] %>% filter_all(all_vars(!is.na(.)))
data.test <- c()
data.test <- temp
data.test[data.test == -1] <- 0