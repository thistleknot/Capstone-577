library(dplyr)
data.train <- c()
data.train <- PostDF.preTrain[,as.character(c(finalList)),drop=FALSE] %>% filter_all(all_vars(!is.na(.)))

data.test <- c()
data.test <- PostDF.holdout[,as.character(finalList), drop=FALSE] %>% filter_all(all_vars(!is.na(.)))
