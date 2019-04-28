#this assumes NewDF exists in memory, which is built from NewDF.R which is called from with cleanDataCode.R
#which means run a minimal case of cleanDataCode.R (widthLoop to c(3) vs (10,5,7,3))
library(stringr)
files <- list.files(path=paste0(sourceDir,'/output/'), pattern="*final.csv", full.names=TRUE, recursive=FALSE)

#works
#threshold=.275
threshold=.45

for (postProcess in 1:length(files))
{ 
  print_tabled <- c()
  print_tabled <- read.csv(files[postProcess], header=TRUE, sep=",")[,-1,drop=FALSE]
  print(c("final: ",print_tabled))
  
  keepersPre <- c()
  keepersPre <- data.frame(na.omit(data.frame(print_tabled)))
  keepersPreSorted <- c()
  keepersPreSorted <- keepersPre[order(-keepersPre$Freq),] 
  
  print(keepersPreSorted)
  plot(keepersPreSorted$Freq)
  hist(keepersPreSorted$Freq)
  
  keepers <- c()
  
  #what a pain
  #hist(tabulatedCrossValidated)
  keepers <- as.character(keepersPre$tabulatedCrossValidated[keepersPre$Freq > (threshold)])
  print(c("keep: > ",threshold,length(keepers),keepers))
  
  filtered <- c()
  filtered <- NewDF[,as.character(c(yname,keepers)), drop=FALSE]
  filtered[filtered == 0] <- NA
  temp <- filtered[] %>% filter_all(all_vars(!is.na(.)))
  filtered <- temp
  filtered[filtered == -1] <- 0    
  trainModel <- suppressMessages(train(filtered[-1], as.factor(filtered[,1]),method = "glm",trControl = train.control))
  testModel <- suppressMessages(train(filtered[-1], as.factor(filtered[,1]), method = "glm",trControl = train.control))
  
  print("population")
  print(summary(trainModel$finalModel))
  
  #removed medianDirection
  write.csv(filtered,(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"filtered.csv")))
}
#validate against population    
#population

