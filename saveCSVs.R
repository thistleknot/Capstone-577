#this assumes NewDF exists in memory, which is built from NewDF.R which is called from with cleanDataCode.R
#which means run a minimal case of cleanDataCode.R (widthLoop to c(3) vs (10,5,7,3))
library(stringr)
library(dplyr)
library(bestglm)
#based on seeder from cleandatacode.R
set.seed(5)

#https://www.rdocumentation.org/packages/caret/versions/6.0-82/topics/trainControl
library(caret)

#works
threshold=.25
#threshold=.275
#threshold=.33
#postProcess=1

train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
sourceDir="C:/Users/user/Documents/School/CSUF/ISDS577/projects/Capstone-577/"

files <- list.files(path=paste0(sourceDir,'/output/'), pattern="*final.csv", full.names=TRUE, recursive=FALSE)

ys <- c() 
ys <- list.files(path=paste0(sourceDir,'/output/'), pattern="V*final.csv", full.names=TRUE, recursive=FALSE)
ynames <- c()
for (i in 1:length(files))
{
  temp <- c()
  yname <- c()
  print(stringr::str_remove(ys[i],paste0(sourceDir,"/output/")))
  temp <- stringr::str_remove(ys[i],paste0(sourceDir,"/output/"))
  yname <- substr(temp, 0, 5)
  ynames <- rbind(ynames,yname)
}
ynames
#set.seed(100)  # for repeatability of samples

#works
threshold=.25
#threshold=.275
#threshold=.33
#postProcess=1
for (postProcess in 1:length(files))
{ 
  yname <- ynames[postProcess]
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
  
  #colnames(NewDF)
  filtered <- c()
  filtered <- NewDF[,c(yname,keepers), drop=FALSE]
  filtered[filtered == 0] <- NA
  temp <- filtered[] %>% filter_all(all_vars(!is.na(.)))
  filtered <- temp
  filtered[filtered == -1] <- 0    
  
  #class balance
  input_ones <- filtered[which(filtered[,1] == 1), ]  # all 1's
  input_zeros <- filtered[which(filtered[,1] == 0), ]  # all 0's
  nrow(input_ones)

  #MC resample  
  training_ones <- c()
  training_zeros <- c()
  #bootstraping with montecarlo
  
  for(i in 1:10)
  {
    input_ones_training_rows <- c()
    input_ones_training_rowsA <- c()
    input_ones_training_rowsB <- c()
    
    input_zeros_training_rows <- c()
    input_zeros_training_rowsA <- c()
    input_zeros_training_rowsB <- c()

    input_ones_training_rowsA <- sample(1:nrow(input_ones), 0.1*nrow(input_ones)) # 1's for training
    input_ones_training_rowsB <- sample(1:nrow(input_ones), 0.1*nrow(input_zeros)) # 1's for training
    input_ones_training_rows <- c(input_ones_training_rowsA,input_ones_training_rowsB)
    summary(training_ones)
    
    input_zeros_training_rowsA <- sample(1:nrow(input_zeros), 0.1*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's
    input_zeros_training_rowsB <- sample(1:nrow(input_zeros), 0.1*nrow(input_zeros))  # 0's for training. Pick as many 0's as 1's
    input_zeros_training_rows <- c(input_zeros_training_rowsA,input_ones_training_rowsB)

    training_ones <- rbind(training_ones,input_ones[input_ones_training_rows, ])
    training_zeros <- rbind(training_zeros,input_zeros[input_zeros_training_rows, ])
    print(summary(training_ones))
    print(summary(training_zeros))
  }
  trainingData <- rbind(training_ones, training_zeros) 
  
  holderOfData <- c()
  summary(holderOfData)
  
  holderOfData <- cbind(data.frame(trainingData[,-1 , drop = FALSE]),data.frame(trainingData[,1 , drop = FALSE]))
  B <- suppressMessages(bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=5, REP=1, TopModels=10, BestModels = 10), family=binomial,method = "exhaustive"))
  
  terms <- B$BestModel$coefficients[-1]
  print(terms)
  names2 <- c()
  names2 <- row.names(data.frame(terms))
  
  #CV Model
  print(summary(B$BestModel))
  
  filtered2 <- c()
  filtered2 <- NewDF[,as.character(c(yname,names2)), drop=FALSE]
  filtered2[filtered2 == 0] <- NA
  temp <- filtered2[] %>% filter_all(all_vars(!is.na(.)))
  filtered2 <- temp
  filtered2[filtered2 == -1] <- 0    
  
  trainModel <- suppressMessages(train(filtered2[-1], as.factor(filtered2[,1]),method = "glm",trControl = train.control))
  #testModel <- suppressMessages(train(filtered[-1], as.factor(filtered[,1]), method = "glm",trControl = train.control))
  
  print("population")
  
  #CV terms applied to population
  print(summary(trainModel$finalModel))
  
  #removed medianDirection
  write.csv(filtered2,(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"filtered.csv")))
}
#validate against population    
#population

