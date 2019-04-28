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
#sourceDir="/home/rstudio/577/Capstone-577/"
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
  
  input_ones <- c()
  input_zeros <- c()
  #class balance
  #colnames(filtered)
  #summary(filtered[,"V7221"])
  input_ones <- filtered[which(filtered[,1] == 1), ]  # all 1's
  input_zeros <- filtered[which(filtered[,1] == 0), ]  # all 0's
  nrow(input_ones)
  nrow(input_zeros)

  #MC resample  
  #training_ones <- c()
  #training_zeros <- c()
  #bootstraping with montecarlo
  #resamples 5% from both classes which are already pre-cleaned to aggregate up to 20 samples of 5% each between 2 classes is 100% for those 2 classes.
  trainingData <- c()
  #i=1
  for(i in 1:10)
  {
    zerosA.index <- c()
    zerosB.index <- c()
        
    onesA.index <- c()
    onesB.index <- c()
    
    onesA.index <- sample(1:nrow(input_ones), round(0.025*nrow(input_ones)))
    onesB.index <- sample(1:nrow(input_ones), round(0.025*nrow(input_zeros)))
 
    ones <- c()
    ones <- rbind(input_ones[onesA.index,],input_ones[onesB.index,])
    
    zerosA.index <- sample(1:nrow(input_zeros), round(0.025*nrow(input_ones)))
    zerosB.index <- sample(1:nrow(input_zeros), round(0.025*nrow(input_zeros)))
    
    zeros <- c()
    zeros <- rbind(input_zeros[zerosA.index,],input_zeros[zerosB.index,])
    
    #training_ones <- rbind(training_ones,ones[input_ones_training_rows, ])
    #training_zeros <- rbind(training_zeros,input_zeros[input_zeros_training_rows, ])
  
    both <- c()
    both <- rbind(ones, zeros)   
    
    trainingData <- rbind(trainingData,both)
  }
  #print(summary(trainingData))
  #print(summary(training_zeros))
  
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

