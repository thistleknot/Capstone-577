#this assumes NewDF exists in memory, which is built from NewDF.R which is called from with cleanDataCode.R
#this means NewDF is preconverted to a greater or greaterEqual
#which means run a minimal case of cleanDataCode.R (widthLoop to c(3) vs (10,5,7,3))
library(stringr)
library(dplyr)
library(bestglm)
library(ModelMetrics)
library("ROCR")
library("caret")
library(corrplot)
library(bestglm)
library(outliers)
library(factoextra)
library(Rfast)
#install.packages("cutpointr")
library(cutpointr)
library(InformationValue)
library(tibble)
library(mctest)
#library(ggthemr)
#library(lsplsGlm)
library(car)

#based on seeder from cleandatacode.R
set.seed(5)

#https://www.rdocumentation.org/packages/caret/versions/6.0-82/topics/trainControl
library(caret)

#works
threshold=.25
#threshold=.275
#threshold=.35
#postProcess=1

train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 1)

source(paste0(sourceDir,"unbalanced_functions.R"))
source(paste0(sourceDir,"sub_returnCVNames.R"))

files <- list.files(path=paste0(sourceDir,'/output/'), pattern="*final.csv", full.names=TRUE, recursive=FALSE)


linux=0
if(linux)
{
  sourceDir="/home/rstudio/577/Capstone-577/"
  
  ys <- c() 
  ys <- list.files(path=paste0(sourceDir,'/output/'), pattern="V*final.csv", full.names=TRUE, recursive=FALSE)
  ynames <- c()
  for (i in 1:length(files))
  {
    temp <- c()
    yname <- c()
    print(stringr::str_remove(ys[i],paste0(sourceDir,"/output/")))
    temp <- stringr::str_remove(ys[i],paste0(sourceDir,"/output//"))
    yname <- substr(temp, 0, 5)
    ynames <- rbind(ynames,yname)
  }
  ynames
}
if(!linux)
{
  sourceDir="C:/Users/user/Documents/School/CSUF/ISDS577/projects/Capstone-577/"
  
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
}

#set.seed(100)  # for repeatability of samples

#postProcess=1
for (postProcess in 1:length(files))
{ 
  print(files[postProcess])
  yname <- ynames[postProcess]
  print_tabled <- c()
  print_tabled <- read.csv(files[postProcess], header=TRUE, sep=",")[,-1,drop=FALSE]
  print(c("y:",yname))
  print(c("final: ",print_tabled))
  
  keepersPre <- c()
  keepersPre <- data.frame(na.omit(data.frame(print_tabled)))
  keepersPreSorted <- c()
  keepersPreSorted <- keepersPre[order(-keepersPre$Freq),] 
  
  print(keepersPreSorted)
  plot(keepersPreSorted$Freq)
  hist(keepersPreSorted$Freq)
#  dev.off()
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

  avgCountHalved <- c()
  avgCountHalved <- mean(nrow(input_ones),nrow(input_zeros))/2

  trainingData <- c()
  ones.index <- c()
  zeros.index <- c()
  
  reloopFactor <- c()
  minFactor <- c()
  
  minFactor <- min(round(.1*nrow(input_ones)),round(.1*nrow(input_zeros)))
  reloopFactor <- min(round(.1*nrow(input_ones)),round(.1*nrow(input_zeros)))/round(.1*avgCountHalved)
  remainder <- c()
  remainder = reloopFactor-floor(reloopFactor)  
  #i=1
  #this might break depending on the size of 1's or 0's, but I hope 
  for(i in 1:10)
  {
   
    if(floor(reloopFactor)>0)
    {
      for (loops in 1:floor(reloopFactor))
      {
        #generates index and samples in place.  I have to do this, else repeat index's get stored as .1's and .2' respectively
        ones.index <- rbind(ones.index,input_ones[sample(c(rownames(input_ones)), minFactor),])  # 1's for training
        zeros.index <- rbind(zeros.index,input_zeros[sample(c(rownames(input_zeros)), minFactor),])  # 0's for training. Pick as many 0's as 1's
      }
    }
    ones.index <- rbind(ones.index,input_ones[sample(c(rownames(input_ones)), minFactor*remainder),])  # 1's for training
    zeros.index <- rbind(zeros.index,input_zeros[sample(c(rownames(input_zeros)), minFactor*remainder),])  # 0's for training. Pick as many 0's as 1's
    
    both <- c()
    both <- rbind(ones.index, zeros.index)
    
    #https://stackoverflow.com/questions/2370515/how-to-get-row-index-number-in-r
    mix <- c()
    mix <- sample(c(rownames(both)),round(nrow(both)/2))
    
    #mix <- sample(both,length(both)/2)
    #colnames(mix) <- colnames(trainingData)
    trainingData <- rbind(trainingData, both[mix,])
  
  }  
  finalTraining <- c()
  finalTrainingI <- c()
  size <- c()
  size <- round(nrow(trainingData)/10)
  finalTrainingI <- sample(c(rownames(trainingData)),size)
  finalTraining <- trainingData[finalTrainingI,]
  nrow(finalTraining)
  summary(finalTraining)
  
  x <- c()
  y <- c()
  y <- finalTraining[,1,drop=FALSE]
  x <- finalTraining[,-1,drop=FALSE]

  holderOfData <- c()
  holderOfData <- cbind(x,y)
  holderOfDataI <- c()
  holderOfDataI <- sample(1:nrow(finalTraining),round(nrow(finalTraining)*.5))
  nrow(finalTraining[holderOfDataI,])
  
  terms <- c()
  terms <- sub_returnCVNames(finalTraining[holderOfDataI,])
  #B <- suppressMessages(bestglm(Xy = finalTraining[holderOfDataI,], IC="CV", CVArgs=list(Method="HTF", K=5, REP=3, TopModels=10, BestModels = 10), family=binomial))
  #B <- lm()
  
  print(summary(finalTraining[holderOfDataI,]))
  #print(summary(training_zeros))
  
  trainModel <- c()
  #trainModel <- B$BestModel
 
  #trainModel <- glm(finalTraining[holderOfDataI,][c(terms,yname)],family=binomial(link="logit"))
  #finalTraining[holderOfDataI,][c(terms,yname)]
  
  filtered2 <- c()
  filtered2 <- finalTraining[holderOfDataI,][c(yname,terms)]
  #train(x,y)
  trainModel <- train(x=filtered2[-1], y=as.factor(filtered2[,1]),method = "glm",trControl = train.control)
  
  #any column
  #https://stackoverflow.com/questions/46285484/if-any-column-in-a-row-meets-condition-than-mutate-column
  #df$c[apply(df == 7, 1, any)] <- 100
  
  #includes proportion of variance
  summary(prcomp(x, center=TRUE, scale=TRUE))
  pca <- summary(prcomp(x, center=TRUE, scale=TRUE))$importance
  
  trainingData <- c()
  trainingData <- finalTraining[holderOfDataI,][c(yname,terms)]
  #pc plot
  #jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"pcaPct.jpg"), width = 400, height = 400)
  #plot(pca[3,1:ncol(pca)])
  #dev.off()
  
  #correlation plot of sample along with pca
  #jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"corrplot.jpg"), width = 400, height = 400)
  #corrplot(cor(cbind(x,prcomp(x, center=TRUE, scale=TRUE)$x)))
  #dev.off()

  #http://rfaqs.com/mctest-r-package-detection-collinearity-among-regressors
  
  #if(length(colnames(x))!=1) omcdiag(x, y, Inter=FALSE)
  result <- c()
  #result <- mctest::omcdiag(x,y, detr=0.001, conf=0.99)
  #https://rdrr.io/cran/mctest/man/mctest.html
  result <- mctest(x, y, type="i", method="VIF")
  print(result)

  #http://r-statistics.co/Logistic-Regression-With-R.html
 
  #logitMod <- glm(ABOVE50K ~ RELATIONSHIP + AGE + CAPITALGAIN + OCCUPATION + EDUCATIONNUM, data=trainingData, family=binomial(link="logit"))
  print("MC summary")
  print(summary(trainModel$finalModel))
  
  #res <- cor(data.train)
  res <- cor(trainingData)
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"corrplot.jpg"), width = 400, height = 400)
  corrplot(res)
  dev.off()
  
  #x= c()
  #y= c()
  #yname <- c()
  
  #x=data.train[,-1]
  #x.test=data.test[,-1]
  #y=data.train[,1]
  
 
  #yhat.test = predict(trainModel$finalModel, x.test)
  #ytest = data.test[,1]
  
  #needs to be continuous
  #https://arulvelkumar.wordpress.com/2017/09/03/prediction-function-in-r-number-of-cross-validation-runs-must-be-equal-for-predictions-and-labels/
  #https://hopstat.wordpress.com/2014/12/19/a-small-introduction-to-the-rocr-package/
  #pred <- prediction(ROCR.simple$predictions,ROCR.simple$labels)
  #labels = classification (i.e. true value)
  
  #yhat = predict(trainModel, PostDF[,-1,drop=FALSE])
  yhat <- c()
  predicted <- c()
  predicted <- plogis(predict(trainModel$finalModel, trainingData[-1]))  # predicted scores
  #summary(predicted)
  
  yhat <- round(predicted)
  ytest <- trainingData[1]
  #summary(trainModel)
  #table(yhat)
  #ytest = trainingData[,1]
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"MCytestdiffyhat.jpg"), width = 400, height = 400)
  #diff <- c()
  #diff <- ytest-yhat
  hist(trainModel$finalModel$residuals)
  dev.off()
  
  pred <- prediction(yhat,ytest)
  #nrow(yhat)
  #nrow(ytest)
  roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"roc.jpg"), width = 400, height = 400)
  plot(roc.perf)
  abline(a=0, b= 1)
  dev.off()

  gain <- performance(pred, "tpr", "rpp")
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"gain.jpg"), width = 400, height = 400)
  plot(gain, main = "Gain Chart")
  abline(a=0, b= 1)
  dev.off()
    
  #plot(yhat,ytest)
  
  yhat <- data.frame(yhat)
  colnames(yhat) <- "yhat"
  
  ytest <- data.frame(ytest)
  colnames(ytest) <- "ytest"
  
  #https://www.r-bloggers.com/r-sorting-a-data-frame-by-the-contents-of-a-column/
  
  
  #http://ethen8181.github.io/machine-learning/unbalanced/unbalanced.html
  #assymetric costs?  My hope is to maximize tp and tn in two separate matrix
  #cost_fp <- 100
  #cost_fn <- 100
  #I'm using cutoff instead
  #roc_info <- ROCInfo( data = cm_info$data, predict = "predict", actual = "actual", cost.fp = cost_fp, cost.fn = cost_fn )
  #grid.draw(roc_info$plot)
  #table(yhat)
  
  #print(paste("equal cutpoint:",round(roc_info$cutoff,3)))
  
  #class 1
  #Maximize sensitivity given a minimal value of specificity
  #cp_sens <- cutpointr(cbind(yhat,ytest), yhat, ytest, method = maximize_metric, metric = sens_constrain)
  
  #https://www.rdocumentation.org/packages/InformationValue/versions/1.2.3/topics/optimalCutoff
  optCutOff_sens <- optimalCutoff(ytest, optimiseFor="Ones", yhat)
  optCutOff_top <- .99
  optCutOff_center <- optimalCutoff(ytest, optimiseFor="Both", yhat)
  optCutOff_cen <- .5
  optCutOff_spec <- optimalCutoff(ytest, optimiseFor="Zeros", yhat)
  
  #both classes
  #cp_center <- cutpointr(cbind(yhat,ytest), yhat, ytest, method = maximize_metric, metric = sum_sens_spec)
  
  #class 0
  #Maximize specificity given a minimal value of sensitivity
  #cp_spec <- cutpointr(cbind(yhat,ytest), yhat, ytest, method = maximize_metric, metric = spec_constrain)
  
  
  #ggthemr("flat")
  #cm_info$plot
  
  #pred <- prediction(yhat.test,data.test[,1])
  #roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
  #plot(roc.perf)
  #abline(a=0, b= 1)
  
  #this is manually converting them
  #test classification using best linear model
  #print(table(yhat))
  #View(yhat)
  #this is where an arbitary threshold is set.
  yhat.transformed_sens = rep(0, nrow(trainingData))
  yhat.transformed_sens[round(yhat,4) >= round(optCutOff_sens,4)] = 1
  yhat.transformed_sens[yhat < optCutOff_sens] = 0
  misClassError(yhat.transformed_sens, ytest, threshold = optCutOff_sens)

  yhat.transformed_top = rep(0, nrow(trainingData))
  yhat.transformed_top[round(yhat,4) >= round(optCutOff_top,4)] = 1
  yhat.transformed_top[yhat < optCutOff_top] = 0
  misClassError(yhat.transformed_top, ytest, threshold = optCutOff_top)

  yhat.transformed_cen = rep(0, nrow(trainingData))
  yhat.transformed_cen[round(yhat,4) >= round(optCutOff_cen,4)] = 1
  yhat.transformed_cen[yhat < optCutOff_cen] = 0
  misClassError(yhat.transformed_cen, ytest, threshold = optCutOff_cen)
    
  yhat.transformed_center = rep(0, nrow(trainingData))
  yhat.transformed_center[round(yhat,4) >= round(optCutOff_center,4)] = 1
  yhat.transformed_center[round(yhat,4) < optCutOff_center] = 0
  misClassError(yhat.transformed_center, ytest, threshold = optCutOff_center)
  
  yhat.transformed_spec = rep(0, nrow(trainingData))
  yhat.transformed_spec[round(yhat,4) >= round(optCutOff_spec,4)] = 1
  yhat.transformed_spec[round(yhat,4) < optCutOff_spec] = 0
  misClassError(yhat.transformed_spec, ytest, threshold = optCutOff_spec)
  
  hist(ytest[,1])
  
  #sum(yhat.transformed)
  ytemp<-c()
  ytemp = data.frame(yhat.transformed_center)[,,drop=FALSE]
  colnames(ytemp)<-"yhat"
  #http://ethen8181.github.io/machine-learning/unbalanced/unbalanced.html
  #cm_info_ce <- ConfusionMatrixInfo( data = cbind(ytemp,ytest), predict = "yhat", actual = "ytest", cutoff = optCutOff_center )
  cm_info_ce <- ConfusionMatrixInfo( data = cbind(ytemp,ytest), predict = "yhat", actual = "ytest", cutoff = optCutOff_center )
  
  ytemp<-c()
  ytemp = data.frame(yhat.transformed_sens)[,,drop=FALSE]
  colnames(ytemp)<-"yhat"
  cm_info_se <- ConfusionMatrixInfo( data = cbind(ytemp,ytest), predict = "yhat", actual = "ytest", cutoff = optCutOff_sens )
  
  ytemp<-c()
  ytemp = data.frame(yhat.transformed_top)[,,drop=FALSE]
  colnames(ytemp)<-"yhat"
  cm_info_top <- ConfusionMatrixInfo( data = cbind(ytemp,ytest), predict = "yhat", actual = "ytest", cutoff = optCutOff_top )
  
  ytemp<-c()
  ytemp = data.frame(yhat.transformed_cen)[,,drop=FALSE]
  colnames(ytemp)<-"yhat"
  cm_info_cen <- ConfusionMatrixInfo( data = cbind(ytemp,ytest), predict = "yhat", actual = "ytest", cutoff = optCutOff_cen )
  
  ytemp<-c()
  ytemp = data.frame(yhat.transformed_spec)[,,drop=FALSE]
  colnames(ytemp)<-"yhat"
  cm_info_sp <- ConfusionMatrixInfo( data = cbind(ytemp,ytest), predict = "yhat", actual = "ytest", cutoff = optCutOff_spec )
  #print(cm_info$data[order(cm_info$data$predict),])

  total_predictions = nrow(data.frame(yhat.transformed_sens))
  correct_predictions = sum(yhat.transformed_sens == ytest)
  classification_accuracy = correct_predictions / total_predictions
  error_rate_se = (1 - (correct_predictions / total_predictions))

  total_predictions = nrow(data.frame(yhat.transformed_top))
  correct_predictions = sum(yhat.transformed_top == ytest)
  classification_accuracy = correct_predictions / total_predictions
  error_rate_top = (1 - (correct_predictions / total_predictions))
  
  total_predictions = nrow(data.frame(yhat.transformed_cen))
  correct_predictions = sum(yhat.transformed_cen == ytest)
  classification_accuracy = correct_predictions / total_predictions
  error_rate_cen = (1 - (correct_predictions / total_predictions))
  
  total_predictions = nrow(data.frame(yhat.transformed_center))
  correct_predictions = sum(yhat.transformed_center == ytest)
  classification_accuracy = correct_predictions / total_predictions
  error_rate_ce = (1 - (correct_predictions / total_predictions))

  total_predictions = nrow(data.frame(yhat.transformed_spec))
  correct_predictions = sum(yhat.transformed_spec == ytest)
  classification_accuracy = correct_predictions / total_predictions
  error_rate_sp = (1 - (correct_predictions / total_predictions))

  #https://machinelearningmastery.com/confusion-matrix-machine-learning/
  #https://www.rdocumentation.org/packages/caret/versions/3.45/topics/confusionMatrix
  #https://www.datacamp.com/community/tutorials/confusion-matrix-calculation-r
  #https://rdrr.io/cran/caret/man/confusionMatrix.html
  
  #class 1
  results_se <- c()
  results_se <- confusionMatrix(yhat.transformed_sens, ytest[,1])
  
  results_ce <- c()
  results_ce <- confusionMatrix(yhat.transformed_center, ytest[,1])
 
  #class 0
  results_sp <- c()
  results_sp <- confusionMatrix(yhat.transformed_spec, ytest[,1])
  
  results_top <- c()
  results_top <- confusionMatrix(yhat.transformed_top, ytest[,1])
  
  results_cen <- c()
  results_cen <- confusionMatrix(yhat.transformed_cen, ytest[,1])
  
  print(c("optCutOff_sens:",round(optCutOff_sens,4)))
  hist(yhat.transformed_sens)
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"cm_info_se.jpg"), width = 400, height = 800)
  plot(cm_info_se$plot)
  dev.off()
  print(paste("error rate sens:",round(error_rate_se,4)))
  print("yhat.transformed_sens matrix")
  print(c("n:",sum(results_se)))
  print(round(results_se/sum(results_se),4))
  
  print(c("optCutOff_center",round(optCutOff_center,4)))
  print(paste("error rate c1:",round(error_rate_ce,4)))
  print("yhat.transformed_center matrix")
  print(round(results_ce/sum(results_ce),4))
  hist(yhat.transformed_center)
  #http://www.sthda.com/english/wiki/creating-and-saving-graphs-r-base-graphs
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"cm_info_ce.jpg"), width = 400, height = 800)
  plot(cm_info_ce$plot)
  dev.off()
  
  print(c("optCutOff_cen",round(optCutOff_cen,4)))
  print(paste("error rate c2:",round(error_rate_cen,4)))
  print("yhat.transformed_cen matrix")
  print(round(results_cen/sum(results_cen),4))
  hist(yhat.transformed_cen)
  #http://www.sthda.com/english/wiki/creating-and-saving-graphs-r-base-graphs
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"cm_info_cen.jpg"), width = 400, height = 800)
  plot(cm_info_cen$plot)
  dev.off()
  
  print(c("optCutOff_top",round(optCutOff_top,4)))
  print(paste("error rate top:",round(error_rate_top,4)))
  print("yhat.transformed_top matrix")
  print(round(results_top/sum(results_top),4))
  hist(yhat.transformed_top)
  #http://www.sthda.com/english/wiki/creating-and-saving-graphs-r-base-graphs
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"cm_info_top.jpg"), width = 400, height = 800)
  plot(cm_info_cen$plot)
  dev.off()
  
  print(c("optCutOff_spec",round(optCutOff_spec,4)))
  print(paste("error rate spec:",round(error_rate_sp,4)))
  print("yhat.transformed_spec matrix")
  print(round(results_sp/sum(results_sp),4))
  hist(yhat.transformed_spec)
  #http://www.sthda.com/english/wiki/creating-and-saving-graphs-r-base-graphs
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"cm_info_spec.jpg"), width = 400, height = 800)
  plot(cm_info_sp$plot)
  dev.off()
  
  #converts to logit
  #apply new terms to class balanaced mc data
  tempNew <- c()
  tempNew <- trainingData[terms]
  
  trainModel <- c()
  holderOfData <- c()
  holderOfData <- cbind(tempNew[-1,],tempNew[1,])
  
  #MCpopModel <- glm(holderOfData,family=binomial(link="logit"))
  #jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"histMC2PopPredicted.jpg"), width = 400, height = 400)
  #dev.off()
  
  #CV Model
  #print("CV Model")
  #print(summary(B$BestModel))
  
  filtered2 <- c()
  filtered2 <- NewDF[,as.character(c(yname,terms)), drop=FALSE]
  filtered2[filtered2 == 0] <- NA
  temp <- filtered2[] %>% filter_all(all_vars(!is.na(.)))
  filtered2 <- temp
  filtered2[filtered2 == -1] <- 0    
  
  colnames(trainingData)
  
  predMCPop <- plogis(predict(B$BestModel, filtered2[terms]))  # predicted scores
  
  print(c("MC model applied to Pop :",(rmse((filtered2[,1]),(round(predMCPop))))))
  
  #popModel <- suppressMessages(train(filtered2[-1], as.factor(filtered2[,1]),method = "glm",trControl = train.control))
  popModel <- train(filtered2[-1], as.factor(filtered2[,1]),method = "glm",trControl = train.control)
  #popModel <- glm(holderOfData,family=binomial(link="logit"))
  
  predPop <- plogis(predict(popModel$finalModel, filtered2[terms]))  # predicted scores
  
  print(c("Pop model applied to pop :",(rmse((filtered2[,1]),(round(predPop))))))

  #predictedMC2Pop <- plogis(predict(trainModel, filtered2[,-which(names(trainingData) %in% c("z","u")),drop=FALSE]))  # predicted scores
  #jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"histpredictedMC2Pop.jpg"), width = 400, height = 400)
  #hist(predictedMC2Pop)
  #dev.off()
  #predictedPop <- plogis(predict(popModel$finalModel, filtered2[,-1,drop=FALSE]))  # predicted scores
  #predictedPop <- plogis(predict(popModel, filtered2[,-1,drop=FALSE]))  # predicted scores
  
  #jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"histpredictedPop.jpg"), width = 400, height = 400)
  #hist(predictedPop)
  #dev.off()
  
  print("CV Model applied to population")
  
  #CV terms applied to population
  print(summary(popModel$finalModel))
  print(summary(filtered2))
  
  #removed medianDirection
  write.csv(filtered2,(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"filtered.csv")))
}
#validate against population    
#population

