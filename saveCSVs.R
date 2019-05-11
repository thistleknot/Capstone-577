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
library(rcompanion)
library(MLmetrics)
#library(sqldf)
library(SparkR)

#based on seeder from cleandatacode.R
set.seed(5)

#https://www.rdocumentation.org/packages/caret/versions/6.0-82/topics/trainControl
library(caret)

#works
threshold=.35
#threshold=.25
#threshold=.25
#threshold=.275
#postProcess=1

train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 1)

#i think this works because files isn't called until AFTER sourceDir is.
files <- c()
files <- list.files(path=paste0(sourceDir,'/output/'), pattern="*final.csv", full.names=TRUE, recursive=FALSE)

ynames <- c()

linux=0
if(linux)
{
  sourceDir="/home/rstudio/577/Capstone-577/"
  
  ys <- c() 
  ys <- list.files(path=paste0(sourceDir,'/output/'), pattern="V*final.csv", full.names=TRUE, recursive=FALSE)
  
  for (i in 1:length(files))
  {
    temp <- c()
    yname <- c()
    print(stringr::str_remove(ys[i],paste0(sourceDir,"/output/")))
    temp <- stringr::str_remove(ys[i],paste0(sourceDir,"/output//"))
    yname <- substr(temp, 0, 5)
    ynames <- rbind(ynames,yname)
  }
  #ynames
}

if(!linux)
{
  sourceDir="C:/Users/user/Documents/School/CSUF/ISDS577/projects/Capstone-577/"
  
  ys <- c() 
  ys <- list.files(path=paste0(sourceDir,'/output/'), pattern="V*final.csv", full.names=TRUE, recursive=FALSE)
  for (i in 1:length(files))
  {
    temp <- c()
    yname <- c()
    print(stringr::str_remove(ys[i],paste0(sourceDir,"/output/")))
    temp <- stringr::str_remove(ys[i],paste0(sourceDir,"/output/"))
    yname <- substr(temp, 0, 5)
    ynames <- rbind(ynames,yname)
  }
  #ynames
}
ynames
source(paste0(sourceDir,"unbalanced_functions.R"))
source(paste0(sourceDir,"sub_returnCVNames.R"))
if(!exists("NewDF"))
{
  source(paste0(sourceDir,"vars.R"))  
  source(paste0(sourceDir,"NewDF.R"))  
}

ynames
#for some reason I have to reinit here after NewDF.R
files <- c()
files <- list.files(path=paste0(sourceDir,'/output/'), pattern="*final.csv", full.names=TRUE, recursive=FALSE)

#set.seed(100)  # for repeatability of samples
yname <- c()
#postProcess=2
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
  #dev.off()
  keepers <- c()
  
  #what a pain
  #hist(tabulatedCrossValidated)
  keepers <- as.character(keepersPre$tabulatedCrossValidated[keepersPre$Freq > (threshold)])
  print(c("keep: > ",threshold,length(keepers),keepers))
  
  #colnames(NewDF)
  filtered <- c()
  filtered <- NewDF[,c(yname,keepers), drop=FALSE]
  filtered[filtered == 0] <- NA
  temp <- c()
  temp <- filtered[] %>% filter_all(all_vars(!is.na(.)))
  filtered <- temp
  filtered[filtered == -1] <- 0    
  
  input_ones <- c()
  input_zeros <- c()
  #class balance
  #colnames(filtered)
  #summary(filtered[,"V7221"])
  input_ones <- filtered[which(filtered[1] == 1), ]  # all 1's
  input_zeros <- filtered[which(filtered[1] == 0), ]  # all 0's
  length(input_ones[,1])
  length(input_zeros[,1])
  
  #MC resample  
  #training_ones <- c()
  #training_zeros <- c()
  #bootstraping with montecarlo
  #resamples 5% from both classes which are already pre-cleaned to aggregate up to 20 samples of 5% each between 2 classes is 100% for those 2 classes.
  
  avgCountHalved <- c()
  avgCountHalved <- round(mean(length(input_ones[,1]),length(input_zeros[,1]))/2)
  
  trainingData <- c()
  ones.index <- c()
  zeros.index <- c()
  
  reloopFactor <- c()
  minFactor <- c()
  
  minFactor <- min(round(.25*length(input_ones[,1])),round(.25*length(input_zeros[,1])))
  reloopFactor <- min(round(.25*length(input_ones[,1])),round(.25*length(input_zeros[,1])))/round(.25*avgCountHalved)
  remainder <- c()
  remainder = reloopFactor-floor(reloopFactor)  
  #i=1
  #this might break depending on the size of 1's or 0's, but I hope 
  for(i in 1:4)
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
    #summary(both)
    #https://stackoverflow.com/questions/2370515/how-to-get-row-index-number-in-r
    mix <- c()
    mix <- sample(c(rownames(both)),round(nrow(both)/2) )
    
    #mix <- sample(both,length(both)/2)
    #colnames(mix) <- colnames(trainingData)
    trainingData <- rbind(trainingData, both[mix,])
    
  }  
  #not reduced column data
  finalTraining <- c()
  finalTrainingI <- c()
  size <- c()
  size <- round(nrow(trainingData))
  finalTrainingI <- sample(c(rownames(trainingData)),size/4)
  finalTraining <- trainingData[finalTrainingI,]
  print(c("MC n:",nrow(finalTraining)))
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
  #trainModelglm <- c()
  #trainModel <- B$BestModel
  
  #trainModelglm <- glm(cbind(filtered2[-1],as.factor(filtered2[,1])),family=binomial(link="logit"))
  #finalTraining[holderOfDataI,][c(terms,yname)]
  
  #reduced column data
  trainingData <- c()
  trainingData <- finalTraining[holderOfDataI,][c(yname,terms)]
  #train(x,y)
  #trainModel$finalModel
  trainModel <- train(x=trainingData[-1], y=as.factor(trainingData[,1]),method = "glm",trControl = train.control)
  
  #any column
  #https://stackoverflow.com/questions/46285484/if-any-column-in-a-row-meets-condition-than-mutate-column
  #df$c[apply(df == 7, 1, any)] <- 100
  
  #includes proportion of variance
  summary(prcomp(x, center=TRUE, scale=TRUE))
  pca <- c()
  pca <- summary(prcomp(x, center=TRUE, scale=TRUE))$importance
  
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
  
  #print(nagelkerke(trainModelglm))
  
  #res <- cor(data.train)
  res <- c()
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
  
  MCPredicted <- c()
  MCPredicted <- plogis(predict(trainModel$finalModel, trainingData[-1]))  # predicted scores
  
  #https://stats.stackexchange.com/questions/121087/count-the-number-of-each-unique-row-in-a-data-frame
  uniqueTrainingXs <- c()
  uniqueTrainingXs <- plyr::count(trainingData[-1], vars = colnames(trainingData[-1]))
  
  uniqueTrainingXs$freq <- round(uniqueTrainingXs$freq / nrow(trainingData[-1]),4)
  
  uniqueTrainingXs$pred <- round(plogis(predict(trainModel$finalModel, uniqueTrainingXs[1:(ncol(uniqueTrainingXs)-1)])),4)  # predicted scores
  
  write.csv(uniqueTrainingXs,(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"uniqueTrainingXs.csv")))
  
  #https://discuss.analyticsvidhya.com/t/how-to-count-number-of-distinct-values-in-a-column-of-a-data-table-in-r/1124/2
  #sqldf("select count(distinct(x)) from df1")
  
  #distinct(trainingData[-1])
  
  #https://www.dummies.com/programming/r/how-to-count-unique-data-values-in-r/
  #sapply(mtcars, function(x) length(unique(x)))
  
  #n_distinct(trainingData[-1])
  
  #uniqueTrainingXs <- c()
  #uniqueTrainingXs <- distinct(trainingData[-1])
  
  #https://stats.stackexchange.com/questions/121087/count-the-number-of-each-unique-row-in-a-data-frame
  
  
  sizePredicted <- c()
  sizePredicted <- 1:length(MCPredicted)
  print(length(MCPredicted))
  print(nrow(trainingData[-1]))
  #summary(predicted)
  
  #class 1
  #Maximize sensitivity given a minimal value of specificity
  #cp_sens <- cutpointr(cbind(yhat,ytest), yhat, ytest, method = maximize_metric, metric = sens_constrain)
  
  yhat <- c()
  yhat <- MCPredicted
  ytest <- data.frame(trainingData[1])
  colnames(ytest) <- "ytest"
  
  #https://www.rdocumentation.org/packages/InformationValue/versions/1.2.3/topics/optimalCutoff
  optCutOff_sens <- c()
  optCutOff_sens <- optimalCutoff(ytest, optimiseFor="Ones", MCPredicted)
  optCutOff_top <- c()
  optCutOff_top <- .99
  optCutOff_center <- c()
  optCutOff_center <- optimalCutoff(ytest, optimiseFor="Both", MCPredicted)
  optCutOff_cen <- c()
  optCutOff_cen <- .5
  optCutOff_spec <- c()
  optCutOff_spec <- optimalCutoff(ytest, optimiseFor="Zeros", MCPredicted)
  
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
  yhat <- c()
  
  #plot(yhat,ytest)
  
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
  
  #different thresholds, same ytest which is MC y
  
  #http://ethen8181.github.io/machine-learning/unbalanced/unbalanced.html
  cm_info_ce <- c()
  cm_info_ce <- ConfusionMatrixInfo( data = cbind(data.frame(yhat.transformed_center)[,,drop=FALSE],data.frame(trainingData[1][,,drop=FALSE])), predict = "yhat.transformed_center", actual = colnames(data.frame(trainingData[1][,,drop=FALSE])), cutoff = optCutOff_center )
  
  cm_info_se <- c()
  cm_info_se <- ConfusionMatrixInfo( data = cbind(data.frame(yhat.transformed_sens)[,,drop=FALSE],data.frame(trainingData[1][,,drop=FALSE])), predict = "yhat.transformed_sens", actual = colnames(data.frame(trainingData[1][,,drop=FALSE])), cutoff = optCutOff_sens )
  
  cm_info_top <- c()
  cm_info_se <- ConfusionMatrixInfo( data = cbind(data.frame(yhat.transformed_top)[,,drop=FALSE],data.frame(trainingData[1][,,drop=FALSE])), predict = "yhat.transformed_top", actual = colnames(data.frame(trainingData[1][,,drop=FALSE])), cutoff = optCutOff_top )
  
  cm_info_cen <- c()
  cm_info_cen <- ConfusionMatrixInfo( data = cbind(data.frame(yhat.transformed_cen)[,,drop=FALSE],data.frame(trainingData[1][,,drop=FALSE])), predict = "yhat.transformed_cen", actual = colnames(data.frame(trainingData[1][,,drop=FALSE])), cutoff = optCutOff_cen )
  
  cm_info_sp <- c()
  cm_info_sp <- ConfusionMatrixInfo( data = cbind(data.frame(yhat.transformed_spec)[,,drop=FALSE],data.frame(trainingData[1][,,drop=FALSE])), predict = "yhat.transformed_spec", actual = colnames(data.frame(trainingData[1][,,drop=FALSE])), cutoff = optCutOff_spec )
  #print(cm_info$data[order(cm_info$data$predict),])
  
  total_predictions = nrow(data.frame(yhat.transformed_sens))
  correct_predictions = sum(yhat.transformed_sens == data.frame(trainingData[1][,,drop=FALSE]))
  classification_accuracy = correct_predictions / total_predictions
  error_rate_se = (1 - (correct_predictions / total_predictions))
  
  total_predictions = nrow(data.frame(yhat.transformed_top))
  correct_predictions = sum(yhat.transformed_top == data.frame(trainingData[1][,,drop=FALSE]))
  classification_accuracy = correct_predictions / total_predictions
  error_rate_top = (1 - (correct_predictions / total_predictions))
  
  total_predictions = nrow(data.frame(yhat.transformed_cen))
  correct_predictions = sum(yhat.transformed_cen == data.frame(trainingData[1][,,drop=FALSE]))
  classification_accuracy = correct_predictions / total_predictions
  error_rate_cen = (1 - (correct_predictions / total_predictions))
  
  total_predictions = nrow(data.frame(yhat.transformed_center))
  correct_predictions = sum(yhat.transformed_center == data.frame(trainingData[1][,,drop=FALSE]))
  classification_accuracy = correct_predictions / total_predictions
  error_rate_ce = (1 - (correct_predictions / total_predictions))
  
  total_predictions = nrow(data.frame(yhat.transformed_spec))
  correct_predictions = sum(yhat.transformed_spec == data.frame(trainingData[1][,,drop=FALSE]))
  classification_accuracy = correct_predictions / total_predictions
  error_rate_sp = (1 - (correct_predictions / total_predictions))
  
  #https://machinelearningmastery.com/confusion-matrix-machine-learning/
  #https://www.rdocumentation.org/packages/caret/versions/3.45/topics/confusionMatrix
  #https://www.datacamp.com/community/tutorials/confusion-matrix-calculation-r
  #https://rdrr.io/cran/caret/man/confusionMatrix.html
  
  #class 1
  results_se <- c()
  results_se <- confusionMatrix(yhat.transformed_sens, data.frame(trainingData[1][,,drop=FALSE]))
  
  results_ce <- c()
  results_ce <- confusionMatrix(yhat.transformed_center, data.frame(trainingData[1][,,drop=FALSE]))
  
  #class 0
  results_sp <- c()
  results_sp <- confusionMatrix(yhat.transformed_spec, data.frame(trainingData[1][,,drop=FALSE]))
  
  results_top <- c()
  results_top <- confusionMatrix(yhat.transformed_top, data.frame(trainingData[1][,,drop=FALSE]))
  
  results_cen <- c()
  results_cen <- confusionMatrix(yhat.transformed_cen, data.frame(trainingData[1][,,drop=FALSE]))
  
  print(c("optCutOff_sens:",round(optCutOff_sens,4)))
  hist(yhat.transformed_sens)
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"cm_info_se.jpg"), width = 400, height = 800)
  plot(cm_info_se$plot)
  dev.off()
  print(paste("error rate sens:",round(error_rate_se,4)))
  print("yhat.transformed_sens matrix")
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
  
  print(c("MC model applied to MC:",(round(rmse((trainingData[,1]),round(yhat.transformed_cen)),4))))
  
  total_predictions = nrow(trainingData)
  correct_predictions = sum(trainingData[1] == round(yhat.transformed_cen))
  classification_accuracy = correct_predictions / total_predictions
  error_rate = (1 - (correct_predictions / total_predictions))
  print(c("error: ",round(error_rate,4)))
  
  #summary(trainModel)
  #table(yhat)
  #ytest = trainingData[,1]
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"MCModResiduals.jpg"), width = 400, height = 400)
  #diff <- c()
  #diff <- ytest-yhat
  hist(trainModel$finalModel$residuals)
  dev.off()
  
  pred <- c()
  pred <- prediction(MCPredicted,trainingData[1])
  #nrow(yhat)
  #nrow(ytest)
  roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"rocMCappMC.jpg"), width = 400, height = 400)
  plot(roc.perf)
  abline(a=0, b= 1)
  dev.off()
  
  gain <- c()
  gain <- performance(pred, "tpr", "rpp")
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"gainMCappMC.jpg"), width = 400, height = 400)
  plot(gain, main = "Gain Chart")
  abline(a=0, b= 1)
  dev.off()
  
  print("Conf matrix: MC CV (robust) model applied to MC data")
  CFMCCVMCData <- c()
  
  CFMCCVMCData <- confusionMatrix(round(yhat.transformed_cen), trainingData[,1,drop=TRUE])
  print(nrow(trainingData[1]))
  print(round(CFMCCVMCData/sum(CFMCCVMCData),4))
  
  popData <- c()
  popData <- NewDF[,as.character(c(yname,terms)), drop=FALSE]
  popData[popData == 0] <- NA
  temp <- c()
  temp <- popData[] %>% filter_all(all_vars(!is.na(.)))
  popData <- temp
  popData[popData == -1] <- 0    
  
  print(c("pop Data n:",nrow(popData)))
  
  print(summary(popData))
  
  colnames(trainingData)
  #pop model applied to pop
  
  #popModel <- suppressMessages(train(popData[-1], as.factor(popData[,1]),method = "glm",trControl = train.control))
  popModel <- c()
  popModel <- train(popData[-1], as.factor(popData[,1]),method = "glm",trControl = train.control)
  
  print("Pop Model Summary")
  print(summary(popModel$finalModel))
  
  #terms applied to pop
  predPopModel <- c()
  predPopModel <- plogis(predict(popModel$finalModel, popData[-1]))  # predicted scores
  print(length(predPopModel))
  print(nrow(popData[-1]))
  
  uniquePopXs <- c()
  uniquePopXs <- plyr::count(popData[-1], vars = colnames(popData[-1]))
  
  uniquePopXs$freq <- round(uniquePopXs$freq / nrow(popData[-1]),4)
  
  uniquePopXs$pred <- round(plogis(predict(popModel$finalModel, uniquePopXs[1:(ncol(uniquePopXs)-1)])),4)  # predicted scores
  
  write.csv(uniquePopXs,(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"uniquePopXs.csv")))
  
  #popModel2 <- glm(cbind(popData[-1], as.factor(popData[,1])),family=binomial(link="logit"))
  ytest <- c()
  ytest <- data.frame(popData[1])
  colnames(ytest) <- "ytest"
  
  #https://www.rdocumentation.org/packages/InformationValue/versions/1.2.3/topics/optimalCutoff
  optCutOff_sens <- c()
  optCutOff_sens <- optimalCutoff(ytest, optimiseFor="Ones", predPopModel)
  optCutOff_top <- c()
  optCutOff_top <- .99
  optCutOff_center <- c()
  optCutOff_center <- optimalCutoff(ytest, optimiseFor="Both", predPopModel)
  optCutOff_cen <- c()
  optCutOff_cen <- .5
  optCutOff_spec <- c()
  optCutOff_spec <- optimalCutoff(ytest, optimiseFor="Zeros", predPopModel)
  
  sizePredicted <- c()
  sizePredicted <- 1:length(predPopModel)
  
  indexLess_sens <- c()
  indexLess_sens <- rownames(data.frame(predPopModel[as.numeric(predPopModel) < optCutOff_sens]))
  
  indexMore_sens <- c()
  indexMore_sens <- sizePredicted[!sizePredicted %in% indexLess_sens]
  
  indexLess_center <- c()
  indexLess_center <- rownames(data.frame(predPopModel[as.numeric(predPopModel) < optCutOff_center]))
  
  indexMore_center <- c()
  indexMore_center <- sizePredicted[!sizePredicted %in% indexLess_center]
  
  predPopModel_center1 <- c()
  predPopModel_center1 <- predPopModel
  predPopModel_center1[noquote(indexMore_center)] <- 1
  predPopModel_center1[noquote(indexLess_center)] <- 0
  
  indexLess_center2 <- c()
  indexLess_center2 <- rownames(data.frame(predPopModel[as.numeric(predPopModel) < .5]))
  
  indexMore_center2 <- c()
  indexMore_center2 <- sizePredicted[!sizePredicted %in% indexLess_center2]
  
  indexLess_spec <- c()
  indexLess_spec <- rownames(data.frame(predPopModel[as.numeric(predPopModel) < optCutOff_spec]))
  
  indexMore_spec <- c()
  indexMore_spec <- sizePredicted[!sizePredicted %in% indexLess_spec]
  
  indexLess_opt_ctr <- c()
  indexLess_opt_ctr <- rownames(data.frame(predPopModel[as.numeric(predPopModel) < optCutOff_center]))
  
  indexMore_opt_ctr <- c()
  indexMore_opt_ctr <- sizePredicted[!sizePredicted %in% indexLess_opt_ctr]
  
  #summary(popModel)
  #print(nagelkerke(popModel2, popData))
  
  print(c("Pop model optimal cutoff for center & RMSE:",round(optCutOff_center,4),(round(rmse((popData[,1]),predPopModel_center1),4))))
  
  print("Conf Matrix: Pop model applied to Pop data Opt Ctr")
  CM_PopOptCtr <- c()
  CM_PopOptCtr <- confusionMatrix(predPopModel_center1, popData[1])
  print(nrow(popData[1]))
  print(round(CM_PopOptCtr/sum(CM_PopOptCtr),4))  
  
  total_predictions = nrow(popData)
  correct_predictions = sum(popData[1] == predPopModel_center1)
  classification_accuracy = correct_predictions / total_predictions
  error_rate = (1 - (correct_predictions / total_predictions))
  print(c("error: ",round(error_rate,4)))
  
  print(c("Pop model optimal cutoff for sens (as-is, over-fitted):",.5,(round(rmse((popData[,1]),round(predPopModel)),4))))
  
  print("Conf Matrix: Pop model applied to Pop data Opt Sens")
  CM_PopOptCtr <- c()
  CM_PopOptCtr <- confusionMatrix(round(predPopModel), popData[1])
  print(nrow(popData[1]))
  print(round(CM_PopOptCtr/sum(CM_PopOptCtr),4))  
  
  total_predictions = nrow(popData)
  correct_predictions = sum(popData[1] == round(predPopModel))
  classification_accuracy = correct_predictions / total_predictions
  error_rate = (1 - (correct_predictions / total_predictions))
  print(c("error: ",round(error_rate,4)))
  
  #summary(popModel)
  #print(nagelkerke(popModel2, popData))
  
  predPopModel_center2 <- c()
  predPopModel_center2 <- predPopModel
  predPopModel_center2[indexMore_center2] <- 1
  predPopModel_center2[indexLess_center2] <- 0
  
  print(c("Pop model optimal cutoff for center2 & RMSE:",.5,(round(rmse((popData[,1]),predPopModel_center2),4))))
  
  total_predictions = nrow(popData)
  correct_predictions = sum(popData[1] == predPopModel_center2)
  classification_accuracy = correct_predictions / total_predictions
  error_rate = (1 - (correct_predictions / total_predictions))
  print(c("error: ",round(error_rate,4)))
  
  predPopModel_sens <- c()
  predPopModel_sens <- predPopModel
  predPopModel_sens[indexMore_sens] <- 1
  predPopModel_sens[indexLess_sens] <- 0
  
  #summary(popModel)
  #print(nagelkerke(popModel2, popData))
  
  print(c("Pop model optimal cutoff for sens & RMSE:",round(optCutOff_sens,4),(round(rmse((popData[,1]),predPopModel_sens),4))))
  
  total_predictions = nrow(popData)
  correct_predictions = sum(popData[1] == predPopModel_sens)
  classification_accuracy = correct_predictions / total_predictions
  error_rate = (1 - (correct_predictions / total_predictions))
  print(c("error: ",round(error_rate,4)))
  
  predPopModel_spec <- c()
  predPopModel_spec <- predPopModel
  predPopModel_spec[indexMore_spec] <- 1
  predPopModel_spec[indexLess_spec] <- 0
  
  #summary(popModel)
  #print(nagelkerke(popModel2, popData))
  
  print(c("Pop model optimal cutoff for spec & RMSE:",round(optCutOff_spec,4),(round(rmse((popData[,1]),predPopModel_spec),4))))
  #print(c("MAPE spec: ", MAPE(predPopModel,popData[,1])))
  #print(c("RMSE spec: ", round(sqrt(sum((predPopModel_spec-popData[,1])^2)/nrow(popData)),4)))
  
  total_predictions = nrow(popData)
  correct_predictions = sum(popData[1] == predPopModel_spec)
  classification_accuracy = correct_predictions / total_predictions
  error_rate = (1 - (correct_predictions / total_predictions))
  print(c("error: ",round(error_rate,4)))
  #yhat = predict(trainModel, PostDF[,-1,drop=FALSE])
  
  print(c("Pop model optimal cutoff for opt ctr"))
  
  predPopModel_opt_ctr <- c()
  predPopModel_opt_ctr <- predPopModel
  predPopModel_opt_ctr[indexMore_opt_ctr] <- 1
  predPopModel_opt_ctr[indexLess_opt_ctr] <- 0
  
  #ytest <- c()
  #ytest <- popData[1]
  #summary(trainModel)
  #table(yhat)
  #ytest = trainingData[,1]
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"DeltaMCPopLogis.jpg"), width = 400, height = 400)
  #diff <- c()
  #diff <- ytest-yhat
  logisdeltas <- c()
  logisdeltas <- predPopModel-MCPredicted
  hist(logisdeltas)
  hist(popModel$finalModel$residuals)
  dev.off()
  
  pred<- c()
  pred <- prediction(predPopModel,popData[1])
  #nrow(yhat)
  #nrow(ytest)
  roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"rocPopappPopOptCenter.jpg"), width = 400, height = 400)
  plot(roc.perf)
  abline(a=0, b= 1)
  dev.off()
  
  gain <- c()
  gain <- performance(pred, "tpr", "rpp")
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"gainPopappPopOptCenter.jpg"), width = 400, height = 400)
  plot(gain, main = "Gain Chart")
  abline(a=0, b= 1)
  dev.off()
  
  #hist(predMCPop-predPopModel)
  #hist(predMCPop-predPopModel)
  
  #CV Model
  #print("CV Model")
  #print(summary(B$BestModel))
  
  #yhat = predict(trainModel, PostDF[,-1,drop=FALSE])
  predMCPop <- c()
  
  #Derive predicted scores using MC Model on pop data
  predMCPop <- plogis(predict(trainModel$finalModel, popData[-1]))  # predicted scores
  print(length(predMCPop))
  print(nrow(popData[-1]))
  #summary(predicted)
  
  #summary(popData)
  #summary(predicted)
  indexLessMCModPopData_center2 <- c()
  indexLessMCModPopData_center2 <- rownames(data.frame(predMCPop[as.numeric(predMCPop) < .5]))
  
  indexLessMCModPopData_sens <- c()
  indexLessMCModPopData_sens <- rownames(data.frame(predMCPop[as.numeric(predMCPop) < optCutOff_sens]))
  
  
  #summary(popData)
  #summary(predicted)
  #indexLessMCModPopData_sens <- c()
  #indexLessMCModPopData_sens <- rownames(data.frame(predMCPop[as.numeric(predMCPop) < .5]))
  
  sizePredicted <- c()
  sizePredicted <- 1:length(predMCPop)
  
  indexMoreMCModPopData_center2 <- c()
  indexMoreMCModPopData_center2 <- sizePredicted[!sizePredicted %in% indexLessMCModPopData_center2]
  
  indexMoreMCModPopData_sens <- c()
  indexMoreMCModPopData_sens <- sizePredicted[!sizePredicted %in% indexLessMCModPopData_sens]
  
  predMCPop_center2 <- c()
  predMCPop_center2 <- predMCPop
  predMCPop_center2[indexMoreMCModPopData_center2] <- 1
  predMCPop_center2[indexLessMCModPopData_center2] <- 0
  
  predMCPop_sens <- c()
  predMCPop_sens <- predMCPop
  predMCPop_sens[indexMoreMCModPopData_sens] <- 1
  predMCPop_sens[indexLessMCModPopData_sens] <- 0
  
  print(c("MC .5 ctr model applied to pop:",(round(rmse((popData[,1]),predMCPop_center2),4))))
  
  total_predictions = nrow(popData)
  correct_predictions = sum( popData[1] == predMCPop_center2)
  classification_accuracy = correct_predictions / total_predictions
  error_rate = (1 - (correct_predictions / total_predictions))
  print(c("error center2: ",round(error_rate,4)))
  
  total_predictions = nrow(popData)
  correct_predictions = sum( popData[1] == predMCPop_sens)
  classification_accuracy = correct_predictions / total_predictions
  error_rate = (1 - (correct_predictions / total_predictions))
  print(c("error sens: ",round(error_rate,4)))
  
  print("Conf Matrix: MC model applied to Pop data Center2")
  MCModPop <- c()
  
  #length(round(predMCPop_center2))
  #nrow(popData[1])
  MCModPop <- confusionMatrix(round(predMCPop_center2), popData[1])
  print(nrow(popData[1]))
  print(round(MCModPop/sum(MCModPop),4))
  
  print("Conf Matrix: MC model applied to Pop data Sens")
  print(c("MC sens model applied to pop:",round(optCutOff_sens,4),(round(rmse((popData[,1]),predMCPop_sens),4))))
  MCModPop <- c()
  
  #length(round(predMCPop_center2))
  #nrow(popData[1])
  MCModPop <- confusionMatrix(round(predMCPop_sens), popData[1])
  print(nrow(popData[1]))
  print(round(MCModPop/sum(MCModPop),4))
  
  #summary(trainModel)
  #table(yhat)
  #ytest = trainingData[,1]
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"MCdiffPopBoxPlot.jpg"), width = 400, height = 400)
  #diff <- c()
  #diff <- ytest-yhat
  boxplot((predMCPop_center2-popData[1]))
  dev.off()
  
  #summary(trainModel)
  #table(yhat)
  #ytest = trainingData[,1]
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"PopdiffPopBoxPlot.jpg"), width = 400, height = 400)
  #diff <- c()
  #diff <- ytest-yhat
  boxplot((predPopModel-popData[1]))
  dev.off()
  
  pred <- c()
  pred <- prediction(predMCPop,popData[1])
  #nrow(yhat)
  #nrow(ytest)
  roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"rocMCappPop.jpg"), width = 400, height = 400)
  plot(roc.perf)
  abline(a=0, b= 1)
  dev.off()
  
  gain <- c()
  gain <- performance(pred, "tpr", "rpp")
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"gainMCappPop.jpg"), width = 400, height = 400)
  plot(gain, main = "Gain Chart")
  abline(a=0, b= 1)
  dev.off()
  
  #predictedMC2Pop <- plogis(predict(trainModel, popData[,-which(names(trainingData) %in% c("z","u")),drop=FALSE]))  # predicted scores
  #jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"histpredictedMC2Pop.jpg"), width = 400, height = 400)
  #hist(predictedMC2Pop)
  #dev.off()
  #predictedPop <- plogis(predict(popModel$finalModel, popData[,-1,drop=FALSE]))  # predicted scores
  #predictedPop <- plogis(predict(popModel, popData[,-1,drop=FALSE]))  # predicted scores
  
  #jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"histpredictedPop.jpg"), width = 400, height = 400)
  #hist(predictedPop)
  #dev.off()
  
  #print("Summary: CV Model applied to population")
  
  #CV terms applied to population
  #print(summary(trainModel$finalModel))
  
  #removed medianDirection
  write.csv(popData,(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"filtered.csv")))
}
#validate against population    
#population

