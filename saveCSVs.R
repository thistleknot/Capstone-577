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
#threshold=.33
#postProcess=1

train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
#sourceDir="/home/rstudio/577/Capstone-577/"
sourceDir="C:/Users/user/Documents/School/CSUF/ISDS577/projects/Capstone-577/"
source(paste0(sourceDir,"unbalanced_functions.R"))

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
  trainingData <- c()
  #i=1
  #this might break depending on the size of 1's or 0's, but I hope 
  for(i in 1:10)
  {
    zerosA.index <- c()
    zerosB.index <- c()
        
    onesA.index <- c()
    onesB.index <- c()
    
    ones.index <- sample(1:nrow(input_ones), round(0.05*nrow(input_ones)))

    ones <- c()
    ones <- input_ones[ones.index,]
    
    zeros.index <- sample(1:nrow(input_zeros), round(0.05*nrow(input_ones)))

    zeros <- c()
    zeros <- input_zeros[zeros.index,]
    
    #training_ones <- rbind(training_ones,ones[input_ones_training_rows, ])
    #training_zeros <- rbind(training_zeros,input_zeros[input_zeros_training_rows, ])
  
    both <- c()
    both <- rbind(ones, zeros)   
    
    trainingData <- rbind(trainingData,both)
  }
  
  x <- c()
  y <- c()
  y <- trainingData[,1,drop=FALSE]
  x <- trainingData[,-1,drop=FALSE]

  holderOfData <- c()
  holderOfData <- cbind(x,y)
  B <- suppressMessages(bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=5, REP=1, TopModels=10, BestModels = 10), family=binomial,method = "exhaustive"))
  
  terms <- B$BestModel$coefficients[-1]
  print(terms)
  names2 <- c()
  names2 <- row.names(data.frame(terms))
  
  
  print(summary(trainingData))
  #print(summary(training_zeros))
  
  trainModel <- c()
  trainModel <- B$BestModel
  holderOfData <- c()
  holderOfData <- cbind(trainingData[-1,],trainingData[1,])
  #trainModel <- glm(holderOfData,family=binomial(link="logit"))
  
  #any column
  #https://stackoverflow.com/questions/46285484/if-any-column-in-a-row-meets-condition-than-mutate-column
  #df$c[apply(df == 7, 1, any)] <- 100
  
  #includes proportion of variance
  summary(prcomp(x, center=TRUE, scale=TRUE))
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
  print(summary(trainModel))
  
  #res <- cor(data.train)
  res <- cor(trainingData)
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"corrplot.jpg"), width = 400, height = 400)
  corrplot(res)
  dev.off()
  
  x= c()
  y= c()
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
  predicted <- plogis(predict(trainModel, trainingData[-1]))  # predicted scores
  #summary(predicted)
  
  yhat <- round(predicted)
  ytest <- trainingData[1]
  #summary(trainModel)
  #table(yhat)
  #ytest = trainingData[,1]
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"MCytestdiffyhat.jpg"), width = 400, height = 400)
  #diff <- c()
  #diff <- ytest-yhat
  hist(trainModel$residuals)
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
  optCutOff_center <- optimalCutoff(ytest, optimiseFor="Both", yhat)
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
  cm_info_ce <- ConfusionMatrixInfo( data = cbind(ytemp,ytest), predict = "yhat", actual = "ytest", cutoff = optCutOff_center )
  cm_info_se <- ConfusionMatrixInfo( data = cbind(ytemp,ytest), predict = "yhat", actual = "ytest", cutoff = optCutOff_sens )
  cm_info_sp <- ConfusionMatrixInfo( data = cbind(ytemp,ytest), predict = "yhat", actual = "ytest", cutoff = optCutOff_spec )
  #print(cm_info$data[order(cm_info$data$predict),])

  total_predictions = nrow(data.frame(yhat.transformed_sens))
  correct_predictions = sum(yhat.transformed_sens == ytest)
  classification_accuracy = correct_predictions / total_predictions
  error_rate_se = (1 - (correct_predictions / total_predictions))

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
  results <- c()
  results_se <- confusionMatrix(yhat.transformed_sens, ytest[,1])
  
  results <- c()
  results_ce <- confusionMatrix(yhat.transformed_center, ytest[,1])
 
  #class 0
  results <- c()
  results_sp <- confusionMatrix(yhat.transformed_spec, ytest[,1])
  
  print(c("optCutOff_sens:",round(optCutOff_sens,4)))
  hist(yhat.transformed_sens)
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"cm_info_se.jpg"), width = 400, height = 800)
  plot(cm_info_se$plot)
  dev.off()
  print(paste("error rate sens:",round(error_rate_se,4)))
  print("yhat.transformed_center sens matrix")
  print(c("n:",sum(results_se)))
  print(round(results_se/sum(results_se),4))
  
  print(c("optCutOff_center",round(optCutOff_center,4)))
  hist(yhat.transformed_center)
  #http://www.sthda.com/english/wiki/creating-and-saving-graphs-r-base-graphs
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"cm_info_ce.jpg"), width = 400, height = 800)
  plot(cm_info_ce$plot)
  dev.off()
  
  plot(cm_info_ce$plot)
  print(paste("error rate c:",round(error_rate_ce,4)))
  print("yhat.transformed_center conf matrix")
  print(c("n:",sum(results_ce)))
  print(round(results_ce/sum(results_ce),4))
  
  print(c("optCutOff_spec",round(optCutOff_spec,4)))
  hist(yhat.transformed_spec)
  jpeg(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"cm_info_sp.jpg"), width = 400, height = 800)
  plot(cm_info_sp$plot)
  dev.off()
  print(paste("error rate spec:",round(error_rate_ce,4)))
  print("yhat.transformed_spec conf matrix")
  print(c("n:",sum(results_sp)))
  print(round(results_sp/sum(results_sp),4))
  
  #converts to logit
  #apply new terms to class balanaced mc data
  tempNew <- c()
  tempNew <- trainingData[names2]
  
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
  filtered2 <- NewDF[,as.character(c(yname,names2)), drop=FALSE]
  filtered2[filtered2 == 0] <- NA
  temp <- filtered2[] %>% filter_all(all_vars(!is.na(.)))
  filtered2 <- temp
  filtered2[filtered2 == -1] <- 0    
  
  colnames(trainingData)
  
  predMCPop <- plogis(predict(B$BestModel, filtered2[names2]))  # predicted scores
  
  print(c("MC model applied to Pop :",(rmse((filtered2[,1]),(round(predMCPop))))))
  
  #popModel <- suppressMessages(train(filtered2[-1], as.factor(filtered2[,1]),method = "glm",trControl = train.control))
  popModel <- glm(holderOfData,family=binomial(link="logit"))
  
  predPop <- plogis(predict(popModel, filtered2[names2]))  # predicted scores
  
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
  print(summary(popModel))
  
  #removed medianDirection
  write.csv(filtered2,(paste0(str_sub(files[postProcess], 1, str_length(files[postProcess])-9),"filtered.csv")))
}
#validate against population    
#population

