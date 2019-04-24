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
#library(ggthemr)
#library(lsplsGlm)

{
  widthDiviser = 2
  
  if (widthDiviser == 1) train.control <- trainControl(method = "repeatedcv", number = 2, repeats = widthDiviser)
  if (!(widthDiviser == 1)) train.control <- trainControl(method = "repeatedcv", number = 2, repeats = widthDiviser)
  
  #set.seed(seedbase)
  #setup holdout
  
  #static holdout
  holdoutSetSize = widthDiviser/100
  #holdoutSetSize = 1.25/100
  
  underOverSampleFactor=1
  
  #% to resample from resampled static hold out set
  holdoutSize = underOverSampleFactor/widthDiviser #(of set) #(never fully iterates over subsample)
  
  #proportion of nonHoldout (i.e. nonholdout: 1-holdoutSize) to use for model building, i.e. sample size.  Holdout can be tuned independently kind of.
  #preNonHoldOutSize = (1.25/100)/(1-holdoutSetSize) #forces it to be 5%, opposite is used for nonholdout
  preNonHoldOutSize = (widthDiviser/100)/(1-holdoutSetSize) #forces it to be 5%, opposite is used for nonholdout
  
  #% of training resamples from static nonholdout
  preTrainSize = underOverSampleFactor/widthDiviser # <1 = (never fully iterates over subsample)
  sourceDir="C:/Users/user/Documents/School/CSUF/ISDS577/projects/Capstone-577/"
  source(paste0(sourceDir,"glm.pcr.R"))
  source(paste0(sourceDir,"unbalanced_functions.R"))
  #sourceDir="/home/rstudio/577/Capstone-577/"
  
  if (widthDiviser == 1) resample = 2
  if ((!widthDiviser == 1)) resample = widthDiviser
  
}

#also doing another pass after this finalList creating a finalListCV
#PCA Analysis, scratch space post analysis, currently need to do classification matrix.  would recommend doing it on samples?  
#Also derive population stuff here
#no need for randomized sets (unless validating, but as long as what is produced is significant each time shown, then the experiment is a success)

#due to way NA's are presented, there is a deviation in the # of records truly presented... but unsure if since na's are represented evenly if this matters or not.
#What I might need to do is remove na's from newDF
files <- list.files(path=paste0(sourceDir,'/output/'), pattern="*.csv", full.names=TRUE, recursive=FALSE)

#postProcess=21
for (postProcess in 1:length(files))
{ 
  
  #NewDF assumes 0's mean NA's, this is more like a population dataframe already precleaned. (i.e. the export of my cleandatacode.R cleans na's)
  PostDF <- read.csv(files[postProcess], header=TRUE, sep=",")[,-1,drop=FALSE]
  
  x <- PostDF[,-1, drop=FALSE]
  
  y <- data.frame(PostDF[,1, drop=FALSE])[,1]
  
  #includes proportion of variance
  summary(prcomp(x, center=TRUE, scale=TRUE))
  te <- summary(prcomp(x, center=TRUE, scale=TRUE))$importance
  #pc plot
  plot(te[3,1:ncol(te)])
  
  #correlation plot of sample along with pca
  corrplot(cor(cbind(x,prcomp(x, center=TRUE, scale=TRUE)$x)))
  
  #include data in new model for inclusion in a linear model
  #https://stats.stackexchange.com/questions/72839/how-to-use-r-prcomp-results-for-prediction
  
  #suppressMessages(pcaModel<- glm(y~pc$x[,1:length(data.frame(pc$x))]))
  
  #predict using pca, just re-applying to training data.
  
  #applied PCA to holdout
  
  #does this make it linear?
  #pc <- prcomp(x, center=TRUE, scale=TRUE)
  #pred <- predict(pc,x)
  #plot(data.frame(y,drop=FALSE),pred)
  #pcaPred <- lm(cbind(data.frame(y),pred))
  
  #yhatPCA = predict(pcaPred, x)
  #colnames(y)<-"y"
  tempy <- data.frame(y)
  colnames(tempy)<-"y"
  #tempy
  
  #http://www.milanor.net/blog/performing-principal-components-regression-pcr-in-r/
  #https://stackoverflow.com/questions/40325165/matrix-multiplication-in-r-requires-numeric-complex-matrix-vector-arguments?rq=1
  #requires Rfast
  #pcr_model <- pcr(y~., data = cbind(tempy,x), scale = TRUE, validation = "CV")
  
  #pcr_model <- glm.pcr(y, x, k=ncol(PostDF[,-1]),xnew = NULL)
  
  #pcr_model <- glm.pcr(y~., data = cbind(tempy,x), scale = TRUE)
  
  #summary(pcr_model)
  
  #pcr_model <- fit.lspcr.glm(Y=Y, x, cbind(x,y), ncol(PostDF), folds = widthSize, proportion = 0.9)
  
  #pcr_pred <- predict.glm(pcr_model, x)
  #hist(pcr_pred)
  #table(y)
  #table(pcr_pred)
  
  #pred <- prediction(data.frame(pcr_pred)[,1],y)
  #roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
  #plot(roc.perf)
  #abline(a=0, b= 1)  
 
  #predict(pcaPred,)
  
  #predict(pcaPred,filteredv7133holdout[-1])
  
  #summary(pcaPred)
  #hist(abs(pcaPred$residuals))
  
  #summary(pcaModel)
  #summary(pcaPred)
  
  trainModel <- suppressMessages(train(PostDF[-1], as.factor(PostDF[,1]),method = "glm",trControl = train.control))
  print("population")
  print(summary(trainModel$finalModel))
  
  finalList <- colnames(PostDF)
  #reseed
  source(paste0(sourceDir,"/reseedPost.R"))
  source(paste0(sourceDir,"/resampleMCpost.R"))
  
  #res <- cor(data.train)
  res <- cor(PostDF)
  corrplot(res)
  
  x= c()
  y= c()
  #yname <- c()
  
  #x=data.train[,-1]
  #x.test=data.test[,-1]
  #y=data.train[,1]
  
  yhat = predict(trainModel$finalModel, PostDF[,-1,drop=FALSE])
  summary(trainModel)
  #table(yhat)
  ytest = PostDF[,1,drop=FALSE][,1]
  hist(yhat)
  hist(ytest)
  #yhat.test = predict(trainModel$finalModel, x.test)
  #ytest = data.test[,1]
  
  #needs to be continuous
  #https://arulvelkumar.wordpress.com/2017/09/03/prediction-function-in-r-number-of-cross-validation-runs-must-be-equal-for-predictions-and-labels/
  #https://hopstat.wordpress.com/2014/12/19/a-small-introduction-to-the-rocr-package/
  #pred <- prediction(ROCR.simple$predictions,ROCR.simple$labels)
  #labels = classification (i.e. true value)
  pred <- prediction(yhat,ytest)
  roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
  plot(roc.perf)
  abline(a=0, b= 1)
  
  gain <- performance(pred, "tpr", "rpp")
  plot(gain, main = "Gain Chart")
  #plot(yhat,ytest)
  
  yhat <- data.frame(yhat)
  colnames(yhat) <- "yhat"
  
  ytest <- data.frame(ytest)
  colnames(ytest) <- "ytest"
  
  #http://ethen8181.github.io/machine-learning/unbalanced/unbalanced.html
  #cm_info <- ConfusionMatrixInfo( data = cbind(yhat,ytest), predict = "yhat", 
                                  #actual = "ytest", cutoff = .5 )
  
  #print(cm_info$data[order(cm_info$data$predict),])
  #cm_info$plot
  
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
  cp_sens <- cutpointr(cbind(yhat,ytest), yhat, ytest, 
                  method = maximize_metric, metric = sens_constrain)
  
  #both classes
  cp_center <- cutpointr(cbind(yhat,ytest), yhat, ytest, 
                       method = maximize_metric, metric = sum_sens_spec)
  
  #class 0
  #Maximize specificity given a minimal value of sensitivity
  cp_spec <- cutpointr(cbind(yhat,ytest), yhat, ytest, 
                   method = maximize_metric, metric = spec_constrain)
  
  
  #ggthemr("flat")
  #cm_info$plot
  
  #pred <- prediction(yhat.test,data.test[,1])
  #roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
  #plot(roc.perf)
  #abline(a=0, b= 1)
  
  #this is manually converting them
  #test classification using best linear model
  
  #this is where an arbitary threshold is set.
  yhat.transformed_sens = rep(0, nrow(PostDF))
  yhat.transformed_sens[yhat >= cp_sens$optimal_cutpoint] = 1
  yhat.transformed_sens[yhat < cp_sens$optimal_cutpoint] = 0

  
  yhat.transformed_center = rep(0, nrow(PostDF))
  yhat.transformed_center[yhat >= cp_center$optimal_cutpoint] = 1
  yhat.transformed_center[yhat < cp_center$optimal_cutpoint] = 0
  
    
  yhat.transformed_spec = rep(0, nrow(PostDF))
  yhat.transformed_spec[yhat >= cp_spec$optimal_cutpoint] = 1
  yhat.transformed_spec[yhat < cp_spec$optimal_cutpoint] = 0

  print(cp_sens$optimal_cutpoint)
  print(cp_center$optimal_cutpoint)
  print(cp_spec$optimal_cutpoint)
  #sum(yhat.transformed)
  
  #typeof(ytest)
  #data.frame(ytest)
  #rmse(ytest, yhat.transformed)
  
  #Calculating MSE for training data
  #mse.train<- mean(residuals(step.model.train)^2)
  #mse.train
  
  #Calculating MSE for testing data
  #mse.test <- mean(residuals(step.model.test)^2)
  #mse.test
  
  total_predictions = nrow(data.frame(yhat.transformed))
  correct_predictions = sum(yhat.transformed == ytest)
  classification_accuracy = correct_predictions / total_predictions
  error_rate = (1 - (correct_predictions / total_predictions))
  print(paste("error rate:",round(error_rate,3)))
  
  #https://machinelearningmastery.com/confusion-matrix-machine-learning/
  #https://www.rdocumentation.org/packages/caret/versions/3.45/topics/confusionMatrix
  #https://www.datacamp.com/community/tutorials/confusion-matrix-calculation-r
  #https://rdrr.io/cran/caret/man/confusionMatrix.html
  
  
  #class 1
  results <- confusionMatrix(data=as.factor(yhat.transformed_sens), reference=as.factor(ytest[,1]))
  print(results)
  
  results <- confusionMatrix(data=as.factor(yhat.transformed_center), reference=as.factor(ytest[,1]))
  print(results)
  
  #class 0
  results <- confusionMatrix(data=as.factor(yhat.transformed_spec), reference=as.factor(ytest[,1]))
  print(results)
  
  
}

