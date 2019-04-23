library(ModelMetrics)
library("ROCR")
library("caret")
library(corrplot)
library(bestglm)
library(outliers)
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
  
  #correlation plot of sample
  corrplot(cor(cbind(x,prcomp(x, center=TRUE, scale=TRUE)$x)))
  
  #include data in new model for inclusion in a linear model
  #https://stats.stackexchange.com/questions/72839/how-to-use-r-prcomp-results-for-prediction
  
  #suppressMessages(pcaModel<- glm(y~pc$x[,1:length(data.frame(pc$x))]))
  
  #predict using pca, just re-applying to training data.
  
  #applied PCA to holdout
  
  #does this make it linear?
  pc <- prcomp(x, center=TRUE, scale=TRUE)
  pred <- predict(pc,x)
  #plot(data.frame(y,drop=FALSE),pred)
  pcaPred <- lm(cbind(data.frame(y),pred))
  
  #yhatPCA = predict(pcaPred, x)
  #colnames(y)<-"y"
  tempy <- data.frame(y)
  colnames(tempy)<-"y"
  #tempy
  
  #http://www.milanor.net/blog/performing-principal-components-regression-pcr-in-r/
  pcr_model <- pcr(y~., data = cbind(tempy,x), scale = TRUE, validation = "CV")
  summary(pcr_model)
  pcr_pred <- predict(pcr_model, x)
  hist(pcr_pred)
  table(y)
  table(pcr_pred)
  
  
  pred <- prediction(data.frame(pcr_pred)[,1],y)
  roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
  plot(roc.perf)
  abline(a=0, b= 1)  
 
  #predict(pcaPred,)
  
  #predict(pcaPred,filteredv7133holdout[-1])
  
  #summary(pcaPred)
  hist(abs(pcaPred$residuals))
  
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
  ytest = PostDF[,1,drop=FALSE][,1]
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
  
  #pred <- prediction(yhat.test,data.test[,1])
  #roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
  #plot(roc.perf)
  #abline(a=0, b= 1)
  
  #this is manually converting them
  #test classification using best linear model
  yhat.transformed = rep(0, nrow(PostDF))
  yhat.transformed[yhat > 0] = 1
  yhat.transformed[yhat < 0] = 0
  sum(yhat.transformed)
  
  #typeof(ytest)
  #data.frame(ytest)
  rmse(ytest, yhat.transformed)
  
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
  
  #https://machinelearningmastery.com/confusion-matrix-machine-learning/
  #https://www.rdocumentation.org/packages/caret/versions/3.45/topics/confusionMatrix
  #https://www.datacamp.com/community/tutorials/confusion-matrix-calculation-r
  #https://rdrr.io/cran/caret/man/confusionMatrix.html
  
  results <- confusionMatrix(data=as.factor(yhat.transformed), reference=as.factor(ytest))
  print(results)
  
}

