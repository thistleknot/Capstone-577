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
  
  
  df <- PostDF[,-1, drop=FALSE]
  colnames(df) <- colnames(PostDF[,-1,drop=FALSE])
  
  y <- data.frame(PostDF[,1, drop=FALSE])[,1]
  
  #https://stackoverflow.com/questions/5233308/is-there-a-r-function-that-applies-a-function-to-each-pair-of-columns
  #pairwise correlations
  #df <- x
  n <- ncol(df)
  
  corpij <- function(i,j,data) {cor.test(data[,i],data[,j])$p.value}
  corp <- Vectorize(corpij, vectorize.args=list("i","j"))
  correlationPairs <- outer(1:n,1:n,corp,data=df)
  
  #test for collinearity
  #"if any of the (absolute) correlations between each pair of predictors is greater than the highest (absolute) correlation between Y and each of the predictors."
  for(featureList in 1:n)
  {
    #featureList matches the correlationPairs column name with c columnname and is also used in the response (it's the central unitary term that holds the concept together)
    #vars relation with response
    #was considering a pairwise with this
    response <- cor(cbind(df[,featureList],y))[2,1]
    #keep response
    
    print(paste(colnames(df[featureList]),featureList,abs(response)))
    print(data.frame(correlationPairs[,featureList][(abs(correlationPairs[,featureList]) >= abs(response))]))
    
    
  }
  
  tempy <- data.frame(y)
  colnames(tempy)<-"y"
  vifModel <- glm(y~.,data=cbind(tempy,df),family=binomial(link="logit"))
  vifResults <- (car::vif(vifModel))
  
  print(drop)
  drop <- c(as.character(vifResults[vifResults >= 5]))
  
  #https://stackoverflow.com/questions/5234117/how-to-drop-columns-by-name-in-a-data-frame
  #df[ , -which(names(df) %in% c("z","u"))]
  #df[ , -which(names(df) %in% c("z","u"))]
  x <- df[ , -which(names(df) %in% drop)]
  
  trainModel <- glm(y~.,data=cbind(tempy,x),family=binomial(link="logit"))
  
  #any column
  #https://stackoverflow.com/questions/46285484/if-any-column-in-a-row-meets-condition-than-mutate-column
  #df$c[apply(df == 7, 1, any)] <- 100

  
  #includes proportion of variance
  summary(prcomp(x, center=TRUE, scale=TRUE))
  te <- summary(prcomp(x, center=TRUE, scale=TRUE))$importance
  #pc plot
  plot(te[3,1:ncol(te)])
  
  #correlation plot of sample along with pca
  corrplot(cor(cbind(x,prcomp(x, center=TRUE, scale=TRUE)$x)))
  
  #http://rfaqs.com/mctest-r-package-detection-collinearity-among-regressors
  omcdiag(x, y, Inter=FALSE)
  
  
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
  
  #trainModel <- suppressMessages(train(PostDF[-1], as.factor(PostDF[,1]),method = "glm",trControl = train.control))

  
  #http://r-statistics.co/Logistic-Regression-With-R.html
  
  predicted <- plogis(predict(trainModel, PostDF[,-1,drop=FALSE]))  # predicted scores
  #logitMod <- glm(ABOVE50K ~ RELATIONSHIP + AGE + CAPITALGAIN + OCCUPATION + EDUCATIONNUM, data=trainingData, family=binomial(link="logit"))
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
  
  #yhat = predict(trainModel, PostDF[,-1,drop=FALSE])
  yhat <- c()
  yhat <- predicted
  #summary(trainModel)
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
  print(table(yhat))
  #View(yhat)
  #this is where an arbitary threshold is set.
  yhat.transformed_sens = rep(0, nrow(PostDF))
  yhat.transformed_sens[round(yhat,4) >= round(optCutOff_sens,4)] = 1
  yhat.transformed_sens[yhat < optCutOff_sens] = 0
  misClassError(yhat.transformed_sens, ytest, threshold = optCutOff_sens)
  
  yhat.transformed_center = rep(0, nrow(PostDF))
  yhat.transformed_center[round(yhat,4) >= round(optCutOff_center,4)] = 1
  yhat.transformed_center[round(yhat,4) < optCutOff_center] = 0
  misClassError(yhat.transformed_center, ytest, threshold = optCutOff_center)
  
    
  yhat.transformed_spec = rep(0, nrow(PostDF))
  yhat.transformed_spec[round(yhat,4) >= round(optCutOff_spec,4)] = 1
  yhat.transformed_spec[round(yhat,4) < optCutOff_spec] = 0
  misClassError(yhat.transformed_spec, ytest, threshold = optCutOff_spec)

  print(optCutOff_sens)
  print(optCutOff_center)
  print(optCutOff_spec)
  #sum(yhat.transformed)
  ytemp<-c()
  ytemp = data.frame(yhat.transformed_center)[,,drop=FALSE]
  colnames(ytemp)<-"yhat"
  #http://ethen8181.github.io/machine-learning/unbalanced/unbalanced.html
  cm_info <- ConfusionMatrixInfo( data = cbind(ytemp,ytest), predict = "yhat", actual = "ytest", cutoff = optCutOff_center )
  
  #print(cm_info$data[order(cm_info$data$predict),])
  cm_info$plot
  
  #typeof(ytest)
  #data.frame(ytest)
  #rmse(ytest, yhat.transformed)
  
  #Calculating MSE for training data
  #mse.train<- mean(residuals(step.model.train)^2)
  #mse.train
  
  #Calculating MSE for testing data
  #mse.test <- mean(residuals(step.model.test)^2)
  #mse.test
  
  total_predictions = nrow(data.frame(yhat.transformed_center))
  correct_predictions = sum(yhat.transformed_center == ytest)
  classification_accuracy = correct_predictions / total_predictions
  error_rate = (1 - (correct_predictions / total_predictions))
  print(paste("error rate:",round(error_rate,3)))
  
  #https://machinelearningmastery.com/confusion-matrix-machine-learning/
  #https://www.rdocumentation.org/packages/caret/versions/3.45/topics/confusionMatrix
  #https://www.datacamp.com/community/tutorials/confusion-matrix-calculation-r
  #https://rdrr.io/cran/caret/man/confusionMatrix.html
  
  #class 1
  results <- c()
  results <- confusionMatrix(yhat.transformed_sens, ytest[,1])
  print(results)
  
  results <- c()
  results <- confusionMatrix(yhat.transformed_center, ytest[,1])
  print(results)
  
  results <- c()
  #class 0
  results <- confusionMatrix(yhat.transformed_spec, ytest[,1])
  print(results)
  
  
}

