{
  widthDiviser = 1
  
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

#postProcess=1
for (postProcess in 1:length(files))
{ 
  
  #NewDF assumes 0's mean NA's, this is more like a population dataframe already precleaned.
  PostDF <- read.csv(files[postProcess], header=TRUE, sep=",")[,-1,drop=FALSE]
  
  trainModel <- suppressMessages(train(PostDF[-1], as.factor(PostDF[,1]),method = "glm",trControl = train.control))
  print("population")
  print(summary(trainModel$finalModel))
  
  finalList <- colnames(PostDF)
  #reseed
  source(paste0(sourceDir,"/reseedPost.R"))
  source(paste0(sourceDir,"/resampleMCpost.R"))
  
  res <- cor(data.train)
  corrplot(res)
  
  x= c()
  y= c()
  #yname <- c()
  
  x=data.train[,-1]
  y=data.train[,1]
  
  pc <- prcomp(data.train[,-1], center=TRUE, scale=TRUE)
  
  #includes proportion of variance
  summary(prcomp(data.train[,-1], center=TRUE, scale=TRUE))
  te <- summary(prcomp(data.train[,-1], center=TRUE, scale=TRUE))$importance
  #pc plot
  plot(te[3,1:ncol(te)])
  
  #correlation plot of sample
  corrplot(cor(cbind(data.train[,1],prcomp(data.train[,-1], center=TRUE, scale=TRUE)$x)))
  
  #include data in new model for inclusion in a linear model
  #https://stats.stackexchange.com/questions/72839/how-to-use-r-prcomp-results-for-prediction
  
  suppressMessages(pcaModel<- glm(y~pc$x[,1:length(data.frame(pc$x))]))
  
  #predict using pca, just re-applying to training data.
  
  #applied PCA to holdout
  
  x <- data.test[,-1, drop=FALSE]
  
  y <- data.frame(data.test[,1, drop=FALSE])
  
  #does this make it linear?
  pred <- predict(pc,x)
  #plot(data.frame(y,drop=FALSE),pred)
  pcaPred <- lm(cbind(y,pred))
  
  #predict(pcaPred,)
  
  #predict(pcaPred,filteredv7133holdout[-1])
  
  #summary(pcaPred)
  hist(abs(pcaPred$residuals))
  
  summary(pcaModel)
  #summary(pcaPred)
  
  regularTrainModel <- suppressMessages(glm(data.train))
  regularTestModel <- suppressMessages(glm(data.test))
  
  # Define training control
  
  #http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/#k-fold-cross-validation
  #yname <- colnames(data.train[,1,drop=FALSE])
  #nrow(y)
  x=data.train[,-1,drop=FALSE]
  y=(data.train[,1,drop=FALSE])
  
  #View(data.train[,1])
  #will this work, train on train partition, and validate on a test partition?  Probably a bad idea, because I'm going to predict using test...
  trainModel <- suppressMessages(train(data.train[-1], as.factor(data.train[,1]),method = "glm",trControl = train.control))
  testModel <- suppressMessages(train(data.test[-1], as.factor(data.test[,1]), method = "glm",trControl = train.control))
  
  A <- c()
  holderOfData <- c()
  holderOfData <- cbind(data.frame(data.train[,-1 , drop = FALSE]),data.frame(data.train[,1 , drop = FALSE]))
  A <- suppressMessages(bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=2, REP=widthDiviser, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive"))

  B <- c()
  holderOfData <- c()
  holderOfData <- cbind(data.frame(data.test[,-1 , drop = FALSE]),data.frame(data.test[,1 , drop = FALSE]))
  B <- suppressMessages(bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=2, REP=widthDiviser, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive"))
    
  print("sig 1")
  print(A$Subsets)
  print(summary(trainModel))
  
  print("sig 2")
  print(summary(testModel))
  print(B$Subsets)
  
  holderOfData.train <- cbind(data.frame(data.train[,-1 , drop = FALSE]),data.frame(data.train[,1 , drop = FALSE]))
  holderOfData.test <- cbind(data.frame(data.test[,-1 , drop = FALSE]),data.frame(data.test[,1 , drop = FALSE]))
  
  if (widthDiviser==1)  A <- bestglm(Xy = holderOfData.train, IC="CV", CVArgs=list(Method="HTF", K=2, REP=widthDiviser, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive")
  if (widthDiviser!=1)  A <- bestglm(Xy = holderOfData.train, IC="CV", CVArgs=list(Method="HTF", K=widthDiviser, REP=widthDiviser, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive")
  print (A$Subsets)
  if (widthDiviser==1)  B <- bestglm(Xy = holderOfData.test, IC="CV", CVArgs=list(Method="HTF", K=2, REP=widthDiviser, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive")
  if (widthDiviser!=1)  B <- bestglm(Xy = holderOfData.test, IC="CV", CVArgs=list(Method="HTF", K=widthDiviser, REP=widthDiviser, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive")
  print(B$Subsets)
  
  merged <- c()
  holderOfData <- c()
  merged <- rbind(data.train,data.test)
  holderOfData <- cbind(data.frame(merged[,-1 , drop = FALSE]),data.frame(merged[,1 , drop = FALSE]))
  
  B <- c()
  B <- bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=2, REP=widthDiviser, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive")
  print(B$Subsets)
  
  trainModel <- c()
  trainModel <- suppressMessages( train(merged[, -1, drop = FALSE], as.factor(merged[,1]),method = "glm",trControl = train.control) )
  print("test 1")
  print(summary(trainModel$finalModel))
  print(B$Subsets)
  print("comb sig")
  print(summary(trainModel))
  #I swear I was doing predicitons before with better accuracy
  
  #reseed
  source(paste0(sourceDir,"/reseedPost.R"))
  source(paste0(sourceDir,"/resampleMCpost.R"))
  
  merged <- c()
  holderOfData <- c()
  merged <- rbind(data.train,data.test)
  holderOfData <- cbind(data.frame(merged[,-1 , drop = FALSE]),data.frame(merged[,1 , drop = FALSE]))
  
  B <- c()
  B <- bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=2, REP=widthDiviser, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive")
  
  testModel <- c()
  testModelPred <- c()
  testModel <- suppressMessages(train(merged[,-1, drop = FALSE], as.factor(merged[,1]),method = "glm",trControl = train.control))
  
  #using newly acquired merged data, and prior trained model, derive predictions
  trainModelPred <- round(predict.glm(trainModel$finalModel, merged))
  print("test 2")
  print(B$Subsets)
  print(summary(testModel$finalModel))
  hist(abs(trainModelPred-merged[,1]))
  
  #http://www.r-tutor.com/elementary-statistics/logistic-regression/estimated-logistic-regression-equation
  #https://www.theanalysisfactor.com/r-tutorial-glm1/
}

