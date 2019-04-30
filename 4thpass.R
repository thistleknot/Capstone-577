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
  widthDiviser = 3
  
  if (widthDiviser == 1) train.control <- trainControl(method = "repeatedcv", number = 2, repeats = widthDiviser)
  if (!(widthDiviser == 1)) train.control <- trainControl(method = "repeatedcv", number = 10, repeats = widthDiviser)
  
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
files <- list.files(path=paste0(sourceDir,'/output/'), pattern="*filtered.csv", full.names=TRUE, recursive=FALSE)

#postProcess=21
for (postProcess in 1:length(files))
{ 
  print(paste("file: ",files[postProcess]))
  #NewDF assumes 0's mean NA's, this is more like a population dataframe already precleaned. (i.e. the export of my cleandatacode.R cleans na's)
  PostDF <- read.csv(files[postProcess], header=TRUE, sep=",")[,-1,drop=FALSE]
  colnames(PostDF)
  
  df <- PostDF[,-1, drop=FALSE]
  colnames(df) <- colnames(PostDF[,-1,drop=FALSE])
  
  y <- data.frame(PostDF[,1, drop=FALSE])[,1]
  x <- data.frame(PostDF[,1, drop=FALSE])[,-1]
  
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
    result <- c()
    result <- data.frame(correlationPairs[,featureList][(abs(correlationPairs[,featureList]) >= abs(response))])
    if(!nrow(result)==0)
    {
      print(data.frame(correlationPairs[,featureList][(abs(correlationPairs[,featureList]) >= abs(response))]))
    }
    
  }
  
  tempy <- data.frame(y)
  colnames(tempy)<-"y"
  vifModel <- glm(y~.,data=cbind(tempy,df),family=binomial(link="logit"))
  if(length(df)!=1) 
    {
    vifResults <- (car::vif(vifModel))

    drop<-c()
        drop <- c(as.character(vifResults[vifResults >= 5]))
      #print(drop)
      if (length(drop)==0) x <- df
      if (!(length(drop)==0)) {
        print(paste("dropping!",c(drop)))
        x <- df[ , -which(names(df) %in% drop)]}
    
    }
  if(length(df)==1) x <- df
  
  #https://stackoverflow.com/questions/5234117/how-to-drop-columns-by-name-in-a-data-frame
  #df[ , -which(names(df) %in% c("z","u"))]
  #df[ , -which(names(df) %in% c("z","u"))]
  
}

