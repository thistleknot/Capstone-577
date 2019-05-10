sub_returnCVOG <- function(data_sent){
  #data_sent=finalTraining[holderOfDataI,]
  #data.train
  
  holderOfData <- cbind(Filter(var,data.frame(data_sent[,-1 , drop = FALSE])),data.frame(data_sent[,1 , drop = FALSE]))
  #table(NewDF[,"V7202"])
  
  #info <- which(colSums(holderOfData)==nrow(holderOfData))
  #name <- rownames(data.frame(info))
  #if(!length(info)==0) holderOfData <- holderOfData[, -which(names(holderOfData) == name)]
  
  B <- suppressMessages(bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=3, REP=1, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive"))
  
  result <- c()
  #return all coefficients
  result <- data.frame(as.character(rownames(data.frame(B$BestModel$coefficients[]))[-1]))
  
  set<-round(colSums(B$Subsets))[-1]
  
  if(!is.null(result)) result <- NA
  
  #if(nrow(result)==2)
  #{
  #result <- 
  #}
  #aboveMedianCV <- as.character(rownames(data.frame(which(result >= median(result)))))
  return(result)
}

#this return everything
sub_returnCVNames <- function(data_sent){
  #data_sent=data.train
  holderOfData <- cbind(Filter(var,data.frame(data_sent[,-1 , drop = FALSE])),data.frame(data_sent[,1 , drop = FALSE]))
  #table(NewDF[,"V7202"])
  
  info <- which(colSums(holderOfData)==nrow(holderOfData))
  name <- rownames(data.frame(info))
  if(!length(info)==0) holderOfData <- holderOfData[, -which(names(holderOfData) == name)]
  
  B <- suppressMessages(bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=3, REP=1, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive"))
  
  set<-round(colSums(B$Subsets))[-1]
  
  if(!is.null(B$Subsets))
  {
    cverrs = B$Subsets[, "CV"]
    sdCV = B$Subsets[, "sdCV"]
    CVLo = cverrs - sdCV
    CVHi = cverrs + sdCV
    ymax = max(CVHi)
    ymin = min(CVLo)
    k = 0:(length(cverrs) - 1)
    if(!(ymax=="Inf" || ymax=="-Inf")) plot(k, cverrs, ylim = c(ymin, ymax), type = "n", yaxt = "n")
    points(k,cverrs,cex = 2,col="red",pch=16)
    lines(k, cverrs, col = "red", lwd = 2)
    axis(2, yaxp = c(0.6, 1.8, 6))
    segments(k, CVLo, k, CVHi,col="blue", lwd = 2)
    eps = 0.15
    segments(k-eps, CVLo, k+eps, CVLo, col = "blue", lwd = 2)
    segments(k-eps, CVHi, k+eps, CVHi, col = "blue", lwd = 2)
    indMin = which.min(cverrs)
    fmin = sdCV[indMin]
    cutOff = fmin + cverrs[indMin]
    abline(h = cutOff, lty = 2)
    indMin = which.min(cverrs)
    fmin = sdCV[indMin]
    cutOff = fmin + cverrs[indMin]
    min(which(cverrs < cutOff))
  }
  
  left=length(set)-3
  result <- set[1:left]
  
  #aboveMedianCV <- as.character(rownames(data.frame(which(result >= median(result)))))
  return(as.character(rownames(data.frame((result)))))
}

sub_returnCVNames <- function(data_sent){
  #data_sent=data.train
  holderOfData <- cbind(Filter(var,data.frame(data_sent[,-1 , drop = FALSE])),data.frame(data_sent[,1 , drop = FALSE]))
  #table(NewDF[,"V7202"])
  
  info <- which(colSums(holderOfData)==nrow(holderOfData))
  name <- rownames(data.frame(info))
  if(!length(info)==0) holderOfData <- holderOfData[, -which(names(holderOfData) == name)]
  
  B <- suppressMessages(bestglm(Xy = holderOfData, IC="CV", CVArgs=list(Method="HTF", K=3, REP=1, TopModels=widthDiviser, BestModels = widthDiviser), family=binomial,method = "exhaustive"))
  
  set<-round(colSums(B$Subsets))[-1]
  
  if(!is.null(B$Subsets))
  {
    cverrs = B$Subsets[, "CV"]
    sdCV = B$Subsets[, "sdCV"]
    CVLo = cverrs - sdCV
    CVHi = cverrs + sdCV
    ymax = max(CVHi)
    ymin = min(CVLo)
    k = 0:(length(cverrs) - 1)
    if(!(ymax=="Inf" || ymax=="-Inf")) plot(k, cverrs, ylim = c(ymin, ymax), type = "n", yaxt = "n")
    points(k,cverrs,cex = 2,col="red",pch=16)
    lines(k, cverrs, col = "red", lwd = 2)
    axis(2, yaxp = c(0.6, 1.8, 6))
    segments(k, CVLo, k, CVHi,col="blue", lwd = 2)
    eps = 0.15
    segments(k-eps, CVLo, k+eps, CVLo, col = "blue", lwd = 2)
    segments(k-eps, CVHi, k+eps, CVHi, col = "blue", lwd = 2)
    indMin = which.min(cverrs)
    fmin = sdCV[indMin]
    cutOff = fmin + cverrs[indMin]
    abline(h = cutOff, lty = 2)
    indMin = which.min(cverrs)
    fmin = sdCV[indMin]
    cutOff = fmin + cverrs[indMin]
    min(which(cverrs < cutOff))
  }
  
  left=length(set)-3
  result <- set[1:left]
  
  #aboveMedianCV <- as.character(rownames(data.frame(which(result >= median(result)))))
  return(as.character(rownames(data.frame(which(result >= median(result))))))
}
