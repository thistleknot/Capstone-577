

pairedLists = function(n) {
  
  n=3
  length=n
  #give me from values ranging in 1:n, [that many] n unique pairs
  
  range <- 1:n
  pool1 <- c()
  pool1 <- sample(1:n,n)
  pool2 <- c()
  pool2 <- sample(1:n,n)
  set <- c()
  #i=3
  for(i in 1:length(pool1))
  {
    currentNumber <- pool1[i]
    print(currentNumber)
    a <-currentNumber
    b <-pool2

    #https://www.biostars.org/p/180451/
    available <- b[!(b %in% a)]
    print(available)
    picked <- available[1]
    #pool2 <- c()
    pool2 <- b[!(b %in% picked)]
    
    #no need to resample
    #chosen <- sample(c(available),1)
    
    set <- rbind(set,picked)
    
  }
  if(is.na(set[nrow(set)]))
    #swap last two values
  {
    set[nrow(set)]=set[nrow(set)-1]
    set[nrow(set)-1]=currentNumber
    
  }
  
  #pool1
  left <- data.frame(pool1)
  right <- data.frame(set)
  
  #print(left)
  #print(right)
  return(cbind(left,right))
}

set.seed(10)
pairedLists(3)