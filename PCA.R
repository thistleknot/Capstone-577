
head(iris)
d<-iris[,-5]
head(d)
pc <- princomp (d, cor=TRUE, score=TRUE)
summary(pc)