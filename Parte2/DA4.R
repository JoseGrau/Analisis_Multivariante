wine<-read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",sep=",")
library("MASS")
library("mvtnorm")
library("mvnormtest")

d<-wine
View(d)

plot(d)

boxplot(d$V2~d$V1)

#LDA
LDA<-lda(d[,2:14],d[,1],prior=c(1/3,1/3,1/3))
a<-LDA$scaling #coeficientes del metodo

LDACV<-lda(d[,2:14],d[,1],prior=c(1/3,1/3,1/3),CV=TRUE)
table(LDACV$class,d[,1])
#Acierta en el 98.87%

#QDA
QDA<-qda(d[, 2:14], d[,1])

QDACV<-qda(d[,2:14],d[,1],prior=c(1/3,1/3,1/3),CV=TRUE)
table(QDACV$class,d[,1])
#Acierta el 99.43% de veces