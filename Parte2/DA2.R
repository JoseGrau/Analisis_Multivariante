data("pottery")
library("MASS")
library("mvtnorm")
library("mvnormtest")

#Preguntar como aglutinar datos (si esta bien hecho)

d<-pottery
D<-1:45
for (i in 1:45) if(d[i,10]==1) D[i]=1
for (i in 1:45) if(d[i,10]==2 || d[i,10]==3) D[i]=2
for (i in 1:45) if(d[i,10]==4 || d[i,10]==5) D[i]=3

tapply(d[,5],D,summary)
plot(d[,5],D)

#LDA
LDA<-lda(d[,1:9],D,prior=c(1/3,1/3,1/3))

LDACV<-lda(d[,1:9],D,prior=c(1/3,1/3,1/3),CV=TRUE)
table(LDACV$class,D)
#Prob acierto 100%

#QDA
QDACV<-qda(d[,1:9],D,CV=TRUE)
table(QDACV$class,D)

#Acierta el 77.77%