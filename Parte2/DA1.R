#bears.rda
library("MASS")
library("mvtnorm")
library("mvnormtest")

#Jugando con los datos
plot(d[5:10])
tapply(d[,5],d[,4],summary)
plot(d[,5],d[,4])
boxplot(d[,5]~d[,4])
plot(d[,5],d[,6],pch=d[,4])
pca<-princomp(d[,5:10],cor=TRUE)
biplot(pca,pc.biplot=TRUE,xlabs=d[,4])

#Aqui empiezo: LDA
LDACV<-lda(d[,5:10],d[,4],prior=c(0.5,0.5),CV=TRUE) #si no especifico el a priori se toma como una muestra
a<-LDACV$scaling #guardamos los coeficientes estimados


table(LDACV$class,d[,4])

#Poco acierto (64.33%)

#Ahora QDA

QDACV<-qda(d[, 5:10], d[, 4],prior=c(0.5,0.5),CV=TRUE)

table(QDACV$class,d[,4])

#Acierto del 60.83% va peor????
