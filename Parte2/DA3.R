#pulgas.rda
library("MASS")
library("mvtnorm")
library("mvnormtest")

tapply(d$LONGITUD,d$ESPECIE,summary)
tapply(d$ANCHURA,d$ESPECIE,summary)


plot(d)

boxplot(d$LONGITUD~d$ESPECIE)
boxplot(d$ANCHURA~d$ESPECIE)

plot(d$LONGITUD,d$ANCHURA,pch=d$ESPECIE)

#LDA
#Preguntar significado LD1 menor que 0 en 1 y luego LD2 o como va?

LDA<-lda(d[,1:2],d[,3]) #si no especifico el a priori se toma como una muestra
a<-LDA$scaling #guardamos los coeficientes estimados
#Aqui tengo los coeficientes del metodo

LDACV<-lda(d[,1:2],d[,3],prior=c(1/3,1/3,1/3),CV=TRUE)
table(LDACV$class,d[,3]) #De esta forma obtengo una tabla que indica mejor si clasifico bien
#acierta el 98.64% de los casos

#QDA

QDA<-qda(d[, 1:2], d[,3],prior=c(1/3,1/3,1/3))

QDACV<-qda(d[,1:2],d[,3],prior=c(1/3,1/3,1/3),CV=TRUE)
table(QDACV$class,d[,3])

#igual que el otro, acierta el 98.64%