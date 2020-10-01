#Analisis discriminante

library("MASS")
library("mvtnorm")
library("mvnormtest")
#load (escarabajos)


#Estudio inicial de los datos

View(d)
d$surco

tapply(d$surco,d$especie,summary) #Estadisticos basicos en cada grupo

#Representar los datos
plot(d$surco,d$codigo)
text(d$surco[40],1.5,labels="e40") #Para añadir el no identificado

#Cajas bigote
boxplot(d$surco ~ d$especie) #clasificamos con esta variable bien el 75% y el 40 va en HC

#Ahora tomamos los datos por parejas
plot(d$surco,d$long,pch=as.integer(d$especie))
legend("topright",legend=c("e40","HC","HO"),pch=1:3)

#Si tomamos las dos primeras componentes principales
pca<-princomp(d[,1:4],cor=TRUE)
biplot(pca,pc.biplot=TRUE,xlabs=d$especie)


#LDA

LDA<-lda(d[1:39,1:4],d[1:39,6],prior=c(0.5,0.5)) #si no especifico el a priori se toma como una muestra
a<-LDA$scaling #guardamos los coeficientes estimados

L<-function(z) sum(a*z) #Para definir una funicon para calcular L (proyeccion de las medias)
mHO<- L(LDA$means[1,]) #Proyecciones de las medias del primer grupo
mHC<- L(LDA$means[2,]) #Proyecciones de las medias del segundo grupo
K<-(mHC+mHO)/2 #Criterio de decision

#Calculamos las proyecciones de los 40 escarabajos
D<-1:40
for (i in 1:40) D[i]<-L(d[i,1:4])

#Y las representamos
plot(D,d$codigo)
text(D,d$codigo,cex=0.7,pos=1,col="red")
#Incluimos el 40
text(D[40],1.5,labels="*") 
text(D[40],1.5,labels="e40",cex=0.7, pos=3,col="red")
#Añadimos el criterio al grafico
text(K,1.5,labels="|")
text(K,1.5,labels="K",cex=0.7,pos=3,col="red")

#Otra forma: Haciendo D-K
predict(LDA,d[,1:4])->P
ldahist(P$x,g=d$especie)
P$class==d[,6]->Resumen #Asi podemos ver cuando ha clasificado bien
table(P$class,d[,6]) #Asi obtengo una tabla con cuantos clasificados donde la columna es el grupo al que pertenece y la fila donde lo clasifique
# P$posterior #Las probabilidades a posteriori de pertenencia a los grupos (vemos por ejemplo que la clasificacion del 40 no es muy fiable)

z<-c(185,280,150,200) #Vemos ahora donde clasificariamos un nuevo escarabajo con estas medidas
predict(LDA,z) #Lo clasificamos en 2 (LD1 > 0 y prob a posteriori 0.81)

#Cross Validation
LDACV<-lda(d[1:39,1:4],d[1:39,6],prior=c(0.5,0.5),CV=TRUE)
table(LDACV$class,d[1:39,6]) #De esta forma obtengo una tabla que indica mejor si clasifico bien


#QDA

QDA<-qda(d[1:39, 1:4], d[1:39, 6],prior=c(0.5,0.5))
#Datos esfericos
QDA$scaling
QDA$ldet

predict(QDA,d[,1:4])->P
table(P$class,d$codigo)

#Con cross validation
QDACV<-qda(d[1:39,1:4],d[1:39,6],prior=c(0.5,0.5),CV=TRUE)
table(QDACV$class,d[1:39,6])

z<-c(185,280,150,200)#Nuevo individuo
predict(QDA,z)


#Comprobaciones del modelo

#¿covarianzas iguales?
d1<-d[d$especie=="HO",1:4]
S1<-cov(d1)
d2<-d[d$especie=="HC",1:4]
S2<-cov(d2)

S<-(18*S1+19*S2)/37
solve(S)->In
LDA$means[1,]->m1
LDA$means[2,]->m2
(m1-m2)%*%In->a
LDA$scaling/t(a) #Y asi sacamos el lambda que usa

ds<-scale(d[,1:4])
lda(ds[1:39,1:4],d[1:39,6],prior=c(0.5,0.5))

lda(d[1:39,c(1,3,4)],d[1:39,6],prior=c(0.5,0.5),CV=TRUE) #Para eliminar la segunda variable del lda

#Probabilidades a posteriori
#Con LDA
dmvnorm(d[40,1:4],m1,S)->f1
dmvnorm(d[40,1:4],m2,S)->f2
f1/(f1+f2) #probabilidad a posteriori del escarabajo 40 en el grupo 1 con a priori iguales
19*f1/(19*f1+20*f2) #igual pero con las a priori dadas por los grupos
#Con QDA
dmvnorm(d[40,1:4],m1,S1)->f1
dmvnorm(d[40,1:4],m2,S2)->f2
f1/(f1+f2)

#Test de normalidad para los grupos 
mshapiro.test(t(d[1:19,1:4])) #pvalor mayor que 0.05, aceptamos normalidad
mshapiro.test(t(d[20:39,1:4])) #pvalor mayor que 0.05 pero por poco, aceptamos con este alfa

#Calculo de las funciones discriminantes por grupos (util para mas de dos grupos)

sum(solve(S) %*%m1*d[40,1:4])-0.5*t(m1) %*%solve(S) %*%m1 #esto es L1
sum(solve(S) %*%m2*d[40,1:4])-0.5*t(m2) %*%solve(S) %*%m2 #esto es L2


#Y las cuadraticas (esto es como hacerlo a mano)

mahalanobis(d[40,1:4],mHO,S1) #Preguntar
