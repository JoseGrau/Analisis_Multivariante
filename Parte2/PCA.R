#ANÁLISIS DE COMPONENTES PRINCIPALES (PCA)

#Estudio inicial de los datos
data() #Para ver los paquetes de datos disponibles en R

d<-LifeCycleSavings

View(d) #Para ver los datos en d
help(d) #Para explicacion de los datos

d[,1] #Para ver la columna sr
d[7,] #Para ver la fila 7

summary(d)
str(d) #tipo de variables, número,...
plot(d)

cov(d) #Matriz de covarianzas
M<-cor(d) #Matriz de correlaciones
M

boxplot(d) #caja bigote
boxplot(d[,1]) #de una sola variable

which.max(d[,5]) #donde esta el maximo de esa variable
sort(d[,5]) #ordena los datos
order(d[,5]) #lineas donde se alcanzan los datos ordenados

hist(d[,1])
mahalanobis(d,colMeans(d),cov(d)) #para detectar valores atipicos (los mas altos)


# Cálculo de las Componentes Principales.
PCA<-princomp(d,cor=TRUE) #Calculo de las componentes principales

summary(PCA,loadings=TRUE) #Importancia de las componentes y loadings(vectores propios unitarios)

T<-PCA$loadings
T[,1] #Primer vector propio que se utiliza para calcular la primera componente principal
S<-PCA$scores
S[,1] #Puntuaciones de la primera componente

PCAbis<-prcomp(d,scale=TRUE) #Para hacer el PCA con el otro metodo (mejor el otro)
summary(PCAbis) #Importancia de las componentes
PCAbis$rotation #Cargas
PCAbis$x #Puntuaciones

#Análisis de Componentes Principales
summary(S[,1]) #Primera componente
plot(S[,1])
sort(S[,1])

which.max(S[,1])
which.min(S[,1]) 

biplot(PCA,pc.biplot=TRUE) # Grafico de las dos primeras componentes principales
biplot(PCA,pc.biplot=TRUE,xlabs=1:50) #Para cambiar las etiquetas por sus numeros de linea
biplot(PCA,pc.biplot=TRUE,choices=c(3,4),xlabs=1:50) #Grafico de las componentes tercera y cuarta

plot(S[,1],S[,2],xlab="Y1",ylab="Y2") # gráfico de las puntuaciones en las dos primeras componentes
text(S[38,1]+0.4,S[38,2],labels="Esp") #Poner etiqueta al dato

pairs(PCA$scores[,1:3]) #Representacion de las tres primeras componentes (poco habitual)

#Saturaciones y comunalidades
S1<-T[,1]*1.6799041 #Saturaciones de la primera componente
S1^2 #Informacion de cada variable en la componente

SAT<-cor(d,S) #Aqui calculamos todas las saturaciones

SAT[,1]^2+ SAT[,2]^2 #Informacion que mantienen las dos primeras componentes

Z1<-0.3084617*S[,1]+ 0.5542456*S[,2]

#Numero de componentes
summary(PCA) #Y aqui elegir un criterio

screeplot(PCA) 
plot(eigen(cor(d))$values,type="l",ylab="valores propios") #grafico de sedimentacion#Grafica de sedimentacion

#Prueba de esfericidad (en este ejemplo con m=2)
eigen(cor(d))$values->L
mean(L[3:5])->ma
exp(mean(log(L[3:5])))->mg
(50-(2*5+11)/6)*(5-2)*log(ma/mg)->Tp
0.5*(5-2-1)*(5-2+2)->gl
1-pchisq(Tp,gl) #como sale menor de 0.05 y no hay esfericidad

