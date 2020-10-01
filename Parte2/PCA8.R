#load("madres")
View(d) 
PCA<-princomp(d[2:10],cor=TRUE) #le quito ID
sort(mahalanobis(d[2:10],colMeans(d[2:10]),cov(d[2:10]))) #Para buscar outliers
summary(PCA,loadings=TRUE) #La primera componente es lo grande que es el recien nacido, la segunda 
#lo grande de la madre con su presion sanguinea y la tercera lo grande de la madre con menos presion
PCA$loadings->T
PCA$scores->S
biplot(PCA,pc.biplot=TRUE)
sort(S[,1]) 
SAT<-cor(d[2:10],S)
SAT[,1]^2+SAT[,2]^2+SAT[,3]^2#Informacion que mantienen las componenetes de las variables originales
