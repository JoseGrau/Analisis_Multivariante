d<-USArrests
PCA<-princomp(d,cor=TRUE)
sort(mahalanobis(d,colMeans(d),cov(d))) #Para buscar outliers
summary(PCA,loadings=TRUE) #Vemos que la primera componente es lo seguro y la segunda lo urbanizado
PCA$loadings->T
PCA$scores->S
biplot(PCA,pc.biplot=TRUE)
sort(S[,1]) #El lugar mas seguro es Dakota del norte y el mas inseguro es Florida
SAT<-cor(d,S)
SAT[,1]^2+SAT[,2]^2+SAT[,3]^2 #Informacion que mantienen las componenetes de las variables originales
