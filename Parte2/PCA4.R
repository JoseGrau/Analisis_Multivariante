#load("bears")
View(d) 
PCA<-princomp(d[,5:10],cor=TRUE) #Para aplicarlo solo a una parte de los datos
sort(mahalanobis(d[,5:10],colMeans(d[,5:10]),cov(d[,5:10]))) #Para buscar outliers
summary(PCA,loadings=TRUE) #Vemos que la primera componente es directamente lo grandes que son
#La segunda seria algo asi como los que tienen una cabeza pequeña (o fina)
PCA$loadings->T
PCA$scores->S
biplot(PCA,pc.biplot=TRUE)
sort(S[,1]) 
SAT<-cor(d[,5:10],S)
SAT[,1]^2 #Informacion que mantienen las componenetes de las variables originales
