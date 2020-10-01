#load("decatlon")
View(d) 
PCA<-princomp(d[1:11],cor=TRUE) #le quito NOMBRE
sort(mahalanobis(d[1:11],colMeans(d[1:11]),cov(d[1:11]))) #Para buscar outliers
summary(PCA,loadings=TRUE)
PCA$loadings->T
PCA$scores->S
biplot(PCA,pc.biplot=TRUE)
sort(S[,1]) 
SAT<-cor(d[1:11],S)
SAT[,1]^2+SAT[,2]^2#Informacion que mantienen las componenetes de las variables originales
