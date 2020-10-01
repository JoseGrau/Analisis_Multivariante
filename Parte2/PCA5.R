#cargar antes el paquete HSAUR2
data("heptathlon")
d <- heptathlon
View(d)
PCA<-princomp(d,cor=TRUE)
sort(mahalanobis(d,colMeans(d),cov(d))) #Para buscar outliers
summary(PCA,loadings=TRUE) 
PCA$loadings->T
PCA$scores->S
biplot(PCA,pc.biplot=TRUE)
sort(S[,1]) 
SAT<-cor(d,S)
SAT[,1]^2+SAT[,2]^2 #Informacion que mantienen las componenetes de las variables originales
