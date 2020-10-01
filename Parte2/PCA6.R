#cargar antes el paquete HSAUR2
data("pottery")
d <- pottery
View(d)
PCA<-princomp(d[1:9],cor=TRUE) #Tenemos que quitar el identificador del horno
sort(mahalanobis(d[1:9],colMeans(d[1:9]),cov(d[1:9]))) #Para buscar outliers
summary(PCA,loadings=TRUE) 
PCA$loadings->T
PCA$scores->S

biplot(PCA,pc.biplot=TRUE,xlabs=d$kiln) #Podemos ver como las medidas si que indican bastante bien el 
# origen de la pieza de ceramica

SAT<-cor(d[1:9],S)
SAT[,1]^2+SAT[,2]^2 #Informacion que mantienen las componenetes de las variables originales
