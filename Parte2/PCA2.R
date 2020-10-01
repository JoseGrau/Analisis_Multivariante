d<-USJudgeRatings
PCA<-princomp(d,cor=TRUE)
sort(mahalanobis(d,colMeans(d),cov(d))) #Para buscar outliers
summary(PCA,loadings=TRUE) #Vemos que la primera componente es una mezcla de todas menos la primera 
#variable y la segunda es la primera variable. La primera es lo MAL abogado que es, la segunda, las
#veces que interactúa con el juez.
PCA$loadings->T
PCA$scores->S
biplot(PCA,pc.biplot=TRUE)
sort(S[,1]) #El mejor abogado es Rubinow, y el peor Cohen
SAT<-cor(d,S)
SAT[,1]^2+SAT[,2]^2 #Informacion que mantienen las componenetes de las variables originales
