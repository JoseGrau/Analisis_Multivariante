help(ability.cov) #En este me dan la matriz de covarianzas
M <- ability.cov
PCA <- princomp(covmat=M)
summary(PCA,loadings=TRUE)

