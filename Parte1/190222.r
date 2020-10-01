#22/02
ks.test(residuos,"pnorm")
library(fBasics)
normalTest(residuos,method="da")

summary(mod4)

nuevopunto=data.frame(sem=c(39,38), pesom=c(62,60), pesop=c(78,90), tallap=c(170,160))

predict(mod4, nuevopunto, interval="confidence") #se estima la media
predict(mod4, nuevopunto, interval="prediction", level=0.95) #se estima un individuo concreto, por eso el intervalo de confianza es mayor
bandasIC(mod4,nivel=0.97) #la banda verde es la de la media

#seleccionar modelo
library(leaps)
sel <- regsubsets(pesor~.,data=peso,nbest=1) #El punto coge todas las variables disponibles en esa base de datos
summary(sel)
plot(sel,scale="Cp")

modtodas <- lm(pesor~.,data=peso)

library(MASS)
passo <- step(modcte,scope=list(lower=modcte,upper=modtodas),direction="forward",k=log(length(pesor))) #Lo cambia luego añadiendo k, usa otro criterio ahora(BIC)
summary(passo) #sí, lo ha escrito mal, lo dejo por si acaso

paso <- stepAIC(modoct,scope=list(lower=modcte,upper=modtodas),direction="forward",k=log(length(pesor)))
anova(paso)

library(RcmdrMisc)
stepwise(modtodas, direction="forward",criterio="BIC")