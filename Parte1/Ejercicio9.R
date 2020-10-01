#Ejercicio 9
#librerias usadas
library(car)
library(fBasics)
library(alr3)
library(lmtest)
library(leaps)
library(MASS)

attach(peso) #Lo hacemos siempre con el array que vayamos a usar para que no de fallos

#a)
mod9 <- lm(pesor ~ sem+pesom+tallam+pesop+tallap)
mod9
summary(mod9)
#tallam no es significativo ya que el p valor es mayor que 0.05
anova(mod9) #Probabilidad de rechazar la hip nula sabiendo que esta es cierta con la
#muestra. pvalor tallam mayor de 0.05 con lo cual aceptamos l hip nula, la constante vale 0
#y no es significativa.

modcte <- lm(pesor~1)#Este es el modelo constante
summary(modcte)
anova(modcte,mod9) #P-valor significantivo, medias distintas, rechazas la hip nula(medias 
#iguales). #El modelo con mas variables te explica mas cosas del modelo, proporciona mas inf

#b)
plot(mod9)#Comprobamos como en 2.5 no podemos mirar el ajuste porque solo hay una variable
#con ese valor. No puedo decir nada de la linealidad o dispersión a simple vista
#Todas las observaciones parecen dentro del límite de apalancamiento.

#Autocorrealción
plot(residuals(mod9), type="o")
abline(h=0,col="red")

#Para detectar bucles este comadno ( en el tag es el salto que queremos)
#Estudias la incorrelación pero con un salto. el valor de lag tienen en cuenta todos los
#???saltos anteriores. No solo de 5 en  5. Así comprobamos que no haya bucles previos, 
#hasta salto con 5
Box.test(residuals(mod9), lag=5, type="Ljung-Box")

residuos <- residuals(mod9)
ajustes <- fitted(mod9)
shapiro.test(residuos)
#Valor muy pequeño, luego rechazamos la normalidad

leveneTest(residuos, ajustes )#Esto sería para igualdad de varianza pero no se puede 
#hacer al no poder hacer la linealidad, eso es la homocedasticidad

#c)
mod1 <-lm(pesor ~ sem)
summary(mod1)
pureErrorAnova(mod1) #No es lineal, no puedes ajustarlo.
dwtest(mod1)#Son incorreladas, lo coge sobre el anteriorr

residuos1 <- residuals(mod1)
ajustes1 <- fitted(mod1)

shapiro.test(residuos1) #En el examen esperemos que salga normal.
Box.test(residuos1, lag=5, type="Ljung-Box") #Son incorreladas, este lo coge intervalo 5
leveneTest(residuos1, ajustes1 ) #Hay homogeneidad de varianzas, aceptamos H0
#Debe ser lineal para que funcione.
#Bartlett sirve para lo msimo que levenne pero en caso de tener normalidad en los residuos.

ks.test(residuos,"pnorm") #Los residuos no siguen normalidad

#d)
summary(mod9)
nuevopunto = data.frame(sem=c(39), pesom=c(62),tallam=c(168), pesop=c(78), tallap=c(170))
predict(mod4, nuevopunto, interval= "prediction",level = 0.95)
bandasIC(mod4,nivel = 0.95)

nuevopunto1 = data.frame(sem=c(36), pesom=c(74),tallam=c(160), pesop=c(88), tallap=c(180))
predict(mod4,nuevopunto1)

#e)
sel<-regsubsets(pesor~.,data=peso,nbest=1)
summary(sel)
plot(sel,scale="Cp") #Usamos criterio de Hocking Como p=8 +1 = 9, nuestro modelo es válido

pasos <- step(modcte,scope = list(lower=modcte,upper=mod9),direction="forward",k=log(length(pesor)))
anova(pasos)
summary(pasos)
