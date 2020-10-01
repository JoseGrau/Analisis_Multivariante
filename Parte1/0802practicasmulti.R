#Practica1 -- 08/02/2019

#Ejercicio9

attach(peso) #Lo hacemos siempre con el array que vayamos a usar para que no de fallos.
View(peso) #Vemos la información de un array.

plot(sem,pesor) #Representar

modelo1 <- lm(pesor ~ sem) #Trabajamos con este modelo
modelo1
abline(modelo1,col="red") #Representa el modelo en el plot 

#Si queremos saber algo sobre el modelo:

names(modelo1) #Todo lo que le podemos pedir
attributes(modelo1) #Como el anterior pero te da la clase de modelo
modelo1[[1]] #Nos da los coeficientes
modelo1$coefficients #Vuelve a dar lo mismo

#hasta aqui, con el siguiente comando lo hace todo:

summary(modelo1) #Influye la semana en la que ha nacido en el peso que tiene, eso lo sabemos 
anova(modelo1)   #con el p-valor si es menor que 0.05 distintas.
confint(modelo1,level = 0.9) #Intervalo de confianza, los intercept son las constantes.

vcov(modelo1) #Matriz de coeficiente

ygorro = modelo1$fitted.values #Valores estimados el fitted values, se puede pedir tambien con [5]
#ygorro=fitted(modelo1)
residuos = residuals(modelo1) #Residuos de un modelo
#residuos = modelo1$residuals


#Para verlo por columnas
cbind(pesor,sem,ygorro,residuos)[1:10,] #Juntamos todos los vectores en una matriz por columnas.
sum(residuos) #Suma los residuos, para ver que el error es pequeño, se aprox a cero o cero.
sum(sem*residuos) #Ortogonalidad de semanas

plot(modelo1,which=1:6) #Coges y lo pintas




# Practica2 -- 15/02/2019

#Ejercicio 9

#a)

#el modelo solo tiene sentido donde tenemos el conjunto de variables (35-41 sem)
modctee <- lm(pesor~1)#Este es el modelo constante (no hace falta incluirlo porque ya viene incluido. Si queremos un moedelo sin la constante, si tengo que quitárselo)
mod9 <- lm(pesor ~ sem+pesom+tallam+pesop+tallap) #Se suma para incluirlo todo, apartado a)
mod9
summary(mod9) #En este caso tallam no sería significativo. p valor mayor de 0.05
              #Coge la primera variable y las va incluyendo al modelo. Sumando todos los "value" tendríamos 
              #la p-value ojo!!! se va añadiendo uno a uno. Por lo que el value se corresponde con lo que ese 
              #parametro influye o no, aporta o no al modelo ya hecho con los parametros que estén antes
              #La altura de la madre es la primera candidata a eliminar como variable no significativa
              #(Esto se corresponde con el apartado a) del eje9)


abline(mod9,col="red")
summary(modcte)

anova(mod9) #Probabilidad de rechazar la hip nula sabiendo que esta es cierta con la muestra.
            #pvalor tallam mayor de 0.05 con lo cual aceptamos l hip nula, la constante vale 0 y
            #no es significativa.

anova(modctee,mod9) #P-valor significantivo, medias distintas, rechazas la hip nula(medias iguales).
                  #El modelo con mas variables te explica mas cosas del modelo, proporciona mas inf.



#b)

plot(modcte) #Comprobamos como en 2.5 no podemos mirar el ajuste porque solo hay una variable con ese valor.
             #No puedo decir nada de la linealidad o dispersión a simple vista
             #Todas las observaciones parecen dentro del límite de apalancamiento. (El apalancamiento se 
             #corresponde con la fuerza d elas variables para mover el modelo, es decir, para hacer que 
             #las variables predictoras influyan más o menos en el modelo)
plot(mod9)

confint(mod9,level=0.9)
vcov(mod9)  #La diagonal es la estimacion de la varianza de los beta gorro
            #DEtermina el factor de incremento de la varianza
            #Para cada vaariable del modelo me da el grado de dependencia con respectoa la optextras::Si son ortogonales entre ellas, el valor va a ser uno.
            #Si no, nos da  la dependencia. Muy cercano a 1, más ortogonales.

library(car)
vif(mod9) #Si te da 1 son ortogonales, si esta entre 1-10 no da problemas.

'''
sem    pesom   tallam    pesop   tallap 
1.811617 3.623448 3.858555 1.713982 2.068638

La menos ortogonal es la altura d ela madre. 
Entre 1 y 10 se considera que no da errores. 
SI alguno sería mayor a 10, tendríamos valores altos en la matriz de covarianzas, 
luego se comprobaria la impresicion y habría que eliminarlo del modelo
'''

ygorro=modelo1$fitted.values #Lo volvemos a definir
residuos=residuals(mod9)

#Para hacer el contraste del error puro
library(alr3)
pureErrorAnova(mod9) #Te da la tabla anova si no hay error puro, no es lineal.

'''
No me lo ha desglosado, luego no hay problema en el modelo
Si hacemos esto con el modelo1, nos lo desglosa y nos da Falta de ajuste jeje Lack of fit y Pure Error
Se comprueba como se rechaza la linealidad con el valor 0.001904 de Pr(>F)
'''


mod1 <-lm(pesor ~ sem)
#Esto es para la linealidad, es lineal este si.
pureErrorAnova(mod1) #Te da la tabla anova si no hay error puro, este da error puro, hay informa
                      #repetida. Sirve para estudiar linealidad. Te da p-valores.
                      #H0: Es lineal (no falta ajuste) H1: no lineal (hay falta de ajuste)


#Incorerlación
#NEn este caso la hipotesis nula es la incorrelación
library(lmtest)
dwtest(mod9)#Para ver la incorrelación ed los residuos del modelo


'''
	Durbin-Watson test

data:  mod9
DW = 1.9905, p-value = 0.4914
alternative hypothesis: true autocorrelation is greater than 0

Estadístico proximoa 2, luego modelo modelo bien ajustado. Si se acercara a 0 o 4, se rechazaría la incorrelación.
'''


#Autocorrealción
plot(residuals(mod9), type="o")#puntos y lineas
plot(residuals(mod9), type="l")
abline(h=0)


#Para detectar bucles este comadno ( en el tag es el salto que queremos)
#Estudias la incorrelación pero con un salto
Box.test(residuals(mod9), lag=5, type="Ljung-Box")
'''

Box-Ljung test

data:  residuals(mod9)
X-squared = 2.4517, df = 5, p-value = 0.7838

el valor de lag tienen en cuenta todos los saltos anteriores. No solo de 5 en  5. Así comprobamos que no haya bucles previos, hasta salto con 5
'''


residuos <- residuals(mod9)
ajustes <- fitted(mod9)
shapiro.test(residuos)
'''
Shapiro-Wilk normality test

data:  residuos
W = 0.94959, p-value = 0.0007793

Valor muy pequeño, luego rechazamos la normalidad. No tiene sentido hacer el test de barlett. Luego usariamos el de levene.test
'''
leveneTest(residuos, ajustes )#Esto sería para igualdad de varianza pero no se puede hacer al no poder hacer la linealidad
#eso es la homocedasticidad

'''
Levene s Test for Homogeneity of Variance (center = median)
Df F value Pr(>F)
group 99               
0               
Warning message:
  In leveneTest.default(residuos, ajustes) : ajustes coerced to factor

Si lo hacemos sobre el modelo1, Pr da 0.7062 y no rechazariamos
'''

#MODELO4
mod4 <- lm(pesor ~ sem+pesom+pesop+tallap)
summary(mod4)
anova(mod4)
plot(mod4)
pureErrorAnova(mod4)
residuos <- residuals(mod4)
ajustes = fitted(mod4)
'''
Hay incorrelación. No puedo hacer el error puro. Normalidad rechazada.No se puede hacer linealidad ni normalidad
levenne necesita linealidad

'''

#Vamos a hacer lo mismo con el 1, todo lo que hacemos son análiss de residuos.

#seria apartado c

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


# Practica3 -- 22/02/2019

#Ejercicio9

#d) Modelo cuatro es el nueve pero sin influir lo que no influye

library(fBasics)
normalTest(residuos,method="da")

summary(mod4)

#Solo queremos predecir una cosa

#nuevopunto

nuevopunto = data.frame(sem=c(39,38), pesom=c(62,60), pesop=c(78,90), tallap=c(170,160))


predict(mod4,nuevopunto)#Predecimos peso

predict(mod4, nuevopunto, interval= "confidence")
predict(mod4, nuevopunto, interval= "prediction",level = 0.95) #Predecimos apelo
bandasIC(mod4,nivel = 0.95) #Grafica, son las bandas verdes, dentro estará la media de todas con un
#95 por ciento de confi

#seleccionar modelo. Con el punto abajo coge todas las variables disponibles

#e)

library(leaps)
sel<-regsubsets(pesor~.,data=peso,nbest=1)#Guardamos aqui el mejor modelo de pesor con una
#variable, la que mejor lo aprox
regsubsets(pesor~.,data=peso,nbest=3)#Igual pero 3
summary(sel) #???Nos dice en que orden habría que añadir las variables para la mejor aprox.
plot(sel,scale="Cp") #Usamos criterio de Hocking Como p=8 +1 = 9, nuestro modelo es válido.

modtodas <- lm(pesor~.,data = peso)

library(MASS) #Para buscar el mejor modelo loco vas añadiendo y ves como se queda puta madre.
pasos <- step(modcte,scope = list(lower=modcte,upper=mod9),direction="forward",k=log(length(pesor)))
anova(pasos)
summary(pasos)


#Buscamos lo mismo de otra forma
paso <- stepAIC(modcte,scope = list(lower=modcte,upper=mod9),direction="forward",k=log(length(pesor)))

anova(paso)

#Esto no le salio.

stepwise(modtodas, direction="forward",criterion="BIC")
