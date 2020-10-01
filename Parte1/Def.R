#(P1)

#para evitar errores en datos(con arrays)
attach(peso)
#para mostrar datos(entre parentesis datos que queremos mostrar)
view(peso)
#representar peso recien nacido a traves semanas de gestacion
plot(sem,pesor)
#Modelo lineal con el que vamos a trabajar
modelo1 <- lm(pesor~sem)
modelo1
#aproximacion obtenida con coefs(representar el modelo en la grafica)
abline(modelo1,col="green")

#Si queremos saber algo sobre el modelo:

#acceder nombre vars(incluidas calculadas)
names(modelo1)
#idem con clases del modelo
attributes(modelo1) 
#si queremos que nos muestre algo de lo anterior escribimos posicion de var o nombre directamente-->ej coeficientes 
modelo1[[1]] 
modelo1$coefficients 

#con los siguientes comandos lo hacemos todo
#resumen de todo
summary(modelo1)#Influye la semana en la que ha nacido en el peso que tiene(coef alto)
#mostramos resultados como antes pero en forma suma de cuadrados vista en clase: SSt=SSe+SSr(contraste de forma conjunta)
anova(modelo1) #(como p-valor menor que 0.05-->distintas)
#intervalo de confianza(por defecto 95% de confianza, si queremos modificar con level)
confint(modelo1,level = 0.9) #(los intercept son las constantes)
#matriz de covarianzas(coeficiente)
vcov(modelo1)

#damos nombre a valores estimados(dos formas de hacerlo)
ygorro=modelo1$fitted.values
#ygorro=fitted(modelo1)
#idem con residuos
residuos=residuals(modelo1)

#extracto de observaciones(representar:filas 1-10 y todas cols)
cbind(pesor,sem,ygorro,residuos)[1:10,] #Juntamos todos los vectores en una matriz por columnas
#suma de residuos siempre da 0(se aproxima)
sum(residuos)
#comprobamos ortogonalidad(de semanas)-->se aproxima a cero,idem que residuos
sum(sem*residuos)

#dibujamos modelo completo
plot(modelo1)
#grafica Q-Q(normalidad)
qqnorm(modelo1)
qqline(modelo1)
#para dibujar todas las graficas juntas
plot(modelo1,which=1:6)


#(P2)
 
#apartado a ejercicio 9
#añadimos modelo cte y caracteristicas pedidas y los mostramos
#(el modelo solo tiene sentido donde tenemos el conjunto de variables (35-41 sem))
modcte <- lm(pesor~1) #no hace falta incluirlo porque ya viene incluido. Si queremos un modelo sin la constante, si tengo que quitárselo
mod9 <- lm(pesor~sem+pesom+tallam+pesop+tallap)
modcte
mod9
#resumen de estos dos modelos
summary(modcte)
summary(mod9) 
#En este caso tallam no sería significativo. p valor mayor de 0.05
#Coge la primera variable y las va incluyendo al modelo. Sumando todos los "value" tendríamos 
#la p-value ojo!!! se va añadiendo uno a uno. Por lo que el value se corresponde con lo que ese 
#parametro influye o no, aporta o no al modelo ya hecho con los parametros que estén antes
#La altura de la madre es la primera candidata a eliminar como variable no significativa
#(Esto se corresponde con el apartado a) del eje9)

anova(mod9)#como pvalor=0.67 tallam(altura madre) mayor que alfa a la hora de estimar los coefs entonces la eliminamos 
#Probabilidad de rechazar la hip nula sabiendo que esta es cierta con la muestra.
#pvalor tallam mayor de 0.05 con lo cual aceptamos l hip nula, la constante vale 0 y
#no es significativa

#comparanos modelo cte y el mod 9
anova(modcte,mod9)
#P-valor significantivo, medias distintas, rechazas la hip nula(medias iguales).
#El modelo con mas variables te explica mas cosas del modelo, proporciona mas inf.

#Apartado b 
#grafico de ambos modelos
plot(modcte) #Comprobamos como en 2.5 no podemos mirar el ajuste porque solo hay una variable con ese valor.
             #No puedo decir nada de la linealidad o dispersión a simple vista
             #Todas las observaciones parecen dentro del límite de apalancamiento. (El apalancamiento se 
             #corresponde con la fuerza delas variables para mover el modelo, es decir, para hacer que 
             #las variables predictoras influyan más o menos en el modelo)
plot(mod9)

#intentamos hacer contraste de igualdad de varianzas
#matriz de covarianzas
vcov(mod9)#La diagonal es la estimacion de la varianza de los beta gorro
          #Determina el factor de incremento de la varianza
          #Para cada var del modelo me da el grado de dependencia con respectoa la optextras::Si son ortogonales entre ellas, el valor va a ser uno.
          #Si no, nos da  la dependencia. Muy cercano a 1, más ortogonales
library(car)#tambien se puede cargar paquete
vif(mod9) #Si te da 1 son ortogonales, si esta entre 1-10 no da problemas.

'''
sem    pesom   tallam    pesop   tallap 
1.811617 3.623448 3.858555 1.713982 2.068638

La menos ortogonal es la altura d ela madre. 
Entre 1 y 10 se considera que no da errores. 
Si alguno fuera mayor a 10, tendríamos valores altos en la matriz de covarianzas, 
luego se comprobaria la imprecision y habría que eliminarlo del modelo
'''

#Volvemos a definir valores estimados y residuos
ygorro=modelo1$fitted.values 
residuos=residuals(mod9)

#contraste del error puro
library(alr3)
#hacemos tabla anova
pureErrorAnova(mod9) #no linealidad-->hay falta de ajuste(no hay error puro), ningun valor igual que X
pureErrorAnova(modelo1) #hay linealidad(no falta ajuste) 

'''
No me lo ha desglosado el mod9, luego no hay problema en el modelo
Si hacemos esto con el modelo1, nos lo desglosa y nos da Falta de ajuste jeje Lack of fit y Pure Error
Se comprueba como se rechaza la linealidad con el valor 0.001904 de Pr(>F)
'''

#(igualdad de correlacion de varianzas)
#En este caso la hipotesis nula es la incorrelacion
library(lmtest)
dwtest(mod9) #para ver incorrelacion de residuos(consecutivos) de modelo--> si la hay(pero dos a dos)
'''
	Durbin-Watson test

data:  mod9
DW = 1.9905, p-value = 0.4914
alternative hypothesis: true autocorrelation is greater than 0

Estadístico proximoa 2, luego modelo modelo bien ajustado. Si se acercara a 0 o 4, se rechazaría la incorrelación.
'''

#Autocorrelacion
plot(residuals(mod9),type="o") #tipo l para añadir lineas/ tipo o con circulitos en los puntos
abline(h=0)#añadiños eje horizontal

#Para detectar bucles
Box.test(residuals(mod9),lag=5,type="Ljung-Box") #aunque pongamos 15 en el salto(lag) sigue saliendo p valor para rechazar
'''

Box-Ljung test

data:  residuals(mod9)
X-squared = 2.4517, df = 5, p-value = 0.7838

el valor de lag tienen en cuenta todos los saltos anteriores. No solo de 5 en  5. Así comprobamos que no haya bucles previos, hasta salto con 5
'''

#normalidad(contraste)
shapiro.test(residuals(mod9)) #-->rechazamos normalidad
'''
Shapiro-Wilk normality test

data:  residuos
W = 0.94959, p-value = 0.0007793

Valor muy pequeño, luego rechazamos la normalidad. No tiene sentido hacer el test de barlett. Luego usariamos el de levene.test
'''

#igualdad de varianzas(residuos)
ajustes<-fitted(mod9)
residuos <- residuals(mod9)
leveneTest(residuals(mod9),ajustes) #da error porque no hay linealidad
#eso es la homocedasticidad


'''
Levenes Test for Homogeneity of Variance (center = median)
Df F value Pr(>F)
group 99               
0               
Warning message:
In leveneTest.default(residuos, ajustes) : ajustes coerced to factor

Si lo hacemos sobre el modelo1, Pr da 0.7062 y no rechazariamos
'''
'''
#idem con modelo del otro dia
#copiamos todo lo ultimo con modelo1
leveneTest(residuals(modelo1),fitted(modelo1)) #da error porque no hay linealidad
bartlett.test(residuals(modelo1),fitted(modelo1))

#cambiamos modelo(idem pero quitando la altura de la madre)
mod4 <- lm(pesor~sem+pesom+pesop+tallap)
mod4
#resumen de estos dos modelo
summary(mod4)
anova(mod4)
#grafico de ambos modelos
plot(mod4)
#intentamos hacer contraste de igualdad de varianzas
#matriz de covarianzas
vcov(mod4)
#determina cual es el factor de incremento de la varianza
library(car)#tambien se puede cargar paquete
vif(mod4)
#contraste del error puro
library(alr3)
pureErrorAnova(mod4) #no linealidad, ningun valor igual que X
#(igualdad de correlacion de varianzas)
library(lmtest)
dwtest(mod4) #para ver incorrelacion de residuos(consecutivos) de modelo--> si la hay(pero dos a dos)
plot(residuals(mod4),type="o") #tipo l para añadir lineas/ tipo o con circulitos en los puntos
abline(h=0)#añadiños eje horizontal
Box.test(residuals(mod4),lag=5,type="Ljung-Box") #aunque pongamos 15 en el salto sigue saliendo p valor para rechazar
#normalidad(contraste)
shapiro.test(residuals(mod4)) #-->rechazamos normalidad
#igualdad de varianzas(residuos)
ajustes<-fitted(mod4)
leveneTest(residuals(mod4),ajustes) #da error porque no hay linealidad
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

#(P3)

attach(peso)
data(peso)
View(peso)
mod9 <- lm(pesor ~ sem+pesom+tallam+pesop+tallap)
residuos <- residuals(mod9)
ajustes <- fitted(mod9)

modTODAS <- lm(pesor~.,data = peso) 
#hemos hecho todos los contrastes. Aplicamos el test de shapiro, leveneTest. Falta:
ks.Test(residuos, "pnorm")
library(fBasics)
normalTest(residuos, method = "da")

mod4 <- lm(pesor ~ sem+pesom+pesop+tallap)
summary((mod4))

predict(mod4)
'''
Nos dalos valores estimados con cada uno de los 100 valores de la muestra. Si 
queremos que nos de el intervalo de confianza o de predicción se lo tenemos que 
especificar en el argumento.
'''

nuevopunto <- data.frame(sem=39, pesom=62, pesop=78, tallap=170)
predict(mod4, nuevopunto)

"Tenemos el primer individuo especificado, vamos a por el segundo. Para ello podemos estimar dos al mismo tiempo de la siguiente forma"

nuevopunto=data.frame(
  sem=c(39,38), pesom=c(62,60), pesop=c(78,90), tallap=c(170,160))

predict(mod4,nuevopunto) #con esto estamos prediciendo peso

'''
Si quetremos añadir intervalo de confianza o predicción, lo añadimos como argumento de la siguiente forma:
'''

predict(mod4, nuevopunto, interval="confidence")
'''
fit      lwr      upr
1 3.400720 3.340349 3.461091
2 3.239583 3.136257 3.342909

El intervalo de confianza es centrado. lwr es el límite inferior y upr es el limite superior.
COn un 95% de garantia de acierto en ese rango está el nuevo valor predicho para el peso del recién nacido
El peso medio de todos los recién nacidos con las carácterísticas especificadas en los puntos, se encuentra en ese rango.
Si quiero un valor en concreto debo cambiar confidence a predicción
'''

predict(mod4, nuevopunto, interval="prediction", level=0.95)

'''
fit      lwr      upr
1 3.400720 3.037778 3.763662
2 3.239583 2.867080 3.612086

Ahora tengo el valor estimado pero el intervalo de confianza.Este es mayor ya que estamos prediciendo el intervalo de confianza 
para ese individuo que tiene esas características, con un 95%
'''

#Función bandasIC() :Hace una predicción sobre los valores ajustados y la variable respuesta
bandasIC(mod4,nivel=0.97)
'''
Vemos como las lineas corresponden al hiperplano. Las verdes son el intervalo de confianza. 
Conforme estamos más en el centro la banda de confianza es menor, luego es más preciso.
Si esta banda estimo el 95%, veo como no quedan dentro. Pero esta banda es la media. 
La banda zul se corresponde a la predicción. Sería correcta la imagen solo con una variable x. 
Esto no es correcto porque es una supersuperficie
'''

'''
¿Como seleccionar el modelo de regresion? PAra seleccionar los mejores subconjuntos usamos lo siguiente:
'''

library(leaps)
#nbest es el numero de mejores regresiones que queremos con esa cantidad de variables
regsubsets(pesor~sem+pesom+pesop+tallap, data=peso,nbest=3)
'''
Subset selection object
Call: regsubsets.formula(pesor ~ sem + pesom + pesop + tallap, data = peso, 
nbest = 3)
4 Variables  (and intercept)
Forced in Forced out
sem        FALSE      FALSE
pesom      FALSE      FALSE
pesop      FALSE      FALSE
tallap     FALSE      FALSE
3 subsets of each size up to 4
Selection Algorithm: exhaustive
'''
#Lo hacemos para todas:
regsubsets(pesor~., data=peso,nbest=3)
'''
Subset selection object
Call: regsubsets.formula(pesor ~ ., data = peso, nbest = 3)

8 Variables  (and intercept)
Forced in Forced out
edadm      FALSE      FALSE
pesom      FALSE      FALSE
tallam     FALSE      FALSE
sem        FALSE      FALSE
ingr       FALSE      FALSE
pesop      FALSE      FALSE
tallap     FALSE      FALSE
tabaco     FALSE      FALSE
3 subsets of each size up to 8
Selection Algorithm: exhaustive
'''
sel <- regsubsets(pesor~., data=peso,nbest=3)
#Guardamos aqui el mejor modelo de pesor con tres
#variables(nbest), la que mejor lo aprox
summary(sel)#???Nos dice en que orden habría que añadir las variables para la mejor aprox.
'''
Subset selection object
Call: regsubsets.formula(pesor ~ ., data = peso, nbest = 3)
8 Variables  (and intercept)
Forced in Forced out
edadm      FALSE      FALSE
pesom      FALSE      FALSE
tallam     FALSE      FALSE
sem        FALSE      FALSE
ingr       FALSE      FALSE
pesop      FALSE      FALSE
tallap     FALSE      FALSE
tabaco     FALSE      FALSE
3 subsets of each size up to 8
Selection Algorithm: exhaustive
edadm pesom tallam sem ingr pesop tallap tabaco
1  ( 1 ) " "   " "   " "    "*" " "  " "   " "    " "   
1  ( 2 ) " "   " "   " "    " " " "  " "   "*"    " "   
1  ( 3 ) " "   "*"   " "    " " " "  " "   " "    " "   
2  ( 1 ) " "   "*"   " "    "*" " "  " "   " "    " "   
2  ( 2 ) " "   " "   " "    "*" " "  "*"   " "    " "   
2  ( 3 ) " "   " "   " "    "*" "*"  " "   " "    " "   
3  ( 1 ) " "   "*"   " "    "*" " "  "*"   " "    " "   
3  ( 2 ) " "   "*"   " "    "*" "*"  " "   " "    " "   
3  ( 3 ) " "   "*"   " "    "*" " "  " "   "*"    " "   
4  ( 1 ) " "   "*"   " "    "*" "*"  "*"   " "    " "   
4  ( 2 ) " "   "*"   " "    "*" "*"  " "   "*"    " "   
4  ( 3 ) " "   "*"   " "    "*" " "  "*"   "*"    " "   
5  ( 1 ) " "   "*"   " "    "*" "*"  "*"   "*"    " "   
5  ( 2 ) " "   "*"   " "    "*" "*"  "*"   " "    "*"   
5  ( 3 ) " "   "*"   "*"    "*" "*"  "*"   " "    " "   
6  ( 1 ) " "   "*"   " "    "*" "*"  "*"   "*"    "*"   
6  ( 2 ) " "   "*"   "*"    "*" "*"  "*"   "*"    " "   
6  ( 3 ) " "   "*"   "*"    "*" "*"  "*"   " "    "*"   
7  ( 1 ) " "   "*"   "*"    "*" "*"  "*"   "*"    "*"   
7  ( 2 ) "*"   "*"   " "    "*" "*"  "*"   "*"    "*"   
7  ( 3 ) "*"   "*"   "*"    "*" "*"  "*"   "*"    " "   
8  ( 1 ) "*"   "*"   "*"    "*" "*"  "*"   "*"    "*"   

Para cada número de variables nos da los tres mejores modelos. El * es el atributo con el que se queda
Vemos como el mejor de 4 no e sel que nosotros habíamos obtenido.
'''

plot(sel,scale="Cp")

'''
Este gráfico nos muestra el valor de Cp. Si solo usa una variable nos da un Cp muy alto
'''
sel <- regsubsets(pesor~., data=peso,nbest=1)
plot(sel,scale="Cp")  #Usamos criterio de Hocking Como p=8 +1 = 9, nuestro modelo es válido.

'''
Regresion simple es muy malo. El cp es un estadistico que tiene como valor medio el 
numero de variables del modelo. 
Nos interesa coger un modelo para predecir en el que se incluyan variables no significativas pero prediga bien.
En el gráfico se observa con 9 el modelo es mejor, aunque incluya variables no significativas.
'''
#PAra estimar si es bueno mi modelo usamos:

library(MASS) #Para buscar el mejor modelo vamos añadiendo y vemos como se queda

'''
Cogemos el modelo cte como modelo de partida
'''
modcte <- lm(pesor~1)
step(modcte, scope = list(lower=modcte, upper=mod9), direction="forward")

'''
El modelo cont tiene ASI -179. Queremos mejorarlo.
Si hacemos esto con el modelo con todas las variables obtenemos queempieza en -179. Y 
nos dice que si incvluyo una, lo que ocurre. Y así va una a una.
Selecciona la mejor, la incluye y vuelve a comprpobar cual es la lmejor de las restantes. Y así con todas.
Incluye la que tenga mayor AIC.
Llegamos hasta obtener un modelo dejando fuera la edad.
'''
#Ahora probamos hacia atras:
step(modcte, scope = list(lower=modTODAS, upper=modcte), direction="backward")
'''
Vemos como quita la edad y llega al mismo resultado. En ambos sentidos:
'''
step(modcte, scope = list(lower=modTODAS, upper=modcte), direction="both")
'''
Obtenemos el mismo resultado. Si guardamos uno y 
'''

paso <- step(modcte, scope = list(lower=modTODAS, upper=modcte), direction="both")
anova(paso)
'''

'''
paso <- step(modcte, scope = list(lower=modTODAS, upper=modcte), direction="forward", k=log(length(pesor)))
summary(paso)
'''
Nos da 4 variables como variables finales. Pero hemos visto como con 4 el Cp no era válido para poder predecir. 
Luego hemos de usar 5 variables
'''

stepAIC(modcte, scope = list(lower=modcte, upper=mod9), direction="forward", k=log(length(pesor)))
'''
Start:  AIC=-176.5
pesor ~ 1

Df Sum of Sq     RSS     AIC
+ sem     1   10.4743  5.8732 -274.27
+ tallap  1    7.9127  8.4348 -238.07
+ pesom   1    7.8997  8.4478 -237.92
+ pesop   1    7.3878  8.9597 -232.03
+ tallam  1    6.7236  9.6239 -224.88
<none>                16.3475 -176.50

Step:  AIC=-274.27
pesor ~ sem

Df Sum of Sq    RSS     AIC
+ pesom   1    1.9690 3.9043 -310.50
+ pesop   1    1.4380 4.4353 -297.74
+ tallam  1    1.1366 4.7367 -291.17
+ tallap  1    1.1007 4.7725 -290.42
<none>                5.8732 -274.27

Step:  AIC=-310.49
pesor ~ sem + pesom

Df Sum of Sq    RSS     AIC
+ pesop   1   0.64822 3.2560 -324.05
+ tallap  1   0.42891 3.4753 -317.53
<none>                3.9043 -310.49
+ tallam  1   0.00569 3.8986 -306.04

Step:  AIC=-324.05
pesor ~ sem + pesom + pesop

Df Sum of Sq    RSS     AIC
+ tallap  1  0.168704 3.0873 -324.76
<none>                3.2560 -324.05
+ tallam  1  0.015613 3.2404 -319.92

Step:  AIC=-324.76
pesor ~ sem + pesom + pesop + tallap

Df Sum of Sq    RSS     AIC
<none>                3.0873 -324.76
+ tallam  1  0.045391 3.0419 -321.64

Call:
lm(formula = pesor ~ sem + pesom + pesop + tallap)

Coefficients:
(Intercept)          sem        pesom        pesop       tallap  
-5.256121     0.152280     0.015861     0.007529     0.006749  



'''
#Comparación modelo con variables:
paso <-  stepAIC(modcte, scope = list(lower=modcte, upper=modTODAS), direction="forward", k=log(length(pesor)))
anova(paso)

#Para comparar entre los distintos modelos
library(alr3)
stepwise(modTODAS, direction="forward", criterion="BIC")
