#para evitar errores en datos
attach(peso)
#para msotrar datos
data(peso)
#ver var peso
view(peso)
#dibujar peso recien nacido y le ponemos nombre(relacion por semanas),lm->modelo lineal
plot(sem,pesor)
modelo1 <- lm(pesor~sem)
modelo1
#aproximacion obtenida con coefs
abline(modelo1,col="green")
#resumen de todo
summary(modelo1)
#acceder nombre vars(incluidas calculadas)
attributes(modelo1)
#acceder a uno de esosatributos(inf-cols y filas)
modelo1[[1]]
#idem pero directamente poniendo a lo que queremso acceder
modelo1$coefficients
#mostramos resultados como antes pero en forma suma de cuadrados vista en clase: SSt=SSe+SSr(contraste de forma conjunta)
anova(modelo1)
#intervalo de confianza(por defecto 95% de confianza, si queremos modificar con level)
confint(modelo1,level = 0.9)
#matriz de covarianzas
vcov(modelo1)
#damos nombre a valores estimados(dos formas de hacerlo)
ygorro=modelo1$fitted.values
#ygorro=fitted(modelo1)
residuos=residuals(modelo1)
#extracto de observaciones(representar:filas 1-10 y todas cols)
cbind(pesor,sem,ygorro,residuos)[1:10,]
#suma de residuos siempre da 0
sum(residuos)
#comprobamos ortogonalidad
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
modcte <- lm(pesor~1)
mod9 <- lm(pesor~sem+pesom+tallam+pesop+tallap)
modcte
mod9
#resumen de estos dos modelo
summary(modcte)
summary(mod9)
anova(mod9)
#como pvalor=0.67 tallam(altura madre) mayor que alfa a la hora de estimar los coefs entonces la eliminamos 
#comparanos modelo cte y el mod 9
anova(modcte,mod9)
#grafico de ambos modelos
plot(modcte)
plot(mod9)
#intentamos hacer contraste de igualdad de varianzas
#matriz de covarianzas
vcov(mod9)
#determina cual es el factor de incremento de la varianza
library(car)#tambien se puede cargar paquete
vif(mod9)
#contraste del error puro
library(alr3)
pureErrorAnova(mod9) #no linealidad, ningun valor igual que X
pureErrorAnova(modelo1) #linealidad
#(igualdad de correlacion de varianzas)
library(lmtest)
dwtest(mod9) #para ver incorrelacion de residuos(consecutivos) de modelo--> si la hay(pero dos a dos)
plot(residuals(mod9),type="o") #tipo l para añadir lineas/ tipo o con circulitos en los puntos
abline(h=0)#añadiños eje horizontal
Box.test(residuals(mod9),lag=5,type="Ljung-Box") #aunque pongamos 15 en el salto sigue saliendo p valor para rechazar
#normalidad(contraste)
shapiro.test(residuals(mod9)) #-->rechazamos normalidad
#igualdad de varianzas(residuos)
ajustes<-fitted(mod9)
leveneTest(residuals(mod9),ajustes) #da error porque no hay linealidad

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

