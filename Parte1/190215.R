#para evitar errores en datos
attach(peso)
#para mostrar datos
data(peso)
#ver var peso
View(peso)
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
#acceder a uno de esos atributos(inf-cols y filas)
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

#15 Febrero
modcte <- lm(pesor ~ 1)
mod9 <- lm(pesor ~ sem+pesom+tallam+pesop+tallap)

modcte
mod9

summary(modcte)
summary(mod9)
anova(mod9)
anova(modcte,mod9)

plot(modcte)

plot(mod9)
vcov(mod9)

library(car)
vif(mod9) #Determina el factor de incremento de la varianza


library(alr3)
pureErrorAnova(mod9)
pureErrorAnova(mod9)
library(lmtest)
dwtest(mod9)

residuos <- residuals(mod9)
ajustes <- fitted(mod9)
plot(residuals(mod9),type="o")
abline(h=0)
Box.test(mod9, lag=15, type="Ljung-Box")

shapiro.test(residuos) 

#Para el mod9 no tiene sentido, ya que tampoco se podia hacer la linealidad
leveneTest(residuos,ajustes)

