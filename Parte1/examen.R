library(car)
library(fBasics)
library(alr3)
library(lmtest)
library(leaps)
library(MASS)
library(lmtest)
attach(Mandible)
View(Mandible)
names(Mandible)
mod <- lm(length ~ age)

mod
summary(mod)
plot(age, length)
modcte <- lm(length~1)
abline(mod,col="red")
anova(mod)

anova(modcte,mod)

plot(mod)
pureErrorAnova(mod)
dwtest(mod2)
residuos <- residuals(mod2)
ajustes <- fitted(mod2)

shapiro.test(residuos) #En el examen esperemos que salga normal.
Box.test(residuos, lag=5, type="Ljung-Box") #Son incorreladas, este lo coge intervalo 5
leveneTest(residuos, ajustes )

nuevopunto = data.frame(age=c(32))


predict(mod2,nuevopunto)#Predecimos peso

predict(mod, nuevopunto, interval= "prediction",level = 0.90) #Predecimos apelo

bandasIC(mod2,nivel = 0.90)

age
age2 <- age^2
mod2 <- lm(length ~ age + age2)
mod2
summary(mod2)
abline(mod,col="green")
plot(length, age)
pureErrorAnova(mod2)

predict(mod, nuevopunto, interval= "confidence")

plot(residuals(mod), type="o")
abline(h=0,col="red")
