#Ejercicio10
library(car)
library(fBasics)
library(alr3)
library(lmtest)
library(leaps)
library(MASS)

attach(pulso)

#a)
names(pulso)
incremento <- pulso$pulse2-pulso$pulse1

tabla = data.frame(pulso,incremento)

#b)
mod10 <- lm(incremento ~ ran + smokes + sex+ activity)
mod10
summary(mod10)

#c)
mod <- lm(incremento ~ ran + sex)
mod
summary(mod)

anova(mod)
modcte <- lm(incremento~1)
anova(modcte,mod)

plot(residuals(mod), type="o")
abline(h=0,col="red")

plot(mod)
