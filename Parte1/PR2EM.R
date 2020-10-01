#Práctica 2 15/02/2019

#Abrir los ejemplos loco que se te olvida.

attach(peso)
data(peso)
View(peso)

plot(sem,pesor)

#el modelo solo tiene sentido donde tenemos el conjunto de variables (35-41 sem)
modcte <- lm(pesor~1)#Este es el modelo constante (no hace falta incluirlo porque ya viene incluido. Si queremos un moedelo sin la constante, si tengo que quitárselo)
mod9 <- lm(pesor ~ sem+pesom+tallam+pesop+tallap)
mod9

'''
Coefficients:
(Intercept)          sem        pesom       tallam        pesop       tallap  
-4.639577     0.154665     0.019925    -0.006510     0.007503     0.007477
'''

abline(mod9,col="red")

summary(modcte)
'''
Residuals:
   Min     1Q Median     3Q    Max 
-0.785 -0.210 -0.085  0.215  1.115 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  3.28500    0.04064   80.84   <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.4064 on 99 degrees of freedom
'''

summary(mod9)
'''
Residuals:
     Min       1Q   Median       3Q      Max 
-0.50224 -0.09215 -0.02160  0.08640  0.60839 

Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept) -4.639577   0.803719  -5.773 9.99e-08 ***
sem          0.154665   0.019793   7.814 7.83e-12 ***
pesom        0.019925   0.004506   4.422 2.62e-05 ***
tallam      -0.006510   0.005496  -1.184 0.239266    
pesop        0.007503   0.002175   3.451 0.000839 ***
tallap       0.007477   0.003019   2.477 0.015053 *  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.1799 on 94 degrees of freedom
Multiple R-squared:  0.8139,	Adjusted R-squared:  0.804 
F-statistic: 82.23 on 5 and 94 DF,  p-value: < 2.2e-16

#Coge la primera variable y las va incluyendo al modelo. Sumando todos los "value" tendríamos 
la p-value ojo!!! se va añadiendo uno a uno. Por lo que el value se corresponde con lo que ese 
parametro influye o no, aporta o no al modelo ya hecho con los parametros que estén antes
#La altura de la madre es la primera candidata a eliminar como variable no significativa
(Esto se corresponde con el apartado a) del eje9)

'''



anova(mod9)
#SSt=SSe+SSr
#En este caso SSe=10.4743, SSr=5.8732
# SSt tiene n grados de libertad, SSe tiene 1 y SSr es n-2, en este caso 98
#SSr/(grados de libertad de SSr) estimacion de sigma^2 y 
#Residual standard error: 0.2448 estimacion de sigma



#Comparamos ambos modelos
anova(modcte,mod9)
'''

Model 1: pesor ~ 1
Model 2: pesor ~ sem + pesom + tallam + pesop + tallap
  Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
1     99 16.3475                                  
2     94  3.0419  5    13.306 82.232 < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
'''




'''
b)Análisis  gráfico de los residuos
'''



plot(modcte)
plot(mod9)
'''
Comprobamos como en 2.5 no podemos mirar el ajuste porque solo hay una variable con ese valor.
No puedo decir nada de la linealidad o dispersión a simple vista
Todas las observaciones parecen dentro del límite de apalancamiento. (El apalancamiento se 
corresponde con la fuerza d elas variables para mover el modelo, es decir, para hacer que 
las variables predictoras influyan más o menos en el modelo)
'''




confint(mod9,level=0.9)

#matriz de covarianza 
vcov(mod9)
'''
   (Intercept)           sem         pesom        tallam         pesop        tallap
(Intercept)  6.459636e-01 -8.449758e-03  2.139768e-03 -2.861301e-03  3.875193e-04 -6.670973e-05
sem         -8.449758e-03  3.917787e-04 -5.023005e-06 -1.106942e-05 -8.638081e-06 -2.092390e-05
pesom        2.139768e-03 -5.023005e-06  2.030084e-05 -1.886303e-05 -1.447244e-06  3.227229e-07
tallam      -2.861301e-03 -1.106942e-05 -1.886303e-05  3.020987e-05  1.200463e-07 -3.378860e-06
pesop        3.875193e-04 -8.638081e-06 -1.447244e-06  1.200463e-07  4.728511e-06 -2.148899e-06
tallap      -6.670973e-05 -2.092390e-05  3.227229e-07 -3.378860e-06 -2.148899e-06  9.114433e-06

La diagonal es la estimacion de la varianza de los beta gorro
'''

#DEtermina el factor de incremento de la varianza
#Para cada vaariable del modelo me da el grado de dependencia con respectoa la optextras::Si son ortogonales entre ellas, el valor va a ser uno.
#Si no, nos da  la dependencia. Muy cercano a 1, más ortogonales.

library(car)
vif(mod9)
vif(mod9)
'''
sem    pesom   tallam    pesop   tallap 
1.811617 3.623448 3.858555 1.713982 2.068638

La menos ortogonal es la altura d ela madre. 
Entre 1 y 10 se considera que no da errores. 
SI alguno sería mayor a 01, tendríamo valores altos en la matriz de covarianza, 
luego se comprobaria la impresicion y habría que eliminarlo del modelo
'''

plot(mod9,which=1:6)

ygorro=modelo1$fitted.values
#ygorro=fitted(modelo1)

residuos=residuals(mod9)
#residuos=modelo1&residuals


#Para hacer el contraste del error puro
library(alr3)
pureErrorAnova(mod9)
'''

Response: pesor
          Df  Sum Sq Mean Sq  F value    Pr(>F)    
sem        1 10.4743 10.4743 323.6690 < 2.2e-16 ***
pesom      1  1.9690  1.9690  60.8444 8.361e-12 ***
tallam     1  0.0057  0.0057   0.1759   0.67586    
pesop      1  0.6581  0.6581  20.3376 1.872e-05 ***
tallap     1  0.1985  0.1985   6.1334   0.01505 *  
Residuals 94  3.0419  0.0324                       
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

No me lo ha desglosado, luego no hay problema en el modelo
Si hacemos esto con el modelo1, nos lo desglosa y nos da Falta de ajuste jeje Lack of fit y Pure Error
Se comprueba como se rechaza la linealidad con el valor 0.001904 de Pr(>F)
'''


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
Box.test(mod9, lag=5, type="Ljung-Box")
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

Valor muy pequeño, luego rechazamos la normalidad. No tiene sentido hacer el test de valdes. Luego usariamos el de levene.test
'''

leveneTest(residuos, ajustes )#Esto sería para igualdad de varianza pero no se puede hacer al no poder hacer la linealidad

'''
'''
Levene's Test for Homogeneity of Variance (center = median)
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


