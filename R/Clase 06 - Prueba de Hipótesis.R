##########################################################
#                   ESTADISTICA                          #
#               Prof. DEL ROSSO - LEVINIS                #
#	            MAESTRIA EN CIENCIA DE DATOS               #
#                FACULTAD DE INGENIERIA                  #
#                 UNIVERSIDAD AUSTRAL                    #
##########################################################

## Asignatura: ESTAD�STICA
## Docentes: Rodrigo Del Rosso - Gustavo Levinis

rm(list = ls())

## CARGAR PAQUETES ## 
library(PASWR2)
library(dplyr)
library(psych)

## RUTA DE TRABAJO ##

path = "...."
setwd(path)

#######################################
####### CONTRASTES DE HIP�TESIS #######
#######################################

# Para la media de una poblaci�n normal

# Generamos una muestra de tama�o 30 de una distribuci�n normal con media 5 y desv�o 1. 

set.seed(34)                           # Fijamos la semilla
x <- rnorm(n = 30,mean = 5,sd = 1)      # Generamos una normal con media 5 y desv�o 1

t.test(x,mu = 5,conf.level = 0.95)

## LA FORMA DE CALCULAR EL P-VALUE
pt(-0.092535,df = 29) + pt(-0.092535, df = 29, lower.tail = T)

# Podemos hacerlo con m�s argumentos

t.test(x, alternative = "two.sided",mu = 5,conf.level = 0.95)

# Si planteamos un test unilateral a izquierda

t.test(x, alternative = "less",mu = 5,conf.level = 0.95)
pt(-0.092535,df = 29) ## LA FORMA DE CALCULAR EL P-VALUE

t.test(x, alternative = "greater",mu = 5,conf.level = 0.95)
pt(0.092535, df = 29, lower.tail = T) ## LA FORMA DE CALCULAR EL P-VALUE

# Si planteamos un test unilateral a derecha para mu = 4 y nivel de confianza 0.99

t.test(x, alternative = "greater",mu = 4,conf.level = 0.99)

# Para comparar la media de dos poblaciones normales independientes

# Por default es un test bilateral, y  un intervalo de confianza del 95%, diferencia 0 y varianzas distintas.
# Asumiendo varianzas iguales
# Por ejemplo, generamos otra muestra de tama�o 25 de una distribuci�n normal con media 6 y desv�o 1. 

nx = 100
ny = 200
# set.seed(35)

x <- rnorm(nx,5,2)
y <- rnorm(ny,6,1)

t.test(x,y,var.equal = TRUE)

#Si queremos ver si la media de x es menor en una unidad que la media de y

t.test(x,y,mu = 1,var.equal = TRUE)

# Cuando la variable de inter�s est� definida mediante un factor

# Generamos el data frame datos 

set.seed(20)
variable <- rnorm(40,20,2)

set.seed(20)
sexo <- sample(c("F","M"),40,replace=TRUE)

datos <- data.frame(variable,sexo)

t.test(variable ~ sexo, data = datos,var.equal=TRUE)

# Asumiendo varianzas distintas (Test de Welch)

set.seed(29)
x1<-rnorm(25,5,0.5)

set.seed(30)
x2<-rnorm(20,5.2,1)

t.test(x1,x2)

# Para muestras apareadas

set.seed(29)
w1<-rnorm(25,5,0.5)

set.seed(30)
w2<-rnorm(25,3,1)

t.test(w1,w2,paired = TRUE)  ## con la instrucci�n paired igual TRUE le indic� que las muestras son apareadas

## cor(w1,w2)
## cor.test(w1,w2)

# Test F para igualdad de varianzas
# La instrucci�n en R para aplicar el test F es  var.test
# El default es un test bilateral para cociente 1  y  un intervalo de confianza del 95%
# Chequeamos el supuesto para los test realizados anteriormente.

var.test(x,y)

var.test(x1,x2)

var.test(variable ~ sexo, data=datos)  ### el s�mbolo para categorizar "variable" por "sexo"

#Ver el help para otras opciones

#Test de Shapiro Wilks 
#Pone a prueba el supuesto de normalidad.
#Para los ejemplos vistos,

w = var.test(x1,x2)
w$p.value
w$statistic

tabla = cbind(w$p.value,w$statistic)
colnames(tabla) = c("P-Value", "Estad�stico")

# Ejemplo 1
shapiro.test(x)
library(nortest)
ad.test(x)
library(tseries)
jarque.bera.test(x)

library(lessR)
Histogram(x, breaks = 30, density = T)

boxplot(x)

qqnorm(x)
qqline(x)

# Ejemplo 13
shapiro.test(y)

# Ejemplo 14
#Para muestras apareadas la diferencia debe tener distribuci�n normal (el supuesto es que la diferencia tiene distribuci�n normal)

shapiro.test(w1 - w2)

#Ejemplo 15, los grupos est�n dados por un un factor
tapply(datos$variable,datos$sexo,shapiro.test)

# Ejemplo "Materiales por Regi�n"
Materiales <- read.csv2("Materiales por regi�n.csv",header=T)
edit(Materiales)
Regi�n <- Materiales$Regi�n
Precio <- Materiales$Precio
tapply(Precio,Regi�n,shapiro.test)
var.test(Precio ~ Regi�n)

# Aplicaci�n del Test t de Student
t.test(Precio ~ Regi�n,var.equal=TRUE)

Street <- read.csv2("Street.csv",header = T)
names(Street) = c("Empresa","Real","Estimacion")
Real <- as.numeric(Street$Real)
Est <- as.numeric(Street$Estimacion)

shapiro.test(Real - Est)
t.test(Real,Est,paired = TRUE)

?prop.test  ## para igualdad de proporciones (es el asint�tico)
?binom.test ## es para proporciones pero es exacto a diferencia del anterior
?wilcox.test
?kruskal.test
?ts

## DATOS ##

ruta <- 'https://raw.githubusercontent.com/dhairavc/MSDS2019_RBridge/master/Wages1.csv'
wages <- read.csv(ruta)
head(wages)

## id que es la identificaci�n del individuo
## exper que representa la experiencia en a�os
## sex es la variable dummy de sexo (1 = hombre, 0 = mujer)
## school que representa la escolaridad en a�os
## wage que corresponde al salario por hora (en d�lares)

## AN�LISIS EXPLORATORIO DE LOS DATOS ##
str(wages)

## INGENIER�A DE ATRIBUTO ##

## Recodificaci�n
wages$sex <- ifelse(wages$sex == "female",0,1)
head(wages)

attach(wages)

## GR�FICOS ##

boxplot(exper ~ sex, 
        xlab = "Sexo", 
        ylab = "Experiencia (a�os)", 
        names = c("Mujer", "Hombre"), 
        main = "Experiencia seg�n el sexo")

boxplot(school ~ sex, 
        xlab = "Sexo", 
        ylab = "Escolaridad (a�os)", 
        names = c("Mujer", "Hombre"), 
        main = "Escolaridad seg�n el sexo")

boxplot(wage ~ sex, 
        xlab = "Sexo", 
        ylab = "Salario (USD por hora)", 
        names = c("Mujer", "Hombre"), 
        main = "Salario seg�n el sexo")

## EDA al atributo wage
eda(wage)

## EDA al atributo wage de las Mujeres
eda(wage[sex==0])

## EDA al atributo wage de los Hombres
eda(wage[sex==1])


wages %>% 
  group_by(sex) %>% 
  summarise(mean(exper), 
            mean(school), 
            mean(wage),
            median(exper), 
            median(school), 
            median(wage), 
            sd(exper), 
            sd(school), 
            sd(wage))

lapply(split(wages$exper,wages$sex),shapiro.test)
lapply(split(wages$school,wages$sex),shapiro.test)
lapply(split(wages$wage,wages$sex),shapiro.test)

describe(wages)

## CONTRASTE DE HIPOTESIS ##

# A DOS COLAS (BILATERAL)
t.test(wage, mu = 5.76)

t.test(wage, mu = 7)

# A LA COLA DERECHA (UNILATERAL)
t.test(wage, mu = 7, alternative = "greater")

# A LA COLA IZQUIERDA (UNILATERAL)
t.test(wage, mu = 7, alternative = "less")

# A DOS COLAS (BILATERAL)
t.test(wage[sex == 0], mu = 6)

# A LA COLA DERECHA (UNILATERAL)
t.test(wage[sex==0], mu = 6, alternative = "greater")

# A LA COLA IZQUIERDA (UNILATERAL)
t.test(wage[sex==0], mu=6, alternative = "less")

# A DOS COLAS (BILATERAL) (SIEMPRE)
var.test(wage ~ sex)

# A DOS COLAS (BILATERAL) (VARIANZAS DISTINTAS)
t.test(wage~sex, var.equal = FALSE)

t.test(wage~sex, var.equal = FALSE, alternative = "greater")

t.test(wage~sex, var.equal = FALSE, alternative = "less")

## Observaci�n: 
## Debido a que el supuesto de normalidad no se cumple, 
## el enfoque param�trico no es el ideal. 
## En cambio, se sugiere el uso del enfoque no param�trico.

wilcox.test(wage ~ sex)
wilcox.test(wage ~ sex, alternative = "greater")
wilcox.test(wage ~ sex, alternative = "less")
