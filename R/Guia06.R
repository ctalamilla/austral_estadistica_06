#Ejercicio n° 1

# Se pudo comprobar que el año pasado, los precios de una determinada canasta de
# productos se distribuye normalmente con media $1780, y un desvío estándar de
# $110. Este año, una muestra de 40 ventas, proporcionó un precio promedio de
# $1900. Con un nivel de significación del 5%, ¿se puede afirmar que el precio
# promedio de estos productos, ha aumentado, con respecto al precio promedio del  
# año pasado?

#Datos
#Precios ─ N (1780, 110)
Mu = 1780
S = 110

n = 40
x = 1900

alpha = 0.05

# resolucion
s = S/sqrt(n)
#1. Hipotesis
#Ho: x <= Mu 
#H1: x > Mu

#2. Estadistico Z (conozco, Mu y S de la problacion)
# Ze = (x - Mu) / s

#3. Valor Critico, Zc para 1-alpha

Zc = qnorm(1-alpha) #entro con la probabildad a buscar el alpha.
# 1.6448
vc = Zc*S + Mu
#1960.934

