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

s = S/sqrt(n) # desvio del estimador
# resolucion

#1. Hipotesis
#Ho: x <= Mu # el estimador es igual o menor a Mu
#H1: x > Mu # el estimador es mayor que Mu

#2. Estadistico Z (conozco, Mu y S de la problacion)
# Zempirico = (x - Mu) / s

#3. Region Critica o zona de rechazo
#Zc para 1-alpha

zc = qnorm(1-alpha) #entro con la probabildad a buscar el alpha.
# zc = 1.6448
# Region de rechazo todos los Z mayores a 1.6448 (zc)


# 4. Regla de decisión
# Si Zempirico >= Zcritico => Rechazo H0
# Si Zempirico >= 1.6448 => Rechazo H0
# Si Zempirico < 1.6448 => No hay evidencia empirica para Rechazar H0.

#5 Calcular Zempirico

ze = (x-Mu)/s
# ze = 6.89

# 6 Aplico la Regla
ze >= zc
# True, Rechazo H0.

# Accion derivada
#Bajo la evidencia empirica se puede afirmar que X aumentó.



