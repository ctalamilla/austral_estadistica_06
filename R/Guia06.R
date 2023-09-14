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
#z-critico para 1-alpha

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


#Ejercicio 2
# El encargado de personal ha informado que el nivel medio de ausencia de los
# empleados durante el trimestre pasado fue inferior a 15 días. Para ello se
# seleccionó una muestra de 50 empleados, quienes tuvieron un promedio de
# ausencias de 12,2 días y un desvío de 6,2 días. Se supone que la población se
# distribuye normalmente.
# Con un nivel de significación del 5%, determine si el informe del encargado puede
# considerarse válido.

n = 50
x = 12.2
s = 6.2
grados_libertad = n-1

# Hipostesis
#Ho: x <= 15 dias # el estimador es igual o menor a 15
#H1: x > 15 dias # el estimador es mayor 15

#2. Estadistico t (solo se tienen datos de la muestra)
# t-empirico = (x - Mu) / s/Sqrt(n)

#3. Region Critica o zona de rechazo
#t-critico para 1-alpha

# se usa alpha por que H0 es a cola izquierda "menor que". 
tc = qt(alpha, df=grados_libertad) #entro con la probabildad a buscar el valor de t.
tc
# tc = -1.67
# Region de rechazo todos los t mayores a 1.6765 (tc)


# 4. Regla de decisión
# Si tempirico <= tcritico => Rechazo H0
# Si tempirico <= -1.67 => Rechazo H0
# Si tempirico > -1.67 => No hay evidencia empirica para Rechazar H0.

#5 Calcular Zempirico

te = (x-15)/(s/sqrt(n))
# te = -3.19

# 6 Aplico la Regla
te <= tc
# True, Rechazo H0.

# Accion derivada
#Bajo la evidencia empirica se puede afirmar que el informe no es correcto.


