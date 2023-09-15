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
alpha = 0.05

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


#Ejercicio 4 

# Con una muestra de 26 unidades que arrojó una media de 120 y una varianza de 25, se ha afirmado que la media poblacional tiene un valor menor a 118 con un nivel de significación del 1%. Suponiendo que la población se distribuye normalmente. ¿Puede considerarse correcta dicha afirmación?
  
n = 26
x = 120
s2 = 25
alpha = 0.01

#1) Hipotesis
# Ho: mu >= 118
# H1: mu < 118

#2) Est. de Prueba. T-empirico

#3) Region Critica, Tcritico. Pruena a 1 cola izquierda (alpha)
tc = qt(alpha, df= n-1)
#-2.4851

#4) Regla de decision
#Si t-empirico <= tc (-2.48) => RHo
#Si t-empirico > tc (-2.48) => NRHo

#5) T-empirico
te = (x - 118) / sqrt(25/26)
#te = 2.039

#6 Aplico Regla
te <= tc
#NRHo


# Ejercicio 5
#A partir de los resultados de una encuesta realizada en una importante universidad pública del país, se afirma que en por lo menos el 90% de los casos, las mujeres alcanzan un rendimiento académico superior al de los hombres. Se eligen al azar 200 alumnos y se encontraron 160 mujeres con promedio mayor al de los hombres. Con un nivel de significación del 5%, demostrar si se puede admitir como válida dicha afirmación.

P = 0.9
n = 200
p = 160/200
alpha = 0.05

#1.
#Ho: P >= 90%
#H1: P < 90%

#2. Estadistico => z-empirico

#3.Reg. Critica. Prueba a 1 cola izquierda (alpha)
zc = qnorm(alpha)
#-1.6448
#4. Regla de decision
# Si Ze <= Zc +> RHo
# Si Ze > Zc => NRHo

#5. Z empirico
ze = (p -P) / sqrt((P*(1-P))/n)
#-4.71

#6. Aplico regla
ze <= zc
# Rechazo Ho

#Ejercicio 6

#Se inyectó una droga a 100 personas y solo 8 tuvieron reacción alérgica, con un nivel de significación del 5% ¿se puede afirmar que la proporción de personas que sufren dicha reacción a esa droga, es menor a 0,10?

n = 100
p = 8/100
alpha = 0.05

P = 0.1 # esto es lo que hay que poner a prueba

#1. Hipotesis
# Ho: P<= 0.1 #cola izquierda
# H1: P> 0.1 

#2 Estadistico Z para proporcion

#3. Zcritico
zc = qnorm(alpha)

#4. Regla
# Si Zemp <= Zcritico => RHo; Si Zemp > Zcritico => NRHo

#5. Zempirico
ze = (p - P) / sqrt(P*(1-P)/n)

# Aplicaco Regla
ze<=zc
# No se rechaza Ho.






# Ejercicio 11

S1 = 75
var1 = S1**2

S2 = 50
var2 = S2**2

n1 = 36
n2 = 40

x1 = 300
x2 = 350

alpha = 0.05

# H0 mu2 - mu1 <= 0
# H1 mu2 - mu1 > 0

#cola derecha

Ze = ((x2 - x1) - 0) / sqrt((var2/n2)+(var1/n1))







