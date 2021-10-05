# EJ 1
# distribución binoamial binomial (30 ,  0.15)
# n = 10 ^ (2 al 5)
# rbinom(
#       n,     Número de observaciones aleatorias a ser generadas
#       size,  Número de ensayos (> = 0)
#       prob)  La probabilidad de éxito en cada ensayo

# Generamos 4 constantes "a", "b", "c" y "d" aplicando la distribución binomial
# para los tamaños de muestra 10^2, 10^3, 10^4 y 10^5 respectivamente
a = rbinom(10^2,30, 0.15)
b = rbinom(10^3,30, 0.15)
c = rbinom(10^4,30, 0.15)
d = rbinom(10^5,30, 0.15)

# Para graficar las 4 mustras en un mismo gráfico de cajas generamos un array
# con los resultados
randomValues <- c(a,b,c,d)

# agregamos un data.frame para identificar cada gráfico
data <- data.frame(values =randomValues,
                   group = c("10^2","10^3","10^4","10^5"))

# Con boxplot generamos el gráfico de cajas agregado color para distiguir 
# cada gráfico y borramos el eje "y" para agregar uno más exacto
boxplot(values ~ group,
        data, 
        col = c("pink",
                "beige", 
                "lightblue", 
                "lightgreen"),
        main = "Distribución binomial",
        xlab = "Muestras",
        ylab = "Valor variable aleatoria",
        yaxt='n')

# Agregamos eje "y" más exacto
axis(2, at=seq(0, 30, 1),las=2)


# Utilizamos "mean" para calcular la esperanza empírica de cada muestra
# y guardamos cada resultado para su posterior comparación
espA = mean(a)
espB = mean(b)
espC = mean(c)
espD = mean(d)

# Utilizamos el producto número de ensayos y la probabilidad de éxito para 
# hallar la esperanza teórica
espT = 30*0.15

# Utilizamos "var" para calcular la varainza empírica de cada muestra
# y guardamos cada resultado para su posterior comparación
varA = var(a)
varB = var(b)
varC = var(c)
varD = var(d)

# Utilizamos el producto del numero de ensayos, la probabilidad de éxito y 
# la probabilidad de fracaso para obtener la varianza teórica
varT = 30*0.15*0.85

# Con "ecdf" calculamos la distribución empírica para las muestras
# de tamaño 10^2 y 10^5
distA = ecdf(a)
distD = ecdf(d)

# Utilizamos "stepfun" con una lista de 1 a 10 junto a pbinom para obetner
# obtener la función de distribuión acumulada teórica y poder graficarla
# utilizando "plot". genrando los escalones característicos. Nos aseguramos
# de eliminar las verticales y de agregar las etiquetas correspondientes
plot(stepfun(c(1:10),pbinom(c(0,1:10),30,0.15)), 
     main = "Función de distribución acumulada", 
     xlab = "k",
     ylab = "F(k)",
     col="red",
     verticals=FALSE)

# Sobre el gráfico anterior graficamos nuestra desitribución empirica 
# con tamaño de muestera 10^2 que obtuubimos previmante con un color
# diferente para distinguirla de la distribución teórica
lines(distA, col="springgreen4")

# Agregamos una leyenda para hacer más clara la identificación
# de los gráficos
legend("bottomright", 
       legend = c("Distribución Acumulada Empírica - 10^2",
                  "Distribución Acumulada Teórica"),
       lty = 1, col = c("red","springgreen4"), lwd = 2, box.lty =1)

# Analogamente al gráfico anterior gráficamos la distribución acumulada teórica
plot(stepfun(c(1:10),pbinom(c(0,1:10),30,0.15)), 
     main = "Función de distribución acumulada", 
     xlab = "k",
     ylab = "F(k)",
     col="red")

# Sobre el gráfico anterior graficamos nuestra desitribución empirica 
# con tamaño de muestera 10^5 que obtubimos previmante con un color
# diferente para distinguirla de la distribución teórica, nuevamente
lines(distD, col="blue")

# Agregamos una leyenda para hacer más clara la identificación
# de los gráficos
legend("bottomright", 
       legend = c("Distribución Acumulada Empírica - 10^5",
                  "Distribución Acumulada Teórica"),
       lty = 1, col = c("blue","red"), lwd = 2, box.lty =1)



# EJ 2
# Distribución Normal(-4,16)
# n = 10 ^ (2 al 5)

# Generamos 4 constantes "a", "b", "c" y "d" aplicando la distribución normal
# para los tamaños de muestra 10^2, 10^3, 10^4 y 10^5 respectivamente
a = rnorm(10^2,-4, sqrt(16))
b = rnorm(10^3,-4, sqrt(16))
c = rnorm(10^4,-4, sqrt(16))
d = rnorm(10^5,-4, sqrt(16))

# Para graficar las 4 mustras en un mismo gráfico de cajas generamos un array
# con los resultados
randomValues <- c(a,b,c,d)
data <- data.frame(values =randomValues,
                   group = c("10^2","10^3","10^4","10^5"))

# Con boxplot generamos el gráfico de cajas agregado color para distiguir 
# cada gráfico y borramos el eje "y" para agregar uno más exacto, al igual
# que lo hicimos con la distribución binomial
boxplot(values ~ group,
        data, 
        col = c("pink",
                "beige", 
                "lightblue", 
                "lightgreen"),
        main = "Distribución normal",
        xlab = "Muestras",
        ylab = "Valor variable aleatoria", 
        yaxt='n')

# Agregamos eje "y" más exacto
axis(2, at=seq(-20, 20, 1),las=2)

# Como estamos trabajando con la dsitribución normal, sabemos que la esperanza
# está definida por el valor otorgado para los parámetros de la
# distribución normal
espT = -4

# Al igual que la esperanza, la varianza es la raíz cuadrada de la desviación
# estandar, en este caso está definida por el valor otorgado 
# para los parámetros de la distribución normal
varT = 16

# Al igual que con la distribución binomial, utilizamos "mean" para calcular
# la esperanza empírica
espA = mean(a)
espB = mean(b)
espC = mean(c)
espD = mean(d)

# Varianza Empirica
# Al igual que con la distribución binomial, utilizamos "var" para calcular
# la varianza teórica empírica
varA = var(a)
varB = var(b)
varC = var(c)
varD = var(d)

# Histogramas
x <- seq(-20, 10, 1)
hist(a, 
     main="Distribución normal - muestra 10^2",
     breaks = 20, 
     xaxt="n",
     ylab="Densidad",
     xlab="x",
     freq=FALSE)


lines(x, 
      dnorm(x, mean = -4, sd = 4), 
      col = "blue",
      lty = 1, 
      lwd = 2,
      xaxt="n")

polygon(x, 
        dnorm(x, mean = -4, sd = 4), 
        col = rgb(0, 0, 1, 
                  alpha = 0.5))
axis(1, 
     at=seq(-20, 10, 1),
     las=2)
legend("topleft", 
       legend = c("Valores aleatorios",
                  "Función de densidad"),
       lty = 1, col = c("grey","blue"), lwd =1, box.lty =1)


hist(d, 
     main="Distribución normal - muestra 10^5",
     ylab="Densidad",
     xlab="x",
     breaks = 50,
     xaxt="n", 
     freq=FALSE)

lines(x, 
      dnorm(x, mean = -4, sd = 4), 
      col = "blue", 
      lty = 1, 
      lwd = 2, 
      xaxt="n")

polygon(x, 
        dnorm(x, mean = -4, sd = 4), 
        col = rgb(0, 1, 0, alpha = 0.5))

axis(1, at=seq(-20, 10, 1),las=1)
legend("topleft", 
       legend = c("Valores aleatorios",
                  "Función de densidad"),
       lty = 1, col = c("grey","green"), lwd =1, box.lty =1)

# EJ 3
# Distribución Normal(-4,16)
# PT 1) n = 10 ^ 3
a = rnorm(10^3,-4, sqrt(16)) 
# media empírica de a
muEmp = mean(a) 
# media teórica
muTeo = -4
#Desvión estandar teórica
desvTeo = sqrt(16)
# Valor estandarizado de mediana empíricia
medEsta = sqrt(1000) * ((muEmp - muTeo)/desvTeo)

# PT 2)
promEst <- 0
for(i in 1:500){
        ab = rnorm(10^3,-4, sqrt(16))
        muEmpb = mean(ab) 
        promEst[i] <- sqrt(10^3) * ((muEmpb - muTeo)/desvTeo)
}
#Presentar en un histograma
s = seq(-4, 4, 0.1)
hist(promEst,
     main="Promedio estandarizados y distribución normal estandar",
     breaks = 50,
     xaxt="n",
     ylab="Densidad",
     xlab="x",
     freq=FALSE)

lines(s, 
      dnorm(s, mean = 0, sd = 1),
      lty = 1, 
      lwd = 2)

polygon(s, 
        dnorm(s, mean = 0, sd = 1), 
        col = rgb(1, 0, 0, alpha = 0.5))

axis(1, at=s,las=1)
legend("topleft", 
       legend = c("Promedio estandarizado",
                  "Distribución normal estandar"),
       lty = 1, col = c("grey","red"), lwd =1, box.lty =1)



