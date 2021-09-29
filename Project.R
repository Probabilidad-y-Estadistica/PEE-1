# EJ 1
# distribución binoamial binomial (30 ,  0.15)
# n = 10 ^ (2 al 5)
# rbinom(
#       n,     Número de observaciones aleatorias a ser generadas
#       size,  Número de ensayos (> = 0)
#       prob)  La probabilidad de éxito en cada ensayo
a = rbinom(10^2,30, 0.15)
b = rbinom(10^3,30, 0.15)
c = rbinom(10^4,30, 0.15)
d = rbinom(10^5,30, 0.15)

randomValues <- c(a,b,c,d)

data <- data.frame(values =randomValues,
                   group = c("10^2","10^3","10^4","10^5"))

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
axis(2, at=seq(0, 30, 1),las=2)


# Esperanza Empírica
espA = mean(a)
espB = mean(b)
espC = mean(c)
espD = mean(d)

# Esperanza Teorica
espT = 30*0.15

# Varianza Empirica
varA = var(a)
varB = var(b)
varC = var(c)
varD = var(d)

# Varianza Teorica
varT = 30*0.15*0.85

# Función de distribución acumulada empirica
distA = ecdf(a)
distD = ecdf(d)

# Función de distribución teórica y empirica
plot(stepfun(c(1:10),pbinom(c(0,1:10),30,0.15)), 
     main = "Función de distribución acumulada", 
     xlab = "k",
     ylab = "F(k)",
     col="red")
lines(distA, col="springgreen4")
legend("bottomright", 
       legend = c("Distribución Acumulada Empírica - 10^2",
                  "Distribución Acumulada Teórica"),
       lty = 1, col = c("red","springgreen4"), lwd = 2, box.lty =1)

plot(stepfun(c(1:10),pbinom(c(0,1:10),30,0.15)), 
     main = "Función de distribución acumulada", 
     xlab = "k",
     ylab = "F(k)",
     col="red")
lines(distD, col="blue")
legend("bottomright", 
       legend = c("Distribución Acumulada Empírica - 10^5",
                  "Distribución Acumulada Teórica"),
       lty = 1, col = c("blue","red"), lwd = 2, box.lty =1)



# EJ 2
# Distribución Normal(-4,16)
# n = 10 ^ (2 al 5)

a = rnorm(10^2,-4, sqrt(16))
b = rnorm(10^3,-4, sqrt(16))
c = rnorm(10^4,-4, sqrt(16))
d = rnorm(10^5,-4, sqrt(16))

randomValues <- c(a,b,c,d)
data <- data.frame(values =randomValues,
                   group = c("10^2","10^3","10^4","10^5"))
# Explicar diferencia de los boxplots por funcionamiento de cajas
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
axis(2, at=seq(-20, 20, 1),las=2)

# Esperanza Teórica
espT = -4

# Varianza Teórica
varT = 16

# Esperanza Empírica
espA = mean(a)
espB = mean(b)
espC = mean(c)
espD = mean(d)

# Varianza Empirica
varA = var(a)
varB = var(b)
varC = var(c)
varD = var(d)

# Histogramas
x <- seq(-20, 10, 1)
hist(a, main="Normal distribution", breaks = 50)
hist(d, main="Normal distribution", breaks = 50)

# https://r-coder.com/normal-distribution-r/
# Grid of X-axis values
x <- seq(-4, 8, 0.1)

#-----------------------------------------
# Same standard deviation, different mean
#-----------------------------------------
# Mean 0, sd 1
plot(x, dnorm(x, mean = 0, sd = 1), type = "l",
     ylim = c(0, 0.6), ylab = "", lwd = 2, col = "red")
# Mean 3, sd 1
lines(x, dnorm(x, mean = 3, sd = 1), col = "blue", lty = 1, lwd = 2)

# Adding a legend
legend("topright", legend = c("0 1", "3 1"), col = c("red", "blue"),
       title = expression(paste(mu, " ", sigma)),
       title.adj = 0.9, lty = 1, lwd = 2, box.lty = 0)
