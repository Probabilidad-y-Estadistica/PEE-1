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


# Grid of X-axis values
x <- 1:80

# size = 80, prob = 0.2
plot(dbinom(x, size = 80, prob = 0.2), type = "h", lwd = 2,
     main = "Binomial probability function",
     ylab = "P(X = x)", xlab = "Number of successes")

# size = 80, prob = 0.3
lines(dbinom(x, size = 80, prob = 0.3), type = "h",
      lwd = 2, col = rgb(1,0,0, 0.7))

# Add a legend
legend("topright", legend = c("80  0.2", "80  0.3"),
       title = "size  prob", title.adj = 0.95,
       lty = 1, col = 1:3, lwd = 2, box.lty = 0)


