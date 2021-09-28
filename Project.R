#Distro binomial
#p=0.15 q=0.85 n = 10^2, 10^3, 10^4 y 10^5
help(pbinom)
valores = c(0,1:10)
valoresN <-c(10^2, 10^3, 10^4, 10^5)
#Lo mismo pero con un vector
miDistro <-pbinom(valores,valoresN,0.25) #Esto suma todas, si de deshace el vector, se puede calcular las puntuales
miDistroEmpirica1 = ecdf(rbinom(50,10^2,0.25))
miDistroEmpirica2 = ecdf(rbinom(50,10^5,0.25)) #Binomial con muestra de tam 50
#ecdf muestra de distribucion acumulada empirica
#a) Presentar en un mismo grafico los 4 boxplot (diagrama de cajas) de las muestras generadas.
boxplot(valoresN, miDistro, xlab = "Muestras",
        ylab = "Distribuciones", main = "Box plot de la primer distro")
#b) Calcular los valores empiricos de la esperanza y de la varianza de cada una de las 4 muestras, y
#compararlos con los valores teoricos correspondientes.
var(miDistro) #Varianza es el cuadrado de la desviaci?n estandar
mean(miDistro) #Desviaci?n estandar
var(miDistroEmpirica) #Varianza empirica es el cuadrado de la desviaci?n estandar
sd(miDistroEmpirica) #Desviaci?n estandar empirica
#c) Presentar en un mismo gr?afico la funci?on de distribuci?on te?orica de la Distribucion 1 y la funcion
#de distribucion empirica de la muestra para n = 102 Hacer otro grafico similar para n = 105
b = stepfun(c(1:10),miDistro)
plot(b,verticals=FALSE)
plot(miDistroEmpirica1,add=T)
plot(miDistroEmpirica2,add=T)


#M?ximo 8
pbinom(8,10,0.15)
#Lo mismo pero con un vector
x<-c(0:8)
sum(dbinom(x,10,0.15))
#M?s de 3 P=(x>3)=1-P(x<=3)
1-pbinom(3,10,0.15)
#Graficar

x<-c(0:10)
y<-dbinom(x,10,0.15)
boxplot(x,y)


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
        ylab = "Valor variable aleatoria")


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

# Función de distribución teórica
plot(distA, main = "Función de distribución")


