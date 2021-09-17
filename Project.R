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
var(miDistro) #Varianza es el cuadrado de la desviación estandar
mean(miDistro) #Desviación estandar
var(miDistroEmpirica) #Varianza empirica es el cuadrado de la desviación estandar
sd(miDistroEmpirica) #Desviación estandar empirica
#c) Presentar en un mismo gr´afico la funci´on de distribuci´on te´orica de la Distribucion 1 y la funcion
#de distribucion empirica de la muestra para n = 102 Hacer otro grafico similar para n = 105
b = stepfun(c(1:10),miDistro)
plot(b,verticals=FALSE)
plot(miDistroEmpirica1,add=T)
plot(miDistroEmpirica2,add=T)


#Máximo 8
pbinom(8,10,0.15)
#Lo mismo pero con un vector
x<-c(0:8)
sum(dbinom(x,10,0.15))
#Más de 3 P=(x>3)=1-P(x<=3)
1-pbinom(3,10,0.15)
#Graficar

x<-c(0:10)
y<-dbinom(x,10,0.15)
boxplot(x,y)

