#p=0.15 q=0.85 n=10
#Distro binomial
#Exactamente 5
dbinom(5,10,0.15)
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
plot(x,y)

