#Comportamiento distribuci?n Poisson:
#Generamos poblaciones:
n<-10000
pb1<-rpois(n,1)
pb2<-rpois(n,5)
pb3<-rpois(n,10)
pb4<-rpois(n,50)
p1<-table(pb1)
p2<-table(pb2)
p3<-table(pb3)
p4<-table(pb4)

x11()
par(mfrow=c(2,2))
plot(p1,type = "h",xlab = "x",ylab = "Frecuencia",main="Poisson(1)")
plot(p2,type = "h",xlab = "x",ylab = "Frecuencia",main="Poisson(5)")
plot(p3,type = "h",xlab = "x",ylab = "Frecuencia",main="Poisson(10)")
plot(p4,type = "h",xlab = "x",ylab = "Frecuencia",main="Poisson(50)")


#-----------------------------------------------------#
#Comportamiento distribuci?n Logistica:
#Generamos poblaciones:
#Variando el parametro de escala y dejando la localizaci?n en 0:
pl1<-rlogis(n,0,1)
pl2<-rlogis(n,0,5)
pl3<-rlogis(n,0,10)
pl4<-rlogis(n,0,50)
l1<-seq(-8,8,by=0.001)
l2<-seq(-40,40,by=0.001)
l3<-seq(-100,100,by=0.001)
l4<-seq(-600,600,by=0.001)
x11()
par(mfrow=c(2,2))
hist(pl1,freq = F,xlab = "x",ylab = "Frecuencia",main="Logistic(0,1)")
lines(l1,dlogis(l1,0,1))
hist(pl2,freq = F,xlab = "x",ylab = "Frecuencia",main="Logistic(0,5)")
lines(l2,dlogis(l2,0,5))
hist(pl3,freq = F,xlab = "x",ylab = "Frecuencia",main="Logistic(0,10)")
lines(l3,dlogis(l3,0,10))
hist(pl4,freq = F,xlab = "x",ylab = "Frecuencia",main="Logistic(0,50)")
lines(l4,dlogis(l4,0,50))
#Variando el parametro de escala y dejando la localizaci?n en 0:
pob1<-rlogis(n,1,1)
pob2<-rlogis(n,5,1)
pob3<-rlogis(n,10,1)
pob4<-rlogis(n,50,1)
lp1<-seq(-8,8,by=0.001)
lp2<-seq(-1,10,by=0.001)
lp3<-seq(0,18,by=0.001)
lp4<-seq(40,60,by=0.001)
x11()
par(mfrow=c(2,2))
hist(pob1,freq = F,xlab = "x",ylab = "Frecuencia",main="Logistic(1,1)")
lines(lp1,dlogis(lp1,1,1))
hist(pob2,freq = F,xlab = "x",ylab = "Frecuencia",main="Logistic(5,1)")
lines(lp2,dlogis(lp2,5,1))
hist(pob3,freq = F,xlab = "x",ylab = "Frecuencia",main="Logistic(10,1)")
lines(lp3,dlogis(lp3,10,1))
hist(pob4,freq = F,xlab = "x",ylab = "Frecuencia",main="Logistic(50,1)")
lines(lp4,dlogis(lp4,50,1))



#Ensayo Grafico de densidades para ver mejor el comportamiento de la logistic:
x11()
plot(density(pl1),xlim=c(-10,100))
lines(density(pl2))
lines(density(pl3))
lines(density(pl4))

x11()
plot(density(pob1),xlim=c(-5,80))
lines(density(pob2))
lines(density(pob3))
lines(density(pob4))

#--------------------------------------------------------------------------------------#
x<-558956 #N??mero al azar de minimo 4 cifras, con cifras pares (2n cifras)  
cifrasinicial<-trunc(log10(x))+1    #Detectar cifras del n??mero incial
n<-cifrasinicial/2 #encontramos el n de la formula 2n
N<-1000 #Cantidad de n??meros aleatorios a generar
u<-c()
for (i in 1:N) {
  x<-x^2
  cif<-trunc(log10(x))+1
  while((cif)!=4*n) {
    x<-x*(10)
    cif<-trunc(log10(x))+1
  }
  xnueva<-toString(x)
  numeroscentrales<-substring(xnueva,n+1,(2*n+n))
  num<-(as.numeric(numeroscentrales))/10^cifrasinicial
  u[i]<-c(num)
  x<-as.numeric(numeroscentrales)
}

x11()
plot(u, main="Metodo de Cuadrados Medios", sub="Generacion de valores pseudoaleatorios", 
     xlab="Cantidad de numeros aleatorios", ylab="valor")





