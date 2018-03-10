x<-55895649 #Número al azar de minimo 4 cifras, con cifras pares (2n cifras)  
cifrasinicial<-trunc(log10(x))+1    #Detectar cifras del número incial
n<-cifrasinicial/2 #encontramos el n de la formula 2n
N<-1000 #Cantidad de números aleatorios a generar
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
plot(u)





