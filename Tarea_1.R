#TAREA 1

#Funcion del paracaidista 
fc<-function(c)
{
  s <- (((9.8*68.1)/as.numeric(c))*(1-exp(-((as.numeric(c)/68.1)*(10))))-40)
  return (s)
}

#Se piden datos 
particion=as.numeric(readline("Ingrese el numero de particiones: "))
a=as.double(readline('Introduzca el primer valor: '));
b=as.double(readline('Introduzca el segundo valor: '));

#Se muestra la grafica de la función
plot(fc, xlim=c(as.numeric(a),as.numeric(b)), type="l", col="green", lwd=2, main="Función del paracaidista", ylim=c(-5, 100)) 
points(valor,fc(as.numeric(valor)),col="red", pch=19)
abline(h=0, lty=3, col="dark grey")

#Se mira si el valor esta definido en la funcion
copA=a
copB=b
#sino se agrega 0.00000001 para que continue ejecutandose 
if(is.na(fc(copA)))
{
  copA=copA+0.00000001
}
if(is.na(fc(copB)))
{
  copB=copB+0.00000001
}

#Se mira si los valores que se ingresaron se pueden usar 
if (fc(copA)*fc(copB)<=0)
{
  #Empieza la particion y continua hasta que este valor sea igual a 0
  while(fc(copA)*fc(copB)!=0)
  {
    t=FALSE
    
    #Calculo de la distancia
    distancia=(abs(a-b)/particion)
    
    #Nuevos limites a tener en cuenta
    x<-0:particion
    y<-c(0:particion)
    y[0:particion + 1]<-a
    x<-x*distancia
    x<-x+y
    i=2
    
    #Ciclo para buscar los intervalos que se pueden usar para la particion
    while(t == FALSE && i <= particion + 1)
    {
      a=x[i-1]
      b=x[i]
      copA=a
      copB=b
      
      if(is.na(fc(copA)))
      {
        copA=copA+0.00000001
      }
      
      if(is.na(fc(copB)))
      {
        copB=copB+0.00000001
      }
      
      if(fc(copA)*fc(copB)<=0)
      {
        t=TRUE
      }
      i=i+1
    }
  }
  
  #Se muestra el resultado obtenido con la particion
  if(fc(a)==0)
  {
    cat("\nValor aproximado: ",format(a,nsmall = 8));
  }else
  {
    cat("\nValor aproximado: ",format(b,nsmall = 8));
  }
  
}

#Se encuentra el valor de la raiz de la ecuacion y se imprime lo obtenido
valor=uniroot(fc,lower=-1,upper = 20)
valor=valor["root"]
cat("\nValor de la raiz: ",format(valor,nsmall = 8))

