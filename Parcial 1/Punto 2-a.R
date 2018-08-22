fc<-function(x)
{
  s <- sin(x)
  return (s)
}

alg<-function(f,x,E)
{
  y3<-(fc(x-1)-fc(x-2))
  y2<-fc(x-1)*((x-1)-(x-2))
  y1<-(x-1)
  y<-y1-(y2/y3)
  
  repeat{
    x<-y
    y3<-(fc(x-1)-fc(x-2))
    y2<-fc(x-1)*((x-1)-(x-2))
    y1<-(x-1)
    y<-y1-(y2/y3)
    
    if(abs(x-y)>E)
      break
  }
  return (y)
}


x11()
plot(fc, type="l", col="green", lwd=2, ylim=c(-10,10), xlim=c(0,5)) 

cat(format(toString(alg(fc, 1, 0.0000001)),nsmall = 4))
