jacobi<-function(A1,B,xo,E)
{
  vr<-eigen(A)
  r <- max(abs(vr$vector))
  
  cont<-1
  fin<-1
  
  if(r<1)
  {
    cat("Si converge:")
    print(r)
    cat("\n")
    
    repeat
    {
      U<-A1
      L<-A1
      U[lower.tri(U,diag = TRUE)]<-0
      L[upper.tri(U,diag = TRUE)]<-0
      A<-A1-L-U
      D=solve(A)
      Z<-(D%*%(L+U))
      Tu<-Z*-1
      C<-D%*%B
      xo<-(Tu%*%xo)+C
      cont=cont+1
      fin<-(A%*%xo)-B
      if(final(A1,B,xo,E))
      {
        break
      }
    }
  }
  else
  {
    cat("No converge")
    print(r)
    cat("\n")
  }
  
 
  cat("Xo:")
  print(xo)
  cat("Contador: ", cont)
  cat("\n\nEvaluando: ")
  print(A1%*%xo)
}


final<-function(A,B,x,E)
{
   r<-A%*%x-B
  for(i in 1:nrow(r))
  {
    w<-r[i,1]
    if(abs(w)<E)
      return (TRUE)
  }
  return (FALSE)
}

A<-matrix(nrow = 3,ncol = 3,c(8,9,2,2,7,2,2,8,6),byrow = TRUE)
B<-matrix(nrow = 3,ncol = 1,c(69,47,68),byrow = TRUE)
x<-matrix(nrow = 3,ncol = 1,c(0,0,0),byrow = TRUE)
jacobi(A,B,x,0.0001)


