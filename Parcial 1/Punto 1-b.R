
suma<-function(A)
{
  resultado<-0
  for(i in seq_len(nrow(A))) 
  {             
    for(j in seq_len(ncol(A))) 
    {
      resultado<-resultado+A[i,j]
    }
  }
  cat("\nMatriz\n")
  print(A)
  cat("\nResultado:\n")
  print(resultado)
}

A<-matrix(nrow = 3,ncol = 3,c(1,5,6,8,7,2,3,1,5),byrow = TRUE)
suma(A)
cat("\nNotación:\n")
print("O(n)=n^2")