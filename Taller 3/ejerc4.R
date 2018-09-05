library(Matrix)
library(PolynomF)
library(rSymPy)

rango=c(35,45,55,65,75)
numero=c(35,48,70,40,22)

plot(rango,numero, pch=19, cex=1, col = "blue", asp=1,xlab="X", ylab="Y", main="Diagrama ")

#lagrange
lagrange.poly <- function(x, y) {
  require(rSymPy)
  
  if (length(x) != length(y)) {
    stop('x and y must be of equal length')
  }
  
  l <- list() # List to store Lagrangian polynomials L_{1,2,3,4}
  k <- 1
  
  for (i in x) {
    # Set the numerator and denominator of the Lagrangian polynomials to 1 and build them up
    num <- 1
    denom <- 1
    
    # Remove the current x value from the iterated list
    p <- x[! x %in% i]
    
    # For the remaining points, construct the Lagrangian polynomial by successively 
    # appending each x value
    for (j in p) {
      num <- paste(num, "*", "(", 'x', " - ", as.character(j), ")", sep = "", collapse = "")
      denom <- paste(denom, "*", "(", as.character(i)," - ", as.character(j), ")", sep = "", collapse = "")
    }
    
    # Set each Lagrangian polynomial in rSymPy to simplify later.
    l[k] <- paste("(", num, ")", "/", "(", denom, ")", sep = "", collapse = "")
    k <- k + 1
  }
  
  # Similar to before, we construct the final Lagrangian polynomial by successively building 
  # up the equation by iterating through the polynomials L_{1,2,3,4} and the y values 
  # corresponding to the x values.
  eq <- 0
  
  for (i in 1:length(y)) {
    eq <- paste(eq, '+', as.character(y[i]), "*", l[[i]], sep = "", collapse = "")
  }
  
  # Define x variable for rSymPy to simplify
  x <- Var('x')
  
  # Simplify the result with rSymPy and return the polynomial
  return(sympy(paste("simplify(", eq, ")")))
}


lagrangeP<-lagrange.poly(rango,numero)
print(lagrangeP)
pr <- as.function(alist(x=,eval(parse(text=lagrangeP))))
resultado=pr(35)+pr(45)+pr(55)

cat("\nRespuesta con ajuste de Lagrange: ",resultado,"\n")

#polinomico
polyAjuste <- poly.calc(rango,numero)

cat("\n ")
print(polyAjuste)

prf <- as.function(polyAjuste)

resultado=prf(35)+prf(45)+prf(55)

cat("\nRespuesta con ajuste polinomico: ",resultado)

curve(polyAjuste,add=T, from=30, to=80)
