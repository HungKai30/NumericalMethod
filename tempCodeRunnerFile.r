myBisectmax <- function(f, a, b, tol = 1e-15)
{
  iter <- 0
  coe <- 0.5
  a.star <- b - coe*abs(b-a)
  b.star <- a.star +coe*abs(b-a.star)
  while (abs(b-a) > tol) {
    iter <- iter + 1 
    if( f(a.star) > f(b.star)){
      b <- b.star
    }
    else {
      a <- a.star
    }
    a.star <- b - 0.5 * abs(b-a)
    b.star <- a.star + 0.5*abs(b-a.star)
  }
  return((a+b)/2)
}
f <- function(x){
  return(- x^2 + 4*x +3 )
}
a <- 0
b <- 10
myBisectmax(f, a, b, tol = 1e-15)
xi <- seq(0,4,by=0.01)
yi <- f(xi)
plot(xi, yi)
abline(v = myBisectmax(f, a, b, tol = 1e-15), col = "red")

