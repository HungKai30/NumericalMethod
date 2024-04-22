#Bisection method? -> phương pháp chia đôi
#1.2 Bisection with relative error (sai số tương đối)
#bài toán tìm nghiệm của 1 hàm số với khoảng phân li cho trước
f <- function(x) {
  return(cos(x) - x)
}
Bisect2 <- function(f, a, b, tol = 1e-16, maxiter = 1000){
  iter <- 0
  f.a <- f(a)
  f.b <- f(b)
  while (abs(b-a)/max(abs(c(a,b)))>tol) {
    iter <- iter +1
    if (iter > maxiter) {
      warning("Max iteration reached")
      break
    }
    c <- (a+b)/2
    ymid <- f(c)
    if (f.a * ymid>0) {
      a <- c
      f.a <- ymid
    } else {
      b <- c
      f.b <- ymid
    }
  }
  root <- (a+b)/2

  #tinh toc do hoi tu cua ham
  return(root)

}
r<-Bisect2(f, 0, 1,1e-15)

plot(1:length(r), r, type = "b",col = "blue", 
     xlab = "Number of Iterations", ylab = "Absolute Rate of Convergence (log scale)",
     main = "Bisection Method Convergence")