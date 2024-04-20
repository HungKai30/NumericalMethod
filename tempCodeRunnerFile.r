#Bisection method? -> phương pháp chia đôi
#1.1 Bisection with absolute error (sai số tuyệt đối)
#bài toán tìm nghiệm của 1 hàm số với khoảng phân li cho trước
Bisect1 <- function(f, a, b, tol = 1e-6, maxiter = 100){
  #f: hàm số cần tìm nghiệm
  #a, b: khoảng phân li
  #tol: sai số tuyệt đối
  #maxiter: số lần lặp tối đa
  #khai báo biến
  f.a <- f(a)
  f.b <- f(b)
  iter <- 0
  while (abs(b-a)>tol) {
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
  return(root)
}

f <- function(x) {
  return(cos(x) - x)
}

Bisect1(f, 0, 1)