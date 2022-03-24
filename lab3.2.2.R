n <- 2
gx <- function(x) {
  return(c(
    tan(x[1] * x[2] + 0.3) - x[1]^2,
    0.9 * x[1]^2 + 2 * x[2]^2 - 1
  ))
}

x0 <- c(0.65, 0.35)
kmax <- 100
eps <- 0.00001

met_iter <- function(g, x, n, eps, kmax) {
  k <- 0
  repeat {
    xk <- g(x)
    y <- xk - g(xk)
    if (((sqrt(t(y) %*% y)) <= eps) | (k == kmax)) {
      break
    }
    x <- xk
    k <- k + 1
  }
  x[n + 1] <- k
  return(x)
}

x <- met_iter(gx, x0, n, eps, kmax)
print(x)
