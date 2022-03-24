fx <- function(x) {
  f <- c(1:2)
  f[1] <- tan(x[1] * x[2] + 0.3) - x[1]^2
  f[2] <- 0.9 * x[1]^2 + 2 * x[2]^2 - 1
  return(f)
}

ffx <- function(x) {
  ff <- matrix(nrow = 2, ncol = 2)
  ff[1, 1] <- 1 / cos(x[1] * x[2] + (1 / 30))^2 * x[2] - 2 * x[1]
  ff[1, 2] <- 1 / cos(x[1] * x[2] + (1 / 30))^2 * x[1]
  ff[2, 1] <- 2 * 0.9 * x[1]
  ff[2, 2] <- 4 * x[2]
  return(ff)
}

x0 <- c(0.65, 0.35)
n <- 2

kmax <- 100
eps <- 0.00001

met_newton <- function(fx, ffx, x, n, eps, kmax) {
  y <- fx(x)
  k <- 0
  while ((sqrt(t(y) %*% y)) > eps) {
    yy <- ffx(x)
    x <- x - solve(yy) %*% y
    y <- fx(x)
    if (k == kmax) {
      break
    }
    k <- k + 1
  }
  x[n + 1] <- k
  return(x)
}

x <- met_newton(fx, ffx, x0, n, eps, kmax)
print(x)
