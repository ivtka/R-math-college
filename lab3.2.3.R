n <- 2

Fx <- function(x) {
  f <- c(1:2)
  f[1] <- tan(x[1] * x[2] + 0.3) - x[1]^2
  f[2] <- 0.9*x[1]^2 + 2 * x[2]^2 - 1
  return(f)
}

FunMNK <- function(x) {
  z <- Fx(x)
  S <- 0
  for (i in 1:n) {
    S <- S + z[i] * z[i]
  }
  return(S)
}

x0 <- c(0.65, 0.35)

res <- optim(fn = FunMNK, par = x0)
x1 <- res$par
print(x1)
