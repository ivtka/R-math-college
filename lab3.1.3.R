F <- function(x) {
  return(exp(x) - sqrt(1 + exp(2 * x)) - 2)
}

DF <- function(x) {
  return(exp(x) - (exp(2 * x)) / (sqrt(1 + exp(2 * x))))
}

DF2 <- function(x) {
  return(exp(x) - (2 * exp(2 * x)) / (sqrt(1 + exp(2 * x))) + (exp(4 * x)) / ((1 + exp(2 * x))^(3 / 2)))
}

zbig <- function(f, df2, x) {
  kx <- f(x) * df2(x)
  return(kx)
}

MNewton <- function(f, df, x0, eps) {
  x <- x0
  while (abs(f(x)) > eps) {
    x <- x - f(x) / df(x)
  }
  return(x)
}
eps <- 10^(-6)

x01 <- 1.3
kx01 <- zbig(F, DF2, x01)

x1 <- MNewton(F, DF, x01, eps)
print(x1)
