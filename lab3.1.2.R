F <- function(x) {
  return(exp(x) - sqrt(1 + exp(2 * x)) - 2)
}

x <- seq(-4, 4, len = 100)
y <- F(x)
plot(x, y, type = "l")
abline(h = 0)
abline(v = 0)

eps <- 10^(-6)
a1 <- 1
b1 <- 2

MDihotom <- function(f, a, b, eps) {
  while (abs(b - a) > eps) {
    x <- (a + b) / 2
    if (f(a) * f(x) < 0) {
      b <- x
    } else {
      a <- x
    }
  }
  return((a + b) / 2)
}

x1 <- MDihotom(F, a1, b1, eps)

print(x1)
