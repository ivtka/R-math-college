trapeze <- function(f, a, b, n) {
  h <- (b - a) / n
  s <- (f(a) + f(b)) / 2
  x <- 1:(n + 1)
  for (i in 2:n) {
    x[i] <- a + (i - 1) * h
    s <- s + f(x[i])
  }
  return (s * h)
}

simpson <- function(f, a, b, n) {
  h <- (b - a) / n
  s1 <- 0
  s2 <- 0
  x <- 1:(n - 1)
  for (k in 1:(n / 2 - 1)) {
    x[2 * k] = a + (2 * k) * h
    s1 <- s1 + f(x[2 * k])
  }
  for (k in 1:(n / 2)) {
    x[2 * k - 1] = a + (2 * k - 1) * h
    s2 <- s2 + f(x[2 * k - 1])
  }
  return (h / 3 * (f(a) + f(b) + 2 * s1 + 4 *s2))
}

f <- function(x) {
  return (3.2 * x ^ (1 / 3) + log(2 * x + 4) ^ 2)
}

a <- 0.6
b <- 0.9
n <- 20

# f <- function(x) {
#   return (4^x*(7.5-x^2)^(1/3))
# }
# 
# a <- 1.35
# b <- 1.95
# n <- 10
