lindebp <- function(p, q, f, a, b, alpha, beta, A, B, n) {
  h <- (b - a) / n
  x <- 1:(n + 1)
  for (i in 1:(n + 1)) {
    x[i] <- a + (i - 1) * h
  }
  c <- matrix(0, nrow = (n + 1), ncol = (n + 1))
  d <- 1:(n + 1)

  c[1, 1] <- alpha[1] * h - alpha[2]
  c[1, 2] <- alpha[2]
  c[n + 1, n] <- -beta[2]
  c[n + 1, n + 1] <- beta[1] * h + beta[2]
  for (i in 2:n) {
    c[i, i - 1] <- 1 - h * p(x[i]) / 2
    c[i, i] <- h^2 * q(x[i]) - 2
    c[i, i + 1] <- 1 + h * p(x[i]) / 2
  }
  d[1] <- A * h
  d[n + 1] <- B * h
  for (i in 2:n) {
    d[i] <- f(x[i]) * h^2
  }
  y <- solve(c, d)
  return(cbind(x, y))
}

p <- function(x) {
  return(x * cos(x))
}

q <- function(x) {
  return(x + 1)
}

f <- function(x) {
  return(3)
}

alpha <- c(1, 1)
beta <- c(1, 0)

A <- 0
B <- 2

a <- 0
b <- 3
n <- 10

xy <- lindebp(p, q, f, a, b, alpha, beta, A, B, n)
plot(xy[, 1], xy[, 2], type = "p")