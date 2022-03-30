eiler <- function(f, a, b, y0, n) {
  h <- (b - a) / n
  x <- 1:(n + 1)
  y <- 1:(n + 1)
  x[1] <- a
  y[1] <- y0
  for (i in 1:n) {
    x[i + 1] <- x[i] + h
    y[i + 1] <- y[i] + h * f(x[i], y[i])
  }
  return (cbind(x, y))
}

eiler1 <- function(df, a, b, y0, n) {
  h <- (b - a) / n
  x <- 1:(n + 1)
  y <- 1:(n + 1)
  x[1] <- a
  y[1] <- y0
  for (i in 1:n) {
    x[i + 1] <- x[i] + h
    dy <- y[i] + h / 2 * f(x[i], y[i])
    y[i + 1] <- y[i] + h * f(x[i] + h / 2, dy)
  }
  return (cbind(x, y))
}

df <- function(x, y) {
  return (2 * x ^ 3 + y ^ 3)
}

a <- 0
b <- 1
y0 <- -1.0
h <- 0.1
