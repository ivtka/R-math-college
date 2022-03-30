adambosh4 <- function(df, a, b, y0, n) {
  h <- (b - a) / n
  x <- 1:(n + 1)
  y <- 1:(n + 1)
  f <- 1:(n + 1)
  x[1] <- a
  y[1] <- y0
  f[1] <- df(x[1], y[1])
  for (i in 1:3) {
    x[i + 1] <- x[i] + h
    k1 <- h * f[i]
    k2 <- h * df(x[i] + h / 2, y[i] + k1 / 2)
    k3 <- h * df(x[i] + h / 2, y[i] + k2 / 2)
    k4 <- h * df(x[i] + h, y[i] + k3)
    y[i + 1] <- y[i] + 1 / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
    f[i + 1] <- df(x[i + 1], y[i + 1])
  }
  for (i in 4:n) {
    x[i + 1] <- x[i] + h
    y[i + 1] <- y[i] + h / 24 * (55 * f[i] - 59 * f[i - 1] + 37 * f[i - 2] -
                                    9 * f[i - 3])
    f[i + 1] <- df(x[i + 1], y[i + 1])
  }
  return (cbind(x, y))
}

predcor4 <- function(df, a, b, y0, n) {
  h <- (b - a) / n
  x <- 1:(n + 1)
  y <- 1:(n + 1)
  f <- 1:(n + 1)
  x[1] <- a
  y[1] <- y0
  f[1] <- df(x[1], y[1])
  for (i in 1:3) {
    x[i + 1] <- x[i] + h
    k1 <- h * f[i]
    k2 <- h * df(x[i] + h / 2, y[i] + k1 / 2)
    k3 <- h * df(x[i] + h / 2, y[i] + k2 / 2)
    k4 <- h * df(x[i] + h, y[i] + k3)
    y[i + 1] <- y[i] + (1 / 6) * (k1 + 2 * k2 + 2 * k3 + k4)
    f[i + 1] <- df(x[i + 1], y[i + 1])
  }
  for (i in 4:n) {
    x[i + 1] <- x[i] + h
    ypred <- y[i] + h / 24 * (55 * f[i] - 59 * f[i - 1] + 37 * f[i - 2] -
                                   9 * f[i - 3])
    fpred  <- df(x[i + 1], y[i + 1])
    y[i + 1] <- y[i] + h / 24 * (9 * fpred + 19 * f[i] - 5 *f[i - 1] + f[i - 2])
    f[i + 1] <- df(x[i + 1], y[i + 1])
  }
  return (cbind(x, y))
}

df <- function(x, y) {
  return ((cos(y)) / (1.5 + x) + 0.1 * y ^ 2)
  # return (1 - sin(1.75 * x + y) + (0.1 * y) / (x + 2))
}

a <- 0
b <- 1
y <- 0.5
n <- 10