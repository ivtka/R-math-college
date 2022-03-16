# Варіант 35

x <- c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0)
y <- c(-4.2, -2.8, -2.2, -1.6, -1.3, -0.9)
n <- 6

fun_model <- function(x, a) {
  return(a[1] + a[2] * x + a[3] * x^2 + a[4] * x^3)
}

fun_mnk <- function(a) {
  s <- 0
  for (i in 1:n) {
    s <- s + (y[i] - fun_model(x[i], a))^2
  }
  return(s)
}

a0 <- c(0, 0, 0, 0)

res <- optim(fn = fun_mnk, par = a0)
a1 <- res$par

ym <- fun_model(x, a1)

r <- y - ym
a <- 0.5
b <- 3.0
num <- 100
h <- (b - a) / (n - 1)
x1 <- c(1:n)
for (i in 1:n) {
  x1[i] <- a + h * (i - 1)
}
y1m <- fun_model(x1, a1)

par(mfrow = c(1, 2))
plot(x, y, type = "p", col = "red")
lines(x1, y1m, col = "blue")
plot(x, r, type = "p")
lines(x1, r, col = "blue")
abline(h = 0)
abline(v = 0)
