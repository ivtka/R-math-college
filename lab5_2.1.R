x <- c(-2, -1, 0, 1, 2)
y <- c(-0.3, 0.5, 0.8, 1.8, 0.8)
n <- 5
k <- 3

fi <- function(x) {
  return(c(1, x, x^2))
}

mnk_solve <- function(x, y, n, fi, k) {
  fi_x <- matrix(nrow = n, ncol = k)
  for (i in 1:n) {
    fi_x[i, ] <- fi(x[i])
  }
  c <- matrix(nrow = k, ncol = k)
  for (j in 1:k) {
    for (m in 1:k) {
      s <- 0
      for (i in 1:n) {
        s <- s + fi_x[i, j] * fi_x[i, m]
      }
      c[j, m] <- s
    }
  }
  d <- c(1:k)
  for (j in 1:k) {
    s <- 0
    for (i in 1:n) {
      s <- s + y[i] * fi_x[i, j]
    }
    d[j] <- s
  }
  a <- solve(c, d)
  return(a)
}

a1 <- mnk_solve(x, y, n, fi, k)
print(a1)

fun_model <- function(x, a) {
  return(a[1] + a[2] * x + a[3] * x^2)
}

a <- -2
b <- 2
n <- 100
h <- (b - a) / (n - 1)
x1 <- c(1:n)
for (i in 1:n) {
  x1[i] <- a + h * (i - 1)
}
y1m <- fun_model(x1, a1)

par(mfrow = c(1, 2))
plot(x, y, type = "p", col = "red")
lines(x1, y1m, col = "blue")
