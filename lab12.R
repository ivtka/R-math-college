# u(x) = x + integral xtu(t)dt, u0(x)= sin x - 1

fcore <- function(x, t) {
  return (x*t)
}

f <- function(x) {
  return (x)
}

met_trap <- function(x, y, n) {
  h <- x[2] - x[1]
  s <- (y[1] + y[n]) / 2
  for (i in 2:(n - 1)) s <- s + y[i]
  return (h * s)
}

norm <- function(x) {
  return (sqrt(crossprod(x, x)))
}

a <- 0
b <- 0.2
lambda <- 1


n <- 10
eps <- 1e-5
h <- (b - a) / (n - 1)
x <- 1:n
for (i in 1:n) 
  x[i] <- a + h * (i - 1)

t <- 1:n
t <- x

k <- matrix(0, ncol = n, nrow = n)
for (i in 1:n) {
  for (j in 1:n) {
    k[i, j] = fcore(x[i], t[j])
  }
}

f_i <- 1:n
f_next <- 1:n

for (i in 1:n) {
  f_i[i] <- f(x[i])
}
y <- 1:n
for (ki in 1:100) {
  for (i in 1:n) {
    for (j in 1:n) {
      y[j] = k[i, j] * f_i[j]
    }
    f_next[i] = f(x[i]) + lambda * met_trap(t, y, n)
  }
  if (norm(f_next - f_i) <= eps) {
    break
  }
  f_i <- f_next
}

plot(x, f_next, type="p", col="red")
t <- seq(a, b, len = 10)
for (i in 1:10) {
  y[i] <- sin(t[i]) - 1 
}
lines(t, y, type="l", col="green")