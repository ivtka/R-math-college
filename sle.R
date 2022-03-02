gauss <- function(A, b, n) {
  for (k in 1:(n - 1)) {
    for (j in (k + 1):n) {
      A[k, j] <- A[k, j] / A[k, k]
    }
    b[k] <- b[k] / A[k, k]
    for (i in (k + 1):n) {
      for (j in (k + 1):n) {
        A[i, j] <- A[i, j] - A[i, k] * A[k, j]
      }
      b[i] <- b[i] - A[i, k] * b[k]
    }
  }
  b[n] <- b[n] / A[n, n]
  for (k in (n - 1):1) {
    for (j in (k + 1):n) {
      b[k] <- b[k] - A[k, j] * b[j]
    }
  }

  return(b)
}

lu <- function(A, b, n) {
  L <- matrix(0, nrow = n, ncol = n)
  U <- matrix(0, nrow = n, ncol = n)

  for (i in 1:n) {
    L[i, 1] <- A[i, 1]
  }
  for (j in 1:n) {
    U[1, j] <- A[1, j] / L[1, 1]
  }

  for (i in 1:n) {
    for (j in 1:n) {
      s <- 0
      for (k in 1:i) {
        s <- s + L[i, k] * U[k, j]
      }
      if ((i >= j) && (j > 1)) {
        L[i, j] <- A[i, j] - s
      }
      if ((j > i) && (i > 1)) {
        U[i, j] <- (A[i, j] - s) / L[i, i]
      }
    }
    U[i, i] <- 1
  }

  y <- solve(L, b)
  x <- solve(U, y)

  return(x)
}

sqroot <- function(A, b, n) {
  U <- matrix(0, nrow = n, ncol = n)
  U[1, 1] <- sqrt(A[1, 1])
  for (j in 2:n) {
    U[1, j] <- A[1, j] / U[1, 1]
  }
  for (i in 2:n) {
    s <- 0
    for (k in 1:(i - 1)) {
      s <- s + U[k, i]^2
    }
    U[i, i] <- sqrt(A[i, i] - s)
    for (j in 2:n) {
      if (i < j) {
        s <- 0
        for (k in 1:(i - 1)) {
          s <- s + U[k, i] * U[k, j]
        }
        U[i, j] <- (A[i, j] - s) / U[i, i]
      }
      if (i > j) {
        U[i, j] <- 0
      }
    }
  }

  y <- solve(t(U), b)
  x <- solve(U, y)

  return(x)
}
