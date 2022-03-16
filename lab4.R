myeigen <- function(mat) {
  symmetric <- isSymmetric.matrix(mat)

  if (symmetric) {
    z <- .Internal(La_rs(mat, FALSE))
    ord <- rev(seq_along(z$values))
  } else {
    z <- .Internal(La_rg(mat, FALSE))
    ord <- sort.list(Mod(z$values), decreasing = TRUE)
  }

  structure(list(
    values = z$values[ord],
    vectors = z$vectors[, ord, drop = FALSE]
  ))
}

mat <- matrix(c(3.4, 1, 1.2, 1.6, 0, 3.3, 1.3, 1.3, 3.3),
  nrow = 3, ncol = 3, byrow = TRUE
)

x <- myeigen(mat)

print(x$values)
print(x$vectors)