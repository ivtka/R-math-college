sle <- modules::use("sle.R")

A <- cbind(c(3.67, 0.68, -1.21), c(0.68, 2.71, -0.96), c(-1.21, -0.96, 2.69))
b <- c(2.467, 3.825, 5.513)

print(sle$gauss(A, b, 3))
print(sle$lu(A, b, 3))
print(sle$sqroot(A, b, 3))
