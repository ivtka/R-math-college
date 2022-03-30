f <- function(x) {
  return (4^x*(7.5-x^2)^(1/3))
}

x <- 2.5
h <- 0.001

df <- c((f(x + h) - f(x)) / h,
        (f(x + h) - f(x - h)) / (2 * h),
        (f(x) - f(x - h)) / h)