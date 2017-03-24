# Author: Rajarshi Sarkar
#
# Linear Regression using Least Square Method

lse <- function(dependent, independent) {
  x <- dependent
  y <- independent
  # beta_hat is the matrix containing the calculated regression parameters
  beta_hat <- solve(crossprod(x)) %*% crossprod(x,y)
  beta_hat
}

# Test
x1 <- rbind(c(1, 1.1, 20), c(.36, .58, 58), c(.368, 3.68, 89), c(5.3, 6.35, 10))
y1 <- rbind(2,4,5,1)

lse(dependent = x1, independent = y1)