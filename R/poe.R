#' Test for the difference in independent empirical distributions
#'
#' @description
#' The function calculates the complete combinatorial of the supplied vectors
#' using a loop implementation by taking the difference between every 
#' combination of the elements in the vectors. The input vectors must 
#' be numeric, but can be of different lengths.
#' 
#' @param x,y Two numeric vectors representing the independent empriical 
#' distributions to be compared
#' 
#' @returns
#' A list with the following components:
#' * `method` The name of the test
#' * `statistic` The test statistic
#' * `means` The means of the two input vectors
#' 
#' @references 
#' Poe G. L., Giraud, K. L. & Loomis, J. B., 2005, Computational methods for 
#' measuring the difference of empirical distributions, American Journal of
#'  Agricultural Economics, 87:353-365
#'  
#' @examples 
#' x <- qnorm(runif(100), mean = -0.5, sd = 1)
#' y <- qnorm(runif(100), mean = 1.5, sd = 2)
#' poe_test(x, y)
#'  
#' @export
poe_test <- function(x, y) {
  # Check input type to ensure they are numeric
  if (!is.numeric(x)) cli_abort("'x' must be numeric")
  if (!is.numeric(y)) cli_abort("'y' must be numeric")
  
  n <- length(x)
  m <- length(y)
  
  condition <- rep(NA, n)
  for (n in seq_len(n)) {
    condition[n] <- sum((x[n] - y) <= 0)
  }
  
  # Create a list of outputs
  test_results <- list(
    method = "Poe et al. (2005) test",
    statistic = sum(condition) * (1 / (n * m)),
    means = setNames(
      c(mean(x), mean(y)),
      c(deparse(substitute(x)), 
        deparse(substitute(y)))
    )
  )
  
  class(test_results) <- "poe_test"
  
  return(
    test_results
  )
}

#' Generic print method for `poe_test()`
#'
#' @param x An object of class `poe_test`
#' @param ... Additional arguments
#'
#' @export
print.poe_test <- function(x, ...) {
  cat("Method: ", x$method, "\n\n")
  cat("Means: \n")
  print(x$means, digits = 3)
  cat("\n")
  cat("H0: x = y \n")
  cat("H1: x > y | x < y \n\n")
  cat("Gamma: ", x$statistic, "\n\n")
  cat("Gamma >.95 and <.05 indicates difference at the 5% level. \n")
  
}
