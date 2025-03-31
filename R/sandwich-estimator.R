#' Extract the empirical estimation function
#' 
#' This function extracts the empirical estimation function from a model object
#' of class `bgw_mle`. T
#' scores matrix from a model object. The scores matrix
#' contains the gradient observations and is used to calculate the robust
#' variance-covariance matrix.
#'
#' @param x A model object of class `bgw_mle`
#' @param ... Additional arguments passed to methods
#'
#' @export
scores <- function(x, ...) {
  if (!has_scores(x)) {
    cli_abort(
      c(
        "The model object {.var x} must contain the scores matrix, i.e., the gradient observations",
        "x" = "Add the scores to the model object using add_scores() before calling scores() again."
      )
    )
  }
  
  return(
    x$scores
  )
}

#' Meat for sandwiches
#'
#' @param x A model object of class `bgw_mle`
#' @param adjust A logical value indicating if the finite sample adjustment should
#' be applied to the meat matrix
#' @param ... Additional arguments passed to methods
#'
#' @return A matrix 
#' 
#' @export
meat <- function(x, adjust = FALSE, ...) {
  
  psi <- scores(x)
  
  k <- ncol(psi)
  n <- nrow(psi)
  
  juices <- crossprod(psi) / n
  
  # Finite sample adjustment
  if (adjust) {
    juices <- juices * (n / (n - k))
    
  }
  
  return(
    juices
  )
}

#' Bread for sandwiches
#' 
#' An implementation of the bread for sandwiches function to use when calculating
#' the robust variance-covariance matrix for the sandwich estimator when using
#' the BGW algorithm
#' 
#' @param x A fitted model object
#' @param n The number of (clustered) observations
#' @param ... Additional arguments passed to methods
#' 
#' @return A matrix containing the expectation of the negative derivative of 
#' the estimatino function. The default is to return the 
#' 
#' @export
bread <- function(x, n, ...) {
  
  crumbs <- vcov(x) * n
  
  return(
    crumbs
  )
}

#' Sandwich estimator
#' 
#' An implementation of the sandwich estimator to use when calculating the robust
#' variance-covariance matrix for the sandwich estimator when using the BGW algorithm
#' 
#' @param x A fitted model object
#' @param ... Additional arguments passed to methods
#' 
#' @return A robust variance-covariance matrix
#' 
#' @export
sandwich <- function(x, ...) {
  
  
  n <- nrow(scores(x))
  bread <- bread(x, n, ...)
  meat <- meat(x, ...)
  
  return(
    (bread %*% meat %*% bread) / n
  )
}
