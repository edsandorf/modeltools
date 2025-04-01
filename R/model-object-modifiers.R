#' Calculates and adds the scores to a fitted model object
#' 
#' Calculates and adds the scores to a fitted model object of.
#' The scores are the gradient observations/first derivatives of the log-likelihood
#' function. The function is a wrapper around [numDeriv::jacobian()].
#' 
#' @param object A fitted model object of class
#' @param func A function with real (vector) results. This is typically the 
#' log-likelihood function. The function must return the function values at the
#' observation level.
#' @param x A real or real vector argument to func, indicating the point at
#' which the gradient is to be calculated.
#' @param ... Additional arguments passed to [numDeriv::jacobian()].
#' 
#' @return A fitted model object with the scores added to the
#' object.
#' 
#' @export
add_scores <- function(object, func, x, ...) {
  # Calculate the scores using the jacobian function
  object$scores <- tryCatch({
    jacobian(func, x, ...)
    
  }, error = function(e) {
    stop("Error in calculating the scores: ", e$message)
    
  })
  
  # Add column names
  colnames(object$scores) <- names(x)
  
  return(
    object
  )
}
