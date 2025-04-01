#' Check if model has converged
#' 
#' Using the return code from the optimizer, check if the code indicates that 
#' the optimization converged to a local optimum.
#' 
#' Objects of class `bgw_mle` will have the following codes:
#' 0 - Initial f(x) cannot be computed
#' 4 - Relative function convergence 
#' 5 - X- and relative function convergence
#' 7 - Singular convergence
#' 8 - False convergence
#' 9 - Function evaluation limit
#' 
#' @param x A model object
#' @param ... Additional arguments
#' 
#' @return A logical value indicating if the model has converged
#' 
#' @export
converged <- function(x, ...) {
  
  boolean <- switch(
    class(x),
    bgw_mle = ifelse(x$code %in% c(4, 5), TRUE, FALSE)
  )

  return(
    boolean
  )
}

#' Check if the model object is of class `bwg_mle`
#' 
#' A simple function checking the class of the object. The function is primarily
#' used to control flow in other functions. 
#' 
#' @param x A model object
#' 
#' @return A Boolean value equal to TRUE if the object is of class `bgw_mle`.
#' 
#' @export
is_bgw <- function(x) {
  return(
    class(x) == "bgw_mle"
  )
}

#' Check if the model object contains the scores
#' 
#' The scores matrix contains the gradient observations and are used to calculate
#' the robust variance-covariance matrix. The function checks whether the scores
#' matrix is present in the model object. The function is primarily used to 
#' control the flow in other functions.
#' 
#' @inheritParams is_bgw
#' 
#' @return A boolean value equal to TRUE if the object contains the scores.
#' 
#' @export
has_scores <- function(x) {
  return(
    "scores" %in% names(x)
  )
}


