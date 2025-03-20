#' Check if model has converged
#' 
#' Using the return code from the optimizer, check if the code indicates that 
#' the optimization converged to a local optimum.
#' 
#' Objects of class bgw_mle will have the following codes:
#' 4 - Relative function convergence 
#' 5 - X- and relative function convergence
#' 7 - Singular convergence
#' 8 - False convergence
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
