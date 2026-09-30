#' Calculates and adds the scores to a fitted model object
#'
#' Calculates and adds the scores to a fitted model object.
#' The scores are the gradient observations/first derivatives of the
#' observation-level log-likelihood function. The function is a wrapper around
#' [numDeriv::jacobian()].
#'
#' @param object A fitted model object
#' @param func A function with real (vector) results returning the function
#' values at the observation level. This is either the log-likelihood function,
#' or, as expected by [bgw::bgw_mle()], the likelihood function (probabilities).
#' @param x A real or real vector argument to func, indicating the point at
#' which the gradient is to be calculated. Defaults to the estimated
#' coefficients.
#' @param log_transform A logical value indicating if `func` returns
#' likelihoods that must be log transformed before differentiating. Defaults
#' to `TRUE` for objects of class `bgw_mle` and `FALSE` otherwise.
#' @param ... Additional arguments passed to [numDeriv::jacobian()].
#'
#' @return A fitted model object with the scores added to the
#' object.
#'
#' @export
add_scores <- function(object, func, x = coef(object),
                       log_transform = is_bgw(object), ...) {
  f <- if (log_transform) function(param) log(func(param)) else func

  # Calculate the scores using the jacobian function
  object$scores <- tryCatch({
    jacobian(f, x, ...)

  }, error = function(e) {
    cli_abort("Error in calculating the scores.", parent = e)

  })

  # Add column names
  colnames(object$scores) <- names(x)

  return(
    object
  )
}
