#' Check if model has converged
#'
#' Using the return code from the optimizer, check if the code indicates that
#' the optimization converged to a local optimum.
#'
#' Objects of class `bgw_mle` are considered converged for the following codes:
#' 3 - X-convergence
#' 4 - Relative function convergence
#' 5 - X- and relative function convergence
#' 6 - Absolute function convergence
#'
#' Objects of class `maxLik` are considered converged for the following codes:
#' 0 - Successful convergence (optim based methods, e.g. BFGS)
#' 1 - Gradient close to zero
#' 2 - Successive function values within tolerance limit
#' 8 - Successive function values within relative tolerance limit
#'
#' @param x A model object
#' @param ... Additional arguments
#'
#' @return A logical value indicating if the model has converged
#'
#' @export
converged <- function(x, ...) {
  UseMethod("converged")
}

#' @rdname converged
#' @export
converged.bgw_mle <- function(x, ...) {
  return(
    x$code %in% c(3, 4, 5, 6)
  )
}

#' @rdname converged
#' @export
converged.maxLik <- function(x, ...) {
  return(
    x$code %in% c(0, 1, 2, 8)
  )
}

#' @rdname converged
#' @export
converged.default <- function(x, ...) {
  cli_abort("{.fn converged} is not implemented for objects of class {.cls {class(x)}}.")
}

#' Check if the model object is of class `bgw_mle`
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
    inherits(x, "bgw_mle")
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
