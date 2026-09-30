#' Extract the empirical estimation function
#'
#' This function extracts the scores matrix from a model object of class
#' `bgw_mle`. The scores matrix contains the gradient observations and is used
#' to calculate the robust variance-covariance matrix.
#'
#' @param x A model object of class `bgw_mle`
#' @param ... Additional arguments passed to methods
#'
#' @return A matrix with one row per observation and one column per parameter
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

#' Sandwich estimator methods for `bgw_mle` objects
#'
#' Methods for the [sandwich::estfun()] and [sandwich::bread()] generics.
#' These make all variance-covariance estimators in the `sandwich` package
#' available for objects of class `bgw_mle`, e.g., [sandwich::sandwich()] for
#' the robust variance-covariance matrix and [sandwich::vcovCL()] for
#' clustering at the individual level. The scores must first be added to the
#' model object using [add_scores()].
#'
#' Note that the default Hessian approximation in [bgw::bgw_mle()] is BHHH, in
#' which case the robust variance-covariance matrix equals `vcov(x)`. Estimate
#' the model with `bgw_settings = list(vcHessianMethod = "finiteDifferences")`
#' to obtain a robust variance-covariance matrix that differs from `vcov(x)`.
#'
#' @param x A model object of class `bgw_mle`
#' @param ... Additional arguments passed to methods
#'
#' @return `estfun()` returns the scores matrix. `bread()` returns the
#' variance-covariance matrix multiplied by the number of observations.
#'
#' @examples
#' \dontrun{
#'   model <- add_scores(model, log_lik)
#'   sandwich(model)
#'   sandwich(model, adjust = TRUE)
#'   vcovCL(model, cluster = db$id)
#' }
#'
#' @name sandwich-methods
NULL

#' @rdname sandwich-methods
#' @export
estfun.bgw_mle <- function(x, ...) {
  return(
    scores(x)
  )
}

#' @rdname sandwich-methods
#' @export
bread.bgw_mle <- function(x, ...) {
  if (identical(x$hessianMethodUsed, "bhhh")) {
    cli_warn(
      c(
        "The variance-covariance matrix of {.var x} is based on the BHHH approximation.",
        "i" = "The robust variance-covariance matrix will equal {.code vcov(x)}.",
        "i" = "Use {.code bgw_settings = list(vcHessianMethod = \"finiteDifferences\")} when estimating the model."
      )
    )
  }

  return(
    vcov(x) * nobs(x)
  )
}

#' @importFrom sandwich estfun
#' @export
sandwich::estfun

#' @importFrom sandwich bread
#' @export
sandwich::bread

#' @importFrom sandwich meat
#' @export
sandwich::meat

#' @importFrom sandwich sandwich
#' @export
sandwich::sandwich

#' @importFrom sandwich vcovCL
#' @export
sandwich::vcovCL
