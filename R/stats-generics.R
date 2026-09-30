#' @importFrom stats logLik
#' @export
stats::logLik

#' Log-likelihood of a `bgw_mle` object
#' 
#' Retrieve the log likelihood value of the model. The value is returned as an
#' object of class `logLik` such that [stats::AIC()], [stats::BIC()] and 
#' likelihood ratio tests work out of the box.
#' 
#' @param object An object of class `bgw_mle`
#' @param ... Additional arguments
#' 
#' @method logLik bgw_mle
#' 
#' @return An object of class `logLik` with attributes `df` (number of 
#' parameters) and `nobs` (number of observations)
#'
#' @export
logLik.bgw_mle <- function(object, ...) {
  return(
    structure(
      object$maximum,
      df = object$numParams,
      nobs = object$numResids,
      class = "logLik"
    )
  )
}

#' @importFrom stats coef
#' @export
stats::coef

#' Coefficients of a `bgw_mle` object
#'
#' Retrieve the estimated coefficients of the model
#' 
#' @param object An object of class `bgw_mle`
#' @param ... Additional arguments
#'
#' @method coef bgw_mle
#'
#' @return a numeric vector
#' 
#' @export
coef.bgw_mle <- function(object, ...) {
  return(
    object$estimate
  )
}

#' @importFrom stats vcov
#' @export
stats::vcov

#' Variance-covariance matrix of a `bgw_mle` object
#'
#' @param object An object of class `bgw_mle`
#' @param ... Additional arguments
#'
#' @method vcov bgw_mle
#'
#' @return A numeric matrix with the variances and covariances of the estimated
#' parameters 
#' 
#' @export
vcov.bgw_mle <- function(object, ...) {
  return(
    object$varcovBGW
  )
}

#' @importFrom stats nobs
#' @export
stats::nobs

#' Number of observations of a `bgw_mle` object
#' 
#' @param object An object of class `bgw_mle`
#' @param ... Additional arguments
#' 
#' @method nobs bgw_mle
#' 
#' @return A single integer
#' 
#' @export
nobs.bgw_mle <- function(object, ...) {
  return(
    object$numResids
  )
}

