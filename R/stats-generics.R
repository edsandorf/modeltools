#' @importFrom stats logLik
#' @export
stats::logLik

#' Log-likelihood of a `bgw_mle` object
#' 
#' Retrieve the log likelihood value of the model. 
#' 
#' @param object An object of class `bgw_mle`
#' @param ... Additional arguments
#' 
#' @method logLik bgw_mle
#' 
#' @return A single double 
#'
#' @export
logLik.bgw_mle <- function(object, ...) {
  return(
    object$maximum
  )
}

#' @importFrom stats AIC
#' @export
stats::AIC

#' AIC of a `bgw_mle` object
#' 
#' Calculate the AIC of the model. 
#' 
#' @param object An object of class `bgw_mle`
#' @param ... Additional arguments
#' @param k The penalty parameter used. The default value is k = 2 for the
#' classical AIC
#' 
#' @method AIC bgw_mle
#' 
#' @return A single double
#' 
#' @export
AIC.bgw_mle <- function(object, ..., k = 2) {
  return(
    -2 * object$maximum + k * object$numParams
  )
}

#' @importFrom stats BIC
#' @export
stats::BIC

#' BIC of a `bgw_mle` object
#' 
#' Calculate the BIC of the model. 
#' 
#' @inheritParams AIC.bgw_mle
#' 
#' @method BIC bgw_mle
#' 
#' @export
BIC.bgw_mle <- function(object, ...) {
  return(
    AIC(object, ..., k = log(object$numResids))
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

