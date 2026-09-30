#' @importFrom generics tidy
#' @export
generics::tidy

#' Tidy a `bgw_mle` object
#'
#' Standard errors are calculated from the supplied variance-covariance matrix
#' and p-values are based on the asymptotic normal distribution.
#'
#' @param x An object of class `bgw_mle`
#' @param vcov A variance-covariance matrix used to calculate the standard
#' errors. Defaults to `vcov(x)`. Use, e.g., `sandwich(x)` for robust standard
#' errors.
#' @param ... Additional arguments
#'
#' @method tidy bgw_mle
#'
#' @return A tidy [tibble::tibble()] with the components of the `bgw_mle` object
#'
#' @export
tidy.bgw_mle <- function(x, vcov = stats::vcov(x), ...) {
  estimate <- coef(x)
  std_error <- sqrt(diag(vcov))
  statistic <- estimate / std_error

  return(
    tibble::tibble(
      term = names(estimate),
      estimate = unname(estimate),
      std.error = unname(std_error),
      statistic = unname(statistic),
      p.value = 2 * stats::pnorm(-abs(statistic))
    )
  )
}

#' @importFrom generics glance
#' @export
generics::glance

#' Glance a `bgw_mle` object
#'
#' @param x An object of class `bgw_mle`
#' @param ... Additional arguments
#'
#' @method glance bgw_mle
#'
#' @return A [tibble::tibble()] with the components of the `bgw_mle` object
#'
#' @export
glance.bgw_mle <- function(x, ...) {
  return(
    tibble::tibble(
      log_lik = as.numeric(logLik(x)),
      aic = AIC(x),
      bic = BIC(x),
      nobs = nobs(x),
      k = length(coef(x))
    )
  )
}
