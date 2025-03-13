#' @importFrom generics tidy
#' @export
generics::tidy

#' Tidy a `bgw_mle´ object
#' 
#' @param x An object of class `bgw_mle`
#' @param ... Additional arguments
#' 
#' @method tidy bgw_mle
#' 
#' @return A tidy [tibble::tibble()] with the components of the `bgw_mle` object
#' 
#' @export
tidy.bgw_mle <- function(x, ...) {
  return(
    tibble::tibble(
      term = names(coef(x)),
      estimate = coef(x),
      std.err = x$seBGW,
      statistic = x$tstatBGW,
      p.value = 2 * stats::pt(-abs(.data$statistic), df = nobs(x) - length(coef(x)))
    )
  )
}

#' @importFrom generics glance
#' @export
generics::glance

#' Glance a `bgw_mle´ object
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
      log_lik = logLik(x),
      aic = AIC(x),
      bic = BIC(x),
      nobs = nobs(x)
    )
  )
}
