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
  tibble::tibble(
    term = names(x$estimate),
    estimate = x$estimate,
    std.err = x$seBGW,
    statistic = x$tstatBGW,
    p.value = 2 * stats::pt(-abs(.data$statistic), df = x$numResids)
  )
}
