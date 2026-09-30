# Tidy a `bgw_mle` object

Standard errors are calculated from the supplied variance-covariance
matrix and p-values are based on the asymptotic normal distribution.

## Usage

``` r
# S3 method for class 'bgw_mle'
tidy(x, vcov = stats::vcov(x), ...)
```

## Arguments

- x:

  An object of class `bgw_mle`

- vcov:

  A variance-covariance matrix used to calculate the standard errors.
  Defaults to `vcov(x)`. Use, e.g., `sandwich(x)` for robust standard
  errors.

- ...:

  Additional arguments

## Value

A tidy
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with the components of the `bgw_mle` object
