# Sandwich estimator methods for `bgw_mle` objects

Methods for the
[`sandwich::estfun()`](https://zeileis.codeberg.page/sandwich/reference/estfun.html)
and
[`sandwich::bread()`](https://zeileis.codeberg.page/sandwich/reference/bread.html)
generics. These make all variance-covariance estimators in the
`sandwich` package available for objects of class `bgw_mle`, e.g.,
[`sandwich::sandwich()`](https://zeileis.codeberg.page/sandwich/reference/sandwich.html)
for the robust variance-covariance matrix and
[`sandwich::vcovCL()`](https://zeileis.codeberg.page/sandwich/reference/vcovCL.html)
for clustering at the individual level. The scores must first be added
to the model object using
[`add_scores()`](https://modeltools.edsandorf.me/reference/add_scores.md).

## Usage

``` r
# S3 method for class 'bgw_mle'
estfun(x, ...)

# S3 method for class 'bgw_mle'
bread(x, ...)
```

## Arguments

- x:

  A model object of class `bgw_mle`

- ...:

  Additional arguments passed to methods

## Value

[`estfun()`](https://zeileis.codeberg.page/sandwich/reference/estfun.html)
returns the scores matrix.
[`bread()`](https://zeileis.codeberg.page/sandwich/reference/bread.html)
returns the variance-covariance matrix multiplied by the number of
observations.

## Details

Note that the default Hessian approximation in
[`bgw::bgw_mle()`](https://rdrr.io/pkg/bgw/man/bgw_mle.html) is BHHH, in
which case the robust variance-covariance matrix equals `vcov(x)`.
Estimate the model with
`bgw_settings = list(vcHessianMethod = "finiteDifferences")` to obtain a
robust variance-covariance matrix that differs from `vcov(x)`.

## Examples

``` r
if (FALSE) { # \dontrun{
  model <- add_scores(model, log_lik)
  sandwich(model)
  sandwich(model, adjust = TRUE)
  vcovCL(model, cluster = db$id)
} # }
```
