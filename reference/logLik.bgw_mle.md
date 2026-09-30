# Log-likelihood of a `bgw_mle` object

Retrieve the log likelihood value of the model. The value is returned as
an object of class `logLik` such that
[`stats::AIC()`](https://rdrr.io/r/stats/AIC.html),
[`stats::BIC()`](https://rdrr.io/r/stats/AIC.html) and likelihood ratio
tests work out of the box.

## Usage

``` r
# S3 method for class 'bgw_mle'
logLik(object, ...)
```

## Arguments

- object:

  An object of class `bgw_mle`

- ...:

  Additional arguments

## Value

An object of class `logLik` with attributes `df` (number of parameters)
and `nobs` (number of observations)
