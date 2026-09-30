# Calculates and adds the scores to a fitted model object

Calculates and adds the scores to a fitted model object. The scores are
the gradient observations/first derivatives of the observation-level
log-likelihood function. The function is a wrapper around
[`numDeriv::jacobian()`](https://rdrr.io/pkg/numDeriv/man/jacobian.html).

## Usage

``` r
add_scores(object, func, x = coef(object), log_transform = is_bgw(object), ...)
```

## Arguments

- object:

  A fitted model object

- func:

  A function with real (vector) results returning the function values at
  the observation level. This is either the log-likelihood function, or,
  as expected by
  [`bgw::bgw_mle()`](https://rdrr.io/pkg/bgw/man/bgw_mle.html), the
  likelihood function (probabilities).

- x:

  A real or real vector argument to func, indicating the point at which
  the gradient is to be calculated. Defaults to the estimated
  coefficients.

- log_transform:

  A logical value indicating if `func` returns likelihoods that must be
  log transformed before differentiating. Defaults to `TRUE` for objects
  of class `bgw_mle` and `FALSE` otherwise.

- ...:

  Additional arguments passed to
  [`numDeriv::jacobian()`](https://rdrr.io/pkg/numDeriv/man/jacobian.html).

## Value

A fitted model object with the scores added to the object.
