# Calculates and adds the scores to a fitted model object

Calculates and adds the scores to a fitted model object of. The scores
are the gradient observations/first derivatives of the log-likelihood
function. The function is a wrapper around
[`numDeriv::jacobian()`](https://rdrr.io/pkg/numDeriv/man/jacobian.html).

## Usage

``` r
add_scores(object, func, x, ...)
```

## Arguments

- object:

  A fitted model object of class

- func:

  A function with real (vector) results. This is typically the
  log-likelihood function. The function must return the function values
  at the observation level.

- x:

  A real or real vector argument to func, indicating the point at which
  the gradient is to be calculated.

- ...:

  Additional arguments passed to
  [`numDeriv::jacobian()`](https://rdrr.io/pkg/numDeriv/man/jacobian.html).

## Value

A fitted model object with the scores added to the object.
