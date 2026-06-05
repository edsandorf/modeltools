# Check if model has converged

Using the return code from the optimizer, check if the code indicates
that the optimization converged to a local optimum.

## Usage

``` r
converged(x, ...)
```

## Arguments

- x:

  A model object

- ...:

  Additional arguments

## Value

A logical value indicating if the model has converged

## Details

Objects of class `bgw_mle` will have the following codes: 0 - Initial
f(x) cannot be computed 4 - Relative function convergence 5 - X- and
relative function convergence 7 - Singular convergence 8 - False
convergence 9 - Function evaluation limit
