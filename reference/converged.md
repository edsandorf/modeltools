# Check if model has converged

Using the return code from the optimizer, check if the code indicates
that the optimization converged to a local optimum.

## Usage

``` r
converged(x, ...)

# S3 method for class 'bgw_mle'
converged(x, ...)

# S3 method for class 'maxLik'
converged(x, ...)

# Default S3 method
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

Objects of class `bgw_mle` are considered converged for the following
codes: 3 - X-convergence 4 - Relative function convergence 5 - X- and
relative function convergence 6 - Absolute function convergence

Objects of class `maxLik` are considered converged for the following
codes: 0 - Successful convergence (optim based methods, e.g. BFGS) 1 -
Gradient close to zero 2 - Successive function values within tolerance
limit 8 - Successive function values within relative tolerance limit
