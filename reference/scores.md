# Extract the empirical estimation function

This function extracts the scores matrix from a model object of class
`bgw_mle`. The scores matrix contains the gradient observations and is
used to calculate the robust variance-covariance matrix.

## Usage

``` r
scores(x, ...)
```

## Arguments

- x:

  A model object of class `bgw_mle`

- ...:

  Additional arguments passed to methods

## Value

A matrix with one row per observation and one column per parameter
