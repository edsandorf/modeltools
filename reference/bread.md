# Bread for sandwiches

An implementation of the bread for sandwiches function to use when
calculating the robust variance-covariance matrix for the sandwich
estimator when using the BGW algorithm

## Usage

``` r
bread(x, n, ...)
```

## Arguments

- x:

  A fitted model object

- n:

  The number of (clustered) observations

- ...:

  Additional arguments passed to methods

## Value

A matrix containing the expectation of the negative derivative of the
estimatino function. The default is to return the
