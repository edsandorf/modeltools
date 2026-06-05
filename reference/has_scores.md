# Check if the model object contains the scores

The scores matrix contains the gradient observations and are used to
calculate the robust variance-covariance matrix. The function checks
whether the scores matrix is present in the model object. The function
is primarily used to control the flow in other functions.

## Usage

``` r
has_scores(x)
```

## Arguments

- x:

  A model object

## Value

A boolean value equal to TRUE if the object contains the scores.
