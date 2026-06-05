# Function to search for starting values

The function takes a vector of starting values. These will then be
adjusted by adding a random unform value between -1 and 1 multiplied by
an adjustment multiplier. The function then evaluates the log-likelihood
for each set of starting values and returns the specified number of best
fitting starting values.

## Usage

``` r
search_starting_values(
  log_lik,
  starting_values,
  N = 10000,
  return = 10,
  adjustment_multiplier = 1
)
```

## Arguments

- log_lik:

  A log likelihood function

- starting_values:

  A vector of starting values. Note: Named vectors are ignored. The
  startign values must be locally bound in the log-likelihood function
  to work correctly.

- N:

  The number of starting value candiates to use. Default is 10,000

- return:

  Number of vectors to return

- adjustment_multiplier:

  An adjustment multiplier for the random uniform adjustment matrix. The
  default value is 1.

## Value

A list of `return` number of parametrs
