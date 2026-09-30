# Function to search for starting values

The function takes a vector of starting values. These will then be
adjusted by adding a random uniform value between -1 and 1 multiplied by
an adjustment multiplier. The function then evaluates the log-likelihood
for each set of starting values and returns the specified number of best
fitting starting values. The original starting values are always
included as a candidate.

## Usage

``` r
search_starting_values(
  prob_fn,
  starting_values,
  N = 10000,
  n_best = 10,
  adjustment_multiplier = 1
)
```

## Arguments

- prob_fn:

  A likelihood function returning the observation-level likelihoods
  (probabilities), i.e., the function passed to
  [`bgw::bgw_mle()`](https://rdrr.io/pkg/bgw/man/bgw_mle.html). The
  function is evaluated as `sum(log(prob_fn(param)))`.

- starting_values:

  A (named) vector of starting values. Names are preserved in the
  output.

- N:

  The number of starting value candidates to use. Default is 10,000

- n_best:

  Number of vectors to return. Default is 10

- adjustment_multiplier:

  An adjustment multiplier for the random uniform adjustment matrix. The
  default value is 1.

## Value

A list of the `n_best` best fitting starting value vectors, ordered from
best to worst.
