# Test for the difference in independent empirical distributions

The function calculates the complete combinatorial of the supplied
vectors using a loop implementation by taking the difference between
every combination of the elements in the vectors. The input vectors must
be numeric, but can be of different lengths.

## Usage

``` r
poe_test(x, y)
```

## Arguments

- x, y:

  Two numeric vectors representing the independent empriical
  distributions to be compared

## Value

A list with the following components:

- `method` The name of the test

- `statistic` The test statistic

- `means` The means of the two input vectors

## References

Poe G. L., Giraud, K. L. & Loomis, J. B., 2005, Computational methods
for measuring the difference of empirical distributions, American
Journal of Agricultural Economics, 87:353-365

## Examples

``` r
x <- qnorm(runif(100), mean = -0.5, sd = 1)
y <- qnorm(runif(100), mean = 1.5, sd = 2)
poe_test(x, y)
#> Method:  Poe et al. (2005) test 
#> 
#> Means: 
#>      x      y 
#> -0.577  1.960 
#> 
#> H0: x = y 
#> H1: x > y | x < y 
#> 
#> Gamma:  0.8631 
#> 
#> Gamma >.95 and <.05 indicates difference at the 5% level. 
 
```
