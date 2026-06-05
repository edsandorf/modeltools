# Repeat rows

This function repeats the rows of a
[matrix](https://rdrr.io/r/base/matrix.html),
[data.frame](https://rdrr.io/r/base/data.frame.html) or
[tibble](https://tibble.tidyverse.org/reference/tibble-package.html) by
a given number of times.

## Usage

``` r
repeat_rows(x, n)
```

## Arguments

- x:

  A [matrix](https://rdrr.io/r/base/matrix.html),
  [data.frame](https://rdrr.io/r/base/data.frame.html) or
  [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)

- n:

  The number of times to repeat each row

## Value

A [matrix](https://rdrr.io/r/base/matrix.html),
[data.frame](https://rdrr.io/r/base/data.frame.html) or
[tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
depending on the type of the input

## Details

In case of a data.frame, new row names are preserved.

## Examples

``` r
repeat_rows(matrix(1:4, nrow = 2), 2)
#>      [,1] [,2]
#> [1,]    1    3
#> [2,]    1    3
#> [3,]    2    4
#> [4,]    2    4
repeat_rows(tibble::tibble(a = 1:2, b = 3:4), 2)
#> # A tibble: 4 × 2
#>       a     b
#>   <int> <int>
#> 1     1     3
#> 2     1     3
#> 3     2     4
#> 4     2     4
repeat_rows(data.frame(a = 1:2, b = 3:4), 2)
#>     a b
#> 1   1 3
#> 1.1 1 3
#> 2   2 4
#> 2.1 2 4
```
