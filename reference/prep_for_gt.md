# Prepare model output for `gt`

Takes a model object as an input and prepares a table with estimated
coefficients, standard errors, and stars indicating significance. Names
in `term` will be converted to lower case to ensure seamless use of left
join.

## Usage

``` r
prep_for_gt(x, ...)
```

## Arguments

- x:

  A model object

- ...:

  Additional arguments passed to `broom::tidy()` and `broom::glance()`

## Value

A tibble with estimated coefficients, standard errors, and stars.

## Details

The table can quickly chained to other tables using left_join().
