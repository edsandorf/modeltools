# Load packages

A wrapper for [`library()`](https://rdrr.io/r/base/library.html) that
can load multiple packages at once. If the package is not installed, the
function will install it first.

## Usage

``` r
load_packages(pkgs, install_missing = TRUE)
```

## Arguments

- pkgs:

  A character vector of package names

- install_missing:

  A boolean indicating whether to install missing packages. Default is
  TRUE

## Value

Invisibly, a character vector of the packages that were loaded

## Examples

``` r
if (FALSE) { # \dontrun{
   load_packages(c("dplyr", "ggplot2"))
 } # }
```
