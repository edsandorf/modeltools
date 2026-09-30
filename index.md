# modeltools

The goal of modeltools is to provide small, reusable tools for
econometric modeling in R, with a particular focus on choice models
estimated with [bgw](https://CRAN.R-project.org/package=bgw). Model
objects work with the `stats`, `broom` and `sandwich` generics, so
standard tools such as [`AIC()`](https://rdrr.io/r/stats/AIC.html),
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`sandwich()`](https://zeileis.codeberg.page/sandwich/reference/sandwich.html)
work out of the box.

## Installation

You can install the development version of modeltools from
[GitHub](https://github.com/) with:

``` r

# install.packages("devtools")
devtools::install_github("edsandorf/modeltools")
```

## Example

Robust standard errors for a model estimated with `bgw_mle()`:

``` r

library(modeltools)

model <- bgw::bgw_mle(
  log_lik, 
  betaStart = beta, 
  bgw_settings = list(vcHessianMethod = "finiteDifferences")
)
model <- add_scores(model, log_lik)

tidy(model, vcov = sandwich(model))
prep_for_gt(model, vcov = vcovCL(model, cluster = db$id))
```

Comparing two empirical distributions using the Poe et al. (2005) test:

``` r

library(modeltools)

set.seed(123)
wtp_a <- rnorm(1000, mean = 0.5)
wtp_b <- rnorm(1000, mean = 1)
poe_test(wtp_a, wtp_b)
#> Method:  Poe et al. (2005) test 
#> 
#> Means: 
#> wtp_a wtp_b 
#> 0.516 1.042 
#> 
#> H0: x = y 
#> H1: x > y | x < y 
#> 
#> Gamma:  0.646704 
#> 
#> Gamma >.95 and <.05 indicates difference at the 5% level.
```
