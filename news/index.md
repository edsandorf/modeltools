# Changelog

## modeltools (development version)

### Bug fixes

- [`add_scores()`](https://modeltools.edsandorf.me/reference/add_scores.md)
  now log transforms the likelihood function for `bgw_mle` objects (new
  argument `log_transform`, default `is_bgw(object)`). Previously the
  scores were derivatives of the probabilities, which made the robust
  variance-covariance matrix wrong for `bgw_mle` objects. `x` now
  defaults to `coef(object)`.
- [`is_bgw()`](https://modeltools.edsandorf.me/reference/is_bgw.md) uses
  [`inherits()`](https://rdrr.io/r/base/class.html) and no longer fails
  for objects with multiple classes, e.g., `maxLik`.
- [`converged()`](https://modeltools.edsandorf.me/reference/converged.md)
  is now an S3 generic with methods for `bgw_mle` and `maxLik`.
  `bgw_mle` return code 3 (X-convergence) and 6 (absolute function
  convergence) are now treated as converged.
- [`search_starting_values()`](https://modeltools.edsandorf.me/reference/search_starting_values.md):
  names are preserved, `n_best = 1` returns a list with one vector,
  `n_best` is capped at the number of candidates, and the original
  starting values are always included as a candidate. Arguments renamed
  from `log_lik` and `return` to `prob_fn` and `n_best` (breaking).
- `load_packages(install_missing = FALSE)` warns and skips missing
  packages instead of erroring, and returns the loaded packages
  invisibly.
- [`poe_test()`](https://modeltools.edsandorf.me/reference/poe_test.md)
  errors on empty input and missing values, uses a sort-based algorithm
  with linear memory use, and
  [`print()`](https://rdrr.io/r/base/print.html) returns the object
  invisibly.
- [`suggest_parallel()`](https://modeltools.edsandorf.me/reference/suggest_parallel.md)
  suggests at least one core.
- Test helpers no longer call
  [`library()`](https://rdrr.io/r/base/library.html), which broke
  `devtools::load_all()` and `devtools::test()`.

### Interoperability

- `bgw_mle` objects now have
  [`estfun()`](https://zeileis.codeberg.page/sandwich/reference/estfun.html)
  and
  [`bread()`](https://zeileis.codeberg.page/sandwich/reference/bread.html)
  methods for the `sandwich` package.
  [`sandwich()`](https://zeileis.codeberg.page/sandwich/reference/sandwich.html),
  [`meat()`](https://zeileis.codeberg.page/sandwich/reference/meat.html),
  [`bread()`](https://zeileis.codeberg.page/sandwich/reference/bread.html),
  [`estfun()`](https://zeileis.codeberg.page/sandwich/reference/estfun.html)
  and
  [`vcovCL()`](https://zeileis.codeberg.page/sandwich/reference/vcovCL.html)
  are re-exported from `sandwich`, replacing the package’s own
  [`sandwich()`](https://zeileis.codeberg.page/sandwich/reference/sandwich.html),
  [`meat()`](https://zeileis.codeberg.page/sandwich/reference/meat.html)
  and
  [`bread()`](https://zeileis.codeberg.page/sandwich/reference/bread.html)
  (breaking: `bread(x, n)` is now `bread(x)`).
  [`bread()`](https://zeileis.codeberg.page/sandwich/reference/bread.html)
  warns when the variance-covariance matrix is based on BHHH, in which
  case the robust variance-covariance matrix equals `vcov(x)`.
- [`logLik()`](https://rdrr.io/r/stats/logLik.html) returns an object of
  class `logLik`, so [`AIC()`](https://rdrr.io/r/stats/AIC.html),
  [`BIC()`](https://rdrr.io/r/stats/AIC.html) and likelihood ratio tests
  use the `stats` defaults. The
  [`AIC()`](https://rdrr.io/r/stats/AIC.html) and
  [`BIC()`](https://rdrr.io/r/stats/AIC.html) methods for `bgw_mle` are
  removed.
- [`tidy()`](https://generics.r-lib.org/reference/tidy.html) returns
  `std.error` instead of `std.err` (broom convention, breaking),
  calculates standard errors from a new `vcov` argument, and uses normal
  p-values. Use `tidy(x, vcov = sandwich(x))` or
  `prep_for_gt(x, vcov = sandwich(x))` for robust standard errors.

### Housekeeping

- The startup message only checks GitHub for the latest version in
  interactive sessions, with a 2 second timeout, and reads from the
  `main` branch.
- `bgw` and `maxLik` moved to Suggests, `lifecycle` removed, `rlang` and
  `sandwich` added to Imports.
- README rewritten and rendered, and a `render-rmarkdown` GitHub Action
  added (from `usethis::use_github_action()`, using
  `setup-r-dependencies` instead of `setup-renv`).
- GitHub Actions updated to `actions/checkout@v7`,
  `codecov/codecov-action@v7`, `actions/upload-artifact@v7` and
  `JamesIves/github-pages-deploy-action@v4.9.0`.
- Expanded unit tests.

## modeltools v0.0.1

- Added function to do a simple search for starting values
- Added functions to help with parallel
- Added functions to calculate the sandwich estimator for the covariance
  matrix when the model has been solved using BGW.
- Added function to add the scores to a BGW object
- Added broom generics for BGW objects
- Added function for the Poe et al. (2005) test
