# modeltools (development version)

## Bug fixes
* `add_scores()` now log transforms the likelihood function for `bgw_mle` 
objects (new argument `log_transform`, default `is_bgw(object)`). Previously the
scores were derivatives of the probabilities, which made the robust 
variance-covariance matrix wrong for `bgw_mle` objects. `x` now defaults to 
`coef(object)`.
* `is_bgw()` uses `inherits()` and no longer fails for objects with multiple 
classes, e.g., `maxLik`.
* `converged()` is now an S3 generic with methods for `bgw_mle` and `maxLik`. 
`bgw_mle` return code 3 (X-convergence) and 6 (absolute function convergence) 
are now treated as converged.
* `search_starting_values()`: names are preserved, `n_best = 1` returns a list
with one vector, `n_best` is capped at the number of candidates, and the 
original starting values are always included as a candidate. Arguments 
renamed from `log_lik` and `return` to `prob_fn` and `n_best` (breaking).
* `load_packages(install_missing = FALSE)` warns and skips missing packages 
instead of erroring, and returns the loaded packages invisibly.
* `poe_test()` errors on empty input and missing values, uses a sort-based 
algorithm with linear memory use, and `print()` returns the object invisibly.
* `suggest_parallel()` suggests at least one core.
* Test helpers no longer call `library()`, which broke `devtools::load_all()`
and `devtools::test()`.

## Interoperability
* `bgw_mle` objects now have `estfun()` and `bread()` methods for the 
`sandwich` package. `sandwich()`, `meat()`, `bread()`, `estfun()` and 
`vcovCL()` are re-exported from `sandwich`, replacing the package's own 
`sandwich()`, `meat()` and `bread()` (breaking: `bread(x, n)` is now `bread(x)`).
`bread()` warns when the variance-covariance matrix is based on BHHH, in which
case the robust variance-covariance matrix equals `vcov(x)`.
* `logLik()` returns an object of class `logLik`, so `AIC()`, `BIC()` and
likelihood ratio tests use the `stats` defaults. The `AIC()` and `BIC()` 
methods for `bgw_mle` are removed.
* `tidy()` returns `std.error` instead of `std.err` (broom convention, 
breaking), calculates standard errors from a new `vcov` argument, and uses 
normal p-values. Use `tidy(x, vcov = sandwich(x))` or 
`prep_for_gt(x, vcov = sandwich(x))` for robust standard errors.

## Housekeeping
* The startup message only checks GitHub for the latest version in interactive
sessions, with a 2 second timeout, and reads from the `main` branch.
* `bgw` and `maxLik` moved to Suggests, `lifecycle` removed, `rlang` and 
`sandwich` added to Imports.
* README rewritten and rendered, and a `render-rmarkdown` GitHub Action added
(from `usethis::use_github_action()`, using `setup-r-dependencies` instead of 
`setup-renv`).
* GitHub Actions updated to `actions/checkout@v7`, `codecov/codecov-action@v7`,
`actions/upload-artifact@v7` and `JamesIves/github-pages-deploy-action@v4.9.0`.
* Expanded unit tests.

# modeltools v0.0.1
* Added function to do a simple search for starting values
* Added functions to help with parallel
* Added functions to calculate the sandwich estimator for the covariance matrix
when the model has been solved using BGW. 
* Added function to add the scores to a BGW object
* Added broom generics for BGW objects
* Added function for the Poe et al. (2005) test
