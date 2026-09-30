test_that("scores() correctly throws errors if the object does not contain a scores matrix", {
  expect_error(scores(bgw_model))
  expect_error(scores(maxlik_model))
  expect_error(scores(list(x = runif(10))))
})

test_that("scores() correctly returns the scores matrix", {
  expect_equal(scores(bgw_modified_model), bgw_modified_model$scores)
  expect_equal(scores(maxlik_modified_model), maxlik_modified_model$scores)
})

test_that("estfun() and bread() methods are registered for bgw_mle", {
  expect_equal(estfun(bgw_fd_modified_model), scores(bgw_fd_modified_model))
  expect_equal(bread(bgw_fd_modified_model), vcov(bgw_fd_model) * nobs(bgw_fd_model))
})

test_that("bread() warns when the vcov is based on BHHH", {
  expect_warning(bread(bgw_modified_model), "BHHH")
  expect_no_warning(bread(bgw_fd_modified_model))
})

test_that("sandwich() equals V %*% crossprod(scores) %*% V", {
  V <- vcov(bgw_fd_model)
  S <- scores(bgw_fd_modified_model)
  expect_equal(sandwich(bgw_fd_modified_model), V %*% crossprod(S) %*% V)
})

test_that("sandwich() equals vcov() under BHHH", {
  expect_equal(
    suppressWarnings(sandwich(bgw_modified_model)),
    vcov(bgw_model),
    tolerance = 1e-3
  )
})

test_that("estfun() for bgw_mle matches the sandwich package on the equivalent maxLik model", {
  expect_equal(
    estfun(bgw_fd_modified_model),
    sandwich::estfun(maxlik_model),
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
})

test_that("sandwich() for bgw_mle matches a sandwich built from the exact Hessian", {
  # maxLik's own Hessian approximation differs by ~1% on this small sample,
  # so the reference is built from numDeriv::hessian() directly.
  H_inv <- solve(-numDeriv::hessian(function(b) sum(maxlik_log_lik(b)), coef(bgw_fd_model)))
  S <- scores(bgw_fd_modified_model)
  expect_equal(
    sandwich(bgw_fd_modified_model),
    H_inv %*% crossprod(S) %*% H_inv,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})

test_that("meat() applies the finite sample adjustment", {
  n <- nobs(bgw_fd_model)
  k <- length(coef(bgw_fd_model))
  expect_equal(
    meat(bgw_fd_modified_model, adjust = TRUE),
    meat(bgw_fd_modified_model) * n / (n - k)
  )
})

test_that("vcovCL() works with bgw_mle objects", {
  vc <- vcovCL(bgw_fd_modified_model, cluster = rep(1:5, each = 2), type = "HC0", cadjust = FALSE)
  S <- rowsum(scores(bgw_fd_modified_model), rep(1:5, each = 2))
  V <- vcov(bgw_fd_model)
  expect_equal(vc, V %*% crossprod(S) %*% V)
})
