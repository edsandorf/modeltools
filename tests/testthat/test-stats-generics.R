test_that("logLik() returns a logLik object with df and nobs", {
  ll <- logLik(bgw_model)
  expect_s3_class(ll, "logLik")
  expect_equal(as.numeric(ll), bgw_model$maximum)
  expect_equal(attr(ll, "df"), bgw_model$numParams)
  expect_equal(attr(ll, "nobs"), bgw_model$numResids)
})

test_that("logLik() matches the equivalent maxLik model", {
  expect_equal(as.numeric(logLik(bgw_model)), as.numeric(logLik(maxlik_model)), tolerance = 1e-6)
})

test_that("AIC() and BIC() use the standard formulas", {
  ll <- bgw_model$maximum
  k <- bgw_model$numParams
  n <- bgw_model$numResids
  expect_equal(AIC(bgw_model), -2 * ll + 2 * k)
  expect_equal(BIC(bgw_model), -2 * ll + log(n) * k)
})

test_that("coef(), vcov() and nobs() extract the correct components", {
  expect_equal(coef(bgw_model), bgw_model$estimate)
  expect_equal(vcov(bgw_model), bgw_model$varcovBGW)
  expect_equal(nobs(bgw_model), nrow(db))
})
