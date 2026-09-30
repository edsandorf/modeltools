test_that("tidy() returns broom compatible column names", {
  expect_named(tidy(bgw_model), c("term", "estimate", "std.error", "statistic", "p.value"))
})

test_that("tidy() calculates standard errors, statistics and p-values from vcov", {
  res <- tidy(bgw_model)
  expect_equal(res$term, names(coef(bgw_model)))
  expect_equal(res$std.error, sqrt(diag(vcov(bgw_model))), ignore_attr = TRUE)
  expect_equal(res$statistic, res$estimate / res$std.error)
  expect_equal(res$p.value, 2 * pnorm(-abs(res$statistic)))
})

test_that("tidy() reproduces the bgw standard errors and t-ratios", {
  res <- tidy(bgw_model)
  expect_equal(res$std.error, bgw_model$seBGW, ignore_attr = TRUE)
  expect_equal(res$statistic, bgw_model$tstatBGW, ignore_attr = TRUE)
})

test_that("tidy() uses a user supplied vcov", {
  robust <- sandwich(bgw_fd_modified_model)
  res <- tidy(bgw_fd_model, vcov = robust)
  expect_equal(res$std.error, sqrt(diag(robust)), ignore_attr = TRUE)
})

test_that("glance() returns the model summary statistics", {
  res <- glance(bgw_model)
  expect_named(res, c("log_lik", "aic", "bic", "nobs", "k"))
  expect_equal(res$log_lik, bgw_model$maximum)
  expect_equal(res$aic, AIC(bgw_model))
  expect_equal(res$bic, BIC(bgw_model))
  expect_equal(res$nobs, nrow(db))
  expect_equal(res$k, 2)
})
