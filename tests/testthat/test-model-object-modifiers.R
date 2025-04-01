test_that("add_scores() adds the scores matrix to the model object", {
  expect_true("scores" %in% names(add_scores(bgw_model, bgw_log_lik, coef(bgw_model))))
  expect_true("scores" %in% names(add_scores(maxlik_model, maxlik_log_lik, coef(maxlik_model))))
})

test_that("add_scores() accepts changes to method", {
  expect_equal(
    jacobian(bgw_log_lik, coef(bgw_model), method = "simple"),
    add_scores(bgw_model, bgw_log_lik, coef(bgw_model), method = "simple")$scores,
    ignore_attr = TRUE
  )
})

