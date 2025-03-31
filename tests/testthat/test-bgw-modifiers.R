test_that("add_scores() adds the scores matrix to the model object", {
  expect_true("scores" %in% names(add_scores(model, log_lik, coef(model))))
})

test_that("add_scores() accepts changes to method", {
  expect_equal(
    jacobian(log_lik, coef(model), method = "simple"),
    add_scores(model, log_lik, coef(model), method = "simple")$scores,
    ignore_attr = TRUE
  )
})

