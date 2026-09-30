test_that("add_scores() adds the scores matrix to the model object", {
  expect_true("scores" %in% names(add_scores(bgw_model, bgw_log_lik, coef(bgw_model))))
  expect_true("scores" %in% names(add_scores(maxlik_model, maxlik_log_lik, coef(maxlik_model))))
})

test_that("add_scores() returns one row per observation and named columns", {
  expect_equal(dim(scores(bgw_modified_model)), c(nrow(db), length(coef(bgw_model))))
  expect_equal(colnames(scores(bgw_modified_model)), names(coef(bgw_model)))
})

test_that("add_scores() accepts changes to method", {
  expect_equal(
    jacobian(function(param) log(bgw_log_lik(param)), coef(bgw_model), method = "simple"),
    add_scores(bgw_model, bgw_log_lik, coef(bgw_model), method = "simple")$scores,
    ignore_attr = TRUE
  )
})

test_that("add_scores() log transforms likelihoods for bgw_mle objects only", {
  expect_equal(
    add_scores(bgw_model, bgw_log_lik, log_transform = FALSE)$scores,
    jacobian(bgw_log_lik, coef(bgw_model)),
    ignore_attr = TRUE
  )
  expect_equal(
    maxlik_modified_model$scores,
    jacobian(maxlik_log_lik, coef(maxlik_model)),
    ignore_attr = TRUE
  )
})

test_that("add_scores() returns log-likelihood scores that sum to zero at the optimum", {
  # Scores of the log-likelihood sum to zero at the MLE. Scores of the
  # likelihood (probabilities) do not, which was a bug for bgw_mle objects.
  expect_equal(colSums(scores(bgw_fd_modified_model)), c(b1 = 0, b2 = 0), tolerance = 1e-4)
  expect_equal(colSums(scores(maxlik_modified_model)), c(b1 = 0, b2 = 0), tolerance = 1e-4)
})

test_that("add_scores() gives the same scores for bgw_mle and maxLik", {
  expect_equal(
    scores(bgw_fd_modified_model),
    scores(maxlik_modified_model),
    tolerance = 1e-4
  )
})

test_that("add_scores() reports errors from the likelihood function", {
  expect_error(
    add_scores(bgw_model, function(param) stop("boom")),
    "Error in calculating the scores"
  )
})
