test_that("scores() correctly throws errors if the object does not contain a scores matrix", {
  expect_error(scores(bgw_model))
  expect_error(scores(maxlik_model))
  expect_error(scores(list(x = runif(10))))
})

test_that("scores() correctly returns the scores matrix", {
  expect_equal(scores(bgw_modified_model), bgw_modified_model$scores)
  expect_equal(scores(maxlik_modified_model), maxlik_modified_model$scores)
})

