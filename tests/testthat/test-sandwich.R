test_that("scores() correctly throws errors if the object does not contain a scores matrix", {
  expect_error(scores(model))
  expect_error(scores(list(x = runif(10))))
})

test_that("scores() correctly returns the scores matrix", {
  expect_equal(scores(modified_model), modified_model$scores)
})

