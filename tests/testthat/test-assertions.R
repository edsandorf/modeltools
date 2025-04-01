test_that("is_bgw() correctly returns TRUE and FALSE", {
  expect_true(is_bgw(model))
  expect_false(is_bgw(list(x = runif(10))))
})

test_that("has_scores() correctly returns TRUE and FALSE", {
  expect_true(has_scores(modified_model))
  expect_false(has_scores(model))
  expect_false(has_scores(list(x = runif(10))))
})

test_that("converged() correctly returns TRUE and FALSE", {
  expect_true(converged(model))
})
