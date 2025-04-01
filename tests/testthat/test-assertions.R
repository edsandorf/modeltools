test_that("is_bgw() correctly returns TRUE and FALSE", {
  expect_true(is_bgw(bgw_model))
  expect_false(is_bgw(list(x = runif(10))))
})

test_that("has_scores() correctly returns TRUE and FALSE", {
  expect_true(has_scores(bgw_modified_model))
  expect_false(has_scores(bgw_model))
  expect_false(has_scores(list(x = runif(10))))
  expect_true(has_scores(maxlik_modified_model))
  expect_false(has_scores(maxlik_model))
})

test_that("converged() correctly returns TRUE and FALSE", {
  expect_true(converged(bgw_model))
})
