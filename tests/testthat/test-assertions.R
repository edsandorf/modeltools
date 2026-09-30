test_that("is_bgw() correctly returns TRUE and FALSE", {
  expect_true(is_bgw(bgw_model))
  expect_false(is_bgw(list(x = runif(10))))
})

test_that("is_bgw() returns a single value for objects with multiple classes", {
  expect_identical(is_bgw(maxlik_model), FALSE)
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
  expect_true(converged(maxlik_model))
})

test_that("converged() uses the favorable bgw return codes", {
  for (code in c(3, 4, 5, 6)) {
    expect_true(converged(modifyList(bgw_model, list(code = code))), label = paste("code", code))
  }
  for (code in c(0, 7, 8, 9)) {
    expect_false(converged(modifyList(bgw_model, list(code = code))), label = paste("code", code))
  }
})

test_that("converged() uses the successful maxLik return codes", {
  for (code in c(0, 1, 2, 8)) {
    expect_true(converged(modifyList(maxlik_model, list(code = code))), label = paste("code", code))
  }
  for (code in c(3, 4, 100)) {
    expect_false(converged(modifyList(maxlik_model, list(code = code))), label = paste("code", code))
  }
})

test_that("converged() errors for unsupported classes", {
  expect_error(converged(lm(dist ~ speed, data = cars)), "not implemented")
})
