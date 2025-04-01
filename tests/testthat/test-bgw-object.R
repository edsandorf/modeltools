test_that("The model object has the correct class", {
  expect_s3_class(bgw_model, "bgw_mle")
})

test_that("The model object has the correct class after modification", {
  expect_s3_class(bgw_modified_model, "bgw_mle")
})

