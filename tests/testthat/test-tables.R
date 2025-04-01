test_that("stars() correctly returns the stars", {
  expect_equal(stars(0.05), "*")
  expect_equal(stars(0.0049999), "**")
  expect_equal(stars(c(0.09, 0.04, 0.11, 0.001)), c(".", "*", "", "***"))
})
