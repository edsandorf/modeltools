context("Test poe_test()")

set.seed(123)
x <- qnorm(runif(100), mean = -0.75, sd = 1)
y <- qnorm(runif(100), mean = 1.5, sd = 2)

test_that("poe_test() fails with non-numeric input", {
  expect_error(poe_test("a", "b"))
  expect_error(poe_test("a", y))
  expect_error(poe_test(x, "b"))
})

test_that("poe_test() returns a list with the correct structure", {
  res <- poe_test(x, y)
  expect_s3_class(res, "poe_test")
  expect_named(res, c("method", "statistic", "means"))
  expect_type(res$means, "double")
  expect_length(res$means, 2)
})

test_that("poe_test() returns the correct results", {
  res <- poe_test(x, y)
  expect_equal(res$method, "Poe et al. (2005) test")
  expect_equal(res$statistic, 0.8986)
  expect_equal(res$means, setNames(c(mean(x), mean(y)), c("x", "y")))
})
