set.seed(123)
x <- qnorm(runif(100), mean = -0.75, sd = 1)
y <- qnorm(runif(100), mean = 1.5, sd = 2)

test_that("poe_test() fails with non-numeric input", {
  expect_error(poe_test("a", "b"))
  expect_error(poe_test("a", y))
  expect_error(poe_test(x, "b"))
})

test_that("poe_test() fails with empty input or missing values", {
  expect_error(poe_test(numeric(0), y), "non-empty")
  expect_error(poe_test(x, numeric(0)), "non-empty")
  expect_error(poe_test(c(x, NA), y), "missing")
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

test_that("poe_test() equals the complete combinatorial for unequal lengths and ties", {
  brute_force <- function(x, y) mean(outer(x, y, "-") <= 0)
  x2 <- rnorm(57)
  y2 <- rnorm(113, mean = 0.2)
  expect_equal(poe_test(x2, y2)$statistic, brute_force(x2, y2))
  expect_equal(poe_test(c(1, 2, 2, 3), c(2, 2, 5))$statistic, brute_force(c(1, 2, 2, 3), c(2, 2, 5)))
})

test_that("print.poe_test() returns the object invisibly", {
  res <- poe_test(x, y)
  expect_output(out <- withVisible(print(res)), "Gamma")
  expect_false(out$visible)
  expect_identical(out$value, res)
})
