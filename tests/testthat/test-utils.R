test_that("repeat_rows() works with a matrix and does not flatten the results", {
  x <- matrix(1:2, nrow = 2)
  expect_equal(repeat_rows(x, 2), matrix(c(1, 1, 2, 2), nrow = 4))
})

test_that("repeat_rows() works with a data.frame and does not flatten the results", {
  x <- data.frame(a = 1:2, b = 3:4)
  expect_equal(repeat_rows(x, 2), structure(list(a = c(1L, 1L, 2L, 2L), b = c(3L, 3L, 4L, 4L)), row.names = c("1", 
                                                                                                              "1.1", "2", "2.1"), class = "data.frame"))
})

test_that("repeat_rows() works with a tibble and does not flatten the results", {
  x <- tibble::tibble(a = 1:2, b = 3:4)
  expect_equal(repeat_rows(x, 2), structure(list(a = c(1L, 1L, 2L, 2L), b = c(3L, 3L, 4L, 4L)), row.names = c(NA, 
                                                                                                              -4L), class = c("tbl_df", "tbl", "data.frame")))
  })

test_that("load_packages() warns and skips missing packages when install_missing = FALSE", {
  expect_warning(
    res <- load_packages(c("stats", "notARealPackageXYZ"), install_missing = FALSE),
    "notARealPackageXYZ"
  )
  expect_equal(res, "stats")
})

test_that("load_packages() returns the loaded packages invisibly", {
  expect_invisible(load_packages("stats", install_missing = FALSE))
})
