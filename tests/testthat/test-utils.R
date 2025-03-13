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


