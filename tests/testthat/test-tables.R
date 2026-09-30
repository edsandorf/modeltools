test_that("stars() correctly returns the stars", {
  expect_equal(stars(0.05), "*")
  expect_equal(stars(0.0049999), "**")
  expect_equal(stars(c(0.09, 0.04, 0.11, 0.001)), c(".", "*", "", "***"))
})

test_that("stars() returns an empty string for missing p-values", {
  expect_equal(stars(NA), "")
})

test_that("prep_for_gt() stacks the tidy and glance output", {
  res <- prep_for_gt(bgw_model)
  expect_equal(nrow(res), length(coef(bgw_model)) + ncol(glance(bgw_model)))
  expect_equal(res$term, tolower(c(names(coef(bgw_model)), names(glance(bgw_model)))))
  expect_equal(res$stars, stars(res$p.value))
})

test_that("prep_for_gt() passes vcov on to tidy()", {
  robust <- sandwich(bgw_fd_modified_model)
  res <- prep_for_gt(bgw_fd_model, vcov = robust)
  expect_equal(res$std.error[1:2], sqrt(diag(robust)), ignore_attr = TRUE)
})
