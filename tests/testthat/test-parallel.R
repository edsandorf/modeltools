test_that("suggest_parallel() suggests at least one core and a valid cluster type", {
  res <- suggest_parallel()
  expect_gte(res$suggested_cores, 1)
  expect_true(res$suggested_cluster_type %in% c("PSOCK", "FORK"))
})

test_that("suggest_parallel() falls back to one core if detectCores() fails", {
  local_mocked_bindings(detectCores = function(...) NA_integer_)
  expect_equal(suggest_parallel()$suggested_cores, 1)
})
