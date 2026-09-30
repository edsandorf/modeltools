test_that("search_starting_values() returns a list of named vectors", {
  set.seed(1)
  res <- search_starting_values(bgw_log_lik, c(b1 = 0, b2 = 0), N = 20, n_best = 3)
  expect_type(res, "list")
  expect_length(res, 3)
  for (sv in res) {
    expect_named(sv, c("b1", "b2"))
  }
})

test_that("search_starting_values() returns one vector when n_best = 1", {
  set.seed(1)
  res <- search_starting_values(bgw_log_lik, c(b1 = 0, b2 = 0), N = 20, n_best = 1)
  expect_length(res, 1)
  expect_length(res[[1]], 2)
})

test_that("search_starting_values() caps n_best at the number of candidates", {
  set.seed(1)
  expect_length(search_starting_values(bgw_log_lik, c(b1 = 0, b2 = 0), N = 3, n_best = 10), 4)
})

test_that("search_starting_values() includes the original starting values", {
  expect_equal(
    search_starting_values(bgw_log_lik, c(b1 = 0, b2 = 0), N = 0, n_best = 10),
    list(c(b1 = 0, b2 = 0))
  )
})

test_that("search_starting_values() orders the candidates by log-likelihood", {
  set.seed(1)
  res <- search_starting_values(bgw_log_lik, c(b1 = 0, b2 = 0), N = 50, n_best = 10)
  ll <- vapply(res, function(param) sum(log(bgw_log_lik(param))), numeric(1))
  expect_false(is.unsorted(rev(ll)))
})

test_that("search_starting_values() respects the adjustment multiplier", {
  set.seed(1)
  res <- search_starting_values(bgw_log_lik, c(b1 = 0, b2 = 0), N = 50, n_best = 51, adjustment_multiplier = 0.1)
  expect_true(all(abs(unlist(res)) <= 0.1))
})
