#' Function to search for starting values
#'
#' The function takes a vector of starting values. These will then be adjusted by adding a random uniform
#' value between -1 and 1 multiplied by an adjustment multiplier. The function then evaluates the
#' log-likelihood for each set of starting values and returns the specified number of best fitting
#' starting values. The original starting values are always included as a candidate.
#'
#' @param prob_fn A likelihood function returning the observation-level likelihoods (probabilities),
#' i.e., the function passed to [bgw::bgw_mle()]. The function is evaluated as `sum(log(prob_fn(param)))`.
#' @param starting_values A (named) vector of starting values. Names are preserved in the output.
#' @param N The number of starting value candidates to use. Default is 10,000
#' @param n_best Number of vectors to return. Default is 10
#' @param adjustment_multiplier An adjustment multiplier for the random uniform adjustment matrix. The default value is 1.
#'
#' @return A list of the `n_best` best fitting starting value vectors, ordered from best to worst.
#'
#' @export
search_starting_values <- function(
  prob_fn,
  starting_values,
  N = 10000,
  n_best = 10,
  adjustment_multiplier = 1
) {
  k <- length(starting_values)

  # Original starting values plus N randomly adjusted candidates
  candidates <- rbind(
    starting_values,
    repeat_rows(t(starting_values), N) +
      matrix(runif(N * k, min = -1, max = 1) * adjustment_multiplier, ncol = k)
  )

  # Define a progress bar
  pb <- progress::progress_bar$new(
    format = "[:bar] :percent :elapsed",
    total = nrow(candidates),
    clear = FALSE,
    width = 80
  )

  # Get the LL values for the different starting values
  ll_values <- apply(candidates, 1, function(param) {
    pb$tick()
    sum(log(prob_fn(param)))
  })

  # Return the specified number of best fitting starting values
  best <- order(ll_values, decreasing = TRUE)[seq_len(min(n_best, length(ll_values)))]

  return(
    lapply(best, function(i) candidates[i, ])
  )
}
