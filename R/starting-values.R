#' Function to search for starting values
#'
#' The function takes a vector of starting values. These will then be adjusted by adding a random unform
#' value between -1 and 1 multiplied by an adjustment multiplier. The function then evaluates the
#' log-likelihood for each set of starting values and returns the specified number of best fitting
#' starting values.
#'
#' @param log_lik A log likelihood function
#' @param starting_values A vector of starting values. Note: Named vectors are ignored. The startign values must be locally bound in the log-likelihood function to work correctly.
#' @param N The number of starting value candiates to use. Default is 10,000
#' @param return Number of vectors to return
#' @param adjustment_multiplier An adjustment multiplier for the random uniform adjustment matrix. The default value is 1.
#'
#' @return A list of `return` number of parametrs
#'
#' @export
search_starting_values <- function(
  log_lik,
  starting_values,
  N = 10000,
  return = 10,
  adjustment_multiplier = 1
) {
  # Define a progress bar
  pb <- progress::progress_bar$new(
    format = "[:bar] :percent :elapsed",
    total = N,
    clear = FALSE,
    width = 80
  )

  # Repeat the vector of starting values N times and turn into a matrix
  starting_values <- matrix(
    rep(starting_values, times = N),
    ncol = length(starting_values),
    byrow = TRUE
  )

  # Create an adjustment matrix of the same size with random values
  adjustment <- matrix(
    runif(N * ncol(starting_values), min = -1, max = 1),
    ncol = ncol(starting_values)
  )

  # Add the adjustment to the starting values
  starting_values <- starting_values + adjustment * adjustment_multiplier

  # Turn the matrix of starting values into a list for faster processing using lapply
  starting_values_list <- as.list(
    as.data.frame(
      t(
        starting_values
      )
    )
  )

  # Fix the scope and set the ticker fro the progress bar.
  ll_scope_fix <- function(param, pb) {
    pb$tick()

    return(
      log_lik(param)
    )
  }

  # Get the LL values for the different starting values
  ll_values <- lapply(
    starting_values_list,
    function(
      param,
      pb
    ) {
      sum(
        log(
          ll_scope_fix(param, pb)
        )
      )
    },
    pb = pb
  )

  # Turn into a matrix, add the LL values, and order by LL value
  ll_values <- do.call(rbind, ll_values)
  starting_values <- cbind(starting_values, ll_values)
  starting_values <- starting_values[
    order(starting_values[, ncol(starting_values)], decreasing = TRUE),
  ]

  # Subset the list to return the specified number of best fitting starting values.
  starting_values_list <- as.list(
    as.data.frame(
      t(
        starting_values[seq_len(return), -ncol(starting_values)]
      )
    )
  )

  # Return the list of starting values
  return(
    starting_values_list
  )
}
