library(bgw)
library(tibble)

set.seed(1)
db <- tibble(
  x = runif(10),
  y = runif(10),
  z = sample(c(0, 1), 10, replace = TRUE)
)

log_lik <- function(param) {
  pr_yes <- 1 / (1 + exp(-(param[1] * db$x - param[2] * db$y)))
  return(
    ifelse(db$z == 1, pr_yes, 1 - pr_yes)
  )
}

model <- bgw_mle(log_lik, betaStart = c(b1 = 0, b2 = 0))

