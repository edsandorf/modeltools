# Load needed packages ----
library(bgw)
library(tibble)

# Generate data ----
set.seed(1)
db <- tibble(
  x = runif(10),
  y = runif(10),
  z = sample(c(0, 1), 10, replace = TRUE)
)

# Define a simple log-likelihood function ----
log_lik <- function(param) {
  pr_yes <- 1 / (1 + exp(-(param[1] * db$x - param[2] * db$y)))
  return(
    ifelse(db$z == 1, pr_yes, 1 - pr_yes)
  )
}

# Estimate the model using BGW ----
model <- bgw_mle(log_lik, betaStart = c(b1 = 0, b2 = 0))

# Modify the model object by adding the scores ----
modified_model <- add_scores(model, log_lik, coef(model))
