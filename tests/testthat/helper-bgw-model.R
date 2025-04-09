# Load needed packages ----
library(maxLik)
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
bgw_log_lik <- function(param) {
  pr_yes <- 1 / (1 + exp(-(param[1] * db$x - param[2] * db$y)))
  return(
    ifelse(db$z == 1, pr_yes, 1 - pr_yes)
  )
}

maxlik_log_lik <- function(param) {
  return(
    log(bgw_log_lik(param))
  )
}

# Estimate the model ----
bgw_model <- bgw_mle(bgw_log_lik, betaStart = c(b1 = 0, b2 = 0))
maxlik_model <- maxLik(maxlik_log_lik, start = c(b1 = 0, b2 = 0))

# Modify the model object by adding the scores ----
bgw_modified_model <- add_scores(bgw_model, bgw_log_lik, coef(bgw_model))
maxlik_modified_model <- add_scores(maxlik_model, maxlik_log_lik, coef(maxlik_model))
