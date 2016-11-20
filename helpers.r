library(dplyr)
library(ggplot2)
library(reshape2)
library(rriskDistributions)

# ---- Log-normal ----
get_params_lnorm <- function(quantiles, probabilities) {
  distr_lnorm <- get.lnorm.par(p=probabilities, q=quantiles, plot=FALSE)
  
  res <- list()
  res$meanlog <- distr_lnorm[["meanlog"]]
  res$sdlog <- distr_lnorm[["sdlog"]]
  return(res)
}

get_salary_dist_lnorm <- function(quantiles, probabilities, salary) {
  fit <- get_params_lnorm(quantiles, probabilities)
  probs <- dlnorm(salary, meanlog=fit$meanlog, sdlog=fit$sdlog)
  return(probs)
}

get_salary_dist_cum_lnorm <- function(quantiles, probabilities, salary) {
  fit <- get_params_lnorm(quantiles, probabilities)
  probs <- plnorm(salary, meanlog=fit$meanlog, sdlog=fit$sdlog)
  return(probs)
}

predict_quantiles_lnorm <- function(true_quantiles, probabilities, probs_predict) {
  fit <- get_params_lnorm(true_quantiles, probabilities)
  return(qlnorm(probabilities, meanlog=fit$meanlog, sdlog=fit$sdlog))
}