# Prior-related functions for sequential Monte Carlo
# Author: Luc Coffeng
# Created: September 13, 2018
# Adopted: October 12, 2018, By Peri Kontoroupis

# Prior distribution (normal) random number generator on the transformed scale
  rprior <- function(size, pars) {
    sapply(pars, function(x) {
      with(x, rnorm(size, mean = mu, sd = sd))
    })
  }


# Prior (normal) density on the transformed scale
  dprior <- function(theta_raw, pars, theta_trans) {

  # # if(ncol(theta_raw) != length(pars))
  ##    stop("Mismatch in number of parameters (columns in theta) and number of priors")

  # Calculate log-density for each parameter value separately
    log_dens <- sapply(1:ncol(theta_raw), function(i) {
      with(pars[[i]], dnorm(theta_raw[, i], mean = mu, sd = sd, log = TRUE))
    })

  # # Correct density for transformation of parameters
  # # If the PDF of x is pdf_X(x) and Y = g(X), then pdf_Y(y) = pdf_X(g.inv(y)) * g.inv(y)'
  #   log.trans.index <- which(theta_trans == "log")
  #   logit.trans.index <- which(theta_trans == "logit")
  #
  #   log_dens[, log.trans.index]   <- log_dens[, log.trans.index] - theta_raw[, log.trans.index]
  #   # log_dens[, logit.trans.index] <- log_dens[, log.trans.index] + [...]

  # Calculate and return total density per parameter combination
    exp(apply(log_dens, 1, sum))

  }


### END OF CODE ###

