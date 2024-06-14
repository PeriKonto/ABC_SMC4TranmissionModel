# Core functions for sequential Monte Carlo
# Author: Luc Coffeng
# Created: September 13, 2018
# Adopted: October 12, 2018, By Peri Kontoroupis

# Function to run SMC
  smc <- function(y_obs,
                  theta_names,
                  theta_trans,
                  prior_par,
                  N_p,
                  cv2.thres = Inf,  # upper threshold for CV2 (if below, particles are resampled and weights reset; set to Inf to nullify)
                  T = NULL,  # specification of terminal tolerance (e_term) takes precedence over T
                  e_perc = 0.5,
                  e_term = NULL,
                  perturb_df = Inf,
                  max.batch.factor = 10,  # maximum factor for number of particles to be generated per try (i.e. factor times N_p)
                  parallel = FALSE) {  # set to true of the gen.pop.data() function runs a foreach loop

    if(is.null(T) & is.null(e_term))
      stop("Must specify either number of iterations (T) or terminal tolerance (e_term). Latter takes precedence if both are specified.")

    automated.T <- is.null(T)

    start.time.SMC <- Sys.time()
    cat("\rStarting SMC at ", paste(start.time.SMC), sep = "")
    flush.console()

  # Generate initial particle population from prior
    if(parallel) {
      cluster <- makeCluster(parallel::detectCores(logical = FALSE))
      registerDoParallel(cluster)
    }

    fit <- list(t0 = SMC.init(N_p = N_p,
                              theta_names = theta_names,
                              theta_trans = theta_trans,
                              prior_par = prior_par,
                              y_obs = y_obs))

    if(parallel){
      stopCluster(cluster)
    }

  # Run SMC
    if(!is.null(e_term) & automated.T) T <- 1  # will be dynamically updated until e_term is reached
    t <- 1

    while(t <= T) {

      if(parallel) {
        cluster <- makeCluster(parallel::detectCores(logical = FALSE))
        registerDoParallel(cluster)
      }

    # Check that particle population from previous SMC iteration is fit enough.
    # If not, sample particles with replacement and set weights to one, and use
    # that surrogate particle population as new ancestors for current iteration.
      if(fit[[t]]$CV2 > cv2.thres) {  # i.e. if N.eff < N.particles / 2

        fit[[t]]$w.reset <- TRUE
        fit.previous <- fit[[t]]
        sample.index <- with(fit.previous,
                             sample(x = length(new.weight), size = N_p, replace = TRUE, prob = new.weight))
        fit.previous$theta.raw <- fit.previous$theta.raw[sample.index, , drop = FALSE]
        fit.previous$new.weight <- rep(1, length(sample.index))

      } else {

        fit.previous <- fit[[t]]

      }

    # Set tolerance based on user-defined quantile of accepted distance in previous
    # particle population
      e <- quantile(fit.previous$distance, probs = e_perc)

      if(!is.null(e_term)) {
        if(e < e_term) {
          e <- e_term
        } else {
          if(automated.T) T <- T + 1
        }
      }

    # Generate first proposal for new particle population
      start.time <- Sys.time()
      cat("\rSMC iteration ", t, "/", T, " (started ", paste0(start.time), ")",
          "                                    ", sep = "")
      flush.console()

      fit[[t+1]] <- SMC.run(fit.prev = fit.previous,
                            theta_names = theta_names,
                            theta_trans = theta_trans,
                            y_obs = y_obs,
                            e_t = e,
                            perturb_df = perturb_df)
      names(fit)[t+1] <- paste("t", t, sep = "")

    # While any rejected particles remain, keep generating new ones to replace
      rejected <- with(fit[[t+1]], N_p - length(weight))

      while(rejected) {

      # Print progress
        cat("\rSMC iteration ", t, "/", T, " (started ", paste0(start.time),
            "): ", N_p - rejected, " of ", N_p,
            " particles accepted                              ", sep = "")
        flush.console()

      # Set number of simulations to perform
        N.sim <- min(1 + ceiling(rejected / fit[[t+1]]$accept), N_p * max.batch.factor)

      # Generate new particles
        temp <- SMC.run(fit.prev = fit.previous,
                        N_sample = N.sim,
                        theta_names = theta_names,
                        theta_trans = theta_trans,
                        y_obs = y_obs,
                        e_t = e,
                        perturb_df = perturb_df)

      # Set number of particles to adopt
        N.accept <- min(with(temp, length(weight)), rejected)

      # Update fit object with new particles
        if(N.accept > 0) {
          fit[[t+1]] <- within(fit[[t+1]], {
            theta.raw <- rbind(theta.raw, temp$theta.raw[1:N.accept, , drop = FALSE])
            theta.raw.proposed <- rbind(theta.raw.proposed,
                                        temp$theta.raw.proposed[1:N.accept, , drop = FALSE])
            weight <- c(weight, temp$weight[1:N.accept])
            data.rep.pop <- c(data.rep.pop, temp$data.rep.pop[1:N.accept])
           # data.rep.obs <- c(data.rep.obs, temp$data.rep.obs[1:N.accept])
            distance <- c(distance, temp$distance[1:N.accept])
            N.particles <- N.particles + N.sim
            accept <- length(weight) / N.particles
          })
        }

        rejected <- with(fit[[t+1]], N_p - length(weight))
        rm(N.sim)
      }

      rm(rejected, N.accept)

    # Print progress
      cat("\rSMC iteration ", t, "/", T, " (started ", paste0(start.time),
          "): ", "finishing up                      ",
          sep = "")
      flush.console()

    # Calculate new weights (prior density / marginal density of the new
    # particle occuring | distance < d) and covariance of perturbation kernel
    # for next iteration based on the accepted perturbations in this iteration
      fit[[t+1]] <- within(fit[[t+1]], {

        new.weight <- gen.new.weight(theta_new = theta.raw,
                                     theta_old = fit.previous$theta.raw,
                                     theta_trans = theta_trans,
                                     weight_old = fit.previous$weight,
                                     scale_K = fit.previous$scale.K,
                                     perturb_df = perturb_df,
                                     prior_par = prior_par)
      # Coefficient of variation as described by Cornebise et al, 2008
      # (arXiv:0803.0054v2), and effective sample size of particle population
      # (must be calculated before normalisation)
        CV2 <- N_p / sum(new.weight)^2 * sum(new.weight^2) - 1
        N.eff <- round(N_p / (1 + CV2), 0)

        e <- e

        scale.K <- ocm.scale(old = theta.raw.proposed, new = theta.raw)

      })

      rm(fit.previous)

    # Print intermediate output and save image in case R crashes on next iteration
      run.time <- Sys.time() - start.time
      cat("\r")
      print(summarise.SMC(fit, theta_trans = theta_trans, digits = 3))
      cat("\nSMC iteration ", t, "/", T, " (started ",
          paste0(start.time), "): finished in ", paste(round(run.time, 1)), " ",
          attributes(run.time)$units, "\n", sep = "")
      cat("\n")

    # # Optional: save intermediate results in case of long simulations times
    #   save.image("[...].RData")

      if(t == T) {

        run.time.SMC <- Sys.time() - start.time.SMC
        cat("\nSMC (started ", paste0(start.time.SMC),
            "): finished in ", paste(round(run.time.SMC, 1)), " ",
            attributes(run.time.SMC)$units, "\n", sep = "")
        cat("\n")

      }

      t <- t + 1

      if(parallel){
        stopCluster(cluster)
      }

    }

    rm(t)

    return(fit)

  }


# Function to initialise SMC sampler
#   N_p = number particles that remain to be sampled
#   prior_par = list of prior parameter values
#   y_obs = observed data
#   calc_distance = logical flag for whether or not to simulate data and
#                   calculate distances
  SMC.init <- function(N_p, theta_names, theta_trans, prior_par, y_obs) {

  # Generate initial particle population
    theta.raw <- rprior(size = N_p, pars = prior_par)
    colnames(theta.raw) <- theta_names

  # Set weight if initial particles are sampled directly from prior
    weight <- rep(1, N_p)
    new.weight <- weight

  # Generate replicate data and calculate distance
    data_rep_pop <- gen.data.pop(theta.raw = theta.raw,
                                 theta_trans = theta_trans)

    #data_rep_obs <- gen.data.obs(x = data_rep_pop)
    y_extract <- llply(.data = data_rep_pop,
                       .fun = sim_study)

    distance <- D(y_rep = y_extract, y_obs = y_obs)

    windows()
    hist(distance)

  # Assemble results
    results <- list(theta.raw = theta.raw,
                    theta.raw.proposed = theta.raw,
                    weight = weight,
                    new.weight = new.weight,
                    data.rep.pop = data_rep_pop,
                    #data.rep.obs = data_rep_obs,
                    distance = distance,
                    scale.K = cov(theta.raw),  # initial perturbation kernel in t1
                    N.particles = N_p,
                    N.eff = N_p,
                    accept = 1.0,
                    e = Inf,
                    CV2 = 0,
                    w.reset = FALSE)

  # Return result
    return(results)
  }


# Function to run SMC sampler
#   fit.prev = object returned by SMC.init or SMC.run from previous iteration
#   theta = particles
#   weight = normalized weights of particles
#   N_sample = number particles that remain to be sampled
#   theta_names = parameter names
#   y_obs = observed data
#   e_t = tolerance for iteration t
  SMC.run <- function(fit.prev = NULL,
                      theta.raw = fit.prev$theta.raw,
                      theta_trans = NULL,
                      weight = fit.prev$new.weight,
                      N_sample = nrow(fit.prev$theta.raw),
                      scale_K = fit.prev$scale.K,
                      theta_names,
                      y_obs,
                      e_t,
                      perturb_df) {

    if(is.null(fit.prev)) stop("Must supply fit object of previous iteration")

  # Sample new particle population from previous population, ensuring that
  # the drawn set is always in matrix format (required by other functions)
    sample.index <- sample(x = length(weight), size = N_sample, replace = TRUE, prob = weight)
    theta_raw_proposed <- theta.raw[sample.index, , drop = FALSE]
    weight_proposed <- weight[sample.index]

  # Perturb proposed particles
    theta_raw_new <- rK(theta = theta_raw_proposed, scale = scale_K, df = perturb_df)

  # Check density of proposals under the prior and redraw if density is zero.
  # This is only necessary when the perturbed particles can end up outside prior
  # support, which is not the case here.

  # Generate replicate data and calculate distance
    data_rep_pop <- gen.data.pop(theta.raw = theta_raw_new,
                                 theta_trans = theta_trans)

    # data_rep_obs <- gen.data.obs(x = data_rep_pop)

    y_extract <- llply(.data = data_rep_pop,
                       .fun = sim_study)

    #distance <- D(y_rep = data_rep_obs, y_obs = y_obs)
    distance <- D(y_rep = y_extract, y_obs = y_obs)

  # Accept particles that fall within the defined tolerance
    accept <- distance < e_t

  # Return result
    return(list(
      theta.raw = matrix(data = theta_raw_new[accept, ],
                         nrow = sum(accept),
                         ncol = length(theta_names),
                         dimnames = list(NULL, theta_names)),
      theta.raw.proposed = matrix(data = theta_raw_proposed[accept, ],
                                  nrow = sum(accept),
                                  ncol = length(theta_names),
                                  dimnames = list(NULL, theta_names)),
      weight = weight_proposed[accept],
      w.reset = FALSE,
      data.rep.pop = data_rep_pop[accept],
      #data.rep.obs = data_rep_obs[accept],
      distance = unname(distance[accept]),
      N.particles = N_sample,
      accept = mean(accept)))
  }


# Function to calculate weighted variance
#   x = vector of observations
#   w = vector of weights
#   na.rm = indicator whether to drop missing observations
  weighted.var <- function(x, w, na_rm = FALSE) {
    if(na_rm) {
      i <- !is.na(x)
      w <- w[i]
      x <- x[i]
    }
    sum.w <- sum(w)
    (sum(w*x^2) * sum.w - sum(w*x)^2) / (sum.w^2 - sum(w^2))
  }


# Calculate scale for OCM pertubation kernel as proposed by Filippi et al, 2014
# (arXiv:1106.6280v2)
#   new = accepted perturbed particles
#   old = accepted particles before pertubation
  ocm.scale <- function(old, new) {

    cov(new - old)

  }


# Pertubation kernel
#   theta = matrix of proposed particles
#   scale = covariance (matrix) of perturbation kernel
#   df = degrees of freedom of multivariate Student t distribution (Inf = MVN)
  rK <- function(theta, scale, df = Inf) {

      N <- nrow(theta)

      if(df == Inf) {

        theta <- theta + rmvn(n = N,
                              mu = rep(0, ncol(scale)),
                              sigma = scale)

      } else {

        theta <- theta + rmvt(n = N,
                              mu = rep(0, ncol(scale)),
                              sigma = scale * (df - 2) / df,
                              df = df)

      }

      return(theta)

  }


# Multivariate normal density (quicker versions than dmvnorm)
#   X = matrix of random variates (one random variate per row)
#   mu = vector of means (length(mu) == ncol(X))
#
#   Require preparations for "rooti" and "constant", given
#   covariance matrix Sigma:
#     k <- ncol(X)
#     rooti <- backsolve(chol(Sigma), diag(k))
#     constant <- -(k/2)*log(2*pi)
  dMvn.quick <- function(X, mu, rooti, constant) {

    quads <- colSums((crossprod(rooti,(t(X)-mu)))^2)
    return(exp(constant + sum(log(diag(rooti))) - .5*quads))

  }


# Pertubation kernel density function
  dK <- function(theta_new, theta_old, scale_K, df) {

    N <- dim(theta_new)[1]
    M <- dim(theta_old)[1]
    dens <- matrix(NA, N, M)

  # For each particle (theta_old), calculate the probability density of other
  # particles (theta_new) being generated from it, using a quick multivariate
  # normal density function
    if(df == Inf) {

      # k <- ncol(theta_old)
      # rooti <- backsolve(chol(scale_K), diag(k))
      # constant <- -(k/2)*log(2*pi)
      #
      # for(i in 1:N) {
      #   dens[, i] <- dMvn.quick(X = theta_new, mu = theta_old[i, ],
      #                           rooti = rooti, constant = constant)
      # }

      for(i in 1:N) {
        dens[, i] <- dmvn(X = theta_new,
                          mu = theta_old[i, ],
                          sigma = scale_K)
      }

    } else {

      for(i in 1:N) {
        dens[, i] <- dmvt(X = theta_new,
                          mu = theta_old[i, ],
                          sigma = scale_K * (df - 2) / df,
                          df = df)
      }

    }

    return(dens)
  }


# Inverse transformation function
  inv.transform.theta <- function(theta_raw, theta_trans) {

    log.trans.index <- which(theta_trans == "log")
    logit.trans.index <- which(theta_trans == "logit")

    theta_raw[, log.trans.index]   <- exp(theta_raw[, log.trans.index])
    theta_raw[, logit.trans.index] <- inv.logit(theta_raw[, logit.trans.index])

    return(theta_raw)  # actually is no longer "raw" (uncontrained)

  }


# Weight calculation function
  gen.new.weight <- function(theta_new, theta_old, weight_old, theta_trans,
                             scale_K, perturb_df, prior_par) {

  # For each particle i (rows), calculate the density mass of it being generated
  # from particle j (columns)
    sum_dK <- dK(theta_new = theta_new,
                 theta_old = theta_old,
                 scale_K = scale_K,
                 df = perturb_df)

  # Divide prior density by marginal density of particles occuring | distance < d_t
    dprior(theta_raw = theta_new, pars = prior_par, theta_trans = theta_trans) /
      c(weight_old %*% t(sum_dK)) #* weight_old  # Del Moral 2012; Toni et al 2009 do not do this
  }


# Function to summarise approximate posterior samples for one SMC iteration
#   theta = two-dimensional array holding 1) samples, and 2) proposals and final
#           values
#   weight = vector holding weights
  summarise.iteration <- function(theta, weights) {

    cbind(
      mu = theta %*% weights / sum(weights),
      sd = sqrt(weighted.var(x = theta, w = weights))
    )

  }


# Function to summarise approximate posterior samples for all SMC iterations
#   theta = four-dimensional array holding 1) samples, 2) parameters, 3) proposals
#           and final values, and 4) iterations of the SMC algorithm
#   weight = three-dimensional array holding 1) samples, 2) raw and normalized
#            weihgts, and 3) iterations of the SMC algorithm
  summarise.SMC <- function(fit, theta_trans, digits = 3, tol.digits = 3) {

    summ.SMC <- array(dim = c(length(fit), 2 * ncol(fit[[1]]$theta.raw)),
                      dimnames = list(names(fit),
                                      c(sapply(colnames(fit[[1]]$theta.raw),
                                               function(x) paste0(x, c(".mu", ".sd"))))))
    summ.SMC <- data.frame(summ.SMC)

    for(t in 1:length(fit)) {

      theta <- inv.transform.theta(fit[[t]]$theta.raw, theta_trans = theta_trans)

      for(j in 1:ncol(theta)) {

        summ.SMC[t, 2*(j-1) + (1:2)] <-
          round(summarise.iteration(theta = theta[, j],
                                    weight = fit[[t]]$new.weight), digits)

      }
    }

    summ.SMC$CV2 <- round(laply(fit, function(x) {x$CV2}), digits)
    summ.SMC$N.eff <- laply(fit, function(x) {x$N.eff})
    summ.SMC$w.reset <- laply(fit, function(x) {x$w.reset})
    summ.SMC$accept <- round(laply(fit, function(x) {x$accept}), digits)
    summ.SMC$N.p <- laply(fit, function(x) {x$N.particles})
    summ.SMC$N.p.cum <- cumsum(summ.SMC$N.p)
    summ.SMC$e <- round(sapply(fit, function(x) x$e), tol.digits)

    return(summ.SMC)
  }


### END OF CODE ###
