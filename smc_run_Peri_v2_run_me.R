# ABC-SMC template code (master branch) for a toy model based the normal
# distribution with unknown mean and standard deviation

# Estimates are benchmarked against HMC-based estimates from the package brms,
# which requires considerable compilation time (so only run that part of code
# once or find a way to port to rstanarm, which is pre-compiled but does not
# allow specification of custom priors).

# Author: Luc Coffeng
# Created: September 13, 2018
# Author: Peri Kontoroupis
# Modified: October 3, 2018
# Modified: October 12, 2018

# Prep session
  rm(list = ls())

  library(plyr)
  library(abind)
  library(data.table)
  library(mvnfast)
  library(rstudioapi)  # requires Rstudio
  library(boot)
  library(foreach)
  library(doParallel)
  library(ks)
  library(ggplot2)
  library(ggthemes)
  library(ggpolypath)
  library(gridExtra)
  library(sn)
  library(moments)
  library(Hmisc)
  library(osDesign)

  #code.dir <-  dirname(getActiveDocumentContext()$path)
  code.dir <- "F:/to archive to X drive/2. ABC testing/Task 2.2"
  output.dir <- "F:/to archive to X drive/2. ABC testing/Task 2.2/05_output"
  wormsim.dir <- file.path(code.dir, "04_wormsim-v2.58Ap27")
  wormsim.source.dir <- file.path(code.dir, "wormsim_source")
  input.template <- file.path(wormsim.source.dir,"template2MDA.xml")
  input.schema <- file.path(wormsim.dir, "wormsim.xsd")

  set.seed(123456)


# Load functions (N.B. the following function are specific to this particular
# model and should be adapted for other problems: dprior, gen.data, D,
# rproposal or rprior)
  setwd(code.dir)
  source("smc_core_functions_v2_Peri.r")
  source("smc_prior_functions_v2_Peri.r")
  source("smc_app_dependent_functions_v2_Peri.r")
  source("smc_plot_functions.r")

  setwd(wormsim.source.dir)
  source("xml_substitute_functions.r")
  source("create_xml_functions.r")
  source("basic_functions.r")
  source("par_grid_functions.r")
  source("special_functions.r")
  source("par_def.r")



# Setup true parameter values and toy dataset
  theta.names <- c("mbr", "exposure.p1")
  theta.trans <- c(mbr="log", exposure.p1="log")


  prior.par <- list(
    mu  = list(mu=6.5, sd=0.3),
    mu = list(mu=-1.2, sd=0.6))

  y <- data.table(read.csv("F:/to archive to X drive/2. ABC testing/Task 2.2/syn.obs.csv"))
  y_obs <- y[seq(dim(y)[1],1),]

  study_size <- data.table(read.csv("F:/to archive to X drive/2. ABC testing/Task 2.2/ydata.csv"))


# Run algorithm "S" as described by Toni et al (J R Soc Interface 2009)
  fit <- smc(N_p = 500,
             theta_names = theta.names,
             theta_trans = theta.trans,
             prior_par = prior.par,
             y_obs = y_obs,
             # T = 20,
             cv2.thres = 1,
             e_term = 0.001,
             e_perc = 0.25,
             perturb_df = Inf,
             parallel = TRUE)


# Summarize benchmark and approximate posterior
  summarise.SMC(fit, theta_trans = theta.trans, digits = 2)

 # save(fit, file="myrun11_10_18.RData")
  #load("myrun11_10_18B.RData")
  setwd("F:/to archive to X drive/2. ABC testing/Task 2.2/output")
 load("myrun14_10_18.RData")
# Plot evolution of particle population
# N.B.: code to be updated to ggplot

  # Prep data
    theta <- unname(laply(fit, function(x){
      abind(inv.transform.theta(x$theta.raw.proposed, theta_trans = theta.trans),
            inv.transform.theta(x$theta.raw, theta_trans = theta.trans),
            along = 3)
    }))

    dimnames(theta)[[1]] <- paste("t", 0:(length(fit)-1), sep = "")
    dimnames(theta)[[3]] <- theta.names
    dimnames(theta)[[4]] <- c("proposed", "final")

  # Plot SMC evolution of individual parameters
    # plot.SMC(theta = theta, par = theta.names[1], log_axis = "")
    # plot.SMC(theta = theta, par = theta.names[2], log_axis = "")

    pdf(file = "myrun14_10_18.pdf")

    summarise.SMC(fit, theta_trans = theta.trans, digits = 2)

    plot.SMC(theta = theta, par = theta.names[1], log_axis = "y")
    plot.SMC(theta = theta, par = theta.names[2], log_axis = "y")


mark.target <- "True parameter\nvalues"
plot.iteration(x=fit[[length(fit)]], par.names =  c("mbr","exposure.p1"),
               trans = TRUE) +
geom_hline(mapping = aes(yintercept = 0.4, linetype = mark.target), col = "red") +
geom_vline(mapping = aes(xintercept = 600, linetype = mark.target), col = "red") +
scale_linetype_manual(name = "", values = 2)

dev.off()
### END OF CODE ###
