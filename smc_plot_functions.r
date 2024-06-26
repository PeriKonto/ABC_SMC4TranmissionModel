# Plot-related functions for sequential Monte Carlo
# Author: Luc Coffeng
# Created: October 9, 2018


# Functions to automatically set breaks for ggplot  scales
  base.breaks.log <- function(n = 10) {
    function(x) {
      axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
    }
  }
  base.breaks <- function(n = 10) {
    function(x) {
      axisTicks(range(x, na.rm = TRUE), log = FALSE, n = n)
    }
  }
  

# Function to plot joint distribution of parameter values for a single SMC iteration
#  x = a single SMC iteration, i.e. a single element of a list object generated by smc()
#  par.names = vector with names of the two parameters to be visualised
#  nticks = approximate number of ticks that will be used on each axis
#  trans = logical flag indicating whether parameter estimates should be plotted
#          on the unconstrained (transformed) scale as specified by theta.trans
#  Hfun = name of function to determine / calculate the bandwith matrix (string),
#         with the options: Hpi, Hpi.diag, Hlscv, Hlscv.diag, Hscv, Hscv.diag,
#         Hbsc, Hbsc.diag, Hscv, Hscv.diag, and Hns
  plot.iteration <- function(x,
                             par.names = NULL,
                             nticks = 5,
                             trans = FALSE,
                             Hfun = "Hpi",
                             probs = c(.5, .9, .95),
                             print.BCI.marg = FALSE) {
    if(is.null(par.names)) stop("Must specify exactly two parameter names")
    
  # Prepare information
    theta.post <- x$theta.raw[, par.names]
    weight.post <- x$new.weight
    level.labels <- paste0(100 * probs, "%")
    colnames(theta.post) <- c("x", "y")
    
  # Produce 2d kernel density estimate
    dens.ks <- kde(x = theta.post,
                   w = weight.post,
                   H = do.call(Hfun, list(x = theta.post)),
                   gridsize = 100,
                   approx.cont = FALSE)
    
  # Extract contours
    contours <- with(dens.ks,
                     contourLines(x = eval.points[[1]],
                                  y = eval.points[[2]],
                                  z = estimate,
                                  levels = cont[paste0(100 * (1 - probs), "%")]))
    
  # Link contour levels (multiple polygons per level possible!) to appropriate
  # probability level
    contour.levels <- unique(sapply(contours, function(x) x$level))
    contours <- lapply(1:length(contours), function(i) {
      data.frame(contours[[i]],
                 label = level.labels[contours[[i]]$level == contour.levels],
                 object = i)
    })
    contours <- rbindlist(contours)
    
  # Transform everything to natural scale
    theta.post <- inv.transform.theta(theta.post, theta_trans = theta.trans[par.names])
    contours[, c("x", "y") := alply(inv.transform.theta(cbind(x, y),
                                                        theta_trans = theta.trans[par.names]),
                                    2, identity)]
    
  # Calculate approximate posterior summary estimates (weighted)
    theta.post.mean <- data.frame(weight.post %*% theta.post / sum(weight.post))
    theta.post.qt <- data.table(apply(theta.post, 2, function(x) {
      wtd.quantile(x = x,
                   weights = weight.post,
                   probs = c(rev((1 - probs) / 2),
                             1 - (1 - probs) / 2),
                   normwt = TRUE)
    }))
    
  # Build base plot  
    p <- ggplot(data = as.data.frame(cbind(theta.post,
                                           weight = weight.post)),
                mapping = aes(x = x, y = y)) +
      scale_x_continuous(name = paste0("\n", par.names[1]),
                         breaks = base.breaks(nticks)) +
      scale_y_continuous(name = paste0(par.names[2], "\n"),
                         breaks = base.breaks(nticks)) +
      theme_bw()
    
  # Transform scales if any are log-transformed
    if(trans == TRUE & any(theta.trans == "log")) {
      if(theta.trans[par.names][1] == "log") {
        p <- p +
          scale_x_log10(name = paste0("\n", par.names[1]),
                        breaks = base.breaks.log(nticks))
      }
      if(theta.trans[par.names][2] == "log") {
        p <- p +
          scale_y_log10(name = paste0(par.names[2], "\n"),
                        breaks = base.breaks.log(nticks))
      }
    }
    
  # Add layers for contour / probability levels (one per level), making sure
  # that in case of multiple polygons islands and holes are correctly rendered
    for(i in rev(levels(contours$label)))
      p <- p +
      geom_polypath(data = contours[label == i], alpha = .8, rule = "evenodd",
                             mapping = aes(x = x, y = y,
                                           group = object, fill = label))
    p <- p + scale_fill_grey(name = "", start = 0.5, end = 0.9)
    
  # Add the generated particles (bubble plot)
    p <- p +
      geom_point(mapping = aes(size = weight,
                               shape = "Particle",
                               col = "Particle"),
                 alpha = 0.75) +
      geom_point(data = theta.post.mean,
                 mapping = aes(shape = "Posterior\nmean",
                               col = "Posterior\nmean")) +
      scale_size_continuous(guide = FALSE) +
      scale_shape_manual(name = "", values = c(1, 3)) +
      scale_colour_manual(name = "", values = c("black", "red"))
    
  # Add marginal BCIs
    if(print.BCI.marg) {
      for(i in 1:length(probs)) {
        p <- p + 
          geom_errorbar(data = theta.post.qt[c(i, .N - i + 1)],
                        mapping = aes(x = theta.post.mean$x,
                                      ymin = min(y),
                                      ymax = max(y)),
                        linetype = (length(probs):1)[i],
                        col = "red", width = .0025) +
          geom_errorbarh(data = theta.post.qt[c(i, .N - i + 1)],
                         mapping = aes(y = theta.post.mean$y,
                                       xmin = min(x),
                                       xmax = max(x)),
                         linetype = (length(probs):1)[i],
                         col = "red", height = .0025)
      }  
    }
    
    p
    
  }
  


# Function to plot evolution of particle population
#   theta = four-dimensional array holding 1) iterations of the SMC algorithm
#           2) samples, 3) parameters, and 4) proposals and final values
#   par = character string with name of parameter that is to be plotted
  plot.SMC <- function(theta,
                       par,
                       x_lim = c(0, dim(theta)[1] - 1),
                       y_lim = NULL,
                       log_axis = "") {
    
    theta.select <- theta[paste0("t", x_lim[1]:x_lim[2]), , par, ]
    N_p <- dim(theta.select)[2]
    
    if(is.null(y_lim)) {
      y_lim <- c(0.9 * min(theta.select),
                 1.1 * max(theta.select))
    }
    
    matplot(x = rep(x_lim[1], N_p),
            y = theta.select[paste0("t", x_lim[1]), , "final"],
            type = "n",
            pch = 1,
            col = rgb(0, 0, 0, alpha = 0.25),
            xlim = x_lim,
            ylim = y_lim,
            xlab = "SMC iteration",
            ylab = par,
            log = log_axis)
    
    for(t in min(x_lim):max(x_lim)) {
      # Particle trajectories
      matlines(
        x = rbind(rep(t-1, N_p), rep(t, N_p)),
        y = t(theta.select[paste0("t", t), , ]),
        lty = 1,
        col = rgb(0, 0, 0, alpha = 0.25))
      
      # Particle positions
      matpoints(
        x = rep(t, N_p),
        y = theta.select[paste0("t", t), , "final"],
        pch = 1,
        col = rgb(0, 0, 0, alpha = 0.25))
    }
  }
  
  
### END OF CODE ###
  
