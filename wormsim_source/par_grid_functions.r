# Functions to create a table of combinations of alternative parameter values.
# Author: Luc Coffeng
# Date created: Oct 11, 2014
# Wormsim version: 2.70

  
# Make table of all unique combinations of alternative parameter values in a list.
#   parameter.alt = named list of vectors with alternative parameter values.

create.par.list <- function(parameter.alt) {
  
  n.alt.par.values <- sapply(parameter.alt, length)
  n.alt.par.comb <- n.alt.par.values[[1]]
  
  # Initialize list of combinations of parameter values and determine indices
  # needed to pull values from parameter.alt.
  index.alt.par <- data.frame(lapply(parameter.alt, function(x) {1:length(x)}))
  par.alt.combi.list <- as.list(rep(list(as.list(rep(NA, dim(index.alt.par)[2]))), n.alt.par.comb))
  
  # Fill list with unique combinations of parameter values
  for (i in 1:dim(index.alt.par)[1]) {
    for (j in 1:dim(index.alt.par)[2]) {
      
      par.alt.combi.list[[i]][[j]] <- parameter.alt[[j]][[index.alt.par[i,j]]]
      
    }
    
    names(par.alt.combi.list[[i]]) <- names(parameter.alt)
    
  }
  
  # Return list of lists of alternative parameter values
  return(par.alt.combi.list)
  
}

# 
#   create.par.list <- function(parameter.alt) {
#     
#     n.alt.par.values <- sapply(parameter.alt, length)
#     n.alt.par.comb <- prod(n.alt.par.values)
#   
#   # Initialize list of combinations of parameter values and determine indices
#   # needed to pull values from parameter.alt.
#     index.alt.par <- expand.grid(lapply(parameter.alt, function(x) {1:length(x)}))
#     par.alt.combi.list <- as.list(rep(list(as.list(rep(NA, dim(index.alt.par)[2]))), n.alt.par.comb))
#   
#   # Fill list with unique combinations of parameter values
#     for (i in 1:dim(index.alt.par)[1]) {
#       for (j in 1:dim(index.alt.par)[2]) {        
#         
#           par.alt.combi.list[[i]][[j]] <- parameter.alt[[j]][[index.alt.par[i,j]]]
#           
#       }
#       
#       names(par.alt.combi.list[[i]]) <- names(parameter.alt)
#       
#     }
#     
#   # Return list of lists of alternative parameter values
#     return(par.alt.combi.list)
#   
#   }