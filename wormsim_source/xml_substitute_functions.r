# Functions to substitute parameter values in a parsed XML files. All functions
# follow the naming convention "set.[name]", where [name] refers to the
# parameter for which the value(s) is/are to be substituted. All functions take
# two arguments:
#   doc = object holding a parsed XML file, created with read.inputfile().
#   pars = named list of parameter value(s).
# Peri May 2018
# Change exposure heterogeneity (for both sexes)
  set.exposure.p1 <- function(doc, par = NULL) {
    
    xpath.expr <- "/wormsim.inputfile/exposure.and.contribution/male/exposure.index"
    dist <- xpathApply(doc, xpath.expr)
    xmlAttrs(dist[[1]])["p1"] <- paste(par)
    
    xpath.expr <- "/wormsim.inputfile/exposure.and.contribution/female/exposure.index"
    dist <- xpathApply(doc, xpath.expr)
    xmlAttrs(dist[[1]])["p1"] <- paste(par)
    
    return(doc)
    
  }
#----------------------------------------------------------------------------------WORKING?

set.surveys <- function(doc, par = list(start = list(year = NULL, month = NULL),
                                        stop = list(year = NULL, month = NULL),
                                        interval = list(year = NULL, month = NULL))) {

  xpath.expr <- "/wormsim.inputfile/simulation/surveillance/periodic.surveys/start"
  surv.start <- xpathApply(doc, xpath.expr)
  xmlAttrs(surv.start[[1]])["year"] <- paste(par$start$year)
  xmlAttrs(surv.start[[1]])["month"] <- paste(par$start$month)
  
  xpath.expr <- "/wormsim.inputfile/simulation/surveillance/periodic.surveys/stop"
  surv.stop <- xpathApply(doc, xpath.expr)
  xmlAttrs(surv.stop[[1]])["year"] <- paste(par$stop$year)
  xmlAttrs(surv.stop[[1]])["month"] <- paste(par$stop$month)
  
  xpath.expr <- "/wormsim.inputfile/simulation/surveillance/periodic.surveys/interval"
  surv.int <- xpathApply(doc, xpath.expr)
  xmlAttrs(surv.int[[1]])["years"] <- paste(par$interval$year)
  xmlAttrs(surv.int[[1]])["months"] <- paste(par$interval$month)
  
  return(doc)
}

#----------------------------------------------------------------------------------WORKING --- SURE IT IS NOT AFFECTED?

set.mda <- function(doc = NULL, par = list(nrounds = NULL, years = NULL,
                                           months = NULL, coverage = NULL)) {
  
  xpath.expr <- "/wormsim.inputfile/mass.treatments/mass.treatment/treatment.rounds/treatment.round"
  treatments <- xpathApply(doc, xpath.expr)
  
  years <- sort(rep(par$year, length(par$month)))
  months <- rep(par$month, length(par$year))
  #coverage <- rep(par$coverage, length(par$month)*length(par$year))
  
  for (i in 1:par$nrounds) { # set attributes to appropriate values
    xmlAttrs(treatments[[i]])["year"] <- paste(years[i])
    xmlAttrs(treatments[[i]])["month"] <- paste(months[i])
   # xmlAttrs(treatments[[i]])["coverage"] <- paste(coverage[i])
  }
  
  # to ask Luc if it ok to remove this and write it seperately where I parse the data to run
  #a  <-  (xmlChildren(doc)$wormsim.inputfile)[["mass.treatment"]][["treatment.rounds"]]
  #n  <-  length(xmlChildren(a))
  
  #if (par$nrounds < n){
  #  removeChildren(a, kids = as.list((par$nrounds+1):n))
  #}
  
  return(doc)
  
}


#----------------------------------------------------------------------------------  
  set.mbr <- function(doc, par = NULL) {
    
    xpath.expr <- "/wormsim.inputfile/fly/monthly.biting.rates/mbr"
    bites <- xpathApply(doc, xpath.expr)
    
    a  <-  (xmlChildren(doc)$wormsim.inputfile)[["fly"]][["monthly.biting.rates"]]
    n  <-  length(xmlChildren(a))
        
    cov <- par
    if(length(cov) < n) cov <- rep(cov, n)
    
    for (i in 1:n ) { # set attributes to appropriate values
      xmlAttrs(bites[[i]])["rate"] <- paste(cov[i])
    }
    
    return(doc)
    
  }
#----------------------------------------------------------------------------------  


#----------------------------------------------------------------------------------WORKING
set.cov <- function(doc = NULL, par = NULL) {
  
  xpath.expr <- "/wormsim.inputfile/mass.treatments/mass.treatment/treatment.rounds/treatment.round"
  treatments <- xpathApply(doc, xpath.expr)
  
  a  <-  (xmlChildren(doc)$wormsim.inputfile)[["mass.treatments"]][["mass.treatment"]][["treatment.rounds"]]
  n  <-  length(xmlChildren(a))
  
  coverage <- par
  if(length(coverage) < n) coverage <- rep(coverage, n)
  
  for (i in 1:n) { # set attributes to appropriate values
    
    xmlAttrs(treatments[[i]])["coverage"] <- paste(coverage[i])
    
  }
  
  return(doc)
  
}
#----------------------------------------------------------------------------------  
#----------------------------------------------------------------------------------  

set.foi <- function(doc, par = NULL) {
  
  xpath.expr <- "/wormsim.inputfile/exposure.and.contribution/external.foi/start"
  dist <- xpathApply(doc, xpath.expr)
  xmlAttrs(dist[[2]])["foi"] <- paste(par)
  
  return(doc)
  
}



#----------------------------------------------------------------------------------
# decrease foi
set.df10 <- function(doc, par = NULL) {
  
  xpath.expr <- "/wormsim.inputfile/exposure.and.contribution/external.foi/start"
  dist <- xpathApply(doc, xpath.expr)
  xmlAttrs(dist[[3]])["foi"] <- paste(par)
  
  return(doc)
  
}

set.df11 <- function(doc, par = NULL) {
  
  xpath.expr <- "/wormsim.inputfile/exposure.and.contribution/external.foi/start"
  dist <- xpathApply(doc, xpath.expr)
  xmlAttrs(dist[[4]])["foi"] <- paste(par)
  
  return(doc)
  
}

set.df12 <- function(doc, par = NULL) {
  
  xpath.expr <- "/wormsim.inputfile/exposure.and.contribution/external.foi/start"
  dist <- xpathApply(doc, xpath.expr)
  xmlAttrs(dist[[5]])["foi"] <- paste(par)
  
  return(doc)
  
}

set.df13 <- function(doc, par = NULL) {
  
  xpath.expr <- "/wormsim.inputfile/exposure.and.contribution/external.foi/start"
  dist <- xpathApply(doc, xpath.expr)
  xmlAttrs(dist[[6]])["foi"] <- paste(par)
  
  return(doc)
  
}

  
  
  set.wormskilled <- function(doc, par = NULL) {
    
    xpath.expr <- "/wormsim.inputfile/mass.treatments/mass.treatment/treatment.effects"
    dist <- xpathApply(doc, xpath.expr)
    xmlAttrs(dist[[1]])["fraction.killed"] <- paste(par)
    return(doc)
  }  
  
  
  set.recovery <- function(doc, par = NULL) {
    
    xpath.expr <- "/wormsim.inputfile/mass.treatments/mass.treatment/treatment.effects"
    dist <- xpathApply(doc, xpath.expr)
    xmlAttrs(dist[[1]])["period.of.recovery"] <- paste(par)
    return(doc)
  }  
  
  
  set.mfsurv <- function(doc, par = NULL) {
    
    xpath.expr <- "/wormsim.inputfile/mass.treatments/mass.treatment/treatment.effects/fraction.mf.surviving"
    dist <- xpathApply(doc, xpath.expr)
    xmlAttrs(dist[[1]])["mean"] <- paste(par)
    return(doc)
  }  
  
### END OF CODE
  
