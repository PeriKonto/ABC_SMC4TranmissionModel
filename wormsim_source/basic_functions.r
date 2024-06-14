# Functions to run simulations in Wormsim from R.
# Author: Luc Coffeng
# Date created: Oct 11, 2014
# Wormsim version: 2.58Ap7b


# Determine OS and set appropriate commands.
  if(.Platform$OS.type == "unix") {
    run.command <- "./run.sh"
    avg.command <- "./avg.sh"
  }
  if(.Platform$OS.type == "windows") {
    run.command <- "run.bat"
    avg.command <- "avg.bat"
  }

  
# Load packages.
  library(abind)
  library(data.table)
  
  
# Run simulation.
#   input.file = input file name without xml extension (character).
#   seed.start = first seed (integer).
#   seed.end = last seed (integer).
  run.sim <- function(input.file = "", seed.start = 0, seed.end = 9) {
    
    system(
      command = paste(run.command, input.file, seed.start, seed.end, sep = " "),
      wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE
    )
    
  }

  
# Process simulation output.
#   input.file = input file name without xml extension (character).
#   seed.start = first seed (integer).
#   seed.end = last seed (integer).
  process.sim <- function(input.file = "", seed.start = 0, seed.end = 9) {
    
    system(
      command = paste(avg.command, input.file, seed.start, seed.end, sep = " "),
      wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE
    )
    
  }


# Run and process simulation.
#   input.file = input file name without xml extension (character).
#   seed.start = first seed (integer).
#   seed.end = last seed (integer).
#   delete.txt = delete text output files and only conserve zip file (logical).
  run.proc.sim <- function(input.file = "", seed.start = 0, seed.end = 9, delete.txt = TRUE) {
    
    run.sim(input.file = input.file, seed.start = seed.start, seed.end = seed.end)
    process.sim(input.file = input.file, seed.start = seed.start, seed.end = seed.end)
    
    if(delete.txt == TRUE) unlink(c("*.txt", "*.log"))
    
  }
  
  
# Read output file (must be tab-delimited).
#   file.root = root of output file name, including extension (character) and tags (X, Y, Z).
#   type = any single of c("", "X", "Y", "Z"), referring to the tag in the file name (character).
#          "" = summary output per simulation.
#          "X" = age-specific standard output per simulation.
#          "Y" = age-specific mf distribution per simulation.
#          "Z" = age-specific OV16 output per simulation.
  read.output <- function(file.root = "", type = "") {
    
    x <- read.table(
      paste(file.root, type, ".txt", sep = ""),
      header = TRUE,
      sep = "\t",
      dec = ".",
      check.names = FALSE
    )
    
    names(x)[3:6] <- paste("M", names(x)[3:6], sep = "")
    names(x)[7:10] <- paste("F", names(x)[7:10], sep = "")
    
    return(x)
  }
  
  
# Unzip archive and read group of output files for individuals simulations.
#   file.root = base of zip file name excluding extension and seed numbers (character).
#   seed.start = first seed (integer).
#   seed.end = last seed (integer).
#   type = any single of c("", "X", "Y", "Z"), referring to the tag in the file name (character).
#          "" = summary output per simulation.
#          "X" = age-specific standard output per simulation.
#          "Y" = age-specific mf distribution per simulation.
#          "Z" = age-specific OV16 output per simulation.
#   write.table = toggle to write csv file (logical).
#   write.path = target path for csv file (character).
  read.output.ind <- function(file.root = "", seed.start = 0, seed.end = 9,
                              type = "", write.table = FALSE, write.path = "") {
    
    base.dir <- getwd()
    temp.dir <- "unzip_temp"
    zip.file <- paste(file.root, seed.start, "-", seed.end, ".zip", sep = "")
    
  # Create temporary folder (clear temporary folder if already present).
    if (file.exists(temp.dir)){
      unlink(temp.dir, recursive = TRUE)
    }
    dir.create(file.path(base.dir, temp.dir))
    
  # Unzip specified type of files from archive.
    files <- as.character(unzip(zip.file, list = TRUE)[,1])
    type.index <- grep(
      pattern = paste("^",file.root, type, "\\.", "[0-9]{3}", sep = ""),
      x = files
    )
    files <- files[type.index]
    unzip(zip.file, files = files, exdir = file.path(base.dir, temp.dir))
  
  # Read output files.
    setwd(file.path(base.dir, temp.dir))
    
    test <- read.table(
      files[1],
      header = TRUE,
      sep = "\t",
      dec = ".",
      check.names = FALSE
    )
  
    output <- array(
      data = NA,
      dim = c(dim(test),length(files)),
      dimnames = list(
        NULL,
        names(test),
        files
      )
    )
  
    for (i in 1:length(files)) {
      output[,,i]  <- as.matrix(
        read.table(
          files[i],
          header = TRUE,
          sep = "\t",
          dec = ".",
          check.names = FALSE
        )
      )
    }
  
  # Clean up, write table, and return output.
    setwd(base.dir)
    unlink(temp.dir, recursive = TRUE)
    
    if(write.table == TRUE) {
      if(write.path == "") {
        setwd(base.dir)
      } else {
        setwd(write.path)
      }
      
      output.table <- matrix(
        data = aperm(output, c(1,3,2)),
        nrow = dim(output)[1] * dim(output)[3],
        ncol = dim(output)[2],
        dimnames = list(
          NULL,
          dimnames(output)[[2]]
        )
      )
      
      output.table <- cbind(
        seed = sort(rep(seed.start:seed.end, dim(output)[1])),
        output.table
      )
      
      write.csv(
        x = output.table,
        file = paste(file.root, "_runs", ".csv", sep = ""),
        row.names = FALSE
      )
      
      setwd(base.dir)
    }
    return(output)
  
  }
  
  
  