# Function to run WORMSIM
  run_wormsim <- function(wormsim_dir,
                          i_run,
                          input_schema,
                          par_alt,
                          seed_start,
                          seed_end) {

    # Create folder to work in
      setwd(wormsim_dir)
      input.file.name <- paste("par_set_", i_run, sep = "")
      dir.create(file.path(wormsim_dir, input.file.name), showWarnings = FALSE)
      file.copy(dir(), file.path(wormsim_dir, input.file.name), overwrite = TRUE)
      setwd(file.path(wormsim_dir, input.file.name))

    # Run model
      generate.inputfile(template = "template2MDA.xml",
                         schema = input_schema,
                         xml.name = input.file.name,
                         pars = par_alt)
      

      run.proc.sim(
        input.file = input.file.name,
        seed.start = seed_start,
        seed.end = seed_end,
        delete.txt = FALSE
      )

      
    # Read summary files
      summ <- data.table(read.output(input.file.name, type = ""))
      age <- data.table(read.output(input.file.name, type = "X"))
      intensity <- data.table(read.output(input.file.name, type = "Y"))

    # Clean up
      setwd(wormsim_dir)
      unlink(input.file.name, recursive = TRUE)

    # Return result
      with(par_alt,
           list(par_set = i_run,
                mbr = mbr,
                exposure.p1 = exposure.p1,
                #recovery = recovery,
                #wormskilled =wormskilled,
                seed = seed_start:seed_end,
                summ = summ,
                age = age,
                intensity = intensity))
  }

  
  # Function to evaluate fit 
  extract_intens_prev <- function(x) {
    
    x[, None := get("M-0") + get("F-0")]
    x[, Light := get("M-2") + get("F-2")]
    x[, Moderate := get("M-16") + get("F-16")]
    x[, Heavy := get("M16+") + get("F16+")]
    x[, N := None + Light + Moderate + Heavy]
    x <- melt(data = x,
              id.vars = c("year", "age"),
              measure.vars = c("None", "Light", "Moderate", "Heavy"),
              variable = "intens",
              value.name = "cases")
    x[, intens := factor(intens, levels = rev(levels(intens)), ordered=TRUE)]
    x[, prev := cases / sum(cases), by = .(age, year)]
    
    setkey(x, age,intens)
    
    return(x)
  }
  
