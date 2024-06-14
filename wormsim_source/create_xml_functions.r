# Load required libraries
  library(XML)

# Create an XML input file, based on the following arguments
#   template = XML template file.
#   schema = XML schema to validate XML input file.
#   xml.name = file name for generated XML input file.
#   pars = named list of named lists with alternative parameter values; only
#          non-NULL elements will be used to redefine parameters in the input 
#          file; assigment of values is based on the element names, using the
#          function "set.[name]".
#   NOTE: the input.template provided should contain a sufficient number of
#   treatment rounds i.e. at least the maximum of rounds (19 in this case); the
#   contents of these lines is not important as they will be overwritten.
  generate.inputfile <- function(template, schema, xml.name,
                                 pars = list(zeta = NULL, mda = NULL)) {
    
  # Parse XML template
    xmldoc <- read.inputfile(template)
    
  # Edit the parsed XML file
    for (i in 1:length(pars)) {
      if(!is.null(pars[[i]])) {
        xmldoc <- do.call(
          what = paste("set.", names(pars)[i], sep = ""),
          args = list(doc = xmldoc, par = pars[[i]]))
      }
    }
    
  # Validate edited XML file
    validation <- xmlSchemaValidate(schema, xmldoc)
    if (validation[[1]] != 0) {
      print(validation)
      stop("XML validation failed")
    }
  
  # Write XML file
    write.inputfile(xmldoc, xml.name)
      
  }
  

# Read an XML input file  
  read.inputfile <- function(filename) {
    
    xmlInternalTreeParse(filename)
    
  }  
  

# Write an XML input file
  write.inputfile <-  function(xmldoc, filename) {
    
    outfilename <- paste(filename, "xml", sep = ".")
    sink(file = outfilename)
    print(xmldoc)
    sink()
    
  }
  