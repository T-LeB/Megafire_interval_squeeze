#### Code for paper "Megafire-induced interval squeeze threatens vegetation at landscape scales" ####
#### megafire_interval_squeeze_code-00-fn-gdal_calc_py ####

#### gdal_calc_py ####
## Wrapper function that specifies inputs to call gdal_py_cmd_builder to compile a python/gdal 
## command to be sent to and run through windows command shell.
##
## Notes
##  1. For my needs this function only required to inputs but you can include more by adding additional input
##    input files (i.e. input_fileC, ...D etc) and assigning them to additional variables (i.e. C, D ... etc)
##  2. NoDataValue - this is only required when you want to assign a NoDataValue. In the main code for my
##    megafire_interval_squeeze analysis I had no use for this and it was only used when creating mappable
##    layers for figures. Accordingly all nodatavalue variables and inputs are hashed out here. If you want to
##    recreate figures either remove the hashes and save this code again or save as a new script and update the
##    code for constructing figures.
##
## Code developed by Tom Le Breton (t.lebreton@unsw.edu.au)



gdal_calc_py <- function(input_fileA, input_fileB, calculation, output_file, #nodatavalue
){
  
  ## As this function uses a modified version of the gdal_cmd_builder that allows the use of python wrappers
  ## you need to set up the file paths so that it can still find the gdal files and dependencies and include
  ## a value for the path to the python program itself.
  
  ## first add the modified function gdal_py_cmd_builder into the global environment
  
  source("megafire_interval_squeeze_code-00-fn-gdal_py_cmd_builder.R")
  
  ## Set the PATH so the function can find where all the gdal files and dependencies are installed
  ## make sure to undo this with sys.unsetenv after - Potentially shift this outside the eventual for loop so you onyl run it once
  
  Sys.setenv(PATH = normalizePath("C:/OSGeo4W64/bin"))
  
  
  ## Input variables for the gdal_py_cmd_builder function.
  
  py_exe_path <- normalizePath("C:/OSGeo4W64/apps/Python37/python.exe") ## path to the python program
  
  executable <- "gdal_calc.py"
  
  parameter_variables <- list(
    character = list(
      varnames <- c("calc", "A", "B", "outfile", "type", #"NoDataValue"
      )),
    repeatable = list(
      varnames <- c("co")),
    logical = list(
      varnames <- c("overwrite")))
  
  parameter_order <- c("A", "B", "calc", "outfile", "type", "co", #"NoDataValue",
                       "overwrite")
  
  parameter_doubledash <- c("calc", "outfile", "type", "co", "overwrite", #"NoDataValue"
  )
  
  ## Assign the values for the variables to run. This is where the gdal_calc function inputs get used
  
  parameter_values <- list(
    A = input_fileA,
    B = input_fileB,
    calc = calculation,
    outfile = output_file,
    type = "Int16",
    co = c("COMPRESS=LZW","predictor=2"),
    #NoDataValue = nodatavalue,
    overwrite = FALSE)
  
  ## call the gdal_cmd_builder function to be passed to the system and run in gdal
  
  shell(gdal_py_cmd_builder(
    py_exe_path = py_exe_path,
    executable = executable,
    parameter_variables = parameter_variables,
    parameter_values = parameter_values,
    parameter_order = parameter_order,
    parameter_doubledash = parameter_doubledash,
    python_util = TRUE,
    verbose = TRUE),
    intern=TRUE,
    translate = FALSE)
  
  
}