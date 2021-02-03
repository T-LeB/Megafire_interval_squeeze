#### Code for paper "Megafire-induced interval squeeze threatens vegetation at landscape scales" ####
#### megafire_interval_squeeze_code-00-fn-gdal_py_cmd_builder ####

#### gdal_py_cmd_builder ####
## Compiles a python/gdal command to be sent to and run through windows command shell.
##
## This is a modified version of the gdal_cmd_builder function in gdal_utils package v 2.0.3.2 
## has been altered to call .py gdal wrapper functions like gdal_calc.py
## Greenberg, J.A. & Mattiuzzi, M. Package 'gdalUtils'. https://cran.r-project.org/web/packages/gdalUtils/index.html (2020).
##
## Code modified and developed by Tom Le Breton (t.lebreton@unsw.edu.au)

## CHANGES:
##  1. Assigning value 'executable' has been changed so that the path now goes to the
##    local installation of python scripts for gdal i.e.
##      getOption("gdalUtils_gdalPath")[[gdal_installation_id]]$path is now "C:/new_dir/Python/etc"
##      I havent got around to generalising this yet but should be straightforward enought to manually find
##      the path you need and chuck it in here.
##
##  2. Not an internal change as of now but becuase the first change removes the gdal bin from the system path
##      you now need to run Sys.setenv(PATH = "path to gdal bin") before running the function so that it can find 
##      the necessary dependencies and gdal files.
##
##  3. Added input py_exe_path parameter to function. This is a path to the local installation of Python.exe
##      which is pasted before the executable string in the output so that the shell command knows to open python.exe
##      before running the rest of the code
##
##  4. Removed qm() around executable when compiling final command, this was causing problems when it was included
##      and i tried to run it not sure why but resolved once i removed it. 
##
##


gdal_py_cmd_builder <- function (py_exe_path, executable, parameter_variables = c(), parameter_values = c(), 
                                 parameter_order = c(), parameter_noflags = c(), parameter_doubledash = c(), 
                                 parameter_noquotes = c(), gdal_installation_id = 1, python_util = FALSE, 
                                 verbose = FALSE) 
{
  if (verbose) 
    message("Checking installation...")
  gdal_setInstallation()
  if (is.null(getOption("gdalUtils_gdalPath"))) 
    return()
  executable <- normalizePath(list.files("C:/OSGeo4W64/apps/Python37/Scripts", 
                                         executable, full.names = TRUE))
  if (!file.exists(executable) && !file.exists(paste0(executable, 
                                                      ".exe"))) {
    stop(paste0(executable, " does not exist on your system.  Please check your installation."))
  }
  parameter_variables_types <- names(parameter_variables)
  defined_variables <- names(parameter_values)[sapply(parameter_values, 
                                                      function(X) class(X)[1] != "name")]
  if (verbose) 
    message("Setting up logical variables...")
  if (any("logical" %in% parameter_variables_types)) {
    parameter_variables_logical <- parameter_variables$logical[[1]]
    parameter_variables_logical_defined <- defined_variables[defined_variables %in% 
                                                               parameter_variables_logical]
    if (length(parameter_variables_logical_defined) > 0) {
      parameter_variables_logical_defined_true <- sapply(parameter_variables_logical_defined, 
                                                         function(X, parameter_values) {
                                                           return(parameter_values[[which(names(parameter_values) == 
                                                                                            X)]])
                                                         }, parameter_values = parameter_values)
      parameter_variables_logical_strings <- sapply(parameter_variables_logical_defined, 
                                                    function(X, parameter_doubledash) {
                                                      if (X %in% parameter_noflags) {
                                                        flag = NULL
                                                      }
                                                      else {
                                                        if (X %in% parameter_doubledash) {
                                                          flag = paste("--", X, " ", 
                                                                       sep = "")
                                                        }
                                                        else {
                                                          flag = paste("-", X, " ", sep = "")
                                                        }
                                                      }
                                                      return(flag)
                                                    }, parameter_doubledash = parameter_doubledash)
      names(parameter_variables_logical_strings) <- names(parameter_variables_logical_defined_true)
      parameter_variables_logical_strings <- parameter_variables_logical_strings[parameter_variables_logical_defined_true == 
                                                                                   T]
    }
    else {
      parameter_variables_logical_strings <- NULL
    }
  }
  if (verbose) 
    message("Setting up vector variables...")
  if (any("vector" %in% parameter_variables_types)) {
    parameter_variables_vector <- parameter_variables$vector[[1]]
    parameter_variables_vector_defined <- defined_variables[defined_variables %in% 
                                                              parameter_variables_vector]
    if (length(parameter_variables_vector_defined) > 0) {
      parameter_variables_vector_strings <- sapply(parameter_variables_vector_defined, 
                                                   function(X, parameter_values, parameter_doubledash) {
                                                     if (X %in% parameter_noflags) {
                                                       flag = NULL
                                                     }
                                                     else {
                                                       if (X %in% parameter_doubledash) {
                                                         flag = paste("--", X, " ", 
                                                                      sep = "")
                                                       }
                                                       else {
                                                         flag = paste("-", X, " ", sep = "")
                                                       }
                                                     }
                                                     if (X %in% parameter_noquotes) {
                                                       parameter_variables_vector_string <- paste(flag, 
                                                                                                  paste(parameter_values[[which(names(parameter_values) == 
                                                                                                                                  X)]], collapse = " "), sep = "")
                                                     }
                                                     else {
                                                       parameter_variables_vector_string <- paste(flag, 
                                                                                                  qm(paste(parameter_values[[which(names(parameter_values) == 
                                                                                                                                     X)]], collapse = " ")), sep = "")
                                                     }
                                                     return(parameter_variables_vector_string)
                                                   }, parameter_values = parameter_values, parameter_doubledash = parameter_doubledash)
    }
    else {
      parameter_variables_vector_strings <- NULL
    }
  }
  else {
    parameter_variables_vector_strings <- NULL
  }
  if (verbose) 
    message("Setting up scalar variables...")
  if (any("scalar" %in% parameter_variables_types)) {
    parameter_variables_scalar <- parameter_variables$scalar[[1]]
    parameter_variables_scalar_defined <- defined_variables[defined_variables %in% 
                                                              parameter_variables_scalar]
    if (length(parameter_variables_scalar_defined) > 0) {
      parameter_variables_scalar_strings <- sapply(parameter_variables_scalar_defined, 
                                                   function(X, parameter_values, parameter_doubledash) {
                                                     if (X %in% parameter_noflags) {
                                                       flag = NULL
                                                     }
                                                     else {
                                                       if (X %in% parameter_doubledash) {
                                                         flag = paste("--", X, " ", 
                                                                      sep = "")
                                                       }
                                                       else {
                                                         flag = paste("-", X, " ", sep = "")
                                                       }
                                                     }
                                                     parameter_variables_scalar_string <- paste(flag, 
                                                                                                qm(parameter_values[[which(names(parameter_values) == 
                                                                                                                             X)]]), sep = "")
                                                     return(parameter_variables_scalar_string)
                                                   }, parameter_values = parameter_values, parameter_doubledash = parameter_doubledash)
    }
    else {
      parameter_variables_scalar_strings <- NULL
    }
  }
  else {
    parameter_variables_scalar_strings <- NULL
  }
  if (verbose) 
    message("Setting up character variables...")
  if (any("character" %in% parameter_variables_types)) {
    parameter_variables_character <- parameter_variables$character[[1]]
    parameter_variables_character_defined <- defined_variables[defined_variables %in% 
                                                                 parameter_variables_character]
    if (length(parameter_variables_character_defined) > 0) {
      parameter_variables_character_strings <- sapply(parameter_variables_character_defined, 
                                                      function(X, parameter_values, parameter_noflags, 
                                                               parameter_doubledash) {
                                                        if (X %in% parameter_noflags) {
                                                          flag = NULL
                                                        }
                                                        else {
                                                          if (X %in% parameter_doubledash) {
                                                            flag = paste("--", X, " ", 
                                                                         sep = "")
                                                          }
                                                          else {
                                                            flag = paste("-", X, " ", sep = "")
                                                          }
                                                        }
                                                        parameter_variables_character_string <- paste(flag, 
                                                                                                      qm(parameter_values[[which(names(parameter_values) == 
                                                                                                                                   X)]]), sep = "")
                                                        return(parameter_variables_character_string)
                                                      }, parameter_values = parameter_values, parameter_noflags = parameter_noflags, 
                                                      parameter_doubledash = parameter_doubledash)
    }
    else {
      parameter_variables_character_strings <- NULL
    }
  }
  else {
    parameter_variables_character_strings <- NULL
  }
  if (verbose) 
    message("Setting up repeatable variables...")
  if (any("repeatable" %in% parameter_variables_types)) {
    parameter_variables_repeatable <- parameter_variables$repeatable[[1]]
    parameter_variables_repeatable_defined <- defined_variables[defined_variables %in% 
                                                                  parameter_variables_repeatable]
    if (length(parameter_variables_repeatable_defined) > 
        0) {
      parameter_variables_repeatable_strings <- sapply(parameter_variables_repeatable_defined, 
                                                       function(X, parameter_values, parameter_doubledash) {
                                                         if (X %in% parameter_noflags) {
                                                           flag = NULL
                                                         }
                                                         else {
                                                           if (X %in% parameter_doubledash) {
                                                             flag = paste("--", X, " ", 
                                                                          sep = "")
                                                           }
                                                           else {
                                                             flag = paste("-", X, " ", sep = "")
                                                           }
                                                         }
                                                         if (X %in% parameter_noquotes) {
                                                           parameter_variables_repeatable_string <- paste(paste(flag, 
                                                                                                                (parameter_values[[which(names(parameter_values) == 
                                                                                                                                           X)]]), sep = ""), collapse = " ")
                                                         }
                                                         else {
                                                           parameter_variables_repeatable_string <- paste(paste(flag, 
                                                                                                                qm(parameter_values[[which(names(parameter_values) == 
                                                                                                                                             X)]]), sep = ""), collapse = " ")
                                                         }
                                                         return(parameter_variables_repeatable_string)
                                                       }, parameter_values = parameter_values, parameter_doubledash = parameter_doubledash)
    }
    else {
      parameter_variables_repeatable_strings <- NULL
    }
  }
  else {
    parameter_variables_repeatable_strings <- NULL
  }
  if (verbose) 
    message("Setting up noflag variables...")
  if (!is.null(parameter_noflags)) {
    parameter_variables_noflag_strings <- sapply(parameter_noflags, 
                                                 function(X, parameter_values) {
                                                   parameter_variables_noflag_string <- paste(parameter_values[[which(names(parameter_values) == 
                                                                                                                        X)]], sep = "")
                                                   return(parameter_variables_noflag_string)
                                                 }, parameter_values = parameter_values)
  }
  else {
    parameter_variables_noflag_strings <- NULL
  }
  if (verbose) 
    message("Putting them all together...")
  parameter_vector <- c(parameter_variables_logical_strings, 
                        parameter_variables_vector_strings, parameter_variables_scalar_strings, 
                        parameter_variables_character_strings, parameter_variables_repeatable_strings, 
                        parameter_variables_noflag_strings)
  if (!missing(parameter_order)) {
    parameter_order_defined <- parameter_order[which(parameter_order %in% 
                                                       names(parameter_vector))]
    parameter_vector <- parameter_vector[parameter_order_defined]
  }
  parameter_vector <- sapply(parameter_vector, function(x) paste(x, 
                                                                 collapse = " "))
  cmd <- paste(c(py_exe_path, executable, parameter_vector), collapse = " ")
  return(cmd)
}