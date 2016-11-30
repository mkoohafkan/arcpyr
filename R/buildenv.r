#' Raster Calculation with arcpyr
#'
#' Raster Calculation with arcpy is handled via this helper function.
#'
#' @param expressions A list of raster calculation expressions to be 
#'   executed in sequence.
#' @param inrasters A named list of file paths to Rasters used in the 
#'   calculation expressions, to be converted to ArcPy raster objects. 
#' @param outrasters A named list of file paths to save Rasters 
#'   resulting the calculation expressions.
#' @return No return value; raster files listed in \code{outrasters} 
#'   will be saved.
#'
RasterCalculator = function(expressions, inrasters = list(), outrasters = list()){
  load_exprs = paste0(names(inrasters), ' = Raster("', inrasters, '")')
  lapply(load_exprs, PythonInR::pyExec)
  lapply(expressions, PythonInR::pyExec)
  save_exprs = paste0(names(outrasters), '.save("', outrasters,'")')
  lapply(save_exprs, PythonInR::pyExec)
  invisible(NULL)
}

#' @importFrom utils head
#' @importFrom utils tail
envfun = function(value){
  field = tail(unlist(
    strsplit(deparse(match.call()), split = "$", fixed = TRUE)), 1)
  field = head(unlist(strsplit(field, "(", fixed = TRUE)), 1)
  if(!missing(value)){
    if(is.logical(value)){
      value = gsub("TRUE", "True", value)
      value = gsub("FALSE", "False", value)
    } else if(is.character(value)){
      value = paste0("'", value, "'")
    }
    PythonInR::pyExec(paste0('arcpy.env.', field, ' = ', value))
  }
  PythonInR::pyGet(paste0("arcpy.env.", field))
}

get_funs = function(module, predicate = "inspect.isfunction"){
  PythonInR::pyExec("import inspect")
  tv = gsub("\\.", "", paste0("objlist", predicate, module))
  PythonInR::pyExec(paste0(tv, "= [i[0] for i in inspect.getmembers(", 
    module, ",", predicate, ")]"))
  if (PythonInR::pyGet(sprintf("len(%s)", tv)) < 1)
    res = list()
  else
    res = PythonInR::pyGet(tv)
  PythonInR::pyExec(paste("del", tv))
  res
}

checkout_extension = function(ext){
  PythonInR::pyExec(paste0('arcpy.CheckOutExtension("', ext, '")'))
  invisible(NULL)
}

import_module = function(module, function.list){
PythonInR::pyExec(paste('import', module))
  module.env = new.env(parent = emptyenv())
  if(is.null(function.list))
    function.list = get_funs(module)
  for(fun in function.list)
    assign(fun, PythonInR::pyFunction(paste(module, fun, sep = ".")), 
      pos = module.env)
  module.env
}

#' Connect ArcGIS Python
#'
#' Connect to the ArcGIS Python environment. 
#'
#' @param python_folder The Python installation folder created by 
#'   ArcGIS. If missing, the function will attempt to automatically 
#'   detect an appropriate Python installation.
#' @param Pro Logical: If True, attempt to connect to an ArcGIS Pro 
#'   Python distribution. Otherwise, attempt to connect to an ArcGIS 
#'   Desktop Python distribution.
#' @param quietly Logical: Suppress messages and warnings.
#' @return No return value.
#'
#' @examples
#' \dontrun{
#' # Try to autodetect
#' connect_ArcPython()
#'
#' # connect to the 32-bit ArcGIS Desktop 10.2 Python environment
#' connect_ArcPython("C:/Python27/ArcGIS10.2")
#'
#' # connect to the 64-bit ArcGIS Desktop 10.2 Python environment
#' connect_ArcPython("C:/Python27/ArcGISx6410.2")
#'
#' # connect to the ArcGIS Pro 1.3 Python environment
#' connect_ArcPython("C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3",
#'   Pro = TRUE)
#' }
#' @export
connect_ArcPython = function(python_folder, Pro = FALSE, quietly = TRUE){
  # check if Python is already connected
  if(PythonInR::pyIsConnected()){
    if(!quietly)
      warning("R is already connected Python.")
    return(invisible(NULL))
  }
  # ArcGIS Pro
  if(Pro){
    if(missing(python_folder))
      python_folder = "C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3"
    if(!dir.exists(python_folder))
      stop("Could not find the ArcGIS Pro Python folder.")
    if(quietly) 
      suppressPackageStartupMessages(
        PythonInR:::pyConnectWinDll(
          dllName = "python34.dll", 
          pyArch = "64bit", 
          majorVersion = 3, 
          dllDir = python_folder, 
          pythonHome = python_folder
        ))
    else
      PythonInR:::pyConnectWinDll(
        dllName = "python34.dll", 
        pyArch = "64bit", 
        majorVersion = 3, 
        dllDir = python_folder, 
        pythonHome = python_folder
      )
    return(invisible(NULL))
  }
  # ArGIS Desktop
  if(missing(python_folder)){
    if(.Platform$r_arch == "x64"){
      python_folder = file.path("C:/Python27", 
        dir("C:/Python27")[grepl("ArcGIS.*x64",dir("C:/Python27"))])
      arch = "64bit"
    } else{
      python_folder = file.path("C:/Python27", 
        dir("C:/Python27")[grepl("ArcGIS.*",dir("C:/Python27")) & 
          !(grepl("ArcGIS.*x64", dir("C:/Python27")))])
      arch = "32bit"
    }
    if(length(python_folder) > 1)
      stop("Multiple ArcGIS Desktop Python folders detected. ",
        "Please manually specify the Python path.")
    if(length(python_folder) < 1)
      stop("Could not find ArcGIS Desktop Python folder.")
    if(!file.exists(python_folder))
      stop("Could not find ArcGIS Desktop Python folder.")
  }
  PYTHON_EXE = file.path(python_folder, "python.exe")
  if(!file.exists(PYTHON_EXE))
    stop("Could not find python.exe in ", python_folder)
  if(quietly) 
    suppressPackageStartupMessages(PythonInR::pyConnect(PYTHON_EXE))
  else 
    PythonInR::pyConnect(PYTHON_EXE)
  return(invisible(NULL))  
}

#' ArcPy Environment
#'
#' Create an environment that contains ArcPy function interfaces. 
#'
#' @param function.list A list of functions from the module to expose. 
#'   If NULL, all functions in the module will be exposed.
#' @return An environment object containing arcpy functions.
#'
#' @examples
#' \dontrun{
#' arcpy = arcpy_env()
#' ls(arcpy)
#'
#' arcpy$Delete_management(path_to_file)
#'
#' arcpy = arcpy_env(list("Delete_management", "Clip_anaylsis"))
#' ls(arcpy)
#' }
#'
#' @export
arcpy_env = function(function.list = NULL){
  if(!PythonInR::pyIsConnected())
    stop("Python is not connected.")
  # create the arcpy environment
  arcpyr = import_module("arcpy", function.list)
  # add environment functions
  arcpyr.env = new.env(parent = emptyenv())
  # add worksapce setting functions
  env.funs = get_funs("arcpy.env", "None")
  notfuns = get_funs("arcpy.env", "inspect.ismethod")
  env.funs = env.funs[!(env.funs %in% notfuns)]
  for(f in env.funs[!grepl("_", env.funs)])
  assign(f, envfun, pos = arcpyr.env)
  # attach env to arcpy environment
  assign("env", arcpyr.env, pos = arcpyr)
  arcpyr
}


#' Attach Custom Toolbox or Module
#'
#' Attach a custom toolbox or module to ArcPy.
#'
#' @param envir The environment to attach the module to.
#' @param name The name of the toolbox or module to attach.
#' @param function.list A list of functions from the module to expose. 
#'   If NULL, all functions in the module will be exposed.
#' @param input.file The input file containing the toolbox.
#'
#' @export
attach_custom = function(envir, name, function.list = NULL, input.file){
  if(!missing(input.file))
    PythonInR::pyExec(paste0("arcpy.ImportToolbox(", 
      paste(input.file, name, sep = ","), ")"))
  attach_module(name, function.list)
}

#' Attach Module
#'
#' Attach a module to arcpy environment.
#'
#' @param envir The environment to attach the module to.
#' @param module.name The name fo the module to attach.
#' @param function.list A list of functions from the module to expose. 
#'   If NULL, all functions in the module will be exposed.
#' @param checkout If required, the name of the ArcGIS extension to 
#'   check out.
#'
#' @return (Invisible) The destination environment \code{envir}.
#'
#' @details Technically, any Python module can be attached. 
#'
#' @export
attach_module = function(envir, module.name, function.list = NULL, 
  checkout = NA){
  if(!is.na(checkout))
    checkout_extension(checkout)
  module = import_module(module.name, function.list)
  if(module.name == "arcpy.sa")
    assign("RasterCalculator", RasterCalculator, module)
  newname = gsub("(arcpy\\.)", "", module.name)
  assign(newname, module, pos = envir)
  invisible(envir)
}

#' @describeIn attach_module Attach the ArcGIS Spatial Analyst module \code{arcpy.sa}.
#' @export
attach_sa = function(envir, function.list = NULL)
  attach_module(envir, "arcpy.sa", function.list, "Spatial")

#' @describeIn attach_module Attach the ArcGIS Geostatistical Analyst module \code{arcpy.ga}.
#' @export
attach_ga = function(envir, function.list = NULL)
  attach_module(envir, "arcpy.ga", function.list, "Geostats")

#' @describeIn attach_module Attach the ArcGIS Network Analyst module \code{arcpy.na}.
#' @export
attach_na = function(envir, function.list = NULL)
  attach_module(envir, "arcpy.na", function.list, "Network")

#' @describeIn attach_module Attach the ArcGIS 3D Analyst module \code{arcpy.ddd}.
#' @export
attach_ddd = function(envir, function.list = NULL)
  attach_module(envir, "arcpy.ddd", function.list, "3D")

#' @describeIn attach_module ArcGIS Tracking Analyst module \code{arcpy.ta}.
#' @export
attach_ta = function(envir, function.list = NULL)
  attach_module(envir, "arcpy.ddd", function.list, "Tracking")

#' @describeIn attach_module Attach the ArcGIS Business Analyst module \code{arcpy.ba}.
#' @export
attach_ba = function(envir, function.list = NULL)
  attach_module(envir, "arcpy.ba", function.list, "Business")

#' @describeIn attach_module Attach the ArcGIS Data Access module \code{arcpy.da}.
#' @export
attach_da = function(envir, function.list = NULL)
  attach_module(envir, "arcpy.da", function.list)

#' @describeIn attach_module Attach the ArcGIS Pro Mapping module \code{arcpy.mp}. 
#' @export
attach_mp = function(envir, function.list = NULL)
  attach_module(envir, "arcpy.mp", function.list)

#' @describeIn attach_module Attach the ArcGIS Desktop mapping module \code{arcpy.mapping}. 
#' @export
attach_mapping = function(envir, function.list = NULL)
  attach_module(envir, "arcpy.mapping", function.list)

#' @describeIn attach_module Attach the ArcGIS Desktop Fabric module \code{arcpy.fabric}.
#' @export
attach_fabric = function(envir, function.list = NULL)
  attach_module(envir, "arcpy.fabric", function.list)

#' @describeIn attach_module Attach the ArcGIS Desktop Coverage module \code{arcpy.arc}.
#' @export
attach_arc = function(envir, function.list = NULL)
  attach_module(envir, "arcpy.arc", function.list)

#' @describeIn attach_module Attach the ArcGIS Desktop Data Interoperability module \code{arcpy.interop}.
#' @export
attach_interop = function(envir, function.list = NULL)
  attach_module(envir, "arcpy.interop", function.list, "DataInteroperability")

#' @describeIn attach_module Attach the ArcGIS Desktop Schematics module \code{arcpy.schematics}.
#' @export
attach_schematics = function(envir, function.list = NULL)
  attach_module(envir, "arcpy.schematics", function.list, "Schematics")
