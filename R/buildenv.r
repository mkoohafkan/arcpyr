#' R interface to arcpy help
#'
#' Print the help documentation for an arcpy function.
#'
#' @param tool The tool to get help for.
#'
#' @examples
#' \dontrun{
#' arcpy = arcpy_env()
#' tool_help(arcpy$Delete_managment)
#' tool_help(arcpy$env)
#' }
#' @export
tool_help = function(tool) {
  field = tail(unlist(strsplit(deparse(substitute(tool)), split = "$",
    fixed = TRUE)), -1)
  PythonInR::pyHelp(paste(c("arcpy", field), collapse = "."))
}

#' R Interface to ArcPy
#'
#' An R interface for the ArcGIS Python module \code{arcpy}. Relies on 
#' the virtual Python environment provided by \code{PythonInR} to generate 
#' function interfaces for \code{arcpy} tools and environment settings.
#' \code{arcpy} toolboxes and extensions are accessed via an R environment
#' object. See the vignette to get started.
#' @name arcpyr-package
#' @aliases arcpyr
#' @docType package
#' @seealso \code{\link{arcpy_env}}, \code{\link{connect_ArcGIS}}
NULL


#' @importFrom utils head
#' @importFrom utils tail
envfun = function(value) {
  field = tail(unlist(
    strsplit(deparse(match.call()), split = "$", fixed = TRUE)), 1)
  field = head(unlist(strsplit(field, "(", fixed = TRUE)), 1)
  if (!missing(value)) {
    if (is.logical(value)) {
      value = gsub("TRUE", "True", value)
      value = gsub("FALSE", "False", value)
    } else if (is.character(value)) {
      value = paste0("'", value, "'")
    }
    PythonInR::pyExec(paste0('arcpy.env.', field, ' = ', value))
  }
  res = PythonInR::pyGet(paste0("arcpy.env.", field))
  if (is.null(res))
    res = ""
  res
}

#' Checkout Extension
#'
#' Checkout an ArcGIS extension. 
#'
#' @param ext The extension to check out.
#' 
#' @export
checkout_extension = function(ext) {
  if (PythonInR::pyGet(sprintf('arcpy.CheckOutExtension("%s")', ext)) != "CheckedOut")
    stop(sprintf("extension '%s' is not available.", ext), call. = FALSE)
  invisible(NULL)
}

import_toolbox = function(toolbox, tools) {
  switch(toolbox,
    "sa" = checkout_extension("Spatial"),
    "ga" = checkout_extension("Geostats"),
    "na" = checkout_extension("Network"),
    "3d" = checkout_extension("3D"),
    "ta" = checkout_extension("Tracking"),
    "ba" = checkout_extension("Business"),
    "interop" = checkout_extension("DataInteroperability"),
    "schematics" = checkout_extension("Schematics")
  )
  toolbox.env = new.env(parent = emptyenv())
  if (missing(tools))
    tools = list_tools(toolbox)
  if (toolbox != "*") {
    tools = gsub("(\\_[a-zA-Z0-9]+)$", "", tools)
    tools = sprintf("%s_%s", tools, toolbox)
    names(tools) = gsub("(\\_[a-zA-Z0-9]+)$", "", tools)
  } else {
    names(tools) = tools
  }
  missing.tools = tools[which(!(tools %in% list_tools(toolbox)))]
  if (length(missing.tools > 0)) {
    warning("The following tools could not be found: ", 
      paste(missing.tools, collapse = ", "), ".", call. = FALSE)
  tools = tools[!(tools %in% missing.tools)]
  }
  for (n in names(tools))
    assign(n, PythonInR::pyFunction(sprintf("arcpy.%s", tools[[n]])), 
      pos = toolbox.env)
  toolbox.env
}

#' Attach Toolbox
#'
#' Attach an ArcGIS toolbox.
#'
#' @param envir The environment to attach the toolbox to.
#' @param name The name of the toolbox.
#' @param tool.list A list of tools from the toolbox to expose. 
#'   If NULL, all tools in the toolbox will be exposed.
#'
#' @seealso \code{\link{attach_custom}}
#' @export
attach_toolbox = function(envir, name, tool.list) {
  toolbox = import_toolbox(name, tool.list)
  assign(name, toolbox, pos = envir)
  invisible(envir)
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
#' connect_ArcGIS()
#'
#' # connect to the 32-bit ArcGIS Desktop 10.2 Python environment
#' connect_ArcGIS("C:/Python27/ArcGIS10.2")
#'
#' # connect to the 64-bit ArcGIS Desktop 10.2 Python environment
#' connect_ArcGIS("C:/Python27/ArcGISx6410.2")
#'
#' # connect to the ArcGIS Pro 1.3 Python environment
#' connect_ArcGIS("C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3",
#'   Pro = TRUE)
#' }
#'
#' @seealso \code{\link{arcpy_env}}
#' @export
connect_ArcGIS = function(python_folder, Pro = FALSE, quietly = TRUE) {
  # check if Python is already connected
  if (PythonInR::pyIsConnected()) {
    if (!quietly)
      warning("R is already connected Python.")
    return(invisible(NULL))
  }
  # ArcGIS Pro
  if (Pro) {
    if (missing(python_folder))
      python_folder = "C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3"
    if (!dir.exists(python_folder))
      stop("Could not find the ArcGIS Pro Python folder.")
    if (quietly)
      suppressPackageStartupMessages(
        PythonInR:::pyConnectWinDll(
          dllName = list.files(python_folder, pattern = "python[0-9]+.dll"),
          pyArch = "64bit",
          majorVersion = 3,
          dllDir = python_folder,
          pythonHome = python_folder
        ))
    else
      PythonInR:::pyConnectWinDll(
        dllName = list.files(python_folder, pattern = "python[0-9]+.dll"),
        pyArch = "64bit",
        majorVersion = 3,
        dllDir = python_folder,
        pythonHome = python_folder
      )
    return(invisible(NULL))
  }
  # ArGIS Desktop
  if (missing(python_folder)) {
    if (.Platform$r_arch == "x64") {
      python_folder = file.path("C:/Python27",
        dir("C:/Python27")[grepl("ArcGIS.*x64", dir("C:/Python27"))])
      arch = "64bit"
    } else {
      python_folder = file.path("C:/Python27",
        dir("C:/Python27")[grepl("ArcGIS.*", dir("C:/Python27")) &
          !(grepl("ArcGIS.*x64", dir("C:/Python27")))])
      arch = "32bit"
    }
    if (length(python_folder) > 1)
      stop("Multiple ArcGIS Desktop Python folders detected. ",
        "Please manually specify the Python path.")
    if (length(python_folder) < 1)
      stop("Could not find ArcGIS Desktop Python folder.")
    if (!file.exists(python_folder))
      stop("Could not find ArcGIS Desktop Python folder.")
    }
  PYTHON_EXE = file.path(python_folder, "python.exe")
  if (!file.exists(PYTHON_EXE))
    stop("Could not find python.exe in ", python_folder)
  if (quietly)
    suppressPackageStartupMessages(PythonInR::pyConnect(PYTHON_EXE))
  else
    PythonInR::pyConnect(PYTHON_EXE)
  PythonInR::pyExec("import arcpy")
  installinfo = PythonInR::pyGet("arcpy.GetInstallInfo()", 
    simplify = FALSE)
  message(sprintf("connected to ArcGIS %s %s (build %s)", 
    installinfo$ProductName, installinfo$Version, 
    installinfo$BuildNumber))
  return(invisible(NULL))
}

#' ArcPy Environment
#'
#' Create an environment that contains ArcPy function interfaces. 
#'
#' @param tool.list A list of tools from the toolbox to expose. 
#'   If NULL, all tools in the toolbox will be exposed.
#' @param env.list A list of geoprocessing environment settings to 
#'   expose. If NULL, all environment settings will be exposed.
#' @return An environment object containing arcpy tools and 
#'   environment settings.
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
#'
#' ls(arcpy$env)
#' arcpy$env$workspace("path_to_workspace")
#' }
#'
#' @seealso \code{\link{connect_ArcGIS}}
#' @export
arcpy_env = function(tool.list, env.list) {
  if (!PythonInR::pyIsConnected())
    stop("Python is not connected.")
  # create the arcpy environment
  if (missing(tool.list))
    tool.list = list_tools("*")
  arcpyr = import_toolbox("*", tool.list)
  # add arcpy.env settings
  arcpyr.env = new.env(parent = emptyenv())
  if (!missing(env.list) && is.null(env.list))
    return(arcpyr)
  if (missing(env.list))
    env.list = list_gpenv()
  missing.env = env.list[which(!(env.list %in% list_gpenv()))]
  if (length(missing.env > 0)) {
    warning("The following environment settings could not be found: ",
      paste(missing.env, collapse = ", "), ".", call. = FALSE)
    env.list = env.list[!(env.list %in% missing.env)]
  }
  for (f in env.list)
    assign(f, envfun, pos = arcpyr.env)
  # attach env to arcpy environment
  assign("env", arcpyr.env, pos = arcpyr)
  arcpyr
}

#' Attach a Custom Toolbox
#'
#' Attach a custom toolbox to ArcPy.
#'
#' @param envir The environment to attach the toolbox to.
#' @param name The alias of the toolbox to attach. 
#'   Underscores \code{_} are not allowed.
#' @param tool.list A list of tools from the toolbox to expose. 
#'   If NULL, all tools in the toolbox will be exposed.
#' @param input.file The input file containing the toolbox.
#'
#' @seealso \code{\link{attach_toolbox}}
#' @export
attach_custom = function(envir, name, tool.list, input.file){
  if (grepl("\\_", name))
    stop(sprintf("The 'name' argument is valid. Try using '%s' instead.",
      gsub("\\_", "", name)))
  if (!file.exists(input.file))
    stop("The input file could not be found.")
  PythonInR::pyExec(sprintf('arcpy.ImportToolbox("%s", "%s")', 
    input.file, name))
  if (missing(tool.list))
    tool.list = list_tools(name)
  attach_toolbox(envir, name, tool.list)
}

#' List Tools
#'
#' List ArcGIS tools available in arcpy.
#'
#' @param toolbox The toolbox name.
#' @return A list of tools.
#'
#' @export
list_tools = function(toolbox = "*") { 
  PythonInR::pyGet(sprintf('arcpy.ListTools("*_%s")', toolbox), 
    simplify = FALSE)
}

#' List Geoprocessing Environments
#'
#' List ArcGIS geoprocessing environment settings, i.e. elements of 
#'   \code{arcpy.env}.c
#'
#' @param wc A wild card (regular expression) to filter the results.
#' @return A list of geoprocessing environment names.
#'
#' @export
list_gpenv = function(wc = "*")
  PythonInR::pyGet(sprintf('arcpy.ListEnvironments("%s")', wc), 
    simplify = FALSE)

#' List Toolboxes
#'
#' List ArcGIS toolboxes.
#'
#' @return A list of toolboxes.
#'
#' @export
list_toolboxes = function() { 
  tools = list_tools()
  unique(gsub("^([a-zA-Z0-9]+\\_)", "", tools))
}

#' List ArcGIS Extensions
#'
#' List available ArcGIS extensions.
#'
#' @export 
list_extensions = function() {
  ext = c("3D", "DataReviewer", "DataInteroperability", "Airports",
    "Aeronautical", "Bathymetry", "Nautical", "LocationReferencing",
    "GeoStats", "Network", "Spatial", "Schematics", "Tracking",
    "JTX", "ArcScan", "Business", "Defense", "Foundation", "Highways",
    "StreetMap")
  ext.list = lapply(ext, function(x)
    PythonInR::pyGet(sprintf("arcpy.CheckExtension('%s')", x)))
  ext[ext.list == "Available"]
}
