#' Initialize the arcypy interface
#'
#' Initialize the R-arcypy interface by opening a connection to the 
#'   ArcGIS Python installation, importing arcpy, and generating 
#'   interfaces for arcpy functions and objects. 
#'
#' @param PYTHON_EXE Path to python executable. If missing, will search for the
#'   default Windows path to the ArcGIS Python distribution.
#' @param quietly Logical: Suppress messages and warnings.
#' @return No return value.
#' @details The following arcpy calls are available:
#'   \itemize{
#'     \item \strong{Environment Settings}
#'     \item env.workspace
#'     \item env.overwriteOutput
#'     \item \strong{Listing}
#'     \item ListDatasets 
#'     \item ListFeatureClasses 
#'     \item ListFields 
#'     \item ListFiles 
#'     \item ListIndexes 
#'     \item ListRasters 
#'     \item ListTables 
#'     \item ListVersions 
#'     \item ListWorkspaces
#'     \item \strong{Extraction}
#'     \item Clip_analysis 
#'     \item Select_analysis
#'   }
#'
#' @examples
#' \dontrun{
#' arcpy.initialize("C:/Python27/ArcGIS10.3/python.exe")
#' }
#'
#' @export
arcpy.initialize <- function(PYTHON_EXE, quietly = FALSE){
  oldwarn = options("warn")$warn
  options(warn = 1)
  on.exit(options(warn = oldwarn))
  if(missing(PYTHON_EXE)){
    if(!quietly) 
      warning("PYTHON_EXE not defined. Searching default filepath based on R architecture.")
    if(.Platform$r_arch == "x64")
      PYTHON_EXE = file.path("C:/Python27", 
        dir("C:/Python27")[grepl("ArcGIS.*x64",dir("C:/Python27"))],
        "python.exe")
    else
      PYTHON_EXE = file.path("C:/Python27", 
        dir("C:/Python27")[grepl("ArcGIS.*",dir("C:/Python27")) & 
          !(grepl("ArcGIS.*x64", dir("C:/Python27")))],
        "python.exe")
  }
  if(length(PYTHON_EXE) < 1)
    stop("Could not find python.exe")
  if(!file.exists(PYTHON_EXE))
    stop("Could not find python.exe")
  if(quietly) 
    suppressPackageStartupMessages(PythonInR::pyConnect(PYTHON_EXE))
  else 
    PythonInR::pyConnect(PYTHON_EXE)
  with(parent.frame(), {
    ################################################################
    ###                    ARCPY INTERFACES                      ###
    ################################################################
    PythonInR::pyExec("import arcpy")
    # environment
    env.workspace = function(path){
      if(!missing(path))
        PythonInR::pyExec(paste0('arcpy.env.workspace = "', path, '"'))
      PythonInR::pyGet("arcpy.env.workspace")
    }
    env.overwriteOutput = function(value){
      if(!missing(value)){
        if(value)
          res = "True"
        else
          res = "False"
        PythonInR::pyExec(paste0('arcpy.env.overwriteOutput = ', res, ''))
      }
        PythonInR::pyGet("arcpy.env.overwriteOutput")
    }
    # listing data
    listing.funs = c(
      "ListDatasets", 
      "ListFeatureClasses", 
      "ListFields", 
      "ListFiles", 
      "ListIndexes", 
      "ListRasters", 
      "ListTables", 
      "ListVersions", 
      "ListWorkspaces"
    )
    PythonInR::pyImport(listing.funs, from = "arcpy")
    # extraction
    extraction.funs = c(
      "Clip_analysis", 
      "Select_analysis"
    )
    PythonInR::pyImport(extraction.funs, from = "arcpy")

  })
  invisible(NULL)
}
