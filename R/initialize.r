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
#' \subsection{Environment Settings}{
#'   \itemize{
#'     \item env.workspace
#'     \item env.overwriteOutput
#'   }
#' }
#' \subsection{Listing}{
#'   \itemize{
#'     \item ListDatasets 
#'     \item ListFeatureClasses 
#'     \item ListFields 
#'     \item ListFiles 
#'     \item ListIndexes 
#'     \item ListRasters 
#'     \item ListTables 
#'     \item ListVersions 
#'     \item ListWorkspaces
#'   }
#' }
#' \subsection{Analysis}{
#'   \itemize{
#'     \item Clip_analysis 
#'     \item Select_analysis
#'     \item Intersect_analysis
#'   }
#' }
#' \subsection{Management}{
#'   \itemize{
#'     \item AddJoin_management
#'     \item RemoveJoin_management
#'     \item JoinField_management
#'     \item Dissolve_management
#'     \item Delete_management
#'     \item MakeFeatureLayer_management
#'     \item SelectLayerByAttribute_management
#'     \item SelectLayerByLocation_management
#'     \item AlterField_management
#'   }
#' }
#' \subsection{Conversion}{
#'   \itemize{
#'     \item RasterToPolygon_conversion
#'     \item TableToTable_conversion
#'   }
#' }
#'
#' @examples
#' \dontrun{
#' arcpy.initialize("C:/Python27/ArcGIS10.3/python.exe")
#' env.workspace("C:/test.gdb")
#' env.overwriteOutput(TRUE)
#' Clip_analysis("feature1", "feature2")
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
    analysis.funs = c(
      "Clip_analysis", 
      "Select_analysis",
      "Intersect_analysis"
    )
    PythonInR::pyImport(analysis.funs, from = "arcpy")
    # management
    management.funs = c(
      "Delete_management",
      "CopyRows_management",
      "AddJoin_management",
      "RemoveJoin_management",
      "JoinField_management",
      "Dissolve_management",
      "MakeFeatureLayer_management",
      "SelectLayerByAttribute_management",
      "SelectLayerByLocation_management",
      "AlterField_management"
    )
    PythonInR::pyImport(management.funs, from = "arcpy")
    # conversion
    conversion.funs = c(
      "RasterToPolygon_conversion",
      "TableToTable_conversion"
    )
    PythonInR::pyImport(conversion.funs, from = "arcpy")
  })
  invisible(NULL)
}

#' Initialize Spatial Analyst Tools
#'
#' Initialize functions from the Spatial Analyst module.
#'
#' @return No return value.
#' @details The following arcpy.sa calls are available:
#' \subsection{Raster tools}{
#'   \itemize{
#'     \item RasterCalculator
#'   }
#' }
#'
#' @examples
#' \dontrun{
#' sa.initialize()
#' RasterCalculator(expr, "outrast")
#' }
#'
#' @export
sa.initialize = function(){
  with(parent.frame(), {
    PythonInR::pyExec('arcpy.CheckOutExtension("Spatial")')
    # RasterCalculator
    RasterCalculator = function(expressions, inrasters = list(), 
      outrasters = list()){
      load_exprs = paste0(names(inrasters), ' = Raster("', inrasters, '")')
      lapply(load_exprs, PythonInR::pyExec)
      lapply(expressions, PythonInR::pyExec)
      save_exprs = paste0(names(outrasters), '.save("', outrasters,'")')
      lapply(save_exprs, PythonInR::pyExec)
      invisible(NULL)
    }
    sa.funs = c(
      "Raster",
      "Con",
      "Pick",
      "SetNull",
      "Abs",
      "Exp",
      "Exp10",
      "Exp2",
      "Float",
      "Int",
      "Ln",
      "Log10",
      "Log2",
      "Mod",
      "Power",
      "RoundDown",
      "RoundUp",
      "Square",
      "SquareRoot",
      "ACos",
      "ACosH",
      "ASin",
      "ASinH",
      "ATan",
      "ATan2",
      "ATanH",
      "Cos",
      "CosH",
      "Sin",
      "SinH",
      "Tan",
      "TanH",
      "Diff",
      "InList",
      "IsNull",
      "Over",
      "Test"
    )    
    PythonInR::pyImport(sa.funs, from = "arcpy.sa")
  }) 
  invisible(NULL)
}
