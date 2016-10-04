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
#'     \item SpatialJoin_analysis
#'     \item Union_analysis
#'     \item Identity_analysis
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
#'     \item CopyFeatures_management
#'     \item AddField_management
#'     \item CalculateField_management
#'     \item DeleteField_management
#'     \item FeatureToPoint_management
#'     \item Merge_management
#'     \item Append_management
#'     \item CreateFileGDB_management
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
#' # using the ArcGIS Desktop 10.x Python environment
#' arcpy.initialize("C:/Python27/ArcGIS10.x/python.exe")
#' env.workspace("C:/test.gdb")
#' env.overwriteOutput(TRUE)
#' Clip_analysis("feature1", "feature2")
#'
#' # using the ArcGIS Pro (>= 1.3) Python environment
#' envpath = "C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3"
#' PythonInR:::pyConnectWinDll(
#'   dllName = "python34.dll", 
#'   pyArch = "64bit", 
#'   majorVersion = 3, 
#'   dllDir = envpath, 
#'   pythonHome = envpath
#' )
#' arcpy.initialize(quietly = TRUE)
#' }
#'
#' @export
arcpy.initialize <- function(PYTHON_EXE, quietly = FALSE){
  oldwarn = options("warn")$warn
  options(warn = 1)
  on.exit(options(warn = oldwarn))
  if(PythonInR::pyIsConnected()){
    if(!quietly)
    warning("R is already connected Python.")
  } else {
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
    if(length(PYTHON_EXE) > 1)
      stop("Multiple python installations dectected. Please manually specify the Python path.")
    if(length(PYTHON_EXE) < 1)
      stop("Could not find python.exe")
    if(!file.exists(PYTHON_EXE))
      stop("Could not find python.exe")
    if(quietly) 
      suppressPackageStartupMessages(PythonInR::pyConnect(PYTHON_EXE))
    else 
      PythonInR::pyConnect(PYTHON_EXE)
  }
  flush.console()
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
    # listing
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
    # analysis
    analysis.funs = c(
      "Clip_analysis", 
      "Select_analysis",
      "Intersect_analysis",
      "SpatialJoin_analysis",
      "Union_analysis",
      "Identity_analysis"
    )
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
      "AlterField_management",
      "CopyFeatures_management",
      "AddField_management",
      "CalculateField_management",
      "DeleteField_management",
      "FeatureToPoint_management",
      "Merge_management",
      "Append_management",
      "CreateFileGDB_management"
    )
    # conversion
    conversion.funs = c(
      "RasterToPolygon_conversion",
      "TableToTable_conversion"
    )
    # import funs and catch errors
    all.funs = c(listing.funs, analysis.funs, management.funs, 
      conversion.funs)
    errlist = setNames(rep(FALSE, length(all.funs)), all.funs)
    for(fun in all.funs){
      res = tail(capture.output(class(try(PythonInR::pyImport(fun, 
        from = "arcpy"), silent = TRUE))), 1)
      if(grepl("try-error", res))
        errlist[[fun]] = TRUE
    }
    names(errlist) = NULL
    if(any(errlist))
      warning("The following arcpy functions could not be imported: ", 
      paste(all.funs[errlist], collapse = ", "), call. = FALSE)
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
#' inrasts = list(wse = "scratch.gdb/wse", elev = "scratch.gdb/wse")
#' outrast = list(depth = "scratch.gdb/depth")
#' expr = list("depth = wse - elev")
#' sa.initialize()
#' RasterCalculator(expr, inrasts, outrast)
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
      "ZonalStatisticsAsTable",
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
    # import funs and catch errors
    errlist = setNames(rep(FALSE, length(sa.funs)), sa.funs)
    for(fun in sa.funs){
      res = tail(capture.output(class(try(PythonInR::pyImport(fun, 
        from = "arcpy.sa"), silent = TRUE))), 1)
      if(grepl("try-error", res))
        errlist[[fun]] = TRUE
    }
    names(errlist) = NULL
    if(any(errlist))
      warning("The following arcpy.sa functions could not be imported: ", 
      paste(sa.funs[errlist], collapse = ", "), call. = FALSE)
  }) 
  invisible(NULL)
}
