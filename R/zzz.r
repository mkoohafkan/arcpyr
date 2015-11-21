.onAttach <- function(libname, pkgname){ 
   packageStartupMessage("Run arcpy.initialize() to connect to arcpy.") 
}

.onDetach <- function(libpath){
  PythonInR::pyExit()
}
