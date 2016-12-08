.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Run connect_ArcPython() to connect to ArcGIS.")
}

.onDetach <- function(libpath) {
  PythonInR::pyExit()
}
