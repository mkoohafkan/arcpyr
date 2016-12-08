.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Run connect_ArcGIS() to connect to ArcGIS.")
}

.onDetach <- function(libpath) {
  PythonInR::pyExit()
}
