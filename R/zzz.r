.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Run connect_ArcPython() to connect to Python.")
}

.onDetach <- function(libpath) {
  PythonInR::pyExit()
}
