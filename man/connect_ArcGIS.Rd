% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildenv.r
\name{connect_ArcGIS}
\alias{connect_ArcGIS}
\title{Connect ArcGIS Python}
\usage{
connect_ArcGIS(python_folder, Pro = FALSE, quietly = TRUE)
}
\arguments{
\item{python_folder}{The Python installation folder created by 
ArcGIS. If missing, the function will attempt to automatically 
detect an appropriate Python installation.}

\item{Pro}{Logical: If True, attempt to connect to an ArcGIS Pro 
Python distribution. Otherwise, attempt to connect to an ArcGIS 
Desktop Python distribution.}

\item{quietly}{Logical: Suppress messages and warnings.}
}
\value{
No return value.
}
\description{
Connect to the ArcGIS Python environment.
}
\examples{
\dontrun{
# Try to autodetect
connect_ArcGIS()

# connect to the 32-bit ArcGIS Desktop 10.2 Python environment
connect_ArcGIS("C:/Python27/ArcGIS10.2")

# connect to the 64-bit ArcGIS Desktop 10.2 Python environment
connect_ArcGIS("C:/Python27/ArcGISx6410.2")

# connect to the ArcGIS Pro 1.3 Python environment
connect_ArcGIS("C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3",
  Pro = TRUE)
}

}
\seealso{
\code{\link{arcpy_env}}
}
