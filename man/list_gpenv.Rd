% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildenv.r
\name{list_gpenv}
\alias{list_gpenv}
\title{List Geoprocessing Environments}
\usage{
list_gpenv(wc = "*")
}
\arguments{
\item{wc}{A wild card (regular expression) to filter the results.}
}
\value{
A list of geoprocessing environment names.
}
\description{
List ArcGIS geoprocessing environment settings, i.e. elements of 
  \code{arcpy.env}.
}
