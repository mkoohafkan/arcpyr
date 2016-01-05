arcpyr
======

[![Build Status](https://travis-ci.org/mkoohafkan/arcpyr.svg)](https://travis-ci.org/mkoohafkan/arcpyr)

An R interface for `arcpy`. Relies on the virtual Python environment provided by 
`PythonInR` to generate function interfaces for a selection of `arcpy` functions
and objects. The list of functions is growing, and contributing is easy! Look 
at `initialize.r` to see how to add other tools using either `pyImport` or 
custom functions that call `pyExec`, `pyGet`, etc.

In time, I hope ESRI will develop an R library that allows us to interface with
ArcGIS in a similar fashion to Python and `arcpy`, but `arcgisbindings` doesn't
seem to be there yet. Until that happens, `arcpyr` will have to do.
