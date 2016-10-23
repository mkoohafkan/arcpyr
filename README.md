# arcpyrenv

An R interface for arcpy. Relies on the virtual Python environment 
provided by PythonInR to generate function interfaces for arcpy 
functions and the arcpy environment settings. Functions are accessible
through creation of an R environment to which arcpy modules and 
extensions can be attached.

In time, I hope ESRI will develop an R library that allows us to interface with ArcGIS in a similar fashion to Python and arcpy, but arcgisbindings doesn't seem to be there yet. Until that happens, arcpyr will have to do.