# arcpyr

An R interface for the ArcGIS Python module `arcpy`. Relies on the 
virtual Python environment provided by `PythonInR` to generate function 
interfaces for `arcpy` functions and environment settings. 
`arcpy` modules and extensions are accessible through an R environment 
object. See the vignette to get started.

In time, I hope ESRI will develop an R library that allows us to 
interface with ArcGIS in a similar fashion to Python and `arcpy`, but 
`arcgisbindings` doesn't cut it yet. Until that happens, `arcpyr` will 
have to do.

Looking for even more functionality? Check out 
[arcpyrextra](https://github.com/mkoohafkan/arcpyrextra), a 
helper package for doing raster math and working with attribute tables.