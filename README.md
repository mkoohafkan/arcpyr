# DEPRECATION NOTICE

This package, and the companion package 
[arcpyrextra](https://github.com/mkoohafkan/arcpyrextra), is now
deprecated. Development has transitioned to a new package 
[arcpy](https://github.com/mkoohafkan/arcpy), 
which uses 
[reticulate](https://cran.r-project.org/package=reticulate) 
for the Python-R
bridge. `arcpy` supports all the functionality of `arcpyr`
without the need for the additional wrapper functions that
made up the bulk of the `arcpyr` code base, instead
producing an `arcpy` object that provides a seamless interface
arcpy functions and classes. The helper functions `da_read`,
`da_update` and `da_insert` have also been ported over from 
`arcpyrextra`, and S3 methods were added to `arcpy` to replace
the raster math tools from `arcpyrextra`.

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