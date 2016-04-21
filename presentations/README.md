# NOTE regarding the presentations in this directory:

The presentations stored here contain some useful information, however, some material is outdated with respect to the current SWB code. Keep the following in mind when viewing these files:

* The most recent code can use raster data inputs whose projection, resolution, and coverage vary from one another.
* Although it is technically possible to specify a THREDDS server when reading from a netCDF file, performance is slow owing to network use. If you are going to make multiple SWB runs, it probably make sense to download a local copy of the netCDF file.
* One example shows the creation of climate data rasters by using an R-script to interpolate raw surface observations obtained from NOAA. Although this is still possible, consider using one of the readily available climate datasets instead:
    * DayMet: https://daymet.ornl.gov/
    * PRISM: http://prism.oregonstate.edu/
    
  DayMet data may be obtained as a mosaic, which is the most convenient form if SWB is to be applied to a large area. With the proper control file specifications, DayMet data may be used with SWB with no further modification of the DayMet data. PRISM data looks interesting, however, it is supplied in the form of image files, with one image file per day. To use with SWB, the images would need to be converted to ASCII grid files. The command-line tools gdal_warp and gdal_transform ( http://www.gdal.org/ ) could be used in a script to perform this conversion relatively painlessly.
    
