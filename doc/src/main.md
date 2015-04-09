@mainpage SWB--A Modified Thornthwaite-Mather Soil-Water-Balance code for estimating groundwater recharge 

The SWB model calculates recharge by use of commonly available geographic information system
(GIS) data layers in combination with tabular climatological data. The code is based on a modified Thornthwaite-Mather
soil-water-balance approach, with components of the soil water balance calculated at a daily timestep. Recharge
calculations are made on a rectangular grid of computational elements that may be easily imported into a regional groundwater-
flow model. Recharge estimates calculated by the code may be output as daily, monthly, or annual values.

The code is written in modern Fortran (Fortran 95/2003/2008), and has been compiled on Windows, Macintosh, and Linux systems using the gfortran, g95, and Intel fortran compilers.

Optional support is provided for:
 - Reading and writing NetCDF files
 - Producing simple plots of model inputs and outputs
 - Estimating irrigation amounts required to sustain plant growth

External libraries are required if NetCDF file access or plotting capabilities are desired. The fortran modules documented here
must be linked against the NetCDF and DISLIN libraries:
 - NetCDF: http://www.unidata.ucar.edu/software/netcdf/
 - DISLIN: http://www.mps.mpg.de/dislin/
@section Documentation
 Westenbroek, S.M., Kelson, V.A., Dripps, W.R., Hunt, R.J., and Bradbury, K.R., 2010, SWB-A modified Thornthwaite-Mather Soil-Water-Balance code for estimating groundwater recharge: U.S. Geological Survey Techniques and Methods 6-A31, 60 p.

http://pubs.usgs.gov/tm/tm6-a31/
