# Soil-Water-Balance Model: control file settings for specific climate datasets {#specific_datasets}

# Gridded Meteorological Observations 1949-2010 (Ed Mauer, 1/8-degree spatial resolution) #

http://cida.usgs.gov/thredds/catalog.html?dataset=cida.usgs.gov/new_gmo

~~~~~~~~~~~~~~~
PRECIPITATION NETCDF http://cida.usgs.gov/thredds/dodsC/new_gmo
PRECIPITATION_GRID_PROJECTION_DEFINITION +proj=lonlat +ellps=GRS80 +datum=WGS84 +no_defs
## convert precipitation in mm/day to inches per day
PRECIPITATION_CONVERSION_FACTOR 0.03936996
PRECIPITATION_MISSING_VALUES_CODE 1.0E+15
PRECIPITATION_MISSING_VALUES_OPERATOR >=
PRECIPITATION_MISSING_VALUES_ACTION MEAN
NETCDF_PRECIP_X_VAR longitude
NETCDF_PRECIP_Y_VAR latitude
NETCDF_PRECIP_Z_VAR pr
NETCDF_PRECIP_TIME_VAR time

TEMPERATURE NETCDF http://cida.usgs.gov/thredds/dodsC/new_gmo http://cida.usgs.gov/thredds/dodsC/new_gmo

TMAX_GRID_PROJECTION_DEFINITION +proj=lonlat +ellps=GRS80 +datum=WGS84 +no_defs
TMAX_MISSING_VALUES_CODE 1.0E+15
TMAX_MISSING_VALUES_OPERATOR >=
TMAX_MISSING_VALUES_ACTION MEAN
NETCDF_TMAX_X_VAR longitude
NETCDF_TMAX_Y_VAR latitude
NETCDF_TMAX_Z_VAR tasmax
NETCDF_TMAX_TIME_VAR time
## convert temperature in degrees C to degrees F
TMAX_SCALE_FACTOR 1.8
TMAX_ADD_OFFSET 32

TMIN_GRID_PROJECTION_DEFINITION +proj=lonlat +ellps=GRS80 +datum=WGS84 +no_defs
TMIN_MISSING_VALUES_CODE 1.0E+15
TMIN_MISSING_VALUES_OPERATOR >=
TMIN_MISSING_VALUES_ACTION MEAN
NETCDF_TMIN_X_VAR longitude
NETCDF_TMIN_Y_VAR latitude
NETCDF_TMIN_Z_VAR tasmax
NETCDF_TMIN_TIME_VAR time
## convert temperature in degrees C to degrees F
TMIN_SCALE_FACTOR 1.8
TMIN_ADD_OFFSET 32
~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~
#PRECIPITATION NETCDF ..\COMMON_CLIMATE\%Y-%0#_prcp.nc

PRECIPITATION_GRID_PROJECTION_DEFINITION +proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs
PRECIPITATION_CONVERSION_FACTOR 0.03936996
PRECIPITATION_MISSING_VALUES_CODE -32768
PRECIPITATION_MISSING_VALUES_OPERATOR <=
PRECIPITATION_MISSING_VALUES_ACTION ZERO
NETCDF_PRECIP_MAKE_LOCAL_ARCHIVE

TEMPERATURE NETCDF http://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/daymet http://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/daymet
#TEMPERATURE NETCDF dods://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/daymet dods://cida-eros-mows1.er.usgs.gov:8080/thredds/dodsC/daymet
TMAX_GRID_PROJECTION_DEFINITION +proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs
TMAX_MISSING_VALUES_CODE -128
TMAX_MISSING_VALUES_OPERATOR <=
TMAX_MISSING_VALUES_ACTION MEAN
TMAX_SCALE_FACTOR 1.8
TMAX_ADD_OFFSET 32
NETCDF_TMAX_MAKE_LOCAL_ARCHIVE

TMIN_GRID_PROJECTION_DEFINITION +proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs
TMIN_MISSING_VALUES_CODE -128
TMIN_MISSING_VALUES_OPERATOR <=
TMIN_MISSING_VALUES_ACTION MEAN
TMIN_SCALE_FACTOR 1.8
TMIN_ADD_OFFSET 32
NETCDF_TMIN_MAKE_LOCAL_ARCHIVE
~~~~~~~~~~~~~~~