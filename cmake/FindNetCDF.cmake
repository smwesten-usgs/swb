find_library(NETCDF_LIBRARY
  NAMES netcdf libnetcdf
  PATHS ENV LD_LIBRARY_PATH ENV NETCDF_DIR ${LIBRARY_PATH} /usr "c:/MinGW64" "c:/Program Files"
  PATH_SUFFIXES  lib lib64 lib/x86_64-linux-gnu/ local/lib/ local/lib64 x86_64-w64-mingw32/lib "NetCDF"
  DOC "netcdf library")

if(NETCDF_LIBRARY)
  add_library(NETCDF_LIBRARY UNKNOWN IMPORTED
             GLOBAL
  )
  add_dependencies(NETCDF_LIBRARY HDF_LIBRARY HDF5_HL_LIBRARY ZLIB_LIBRARY SZIP_LIBRARY CURL_LIBRARY)
endif()
