find_library(HDF5_LIBRARY
  NAMES libhdf5 hdf5
  PATHS ENV LD_LIBRARY_PATH ENV HDF5_DIR ${LIBRARY_PATH} /usr "d:/MinGW64" "c:/Program Files"
  PATH_SUFFIXES lib lib/x86_64-linux-gnu/ local/lib/ local/lib64 x86_64-w64-mingw32/lib lib/x86_64-linux-gnu/hdf5/serial/ "netCDF"
  DOC "hdf5 library"
  NO_DEFAULT_PATH
)


if(HDF5_LIBRARY)
  add_library(HDF5_LIBRARY UNKNOWN IMPORTED
              GLOBAL
  )
  add_dependencies(HDF5_LIBRARY ZLIB_LIBRARY SZIP_LIBRARY)
endif()
