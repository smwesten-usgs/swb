find_library(HDF5_HL_LIBRARY
  NAMES hdf5_hl libhdf5_hl libhdf5_serial_hl
  PATHS ENV LD_LIBRARY_PATH ENV HDF5_DIR ${LIBRARY_PATH} /usr "d:/MinGW64" "c:/Program Files"
  PATH_SUFFIXES lib lib/x86_64-linux-gnu/ local/lib/ local/lib64 x86_64-w64-mingw32/lib lib/x86_64-linux-gnu/hdf5/serial/ "netCDF"
  DOC "hdf5_hl library")

if(HDF5_HL_LIBRARY)
  add_library(HDF5_HL_LIBRARY UNKNOWN IMPORTED
              GLOBAL
  )
  add_dependencies(HDF5_HL_LIBRARY HDF5_LIBRARY ZLIB_LIBRARY)
endif()
