find_library(DISLIN_LIBRARY
  NAMES disifl
  PATHS ENV LD_LIBRARY_PATH ${LIBRARY_PATH} /usr "c:/dislin"
  PATH_SUFFIXES  lib lib64 lib/x86_64-linux-gnu/ local/lib/ local/lib64 x86_64-w64-mingw32/lib "dislin"
  DOC "DISLIN library")

find_file(DISLIN_MODFILE
  NAMES dislin.f90
  PATHS ENV LD_LIBRARY_PATH ${LIBRARY_PATH} /usr "c:/dislin"
  PATH_SUFFIXES  lib lib64 lib/x86_64-linux-gnu/ local/lib/ local/lib64 x86_64-w64-mingw32/lib "ifc"
  DOC "DISLIN module file")

if(DISLIN_LIBRARY)
  add_library(DISLIN_LIBRARY UNKNOWN IMPORTED
             GLOBAL
  )
  #add_dependencies(NETCDF_LIBRARY HDF_LIBRARY HDF5_HL_LIBRARY ZLIB_LIBRARY SZIP_LIBRARY CURL_LIBRARY)

endif()

if(DISLIN_MODFILE)
  add_library(DISLIN_FORTRAN_MOD OBJECT ${DISLIN_MODFILE})
  link_libraries(DISLIN_FORTRAN_MOD)
endif()
