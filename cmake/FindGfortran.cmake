find_library(GFORTRAN_LIBRARY
  NAMES libgfortran.a gfortran.a gfortran
  HINTS ${LD_LIBRARY_PATH} "c:/MinGW64" /usr
  PATH_SUFFIXES lib local/lib64 x86_64-w64-mingw32/lib lib/gcc/x86_64-w64-mingw32/8.1.0
  DOC "gfortran library"
  NO_DEFAULT_PATH
)

if(GFORTRAN_LIBRARY)
  message("-- adding GFORTRAN_LIBRARY to project")
  add_library(GFORTRAN_LIBRARY UNKNOWN IMPORTED
              GLOBAL
  )
endif()

