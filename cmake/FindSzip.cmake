find_library(SZIP_LIBRARY
  NAMES sz.a sz libsz.lib szip.lib
  HINTS ${LD_LIBRARY_PATH} /usr/local/opt/curl /usr "c:/MinGW64" "c:/Program Files"
  PATH_SUFFIXES lib lib/x86_64-linux-gnu/ local/lib/ local/lib64 x86_64-w64-mingw32/lib 
  DOC "szip library"
  NO_DEFAULT_PATH
)

if(SZIP_LIBRARY)
  message("-- adding SZIP_LIBRARY to project")
  add_library(SZIP_LIBRARY UNKNOWN IMPORTED
              GLOBAL
  )
endif()

