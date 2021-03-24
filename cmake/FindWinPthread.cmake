find_library(WINPTHREAD_LIBRARY
  NAMES libwinpthread.a winpthread.a winpthread
  HINTS ${LD_LIBRARY_PATH} "c:/MinGW64"
  PATH_SUFFIXES lib local/lib64 x86_64-w64-mingw32/lib 
  DOC "winpthread library"
  NO_DEFAULT_PATH
)

if(WINPTHREAD_LIBRARY)
  message("-- adding WINPTHREAD_LIBRARY to project")
  add_library(WINPTHREAD_LIBRARY UNKNOWN IMPORTED
              GLOBAL
  )
endif()

