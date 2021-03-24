find_library(CURL_LIBRARY
  NAMES curl.a curl libcurl.lib curl.lib
  HINTS ${LD_LIBRARY_PATH} /usr/local/opt/curl /usr "c:/MinGW64" "c:/Program Files"
  PATH_SUFFIXES lib lib/x86_64-linux-gnu/ local/lib/ local/lib64 x86_64-w64-mingw32/lib 
  DOC "curl library"
  NO_DEFAULT_PATH
)

if(CURL_LIBRARY)
  message("-- adding CURL_LIBRARY to project")
  add_library(CURL_LIBRARY UNKNOWN IMPORTED
              GLOBAL
  )
endif()

