
set(CMAKE_FIND_LIBRARY_PREFIXES "lib")

find_program( R_SCRIPT Rscript.exe Rscript
    HINTS
    ENV R_HOME
  ${PATH_TO_R}
    PATHS
    "c:/Program Files/R"
    "c:/Program Files/R/R-3.0.1/bin"
    "/usr/bin"
)

if ("${OS}" STREQUAL "win_x64" OR "${OS}" STREQUAL "win_x86")

  set(CMAKE_FIND_LIBRARY_SUFFIXES ".a" )
  set( SWB_EXECUTABLE ${CMAKE_INSTALL_PREFIX}/swb.exe )

elseif( "${OS}" STREQUAL "mac" OR "${OS}" STREQUAL "mac_osx" )

  set(CMAKE_FIND_LIBRARY_SUFFIXES ".dylib" ".a" )
  set( SWB_EXECUTABLE ${CMAKE_INSTALL_PREFIX}/swb )
  set ( EXTRA_INCLUDEDIR "/usr/local/dislin/gf")

else()

  set(CMAKE_FIND_LIBRARY_SUFFIXES ".a" ".so" )
  set ( EXTRA_INCLUDEDIR "/usr/local/dislin/gf")
  
endif()

################################################################
## NOTE: CMAKE_FIND_LIBRARY_SUFFIXES works...
##       CMAKE_FILE_LIBRARY_SUFFIX does not
##       PATH doesn not work
##       PATHS works
##
##  in main CMakeLists.txt the following works:
##
##    set(LIB_PATH ${COMPILER_DIR}/lib/gcc/${COMPILER_TRIPLET}/${COMPILER_VERSION} )
##
##  the following does not (note double quotes):
##
##    set(LIB_PATH "${COMPILER_DIR}/lib/gcc/${COMPILER_TRIPLET}/${COMPILER_VERSION}" )
##
################################################################

message("MOD: LIB_PATH = ${LIB_PATH}")

find_library(LIBZ
        NAMES z libz libz.a
        PATHS
        /usr/local/opt/zlib/lib
        ${LIBZ_PATH}
        ${SWB_LIBPATH}
        ${LIB_PATH}
        NO_SYSTEM_ENVIRONMENT_PATH
        NO_CMAKE_SYSTEM_PATH )

find_library(LIBNETCDF
        NAMES netcdf libnetcdf libnetcdf.a
        PATHS 
        /usr/local/lib64
        ${SWB_LIBPATH}
        ${LIBNETCDF_PATH}
        ${LIB_PATH}
        NO_CMAKE_SYSTEM_PATH )


find_library(LIBHDF5
        NAMES hdf5 libhdf5 libhdf5.a
        ${LIBHDF5_PATH}
        PATHS 
        /usr/local/lib64
        ${SWB_LIBPATH}
        ${LIB_PATH}
        NO_CMAKE_SYSTEM_PATH )


find_library(LIBHDF5_HL
        NAMES hdf5_hl libhdf5_hl libhdf5_hl.a
        PATHS 
        /usr/local/lib64
        ${LIB_PATH}
        NO_CMAKE_SYSTEM_PATH )

find_library(LIBCURL
        NAMES curl libcurl libcurl.so libcurl.dylib
        PATHS 
        /usr/lib64
        /usr/local/Cellar/curl/7.47.1/lib
        ${LIBCURL_PATH}
        ${LIB_PATH}
        NO_SYSTEM_ENVIRONMENT_PATH
        NO_CMAKE_SYSTEM_PATH )

find_library(LIBDISLIN
        NAMES
        dismg libdismg libdismg.a disgf libdisgf libdisgf.a dislin.10 dislin dislin.10.5.0.dylib
        PATHS
        /usr/local/lib
        /usr/local/dislin/lib
        /usr/local/dislin
        ${SWB_LIBPATH} )

find_library(LIBGCC
        NAMES gcc libgcc libgcc.a
        PATHS 
        /usr/local/lib64
        ${LIBGCC_PATH}
        /usr/local/Cellar/gcc5/5.3.0/lib/gcc/5/gcc/x86_64-apple-darwin15.3.0/5.3.0
        ${LIB_PATH} )

find_library(LIBGFORTRAN
        NAMES gfortran libgfortran libgfortran.a
        PATHS
        /usr/local/lib64
        ${LIBGCC_PATH}
        /usr/local/Cellar/gcc5/5.3.0/lib/gcc/5
        ${LIB_PATH} )

set( EXTERNAL_LIBS ${LIBNETCDF} ${LIBHDF5_HL} ${LIBHDF5} ${LIBCURL} ${LIBZ}
                   ${LIBDISLIN} ${LIBGCC} ${LIBGFORTRAN} )

# Now, add platform-specific libraries as needed

if ("${OS}" STREQUAL "win_x64" OR "${OS}" STREQUAL "win_x86")

#  find_library(LIBWINPTHREAD
#          NAMES libwinpthread.a winpthread winpthread
#          PATHS ${LIB_PATH} )


  find_library(LIBSZ
          NAMES sz libsz libsz.a
          PATHS
          ${LIBSZ_PATH}
          ${SWB_LIBPATH}
          ${LIB_PATH}
          NO_SYSTEM_ENVIRONMENT_PATH
          NO_CMAKE_SYSTEM_PATH )

  find_library(LIBWS2_32
          NAMES ws2_32 libws2_32 libws2_32.a
          PATHS ${LIB_PATH} )

  find_library(LIBOPENGL
          NAMES opengl32 libopengl32 libopengl32.a
          PATHS ${LIB_PATH} )

  find_library(LIBGDI32
          NAMES gdi32 libgdi32 libgdi32.a
          PATHS ${LIB_PATH} )

  set( EXTERNAL_LIBS ${EXTERNAL_LIBS} ${LIBSZ} ${LIBWINPTHREAD} ${LIBWS2_32} ${LIBOPENGL} ${LIBGDI32} )

elseif ("${OS}" STREQUAL "mac_osx" )

  set(CMAKE_FIND_LIBRARY_SUFFIXES ".dylib" "*.a")

  find_library(LIBXM
          NAMES Xm libXm libXm.dylib
          PATHS ${LIB_PATH} )

  set(CMAKE_FIND_LIBRARY_SUFFIXES ".a" ".dylib")

  set( EXTERNAL_LIBS ${EXTERNAL_LIBS} ${LIBXM} )

  find_library(LIBCRYPTO
          NAMES crypto libcrypto libcrypto.a
          PATHS ${SWB_LIBPATH}
          ${LIB_PATH} )

  find_library(LIBLDAP
          NAMES ldap libldap libldap.dylib
          PATHS /usr/local/opt/openldap/lib
          NO_CMAKE_SYSTEM_PATH )

  find_library(LIBSASL2
          NAMES gsasl libgsasl sasl2 libsasl2 libsasl2.dylib
          PATHS /usr/local/opt/gsasl/lib
#          ${SWB_PATH}
          NO_CMAKE_SYSTEM_PATH )

  find_library(LIBLBER
          NAMES lber liblber liblber.dylib
#          PATHS ${SWB_PATH}
          ${LIB_PATH}
          NO_CMAKE_SYSTEM_PATH )

  find_library(LIBSSH2
          NAMES ssh2 libssh2 libssh2.dylib
          PATHS
          /usr/local/opt/libssh2/lib
#          ${SWB_PATH}
          ${LIB_PATH})

  find_library(LIBSSL
          NAMES ssl libssl libssl.dylib
          PATHS
          /usr/local/opt/openssl/lib
#          ${SWB_PATH}
          ${LIB_PATH} )

  set( EXTERNAL_LIBS ${EXTERNAL_LIBS}  ${LIBLDAP} ${LIBCRYPTO} ${LIBSSL} ${LIBLBER}
        ${LIBSSH2} ${LIBSASL2} )

else()

  set(CMAKE_FIND_LIBRARY_SUFFIXES ".dylib" ".so" ".a" )

  find_library(LIBXT
          NAMES Xt libXt libXt.dylib libXt.so
          PATHS
          /usr/lib
          /usr/lib64
          ${LIB_PATH} )

  find_library(LIBXEXT
          NAMES Xext libXext
          PATHS
          /usr/lib
          /usr/lib64
          ${LIB_PATH} )

  find_library(LIBX11
          NAMES X11 libX11
          PATHS
          /usr/lib
          /usr/lib64
          ${LIB_PATH} )

  find_library(LIBXM
          NAMES Xm libXm
          PATHS
          /usr/lib
          /usr/lib64
          ${LIB_PATH} )

  find_library(LIBGL
          NAMES GL libGL
          PATHS
          /usr/lib
          /usr/lib64
          ${LIB_PATH} )
  
  find_library(LIBXFT
          NAMES Xft libXft
          PATHS
          /usr/lib
          /usr/lib64
          ${LIB_PATH} )

  find_library(LIBCRYPTO
          NAMES crypto libcrypto
          PATHS
          /usr/lib
          /usr/lib64
          ${LIB_PATH} )

 find_library(LIBSSH2
          NAMES ssh2 libssh2 libssh2.dylib libssh2.so.1
          PATHS
          /usr/lib
          /usr/lib64
          ${LIB_PATH})

  find_library(LIBSSL
          NAMES ssl libssl libssl.dylib
          PATHS
          /usr/lib
          /usr/lib64
          ${LIB_PATH} )

  set( EXTERNAL_LIBS ${EXTERNAL_LIBS} ${LIBCRYPTO} ${LIBGL} ${LIBXM} ${LIBXT} ${LIBXFT} ${LIBXEXT} ${LIBX11} ${LIBSSH2} ${LIBSSL} )

endif()


link_libraries( ${EXTERNAL_LIBS} )
include_directories( ${EXTRA_INCLUDEDIR} ${SWB_INCPATH} "${PROJECT_SOURCE_DIR}/src/proj4")