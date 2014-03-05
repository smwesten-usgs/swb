
set(CMAKE_FIND_LIBRARY_PREFIXES "lib")

set(CMAKE_FIND_LIBRARY_SUFFIXES ".dylib" ".a")

find_program( R_SCRIPT Rscript.exe Rscript
    HINTS
    ENV R_HOME
	${PATH_TO_R}
    PATHS
    "c:/Program Files/R"
    "c:/Program Files/R/R-3.0.1/bin"
    "/usr/bin"
)

include_directories( ${SWB_INCPATH} "${PROJECT_SOURCE_DIR}/src/proj4")

if ("${OS}" STREQUAL "win_x64" OR "${OS}" STREQUAL "win_x86")

  set( SWB_EXECUTABLE ${CMAKE_INSTALL_PREFIX}/swb.exe )

else()

  set( SWB_EXECUTABLE ${CMAKE_INSTALL_PREFIX}/swb )

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


# Now, add prepared static libraries as needed
file(GLOB files "${SWB_LIBPATH}/*.a")

foreach( file ${files})
  set( EXTERNAL_LIBS ${EXTERNAL_LIBS} ${file} )
  message("  ==>> adding ${file} to list of libraries to link against")
endforeach()

# examine system for gcc and gfortran libraries
find_library(LIBGCC
        NAMES gcc libgcc libgcc.a
        PATHS /usr/local/opt/gcc48/lib/gcc/x86_64-apple-darwin13.0.0/4.8.2
        ${LIB_PATH} )

find_library(LIBGFORTRAN
        NAMES gfortran libgfortran libgfortran.a
        PATHS ${LIB_PATH} )

set( EXTERNAL_LIBS ${EXTERNAL_LIBS} ${LIBGCC} ${LIBGFORTRAN} )

# on Macintosh, "dylibs" are the rule; Apple apparently discourages
# static linking of libraries.

if ("${OS}" STREQUAL "mac_osx" )

  find_library(LIBDISLIN
        NAMES dislin.10 dislin dislin.10.dylib
        PATHS
        /usr/local/lib
        ${SWB_LIBPATH} )


  find_library(LIBXM
          NAMES Xm libXm libXm.dylib
          PATHS ${LIB_PATH} )

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

endif()


link_libraries( ${EXTERNAL_LIBS} )
