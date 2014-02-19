
set(CMAKE_FIND_LIBRARY_PREFIXES "lib")

set(CMAKE_FIND_LIBRARY_SUFFIXES ".a" ".dylib")

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

find_library(LIBZ
        NAMES z libz libz.a
        PATHS ${SWB_LIBPATH}
        ${LIB_PATH}
        NO_CMAKE_SYSTEM_PATH )

find_library(LIBSZ
        NAMES sz libsz libsz.a
        PATHS ${SWB_LIBPATH}
        ${LIB_PATH}
        NO_CMAKE_SYSTEM_PATH )


find_library(LIBNETCDF
        NAMES netcdf libnetcdf libnetcdf.a
        PATHS ${SWB_LIBPATH}
        ${LIB_PATH}
        NO_CMAKE_SYSTEM_PATH )


find_library(LIBHDF5
        NAMES hdf5 libhdf5 libhdf5.a
        PATHS ${SWB_LIBPATH}
        ${LIB_PATH}
        NO_CMAKE_SYSTEM_PATH )


find_library(LIBHDF5_HL
        NAMES hdf5_hl libhdf5_hl libhdf5_hl.a
        PATHS ${SWB_LIBPATH}
        ${LIB_PATH}
        NO_CMAKE_SYSTEM_PATH )

find_library(LIBCURL
        NAMES curl libcurl libcurl.a
        PATHS ${SWB_LIBPATH}
        ${LIB_PATH}
        NO_CMAKE_SYSTEM_PATH )

find_library(LIBDISLIN
        NAMES dismg libdismg libdismg.a dislin.10.4.0 dislin
        PATHS ${SWB_LIBPATH} )

find_library(LIBGCC
        NAMES gcc libgcc libgcc.a
        PATHS ${LIB_PATH} )

find_library(LIBGFORTRAN
        NAMES gfortran libgfortran libgfortran.a
        PATHS ${LIB_PATH} )

set( EXTERNAL_LIBS ${LIBNETCDF} ${LIBHDF5_HL} ${LIBHDF5} ${LIBCURL} ${LIBZ}
                   ${LIBSZ} ${LIBDISLIN} ${LIBGCC} ${LIBGFORTRAN} )

# Now, add platform-specific libraries as needed

if ("${OS}" STREQUAL "win_x64" OR "${OS}" STREQUAL "win_x86")

  find_library(LIBWINPTHREAD  
          NAMES libwinpthread.a winpthread winpthread
          PATHS ${LIB_PATH} )

  find_library(LIBWS2_32
          NAMES ws2_32 libws2_32 libws2_32.a
          PATHS ${LIB_PATH} )
		  
  find_library(LIBOPENGL
          NAMES opengl32 libopengl32 libopengl32.a
          PATHS ${LIB_PATH} )

  find_library(LIBGDI32
          NAMES gdi32 libgdi32 libgdi32.a
          PATHS ${LIB_PATH} )        

  set( EXTERNAL_LIBS ${EXTERNAL_LIBS} ${LIBWINPTHREAD} ${LIBWS2_32} ${LIBOPENGL} ${LIBGDI32} )

else()

  set(CMAKE_FIND_LIBRARY_SUFFIXES ".dylib" "*.a")

  find_library(LIBXM
          NAMES Xm libXm libXm.dylib
          PATHS ${LIB_PATH} )

  set(CMAKE_FIND_LIBRARY_SUFFIXES ".a" ".dylib")        

  set( EXTERNAL_LIBS ${EXTERNAL_LIBS} ${LIBXM} )        

endif()

if ("${OS}" STREQUAL "mac_osx" )


  find_library(LIBCRYPTO
          NAMES crypto libcrypto libcrypto.a
          PATHS ${SWB_LIBPATH}
          ${LIB_PATH} )

  find_library(LIBLDAP  
          NAMES ldap libldap libldap.a
          PATHS ${SWB_PATH} 
          NO_CMAKE_SYSTEM_PATH )

  find_library(LIBSASL2  
          NAMES sasl2 libsasl2 libsasl2.a
          PATHS ${SWB_PATH} 
          NO_CMAKE_SYSTEM_PATH )

  find_library(LIBLBER
          NAMES lber liblber liblber.a
          PATHS ${SWB_PATH}
          ${LIB_PATH}
          NO_CMAKE_SYSTEM_PATH )

  find_library(LIBSSH2  
          NAMES ssh2 libssh2 libssh2.a
          PATHS ${SWB_PATH} 
          ${LIB_PATH})

  find_library(LIBSSL  
          NAMES ssl libssl libssl.a
          PATHS ${SWB_PATH} 
          ${LIB_PATH})

  set( EXTERNAL_LIBS ${EXTERNAL_LIBS} ${LIBLDAP} ${LIBSASL2} ${LIBLBER} ${LIBCRYPTO} 
       ${LIBSSH2} ${LIBSSL})

endif()


link_libraries( ${EXTERNAL_LIBS} )
