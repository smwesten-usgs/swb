
set(CMAKE_FIND_LIBRARY_PREFIXES "lib")
set(CMAKE_FIND_LIBRARY_SUFFIXES ".a")

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
        PATHS ${SWB_LIBPATH} )
  
find_library(LIBSZ
        NAMES sz libsz libsz.a
        PATHS ${SWB_LIBPATH} )

find_library(LIBNETCDF
        NAMES netcdf libnetcdf libnetcdf.a
        PATHS ${SWB_LIBPATH} )
		
find_library(LIBHDF5
        NAMES hdf libhdf5 libhdf5.a
        PATHS ${SWB_LIBPATH} )
		
find_library(LIBHDF5_HL
        NAMES hdf5_hl libhdf5_hl libhdf5_hl.a
        PATHS ${SWB_LIBPATH} )

find_library(LIBCURL
        NAMES curl libcurl libcurl.a
        PATHS ${SWB_LIBPATH} )
		
find_library(LIBDISLIN
        NAMES dismg libdismg libdismg.a
        PATHS ${SWB_LIBPATH} )
		
find_library(LIBGCC
        NAMES gcc libgcc libgcc.a
        PATHS ${LIB_PATH} )		

find_library(LIBGFORTRAN
        NAMES gfortran libgfortran libgfortran.a
        PATHS ${LIB_PATH} )	
		
find_library(LIBOPENGL
        NAMES opengl32 libopengl32 libopengl32.a
		PATHS ${LIB_PATH} )
		
find_library(LIBGDI32
        NAMES gdi32 libgdi32 libgdi32.a
		PATHS ${LIB_PATH} )
		
#        PATHS "C:/MinGW64/x86_64-w64-mingw32"
#		PATH_SUFFIXES "lib" )
		
if (WIN32) 
  find_library(LIBWS2_32
          NAMES ws2_32 libws2_32 libws2_32.a
          PATHS ${LIB_PATH} )
		  
endif()
		
set( EXTERNAL_LIBS ${LIBNETCDF} ${LIBHDF5_HL} ${LIBHDF5} ${LIBCURL} ${LIBZ} 
                   ${LIBSZ} ${LIBDISLIN} ${LIBGCC} ${LIBGFORTRAN} ${LIBWS2_32}
				   ${LIBOPENGL} ${LIBGDI32})

link_libraries( ${EXTERNAL_LIBS} )
