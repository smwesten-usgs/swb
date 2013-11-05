# CMake project for swb

# This script expects that the following configuration variables are
# set and passed during CMake invocation:

# CMAKE_BINARY_DIR
# CMAKE_BUILD_TYPE       | possible values: "debug", "release", "profile"
# CMAKE_INSTALL_PREFIX   | ex: "D:/DOS"
# Fortran_FLAGS_DEBUG
# Fortran_FLAGS_RELEASE
# Fortran_FLAGS_PROFILE 
# PATH_TO_R
# OS                     | possible values: "win_x86", "win_x64", "max_osx", "linux"
# Fortran_COMPILER_NAME  | ex: "gfortran", "ifort"
# COMPILER_TRIPLET       | ex: "x86_64-w64-mingw32", "x86_64-apple-darwin12.4.0"
# COMPILER_VERSION       | ex: "4.8.0"


message("Processing top-level CMakelists.txt for project swb")

#------------------------------------------------------------------------------------#
#                           Build directory check                                    #
#------------------------------------------------------------------------------------#
if(${CMAKE_SOURCE_DIR}/src STREQUAL ${CMAKE_BINARY_DIR})
  message(FATAL_ERROR "Cannot be built in the source directory. Use
  out-of-source build instead.")
  message(FATAL_ERROR "  cd /some/work/dir/build")
  message(FATAL_ERROR "  cmake -i /path/to/source")
endif()

#------------------------------------------------------------------------
# Set basic project settings
#------------------------------------------------------------------------
#
# NOTE: must enable CXX as a language in order to use the GNU compiler
#       toolchain
#
project (swb Fortran C CXX)
enable_language (Fortran C CXX)

SET(CPACK_GENERATOR "TGZ")
include(CPack)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(PACKAGE_BUGREPORT "smwesten@usgs.gov")
set(PACKAGE_NAME "swb")
set(PACKAGE_STRING "swb 1.2 BETA")
set(PACKAGE_TARNAME "swb")
set(PACKAGE_VERSION "1.2 BETA")

cmake_minimum_required(VERSION 2.8)

import(cmake/options_and_targets.cmake)

#------------------------------------------------------------------------
# Locate utility programs
#------------------------------------------------------------------------



find_program( R_SCRIPT Rscript.exe Rscript
    HINTS
    ENV R_HOME
	${PATH_TO_R}
    PATHS
    "c:/Program Files/R"
    "c:/Program Files/R/R-3.0.1/bin"
    "/usr/bin"
)


set(LIB_PATH ${PROJECT_SOURCE_DIR}/lib/${OS}/${Fortran_COMPILER_NAME})

#------------------------------------------------------------------------
# Find all STATIC library files in the appropriate subdirectory beneath
#    the swb/lib dir structure; add to list of libraries to link against
#------------------------------------------------------------------------

FILE(GLOB staticLibFiles RELATIVE "${LIB_PATH}" "${LIB_PATH}/*.a")
#FILE(GLOB staticLibFiles "${LIB_PATH}/*.a")

FOREACH(staticLibFile ${staticLibFiles})
    MESSAGE(STATUS "==> Adding library to build: ${staticLibFile}")

  set(EXTRA_LIBS ${EXTRA_LIBS} ${staticLibFile} )

ENDFOREACH()

#------------------------------------------------------------------------
# Find all DYNAMIC library files in the appropriate subdirectory beneath
#    the swb/lib dir structure; add to list of libraries to link against
#------------------------------------------------------------------------

FILE(GLOB dynamicLibFiles "${LIB_PATH}/*.dylib")
#FILE(GLOB dynamicLibFiles RELATIVE "${LIB_PATH}" "${LIB_PATH}/*.dylib")

FOREACH(dynamicLibFile ${dynamicLibFiles})
    MESSAGE(STATUS "==> Adding shared library to build: ${dynamicLibFile}")

  set(EXTRA_LIBS ${EXTRA_LIBS} ${dynamicLibFile} )

ENDFOREACH()


#------------------------------------------------------------------------
# Add standard compiler libraries to link against
#------------------------------------------------------------------------

set(GCC_LIB ${COMPILER_DIR}/lib/gcc/${COMPILER_TRIPLET}/${COMPILER_VERSION}/libgcc.a)
set(GFORTRAN_LIB ${COMPILER_DIR}/lib/libgfortran.a)
set(EXTRA_LIBS ${EXTRA_LIBS} ${GCC_LIB} )
set(EXTRA_LIBS ${EXTRA_LIBS} ${GFORTRAN_LIB} )

set(MODULE_DIR ${INCLUDE_DIR} ${PROJECT_SOURCE_DIR}/include/${OS}/${Fortran_COMPILER_NAME} )

include_directories( ${MODULE_DIR} ${EXTRA_INCLUDES} "${PROJECT_SOURCE_DIR}/src/proj4")

if (${OS} STREQUAL "win_x64" OR ${OS} STREQUAL "win_x86")
  add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swb.exe ${CMAKE_INSTALL_PREFIX}/swb.exe
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbstats.exe ${CMAKE_INSTALL_PREFIX}/swbstats.exe
  )
else()  
  add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swb ${CMAKE_INSTALL_PREFIX}/swb
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbstats ${CMAKE_INSTALL_PREFIX}/swbstats
  )
endif()

include_directories(${INCLUDE_DIR} ${INCLUDE_DISLIN})
link_libraries( ${EXTRA_LIBS} )

get_filename_component(COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

include( cmake/print_summary.cmake )

enable_testing()
add_subdirectory(src)
add_subdirectory(tests/general)
add_subdirectory(tests/irrigation)
