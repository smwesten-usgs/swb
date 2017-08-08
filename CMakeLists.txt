# CMake project for swb

# This script expects that the following configuration variables are
# set and passed during CMake invocation:

# CMAKE_BINARY_DIR
# DESIRED_BUILD_TYPE     | possible values: "debug", "release", "profile"
# CMAKE_INSTALL_PREFIX   | ex: "D:/DOS"
# Fortran_FLAGS_DEBUG
# Fortran_FLAGS_RELEASE
# Fortran_FLAGS_PROFILE
# PATH_TO_R
# SYSTEM_TYPE                     | possible values: "win_x86", "win_x64", "max_osx", "linux_x64"
# Fortran_COMPILER_NAME  | ex: "gfortran", "ifort"
# COMPILER_VERSION       | ex: "4.8.2"
# LIBRARIES

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
project (swb Fortran C)
enable_language (Fortran C)

SET(CPACK_GENERATOR "TGZ")
include(CPack)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(PACKAGE_BUGREPORT "smwesten@usgs.gov")
set(PACKAGE_NAME "swb")
set(PACKAGE_STRING "swb 1.2 BETA")
set(PACKAGE_TARNAME "swb")
set(PACKAGE_VERSION "1.2 BETA")

cmake_minimum_required(VERSION 2.8)

include( cmake/version_control.cmake )
include( cmake/options_and_targets.cmake )

#------------------------------------------------------------------------
# Locate utility programs
#------------------------------------------------------------------------

set( EXTERNAL_LIBS ${LIB_NETCDF} ${LIB_HDF5_HL} ${LIB_HDF5} )

if (NOT ${LIB_Z} STREQUAL "NOTFOUND")
  set( EXTERNAL_LIBS ${EXTERNAL_LIBS} ${LIB_Z} )
endif()

if (NOT ${LIB_SZ} STREQUAL "NOTFOUND")
  set( EXTERNAL_LIBS ${EXTERNAL_LIBS} ${LIB_SZ} )
endif()

set( EXTERNAL_LIBS ${EXTERNAL_LIBS} ${LIB_DISLIN} )

if (NOT ${LIB_XM} STREQUAL "NOTFOUND" )
  set( EXTERNAL_LIBS ${EXTERNAL_LIBS} ${LIB_XM} )
endif()

if (NOT ${LIB_XT} STREQUAL "NOTFOUND" )
  set( EXTERNAL_LIBS ${EXTERNAL_LIBS} ${LIB_XT} )
endif()

if (NOT ${LIB_OPENGL32} STREQUAL "NOTFOUND")
  set( EXTERNAL_LIBS ${EXTERNAL_LIBS} ${LIB_OPENGL32} )
endif()

if (NOT ${LIB_GDI32} STREQUAL "NOTFOUND")
  set( EXTERNAL_LIBS ${EXTERNAL_LIBS} ${LIB_GDI32} )
endif()

if (NOT ${LIB_CURL} STREQUAL "NOTFOUND")
  set( EXTERNAL_LIBS ${EXTERNAL_LIBS} ${LIB_CURL} )
endif()

list( LENGTH LIB_EXTRAS LEN_LIB_EXTRAS )

if ( "${LEN_LIB_EXTRAS}" GREATER "0" )
  set( EXTERNAL_LIBS ${EXTERNAL_LIBS} ${LIB_EXTRAS} )
endif()

set( EXTERNAL_LIBS ${EXTERNAL_LIBS} ${LIB_GFORTRAN} ${LIB_GCC} )

link_libraries( ${EXTERNAL_LIBS} )
include_directories( ${INCLUDE_DIRECTORIES} ${DISLIN_MODULE_DIR} "${PROJECT_SOURCE_DIR}/src/proj4" )

if ("${SYSTEM_TYPE}" STREQUAL "win_x64" OR "${SYSTEM_TYPE}" STREQUAL "win_x86")
  add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swb.exe ${CMAKE_INSTALL_PREFIX}/swb.exe
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbstats.exe ${CMAKE_INSTALL_PREFIX}/swbstats.exe
  )
else()
  add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swb ${CMAKE_INSTALL_PREFIX}/swb
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbstats ${CMAKE_INSTALL_PREFIX}/swbstats
  )
endif()

include( cmake/print_summary.cmake )

enable_testing()
add_subdirectory(src)
add_subdirectory(tests/general)
add_subdirectory(tests/irrigation)
