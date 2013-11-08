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

include( cmake/options_and_targets.cmake )

#------------------------------------------------------------------------
# Locate utility programs
#------------------------------------------------------------------------

set(SWB_LIBPATH ${PROJECT_SOURCE_DIR}/lib/${OS}/${Fortran_COMPILER_NAME} )
set(SWB_INCPATH ${PROJECT_SOURCE_DIR}/include/${OS}/${Fortran_COMPILER_NAME} )
set(LIB_PATH "${COMPILER_DIR}/lib/gcc/${COMPILER_TRIPLET}/${COMPILER_VERSION}" )
set(LIB_PATH ${LIB_PATH} "${COMPILER_DIR}/${COMPILER_TRIPLET}/lib" )
set(LIB_PATH ${LIB_PATH} "${COMPILER_DIR}/lib" )
set(CMAKE_LIBRARY_PATH ${LIB_PATH} ${SWB_PATH})

include( cmake/find_programs_and_libraries.cmake )

#set(MODULE_DIR ${INCLUDE_DIR} ${PROJECT_SOURCE_DIR}/include/${OS}/${Fortran_COMPILER_NAME} )

if (${OS} STREQUAL "win_x64" OR ${OS} STREQUAL "win_x86")
  add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swb.exe ${CMAKE_INSTALL_PREFIX}/swb.exe
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbstats.exe ${CMAKE_INSTALL_PREFIX}/swbstats.exe
  )
else()  
  add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swb ${CMAKE_INSTALL_PREFIX}/swb
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbstats ${CMAKE_INSTALL_PREFIX}/swbstats
  )
endif()

get_filename_component(COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

include( cmake/print_summary.cmake )

enable_testing()
add_subdirectory(src)
add_subdirectory(tests/general)
add_subdirectory(tests/irrigation)
