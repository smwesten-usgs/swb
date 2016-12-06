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
# LIB_PATH1
# LIB_PATH2
# LIB_PATH3
# LIB_PATH4
# LIB_PATH5

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

# search for programs in the build host directories
SET(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
# for libraries and headers in the target directories
#SET(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
SET(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)

set(SWB_LIBPATH ${PROJECT_SOURCE_DIR}/lib/${OS}/${Fortran_COMPILER_NAME} )
set(SWB_INCPATH ${EXTRA_INCLUDEDIR} ${PROJECT_SOURCE_DIR}/include/${OS}/${Fortran_COMPILER_NAME} )

set(LIB_PATH ${LIB_PATH1} ${LIB_PATH2} ${LIB_PATH3} ${LIB_PATH4} ${LIB_PATH5} ${EXTRA_INCLUDEDIR} )

set(CMAKE_LIBRARY_PATH ${SWB_LIBPATH} ${LIB_PATH} )

include( cmake/find_programs_and_libraries.cmake )

#set(MODULE_DIR ${INCLUDE_DIR} ${PROJECT_SOURCE_DIR}/include/${SYSTEM_TYPE}/${Fortran_COMPILER_NAME} )

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
