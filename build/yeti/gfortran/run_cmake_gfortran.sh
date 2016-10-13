#!/bin/sh
#remove existing Cmake cache and directories
# remove existing Cmake cache and directories
rm -fr CMake*
rm -f *.txt
module load tools/hdf5-1.8.13-gnu
module load tools/netcdf-4.3.2-gnu
module load dislin/10.6
module load tools/szip-2.1-gnu
module load tools/cmake-2.8.12.1
module load gcc/5.3

# set CMAKE-related and build-related variables
#export CMAKEROOT=/home/smwesten/lib/cmake-3.6.0-Linux-x86_64/bin
export GCC_VERSION=5.3.0
export COMPILER_TRIPLET=x86_64-unknown-linux-gnu
export Fortran_COMPILER_NAME=gfortran
export R_HOME=/usr/bin/R

# define where 'make copy' will place executables
export INSTALL_PREFIX=/usr/local/bin

# define other variables for use in the CMakeList.txt file
# options are "Release" or "Debug"
export BUILD_TYPE="Release"
# options are "x86" (32-bit) or "x64" (64-bit)
export PLATFORM_TYPE="x64"
# e.g. Linux, win_x64, mac_osx, or Yeti
export SYSTEM_TYPE="Yeti"

export LIBGCC=$(find /cxfs/projects/spack/opt/spack -name "*libgcc.a" | grep "5.3.0" | sed -e 's/\/libgcc.a//')

# define platform and compiler specific compilation flags
export CMAKE_Fortran_FLAGS_DEBUG="-O0 -g -ggdb -Wall -fbacktrace -fcheck=all -fexceptions -ffree-line-length-none -static -cpp"
#set CMAKE_Fortran_FLAGS_RELEASE="-O2 -mtune=native -floop-parallelize-all -flto -ffree-line-length-none -static-libgcc -static-libgfortran"
export CMAKE_Fortran_FLAGS_RELEASE="-O2 -mtune=native -ffree-line-length-none -finit-local-zero -cpp"

# set important environment variables
export FC=$(which gfortran)
export CC=$(which gcc)
export CXX=$(which g++)
export AR=$(which gcc-ar)
export NM=$(which gcc-nm)

set LIBRARY_PATH=/usr/lib64

# set compiler-specific link and compile flags
export LDFLAGS="-flto -ldl"
export CPPFLAGS="-DgFortran"
# define which portions of swb to build (i.e. swbstats? as library?)
export TARGET__SWB_EXECUTABLE="TRUE"
export TARGET__SWB_LIBRARY="FALSE"
export TARGET__SWBSTATS="FALSE"

# define which conditional compilation statements to include
export OPTION__GRAPHICS_SUPPORT="FALSE"
export OPTION__STREAM_INTERACTIONS="FALSE"
export OPTION__NETCDF_SUPPORT="TRUE"
export OPTION__STRICT_DATE_CHECKING="FALSE"
export OPTION__DEBUG_PRINT="FALSE"

cmake ../../.. -G "Unix Makefiles"                              \
-DFortran_COMPILER_NAME="$Fortran_COMPILER_NAME"                \
-DPLATFORM_TYPE="$PLATFORM_TYPE "                               \
-DCMAKE_BUILD_TYPE="$BUILD_TYPE "                               \
-DCMAKE_INSTALL_PREFIX:PATH="$INSTALL_PREFIX "                  \
-DSYSTEM_TYPE="Yeti"                                            \
-DLIBNETCDF_PATH="$NETCDF_LIB"                                  \
-DLIBHDF5_PATH="$HDF5_LIB_DIR"                                  \
-DTARGET__SWB_EXECUTABLE:BOOLEAN="$TARGET__SWB_EXECUTABLE "     \
-DTARGET__SWB_LIBRARY:BOOLEAN="$TARGET__SWB_LIBRARY "           \
-DTARGET__SWBSTATS:BOOLEAN="$TARGET__SWBSTATS "                 \
-DOPTION__GRAPHICS_SUPPORT="$OPTION__GRAPHICS_SUPPORT "         \
-DOPTION__STREAM_INTERACTIONS="$OPTION__STREAM_INTERACTIONS "   \
-DOPTION__NETCDF_SUPPORT="$OPTION__NETCDF_SUPPORT"              \
-DOPTION__STRICT_DATE_CHECKING="$OPTION__STRICT_DATE_CHECKING " \
-DOPTION__DEBUG_PRINT="$OPTION__DEBUG_PRINT "                   \
-DCMAKE_Fortran_FLAGS_DEBUG="$CMAKE_Fortran_FLAGS_DEBUG "       \
-DCMAKE_Fortran_FLAGS_RELEASE="$CMAKE_Fortran_FLAGS_RELEASE"    \
-DPLATFORM_TYPE="$PLATFORM_TYPE "                               \
-DCMAKE_INSTALL_PREFIX:PATH="$INSTALL_PREFIX "                  \
-DLIB_PATH1="$LD_LIBRARY_PATH"                                  \
-DLIBGCC_PATH="$LIBGCC"                                         \
-DPATH_TO_R="$(which R)" 

