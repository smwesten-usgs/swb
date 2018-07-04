#!/bin/bash
#remove existing Cmake cache and directories
# remove existing Cmake cache and directories
rm -rf CMake*
rm -rf Testing
rm -rf src
rm -rf tests
rm -f CPack*
rm -f *.txt

# set CMAKE-related and build-related variables
export CMAKEROOT=/usr/bin/cmake
export COMPILER_MAJ_VERSION=8
export COMPILER_VERSION=8.1.0
export COMPILER_DIR=/usr/local
export DISLIN_VERSION=11
export PYTHON_CMD=~/miniconda3/bin/python

# if locate is finding old library versions, run the following.
# source https://superuser.com/questions/109590/whats-the-equivalent-of-linuxs-updatedb-command-for-the-mac#109592
# sudo /usr/libexec/locate.updatedb

# explicitly locate each key library
export LIB_HDF5_HL=$( glocate libhdf5_hl.a | grep Cellar | grep -v "386" )
export LIB_HDF5=$( glocate libhdf5.a | grep Cellar | grep -v "386" )
# prevent locate from glomming onto the i386 version or the miniconda version
export LIB_Z=$(glocate libz.a | grep -v "386" | grep Cellar | grep -v "conda" )
export LIB_SZ=$(glocate libsz.a | grep Cellar | grep -v "386" )
export LIB_NETCDF=$( glocate libnetcdf.dylib | grep Cellar | grep -v "386" )
export LIB_DISLIN=/usr/local/dislin/libdislin.dylib

# DISLIN can be a pain to get running without resorting to modification of the
# environment variable DYLD_LIBRARY_PATH, which is generally frowned upon.
# setting up a soft link is one way to avoid this.
ln -s /usr/local/dislin/libdislin.$DISLIN_VERSION.dylib /usr/local/lib/libdislin.$DISLIN_VERSION.dylib

export LIB_GCC=$(glocate libgcc.a | grep Cellar | grep -v "386" | grep $COMPILER_VERSION)
export LIB_GFORTRAN=$(glocate libgfortran.a | grep Cellar | grep -v "386" | grep $COMPILER_VERSION )

export LIB_XM=$(glocate libXm.dylib | grep -v "386" | grep Cellar )

export DISLIN_MODULE_DIR=$(glocate "mac_osx/gfortran/dislin.mod" | sed -e "s/dislin.mod//g")

export R_SCRIPT=/usr/local/bin/Rscript
export SWB_EXECUTABLE=/usr/local/bin/swb

# define where 'make copy' will place executables
export INSTALL_PREFIX=/usr/local/bin

# define other variables for use in the CMakeList.txt file
# options are "Release" or "Debug"
export BUILD_TYPE="Release"
# options are "x86" (32-bit) or "x64" (64-bit)
export SYSTEM_TYPE="mac_osx"

# define which portions of swb to build (i.e. swbstats? as library?)
export TARGET__SWB_EXECUTABLE="TRUE"
export TARGET__SWB_LIBRARY="FALSE"
export TARGET__SWBSTATS="FALSE"

# define which conditional compilation statements to include
export OPTION__GRAPHICS_SUPPORT="TRUE"
export OPTION__NETCDF_SUPPORT="TRUE"
export OPTION__STRICT_DATE_CHECKING="FALSE"
export OPTION__DEBUG_PRINT="FALSE"

# define platform and compiler specific compilation flags
export CMAKE_Fortran_FLAGS_DEBUG="-O0 -g -ggdb -cpp -fcheck=all -fexceptions -ffree-line-length-none"
#set CMAKE_Fortran_FLAGS_RELEASE="-O2 -mtune=native -floop-parallelize-all -flto -ffree-line-length-none -static-libgcc -static-libgfortran"
export CMAKE_Fortran_FLAGS_RELEASE="-O2 -cpp -mtune=native -ffree-line-length-none"

export PATH=/usr/local/bin:/usr/local/lib:$PATH

# set important environment variables
export FC=gfortran-$COMPILER_MAJ_VERSION
export CC=gcc-$COMPILER_MAJ_VERSION
export CXX=g++-$COMPILER_MAJ_VERSION
export AR=gcc-ar-$COMPILER_MAJ_VERSION
export NM=gcc-nm-$COMPILER_MAJ_VERSION
export LD=/usr/bin/ld
export STRIP=/usr/bin/strip
export CMAKE_RANLIB=gcc-ranlib-$COMPILER_MAJ_VERSION

cmake ../../.. -G "Unix Makefiles"                \
-DDISLIN_MODULE_DIR="$DISLIN_MODULE_DIR "         \
-DFortran_COMPILER_NAME="$Fortran_COMPILER_NAME"  \
-DSWB_EXECUTABLE="$SWB_EXECUTABLE"                \
-DCOMPILER_VERSION="$COMPILER_VERSION "           \
-DLIB_HDF5_HL="$LIB_HDF5_HL "                     \
-DLIB_HDF5="$LIB_HDF5 "                           \
-DLIB_SZ="$LIB_SZ "                               \
-DLIB_Z="$LIB_Z "                                 \
-DLIB_NETCDF="$LIB_NETCDF "                       \
-DLIB_DISLIN="$LIB_DISLIN "                       \
-DLIB_GCC="$LIB_GCC "                             \
-DLIB_GFORTRAN="$LIB_GFORTRAN "                   \
-DLIB_XM="$LIB_XM "                               \
-DPYTHON_CMD="$PYTHON_CMD "                       \
-DR_SCRIPT="$R_SCRIPT "                           \
-DCMAKE_EXE_LINKER_FLAGS="$LINKER_FLAGS "         \
-DSYSTEM_TYPE="$SYSTEM_TYPE "                     \
-DCMAKE_BUILD_TYPE="$BUILD_TYPE "                 \
-DCMAKE_INSTALL_PREFIX:PATH="$INSTALL_PREFIX "    \
-DTARGET__SWB_EXECUTABLE:BOOLEAN="$TARGET__SWB_EXECUTABLE " \
-DTARGET__SWB_LIBRARY:BOOLEAN="$TARGET__SWB_LIBRARY " \
-DTARGET__SWBSTATS:BOOLEAN="$TARGET__SWBSTATS " \
-DOPTION__GRAPHICS_SUPPORT="$OPTION__GRAPHICS_SUPPORT " \
-DOPTION__STREAM_INTERACTIONS="$OPTION__STREAM_INTERACTIONS " \
-DOPTION__NETCDF_SUPPORT="$OPTION__NETCDF_SUPPORT" \
-DOPTION__STRICT_DATE_CHECKING="$OPTION__STRICT_DATE_CHECKING " \
-DOPTION__DEBUG_PRINT="$OPTION__DEBUG_PRINT " \
-DCMAKE_Fortran_FLAGS_DEBUG="$CMAKE_Fortran_FLAGS_DEBUG " \
-DCMAKE_Fortran_FLAGS_RELEASE="$CMAKE_Fortran_FLAGS_RELEASE"
