#!/bin/bash
#remove existing Cmake cache and directories
# remove existing Cmake cache and directories
rm -fr CMake*
rm -f *.txt

# set CMAKE-related and build-related variables
export CMAKEROOT=/usr/bin/cmake
export COMPILER_VERSION=4.9.0
export COMPILER_MAJ_VERSION=4.9
export COMPILER_TRIPLET=x86_64-apple-darwin13.1.0
export COMPILER_DIR=/usr/local
export LIB_PATH1="/usr/local/lib/gcc/$COMPILER_TRIPLET/$COMPILER_VERSION"
export LIB_PATH2=/usr/OpenMotif/lib
export LIB_PATH3=/usr/local/lib
export LIB_PATH4=/usr/local/Library/LinkedKegs/gfortran/gfortran/lib
export Fortran_COMPILER_NAME=gfortran
export R_HOME=/usr/bin/R

# define where 'make copy' will place executables
export INSTALL_PREFIX=/usr/local/bin

# define other variables for use in the CMakeList.txt file
# options are "Release" or "Debug"
export BUILD_TYPE="Release"
# options are "x86" (32-bit) or "x64" (64-bit)
export OS="mac_osx"

# define which portions of swb to build (i.e. swbstats? as library?)
export TARGET__SWB_EXECUTABLE="TRUE"
export TARGET__SWB_LIBRARY="FALSE"
export TARGET__SWBSTATS="FALSE"

# define which conditional compilation statements to include
export OPTION__GRAPHICS_SUPPORT="TRUE"
export OPTION__STREAM_INTERACTIONS="FALSE"
export OPTION__NETCDF_SUPPORT="TRUE"
export OPTION__STRICT_DATE_CHECKING="FALSE"
export OPTION__DEBUG_PRINT="FALSE"

# define platform and compiler specific compilation flags
export CMAKE_Fortran_FLAGS_DEBUG="-O0 -g -ggdb -fcheck=all -fexceptions -ffree-line-length-none -static -static-libgcc -static-libgfortran"
#set CMAKE_Fortran_FLAGS_RELEASE="-O2 -mtune=native -floop-parallelize-all -flto -ffree-line-length-none -static-libgcc -static-libgfortran"
export CMAKE_Fortran_FLAGS_RELEASE="-O3 -mtune=native -ffree-line-length-none"

# set important environment variables
export FC=gfortran-$COMPILER_MAJ_VERSION
export CC=gcc-$COMPILER_MAJ_VERSION
export CXX=g++-$COMPILER_MAJ_VERSION
export AR=gcc-ar-$COMPILER_MAJ_VERSION
export NM=gcc-nm-$COMPILER_MAJ_VERSION
export LD=/usr/bin/ld
export STRIP=/usr/bin/strip
export CMAKE_RANLIB=gcc-ranlib-$COMPILER_MAJ_VERSION

cmake ../../.. -G "Unix Makefiles" \
-DCOMPILER_DIR="$COMPILER_DIR " \
-DCOMPILER_TRIPLET="$COMPILER_TRIPLET " \
-DFortran_COMPILER_NAME="$Fortran_COMPILER_NAME" \
-DCOMPILER_VERSION="$COMPILER_VERSION " \
-DLIB_PATH1="$LIB_PATH1 " \
-DLIB_PATH2="$LIB_PATH2 " \
-DLIB_PATH3="$LIB_PATH3 " \
-DLIB_PATH4="$LIB_PATH4 " \
-DCMAKE_EXE_LINKER_FLAGS="$LINKER_FLAGS " \
-DOS="$OS " \
-DCMAKE_BUILD_TYPE="$BUILD_TYPE " \
-DCMAKE_INSTALL_PREFIX:PATH="$INSTALL_PREFIX " \
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

