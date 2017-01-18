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
export COMPILER_VERSION=5.4.0
export COMPILER_MAJ_VERSION=5
export COMPILER_TRIPLET=x86_64-apple-darwin15.5.0
export COMPILER_DIR=/usr/local

# identify the versions for key libraries that SWB relies upon
export HDF5_VERSION=1.8.17
export NETCDF_VERSION=4.3.3
export DISLIN_VERSION=10.6.0
export LIBZ_VERSION=1.2.8
export LIBSZ_VERSION=2.1

export OPENMOTIF_VERSION=2.3.6

# explicitly locate each key library
export LIB_HDF5_HL=$( locate libhdf5_hl.a | grep $HDF5_VERSION | grep -v "386" )
export LIB_HDF5=$( locate libhdf5.a | grep $HDF5_VERSION | grep -v "386" )
# prevent locate from glomming onto the i386 version or the miniconda version
export LIB_Z=$(locate libz.a | grep $LIBZ_VERSION | grep -v "386" | grep -v "conda" )
export LIB_SZ=$(locate libsz.a | grep $LIBSZ_VERSION | grep -v "386" )
export LIB_NETCDF=$( locate libnetcdf.a | grep $NETCDF_VERSION | grep -v "386" )
export LIB_DISLIN=$(locate libdislin.$DISLIN_VERSION.dylib | grep -v "386" )
export LIB_GCC=$(locate libgcc.a | grep $COMPILER_VERSION | grep -v "386" )
export LIB_GFORTRAN=$(locate libgfortran.a | grep $COMPILER_VERSION | grep -v "386" )

export LIB_XM=$(locate libXm.dylib | grep $OPENMOTIF_VERSION | grep -v "386" )

export DISLIN_MODULE_DIR=$(locate "gf/dislin.mod" | sed -e "s/dislin.mod//g")

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
export CMAKE_Fortran_FLAGS_RELEASE="-O3 -cpp -mtune=native -ffree-line-length-none"

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
-DDISLIN_MODULE_DIR="$DISLIN_MODULE_DIR "   \
-DFortran_COMPILER_NAME="$Fortran_COMPILER_NAME" \
-DSWB_EXECUTABLE="$SWB_EXECUTABLE"      \
-DCOMPILER_VERSION="$COMPILER_VERSION " \
-DLIB_HDF5_HL="$LIB_HDF5_HL "    \
-DLIB_HDF5="$LIB_HDF5 "          \
-DLIB_SZ="$LIB_SZ"               \
-DLIB_Z="$LIB_Z "                \
-DLIB_NETCDF="$LIB_NETCDF "      \
-DLIB_DISLIN="$LIB_DISLIN "      \
-DLIB_GCC="$LIB_GCC "            \
-DLIB_GFORTRAN="$LIB_GFORTRAN "  \
-DLIB_XM="$LIB_XM"               \
-DR_SCRIPT="$R_SCRIPT"           \
-DCMAKE_EXE_LINKER_FLAGS="$LINKER_FLAGS " \
-DSYSTEM_TYPE="$SYSTEM_TYPE " \
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
