#!/bin/sh
#remove existing Cmake cache and directories
# remove existing Cmake cache and directories
rm -fr CMake*
rm -f *.txt
module load tools/hdf5-1.8.13-gnu
module load tools/netcdf-4.3.2-gnu
module load dislin/10.6
module load tools/szip-2.1-gnu
module load cmake/2.8.12.1
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

# explicitly locate each key library
export LIB_HDF5_HL=$( locate libhdf5_hl.so.6.0.4 )
export LIB_HDF5=$( locate libhdf5.so.6.0.4 )
# prevent locate from glomming onto the i386 version or the miniconda version
export LIB_Z=$(locate lib64/libz.a )
export LIB_NETCDF=$( locate libnetcdf.so.6.0.0 )
export LIB_DISLIN=$(locate libdislin )
export LIB_GCC=$(locate libgcc.a | grep -v "/32/" )
export LIB_GFORTRAN=$(locate libgfortran.a | grep -v "/32/" )

export LIB_XM=$(locate libXm.dylib | grep $OPENMOTIF_VERSION | grep -v "386" )

export DISLIN_MODULE_DIR=$(locate "gf/dislin.mod" | sed -e "s/dislin.mod//g")

export R_SCRIPT=/usr/local/bin/Rscript
export SWB_EXECUTABLE=/usr/local/bin/swb# define platform and compiler specific compilation flags
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
