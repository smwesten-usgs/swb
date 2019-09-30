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
export COMPILER_VERSION=6.3.1
export COMPILER_MAJ_VERSION=6
export COMPILER_TRIPLET=x86_64-linux-gnu
export COMPILER_DIR=/usr/local

# if locate is finding old library versions, run the following.
# source https://superuser.com/questions/109590/whats-the-equivalent-of-linuxs-updatedb-command-for-the-mac#109592
# sudo /usr/libexec/locate.updatedb
#
# on Linux, the command in normally just 'sudo updatedb'

# explicitly locate each key library
export LIB_HDF5_HL=$( locate libhdf5_hl.so | grep -v "so." | grep -v "openmpi" )
export LIB_HDF5=$( locate libhdf5.so | grep -v "so." | grep -v "openmpi" )
# prevent locate from glomming onto the i386 version or the miniconda version
export LIB_Z=$(locate libz.so | grep "/usr/lib" | grep -v "openmpi" )
export LIB_CURL="NOTFOUND"
#export LIB_SZ=$(locate libsz.a | grep "/usr/lib" | grep -v "openmpi" )
export LIB_NETCDF=$( locate libnetcdf.so| grep -v "so." | grep -v "openmpi")
export LIB_DISLIN=$(locate libdislin.so | grep "/usr/lib64" | grep -v "so.")
export LIB_GCC=$(locate libgcc.a | grep "/usr/lib" | grep -v "/32" | grep -v "mxe" | grep -v "x32" )
export LIB_GFORTRAN=$(locate libgfortran.a | grep "/usr/lib" | grep -v "/32" | grep -v "mxe" | grep -v "x32")


export LIB_XM=$(locate libXm.so.4 | grep "/usr/lib" | grep -v "so.4." )
export LIB_XT=$(locate libXt.so | grep "/usr/lib" | grep -v "so.6." )

# the following tend to be needed for Windows builds
#export LIB_OPENGL32=$(locate libopengl32.a | grep "/usr/lib" | grep -v "mxe")
#export LIB_GDI32=$(locate libgdi32.a | grep "/usr/lib" | grep -v "mxe")

export LIB_OPENGL32="NOTFOUND"
export LIB_GDI32="NOTFOUND"
export LIB_SZ="NOTFOUND"

#if [ -n "$LIB_OPENGL" ]; then
#  export LIB_OPENGL="NOTFOUND"
#fi

export DISLIN_MODULE_DIR=$(locate "/gf/dislin.mod" | sed -e "s/dislin.mod//g")

export R_SCRIPT=/usr/bin/Rscript
export SWB_EXECUTABLE=/usr/local/bin/swb

# define where 'make copy' will place executables
export INSTALL_PREFIX=/usr/local/bin

# define other variables for use in the CMakeList.txt file
# options are "Release" or "Debug"
export BUILD_TYPE="Release"
# options are "x86" (32-bit) or "x64" (64-bit)
export SYSTEM_TYPE="linux"

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

export EXTRA_LIBS=$(locate libdl.so | grep -v "lib32" grep -v "libx32" | grep -v "so." )
export EXTRA_LIBS="$EXTRA_LIBS;$(locate librt.so | grep -v "lib32" | grep -v "libx32" | grep -v "so." )"

export PATH=/usr/local/bin:/usr/local/lib:$PATH

# set important environment variables
export FC=gfortran
export CC=gcc
export CXX=g++
export AR=gcc-ar
export NM=gcc-nm
export LD=/usr/bin/ld
export STRIP=/usr/bin/strip
export CMAKE_RANLIB=gcc-ranlib

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
-DLIB_OPENGL32="$LIB_OPENGL32 "                   \
-DR_SCRIPT="$R_SCRIPT "                           \
-DLIB_XM="$LIB_XM "                               \
-DLIB_XT="$LIB_XT "                               \
-DLIB_CURL="$LIB_CURL "                           \
-DLIB_EXTRAS="$EXTRA_LIBS "                       \
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
