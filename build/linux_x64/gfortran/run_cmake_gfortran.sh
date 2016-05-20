!/bin/sh
#remove existing Cmake cache and directories
# remove existing Cmake cache and directories
rm -fr CMake*
rm -f *.txt

# set CMAKE-related and build-related variables
export CMAKEROOT=/usr/bin
export GCC_VERSION=5.3.1
export COMPILER_TRIPLET=x86_64-redhat-linux
export Fortran_COMPILER_NAME=gfortran
export R_HOME=/usr/bin/R

# define where 'make copy' will place executables
export INSTALL_PREFIX=/usr/local/bin

# define other variables for use in the CMakeList.txt file
# options are "Release" or "Debug"
export BUILD_TYPE="Release"
# options are "x86" (32-bit) or "x64" (64-bit)
export PLATFORM_TYPE="x64"

# define platform and compiler specific compilation flags
export CMAKE_Fortran_FLAGS_DEBUG="-O0 -g -ggdb -Wall -fbacktrace -fcheck=all -fexceptions -ffree-line-length-none -static"
#set CMAKE_Fortran_FLAGS_RELEASE="-O2 -mtune=native -floop-parallelize-all -flto -ffree-line-length-none -static-libgcc -static-libgfortran"
export CMAKE_Fortran_FLAGS_RELEASE="-O2 -mtune=native -ffree-line-length-none -finit-local-zero"

# set important environment variables
export FC=/usr/bin/gfortran
export CC=/usr/bin/gcc
export CXX=/usr/bin/g++
export AR=/usr/bin/gcc-ar
export NM=/usr/bin/gcc-nm

set LIBRARY_PATH=/usr/lib

# set compiler-specific link and compile flags
export LDFLAGS="-flto -ldl"
export CPPFLAGS="-DgFortran"
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

cmake ../../.. -G "Unix Makefiles" \
-DGCC_VERSION="$GCC_VERSION " \
-DCOMPILER_TRIPLET="$COMPILER_TRIPLET " \
-DFortran_COMPILER_NAME="$Fortran_COMPILER_NAME" \
-DPLATFORM_TYPE="$PLATFORM_TYPE " \
-DCMAKE_BUILD_TYPE="$BUILD_TYPE " \
-DCMAKE_INSTALL_PREFIX:PATH="$INSTALL_PREFIX " \
-DOS="Linux"                                    \
-DLIBGCC_PATH="/usr/lib/gcc/x86_64-redhat-linux/5.3.1"     \
-DTARGET__SWB_EXECUTABLE:BOOLEAN="$TARGET__SWB_EXECUTABLE " \
-DTARGET__SWB_LIBRARY:BOOLEAN="$TARGET__SWB_LIBRARY " \
-DTARGET__SWBSTATS:BOOLEAN="$TARGET__SWBSTATS " \
-DOPTION__GRAPHICS_SUPPORT="$OPTION__GRAPHICS_SUPPORT " \
-DOPTION__STREAM_INTERACTIONS="$OPTION__STREAM_INTERACTIONS " \
-DOPTION__NETCDF_SUPPORT="$OPTION__NETCDF_SUPPORT" \
-DOPTION__IRRIGATION_MODULE="$OPTION__IRRIGATION_MODULE " \
-DOPTION__STRICT_DATE_CHECKING="$OPTION__STRICT_DATE_CHECKING " \
-DOPTION__DEBUG_PRINT="$OPTION__DEBUG_PRINT " \
-DCMAKE_Fortran_FLAGS_DEBUG="$CMAKE_Fortran_FLAGS_DEBUG " \
-DCMAKE_Fortran_FLAGS_RELEASE="$CMAKE_Fortran_FLAGS_RELEASE" \
-DFC="$FC "                       \
-DCC="$CC "                       \
-DPLATFORM_TYPE="$PLATFORM_TYPE " \
-DBUILD_TYPE="$BUILD_TYPE " \
-DCMAKE_INSTALL_PREFIX:PATH="$INSTALL_PREFIX " \
-DLIB_PATH1="/usr//local/lib64" \
-DLIB_PATH2="/usr/local/lib" \
-DLIB_PATH3="/usr/lib64" \
-DLIB_CURL="/usr/local/lib"

