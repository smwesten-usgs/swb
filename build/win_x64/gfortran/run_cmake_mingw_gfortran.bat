@echo off
:: cmake -G "Mi:: remove existing Cmake cache and directories
:: remove existing Cmake cache and directories
rmdir /S /Q CMake*
del /S /Q *.txt

:: set CMAKE-related and build-related variables
set CMAKEROOT=C:\Program Files (x86)\CMake 2.8
set MINGWBASE=c:\MinGW64
set MINGW_VERSION=4.8.0
set COMPILER_TRIPLET=x86_64-w64-mingw32
set Fortran_COMPILER_NAME=gfortran
set R_HOME="C:\Program Files\R\R-3.0.1\bin"
set OMP_NUM_THREADS=8

:: define where 'make copy' will place executables
set INSTALL_PREFIX=d:\DOS

:: define other variables for use in the CMakeList.txt file
:: options are "Release", "Profile" or "Debug"
set BUILD_TYPE="Release"
:: options are "x86" (32-bit) or "x64" (64-bit)
set PLATFORM_TYPE="x64"

:: define which portions of swb to build (i.e. swbstats? as library?)
set TARGET__SWB_EXECUTABLE="TRUE"
set TARGET__SWB_LIBRARY="FALSE"
set TARGET__SWBSTATS="TRUE"

:: define which conditional compilation statements to include
set OPTION__GRAPHICS_SUPPORT="TRUE"
set OPTION__STREAM_INTERACTIONS="FALSE"
set OPTION__NETCDF_SUPPORT="TRUE"
set OPTION__STRICT_DATE_CHECKING="FALSE"
set OPTION__DEBUG_PRINT="FALSE"

:: define platform and compiler specific compilation flags
set CMAKE_Fortran_FLAGS_DEBUG="-O0 -g -ggdb -fcheck=all -fstack-usage -fexceptions -ffree-line-length-none -static-libgcc -static-libgfortran -DCURL_STATICLIB"
set CMAKE_Fortran_FLAGS_RELEASE="-O3 -mtune=generic -floop-parallelize-all -flto -ffree-line-length-none -static-libgcc -static-libgfortran -DCURL_STATICLIB"
set CMAKE_Fortran_FLAGS_PROFILE="-O2 -pg -g -fno-omit-frame-pointer -DNDEBUG -fno-inline-functions -fno-inline-functions-called-once -fno-optimize-sibling-calls -ffree-line-length-none -static-libgcc -static-libgfortran -DCURL_STATICLIB"
::set CMAKE_Fortran_FLAGS_RELEASE="-O3 -mtune=native -fopenmp -flto -ffree-line-length-none -static-libgcc -static-libgfortran -DCURL_STATICLIB"

:: IMPORTANT!! Make sure a valid TEMP directory exists!!
set TEMP=d:\TEMP

:: set path to include important MinGW locations
set PATH=%MINGWBASE%\bin;%MINGWBASE%\include;%MINGWBASE%\lib

:: recreate clean Windows environment
set PATH=%PATH%;c:\windows;c:\windows\system32;c:\windows\system32\Wbem
set PATH=%PATH%;C:\Program Files (x86)\7-Zip
set PATH=%PATH%;%CMAKEROOT%\bin;%CMAKEROOT%\share

:: not every installation will have these; I (SMW) find them useful
set PATH=%PATH%;c:\Program Files (x86)\Zeus
set PATH=%PATH%;D:\DOS\gnuwin32\bin

:: set important environment variables
set FC=%MINGWBASE%\bin\gfortran.exe
set CC=%MINGWBASE%\bin\gcc.exe
set CXX=%MINGWBASE%\bin\g++.exe
set AR=%MINGWBASE%\bin\ar.exe
set NM=%MINGWBASE%\bin\nm.exe
set LD=%MINGWBASE%\bin\ld.exe
set STRIP=%MINGWBASE%\bin\strip.exe
set CMAKE_RANLIB=%MINGWBASE%\bin\ranlib.exe

set INCLUDE=%MINGWBASE%\include
set LIB=%MINGWBASE%\lib;%MINGWBASE%\lib\gcc\%COMPILER_TRIPLET%\%MINGW_VERSION%
set LIBRARY_PATH=%MINGWBASE%\lib;%MINGWBASE%\lib\gcc\%COMPILER_TRIPLET%\%MINGW_VERSION%

:: set compiler-specific link and compile flags
set LDFLAGS="-flto"
set CFLAGS="-DCURL_STATICLIB"
set CPPFLAGS="DgFortran -DCURL_STATICLIB"

set CMAKE_INCLUDE_PATH=%INCLUDE%
set CMAKE_LIBRARY_PATH=%LIB%
set CTEST_OUTPUT_ON_FAILURE=1

:: add --trace to see copious details re: CMAKE

cmake ..\..\.. -G "MinGW Makefiles" ^
-DMINGWBASE=%MINGWBASE% ^
-DMINGW_VERSION=%MINGW_VERSION% ^
-DCOMPILER_TRIPLET=%COMPILER_TRIPLET% ^
-DFortran_COMPILER_NAME=%Fortran_COMPILER_NAME% ^
-DPLATFORM_TYPE=%PLATFORM_TYPE% ^
-DCMAKE_BUILD_TYPE=%BUILD_TYPE% ^
-DCMAKE_INSTALL_PREFIX:PATH=%INSTALL_PREFIX% ^
-DCMAKE_MAKE_PROGRAM:FILEPATH=%MINGWBASE%\bin\make.exe ^
-DCMAKE_RANLIB:FILEPATH=%MINGWBASE%\bin\ranlib.exe ^
-DCMAKE_C_COMPILER:FILEPATH=%MINGWBASE%\bin\gcc.exe ^
-DCMAKE_Fortran_COMPILER:FILEPATH=%MINGWBASE%\bin\gfortran.exe ^
-DTARGET__SWB_EXECUTABLE:BOOLEAN=%TARGET__SWB_EXECUTABLE% ^
-DTARGET__SWB_LIBRARY:BOOLEAN=%TARGET__SWB_LIBRARY% ^
-DTARGET__SWBSTATS:BOOLEAN=%TARGET__SWBSTATS% ^
-DOPTION__GRAPHICS_SUPPORT=%OPTION__GRAPHICS_SUPPORT% ^
-DOPTION__STREAM_INTERACTIONS=%OPTION__STREAM_INTERACTIONS% ^
-DOPTION__NETCDF_SUPPORT=%OPTION__NETCDF_SUPPORT% ^
-DOPTION__IRRIGATION_MODULE=%OPTION__IRRIGATION_MODULE% ^
-DOPTION__STRICT_DATE_CHECKING=%OPTION__STRICT_DATE_CHECKING% ^
-DOPTION__DEBUG_PRINT=%OPTION__DEBUG_PRINT% ^
-DCMAKE_Fortran_FLAGS_DEBUG=%CMAKE_Fortran_FLAGS_DEBUG% ^
-DCMAKE_Fortran_FLAGS_RELEASE=%CMAKE_Fortran_FLAGS_RELEASE% ^
-DCMAKE_Fortran_FLAGS_PROFILE=%CMAKE_Fortran_FLAGS_PROFILE%
