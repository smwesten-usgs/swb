REM @echo off
:: remove existing Cmake cache and directories
rmdir /S /Q CMakeFiles
rmdir /S /Q src
rmdir /S /Q Testing
rmdir /S /Q tests
del /S /Q *.txt

:: set CMAKE-related and build-related variables
set CMAKEROOT=C:\Program Files (x86)\CMake
set COMPILER_DIR=C:\MinGW64
set COMPILER_VERSION=5.4.0
set COMPILER_TRIPLET=x86_64-w64-mingw32

set MAKE_EXECUTABLE_NAME=mingw32-make.exe
set Fortran_COMPILER_NAME=gfortran
set CMAKE_C_COMPILER=gcc
set R_SCRIPT=C:/Program Files/R/R-3.4.1/bin/Rscript.exe
set SWB_EXECUTABLE=d:/DOS/swb.exe

:: explicitly locate each key library
for /f %%x in ('dir /b /s c:\MinGW64\*libhdf5_hl.a') do call set LIB_HDF5_HL=%%x
for /f %%x in ('dir /b /s c:\MinGW64\*libhdf5.a') do call set LIB_HDF5=%%x
for /f %%x in ('dir /b /s c:\MinGW64\*libsz.a') do call set LIB_SZ=%%x
for /f %%x in ('dir /b /s c:\MinGW64\*libz.a') do call set LIB_Z=%%x
for /f %%x in ('dir /b /s c:\MinGW64\*libnetcdf.a') do call set LIB_NETCDF=%%x
for /f %%x in ('dir /b /s c:\MinGW64\*libdismg.a') do call set LIB_DISLIN=%%x
::set LIB_DISLIN=$(locate libdislin.$DISLIN_VERSION.dylib | grep -v "386" )
for /f %%x in ('dir /b /s c:\MinGW64\*libgcc.a') do call set LIB_GCC=%%x
for /f %%x in ('dir /b /s c:\MinGW64\*libgfortran.a') do call set LIB_GFORTRAN=%%x
for /f %%x in ('dir /b /s c:\MinGW64\*libopengl32.a') do call set LIB_OPENGL32=%%x
for /f %%x in ('dir /b /s c:\MinGW64\*libgdi32.a') do call set LIB_GDI32=%%x

set LIB_HDF5_HL=%LIB_HDF5_HL:\=/%
set LIB_HDF5=%LIB_HDF5:\=/%
set LIB_NETCDF=%LIB_NETCDF:\=/%
set LIB_DISLIN=%LIB_DISLIN:\=/%
set LIB_GCC=%LIB_GCC:\=/%
set LIB_GFORTRAN=%LIB_GFORTRAN:\=/%
set LIB_Z=%LIB_Z:\=/%
set LIB_SZ=%LIB_SZ:\=/%
set LIB_OPENGL32=%LIB_OPENGL32:\=/%
set LIB_GDI32=%LIB_GDI32:\=/%

set DISLIN_MODULE_DIR="include/win_x64/gfortran"

:: define where 'make copy' will place executables
set INSTALL_PREFIX=d:/DOS

:: define other variables for use in the CMakeList.txt file
:: options are "Release", "Profile" or "Debug"
set BUILD_TYPE="Release"

:: options are "x86" (32-bit) or "x64" (64-bit)
set SYSTEM_TYPE="win_x64"

:: define which portions of swb to build (i.e. swbstats? as library?)
set TARGET__SWB_EXECUTABLE="TRUE"
set TARGET__SWB_LIBRARY="FALSE"
set TARGET__SWBSTATS="FALSE"

:: define which conditional compilation statements to include
set OPTION__STREAM_INTERACTIONS="FALSE"
set OPTION__STRICT_DATE_CHECKING="FALSE"
set OPTION__DEBUG_PRINT="FALSE"

:: define platform and compiler specific compilation flags
set CMAKE_Fortran_FLAGS_DEBUG="-O0 -g -ggdb -cpp -fcheck=all -fstack-usage -fexceptions -ffree-line-length-none -static -static-libgcc -static-libgfortran -DCURL_STATICLIB"
set CMAKE_Fortran_FLAGS_RELEASE="-O2 -cpp -ffree-line-length-none -static -static-libgcc -static-libgfortran -DCURL_STATICLIB -mtune=native"
set CMAKE_Fortran_FLAGS_PROFILE="-O2 -pg -g -cpp -fno-omit-frame-pointer -DNDEBUG -fno-inline-functions -fno-inline-functions-called-once -fno-optimize-sibling-calls -ffree-line-length-none -static -static-libgcc -static-libgfortran -DCURL_STATICLIB"
::set CMAKE_Fortran_FLAGS_RELEASE="-O3 -mtune=native -fopenmp -flto -ffree-line-length-none -static-libgcc -static-libgfortran -DCURL_STATICLIB"

:: set a useful alias for make
echo %COMPILER_DIR%\bin\%MAKE_EXECUTABLE_NAME% %%1 > make.bat

:: set compiler-specific link and compile flags
set LDFLAGS="-flto"
set CFLAGS="-DCURL_STATICLIB"
set CPPFLAGS="DgFortran -DCURL_STATICLIB"

set CTEST_OUTPUT_ON_FAILURE=1

:: invoke CMake; add --trace to see copious details re: CMAKE
for %%f in ( "CodeBlocks - MinGW Makefiles" "MinGW Makefiles" ) do ^
cmake ..\..\.. -G %%f ^
-DDISLIN_MODULE_DIR=%DISLIN_MODULE_DIR%    ^
-DCMAKE_Fortran_COMPILER=%Fortran_COMPILER_NAME% ^
-DCMAKE_C_COMPILER=%CMAKE_C_COMPILER% ^
-DSWB_EXECUTABLE=%SWB_EXECUTABLE%      ^
-DCOMPILER_VERSION=%COMPILER_VERSION%  ^
-DLIB_HDF5_HL=%LIB_HDF5_HL%     ^
-DLIB_HDF5=%LIB_HDF5%           ^
-DLIB_SZ=%LIB_SZ%               ^
-DLIB_Z=%LIB_Z%                 ^
-DLIB_NETCDF=%LIB_NETCDF%       ^
-DLIB_DISLIN=%LIB_DISLIN%       ^
-DLIB_GCC=%LIB_GCC%             ^
-DLIB_GFORTRAN=%LIB_GFORTRAN%   ^
-DLIB_OPENGL32=%LIB_OPENGL32%   ^
-DLIB_GDI32=%LIB_GDI32%         ^
-DR_SCRIPT="%R_SCRIPT%"           ^
-DCMAKE_EXE_LINKER_FLAGS=%LINKER_FLAGS%  ^
-DSYSTEM_TYPE=%SYSTEM_TYPE%  ^
-DCMAKE_BUILD_TYPE=%BUILD_TYPE%  ^
-DCMAKE_INSTALL_PREFIX:PATH=%INSTALL_PREFIX% ^
-DTARGET__SWB_EXECUTABLE:BOOLEAN=%TARGET__SWB_EXECUTABLE%  ^
-DTARGET__SWB_LIBRARY:BOOLEAN=%TARGET__SWB_LIBRARY%  ^
-DTARGET__SWBSTATS:BOOLEAN=%TARGET__SWBSTATS%  ^
-DOPTION__GRAPHICS_SUPPORT=%OPTION__GRAPHICS_SUPPORT%  ^
-DOPTION__STREAM_INTERACTIONS=%OPTION__STREAM_INTERACTIONS%  ^
-DOPTION__NETCDF_SUPPORT=%OPTION__NETCDF_SUPPORT% ^
-DOPTION__STRICT_DATE_CHECKING=%OPTION__STRICT_DATE_CHECKING%  ^
-DOPTION__DEBUG_PRINT=%OPTION__DEBUG_PRINT%  ^
-DCMAKE_Fortran_FLAGS_DEBUG=%CMAKE_Fortran_FLAGS_DEBUG%  ^
-DCMAKE_Fortran_FLAGS_RELEASE=%CMAKE_Fortran_FLAGS_RELEASE%
