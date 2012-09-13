:: cmake -G "MinGW Makefiles" -DCMAKE_MAKE_PROGRAM:FILEPATH=c:/MinGW32/bin/make.exe ^
:: -DCMAKE_C_COMPILER:FILEPATH=c:/MinGW32/bin/gcc.exe ^
:: -DCMAKE_FORTRAN_COMPILER:FILEPATH=c:/MinGW32/bin/gfortran.exe  ..\..

set CMAKEROOT=C:/Program Files (x86)/CMake 2.8
set MinGWbase=c:/MinGW64

set INCLUDE=%MinGWbase%\include
set LIB=%MinGWbase%\lib

set TEMP=d:/TEMP
set FC=%MinGWbase%/bin/gfortran.exe
set F90=%MinGWbase%/bin/gfortran.exe
set CC=%MinGWbase%/bin/gcc.exe
set CXX=%MinGWbase%/bin/g++.exe
set AR=%MinGWbase%/bin/ar.exe
set NM=%MinGWbase%/bin/nm.exe
set LD=%MinGWbase%/bin/ld.exe
set STRIP=%MinGWbase%/bin/strip.exe
set CMAKE_RANLIB=%MinGWbase%/bin/ranlib.exe
set LIBRARY_PATH=%MinGWbase%/lib
set CPPFLAGS=-DgFortran
 
set CMAKE_INCLUDE_PATH=%INCLUDE%
set CMAKE_LIBRARY_PATH=%LIB%

:: add --trace to see copious details re: CMAKE

cmake ..\..\.. -G "MinGW Makefiles" -DPLATFORM_TYPE="x64" -DCMAKE_BUILD_TYPE="Release" -DCMAKE_INSTALL_PREFIX:PATH="D:\DOS" -DCMAKE_MAKE_PROGRAM="c:\MinGW64\bin\make.exe" -DCMAKE_RANLIB:FILEPATH="c:\MinGW64\bin\ranlib.exe" -DCMAKE_C_COMPILER:FILEPATH="c:\MinGW64\bin\gcc.exe" -DCMAKE_Fortran_COMPILER:FILEPATH="c:\MinGW64\bin\gfortran.exe"