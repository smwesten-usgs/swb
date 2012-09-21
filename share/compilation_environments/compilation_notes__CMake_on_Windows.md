CMake lessons learned
===========================

CMake seems to work pretty nicely on Linux and Mac boxes. On my Windows box, it has taken
a long time for me to sort out why CMake failed to find the appropriate executables until
finally one day I examined the modules (see C:\Program Files (x86)\CMake 2.8\share\cmake-2.8\Modules).
In particular, this module file caused much grief: CMakeMinGWFindMake.cmake

The following statement apparently was the problem:

~~~
FIND_PROGRAM(CMAKE_MAKE_PROGRAM make.exe PATHS
  "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\MinGW;InstallLocation]/bin" 
  c:/MinGW/bin /MinGW/bin)
~~~

Despite the fact that I very carefully cleansed my cmd.exe environment of all extraneous environment variables
and explicitly set the key environment variables in my call to cmake, the CMake module file was helpfully pulling executables from 
all over my Windows machine: there's a copy of MinGW distributed with Python, for example, that it seemed to pick up on regularly. 

There may be a more "correct" solution, but mine was to simply delete the reference to the Windows registry and insert additional PATHS
arguments (e.g. c:\MinGW64\bin).

~~~
:: nuke all existing environment variables

for /f "delims==" %%a in ('set') do set %%a=

del /F /Q CMakeCache.txt

set PATH=C:\MinGW64\bin
set PATH=%PATH%;C:\MinGW64\include;C:\MinGW64\lib

:: recreate environment from scratch

set PATH=%PATH%;c:\windows;c:\windows\system32;c:\windows\system32\Wbem
set PATH=%PATH%;C:\Program Files (x86)\7-Zip
set PATH=%PATH%;C:\Program Files (x86)\CMake 2.8\bin
set PATH=%PATH%;C:\Program Files (x86)\CMake 2.8\bin
set PATH=%PATH%;C:\Program Files (x86)\CMake 2.8\share
set PATH=%PATH%;c:\Program Files (x86)\Zeus
set PATH=%PATH%;D:\DOS\gnuwin32\bin

set FC=c:\MinGW64\bin\gfortran
set CC=c:\MinGW64\bin\gcc
set CXX=c:\MinGW64\bin\g++
set LIB=c:\MinGW64\lib
set INCLUDE=c:\MinGW64\include
set TEMP=d:\TEMP

:: now set CMAKE-related and build-related variables

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
set LDFLAGS="-flto"
set CPPFLAGS="-DgFortran"
set CFLAGS=-O2
 
set CMAKE_INCLUDE_PATH=%INCLUDE%
set CMAKE_LIBRARY_PATH=%LIB%

:: add --trace to see copious details re: CMAKE

cmake .. -G "MinGW Makefiles" -DPLATFORM_TYPE="x64" -DCMAKE_BUILD_TYPE="Release" -DCMAKE_INSTALL_PREFIX=install 
 -DCMAKE_MAKE_PROGRAM="c:/MinGW64/bin/make.exe" -DCMAKE_RANLIB:FILEPATH="c:/MinGW64/bin/ranlib.exe" 
 -DCMAKE_C_COMPILER:FILEPATH="c:/MinGW64/bin/gcc.exe" -DCMAKE_Fortran_COMPILER:FILEPATH="c:/MinGW64/bin/gfortran.exe" 
 -DCMAKE_Fortran_FLAGS="-O2"  
 ~~~ 