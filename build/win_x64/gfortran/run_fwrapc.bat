del /Q /S /F D:\SMWData\Source_Code\SWB\build\win_x64\gfortran\src\tmp\*.*

::gcc -c D:\SMWData\Source_Code\SWB_SVN\branches\development\src\swb_python_interface.F90

::copy /Y D:\SMWData\Source_Code\SWB_SVN\branches\development\src\swb_python_interface.F90 ^
::D:\SMWData\Source_Code\SWB_SVN\branches\development\build\win32\src\swb_python_interface.F90

::cd src

:: gcc -c swb_python_interface.F90

set CC=c:/mingw64/bin/gcc.exe
set CXX=c:/mingw64/bin/g++.exe
set FC=c:/mingw64/bin/gfortran.exe

set CFLAGS="-O2 -Wall -Wstrict-prototypes"
set F90FLAGS="-O2 -ID:\SMWData\Source_Code\SWB\build\win_x64\gfortran\src"
f2py --build-dir .\tmp -m pyswb ^
-LD:\SMWData\Source_Code\SWB\build\win_x64\gfortran\src ^
-LD:\SMWData\Source_Code\SWB\lib\win_x64\gfortran ^
-Lc:\MinGW64\lib ^
-lgfortran ^
-lswblib ^
--fcompiler=gnu95 ^
--f90exec=c:\MinGW64\bin\gfortran.exe ^
--compiler=mingw32 ^
-c pyswb.pyf ^
-Ic:\MinGW64\include ^
-ID:\SMWData\Source_Code\SWB\build\win_x64\gfortran\src
