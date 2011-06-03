del /Q /S /F D:\SMWData\Source_Code\SWB\build\win32\src\tmp\*.*

::gcc -c D:\SMWData\Source_Code\SWB_SVN\branches\development\src\swb_python_interface.F90

::copy /Y D:\SMWData\Source_Code\SWB_SVN\branches\development\src\swb_python_interface.F90 ^
::D:\SMWData\Source_Code\SWB_SVN\branches\development\build\win32\src\swb_python_interface.F90

::cd src

:: gcc -c swb_python_interface.F90

set CFLAGS="-O2 -Wall -Wstrict-prototypes"
   f2py --build-dir .\tmp -m pyswb ^
   -LD:\SMWData\Source_Code\SWB\build\win32\src ^
   -Lc:\MinGW-32\lib ^
   --fcompiler=gnu95 ^
   --f90exec=c:\MinGW-32\bin\gfortran.exe ^
   --compiler=mingw32 -c pyswb.pyf -lswb ^
   -Ic:\MinGW-32\include ^
   -ID:\SMWData\Source_Code\SWB\build\win32\src
