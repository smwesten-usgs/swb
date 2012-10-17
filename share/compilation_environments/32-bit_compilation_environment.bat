for /f "delims==" %%a in ('set') do set %%a=

set PATH=C:\MinGW32\bin
set PATH=%PATH%;C:\MinGW32\include;C:\MinGW32\lib
set PATH=%PATH%;C:\MinGW32\include\c++
::
set PATH=%PATH%;c:\windows;c:\windows\system32;c:\windows\system32\Wbem
set PATH=%PATH%;C:\Program Files (x86)\7-Zip
set PATH=%PATH%;C:\Program Files (x86)\CMake 2.8\bin
set PATH=%PATH%;C:\Program Files (x86)\CMake 2.8\bin
set PATH=%PATH%;C:\Program Files (x86)\CMake 2.8\share
set PATH=%PATH%;c:\Program Files (x86)\Zeus
set PATH=%PATH%;c:\Python26;c:\Python26\Lib;c:\Python26\Scripts;C:\Python26\include
set PATH=%PATH%;D:\DOS\gnuwin32\bin

set FC=c:\MinGW32\bin\gfortran
set CC=c:\MinGW32\bin\gcc
set LIB=c:\MinGW32\lib
set INCLUDE=c:\MinGW32\include

set CMDTXT=%CMDTXT% && set path=%PATH% && set FC=%FC% && set CC=%CC% && set LIB=%LIB% && set INCLUDE=%INCLUDE%
set CMDTXT=%CMDTXT% 
set CMDTXT=%CMDTXT% && set prompt=$m_$p$g && d: && cd \SMWData\Source_Code
set GDFONTPATH=C:\WINDOWS\FONTS

start "32-bit compilation environment" /SEPARATE cmd.exe \K %CMDTXT%
