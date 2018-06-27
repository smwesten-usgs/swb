for /f "delims==" %%a in ('set') do set %%a=

set PATH=C:\MinGW64\bin
set PATH=%PATH%;C:\MinGW64\include;C:\MinGW64\lib
::
set PATH=%PATH%;c:\windows;c:\windows\system32;c:\windows\system32\Wbem
set PATH=%PATH%;C:\Program Files (x86)\7-Zip
set PATH=%PATH%;C:\Program Files (x86)\CMake 2.8\bin
set PATH=%PATH%;C:\Program Files (x86)\CMake 2.8\bin
set PATH=%PATH%;C:\Program Files (x86)\CMake 2.8\share
set PATH=%PATH%;c:\Program Files (x86)\Zeus
set PATH=%PATH%;D:\DOS\gnuwin32\bin
set PATH=%PATH%;d:\saga_2.0.8;d:\saga_2.0.8\modules;d:\saga_2.0.8\dll;.

set FC=c:\MinGW64\bin\gfortran
set CC=c:\MinGW64\bin\gcc
set LIB=c:\MinGW64\lib
set INCLUDE=c:\MinGW64\include
set TEMP=d:\TEMP

set CMDTXT=set SAGA=d:\saga_2.0.8 && set SAGA_MLB=d:\saga_2.0.8\modules
set CMDTXT=%CMDTXT% && set path=%PATH% && set FC=%FC% && set CC=%CC% && set LIB=%LIB% && set INCLUDE=%INCLUDE%
set CMDTXT=%CMDTXT% && set TEMP=%TEMP%
set CMDTXT=%CMDTXT% && set prompt=$m_$p$g && d: && cd \SMWData\Source_Code
set GDFONTPATH=C:\WINDOWS\FONTS

start "64-bit MinGW compilation environment" /SEPARATE cmd.exe \K %CMDTXT%
