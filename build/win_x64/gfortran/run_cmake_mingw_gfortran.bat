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
set COMPILER_VERSION=4.9.3
set COMPILER_TRIPLET=x86_64-w64-mingw32
set LIB_PATH1=%COMPILER_DIR%/%COMPILER_TRIPLET%/lib
set LIB_PATH2=%COMPILER_DIR%/lib/gcc/%COMPILER_TRIPLET%/%COMPILER_VERSION%

set MAKE_EXECUTABLE_NAME=mingw32-make.exe
set Fortran_COMPILER_NAME=gfortran
set R_HOME=C:\Program Files\R\R-3.2.1\bin
set OMP_NUM_THREADS=8

:: define where 'make copy' will place executables
set INSTALL_PREFIX=d:/DOS

:: define other variables for use in the CMakeList.txt file
:: options are "Release", "Profile" or "Debug"
set BUILD_TYPE="Release"

:: options are "x86" (32-bit) or "x64" (64-bit)
set OS="win_x64"

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
set CMAKE_Fortran_FLAGS_RELEASE="-O2 -cpp -mtune=core2 -ffree-line-length-none -static -static-libgcc -static-libgfortran -DCURL_STATICLIB"
set CMAKE_Fortran_FLAGS_PROFILE="-O2 -pg -g -cpp -fno-omit-frame-pointer -DNDEBUG -fno-inline-functions -fno-inline-functions-called-once -fno-optimize-sibling-calls -ffree-line-length-none -static -static-libgcc -static-libgfortran -DCURL_STATICLIB"
::set CMAKE_Fortran_FLAGS_RELEASE="-O3 -mtune=native -fopenmp -flto -ffree-line-length-none -static-libgcc -static-libgfortran -DCURL_STATICLIB"

:: recreate clean Windows environment
set PATH=c:\windows;c:\windows\system32;c:\windows\system32\Wbem
set PATH=%PATH%;C:\Program Files (x86)\7-Zip
set PATH=%PATH%;%CMAKEROOT%\bin;%CMAKEROOT%\share
set PATH=%PATH%;C:\MinGW64\bin
set PATH=%PATH%;C:\Program Files (x86)\Git\bin
set PATH=%PATH%;C:\MinGW64\include;C:\MinGW64\lib

:: set a useful alias for make
echo %COMPILER_DIR%\bin\%MAKE_EXECUTABLE_NAME% %%1 > make.bat

:: not every installation will have these; I (SMW) find them useful
set PATH=%PATH%;c:\Program Files (x86)\Zeus
set PATH=%PATH%;D:\DOS\gnuwin32\bin

:: set compiler-specific link and compile flags
set LDFLAGS="-flto"
set CFLAGS="-DCURL_STATICLIB"
set CPPFLAGS="DgFortran -DCURL_STATICLIB"

set COMPILER_LIB_PATH1=%COMPILER_DIR%/lib/gcc/%COMPILER_TRIPLET%/%COMPILER_VERSION% 
set COMPILER_LIB_PATH2=%COMPILER_DIR%/%COMPILER_TRIPLET%/lib
set COMPILER_LIB_PATH3=%COMPILER_DIR%/lib

set CTEST_OUTPUT_ON_FAILURE=1


:: invoke CMake; add --trace to see copious details re: CMAKE
for %%f in ( "CodeBlocks - MinGW Makefiles" "MinGW Makefiles" ) do ^
cmake ..\..\.. -G %%f ^
-DCOMPILER_VERSION=%COMPILER_VERSION% ^
-DLIB_PATH1=%COMPILER_LIB_PATH1% ^
-DLIB_PATH2=%COMPILER_LIB_PATH2% ^
-DFortran_COMPILER_NAME=%Fortran_COMPILER_NAME% ^
-DOS=%OS% ^
-DCMAKE_BUILD_TYPE=%BUILD_TYPE% ^
-DCMAKE_INSTALL_PREFIX:PATH=%INSTALL_PREFIX% ^
-DCMAKE_MAKE_PROGRAM:FILEPATH=%COMPILER_DIR%\bin\%MAKE_EXECUTABLE_NAME% ^
-DCMAKE_RANLIB:FILEPATH=%COMPILER_DIR%\bin\ranlib.exe ^
-DCMAKE_C_COMPILER:FILEPATH=%COMPILER_DIR%\bin\%COMPILER_TRIPLET%-gcc.exe ^
-DCMAKE_Fortran_COMPILER:FILEPATH=%COMPILER_DIR%\bin\%COMPILER_TRIPLET%-gfortran.exe ^
-DTARGET__SWB_EXECUTABLE:BOOLEAN=%TARGET__SWB_EXECUTABLE% ^
-DTARGET__SWB_LIBRARY:BOOLEAN=%TARGET__SWB_LIBRARY% ^
-DTARGET__SWBSTATS:BOOLEAN=%TARGET__SWBSTATS% ^
-DOPTION__STREAM_INTERACTIONS=%OPTION__STREAM_INTERACTIONS% ^
-DOPTION__STRICT_DATE_CHECKING=%OPTION__STRICT_DATE_CHECKING% ^
-DOPTION__DEBUG_PRINT=%OPTION__DEBUG_PRINT% ^
-DCMAKE_Fortran_FLAGS_DEBUG=%CMAKE_Fortran_FLAGS_DEBUG% ^
-DCMAKE_Fortran_FLAGS_RELEASE=%CMAKE_Fortran_FLAGS_RELEASE% ^
-DCMAKE_Fortran_FLAGS_PROFILE=%CMAKE_Fortran_FLAGS_PROFILE%
