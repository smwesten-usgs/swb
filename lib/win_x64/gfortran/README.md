NETCDF library:
---------------

* compiled from NetCDF version 3.6.3 source code obtained from the
  Unidata website.

* v2i.c: part of libsrc directory; modified by adding header file "nc.h"
  to the list of included files.

* compiled from within a working MSYS installation (part of GRASS GIS)
  on a Windows 7, 64-bit machine

* gcc toolchain was obtained from the MinGW-w64 project:
  http://mingw-w64.sourceforge.net/

* gcc toolchain version information:

_d:\SMWData\Source_Code\swb\build\win_x64\gfortran> gfortran -v
Using built-in specs.
COLLECT_GCC=gfortran
COLLECT_LTO_WRAPPER=c:/mingw64/bin/../libexec/gcc/x86_64-w64-mingw32/4.7.1/lto-wrapper.exe
Target: x86_64-w64-mingw32
Configured with: /home/ruben/mingw-w64/src/gcc/configure --host=x86_64-w64-mingw32 --build=x86_64-linux-gnu --target=x86_64-w64-mingw32 --with-sysroot=/hom
e/ruben/mingw-w64/mingw64mingw64/mingw64 --prefix=/home/ruben/mingw-w64/mingw64mingw64/mingw64 --with-gmp=/home/ruben/mingw-w64/prereq/x86_64-w64-mingw32/i
nstall --with-mpfr=/home/ruben/mingw-w64/prereq/x86_64-w64-mingw32/install --with-mpc=/home/ruben/mingw-w64/prereq/x86_64-w64-mingw32/install --with-ppl=/h
ome/ruben/mingw-w64/prereq/x86_64-w64-mingw32/install --with-cloog=/home/ruben/mingw-w64/prereq/x86_64-w64-mingw32/install --disable-ppl-version-check --di
sable-cloog-version-check --enable-cloog-backend=isl --with-host-libstdcxx='-static -lstdc++ -lm' --enable-shared --enable-static --enable-threads=win32 --
enable-plugins --disable-multilib --enable-languages=c,lto,c++,objc,obj-c++,fortran,java --enable-libgomp --enable-fully-dynamic-string --enable-libstdcxx-
time --disable-nls --disable-werror --enable-checking=release --with-gnu-as --with-gnu-ld --disable-win32-registry --disable-rpath --disable-werror --with-
libiconv-prefix=/home/ruben/mingw-w64/prereq/x86_64-w64-mingw32/install --with-pkgversion=rubenvb-4.7.1-2-release --with-bugurl=mingw-w64-public@lists.sour
ceforge.net CC= CFLAGS='-O2 -march=nocona -mtune=core2 -fomit-frame-pointer -momit-leaf-frame-pointer' LDFLAGS=
Thread model: win32
gcc version 4.7.1 (rubenvb-4.7.1-2-release)

* NetCDF configured with the following script:

export FC=c:/MinGW64/bin/gfortran.exe
export F90=c:/MinGW64/bin/gfortran.exe
export CC=c:/MinGW64/bin/gcc.exe
export CXX=c:/MinGW64/bin/g++.exe
export AR=c:/MinGW64/bin/ar.exe
export NM=c:/MinGW64/bin/nm.exe
export LD=c:/MinGW64/bin/ld.exe
export STRIP=C:/MinGW64/bin/strip.exe
export RANLIB=c:/MinGW64/bin/ranlib.exe
export CPPFLAGS=-DgFortran
export LIBRARY_PATH=c:/MinGW64/lib:d:/smwdata/source_code_netcdf-3.6.3/
./configure --disable-examples \
--prefix="/d/smwdata/source_code/netcdf-3.6.3/netlib64" \
--build=x86_64-w64-mingw32

* cygpath.exe and cygwin1.dll needed to be copied into the main MinGW\bin
  directory in order to allow the make to occur

* the definition given for CPPFLAGS is **crucial** in getting the library to link with swb later on

* CPPFLAGS should apparently be set to -DpgiFortran for Intel Fortran