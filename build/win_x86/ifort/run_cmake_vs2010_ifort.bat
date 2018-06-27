:: cmake -G "MinGW Makefiles" -DCMAKE_MAKE_PROGRAM:FILEPATH=c:/MinGW32/bin/make.exe ^
:: -DCMAKE_C_COMPILER:FILEPATH=c:/MinGW32/bin/gcc.exe ^
:: -DCMAKE_FORTRAN_COMPILER:FILEPATH=c:/MinGW32/bin/gfortran.exe  ..\..
cmake -G "NMake Makefiles" -DCMAKE_INSTALL_PREFIX:PATH="D:/DOS" -DCMAKE_LINKER="C:/Program Files (x86)/Intel/Composer XE 2011 SP1/bin/ia32/ifort.exe" ../..