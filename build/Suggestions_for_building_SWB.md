#Requirements for Building SWB#

#  #

##CMake##

 CMake is a cross-platform development tool that makes it easier to build software on multiple platforms. Within the build subdirectory are scripts that can help compile SWB for Macintosh, Linux, or Windows. CTest is also used to automate several tests; it is recommended that you run these tests ("make test" from the command line) each time major changes have been made to the code. Cmake may be obtained here: [http://www.cmake.org/](http://www.cmake.org/)

##MinGW (Windows only)##

'Minimalist GNU for Windows' is a development environment for developing native Microsoft Windows applications. MinGW may be installed by downloading and unzipping by downloading this file:

[http://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/4.8.2/threads-posix/seh/x86_64-4.8.2-release-posix-seh-rt_v3-rev0.7z](http://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/4.8.2/threads-posix/seh/x86_64-4.8.2-release-posix-seh-rt_v3-rev0.7z)

The build script currently expects to find the 64-bit version of MinGW in c:\MinGW64. It doesn't really matter where the MinGW files are placed, so long as the build script is updated accordingly.

##R##

R is a statistical programming language. R is needed in order to perform some of the automated tests on SWB output each time SWB is rebuilt. R may be obtained here: [http://cran.r-project.org/bin/](http://cran.r-project.org/bin/)