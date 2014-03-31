# Compiling SWB #
### Compilation notes 21 March 2014 ###

These notes document the specific packages and process used to successfully compile SWB on a virtual Windows 7 (64-bit) platform. Although the operating system is 64-bit, the code is targeted for execution on a 32-bit machine. 

Basically, the process one uses to compile SWB on a new platform consists of the following steps:

1. Install the appropriate compilers and build software;
2. Open up a clean command prompt; navigate to the appropriate build subdirectory;
3. Run a batch file that sets system-specific values and executes the *CMake* program;
4. CMake further examines the system and writes out a *makefile* that will be used to build SWB;
5. Run *'make'* to compile the SWB modules, PROJ4 components, and link to the required libraries. 

Once steps 1 through 4 have been completed successfully, they should not need to be repeated unless new libraries, compilers or or support software are installed on the system.


###Install the required compilers and support software###

At a minimum, a MinGW compiler package, Git version control software, and CMake package build software must be presesnt on the target system. In this example the specific software sources were as follows:

__32-bit gcc/gfortran compilers__ ( install to c:\mingw32 )

[i686-4.8.2-release-posix-sjlj-rt_v3-rev2.7z](http://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/4.8.2/threads-posix/sjlj/i686-4.8.2-release-posix-sjlj-rt_v3-rev2.7z/download)

__64-bit gcc/gfortran compilers__ ( install to c:\mingw64 )

[x86_64-4.8.2-release-win32-sjlj-rt_v3-rev2.7z](http://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/4.8.2/threads-win32/sjlj/)

__CMake__ ( install to c:\Program Files (x86)\CMake 2.8 )

[cmake-2.8.12.2-win32-x86.exe](http://www.cmake.org/files/v2.8/)

__Git__ ( install to c:\Program Files (x86)\Git )

[Git-1.9.0-preview20140217.exe](http://msysgit.googlecode.com/files/Git-1.9.0-preview20140217.exe)

__R__ ( install to c:\Program Files\R )

[R-3.0.3-win.exe](http://cran.r-project.org/bin/windows/base/R-3.0.3-win.exe)

###Establish a clean compilation environment###

I have a little DOS batch file that I use to create a clean environment for compilation of SWB. Basically this batch file eliminates all existing environment variables, then replaces only the specific path and variable values needed to compile SWB. This seems to help CMake find the proper file locations during the makefile generation process.

__You must modify this batch file to make it reflect the configuration of your system. Specifically, make sure that you change the *MINGW*, *CMAKE*, *GIT*, *TEMP*, *SOURCE_CODE_LOC*, and *SOURCE_CODE_DRIVE_LETTER* variables in the file below.__

######DOS batch file for establishing a clean 32-bit compilation environment

```
:: The command below removes any existing environment variables
for /f "delims==" %%a in ('set') do set %%a=

:: Modify the value below to reflect the location of MinGW on your system
set MINGW=c:\MinGW32

:: Modify the value below to reflect the location of CMake on your system
set CMAKE=c:\Program Files (x86)\CMake 2.8

:: Modify the value below to reflect the location of Git on your system
set GIT=c:\Program Files (x86)\Git

:: Set the location for the TEMP directory
:: *** THIS IS CRITICAL!!! ***
:: If this directory does not exist or is not writable, gfortran may crash during
:: the CMake build process
set TEMP=c:\TEMP

:: Set the location where the command prompt should open to.
set SOURCE_CODE_LOC=\Users\smwesten\Source_Code
set SOURCE_CODE_DRIVE_LETTER=c:

:: Now add the MinGW bin, include, and lib subdirectories to path
set PATH=%MINGW%\bin
set PATH=%PATH%;%MINGW%\include
set PATH=%PATH%;%MINGW%\lib

:: I often place useful command line files in a folder called 'DOS'
set PATH=%PATH%;c:\Users\smwesten\DOS

:: Add in path to Windows system files
set PATH=%PATH%;c:\windows;c:\windows\system32;c:\windows\system32\Wbem

:: Add 7-zip to path. Not critical, but 7-zip is often handy for zipping and 
:: unzipping file archives.
set PATH=%PATH%;C:\Program Files\7-Zip

:: Add CMake subdirectories to path
set PATH=%PATH%;%CMAKE%\bin
set PATH=%PATH%;%CMAKE%\share
set PATH=%PATH%;%GIT%\bin;.

:: The FC, CC, LIB, and INCLUDE environment variables are othen used in configuration scripts
:: to identify compiler and library locations
set FC=%MINGW%\bin\gfortran
set CC=%MINGW%\bin\gcc
set LIB=%MINGW%\lib
set INCLUDE=%MINGW%\include

set GDFONTPATH=C:\WINDOWS\FONTS

start "32-bit MinGW compilation environment" cmd.exe /K "set prompt=$m_$p$g && %SOURCE_CODE_DRIVE_LETTER% && cd %SOURCE_CODE_LOC%"
```
### Obtain a fresh copy of SWB ###

Run the program "Git bash" (In Windows: Start, Git, Git bash). At the resulting command prompt, type:
```git clone https://github.com/smwesten-usgs/swb.git```

After allowing this command to finish, you should now have a complete and up-to-date copy of the Git repository that contains the SWB source_code. If you examine the structure and contents of the SWB directory, you should see something like that shown below.

![][image]

[image]: images/SWB_subdirectories.png

The most significant of these subdirectories are the *build*, *cmake*, *lib*, *include*, and *src* subdirectories. A brief description of the contents of the SWB subdirectories is given below.

Subdirectory Name | Contains
------------------|-----------------------------------------------
bin               | archived compiled SWB binaries
build             | batch files designed to run CMake; organized by platform and compiler
cmake             | several CMake macros designed to locate the libraries that SWB relies on
dist              | archival version of a SWB install program
doc               | copy of the USGS techniques and methods report that documents the code
eclipse_projects  | old Eclipse projects aimed at allowing SWB to be edited with the Photran package within Eclipse
include           | Fortran module (*.mod) files required to link SWB to external libraries
lib               | binary static library objects required for linking to SWB; organized by platform and compiler
presentations     | old presentations (PowerPoint) describing SWB use and capabilities
share             | R scripts, Python scripts and other goodies developed for use with SWB
src               | SWB source code directory
test_case         | a generic test case for use with SWB
tests             | tests targeting specific functionality of SWB; integrated into CMake's CTest package, and designed to be run by issuing the command *'make test'*
webpages          | html and supporting files in support of the webpage [http://wi.water.usgs.gov/Soil_Water_Balance/](http://wi.water.usgs.gov/Soil_Water_Balance/)

The directory structure is set up to easily perform an *"out-of-source"* build. In other words, the source code for SWB lives in the *src* subdirectory. The only files that belong in the *src* subdirectory are the Fortran or C source code and a file called *CMakeFiles.txt*. 

When building SWB, a copy of the source code is moved into the appropriate *build* subdirectory, CMake is run, many ancillary subdirectories are created, and a makefile is generated. When this makefile is executed, it runs in the *build* subdirectory and not in the *source* directory. Thus the build is *"out-of-source"*.

The appropriate build subdirectories for various system configurations is shown below:

System / OS        | Compiler        | Proper build subdirectory
-------------------|-----------------|----------------------
Macintosh OSX      | gfortran / gcc  | swb/build/mac_osx/gfortran
Linux Mint         | gfortran / gcc  | swb/build/linux_x64/gfortran
Windows (32-bit)   | gfortran / gcc  | swb/build/win_x86/gfortran
Windows (64-bit)   | gfortran / gcc  | swb/build/win_x64/gfortran

__*NOTE: The repository contains build subdirectories for Intel Fortran as well. As of this writing (March 2014), the only Intel Fortran build that has been successful is on a Linux platform. Now that Unidata (authors of NetCDF) is officially supporting Windows with binaries of the NetCDF libraries, it may be a lot easier to compile SWB on a Windows platform than in the past. However, the scripts that are currently in the ifort subdirectories are almost guaranteed to fail.*__

### Recompile module files with your local compiler###
This step is not always needed. However, there have been cases recently where the *gfortran* compiler internals have changed enough to render the module files between versions incompatible.

Below is an example showing how the module file would be recompiled, assuming that we are using a 32-bit MinGW gfortran compiler. *NOTE: example shown on a Macintosh, but the syntax is the same in Windows: only the subdirectory name would change (mac_osx => win_x86).*

![][image2]

[image2]: images/Rebuild_mod_files.png

### Modify batch file with local pathnames and software versions ###
Before one can successfully build SWB, the batch file used to configure CMake must be customized to take account of the specific software versions and pathnames present on the local system.

If one is building for a 32-bit system (win_x86) under MinGW gfortran, the first thing to do is to navigate to the appropriate build subdirectory: _swb/build/win_x86/gfortran_.

If the compilers are gfortran/gcc, it is easy to find the version of the software by typing *'gcc - v'* at the command prompt:

![][image3]

[image3]: images/gcc_minus_v.png

The compiler will spit out a lot of details, but one piece of really useful information about the compiler is located in the third or fourth line of output (circled in white, above), and labeled *"target"*. The target is also referred to as a compiler triplet. The other useful bit of information is shown at the bottom of the figure: the version number. All of the gcc-related libraries are contained in a version-specific directory located under the main MinGW directory. In this case the version number is __4.8.2__.

#### Example of a failed CMake run ####

An example of why the batchfile will likely need to be modified is given here. The batchfile was written with gfortran version 4.8.1 in mind, however, on the test machine this document was written on, version 4.8.2 is present. When the batchfile is run, CMake is invoked and it attempts to find the required libraries and compilers. However, in this case it fails to find the compilers, along with the R statistical programming language:

![][image4]

[image4]: images/cmake_failure.png

Examination of the batchfile, *run_cmake_mingw_gfortran.bat*, shows that the batchfile indeed expects to find R version 3.0.1 and gfortran version 4.8.1.

![][image5]

[image5]: images/Faulty_batchfile_cmake.png

This file can now be updated so that _COMPILER_VERSION=4.8.2_, and _R_HOME=c:\Program Files\R\R-3.0.3_. This should make the batchfile functional again on this particular machine.

#### Example of a successful CMake run ####

With the changes made as discussed above, the *run_cmake_mingw_gfortran.bat* batchfile is again run, with a successful outcome. The CMake output is given below. Note that there are no instances where libraries or executables are reported as __NOTFOUND__. 

![][image6]

[image6]: images/success_cmake.png

### Build SWB ###

A successful run of the CMake program will result in a new *makefile* being written to the directory in which the batchfile was run. At this point, it should be possible to build SWB by invoking the *make* program: simply type *make* at the command line. It should not be necessary to rerun the batchfile and CMake again unless the underlying libraries are updated.
