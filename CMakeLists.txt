# CMake project for swb

# based in part on an example found at:
# http://lagrange.mechse.illinois.edu/mwest/partmc/partmc-1.2.0/CMakeLists.txt

message("Processing top-level CMakelists.txt for project swb")

#------------------------------------------------------------------------
# Set basic project settings
#------------------------------------------------------------------------
project(swb Fortran)
set(PACKAGE_BUGREPORT "smwesten@usgs.gov")
set(PACKAGE_NAME "swb")
set(PACKAGE_STRING "swb 1.0")
set(PACKAGE_TARNAME "swb")
set(PACKAGE_VERSION "1.0")

cmake_minimum_required(VERSION 2.6)

#------------------------------------------------------------------------
# Restrict CMAKE_BUILD_TYPE to "Release" or "Debug"
#------------------------------------------------------------------------
set( CMAKE_BUILD_TYPE "Release" CACHE STRING
       "Compile in DEBUG or RELEASE mode" )

#------------------------------------------------------------------------
# What Fortran compiler are we using?
#------------------------------------------------------------------------
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

#------------------------------------------------------------------------
# Set PREPROCESSOR DEFINES for conditional compilation
#------------------------------------------------------------------------
option (OPTION__GRAPHICS_SUPPORT
      "Compile the code with graphics (DISLIN) support?" ON)
if(OPTION__GRAPHICS_SUPPORT)
  set ( PREPROCESSOR_DEFINES ${PREPROCESSOR_DEFINES} "GRAPHICS_SUPPORT")
endif()

option (OPTION__STREAM_INTERACTIONS
      "Compile the code with stream interactions support?" OFF)
if(OPTION__STREAM_INTERACTIONS)
  set ( PREPROCESSOR_DEFINES ${PREPROCESSOR_DEFINES} "STREAM_INTERACTIONS")
endif()

option (OPTION__NETCDF_SUPPORT
      "Compile the code with NETCDF support?" ON)
if(OPTION__NETCDF_SUPPORT)
  set ( PREPROCESSOR_DEFINES ${PREPROCESSOR_DEFINES}  "NETCDF_SUPPORT")
endif()

option (OPTION__DEBUG_PRINT
      "Compile the code with extra debug print options enabled?" OFF )
if(OPTION__DEBUG_PRINT)
  set ( PREPROCESSOR_DEFINES ${PREPROCESSOR_DEFINES}  "DEBUG_PRINT")
endif()

#------------------------------------------------------------------------
# Set platform-specific options
#------------------------------------------------------------------------
if(CMAKE_HOST_WIN32)

   if (Fortran_COMPILER_NAME MATCHES "gfortran[.a-z]*")
     #------------------------------------------------------------------------
     # Set compilation and link preferences for GFORTRAN
     #------------------------------------------------------------------------
     set (Fortran_FLAGS_RELEASE "-Ofast -ffree-form -ffree-line-length-none -x f95-cpp-input")
     set (Fortran_FLAGS_DEBUG "-O0 -g -Wall -ffree-form -ffree-line-length-none -x f95-cpp-input")
     set(CMAKE_FIND_LIBRARY_PREFIXES lib)
     set(CMAKE_FIND_LIBRARY_SUFFIXES ".a; .so")

     set_property(CACHE CMAKE_BUILD_TYPE
             PROPERTY STRINGS "Release" "Debug")


     find_program(CMAKE_MAKE_PROGRAM make make.exe
       DOC "Find a suitable make program for building under Windows/MinGW"
       HINTS
       C:/gfortran_32/bin
       C:/MinGW_MSYS/msys/1.0/bin
       C:/MinGW/bin
       D:/MinGW/bin)
       message("CMAKE_MAKE_PROGRAM: " ${CMAKE_MAKE_PROGRAM})

     find_program(CMAKE_SH sh cmd.exe
       DOC "Find a suitable shell for building under Windows/MinGW"
       HINTS
       C:/Windows/System32)
       message("CMAKE_SH: " ${CMAKE_SH})

     find_path(INCLUDE_DIR netcdf.mod NETCDF.mod
       DOC "Include directory (must contain netcdf.mod)"
       HINTS
       ${CMAKE_CURRENT_SOURCE_DIR}/include/win32/gfortran_mingw
       PATH_SUFFIXES
       include/win32/gfortran_mingw)
       message("Using include directory: " ${INCLUDE_DIR})

     find_library(NETCDF_FORTRAN_LIB
       NAMES libnetcdf.a netcdf NETCDF
       DOC "NetCDF Fortran library"
       HINTS
       ${CMAKE_CURRENT_SOURCE_DIR}/lib/win32/gfortran_mingw
       PATH_SUFFIXES
       lib/win32/gfortran_mingw)

     find_library(DISLIN_FORTRAN_LIB
       NAMES disgf.a
       DOC "DISLIN Fortran library"
       HINTS
       ${CMAKE_CURRENT_SOURCE_DIR}/lib/win32/gfortran_mingw
       PATH_SUFFIXES
       lib/win32/gfortran_mingw)

     find_library(GCC_LIB
       NAMES libgcc.a gcc
       DOC "GCC library"
       HINTS
       C:/gfortran_32/lib/gcc/i586-pc-mingw32/4.6.0 )

     find_library(GFORTRAN_LIB
       NAMES gfortran libgfortran.a
       DOC "GFORTRAN library"
       HINTS
       C:/gfortran_32/lib )

      find_library(USER32_LIB
        NAMES user32 libuser32.a
        DOC "MinGW USER32 library"
        HINTS
        C:/gfortran_32/lib )

      find_library(GDI32_LIB
        NAMES gdi32 libgdi32.a
        DOC "MinGW USER32 library"
        HINTS
        C:/gfortran_32/lib )

     set(EXTRA_LIBS ${EXTRA_LIBS} ${NETCDF_FORTRAN_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${DISLIN_FORTRAN_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${GCC_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${GFORTRAN_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${USER32_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${GDI32_LIB} )

     SET(CMAKE_INSTALL_PREFIX "d:/dos")
     add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swb.exe D:/dos/swb.exe
           COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbread.exe D:/dos/swbread.exe
           COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbstats.exe D:/dos/swbstats.exe )
   endif ()

elseif(CMAKE_HOST_UNIX)

   if (Fortran_COMPILER_NAME MATCHES "ifort[.a-z]*")
     #------------------------------------------------------------------------
     # Set compilation and link preferences for Intel Fortran on Linux
     #------------------------------------------------------------------------
     set (Fortran_FLAGS_RELEASE "-fast -fPIC")
     set (Fortran_FLAGS_DEBUG "-O0 ")
     set(CMAKE_FIND_LIBRARY_PREFIXES lib)
     set(CMAKE_FIND_LIBRARY_SUFFIXES ".a; .so")

     find_program(CMAKE_MAKE_PROGRAM make
       DOC "Find a suitable make program for building under Linux/IFORT"
       HINTS
       /bin
       /usr/bin)
       message("CMAKE_MAKE_PROGRAM: " ${CMAKE_MAKE_PROGRAM})

     find_program(CMAKE_SH sh
       DOC "Find a suitable shell for building  under Linux/IFORT"
       HINTS
       /bin)
       message("CMAKE_SH: " ${CMAKE_SH})

     find_path(INCLUDE_DIR netcdf.mod NETCDF.mod
       DOC "Include directory (must contain netcdf.mod)"
       HINTS
       ${CMAKE_CURRENT_SOURCE_DIR}/include/ubuntu_x64/ifort
       PATH_SUFFIXES
       ${CMAKE_CURRENT_SOURCE_DIR}/include/ubuntu_x64/ifort)
       message("Using include directory: " ${INCLUDE_DIR})

     find_library(NETCDF_FORTRAN_LIB
       NAMES libnetcdf.a netcdf NETCDF
       DOC "NetCDF Fortran library"
       HINTS
       ${CMAKE_CURRENT_SOURCE_DIR}/lib/ubuntu_x64/ifort
       PATH_SUFFIXES
       lib/ubuntu_x64/ifort)

     find_library(HDF5_HL_FORTRAN_LIB
       NAMES libhdf5_hl.a hdf5_hl
       DOC "hdf5_hl library"
       HINTS
       ${CMAKE_CURRENT_SOURCE_DIR}/lib/ubuntu_x64/ifort
       PATH_SUFFIXES
       lib/ubuntu_x64/ifort)

     find_library(HDF5_FORTRAN_LIB
       NAMES libhdf5.a hdf5
       DOC "hdf5 library"
       HINTS
       ${CMAKE_CURRENT_SOURCE_DIR}/lib/ubuntu_x64/ifort
       PATH_SUFFIXES
       lib/ubuntu_x64/ifort)

     find_library(Z_LIB
       NAMES libz.a z
       DOC "z library"
       HINTS
       ${CMAKE_CURRENT_SOURCE_DIR}/lib/ubuntu_x64/ifort
       PATH_SUFFIXES
       lib/ubuntu_x64/ifort)

     find_library(M_LIB
       NAMES libm.a m
       DOC "m library"
       HINTS
       ${CMAKE_CURRENT_SOURCE_DIR}/lib/ubuntu_x64/ifort
       PATH_SUFFIXES
       lib/ubuntu_x64/ifort)

     find_library(SZ_LIB
       NAMES libsz.a sz
       DOC "sz library"
       HINTS
       ${CMAKE_CURRENT_SOURCE_DIR}/lib/ubuntu_x64/ifort
       PATH_SUFFIXES
       lib/ubuntu_x64/ifort)

     find_library(OPEN_MOTIF_LIB
       NAMES libXm.so.3 Xm
       DOC "Open Motif library"
       HINTS
       /usr/lib
       lib/ubuntu_x64/ifort)

     find_library(DISLIN_FORTRAN_LIB
       NAMES dislin-10.0.a dislin-10.0
       DOC "DISLIN Fortran library"
       HINTS
       ${CMAKE_CURRENT_SOURCE_DIR}/lib/ubuntu_x64/ifort
       PATH_SUFFIXES
       lib/ubuntu_x64/ifort)

     find_library(GCC_LIB
       NAMES libgcc.a gcc
       DOC "gcc library"
       HINTS
       /usr/lib
       /usr/lib/gcc
       /usr/lib/gcc/x86_64-linux-gnu/4.4)

     set(EXTRA_LIBS ${EXTRA_LIBS} ${NETCDF_FORTRAN_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${DISLIN_FORTRAN_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${GCC_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${SZ_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${Z_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${M_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${HDF5_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${HDF5_HL_LIB} )
	 set(EXTRA_LIBS ${EXTRA_LIBS} ${OPEN_MOTIF_LIB} )

     SET(CMAKE_INSTALL_PREFIX "/usr/local/bin")
     add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swb /usr/local/bin/swb
           COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbread /usr/local/bin/swbread
           COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbstats /usr/local/bin/swbstats )

   endif (Fortran_COMPILER_NAME MATCHES "ifort[.a-z]*")

endif()

if( CMAKE_BUILD_TYPE STREQUAL "Debug")
  add_definitions(${Fortran_FLAGS_DEBUG})
else()
  set(CMAKE_BUILD_TYPE "Release")
  add_definitions(${Fortran_FLAGS_RELEASE})
endif()

message("--------------------------------------------------------------------")
message("BUILD TYPE: " ${CMAKE_BUILD_TYPE})
message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
message ("Linker:   " ${CMAKE_Fortran_LINK_EXECUTABLE})
message ("CMAKE_AR: " ${CMAKE_AR})
message ("Fortran flags (Debug):      " ${Fortran_FLAGS_DEBUG})
message ("Fortran flags (Release):    " ${Fortran_FLAGS_RELEASE})
MESSAGE ("Install to directory:       " ${CMAKE_INSTALL_PREFIX})
message("Using NetCDF library: " ${NETCDF_FORTRAN_LIB})
message("Using DISLIN library: " ${DISLIN_FORTRAN_LIB})
message("CMAKE_BINARY_DIR: " ${CMAKE_BINARY_DIR})
message("CMAKE_CURRENT_SOURCE_DIR: " ${CMAKE_CURRENT_SOURCE_DIR})
message("PROJECT_SOURCE_DIR: " ${PROJECT_SOURCE_DIR})
message("--------------------------------------------------------------------")

include_directories(${INCLUDE_DIR})
set(EXTRA_LIBS ${EXTRA_LIBS} ${NETCDF_LIBS} ${DISLIN_LIBS})
link_libraries( ${EXTRA_LIBS} )

get_filename_component(COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

######################################################################

add_subdirectory(src)
