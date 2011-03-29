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

#
option (MAKEFILE_VERBOSE
      "Produce verbose makefiles?" OFF)

#------------------------------------------------------------------------
# Restrict CMAKE_BUILD_TYPE to "Release" or "Debug"
#------------------------------------------------------------------------
set( CMAKE_BUILD_TYPE "Release" CACHE STRING
       "Compile in DEBUG or RELEASE mode" )

#------------------------------------------------------------------------
# What Fortran compiler are we using?
#------------------------------------------------------------------------
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

option (TARGET__SWB_EXECUTABLE
      "Compile the main SWB executable?" ON)

option (TARGET__SWB_LIBRARY
      "Compile SWB as a library?" ON)

option (TARGET__SWBREAD
      "Compile the swbread application?" ON)

option (TARGET__SWBSTATS
      "Compile the swbstats application?" ON)

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
  set(STREAM_INTERACTIONS_VARS 1)
endif()

option (OPTION__NETCDF_SUPPORT
      "Compile the code with NETCDF support?" ON)
if(OPTION__NETCDF_SUPPORT)
  set ( PREPROCESSOR_DEFINES ${PREPROCESSOR_DEFINES}  "NETCDF_SUPPORT")
endif()

option (OPTION__THORNTHWAITE_MATHER_TABLES
      "Use T-M tables (if not use T-M equations)?" ON)
if(OPTION__THORNTHWAITE_MATHER_TABLES)
  set ( PREPROCESSOR_DEFINES ${PREPROCESSOR_DEFINES}  "TM_TABLES")
endif()

option (OPTION__IRRIGATION_MODULE
      "Compile the code with irrigation module support?" OFF)
if(OPTION__IRRIGATION_MODULE)
  set ( PREPROCESSOR_DEFINES ${PREPROCESSOR_DEFINES} "IRRIGATION_MODULE")
  set(IRRIGATION_MODULE_VARS 3)
endif()

option (OPTION__DEBUG_PRINT
      "Compile the code with extra debug print options enabled?" OFF )
if(OPTION__DEBUG_PRINT)
  set ( PREPROCESSOR_DEFINES ${PREPROCESSOR_DEFINES}  "DEBUG_PRINT")
endif()

#math(EXPR TOTAL_T_STATS_VARS "(26 + ${STREAM_INTERACTIONS_VARS} + ${IRRIGATION_MODULE_VARS})")
#set ( PREPROCESSOR_DEFINES ${PREPROCESSOR_DEFINES} "CPP_NUM_VARS=${TOTAL_T_STATS_VARS}")
#message("Setting dimension for number of T_STATS elements to: " ${TOTAL_T_STATS_VARS})

#------------------------------------------------------------------------
# Set platform-specific options
#------------------------------------------------------------------------
if(CMAKE_HOST_WIN32)

   if (Fortran_COMPILER_NAME MATCHES "gfortran[.a-z]*")


   set(CMAKE_MAKE_PROGRAM "c:/gfortran_32/bin/make.exe")
   set(CMAKE_LINKER "c:/gfortran_32/bin/ld.exe")
   set(CMAKE_AR "c:/gfortran_32/bin/ar.exe")
     #------------------------------------------------------------------------
     # Set compilation and link preferences for GFORTRAN
     #------------------------------------------------------------------------
#     set (Fortran_FLAGS_RELEASE "-Ofast -ffree-form -ffree-line-length-none -cpp")

     set (Fortran_FLAGS_RELEASE " -Ofast -ffree-line-length-none -static-libgfortran -static -static-libgcc")

#     set (Fortran_FLAGS_DEBUG " -O0 -g -Wall -ffpe-trap=invalid,underflow,overflow,zero -ffree-form -ffree-line-length-none -cpp -fbacktrace -static-libgfortran -static -static-libgcc")
     set (Fortran_FLAGS_DEBUG " -O0 -g -Wall -fbounds-check -frange-check -ffpe-trap=zero -ffree-form -ffree-line-length-none -cpp -fbacktrace -static-libgfortran -static -static-libgcc")

     set(CMAKE_EXE_LINKER_FLAGS "")


     set(CMAKE_FIND_LIBRARY_PREFIXES lib)
     set(CMAKE_FIND_LIBRARY_SUFFIXES ".a; .so")

     set_property(CACHE CMAKE_BUILD_TYPE
             PROPERTY STRINGS "Release" "Debug")

#     set (CMAKE_PREFIX_PATH "C:/gfortran_32")

#     find_program(CMAKE_MAKE_PROGRAM make make.exe
#       DOC "Find a suitable make program for building under Windows/MinGW"
#       PATHS
#       C:/gfortran_32/bin
#       C:/MinGW/bin
#       D:/MinGW/bin
#       NO_DEFAULT_PATH)
##       NO_SYSTEM_ENVIRONMENT_PATH)
      message("CMAKE_MAKE_PROGRAM: " ${CMAKE_MAKE_PROGRAM})

#     find_program(CMAKE_LINKER ld.exe ld
#       DOC "Find a suitable linker for building under Windows/MinGW"
#       HINTS
#       C:/gfortran_32/bin)
      message("CMAKE_LINKER: " ${CMAKE_LINKER})

#     find_program(CMAKE_AR ar.exe ar
#       DOC "Find a suitable archiving program for building under Windows/MinGW"
#       HINTS
#       C:/gfortran_32/bin )
      message("CMAKE_AR: " ${CMAKE_AR})

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
       C:/gfortran_32/lib/gcc/i686-pc-mingw32/4.6.0 )

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
        DOC "MinGW GDI32 library"
        HINTS
        C:/gfortran_32/lib )

      find_library(OPENGL32_LIB
        NAMES opengl32 libopengl32.a
        DOC "MinGW OPENGL32 library"
        HINTS
        C:/gfortran_32/lib )

      find_library(GLU32_LIB
        NAMES glu32 libglu32.a
        DOC "MinGW GLU32 library"
        HINTS
        C:/gfortran_32/lib )

     set(EXTRA_LIBS ${EXTRA_LIBS} ${NETCDF_FORTRAN_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${DISLIN_FORTRAN_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${GCC_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${GFORTRAN_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${USER32_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${GDI32_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${OPENGL32_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${GLU32_LIB} )

     SET(CMAKE_INSTALL_PREFIX "d:/dos")

     add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swb.exe D:/dos/swb.exe
           COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbread.exe D:/dos/swbread.exe
           COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbstats.exe D:/dos/swbstats.exe )

     add_custom_target(makelib
         ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -xv ${NETCDF_FORTRAN_LIB}
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -xv ${DISLIN_FORTRAN_LIB}
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -xv ${GDI32_LIB}
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -xv ${USER32_LIB}
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -xv ${OPENGL32_LIB}
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -xv ${GLU32_LIB}
         COMMAND ${CMAKE_COMMAND} -E copy  ${PROJECT_SOURCE_DIR}/build/win32/src/libswblib.a ${PROJECT_SOURCE_DIR}/build/temp/libswblib.a
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -xv libswblib.a
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -rc libswb.a *.o
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -q libswb.a *.obj
         COMMAND ${CMAKE_COMMAND} -E copy ${PROJECT_SOURCE_DIR}/build/temp/libswb.a ${PROJECT_SOURCE_DIR}/build/win32/src/libswb.a
         VERBATIM)

   endif ()

elseif(CMAKE_HOST_UNIX)

   if (Fortran_COMPILER_NAME MATCHES "ifort[.a-z]*")
     #------------------------------------------------------------------------
     # Set compilation and link preferences for Intel Fortran on Linux
     #------------------------------------------------------------------------
     set (Fortran_FLAGS_RELEASE "-fast -fPIC")
     set (Fortran_FLAGS_DEBUG "-O0 -g -traceback -fpe0 -check all -debug all -fPIC")
     set(CMAKE_FIND_LIBRARY_PREFIXES lib)
#     set(CMAKE_FIND_LIBRARY_SUFFIXES ".so" "")
     set(CMAKE_FIND_LIBRARY_SUFFIXES ".a" "")

#     set(CMAKE_Fortran_LINK_FLAGS "-Bdynamic -pie")
#     set(CMAKE_LINKER "/usr/bin/gcc")
#     set(CMAKE_Fortran_LINK_EXECUTABLE "/usr/bin/gcc")

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
#       /usr/include )
       ${CMAKE_CURRENT_SOURCE_DIR}/include/ubuntu_x64/ifort
#       PATH_SUFFIXES
       ${CMAKE_CURRENT_SOURCE_DIR}/include/ubuntu_x64/ifort)
       message("Using include directory: " ${INCLUDE_DIR})

     find_path(INCLUDE_DISLIN dislin.mod
       DOC "Find the DISLIN module file"
       HINTS
       /usr/local/dislin/gf )
       message("Using DISLIN include directory: " ${INCLUDE_DISLIN})

     find_library(NETCDF_FORTRAN_LIB
       NAMES netcdf NETCDF
       DOC "NetCDF Fortran library"
       HINTS
#       /usr/lib )
       ${CMAKE_CURRENT_SOURCE_DIR}/lib/ubuntu_x64/ifort
       PATH_SUFFIXES
       lib/ubuntu_x64/ifort)

     find_library(OPEN_MOTIF_LIB
       NAMES Xm
#       NAMES Xm
       DOC "Open Motif library"
       HINTS
       /usr/lib)
#       lib/ubuntu_x64/ifort)

#     set(OPEN_MOTIF_LIB "/usr/lib/libXm.a")

     # I have no idea why such effort was required to get DISLIN to link with SWB under Linux/ifort!
     set(X_LIB "/usr/lib/libXt.a;/usr/lib/libX11.a;/usr/lib/libxcb.a")
     set(X_LIB ${X_LIB} "/usr/lib/libXdmcp.a;/usr/lib/libXau.a")
     set(X_LIB ${X_LIB} "/opt/mesa/lib/libGLU.a;/opt/mesa/lib/libOSMesa.a")
     set(X_LIB ${X_LIB} "/opt/mesa/lib/libEGL.a;/opt/mesa/lib/libglut.a")
     set(X_LIB ${X_LIB} "/opt/mesa/lib/libGLw.a")

     find_library(DISLIN_FORTRAN_LIB
       NAMES dislin
       DOC "DISLIN Fortran library"
       HINTS
       /usr/lib
       /usr/local/lib
       /usr/local/dislin
#       ${CMAKE_CURRENT_SOURCE_DIR}/lib/ubuntu_x64/ifort
       PATH_SUFFIXES
       /usr/lib)
#       lib/ubuntu_x64/ifort)

#     find_library(GCC_LIB
#       NAMES libgcc gcc
#       DOC "gcc library"
#       HINTS
#       /usr/lib
#       /usr/lib/gcc
#       /usr/lib/gcc/x86_64-linux-gnu/4.4)

     set(EXTRA_LIBS ${EXTRA_LIBS} ${NETCDF_FORTRAN_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${DISLIN_FORTRAN_LIB} )
#     set(EXTRA_LIBS ${EXTRA_LIBS} ${GCC_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${OPEN_MOTIF_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${X_LIB} )

     message("libs: " "${EXTRA_LIBS}")

     SET(CMAKE_INSTALL_PREFIX "/usr/local/bin")
     add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swb /usr/local/bin/swb
           COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbread /usr/local/bin/swbread
           COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbstats /usr/local/bin/swbstats )

   endif (Fortran_COMPILER_NAME MATCHES "ifort[.a-z]*")

endif()

set ( CMAKE_Fortran_FLAGS_DEBUG ${Fortran_FLAGS_DEBUG})
set ( CMAKE_Fortran_FLAGS_RELEASE ${Fortran_FLAGS_RELEASE})

if( CMAKE_BUILD_TYPE STREQUAL "Debug")
#  add_definitions(${Fortran_FLAGS_DEBUG})
else()
  set(CMAKE_BUILD_TYPE "Release")
#  add_definitions(${Fortran_FLAGS_RELEASE})
endif()

include_directories(${INCLUDE_DIR} ${INCLUDE_DISLIN})
#set(EXTRA_LIBS ${EXTRA_LIBS} ${NETCDF_LIBS} ${DISLIN_LIBS})
link_libraries( ${EXTRA_LIBS} )

get_filename_component(COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

message("--------------------------------------------------------------------")
message("   Summary of CMAKE environment variables")
message("--------------------------------------------------------------------")
message("BUILD TYPE: " ${CMAKE_BUILD_TYPE})
message("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
message("Fortran compiler: " ${Fortran_COMPILER_NAME})
message("Linker:   " ${CMAKE_Fortran_LINK_EXECUTABLE})
message("CMAKE_AR: " ${CMAKE_AR})
message("CMAKE Fortran flags (Debug):      " ${CMAKE_Fortran_FLAGS_DEBUG})
message("CMAKE Fortran flags (Release):      " ${CMAKE_Fortran_FLAGS_RELEASE})
MESSAGE("Install to directory:       " ${CMAKE_INSTALL_PREFIX})
message("Using NetCDF library: " ${NETCDF_FORTRAN_LIB})
message("Using DISLIN library: " ${DISLIN_FORTRAN_LIB})
message("CMAKE_BINARY_DIR: " ${CMAKE_BINARY_DIR})
message("CMAKE_CURRENT_SOURCE_DIR: " ${CMAKE_CURRENT_SOURCE_DIR})
message("PROJECT_SOURCE_DIR: " ${PROJECT_SOURCE_DIR})
message("CMAKE_Fortran_LINK_FLAGS: " ${CMAKE_Fortran_LINK_FLAGS})
message("--------------------------------------------------------------------")

add_subdirectory(src)
