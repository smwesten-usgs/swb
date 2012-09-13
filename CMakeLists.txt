# CMake project for swb

# based in part on an example found at:
# http://lagrange.mechse.illinois.edu/mwest/partmc/partmc-1.2.0/CMakeLists.txt

message("Processing top-level CMakelists.txt for project swb")

#------------------------------------------------------------------------
# Set basic project settings
#------------------------------------------------------------------------
project (swb)
enable_language (Fortran)

set(PACKAGE_BUGREPORT "smwesten@usgs.gov")
set(PACKAGE_NAME "swb")
set(PACKAGE_STRING "swb 1.0.1")
set(PACKAGE_TARNAME "swb")
set(PACKAGE_VERSION "1.0.1")

cmake_minimum_required(VERSION 2.6)

#
option (MAKEFILE_VERBOSE
      "Produce verbose makefiles?" OFF)

#------------------------------------------------------------------------
# Restrict CMAKE_BUILD_TYPE to "Release" or "Debug"
#------------------------------------------------------------------------

set_property(CACHE CMAKE_BUILD_TYPE
        PROPERTY STRINGS "Release" "Debug")

set( CMAKE_BUILD_TYPE "Release" CACHE STRING
       "Compile in DEBUG or RELEASE mode" )

set_property(CACHE PLATFORM_TYPE
        PROPERTY STRINGS "x86" "x64")

set( PLATFORM_TYPE "x64" CACHE STRING
       "Compile for 32-bit or 64-bit" )

#------------------------------------------------------------------------
# What Fortran compiler are we using?
#------------------------------------------------------------------------
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

#------------------------------------------------------------------------
# Enable or disable compilation TARGETS from the GUI
#------------------------------------------------------------------------

option (TARGET__SWB_EXECUTABLE
      "Compile the main SWB executable?" ON)

option (TARGET__SWB_LIBRARY
      "Compile SWB as a library?" ON)

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
  set ( PREPROCESSOR_DEFINES ${PREPROCESSOR_DEFINES}  "THORNTHWAITE_MATHER_TABLE")
endif()

option (OPTION__IRRIGATION_MODULE
      "Compile the code with irrigation module support?" OFF)
if(OPTION__IRRIGATION_MODULE)
  set ( PREPROCESSOR_DEFINES ${PREPROCESSOR_DEFINES} "IRRIGATION_MODULE")
  set(IRRIGATION_MODULE_VARS 3)
endif()

option (OPTION__STRICT_DATE_CHECKING
      "Compile the code with strict enforcement of dates?" OFF)
if(OPTION__STRICT_DATE_CHECKING)
  set ( PREPROCESSOR_DEFINES ${PREPROCESSOR_DEFINES} "STRICT_DATE_CHECKING")
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

     message(" <-- setting options for gfortran on MinGW (32-bit/64-bit) -->")

     set (CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -Wall -Wextra -fcheck=all -fexceptions -ffree-line-length-none -static-libgcc -static-libgfortran")

     set_property(CACHE CMAKE_BUILD_TYPE
             PROPERTY STRINGS "Release" "Debug")

     if( PLATFORM_TYPE MATCHES "x86" )
       set (MINGW_BASE_DIR "c:/MinGW32" CACHE FILEPATH "Path to base MinGW install")
       set(NETCDF_FORTRAN_LIB ${PROJECT_SOURCE_DIR}/lib/win_x86/gfortran/libnetcdf.a
        CACHE FILEPATH "Location of NetCDF library")
       set(DISLIN_FORTRAN_LIB ${PROJECT_SOURCE_DIR}/lib/win_x86/gfortran/disgf.a )
       set(MODULE_DIR ${INCLUDE_DIR} ${PROJECT_SOURCE_DIR}/include/win_x86/gfortran )
       set(USER32_LIB ${MINGW_BASE_DIR}/lib/libuser32.a)
       set(GDI32_LIB ${MINGW_BASE_DIR}/lib/libgdi32.a)
       set(OPENGL32_LIB ${MINGW_BASE_DIR}/lib/libopengl32.a)
       set(GCC_LIB ${MINGW_BASE_DIR}/lib/libgcc.a)
       set (CMAKE_Fortran_FLAGS_RELEASE "-O2 -ffree-line-length-none -static-libgcc -static-libgfortran")
     else()
       set (MINGW_BASE_DIR "c:/MinGW64" CACHE FILEPATH "Path to base MinGW install")
       set(NETCDF_FORTRAN_LIB ${PROJECT_SOURCE_DIR}/lib/win_x64/gfortran/libnetcdff.a
         CACHE FILEPATH "Location of NetCDF Fortran library")
       set(NETCDF_C_LIB ${PROJECT_SOURCE_DIR}/lib/win_x64/gfortran/libnetcdf.a
         CACHE FILEPATH "Location of NetCDF C library")
       set(DISLIN_FORTRAN_LIB ${PROJECT_SOURCE_DIR}/lib/win_x64/gfortran/dismg.a )
       set(MODULE_DIR ${INCLUDE_DIR} ${PROJECT_SOURCE_DIR}/include/win_x64/gfortran )
       set(USER32_LIB ${MINGW_BASE_DIR}/x86_64-w64-mingw32/lib/libuser32.a)
       set(GDI32_LIB ${MINGW_BASE_DIR}/x86_64-w64-mingw32/lib/libgdi32.a)
       set(OPENGL32_LIB ${MINGW_BASE_DIR}/x86_64-w64-mingw32/lib/libopengl32.a)
       set(GCC_LIB ${MINGW_BASE_DIR}/lib/gcc/x86_64-w64-mingw32/4.7.1/libgcc.a)
       set (CMAKE_Fortran_FLAGS_RELEASE "-O2 -floop-parallelize-all -ffree-line-length-none -static-libgcc -static-libgfortran")
     endif()

     set(GFORTRAN_LIB ${MINGW_BASE_DIR}/lib/libgfortran.a)
     set(EXTRA_LIBS ${EXTRA_LIBS} ${GCC_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${GFORTRAN_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${NETCDF_C_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${NETCDF_FORTRAN_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${DISLIN_FORTRAN_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${USER32_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${GDI32_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${OPENGL32_LIB} )

     include_directories( ${MODULE_DIR} )

     SET(CMAKE_INSTALL_PREFIX "d:/dos" CACHE PATHFILE "Installation directory")

     add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swb.exe ${CMAKE_INSTALL_PREFIX}/swb.exe
           COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbstats.exe ${CMAKE_INSTALL_PREFIX}/swbstats.exe
     )

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

    elseif (Fortran_COMPILER_NAME MATCHES "ifort[.a-z]*")
      #------------------------------------------------------------------------
      # Set compilation and link preferences for Intel Fortran on Windows, 64-bit
      #------------------------------------------------------------------------
      set (CMAKE_Fortran_FLAGS_RELEASE "/fast /fpp")
      set (CMAKE_Fortran_FLAGS_DEBUG "/traceback /fpp")

      set(CMAKE_FIND_LIBRARY_SUFFIXES ".lib" ".a")

      find_library(NETCDF_FORTRAN_LIB
         NAMES netcdff90 NETCDFF libnetcdff90.lib
         DOC "NetCDF Fortran library"
         HINTS
           ${PROJECT_SOURCE_DIR}/lib/win_x64/ifort
         )

     find_library(NETCDF_C_LIB
        NAMES netcdf NETCDF netcdf.lib
        DOC "NetCDF C library"
        HINTS
          ${PROJECT_SOURCE_DIR}/lib/win_x64/ifort
        )

     find_library(DISLIN_FORTRAN_LIB
       NAMES disifl
       DOC "DISLIN Fortran library"
       HINTS
         ${PROJECT_SOURCE_DIR}/lib/win_x64/ifort
       )

    find_path(MODULE_DIR netcdf.mod dislin.mod
       DOC "Find the path to the fortran module files"
       HINTS
         ${PROJECT_SOURCE_DIR}/include/win_x64/ifort
       )

    set( Fortran_MODULE_DIRECTORY ${MODULE_DIR} )
    include_directories( ${MODULE_DIR} )

    set(EXTRA_LIBS ${EXTRA_LIBS} ${NETCDF_FORTRAN_LIB} )
    set(EXTRA_LIBS ${EXTRA_LIBS} ${DISLIN_FORTRAN_LIB} )

  endif()

elseif(CMAKE_HOST_UNIX)

   if (Fortran_COMPILER_NAME MATCHES "ifort[.a-z]*")
     #------------------------------------------------------------------------
     # Set compilation and link preferences for Intel Fortran on Linux
     #------------------------------------------------------------------------
     set (CMAKE_Fortran_FLAGS_RELEASE "-fast -fPIC")
     set (CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -traceback -fpe0 -check all -debug all -fPIC")
     set(CMAKE_FIND_LIBRARY_PREFIXES lib)
     set(CMAKE_FIND_LIBRARY_SUFFIXES ".so" "")
#     set(CMAKE_FIND_LIBRARY_SUFFIXES ".a" "")

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

     find_path(MODULE_DIR netcdf.mod NETCDF.mod
       DOC "Include directory (must contain netcdf.mod)"
       HINTS
       /usr/include )
       message("Using include directory: " ${INCLUDE_DIR})

     find_path(INCLUDE_DISLIN dislin.mod
       DOC "Find the DISLIN module file"
       HINTS
       /usr/local/dislin/gf )
       message("Using DISLIN include directory: " ${INCLUDE_DISLIN})

     find_library(NETCDF_C_LIB
       NAMES netcdf NETCDF
       DOC "NetCDF C library"
       HINTS
       /usr/local/lib )

     find_library(NETCDF_FORTRAN_LIB
       NAMES netcdff NETCDFF
       DOC "NetCDF Fortran library"
       /usr/local/lib )

#     find_library(OPEN_MOTIF_LIB
#       NAMES Xm
#       NAMES Xm
#       DOC "Open Motif library"
#       HINTS
#       /usr/lib)
#       lib/ubuntu_x64/ifort)

#     set(OPEN_MOTIF_LIB "/usr/lib/libXm.a")

     # I have no idea why such effort was required to get DISLIN to link with SWB under Linux/ifort!
#     set(X_LIB "/usr/lib/libXt.a;/usr/lib/libX11.a;/usr/lib/libxcb.a")
#     set(X_LIB ${X_LIB} "/usr/lib/libXdmcp.a;/usr/lib/libXau.a")
#     set(X_LIB ${X_LIB} "/opt/mesa/lib/libGLU.a;/opt/mesa/lib/libOSMesa.a")
#     set(X_LIB ${X_LIB} "/opt/mesa/lib/libEGL.a;/opt/mesa/lib/libglut.a")
#     set(X_LIB ${X_LIB} "/opt/mesa/lib/libGLw.a")

     find_library(DISLIN_FORTRAN_LIB
       NAMES dislin
       DOC "DISLIN Fortran library"
       HINTS
       /usr/local/dislin)

     set(EXTRA_LIBS ${EXTRA_LIBS} ${NETCDF_C_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${NETCDF_FORTRAN_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${DISLIN_FORTRAN_LIB} )

     include_directories( ${MODULE_DIR} )
      message("libs: " "${EXTRA_LIBS}")

     SET(CMAKE_INSTALL_PREFIX "/usr/local/bin")
     add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swb /usr/local/bin/swb
           COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbstats /usr/local/bin/swbstats )

   endif (Fortran_COMPILER_NAME MATCHES "ifort[.a-z]*")

else()

  message(FATAL_ERROR "Windows 32/gfortran/MinGW or Linux-64/Intel environments were not detected!")

endif()

#set ( CMAKE_Fortran_FLAGS_DEBUG ${Fortran_FLAGS_DEBUG})
#set ( CMAKE_Fortran_FLAGS_RELEASE ${Fortran_FLAGS_RELEASE})

#if( CMAKE_BUILD_TYPE STREQUAL "Debug")
##  add_definitions(${Fortran_FLAGS_DEBUG})
#else()
#  set(CMAKE_BUILD_TYPE "Release")
##  add_definitions(${Fortran_FLAGS_RELEASE})
#endif()

include_directories(${INCLUDE_DIR} ${INCLUDE_DISLIN})
link_libraries( ${EXTRA_LIBS} )

get_filename_component(COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

message("--------------------------------------------------------------------")
message("   Summary of CMAKE environment variables")
message("--------------------------------------------------------------------")
message("BUILD TYPE: " ${CMAKE_BUILD_TYPE})
message("Linker:   " ${CMAKE_Fortran_LINK_EXECUTABLE})
message("CMAKE_AR: " ${CMAKE_AR})
message("CMAKE Fortran flags (Debug):      " ${CMAKE_Fortran_FLAGS_DEBUG})
message("CMAKE Fortran flags (Release):      " ${CMAKE_Fortran_FLAGS_RELEASE})
MESSAGE("Install to directory:       " ${CMAKE_INSTALL_PREFIX})
message("Using NetCDF library: " ${NETCDF_FORTRAN_LIB})
message("Using Module include directory: " ${MODULE_DIR})
message("Using DISLIN library: " ${DISLIN_FORTRAN_LIB})
message("CMAKE_BINARY_DIR: " ${CMAKE_BINARY_DIR})
message("CMAKE_CURRENT_SOURCE_DIR: " ${CMAKE_CURRENT_SOURCE_DIR})
message("PROJECT_SOURCE_DIR: " ${PROJECT_SOURCE_DIR})
message("CMAKE_Fortran_LINK_FLAGS: " ${CMAKE_Fortran_LINK_FLAGS})
message( " ")
message("CMAKE_HOST_WIN32: " ${CMAKE_HOST_WIN32})
message("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
message("Fortran compiler: " ${Fortran_COMPILER_NAME})
message("--------------------------------------------------------------------")

add_subdirectory(src)
