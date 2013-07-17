# CMake project for swb

# based in part on an example found at:
# http://lagrange.mechse.illinois.edu/mwest/partmc/partmc-1.2.0/CMakeLists.txt

message("Processing top-level CMakelists.txt for project swb")

#------------------------------------------------------------------------------------#
#                           Build directory check                                    #
#------------------------------------------------------------------------------------#
if(${CMAKE_SOURCE_DIR}/src STREQUAL ${CMAKE_BINARY_DIR})
  message(FATAL_ERROR "Cannot be built in the source directory. Use
  out-of-source build instead.")
  message(FATAL_ERROR "  cd /some/work/dir/build")
  message(FATAL_ERROR "  cmake -i /path/to/source")
endif(${CMAKE_SOURCE_DIR}/src STREQUAL ${CMAKE_BINARY_DIR})

set(CMAKE_C_COMPILER_FORCED TRUE)
set(CMAKE_CXX_COMPILER_FORCED TRUE)

#------------------------------------------------------------------------
# Set basic project settings
#------------------------------------------------------------------------
#
# NOTE: must enable CXX as a language in order to use the GNU compiler
#       toolchain
#
project (swb Fortran C CXX)
enable_language (Fortran C CXX)

SET(CPACK_GENERATOR "TGZ")
include(CPack)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(PACKAGE_BUGREPORT "smwesten@usgs.gov")
set(PACKAGE_NAME "swb")
set(PACKAGE_STRING "swb 1.2 BETA")
set(PACKAGE_TARNAME "swb")
set(PACKAGE_VERSION "1.2 BETA")

cmake_minimum_required(VERSION 2.8)

#------------------------------------------------------------------------
# Locate utility programs
#------------------------------------------------------------------------

find_program( R_SCRIPT Rscript.exe Rscript
    HINTS
    ENV R_HOME
	${PATH_TO_R}
    PATHS
    "c:/Program Files/R"
    "c:/Program Files/R/R-3.0.1/bin"
    "/usr/bin"
)

message("Rscript is defined as: " ${R_SCRIPT})

#------------------------------------------------------------------------
# Restrict CMAKE_BUILD_TYPE to "Release" or "Debug"
#------------------------------------------------------------------------

set_property(CACHE CMAKE_BUILD_TYPE
        PROPERTY STRINGS "Release" "Debug")

set( CMAKE_BUILD_TYPE "Debug" CACHE STRING
       "Compile in DEBUG or RELEASE mode" )

set_property(CACHE PLATFORM_TYPE
        PROPERTY STRINGS "x86" "x64")

set( PLATFORM_TYPE "x64" CACHE STRING
       "Compile for 32-bit or 64-bit" )

#------------------------------------------------------------------------
# Enable or disable compilation TARGETS from the GUI
#------------------------------------------------------------------------

option (TARGET__SWB_EXECUTABLE
      "Compile the main SWB executable?" OFF)

option (TARGET__SWB_LIBRARY
      "Compile SWB as a library?" OFF)

option (TARGET__SWBSTATS
      "Compile the swbstats application?" OFF)

#------------------------------------------------------------------------
# Set PREPROCESSOR DEFINES for conditional compilation
#------------------------------------------------------------------------

option (OPTION__GRAPHICS_SUPPORT
      "Compile the code with graphics (DISLIN) support?" OFF)
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
      "Compile the code with NETCDF support?" OFF)
if(OPTION__NETCDF_SUPPORT)
  set ( PREPROCESSOR_DEFINES ${PREPROCESSOR_DEFINES} "NETCDF_SUPPORT")
endif()

option (OPTION__STRICT_DATE_CHECKING
      "Compile the code with strict enforcement of dates?" OFF)
if(OPTION__STRICT_DATE_CHECKING)
  set ( PREPROCESSOR_DEFINES ${PREPROCESSOR_DEFINES} "STRICT_DATE_CHECKING")
endif()

option (OPTION__DEBUG_PRINT
      "Compile the code with extra debug print options enabled?" OFF )
if(OPTION__DEBUG_PRINT)
  set ( PREPROCESSOR_DEFINES ${PREPROCESSOR_DEFINES} "DEBUG_PRINT")
endif()

#------------------------------------------------------------------------
# Set platform-specific options
#------------------------------------------------------------------------
if(CMAKE_HOST_WIN32)

  if (Fortran_COMPILER_NAME MATCHES "gfortran[.a-z]*")

     message(" <-- setting options for gfortran on MinGW (32-bit/64-bit) -->")

     set_property(CACHE CMAKE_BUILD_TYPE
             PROPERTY STRINGS "Release" "Debug")

     if( PLATFORM_TYPE MATCHES "x86" )
       set(NETCDF_C_LIB ${PROJECT_SOURCE_DIR}/lib/win_x86/gfortran/libnetcdf.a
         CACHE FILEPATH "Location of NetCDF C library")
       set(HDF5_LIB ${PROJECT_SOURCE_DIR}/lib/win_x86/gfortran/libhdf5.a
         CACHE FILEPATH "Location of HDF5 library")
       set(HDF5_HL_LIB ${PROJECT_SOURCE_DIR}/lib/win_x86/gfortran/libhdf5_hl.a
         CACHE FILEPATH "Location of HDF5_HL library")

       set(SZLIB_LIB ${PROJECT_SOURCE_DIR}/lib/win_x86/gfortran/libsz.a
         CACHE FILEPATH "Location of szlib library")

       set(CURL_LIB ${PROJECT_SOURCE_DIR}/lib/win_x86/gfortran/libcurl.a
         CACHE FILEPATH "Location of cURL library")

       set(ZLIB_LIB ${PROJECT_SOURCE_DIR}/lib/win_x86/gfortran/libzlib.a
         CACHE FILEPATH "Location of zlib library")

       set(PROJ4_LIB ${PROJECT_SOURCE_DIR}/lib/win_x86/gfortran/libproj4.a
         CACHE FILEPATH "Location of PROJ4 library")

       set(WS2_32_LIB ${MINGWBASE}/${COMPILER_TRIPLET}/lib/libws2_32.a
	       CACHE FILEPATH "Location of Winsock for Windows library" )

       set(DISLIN_FORTRAN_LIB ${PROJECT_SOURCE_DIR}/lib/win_x86/gfortran/disgf.a )
       set(MODULE_DIR ${INCLUDE_DIR} ${PROJECT_SOURCE_DIR}/include/win_x86/gfortran )
       set(USER32_LIB ${MINGWBASE}/lib/libuser32.a)
       set(GDI32_LIB ${MINGWBASE}/lib/libgdi32.a)
       set(OPENGL32_LIB ${MINGWBASE}/lib/libopengl32.a)
       set(GCC_LIB ${MINGWBASE}/lib/libgcc.a)
     else()
       set(NETCDF_C_LIB ${PROJECT_SOURCE_DIR}/lib/win_x64/gfortran/libnetcdf.a
         CACHE FILEPATH "Location of NetCDF C library")
       set(HDF5_LIB ${PROJECT_SOURCE_DIR}/lib/win_x64/gfortran/libhdf5.a
         CACHE FILEPATH "Location of HDF5 library")
       set(HDF5_HL_LIB ${PROJECT_SOURCE_DIR}/lib/win_x64/gfortran/libhdf5_hl.a
         CACHE FILEPATH "Location of HDF5_HL library")
       set(CURL_LIB ${PROJECT_SOURCE_DIR}/lib/win_x64/gfortran/libcurl.a
         CACHE FILEPATH "Location of cURL library")
       set(SZLIB_LIB ${PROJECT_SOURCE_DIR}/lib/win_x64/gfortran/libsz.a
         CACHE FILEPATH "Location of szlib library")
       set(ZLIB_LIB ${PROJECT_SOURCE_DIR}/lib/win_x64/gfortran/libzlib.a
         CACHE FILEPATH "Location of zlib library")

       set(WS2_32_LIB ${MINGWBASE}/${COMPILER_TRIPLET}/lib/libws2_32.a
	       CACHE FILEPATH "Location of Winsock for Windows library" )

#       set(PROJ4_LIB ${PROJECT_SOURCE_DIR}/lib/win_x64/gfortran/libproj4.a
#         CACHE FILEPATH "Location of PROJ4 library")


       set(DISLIN_FORTRAN_LIB ${PROJECT_SOURCE_DIR}/lib/win_x64/gfortran/dismg.a )
       set(MODULE_DIR ${INCLUDE_DIR} ${PROJECT_SOURCE_DIR}/include/win_x64/gfortran )
       set(USER32_LIB ${MINGWBASE}/${COMPILER_TRIPLET}/lib/libuser32.a)
       set(GDI32_LIB ${MINGWBASE}/${COMPILER_TRIPLET}/lib/libgdi32.a)
       set(OPENGL32_LIB ${MINGWBASE}/${COMPILER_TRIPLET}/lib/libopengl32.a)
       set(GCC_LIB ${MINGWBASE}/lib/gcc/${COMPILER_TRIPLET}/${MINGW_VERSION}/libgcc.a)
     endif()

     set(GFORTRAN_LIB ${MINGWBASE}/lib/libgfortran.a)
     set(EXTRA_LIBS ${EXTRA_LIBS} ${GCC_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${GFORTRAN_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${HDF5_HL_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${HDF5_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${CURL_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${SZLIB_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${ZLIB_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${NETCDF_C_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${DISLIN_FORTRAN_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${USER32_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${GDI32_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${WS2_32_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${OPENGL32_LIB} )



     include_directories( ${MODULE_DIR} ${EXTRA_INCLUDES} "${PROJECT_SOURCE_DIR}/src/proj4")

     SET(CMAKE_INSTALL_PREFIX "d:/dos" CACHE PATHFILE "Installation directory")

     add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swb.exe ${CMAKE_INSTALL_PREFIX}/swb.exe
           COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbstats.exe ${CMAKE_INSTALL_PREFIX}/swbstats.exe
     )

     add_custom_target(makelib
         ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -xv ${NETCDF_C_LIB}
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -xv ${HDF5_HL_LIB}
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -xv ${HDF5_LIB}
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -xv ${ZLIB_LIB}
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -xv ${SZLIB_LIB}
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -xv ${WS2_32_LIB}
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -xv ${DISLIN_FORTRAN_LIB}
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -xv ${GDI32_LIB}
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -xv ${USER32_LIB}
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -xv ${OPENGL32_LIB}
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -xv ${GLU32_LIB}
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -xv ${CURL_LIB}
         COMMAND ${CMAKE_COMMAND} -E copy  ${PROJECT_SOURCE_DIR}/build/win32/src/libswblib.a ${PROJECT_SOURCE_DIR}/build/temp/libswblib.a
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -xv libswblib.a
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -rc libswb.a *.o
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -q libswb.a *.obj
         COMMAND ${CMAKE_COMMAND} -E copy ${PROJECT_SOURCE_DIR}/build/temp/libswb.a ${PROJECT_SOURCE_DIR}/build/win/src/libswb.a
         VERBATIM
     )

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
      include_directories( ${MODULE_DIR} ${EXTRA_INCLUDES} "${PROJECT_SOURCE_DIR}/src/proj4")

      set(EXTRA_LIBS ${EXTRA_LIBS} ${NETCDF_FORTRAN_LIB} )
      set(EXTRA_LIBS ${EXTRA_LIBS} ${DISLIN_FORTRAN_LIB} )

    endif()

elseif(CMAKE_HOST_APPLE)

   if (Fortran_COMPILER_NAME MATCHES "gfortran[.a-z]*")

     message(" <-- setting options for gfortran on OSX -->")

     set (CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -Wall -Wextra -fcheck=all -fexceptions -ffree-line-length-none -static-libgcc -static-libgfortran")

     set_property(CACHE CMAKE_BUILD_TYPE
             PROPERTY STRINGS "Release" "Debug")
     set(NETCDF_FORTRAN_LIB ${PROJECT_SOURCE_DIR}/lib/osx/gfortran/libnetcdf.a
       CACHE FILEPATH "Location of NetCDF Fortran library")
       set(DISLIN_FORTRAN_LIB /usr/local/dislin/libdislin.10.dylib )
       set(MODULE_DIR ${INCLUDE_DIR} ${PROJECT_SOURCE_DIR}/include/osx/gfortran )
       set (CMAKE_Fortran_FLAGS_RELEASE "-O2 -ffree-line-length-none -static-libgcc -static-libgfortran")

     set(MODULE_DIR ${PROJECT_SOURCE_DIR}/include/osx/gfortran)
     set(GFORTRAN_LIB /usr/local/lib/libgfortran.a)
     set(EXTRA_LIBS ${EXTRA_LIBS} ${GCC_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${GFORTRAN_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${NETCDF_FORTRAN_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${DISLIN_FORTRAN_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${USER32_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${GDI32_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${OPENGL32_LIB} )

     include_directories( ${MODULE_DIR} ${EXTRA_INCLUDES} "${PROJECT_SOURCE_DIR}/src/proj4")

     SET(CMAKE_INSTALL_PREFIX "/Users/smwesten" CACHE PATHFILE "Installation directory")

     add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swb.exe ${CMAKE_INSTALL_PREFIX}/swb.exe
           COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbstats.exe ${CMAKE_INSTALL_PREFIX}/swbstats.exe
     )

     add_custom_target(makelib
         ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -xv ${NETCDF_FORTRAN_LIB}
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -xv ${DISLIN_FORTRAN_LIB}
         COMMAND ${CMAKE_COMMAND} -E copy  ${PROJECT_SOURCE_DIR}/build/win32/src/libswblib.a ${PROJECT_SOURCE_DIR}/build/temp/libswblib.a
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -xv libswblib.a
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -rc libswb.a *.o
         COMMAND ${CMAKE_COMMAND} -E chdir ${PROJECT_SOURCE_DIR}/build/temp ar -q libswb.a *.obj
         COMMAND ${CMAKE_COMMAND} -E copy ${PROJECT_SOURCE_DIR}/build/temp/libswb.a ${PROJECT_SOURCE_DIR}/build/APPLE/gfortran/src/libswb.a
         VERBATIM)

   endif()

elseif(CMAKE_HOST_UNIX)

   #------------------------------------------------------------------------
   # Set compilation and link preferences for Linux
   #------------------------------------------------------------------------

     set(CMAKE_FIND_LIBRARY_PREFIXES lib)
     set(CMAKE_FIND_LIBRARY_SUFFIXES ".so" "")
#     set(CMAKE_FIND_LIBRARY_SUFFIXES ".a" "")

   find_program(CMAKE_MAKE_PROGRAM make
     DOC "Find a suitable make program for building under Linux"
     HINTS
     /bin
     /usr/bin)
     message("CMAKE_MAKE_PROGRAM: " ${CMAKE_MAKE_PROGRAM})

   find_program(CMAKE_SH sh
     DOC "Find a suitable shell for building  under Linux"
     HINTS
     /bin)
     message("CMAKE_SH: " ${CMAKE_SH})

   find_path(MODULE_DIR
             NAMES netcdf.mod NETCDF.mod
             DOC "Include directory (must contain netcdf.mod)"
             HINTS
             "${PROJECT_SOURCE_DIR}/include/linux_x64/gfortran"
             /usr/include )
             message("Using include directory: " ${INCLUDE_DIR})

   find_path(INCLUDE_DISLIN
     NAMES dislin.mod
     DOC "Find the DISLIN module file"
     PATHS
     "${PROJECT_SOURCE_DIR}/include/linux_x64/gfortran"
     /usr/local/dislin/gf )

   find_library(NETCDF_C_LIB
     NAMES netcdf NETCDF
     DOC "NetCDF C library"
     HINTS
     /usr/local/lib )

   if (Fortran_COMPILER_NAME MATCHES "gfortran[.a-z]*")

     find_library(DISLIN_FORTRAN_LIB
       NAMES dislin dislin.a libdislin.a
       DOC "DISLIN Fortran library"
       HINTS
       /usr/local/dislin
       "${PROJECT_SOURCE_DIR}/lib/linux_x64/gfortran"
     )

   elseif (Fortran_COMPILER_NAME MATCHES "ifort[.a-z]*")

     find_library(DISLIN_FORTRAN_LIB
       NAMES disifl
       DOC "DISLIN Fortran library"
       PATHS
       ${PROJECT_SOURCE_DIR}/lib/linux_x64/ifort
     )

   endif()

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

   set(EXTRA_LIBS ${EXTRA_LIBS} ${NETCDF_C_LIB} )
   set(EXTRA_LIBS ${EXTRA_LIBS} ${DISLIN_FORTRAN_LIB} )

   include_directories( ${MODULE_DIR} "${PROJECT_SOURCE_DIR}/src/proj4")

   SET(CMAKE_INSTALL_PREFIX "/usr/local/bin")
   add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swb /usr/local/bin/swb
         COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbstats /usr/local/bin/swbstats )

else()

  message(FATAL_ERROR "Windows 32/64/gfortran/MinGW or Linux-64/Intel environments were not detected!")

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
message( " ")
message("CMAKE_HOST_WIN32: " ${CMAKE_HOST_WIN32})
message("CMAKE_HOST_APPLE: " ${CMAKE_HOST_APPLE})
message("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
message("Fortran compiler: " ${Fortran_COMPILER_NAME})
message("CMAKE Fortran flags (Debug):      " ${CMAKE_Fortran_FLAGS_DEBUG})
message("CMAKE Fortran flags (Profile):      " ${CMAKE_Fortran_FLAGS_PROFILE})
message("CMAKE Fortran flags (Release):      " ${CMAKE_Fortran_FLAGS_RELEASE})
message( " ")
MESSAGE("Install to directory: " ${CMAKE_INSTALL_PREFIX})
message("NetCDF C library: " ${NETCDF_C_LIB})
message("Module include directory: " ${MODULE_DIR})
message("DISLIN library: " ${DISLIN_FORTRAN_LIB})
message("CMAKE_BINARY_DIR: " ${CMAKE_BINARY_DIR})
message("CMAKE_CURRENT_SOURCE_DIR: " ${CMAKE_CURRENT_SOURCE_DIR})
message("PROJECT_SOURCE_DIR: " ${PROJECT_SOURCE_DIR})
message( " ")
message("--------------------------------------------------------------------")

enable_testing()
add_subdirectory(src)
add_subdirectory(tests/general)
add_subdirectory(tests/irrigation)
