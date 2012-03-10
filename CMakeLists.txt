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

#------------------------------------------------------------------------
# Set basic project settings
#------------------------------------------------------------------------
project (swb Fortran)
enable_language (Fortran)

SET(CPACK_GENERATOR "TGZ")
include(CPack)

set(PACKAGE_BUGREPORT "smwesten@usgs.gov")
set(PACKAGE_NAME "swb")
set(PACKAGE_STRING "swb 1.1")
set(PACKAGE_TARNAME "swb")
set(PACKAGE_VERSION "1.1")

cmake_minimum_required(VERSION 2.6)

#------------------------------------------------------------------------
# Offer option to create 'verbose' makefiles
#------------------------------------------------------------------------
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

#------------------------------------------------------------------------
# Enable or disable compilation TARGETS from the GUI
#------------------------------------------------------------------------

option (TARGET__SWB_EXECUTABLE
      "Compile the main SWB executable?" ON)

option (TARGET__SWB_LIBRARY
      "Compile SWB as a library?" OFF)

option (TARGET__SWBREAD
      "Compile the swbread application?" OFF)

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
      "Compile the code with NETCDF support?" OFF)
if(OPTION__NETCDF_SUPPORT)
  set ( PREPROCESSOR_DEFINES ${PREPROCESSOR_DEFINES}  "NETCDF_SUPPORT")
endif()

option (OPTION__THORNTHWAITE_MATHER_TABLES
      "Use T-M tables (if not use T-M equations)?" ON)
if(OPTION__THORNTHWAITE_MATHER_TABLES)
  set ( PREPROCESSOR_DEFINES ${PREPROCESSOR_DEFINES}  "THORNTHWAITE_MATHER_TABLE")
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

     message(" <-- setting options for gfortran on MinGW -->")

     set(CMAKE_INSTALL_PREFIX "d:/DOS" CACHE STRING
         "Location for installation of the executable")

     FIND_LIBRARY(GCC_LIB libgcc.a
       PATHS
         C:/MinGW/lib/gcc
         C:/MinGW/lib/gcc/mingw32
         C:/MinGW32/lib/gcc
         C:/MinGW32/lib/gcc/mingw32
       PATH_SUFFIXES
         4.6.2
         4.6.1
         4.5.2
         4.5.1
     )

     FIND_LIBRARY(GFORTRAN_LIB libgfortran.a
       PATHS
         C:/MinGW/lib/gcc
         C:/MinGW/lib/gcc/mingw32
         C:/MinGW32/lib/gcc
         C:/MinGW32/lib/gcc/mingw32
       PATH_SUFFIXES
         4.6.2
         4.6.1
         4.5.2
         4.5.1
     )

     FIND_LIBRARY(USER32_LIB libuser32.a
       PATHS
         C:/MinGW/lib
         C:/MinGW/lib/gcc
         C:/MinGW/lib/gcc/mingw32
         C:/MinGW32/lib
         C:/MinGW32/lib/gcc
         C:/MinGW32/lib/gcc/mingw32
       PATH_SUFFIXES
         4.6.2
         4.6.1
         4.5.2
         4.5.1
     )

     FIND_LIBRARY(GDI32_LIB libgdi32.a
       PATHS
         C:/MinGW/lib
         C:/MinGW/lib/gcc
         C:/MinGW/lib/gcc/mingw32
         C:/MinGW32/lib
         C:/MinGW32/lib/gcc
         C:/MinGW32/lib/gcc/mingw32
       PATH_SUFFIXES
         4.6.2
         4.6.1
         4.5.2
         4.5.1
     )

     FIND_LIBRARY(OPENGL32_LIB libopengl32.a
       PATHS
         C:/MinGW/lib
         C:/MinGW/lib/gcc
         C:/MinGW/lib/gcc/mingw32
         C:/MinGW32/lib
         C:/MinGW32/lib/gcc
         C:/MinGW32/lib/gcc/mingw32
       PATH_SUFFIXES
         4.6.2
         4.6.1
         4.5.2
         4.5.1
     )

     FIND_LIBRARY(GLU32_LIB libglu32.a
       PATHS
         C:/MinGW/lib
         C:/MinGW/lib/gcc
         C:/MinGW/lib/gcc/mingw32
         C:/MinGW32/lib
         C:/MinGW32/lib/gcc
         C:/MinGW32/lib/gcc/mingw32
       PATH_SUFFIXES
         4.6.2
         4.6.1
         4.5.2
         4.5.1
     )

     message("  GCC_LIB = " ${GCC_LIB})
     message("  GFORTRAN_LIB = " ${GFORTRAN_LIB})

     set (CMAKE_Fortran_FLAGS_RELEASE "-Ofast -fwhole-file -ffree-line-length-none")

     set (CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -gdb -Wall -Wextra -fcheck=all")
     set (CMAKE_Fortran_FLAGS_DEBUG ${CMAKE_Fortran_FLAGS_DEBUG} "-fexceptions -ffree-line-length-none")

     SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static -static-libgcc -static-libgfortran")

     set_property(CACHE CMAKE_BUILD_TYPE
             PROPERTY STRINGS "Release" "Debug")

     set(NETCDF_FORTRAN_LIB ${PROJECT_SOURCE_DIR}/lib/win32/gfortran_mingw/libnetcdf.a
       CACHE FILEPATH "Location of NetCDF library")
     set(DISLIN_FORTRAN_LIB ${PROJECT_SOURCE_DIR}/lib/win32/gfortran_mingw/disgf.a )

     set(INCLUDE_DIR ${INCLUDE_DIR} ${PROJECT_SOURCE_DIR}/include/win32/gfortran_mingw )

     set(EXTRA_LIBS ${EXTRA_LIBS} ${NETCDF_FORTRAN_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${DISLIN_FORTRAN_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${GCC_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${GFORTRAN_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${USER32_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${GDI32_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${OPENGL32_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${GLU32_LIB} )

     set(CMAKE_INSTALL_PREFIX "D:/dos" CACHE PATHFILE "Installation directory")

     add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swb.exe ${CMAKE_INSTALL_PREFIX}/swb.exe
           COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbstats.exe ${CMAKE_INSTALL_PREFIX}/swbstats.exe
           COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbread.exe ${CMAKE_INSTALL_PREFIX}/swbread.exe
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
         VERBATIM
     )

  else()

    message(" <-- setting options for Intel Fortran on Windows -->")
		
    set(CMAKE_INSTALL_PREFIX "d:/DOS" CACHE STRING
      "Location for installation of the executable")

		set_target_properties(${SWB} PROPERTIES PLATFORM_TOOLSET "Intel C++ Compiler XE 12.1")

		set (CMAKE_Fortran_FLAGS_RELEASE "/O2 /libs:static /MG /fast")
		set (CMAKE_Fortran_FLAGS_DEBUG "/O2 /libs:static /MG /fast")
    SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /NODEFAULTLIB:libc.lib")				
				
		set_property(CACHE CMAKE_BUILD_TYPE
				PROPERTY STRINGS "Release" "Debug")

     FIND_LIBRARY(USER32_LIB user32.lib
       PATHS
         C:/Program Files (x86)/Microsoft Visual Studio 10.0/Intel Fortran/Microsoft Files/VC/PlatformSDK/Lib
     )

     FIND_LIBRARY(GDI32_LIB gdi32.lib
       PATHS
         C:/Program Files (x86)/Microsoft Visual Studio 10.0/Intel Fortran/Microsoft Files/VC/PlatformSDK/Lib
     )

		set(NETCDF_FORTRAN_LIB ${PROJECT_SOURCE_DIR}/lib/win32/ifort/netcdf.lib
		CACHE FILEPATH "Location of NetCDF library")
		set(DISLIN_FORTRAN_LIB ${PROJECT_SOURCE_DIR}/lib/win32/ifort/disif.lib )
    set(DISLIN_FORTRAN_LIB2 ${PROJECT_SOURCE_DIR}/lib/win32/ifort/disifc.lib )		
    set(DISLIN_FORTRAN_LIB3 ${PROJECT_SOURCE_DIR}/lib/win32/ifort/disdll.lib )				

	#  set(EXTRA_LIBS ${EXTRA_LIBS} ${NETCDF_FORTRAN_LIB} )
	  set(EXTRA_LIBS ${EXTRA_LIBS} ${DISLIN_FORTRAN_LIB})
    set(EXTRA_LIBS ${EXTRA_LIBS} ${DISLIN_FORTRAN_LIB2})		
		set(EXTRA_LIBS ${EXTRA_LIBS} ${DISLIN_FORTRAN_LIB3})		
    set(EXTRA_LIBS ${EXTRA_LIBS} ${USER32_LIB} )
    set(EXTRA_LIBS ${EXTRA_LIBS} ${GDI32_LIB} )


	  set(INCLUDE_DIR ${INCLUDE_DIR} ${PROJECT_SOURCE_DIR}/include/win32/ifort )

		set(CMAKE_INSTALL_PREFIX "D:/dos" CACHE PATHFILE "Installation directory")

		add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swb.exe ${CMAKE_INSTALL_PREFIX}/swb.exe
		)

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

     find_path(INCLUDE_DIR netcdf.mod NETCDF.mod
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
set(EXTRA_LIBS ${EXTRA_LIBS} ${NETCDF_LIBS} ${DISLIN_LIBS})
#link_libraries( ${EXTRA_LIBS} )

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
