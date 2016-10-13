# define which portions of swb to build (i.e. swbstats? as library?)
set( TARGET__SWB_EXECUTABLE "TRUE" )
set( TARGET__SWB_LIBRARY "FALSE" )
set( TARGET__SWBSTATS "TRUE" )

# define which conditional compilation statements to include
set( OPTION__GRAPHICS_SUPPORT "TRUE" )
set( OPTION__STREAM_INTERACTIONS "FALSE" )
set( OPTION__NETCDF_SUPPORT "TRUE" )
set( OPTION__STRICT_DATE_CHECKING "FALSE" )
set( OPTION__DEBUG_PRINT "FALSE" )

if( ${OPTION__GRAPHICS_SUPPORT} )
  set( PREPROCESSOR_DEFINES "-DGRAPHICS_SUPPORT" )
  message("   ==> adding -DGRAPHICS_SUPPORT to the list of compiler definitions.")
endif()

if( ${OPTION__NETCDF_SUPPORT} )
  set( PREPROCESSOR_DEFINES ${PREPROCESSOR_DEFINES} " -DNETCDF_SUPPORT" )
  message("   ==> adding -DNETCDF_SUPPORT to the list of compiler definitions.")
endif()

if( ${OPTION__DEBUG_PRINT} )
  set( PREPROCESSOR_DEFINES ${PREPROCESSOR_DEFINES} " -DDEBUG_PRINT" )
  message("   ==> adding -DDEBUG_PRINT to the list of compiler definitions.")
endif()

if( ${OPTION__STREAM_INTERACTIONS} )
  set( PREPROCESSOR_DEFINES ${PREPROCESSOR_DEFINES} " -DSTREAM_INTERACTIONS" )
  message("   ==> adding -DSTREAM_INTERACTIONS to the list of compiler definitions.")
endif()

if( ${OPTION__STRICT_DATE_CHECKING} )
  set( PREPROCESSOR_DEFINES ${PREPROCESSOR_DEFINES} " -DSTRICT_DATE_CHECKING" )
  message("   ==> adding -DSTRICT_DATE_CHECKING to the list of compiler definitions.")
endif()

add_definitions( ${PREPROCESSOR_DEFINES} )
