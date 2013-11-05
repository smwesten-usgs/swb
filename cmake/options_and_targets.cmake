# set build type; can be "RELEASE", "DEBUG", or "PROFILE"
set(BUILD_TYPE "Release")

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


