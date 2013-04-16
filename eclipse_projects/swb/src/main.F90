!> @mainpage SWB - A Modified Thornthwaite-Mather Soil-Water-Balance Code for Estimating Groundwater Recharge
!>
!> The SWB model calculates recharge
!> by use of commonly available geographic information system
!> (GIS) data layers in combination with tabular climatological
!> data. The code is based on a modified Thornthwaite-Mather
!> soil-water-balance approach, with components of the soilwater
!> balance calculated at a daily timestep. Recharge
!> calculations are made on a rectangular grid of computational
!> elements that may be easily imported into a regional groundwater-
!> flow model. Recharge estimates calculated by the code
!> may be output as daily, monthly, or annual values.
!> @par
!> The code is written in Fortran 95/2003, and has been compiled
!> on Windows and Linux systems using the gfortran, g95, and
!> Intel fortran compilers.
!> @par
!> Optional support is provided for:
!> - Reading and writing NetCDF files
!> - Producing simple plots of model inputs and outputs
!> - Estimating irrigation amounts required to sustain plant growth
!> @par
!> External libraries are required if NetCDF file access or plotting
!> capabilities are desired. The fortran modules documented here
!> must be linked against the NetCDF and DISLIN libraries:
!> - NetCDF: http://www.unidata.ucar.edu/software/netcdf/
!> - DISLIN: http://www.mps.mpg.de/dislin/
!> @section Documentation
!> Westenbroek, S.M., Kelson, V.A., Dripps, W.R., Hunt, R.J.,
!> and Bradbury,K.R., 2010, SWB-A modified Thornthwaite-Mather
!> Soil-Water-Balance code for estimating groundwater recharge:
!> U.S. Geological Survey Techniques and Methods 6-A31, 60 p.
!> @par
!> http://pubs.usgs.gov/tm/tm6-a31/

!> @page Conventions
!> In order to make the code more easily understood, a set of rules
!> for naming variables, subroutines, functions, and parameters
!> was adopted.
!> @section Variables
!>  - \b integer: names begin with \em i \n
!>      example: iCount
!>  - \b real: names begin with \em r \n
!>      example: rValue
!>  - \b double precision: names begin with \em dp \n
!>      example: dpValue
!>  - \b logical: names begin with \em l \n
!>      example: lMatch
!>  - \b character: names begin with \em s \n
!>      example: sFileName
!>  - \b pointer: names begin with \em p; applies to pointer of any type \n
!>      example: pGrid
!> @section Parameters
!>  - Parameter names are generally entirely UPPERCASE letters
!>  - Normal parameter names begin with the letters specified above,
!>    depending on the type of the parameter
!>  - Constants used throughout the code to specify program options
!>    are composed of UPPERCASE letters without a type prefix
!>  - Constants specifying fortran logical unit numbers (for i/o) have prefix \em LU_
!>  - Parameters of derived type have prefix \em T_


!> @file
!> @brief  Main program which references all other modules; execution begins here.


!> @brief  Main program which references all other modules; execution begins here.
!>
!> Accepts command-line arguments and makes a single call
!> to the control_setModelOptions routine in module \ref control.
program main

  use types
  use control
  use ISO_FORTRAN_ENV

  implicit none

  character (len=256) :: sControlFile
  integer (kind=T_INT) :: iNumArgs
  character (len=512) :: sCompilerFlags

  ! warning - calling a Fortran 2003 extension function here
  iNumArgs = COMMAND_ARGUMENT_COUNT()

  if(iNumArgs/=1) then

    write(UNIT=*,FMT="(/,a,/)") &
      "Soil Water Balance Code version "//trim(SWB_VERSION)//" -- compiled on: "// &
      TRIM(__DATE__) //" "// TRIM(__TIME__)
#ifdef __GFORTRAN__
    write(UNIT=*,FMT="(a,/)") "Compiled with: GNU gfortran version "//TRIM(__VERSION__)
    sCompilerFlags = COMPILER_OPTIONS()
    write(UNIT=*,FMT="(a)") "Compiler flags:"
    write(UNIT=*,FMT="(a)") "-------------------------------"
    write(UNIT=*,FMT="(a,/)") TRIM(sCompilerFlags)
#endif

#ifdef __INTEL_COMPILER
    write(UNIT=*,FMT="(a)") "Compiled with: Intel Fortran version " &
      //TRIM(int2char(__INTEL_COMPILER))
      write(UNIT=*,FMT="(a,/)") "Compiler build date:"//TRIM(int2char(__INTEL_COMPILER_BUILD_DATE))
#endif

#ifdef __G95__
    write(UNIT=*,FMT="(a,/)") "Compiled with: G95 minor version " &
      //TRIM(int2char(__G95_MINOR__))
#endif

    write(UNIT=*,FMT="(a)") "Compilation options:"
    write(UNIT=*,FMT="(a)") "----------------------------------"
#ifdef STREAM_INTERACTIONS
    write(UNIT=*,FMT="(a)") " STREAM_INTERACTIONS         yes"
#else
    write(UNIT=*,FMT="(a)") " STREAM_INTERACTIONS          no"
#endif

#ifdef GRAPHICS_SUPPORT
    write(UNIT=*,FMT="(a)") " GRAPHICS_SUPPORT            yes"
#else
    write(UNIT=*,FMT="(a)") " GRAPHICS_SUPPORT             no"
#endif

#ifdef NETCDF_SUPPORT
    write(UNIT=*,FMT="(a)") " NETCDF_SUPPORT              yes"
#else
    write(UNIT=*,FMT="(a)") " NETCDF_SUPPORT               no"
#endif

#ifdef IRRIGATION_MODULE
    write(UNIT=*,FMT="(a)") " IRRIGATION calculations     yes"
#else
    write(UNIT=*,FMT="(a)") " IRRIGATION calculations      no"
#endif

#ifdef STRICT_DATE_CHECKING
    write(UNIT=*,FMT="(a)") " STRICT_DATE_CHECKING        yes"
#else
    write(UNIT=*,FMT="(a)") " STRICT_DATE_CHECKING         no"
#endif

#ifdef THORNTHWAITE_MATHER_TABLE
    write(UNIT=*,FMT="(a)") " THORNTHWAITE_MATHER_TABLE   yes"
#else
    write(UNIT=*,FMT="(a)") " THORNTHWAITE_MATHER_TABLE    no"
#endif


#ifdef DEBUG_PRINT
    write(UNIT=*,FMT="(a)") " DEBUG_PRINT                 yes"
#else
    write(UNIT=*,FMT="(a)") " DEBUG_PRINT                  no"
#endif

    write(UNIT=*,FMT="(/,/,a,/)")    "Usage: swb [control file name]"

    stop

  end if

  ! warning - calling a Fortran 2003 extension function here
  call GET_COMMAND_ARGUMENT(1,sControlFile)

  ! pass control to control module
  call control_setModelOptions(sControlFile)

  close(unit=LU_LOG)

  stop

end program main
