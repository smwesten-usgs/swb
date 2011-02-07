!> @mainpage SWB - A Modified Thornthwaite-Mather soil-water-balance Code for Estimating Groundwater Recharge
!>
!>
!>

!> \brief Main control routine makes calls to functions in model.
!!
!! Program SWB accepts command-line arguments and makes a single call
!! to the model_Run routine in module model.
program SWB

  use types
  use model

  implicit none

  character (len=256) :: sControlFile
  integer (kind=T_INT) :: iNumArgs

  ! warning - calling a Fortran 2003 extension function here
  iNumArgs = COMMAND_ARGUMENT_COUNT()

  if(iNumArgs/=1) then

    write(UNIT=*,FMT="(/,a,/)") &
      "Soil Water Balance Code -- compiled on: "// &
      TRIM(__DATE__) //" "// TRIM(__TIME__)
#ifdef __GFORTRAN__
    write(UNIT=*,FMT="(a,/)") "Compiled with GNU gfortran version "//TRIM(__VERSION__)
#endif

#ifdef __INTEL_COMPILER
    write(UNIT=*,FMT="(a,/)") "Compiled with Intel Fortran version " &
      //TRIM(int2char(__INTEL_COMPILER))
#endif

#ifdef __G95__
    write(UNIT=*,FMT="(a,/)") "Compiled with G95 minor version " &
      //TRIM(int2char(__G95_MINOR__))
#endif

    write(UNIT=*,FMT=*) "  Compilation options:"
    write(UNIT=*,FMT=*) " -------------------------------"
#ifdef STREAM_INTERACTIONS
    write(UNIT=*,FMT=*) "  STREAM_INTERACTIONS      yes"
#else
    write(UNIT=*,FMT=*) "  STREAM_INTERACTIONS       no"
#endif

#ifdef GRAPHICS_SUPPORT
    write(UNIT=*,FMT=*) "  GRAPHICS_SUPPORT         yes"
#else
    write(UNIT=*,FMT=*) "  GRAPHICS_SUPPORT          no"
#endif

#ifdef NETCDF_SUPPORT
    write(UNIT=*,FMT=*) "  NETCDF_SUPPORT           yes"
#else
    write(UNIT=*,FMT=*) "  NETCDF_SUPPORT            no"
#endif

#ifdef IRRIGATION_MODULE
    write(UNIT=*,FMT=*) "  IRRIGATION calculations  yes"
#else
    write(UNIT=*,FMT=*) "  IRRIGATION calculations   no"
#endif

#ifdef DEBUG_PRINT
    write(UNIT=*,FMT=*) "  DEBUG_PRINT              yes"
#else
    write(UNIT=*,FMT=*) "  DEBUG_PRINT               no"
#endif

    write(UNIT=*,FMT="(/,/,a,/)")    "Usage: swb [control file name]"

    stop

  end if

  ! warning - calling a Fortran 2003 extension function here
  call GET_COMMAND_ARGUMENT(1,sControlFile)

  ! Now, let's fire this baby up!
  call model_Run(sControlFile)

  stop
end program SWB
