!> @file
!>  Main program which references all other modules; execution begins here.


!>  Main program which references all other modules; execution begins here.
!>
!> Accepts command-line arguments and makes a single call
!> to the control_setModelOptions routine in module \ref control.
program main

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use types
  use control
  use iso_fortran_env

  implicit none

  character (len=256) :: sControlFile
  integer (kind=c_int) :: iNumArgs
  character (len=1024) :: sCompilerFlags
  character (len=256) :: sCompilerVersion
  logical             :: lExists

  iNumArgs = COMMAND_ARGUMENT_COUNT()

  if(iNumArgs/=1) then

    write(UNIT=*,FMT="(/,a,/)") &
      "Soil Water Balance Code version "//trim(SWB_VERSION)//" -- compiled on: "//trim(COMPILE_DATE)
#ifdef __GFORTRAN__
    sCompilerFlags = COMPILER_OPTIONS()
    sCompilerVersion = COMPILER_VERSION()
    write(UNIT=*,FMT="(a,/)") "Compiled with: gfortran ("//TRIM(sCompilerVersion)//")"
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

#ifdef STRICT_DATE_CHECKING
    write(UNIT=*,FMT="(a)") " STRICT_DATE_CHECKING        yes"
#else
    write(UNIT=*,FMT="(a)") " STRICT_DATE_CHECKING         no"
#endif

#ifdef DEBUG_PRINT
    write(UNIT=*,FMT="(a)") " DEBUG_PRINT                 yes"
#else
    write(UNIT=*,FMT="(a)") " DEBUG_PRINT                  no"
#endif

    write(UNIT=*,FMT="(/,/,a,/)")    "Usage: swb [control file name]"

  else

    call GET_COMMAND_ARGUMENT(1,sControlFile)

    ! pass control to control module
    call control_setModelOptions(sControlFile)

    close(unit=LU_LOG)

  endif

end program main
