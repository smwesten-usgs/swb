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
  use version_control
  implicit none

  character (len=256)             :: sControlFile
  integer (c_int)            :: iNumArgs
  character (len=1024)            :: sCompilerFlags
  character (len=256)             :: sVersionString
  character (len=256)             :: sCompilerVersion
  character (len=256)             :: sCompilationDateString
  character (len=256)             :: sCompilationSystemString
  character (len=256)             :: sGitHashString
  logical                         :: lExists
  integer (c_int)            :: iCount
  integer (c_int), parameter :: NUM_REPEAT_CHARS = 36

  iNumArgs = COMMAND_ARGUMENT_COUNT()

  if(iNumArgs < 1) then


    sVersionString = "  Soil Water Balance Code version "//trim( SWB_VERSION )
    sCompilationDateString   = "    compilation date           : "              &
                                 //trim(COMPILE_DATE)//" "//trim(COMPILE_TIME)
    sCompilationSystemString = "    compiled on                : "//trim(SYSTEM_NAME)

    if (     (str_compare(SYSTEM_NAME,"Windows") )                           &
        .or. (str_compare(SYSTEM_NAME, "Mingw") ) ) then
      OS_NATIVE_PATH_DELIMITER = "\"
    else
      OS_NATIVE_PATH_DELIMITER = "/"
    endif

    sGitHashString = "    Git branch and commit hash : "                        &
                          //trim( adjustl(GIT_BRANCH_STRING ) )                 &
                          //", "//trim( GIT_COMMIT_HASH_STRING )

    iCount = max( max( len_trim( sVersionString ), len_trim( sGitHashString ) ), &
                  len_trim( sCompilationSystemString) )

    write(unit=*, fmt="(/,a)") repeat("-",iCount + 4)
    write(UNIT=*,FMT="(a,/)") trim( sVersionString )
    write(UNIT=*,FMT="(a)") trim( sCompilationDateString )
    write(UNIT=*,FMT="(a)") trim( sCompilationSystemString )
    write(UNIT=*,FMT="(a)") trim( sGitHashString )
    write(unit=*, fmt="(a,/)") repeat("-",iCount + 4)

#ifdef __GFORTRAN__
    sCompilerFlags = COMPILER_OPTIONS()
    sCompilerVersion = COMPILER_VERSION()
    write(UNIT=*,FMT="(a,/)") "compiled with: gfortran ("//TRIM(sCompilerVersion)//")"
    write(UNIT=*,FMT="(a)") "compiler flags:"
    write(UNIT=*,FMT="(a)") repeat("-", NUM_REPEAT_CHARS)
    write(UNIT=*,FMT="(a,/)") TRIM(sCompilerFlags)
#endif

#ifdef __INTEL_COMPILER
    write(UNIT=*,FMT="(a)") "compiled with: Intel Fortran version " &
      //TRIM(int2char(__INTEL_COMPILER))
      write(UNIT=*,FMT="(a,/)") "compiler build date:"//TRIM(int2char(__INTEL_COMPILER_BUILD_DATE))
#endif

#ifdef __G95__
    write(UNIT=*,FMT="(a,/)") "compiled with: G95 minor version " &
      //TRIM(int2char(__G95_MINOR__))
#endif

    write(UNIT=*,FMT="(a)") "compilation options:"
    write(UNIT=*,FMT="(t2,a)") repeat("-", NUM_REPEAT_CHARS)
#ifdef STREAM_INTERACTIONS
    write(UNIT=*,FMT="(t2,a)") "STREAM_INTERACTIONS         yes"
#else
    write(UNIT=*,FMT="(t2,a)") "STREAM_INTERACTIONS          no"
#endif

#ifdef STRICT_DATE_CHECKING
    write(UNIT=*,FMT="(t2,a)") "STRICT_DATE_CHECKING        yes"
#else
    write(UNIT=*,FMT="(t2,a)") "STRICT_DATE_CHECKING         no"
#endif

#ifdef DEBUG_PRINT
    write(UNIT=*,FMT="(t2,a)") "DEBUG_PRINT                 yes"
#else
    write(UNIT=*,FMT="(t2,a)") "DEBUG_PRINT                  no"
#endif

    write(UNIT=*,FMT="(/,/,a,/)")    "Usage: swb [control file name]"

  else

    call GET_COMMAND_ARGUMENT(1,sControlFile)

    ! pass control to control module
    call control_setModelOptions(sControlFile)

    close(unit=LU_LOG)

  endif

  ! exit with normal result code
  call exit_with_code( 0 )

end program main
