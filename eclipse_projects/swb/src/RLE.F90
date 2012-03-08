!> @file
!> @brief  Contains a single module, @ref RLE, which
!>  provides data compression (run-length encoding) for internal data structures.

!> @brief  Provides data compression (run-length encoding) for internal data structures.
module RLE

use types

implicit none

contains

!--------------------------------------------------------------------------
!!****s* RLE/RLE_writeByte
! NAME
!   RLE_writeByte - Apply run-length encoding to a bytestream and write out.
!
! SYNOPSIS
!   Converts a real value to an integer, and applies
!   a run-length encoding scheme to compress output data.
!
! INPUTS
!   iLU - FORTRAN logical unit to which output should be written
!   rValue - real value from the datastream to be compressed.
!   iMultiplicationFactor - integer multiplication factor used to
!      convert the real to an integer.  Correlates to the number of
!      decimal places to preserve in the datastream.
!   lEOF - logical value - .TRUE. only if the datastream transmission has
!          ended.
!
! OUTPUTS
!   None.
!
! SOURCE

!> @brief  Converts a real value to an integer, and applies
!>  a run-length encoding scheme to compress output data.
subroutine RLE_writeByte(iLU, rValue, iRLE_MULT, rRLE_OFFSET, &
                    iByteTotal, iVarNum)

  ![ARGUMENTS]
  integer(kind=T_INT), intent(in) :: iLU
  real(kind=T_SGL),intent(in) :: rValue
  integer(kind=T_INT),intent(in) :: iRLE_MULT
  real(kind=T_SGL),intent(in) :: rRLE_OFFSET
  integer(kind=T_INT), intent(in) :: iByteTotal      ! total # of bytes in
                                                     ! original datastream
  integer(kind=T_INT), intent(in) :: iVarNum

  ![LOCALS]
  integer(kind=T_INT),dimension(1:iNUM_VARIABLES), save :: iRunCount
  integer(kind=T_INT),dimension(1:iNUM_VARIABLES), save :: iByteCount = 0
                                                ! total # of bytes fed into
                                                ! RLE_writeByte so far
  integer(kind=T_INT) :: i
  integer(kind=T_INT),dimension(1:iNUM_VARIABLES), save :: iPrevious, iCurr
  logical(kind=T_INT),dimension(1:iNUM_VARIABLES),save :: lRun

  ! iByteCount is incremented *EVERY* time this subroutine is called.
  ! This subroutine should be called a number of times that EXACTLY
  ! matches the number of items in the array to be compressed
  iByteCount(iVarNum) = iByteCount(iVarNum) + 1

  if(iByteCount(iVarNum) == 1) then

   iCurr(iVarNum) = NINT((rValue + rRLE_OFFSET) * iRLE_MULT)
	iRunCount(iVarNum) = 0
	lRun(iVarNum) = lFALSE

  else ! 1 < iByteCount <= iByteTotal

    ! iCurr from last iteration becomes iPrevious in this iteration

    iPrevious(iVarNum) = iCurr(iVarNum)

    iCurr(iVarNum) = NINT((rValue + rRLE_OFFSET) * iRLE_MULT)

    if(iPrevious(iVarNum) == iCurr(iVarNum)) then ! we have a new or existing run

      if(lRun(iVarNum)) then  ! EXISTING RUN
	    lRun(iVarNum) = lTRUE
        iRunCount(iVarNum) = iRunCount(iVarNum) + 1
      else  ! BEGIN A NEW RUN
        write(iLU) iPrevious(iVarNum)
        write(iLU) iCurr(iVarNum)
        lRun(iVarNum) = lTRUE
        iRunCount(iVarNum) = 0
      end if
	elseif(iPrevious(iVarNum)/=iCurr(iVarNum)) then
      if(lRun(iVarNum)) then  ! *END* EXISTING RUN
        write(iLU) iRunCount(iVarNum)
		lRun(iVarNum) = lFALSE
		iRunCount(iVarNum) = 0
      else  ! OK to write out iPrevious
        write(iLU) iPrevious(iVarNum)
        lRun(iVarNum) = lFALSE
        iRunCount(iVarNum) = 0
      end if

	endif

  endif


!#ifdef DEBUG_PRINT
!    write(UNIT=LU_LOG,FMT=*) iByteCount(iVarNum),"  rValue: ",real2char(rValue), &
!      ":  iPrev: ",iPrevious(iVarNum),"  iCurr: ", iCurr(iVarNum), "  lRun: ", &
!      lRun(iVarNum), "  iRunCount: ", iRunCount(iVarNum)
!#endif

  if(iByteCount(iVarNum) == iByteTotal) then  ! last byte of information... clean up and exit

      if(lRun(iVarNum)) then  ! EOF in middle of EXISTING RUN
      write(iLU) iRunCount(iVarNum)    ! - 1
#ifdef DEBUG_PRINT
	  write(UNIT=LU_LOG,FMT=*) "iByteCount == iByteTotal: EOF in EXISTING RUN"
#endif
	else
      write(iLU) iCurr(iVarNum)
#ifdef DEBUG_PRINT
  	  write(UNIT=LU_LOG,FMT=*) "iByteCount == iByteTotal: EOF: no existing run"
#endif
    end if

	! write EOF flag to binary file
    write(iLU) iEOF
    FLUSH(iLU)

	! reset byte count
	iByteCount(iVarNum) = 0

  endif

!#ifdef DEBUG_PRINT
!  write(UNIT=LU_LOG,FMT=*) iByteCount,":  iPrev: ",iPrevious,"  iCurr: ", iCurr, "  lRun: ", &
!     lRun, "  iRunCount: ", iRunCount
!#endif

  return

end subroutine RLE_writeByte

!!***
!--------------------------------------------------------------------------
!!****s* RLE/RLE_readByte
! NAME
!   RLE_readByte - Read and apply run-length encoding to a bytestream.
!
! SYNOPSIS
!   Converts a real value to an integer, and applies
!   a run-length encoding scheme to compress output data.
!
! INPUTS
!   iLU - FORTRAN logical unit to which output should be written
!   rValue - real value from the datastream to be compressed.
!   iMultiplicationFactor - integer multiplication factor used to
!      convert the real to an integer.  Correlates to the number of
!      decimal places to preserve in the datastream.
!   lEOF - logical value - .TRUE. only if the datastream transmission has
!          ended.
!
! OUTPUTS
!   None.
!
! SOURCE

!> @brief  Converts an integer value to a real value by decompressing a data stream
!>  by means of a run-length encoding scheme.
subroutine RLE_readByte(iLU,iRLE_MULT, rRLE_OFFSET, rValue, &
                        iByteTotal,lEOF)

  ![ARGUMENTS]
  integer(kind=T_INT), intent(in) :: iLU
  integer(kind=T_INT),intent(in) :: iRLE_MULT
  real(kind=T_SGL),intent(in) :: rRLE_OFFSET
  real(kind=T_SGL),dimension(iByteTotal), intent(out) :: rValue
  integer(kind=T_INT), intent(in) :: iByteTotal      ! total # of bytes in
                                                     ! original datastream
  logical(kind=T_LOGICAL),intent(out) :: lEOF

  ![LOCALS]
  integer(kind=T_INT), save :: iByteCount   ! total # of bytes reconstituted
                                            ! by RLE_readByte so far

  integer(kind=T_INT) iCurr, i, iRepeat, iStat
  integer(kind=T_INT), save :: iPrevious
  logical(kind=T_LOGICAL),save :: lRun
  lEOF = lFALSE


  ! begin reconstituting the array by assigning an initial value to
  ! iCurr
  read(iLU,iostat=iStat) iCurr  ! read in a byte of data
  call Assert(iStat == 0, &
     "Error reading input file: module RLE, subroutine RLE_readByte")
  lRun = lFALSE
  iByteCount = 0

  LOOP: do

    ! iCurr from last iteration becomes iPrevious in this iteration
    iPrevious = iCurr

    read(iLU,iostat=iStat) iCurr  ! read in a byte of data
    call Assert(iStat == 0, &
       "Error reading input file: module RLE, subroutine RLE_readByte")
    call Assert(LOGICAL(iCurr>=-HUGE(iCurr),kind=T_LOGICAL), &
        "Integer overflow; iLU= "//TRIM(int2char(iLU)) &
        //"; iCurr= "//TRIM(int2char(iCurr)) &
          //". Use a smaller value for RLE_MULTIPLIER", &
                trim(__FILE__),__LINE__)

	if(iPrevious /= iCurr) then  ! set rValue equal to out the value of iPrevious
	  iByteCount = iByteCount + 1

	  lRun = lFALSE

	  call Assert(LOGICAL( iByteCount <=iByteTotal, kind=T_LOGICAL), &
        "iByteCount exceeds iByteTotal: module RLE, subroutine RLE_readByte")

      rValue(iByteCount) = REAL(iPrevious, kind=T_SGL) &
                           / REAL(iRLE_MULT, kind=T_SGL) &
                           - rRLE_OFFSET

!      write(UNIT=LU_LOG,FMT=*) iByteCount,":  iPrev: ",iPrevious,"  iCurr: ", iCurr, "  rValue: ", &
!         rValue(iByteCount)

    else if(iCurr==iPrevious) then  ! we have a run

	  lRun = lTRUE

	  ! assign value of iCurr
      iByteCount = iByteCount + 1
      call Assert(LOGICAL( iByteCount <=iByteTotal, kind=T_LOGICAL), &
        "iByteCount exceeds iByteTotal", &
                trim(__FILE__),__LINE__)

!      rValue(iByteCount) = REAL(iCurr, kind=T_SGL) / REAL(iRLE_MULT, kind=T_SGL)

      rValue(iByteCount) = REAL(iCurr, kind=T_SGL) &
                           / REAL(iRLE_MULT, kind=T_SGL) &
                           - rRLE_OFFSET

      call Assert(LOGICAL(iCurr>=-iEOF,kind=T_LOGICAL), &
        "Read beyond end of file marker", &
                trim(__FILE__),__LINE__)

!      write(UNIT=LU_LOG,FMT=*) iByteCount,":  iPrev: ",iPrevious,"  iCurr: ", iCurr, "  rValue: ", &
!        rValue(iByteCount)


	  ! now read in the number of times to repeat iCurr
      read(iLU,iostat=iStat) iRepeat

	  ! if iRepeat is ZERO, loop will *NOT* execute
      do i=iRepeat,1,-1
        iByteCount = iByteCount + 1
	    call Assert(LOGICAL( iByteCount <=iByteTotal, kind=T_LOGICAL), &
          "iByteCount exceeds iByteTotal: module RLE, subroutine RLE_readByte", &
                trim(__FILE__),__LINE__)
!        rValue(iByteCount) = REAL(iCurr, kind=T_SGL) / REAL(iRLE_MULT, kind=T_SGL)

        rValue(iByteCount) = REAL(iCurr, kind=T_SGL) &
                           / REAL(iRLE_MULT, kind=T_SGL) &
                           - rRLE_OFFSET

        call Assert( iCurr >= -iEOF, &
         "Integer overflow; use a smaller value for RLE_MULTIPLIER", &
         TRIM(__FILE__), __LINE__)

!    	write(UNIT=LU_LOG,FMT=*) iByteCount,":  iPrev: ",iPrevious,"  iCurr: ", iCurr, "  rValue: ", &
!          rValue(iByteCount)

      end do

	else

	  call Assert(lFALSE,"Uncaught condition in IF-THEN block: module RLE.f95", &
	    TRIM(__FILE__), __LINE__)

	end if

    if(iByteCount == iByteTotal) then  ! we have already assigned the last
	                                   ! value to rValue....check for EOF

      call Assert( iCurr == iEOF, &
        "Expected to find an EOF marker in binary file", &
        TRIM(__FILE__), __LINE__)

	  exit LOOP

	end if

  end do LOOP

  return

end subroutine RLE_readByte

!!***

!-----------------------------------------------------------------------

end module RLE
