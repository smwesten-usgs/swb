module swbstats_support

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use types

  implicit none

  logical (kind=c_bool) :: lCUMULATIVE = lFALSE
  logical (kind=c_bool) :: lSUM = lFALSE
  logical (kind=c_bool) :: lPERIOD_SLICE = lFALSE
  logical (kind=c_bool) :: lRESET = lFALSE
  logical (kind=c_bool) :: lVERBOSE = lFALSE
  logical (kind=c_bool) :: lMASKSSF = lFALSE
  logical (kind=c_bool) :: lBASINSTATS = lFALSE
  logical (kind=c_bool) :: lMASKSTATS = lFALSE
  logical (kind=c_bool) :: lF_TO_C = lFALSE
  logical (kind=c_bool) :: lGAP_DIFFERENCE = lFALSE
  logical (kind=c_bool) :: lPLOT = lFALSE
  logical (kind=c_bool) :: lSTATS = lFALSE
  logical (kind=c_bool) :: lGRID = lFALSE
  logical (kind=c_bool) :: lBOTH = lFALSE
  logical (kind=c_bool) :: lANNUAL = lFALSE
  logical (kind=c_bool) :: lMONTHLY = lFALSE
  logical (kind=c_bool) :: lDAILY = lFALSE

  character (len=256) :: sStatsDescription = ""
  integer (kind=c_int) :: LU_STATS

  integer (kind=c_int) :: iSlcStartMM = 1
  integer (kind=c_int) :: iSlcStartDD = 1
  integer (kind=c_int) :: iSlcEndMM = 12
  integer (kind=c_int) :: iSlcEndDD = 31
  logical (kind=c_bool) :: lDATE_RANGE_ACTIVE

  character (len=78), dimension(33), parameter :: sUsageText = &
    [ "Usage: swbstats [binary file name]                                            ", &
      "                {GRID|PLOT|BOTH|STATS}                                        ", &
      "                {YEARLY|MONTHLY|DAILY}                                        ", &
      "                {SUM}                                                         ", &
      "                {SURFER}                                                      ", &
      "                {MASK mask_filename}                                          ", &
      "                {MASK_SSF mask_filename ssf_site_id mask_value}               ", &
      "                {CUMULATIVE}                                                  ", &
      "                {PERIOD_SLICE start_date (mm/dd) end_date (mm/dd)}            ", &
      "                {start_date (mm/dd/yyyy)}                                     ", &
      "                {end_date (mm/dd/yyyy)}                                       ", &
      "                {VERBOSE}                                                     ", &
      "                                                                              ", &
      "A filename MUST be specified. All other arguments are OPTIONAL.               ", &
      "                                                                              ", &
      "NOTES:                                                                        ", &
      "                                                                              ", &
      "  1) If no output frequency is provided (e.g. ANNUAL, MONTHLY, DAILY), a      ", &
      "     summary for the entire model simulation period is calculated.            ", &
      "  2) SUM: will calculate statistics based on SUMS rather than MEAN values.    ", &
      "  3) VERBOSE: writes daily min, mean, and max values to logfile.              ", &
      "  4) SURFER: directs output to Surfer grids rather than Arc ASCII grids.      ", &
      "  5) MASK: specifies a *single* mask file; stats are calculated for each      ", &
      "     cell for which the mask file contains an integer greater than zero.      ", &
      "  6) MASK_SSF: specifies a single mask file; generates a PEST site-sample     ", &
      "     file (*.SSF). User must specify a site label or id, and a mask value     ", &
      "     which must be met in order for the grid cell to be included in the       ", &
      "     calculation.                                                             ", &
      "  7) CUMULATIVE: specifies that MASK statistics are cumulative;               ", &
      "     these cumulative statistics are reset at the beginning of each year.     ", &
      "  8) PERIOD_SLICE: calculates stats on a specified subset of days each year.  ", &
      "     For example, 'PERIOD_SLICE 06/01 08/31' will report stats for the        ", &
      "     subset of output that occurs between June 1st and Aug. 31st of each year." ]

contains

!----------------------------------------------------------------------

!> This routine is designed to comb through the SWB output and calculate
!> statistics on the basis of the unique integer values found in the mask file.
subroutine CalcMaskStats(pGrd, pMaskGrd, pConfig, sVarName, sLabel, iNumDays)

  use types
  use graph
  use swb_grid

  implicit none

  type (T_GENERAL_GRID), pointer :: pGrd            ! pointer to model grid
  type (T_GENERAL_GRID), pointer :: pMaskGrd        ! pointer to grid mask to process
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings


  character(len=*) :: sVarName
  character (len=*) :: sLabel
  integer (kind=c_int), optional :: iNumDays

  ![LOCALS]
  integer (kind=c_int) :: j, k, iStat, iCount, m, n
  integer (kind=c_int) ::   iNumGridCells
  real (kind=c_float) :: rSum, rAvg, rMin, rMax
  real (kind=c_float) :: rBaseSum, rBaseAvg
  real (kind=c_double), dimension(:), allocatable, save :: rRunningSum
  !integer (kind=c_int), save :: i
  real (kind=c_double) :: rConversionFactor
  real (kind=c_float) :: rDenominator

  character (len=256) :: sBuf

  integer (kind=c_int), save :: iNumRecs

  if(present(iNumDays)) then
    rDenominator = real(iNumDays, kind=c_double)
  else
    rDenominator = 1_c_double
  end if

  if(pGrd%iLengthUnits == iGRID_LENGTH_UNITS_METERS) then
    rConversionFactor = 2589988.11_c_double
  else
    rConversionFactor = 27878400_c_double
  endif

  if(pConfig%lFirstDayOfSimulation) then

    iNumRecs = maxval(pMaskGrd%iData)

    if(lCUMULATIVE) then
      allocate(rRunningSum(iNumRecs))
      rRunningSum = 0.0_c_double
    endif

    sBuf = "SWB_"//trim(sVarName)//"_"//trim(sStatsDescription)//".txt"
    open(newunit=LU_STATS,FILE=trim(sBuf), &
          iostat=iStat, STATUS='REPLACE')

    call Assert ( iStat == 0, &
      "Could not open MASK statistics file "//dquote(sBuf))

    write(UNIT=LU_STATS,FMT="(A,a)",advance='NO') "Period",sTAB

    if(.not. lGAP_DIFFERENCE) then

      do k=1,iNumRecs - 1

        iCount = COUNT(pMaskGrd%iData == k)

        if (iCount > 0) &
          write(UNIT=LU_STATS,FMT="(A,a)",advance='NO') &
              trim(int2char(k)),sTAB
      end do

      write(UNIT=LU_STATS,FMT="(A)",advance='YES') trim(int2char(iNumRecs))

    else

      write(UNIT=LU_STATS,FMT="(A,a)",advance='NO') &
          trim(int2char(iNumRecs)),sTAB

      do k=1,iNumRecs - 2
        iCount = COUNT(pMaskGrd%iData == k)
        if (iCount > 0) &
          write(UNIT=LU_STATS,FMT="(A,a)",advance='NO') &
            trim(int2char(k)),sTAB
      end do

      write(UNIT=LU_STATS,FMT="(A)",advance='YES') &
          trim(int2char(iNumRecs - 1))

    endif

    pConfig%lFirstDayOfSimulation = lFALSE

  end if

  if(lDATE_RANGE_ACTIVE) write(UNIT=LU_STATS,FMT="(a,a)", advance='NO') TRIM(sLabel),sTAB

  if(lRESET .and. lCUMULATIVE) then
    rRunningSum = 0.0_c_double
  endif  

  if(.not. lGAP_DIFFERENCE) then

    do k = 1,iNumRecs

      iCount = COUNT(pMaskGrd%iData == k .and. pGrd%iMask == iACTIVE_CELL)

      if (iCount == 0) cycle

      ! sum of the sum of values within basin mask boundaries
      rSum = SUM(pGrd%rData,MASK=pMaskGrd%iData == k .and. pGrd%iMask == iACTIVE_CELL) / rDenominator
      rMax = MAXVAL(pGrd%rData,MASK=pMaskGrd%iData == k .and. pGrd%iMask == iACTIVE_CELL) / rDenominator
      rMin = MINVAL(pGrd%rData,MASK=pMaskGrd%iData == k .and. pGrd%iMask == iACTIVE_CELL) / rDenominator

      rAvg = rSum / iCount

      if(lVERBOSE) then
        write(UNIT=LU_LOG,FMT="(A)") ""
        write(UNIT=LU_LOG,FMT="(5x,A)") "Summary for cells with mask value of: " &
           //trim(int2char(k) )
        write(UNIT=LU_LOG,FMT="(5x,A)") "==> "//TRIM(sLabel)
        write(UNIT=LU_LOG,FMT="(5x,'Grid cell area (sq mi):', f14.2)") &
            real(iCount, kind=c_double) * pGrd%rGridCellSize * pGrd%rGridCellSize / rConversionFactor
        write(UNIT=LU_LOG,FMT="(8x,A7,i12)") "count:",iCount
        write(UNIT=LU_LOG,FMT="(8x,A7,f14.2)") "sum:",rSum
        write(UNIT=LU_LOG,FMT="(8x,A7,f14.2)") "avg:",rAvg
      endif

      if(lCUMULATIVE) then

        rRunningSum(k) = rRunningSum(k) + rAvg

          if (lVERBOSE) then
            write(UNIT=LU_LOG,FMT="(8x,A7,f14.2)") "running sum:", rRunningSum(k)
          endif

        if(lDATE_RANGE_ACTIVE) then

          if( k < iNumRecs ) then

            write(UNIT=LU_STATS,FMT="(g16.8,a)", advance='NO') rRunningSum(k),sTAB

          else

            write(UNIT=LU_STATS,FMT="(g16.8)") rRunningSum(k)

          endif

        endif

      else

        if(lDATE_RANGE_ACTIVE) then

          if( k < iNumRecs ) then

            write(UNIT=LU_STATS,FMT="(g16.8,a)", advance='NO') rAvg,sTAB

          else

            write(UNIT=LU_STATS,FMT="(g16.8)") rAvg

          endif

        endif

      endif

    enddo

  else  ! GAP_DIFFERENCE

    iCount = COUNT(pMaskGrd%iData == iNumRecs .and. pGrd%iMask == iACTIVE_CELL)

    ! sum of the sum of values within basin mask boundaries
    rBaseSum = SUM(pGrd%rData,MASK=pMaskGrd%iData == iNumRecs &
                  .and. pGrd%iMask == iACTIVE_CELL) / rDenominator

    rBaseAvg = rBaseSum / iCount

    if(lDATE_RANGE_ACTIVE) &
      write(UNIT=LU_STATS,FMT="(g16.8,a)", advance='NO') rBaseAvg,sTAB

    do k = 1,iNumRecs-1

      iCount = COUNT(pMaskGrd%iData == k .and. pGrd%iMask == iACTIVE_CELL)

      if (iCount == 0) cycle

      ! sum of the sum of values within basin mask boundaries
      rSum = SUM(pGrd%rData,MASK=pMaskGrd%iData == k .and. pGrd%iMask == iACTIVE_CELL) / rDenominator

      rAvg = rSum / iCount

      if(lVERBOSE) then
        write(UNIT=LU_LOG,FMT="(A)") ""
        write(UNIT=LU_LOG,FMT="(5x,A)") "Summary for cells with mask value of: " &
           //trim(int2char(k) )
        write(UNIT=LU_LOG,FMT="(5x,A)") "==> "//TRIM(sLabel)
        write(UNIT=LU_LOG,FMT="(5x,'Grid cell area (sq mi):', f14.2)") &
            real(iCount, kind=c_double) * pGrd%rGridCellSize * pGrd%rGridCellSize / rConversionFactor
        write(UNIT=LU_LOG,FMT="(8x,A7,i12)") "count:",iCount
        write(UNIT=LU_LOG,FMT="(8x,A7,f14.2)") "sum:",rSum
        write(UNIT=LU_LOG,FMT="(8x,A7,f14.2)") "avg:",rAvg
        write(UNIT=LU_LOG,FMT="(A)") REPEAT("-",80)
      endif

      if(lDATE_RANGE_ACTIVE) then

        if( k < iNumRecs -1 ) then

          write(UNIT=LU_STATS,FMT="(g16.8,a)", advance='NO') rBaseAvg  - rAvg,sTAB

        else

          write(UNIT=LU_STATS,FMT="(g16.8)") rBaseAvg - rAvg

        endif

      endif

    enddo

  endif

  if (lVERBOSE)  write(UNIT=LU_LOG,FMT="(A)") REPEAT("-",80)

  flush(UNIT=LU_STATS)

end subroutine CalcMaskStats

!------------------------------------------------------------------------------

subroutine CalcMaskStatsSSF(pGrd, pMaskGrd, pConfig, sVarName, iGridValue, sLabel, iNumDays)

  use types
  use graph
  use swb_grid

  implicit none

  type (T_GENERAL_GRID), pointer :: pGrd            ! pointer to model grid
  type (T_GENERAL_GRID), pointer :: pMaskGrd        ! pointer to grid mask to process
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings


  character(len=*) :: sVarName
  integer (kind=c_int) :: iGridValue
  character (len=*) :: sLabel
  integer (kind=c_int), optional :: iNumDays

  ![LOCALS]
  integer (kind=c_int) :: j, iStat, iCount, m, n
  integer (kind=c_int) ::   iNumGridCells
  real (kind=c_double) :: rSum, rAvg, rMin, rMax
  real (kind=c_double) :: rDenominator
  real (kind=c_double),save :: rConversionFactor
  real (kind=c_double),save :: rGridCellAreaSF

  character (len=256) :: sBuf

  integer (kind=c_int), save :: iNumRecs

  if(present(iNumDays)) then
    rDenominator = real(iNumDays, kind=c_double)
  else
    rDenominator = 1_c_double
  end if

  if(pConfig%lFirstDayOfSimulation) then

    ! conversion factor: grid cell sum to square feet
    if(pGrd%iLengthUnits == iGRID_LENGTH_UNITS_METERS) then
      rConversionFactor = 10.76391042_c_double
    else
      rConversionFactor = 1_c_double
    endif

    rGridCellAreaSF = real(pGrd%rGridCellSize, kind=c_double) &
      * real(pGrd%rGridCellSize, kind=c_double) * rConversionFactor

    sBuf = "SWB_"//trim(sVarName)//"_"//trim(sStatsDescription)//".ssf"
    open(newunit=LU_STATS,FILE=trim(sBuf), &
          iostat=iStat, STATUS='REPLACE')
    call Assert ( iStat == 0, &
      "Could not open PEST ssf file "//dquote(sBuf))

    pConfig%lFirstDayOfSimulation = lFALSE

  end if

  iCount = COUNT(pMaskGrd%iData == iGridValue .and. pGrd%iMask == iACTIVE_CELL)

  ! sum of the sum of values within basin mask boundaries
  rSum = SUM(pGrd%rData,MASK=pMaskGrd%iData == iGridValue .and. pGrd%iMask == iACTIVE_CELL) / rDenominator
  rMax = MAXVAL(pGrd%rData,MASK=pMaskGrd%iData == iGridValue .and. pGrd%iMask == iACTIVE_CELL) / rDenominator
  rMin = MINVAL(pGrd%rData,MASK=pMaskGrd%iData == iGridValue .and. pGrd%iMask == iACTIVE_CELL) / rDenominator

  rAvg = rSum / iCount

  if(lVERBOSE) then
    write(UNIT=LU_LOG,FMT="(A)") ""
    write(UNIT=LU_LOG,FMT="(5x,A)") "Summary for cells with mask value of: " &
       //trim(int2char(iGridValue) )
    write(UNIT=LU_LOG,FMT="(A)") "==> "//TRIM(sLabel)
    write(UNIT=LU_LOG,FMT="(2x,A,t30,i12)") "count:",iCount
    write(UNIT=LU_LOG,FMT="(2x,a,t30,f16.3)") "cell area:", &
       rGridCellAreaSF
    write(UNIT=LU_LOG,FMT="(2x,a,t30,f16.3)") "sum cell area (sq ft):", &
      real(iCount, kind=c_double) * rGridCellAreaSF
    write(UNIT=LU_LOG,FMT="(2x,A,t30,g14.4)") "sum:",rSum
    write(UNIT=LU_LOG,FMT="(2x,A,t30,g14.4)") "avg:",rAvg
    write(UNIT=LU_LOG,FMT="(2x,'avg (cfs):',t30,g14.4)") rSum * rGridCellAreaSF / 86400_c_double / 12_c_double
    write(UNIT=LU_LOG,FMT="(A)") REPEAT("-",80)
  endif

  if(lDATE_RANGE_ACTIVE) write(UNIT=LU_STATS,FMT="(a,3x,g16.8)") TRIM(sLabel), &
     rSum * rGridCellAreaSF / 86400_c_double / 12_c_double

  flush(UNIT=LU_STATS)

end subroutine CalcMaskStatsSSF

end module swbstats_support

!------------------------------------------------------------------------------

program swbstats

use types
use data_factory
use graph
use stats
use swb_grid
use RLE
use swbstats_support
implicit none

  ! general temporary variables
  character (len=256)  :: sBinFile, sBuf, sBuf2, sBuf3 = ""
  character (len=256)  :: sOutputFilename = ""
  character (len=256)  :: sItem
  integer (kind=c_int) :: iNumArgs
  integer (kind=c_int) :: iNumGridCells
  integer (kind=c_int) :: iStat
  integer (kind=c_int) :: i, k
  integer (kind=c_int) :: iIndex
  integer (kind=c_int) :: iDateNum = 0
  integer (kind=c_int) :: iNumDaysInYear
  integer (kind=c_int) :: iLen1, iLen2
  integer (kind=c_int), dimension(2) :: iTempDate
  character (len=256) :: sSiteNumber
  character (len=7) :: sLengthUnits
  integer (kind=c_int) :: iPos

  ! variables that are read in from the binary file header
  integer (kind=c_int) :: iNX
  integer (kind=c_int) :: iNY
  integer (kind=c_int) :: iDataType
  real (kind=c_double)    :: rGridCellSize
  integer (kind=c_int) :: iLengthUnits
  integer (kind=c_int) :: iVariableNumber
  integer (kind=c_int) :: iRLE_MULT
  real (kind=c_float)    :: rRLE_OFFSET
  real (kind=c_double)    :: rX0, rX1
  real (kind=c_double)    :: rY0, rY1
  real (kind=c_double)    :: rX0_cntr, rX1_cntr
  real (kind=c_double)    :: rY0_cntr, rY1_cntr
  integer (kind=c_int) :: iStartMM, iStartDD, iStartYYYY
  integer (kind=c_int) :: iEndMM, iEndDD, iEndYYYY
  integer (kind=c_int) :: iPROJ4_length
  character (len=256) :: sPROJ4_string

  integer (kind=c_int) :: iCurrMM, iCurrDD, iCurrYYYY, iCurrDOY, iCurrJD, iTemp
  integer (kind=c_int) :: iTomorrowMM, iTomorrowDD, iTomorrowYYYY, iTomorrowDOY, iTomorrowJD
  integer (kind=c_int) :: iTempStartDate, iTempEndDate
  character (len=256) :: sMonthName = ""
  logical (kind=c_bool) :: lMonthEnd
  logical (kind=c_bool) :: lYearEnd

  integer (kind=c_int) :: LU_SWBSTATS

  character (len=256) :: sTitleTxt
  character (len=10) :: sDateTxt

  type ( T_GENERAL_GRID ),pointer :: pGrd
  type ( T_GENERAL_GRID ),pointer :: pMaskGrd
  type ( T_GENERAL_GRID ),pointer :: pMonthGrd
  type ( T_GENERAL_GRID ),pointer :: pMonthGrdMean
  type ( T_GENERAL_GRID ),pointer :: pYearGrd
  type ( T_GENERAL_GRID ),pointer :: pYearGrdMean
  type ( T_GENERAL_GRID ),pointer :: pSummaryGrd
  type ( T_GENERAL_GRID ),pointer :: pSummaryGrdMean

  integer (kind=c_int) :: iSWBStatsStartDate, iSWBStatsStartMM, &
                          iSWBStatsStartDD,iSWBStatsStartYYYY
  integer (kind=c_int) :: iSWBStatsEndDate, iSWBStatsEndMM, &
                          iSWBStatsEndDD,iSWBStatsEndYYYY
!  integer (kind=c_int) :: iSWBStatsType = iNONE
!  integer (kind=c_int) :: iStatsType = iMEAN

  character (len=15) :: sSWBCompileDate

  real(kind=c_float),dimension(:), allocatable :: rVal,rValSum,rPad, rValTmp

  character (len=8) :: sDate
  character (len=10) :: sTime
  character (len=5) :: sTZ

  character (len=1) :: sSlash = "/"
  character (len=20) :: sOutputFilePrefix = ""
  character (len=20) :: sOutputFileSuffix = "asc"
  character (len=256) :: sVarName
  character (len=256) :: sLabel = ""

  integer (kind=c_int) :: iOutputFormat = OUTPUT_ARC
  logical (kind=c_bool) :: lYearBegin
  logical (kind=c_bool) :: lEOF
  logical (kind=c_bool) :: lPrematureEOF = lFALSE
  integer (kind=c_int) :: iMonthCount, iYearCount, iPeriodCount
  integer (kind=c_int) :: iGridCellValue
  character (len=1) :: sSingleChar
  character (len=512) :: sCommandText
  integer (kind=c_int) :: iSWBStatsOutputType = iNONE
  integer (kind=c_int) :: iRow, iCol
  integer (kind=c_int) :: LU


  integer (kind=c_int), parameter :: MAX_NUM_RETRIES = 4

  !> Global instantiation of a pointer of type T_MODEL_CONFIGURATION
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings

  ALLOCATE (pConfig, STAT=iStat)
  call Assert( iStat == 0, &
     "Could not allocate memory for model control data structure")

  call date_and_time(sDate,sTime,sTZ)
  call GET_COMMAND(sCommandText)

  write(sBuf,FMT=*) "SWBSTATS_LOGFILE_"//sDate//"_"//sTime(1:6)//".txt"

  ! open up the log file
  open(LU_LOG, file=TRIM(ADJUSTL(sBuf)),iostat=iStat,&
      status='REPLACE')
  call Assert( iStat == 0, "Problem opening log file file for output.")

  write(UNIT=LU_LOG,FMT=*) "Soil Water Balance Code BINARY FILE READER compiled on: "// &
    TRIM(__DATE__) //" "// TRIM(__TIME__)
  write(UNIT=LU_LOG,FMT=*) "SWB reader execution started: "// &
    sDate//"_"//sTime(1:6)
  write (unit=LU_LOG, fmt="(/,a)") "SWB reader was started with the following command line:"
  write (unit=LU_LOG, fmt="(a,/)") dquote(sCommandText)

  flush(unit=LU_LOG)

  iNumArgs = COMMAND_ARGUMENT_COUNT()

  if(iNumArgs < 1) then

    write(UNIT=*,FMT="(/,a,/)") &
      "Soil Water Balance Code - statistics calculator -- compiled on: "// &
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

    do i=1,ubound(sUsageText,1)
      write(unit=*,fmt="(a)") sUsageText(i)
    enddo

    stop

  end if

  call GET_COMMAND_ARGUMENT(1,sBinFile)

  iIndex = 1

  !> Open the binary file; attempt to open file MAX_NUM_RETRIES times
  !> in the event that activity on the CPU and HDD has prevented the output file
  !> from being opened correctly the first time around
  do

    open(nextunit(LU_SWBSTATS), FILE=TRIM(sBinFile),FORM='UNFORMATTED', &
       status='OLD',ACCESS='STREAM', ACTION='READWRITE', IOSTAT=iStat )

    if( iIndex > MAX_NUM_RETRIES .or. iStat == 0) exit

    iIndex = iIndex + 1
    call sleep(5)

  enddo

  call Assert(iStat==0,"Failed to open input binary file: "//&
    TRIM(sBinFile),TRIM(__FILE__),__LINE__)

  sPROJ4_string = ""
  read(UNIT=LU_SWBSTATS) iNX             ! Number of cells in the x-direction
  read(UNIT=LU_SWBSTATS) iNY             ! Number of cells in the y-direction
  read(UNIT=LU_SWBSTATS) iDataType       ! Type of the grid
  read(UNIT=LU_SWBSTATS) rGridCellSize   ! size of one side of a grid cell
  read(UNIT=LU_SWBSTATS) iLengthUnits    ! length units code
  read(UNIT=LU_SWBSTATS) sSWBCompileDate ! date of SWB compilation;
  read(UNIT=LU_SWBSTATS) iVariableNumber ! STAT_INFO variable number
  read(UNIT=LU_SWBSTATS) iRLE_MULT       ! RLE Multiplier
  read(UNIT=LU_SWBSTATS) rRLE_OFFSET     ! RLE Offset
  read(UNIT=LU_SWBSTATS) rX0, rX1        ! World-coordinate range in X
  read(UNIT=LU_SWBSTATS) rY0, rY1        ! World-coordinate range in Y
  read (unit=LU_SWBSTATS) iPROJ4_length
  sPROJ4_string = ""

  do i=1, iPROJ4_length
    read (unit=LU_SWBSTATS) sSingleChar
    sPROJ4_string(i:i) = sSingleChar
  enddo

  pGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, GRID_DATATYPE_REAL)
  ! copy some key information into the grid data structure
  pGrd%iDataType =iDataType              ! Type of the grid
  pGrd%rGridCellSize = rGridCellSize     ! size of one side of a grid cell
  pGrd%iLengthUnits = iLengthUnits       ! length units code

  ! this will overwrite values in the pGrd data structure with each iteration
  do iRow=1, iNY
    do iCol=1, iNX
      read(UNIT=LU_SWBSTATS) pGrd%iMask(iCol, iRow)
    enddo
  enddo

  ! give a warning if SWB compilation date is different from the SWBSTATS 
  ! compilation date; different compilation dates could cause problems if
  ! the binary file format differs between the two.
  if (trim(sSWBCompileDate) /= trim(COMPILE_DATE) ) &
    call warn("The SWB compilation date used to create the binary file" &
     //" is different from the compilation date of the SWBSTATS reader.")

  read(UNIT=LU_SWBSTATS) iStartMM, iStartDD, iStartYYYY
  read(UNIT=LU_SWBSTATS) iEndMM, iEndDD, iEndYYYY

  rX0_cntr = rX0 + rGridCellSize /2.
  rX1_cntr = rX1 - rGridCellSize /2.
  rY0_cntr = rY0 + rGridCellSize /2.
  rY1_cntr = rY1 - rGridCellSize /2.

  ! check to see if binary file was closed normally or not. If SWB
  ! stopped prematurely, iEndMM, iEndDD, iEndYYYY will all have the
  ! value "9999". We need to check for this and overwrite with a
  ! legal date value far into the future.
  if(iEndMM == 9999 .or. iEndDD == 9999 .or. iEndYYYY == 9999) then
    iEndMM = 12
    iEndDD = 31
    iEndYYYY = 2199
    lPrematureEOF = lTRUE
  endif

  ! set default values for program options
  iSWBStatsStartDate = julian_day ( iStartYYYY, iStartMM, iStartDD)
  iSWBStatsEndDate = julian_day ( iEndYYYY, iEndMM, iEndDD)

  iNumGridCells = iNX * iNY

  allocate(rVal(iNumGridCells))
  allocate(rValTmp(iNumGridCells))
  allocate(rValSum(iNumGridCells))
  allocate(rPad(iNumGridCells))

  pMaskGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, GRID_DATATYPE_INT)
  pMaskGrd%sPROJ4_string = trim(sPROJ4_string)
  pMaskGrd%iMask = pGrd%iMask

  pMonthGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, GRID_DATATYPE_REAL)
  pMonthGrd%iMask = pGrd%iMask

  pMonthGrdMean => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, GRID_DATATYPE_REAL)
  pMonthGrdMean%iMask = pGrd%iMask

  pYearGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, GRID_DATATYPE_REAL)
  pYearGrd%iMask = pGrd%iMask

  pYearGrdMean => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, GRID_DATATYPE_REAL)
  pYearGrdMean%iMask = pGrd%iMask

  pSummaryGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, GRID_DATATYPE_REAL)
  pSummaryGrd%iMask = pGrd%iMask

  pSummaryGrdMean => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, GRID_DATATYPE_REAL)
  pSummaryGrdMean%iMask = pGrd%iMask

  STAT_INFO(iVariableNumber)%iMonthlyOutput = iNONE
  STAT_INFO(iVariableNumber)%iDailyOutput = iNONE
  STAT_INFO(iVariableNumber)%iAnnualOutput = iNONE

  rPad = -9999_c_float

  do LU=LU_LOG, LU_STD_OUT, -1

    write(unit=LU,fmt="(/,'Information about binary file ',a)") &
       TRIM(sBinFile)

    if(lPrematureEOF) then
      write(unit=LU,fmt="(/,'  File ends prematurely - cannot determine the ending date ')")
      write(unit=LU,fmt="(/,'  Starting date is ',i02.2,'/',i02.2,'/',i04.4)") &
        iStartMM, iStartDD, iStartYYYY
    else
      write(unit=LU,fmt="(/,'  Dates range from ',i02.2,'/',i02.2,'/',i04.4,"// &
        "' to ',i02.2,'/',i02.2,'/',i04.4)") iStartMM, iStartDD, iStartYYYY, &
        iEndMM, iEndDD, iEndYYYY
    endif
    write(unit=LU,fmt="(/,'  Contains SWB output for ',a,': ',a,' (',a,')')") &
      TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
      TRIM(STAT_INFO(iVariableNumber)%sLongName), &
      TRIM(STAT_INFO(iVariableNumber)%sUNITS)
    write(unit=LU,fmt="(/,'  Grid dimensions (NX, NY): ',t42,a,', ',a)") &
      trim(int2char(iNX)), trim(int2char(iNY))
    write(unit=LU,fmt="(/,'  Grid bounds (lower left: X0, Y0): ',t42,f14.2,2X,f14.2)") &
      rX0, rY0
    write(unit=LU,fmt="('  Grid bounds (lower left: X1, Y1): ',t42,f14.2,2X,f14.2)") &
      rX1, rY1
    write(sBuf,fmt="(f14.2)") rGridCellSize
    write(unit=LU,fmt="(/,'  Grid cell size: ',t28,a)") trim(adjustl(sBuf))


    if(iLengthUnits == iGRID_LENGTH_UNITS_METERS) then
      write(unit=LU,fmt="('  Grid cell units: ',t28,'meters')")
    else
      write(unit=LU,fmt="('  Grid cell units: ',t28,'feet')")
    endif

    write(unit=LU,fmt="(/,'  Grid data type: ',t28,a)") trim(int2char(iDataType))
    write(unit=LU,fmt="('  Length units code: ',t28,a,/)") trim(int2char(iLengthUnits))
    if (iPROJ4_length > 0) &
      write(unit=LU,fmt="('  PROJ4 string: ',/,'  ',a,/)") trim(sPROJ4_string)
    write(unit=LU, fmt="('  ',a, ' cells are inactive (of ',a,' total cells)')") &
      trim(asCharacter(count(pGrd%iMask == iINACTIVE_CELL))), &
      trim(asCharacter(iNumGridCells))
    write(unit=LU,fmt="(/,'  RLE Offset: ',t42,a)") trim(real2char( rRLE_OFFSET ))
    write(unit=LU,fmt="('  RLE Multiplier: ',t42,a)") trim(int2char( iRLE_MULT ))


    if (iNumArgs /= 1) exit

  end do

  if (iNumArgs == 1) stop

  sVarName = TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)

  i=2

  do

   call GET_COMMAND_ARGUMENT(i,sBuf)
    sBuf2 = TRIM(ADJUSTL(sBuf))  ! make a copy
    sBuf3 = ""

   ! the following will be used later to test whether we've found a possible date
   ! if there is no slash found, the string initially found in sBuf2 will be
   ! copied in its entirety to sBuf3; therefore iLen1 and iLen2 will be equal if
   ! no slash (i.e. no date) is found
   iLen1 = len_trim(ADJUSTL(sBuf2))
   call Chomp_slash(sBuf2,sBuf3)
   iLen2 = len_trim(ADJUSTL(sBuf3))

    if( str_compare(sBuf, "GRID") ) then
      lGRID = lTRUE

    elseif( str_compare(sBuf, "PLOT") &
      .or. str_compare(sBuf, "GRID") ) then
      lPLOT = lTRUE

    elseif( str_compare(sBuf, "BOTH") ) then
!      STAT_INFO(iVariableNumber)%iAnnualOutput = iBOTH
      lBOTH = lTRUE

    elseif( str_compare(sBuf, "STATS") ) then
!      STAT_INFO(iVariableNumber)%iAnnualOutput = iSTATS
      lSTATS = lTRUE

    elseif( str_compare(sBuf, "SUM") ) then
      lSUM = lTRUE

    elseif( str_compare(sBuf, "F_TO_C") ) then
      lF_TO_C = lTRUE

    elseif( str_compare(sBuf, "GAP_DIFFERENCE") ) then
      lGAP_DIFFERENCE = lTRUE

    elseif( str_compare(sBuf, "YEARLY") &
      .or. str_compare(sBuf, "ANNUAL") ) then
      lANNUAL = lTRUE

    elseif( str_compare(sBuf, "MONTHLY") ) then
      lMONTHLY = lTRUE

    elseif( str_compare(sBuf, "DAILY") ) then
      lDAILY = lTRUE

    elseif( str_compare(sBuf, "VERBOSE") ) then
      lVERBOSE = lTRUE

    elseif( str_compare(sBuf, "CUMULATIVE") ) then
        lCUMULATIVE = lTRUE

    elseif( str_compare(sBuf, "SURFER") ) then
      iOutputFormat = OUTPUT_SURFER
      sOutputFileSuffix = "grd"

    elseif(iLen1 /= iLen2) then  ! we have found forward slashes...probably a date
      iDateNum = iDateNum + 1
      call Assert(iDateNum <=2, "Too many dates entered on the command line", &
        TRIM(__FILE__),__LINE__)
      iTempDate(iDateNum) = mmddyyyy2julian(sBuf)

    elseif( str_compare(sBuf, "PERIOD_SLICE") ) then
        lPERIOD_SLICE = lTRUE
        i = i + 1
        call GET_COMMAND_ARGUMENT(i,sBuf)
        call chomp(sBuf,sItem,"/-")
        read(sItem,*) iSlcStartMM
        read(sBuf,*) iSlcStartDD

        i = i + 1
        call GET_COMMAND_ARGUMENT(i,sBuf)
        call chomp(sBuf,sItem,"/-")
        read(sItem,*) iSlcEndMM
        read(sBuf,*) iSlcEndDD

    elseif( str_compare(sBuf, "MASK") ) then
      lMASKSTATS = lTRUE
!      STAT_INFO(iVariableNumber)%iAnnualOutput = iSTATS
      lDAILY = lTRUE
      lSTATS = lTRUE
      i = i + 1
      call GET_COMMAND_ARGUMENT(i,sBuf)

      call DAT(MASK_DATA)%initialize(sDescription="Mask file: "//dquote(sBuf), &
        sFileType="ARC_GRID", &
        sFilename=trim(sBuf), &
        iDataType=DATATYPE_INT )
      call DAT(MASK_DATA)%set_PROJ4(trim(sPROJ4_string))

      call DAT(MASK_DATA)%getvalues( pGrdBase=pMaskGrd, iValues=pMaskGrd%iData)

    elseif( str_compare(sBuf, "MASK_SSF") ) then
        lMASKSSF = lTRUE
        lSTATS = lTRUE
        lDAILY = lTRUE

        i = i + 1
        call GET_COMMAND_ARGUMENT(i,sBuf)

        call DAT(MASK_DATA)%initialize(sDescription="Mask file: "//dquote(sBuf), &
          sFileType="ARC_GRID", &
          sFilename=trim(sBuf), &
          iDataType=DATATYPE_INT )

        i = i + 1
        call GET_COMMAND_ARGUMENT(i,sBuf)
        sSiteNumber = trim(sBuf)
        call assert (len_trim(sSiteNumber) > 0, "Must supply a site id" &
        //" name or number to associate with the summarized data.")

        i = i + 1
        call GET_COMMAND_ARGUMENT(i,sBuf)
        read(sBuf,fmt=*, iostat=iStat) iGridCellValue
        call assert (iStat == 0, "Must supply an integer value" &
          //" that represents the mask values you wish to have included in" &
          //"~summary calculations.")

        call DAT(MASK_DATA)%set_PROJ4(trim(sPROJ4_string))
        call DAT(MASK_DATA)%getvalues( pGrdBase=pMaskGrd, iValues=pMaskGrd%iData)

    endif

    i = i + 1
    if (i > iNumArgs) exit

  end do

  ! end of loop to process command line arguments
  ! begin main processing section

  if (lGRID) iSWBStatsOutputType = iGRID
  if (lPLOT) iSWBStatsOutputType = iGRAPH
  if (lBOTH) iSWBStatsOutputType = iBOTH
  if (lSTATS) iSWBStatsOutputType = iSTATS

  if (lMONTHLY)  STAT_INFO(iVariableNumber)%iMonthlyOutput = iSWBStatsOutputType
  if (lANNUAL)  STAT_INFO(iVariableNumber)%iAnnualOutput = iSWBStatsOutputType
  if (lDAILY)  STAT_INFO(iVariableNumber)%iDailyOutput = iSWBStatsOutputType

  call assert (iSWBStatsOutputType /= iNONE, &
    "Output type must be defined as one of: GRID, GRAPH/PLOT, BOTH, or STATS")

  call Assert(iDateNum == 0 .or. iDateNum == 2, &
    "Two dates must be entered in order to perform analysis on a subset of the data", &
    TRIM(__FILE__),__LINE__)

  if(iDateNum ==2) then
    iTempStartDate = minval(iTempDate)
    iTempEndDate = maxval(iTempDate)

    call Assert(iTempStartDate >= iSWBStatsStartDate, &
      "Your specified start date for data slicing begins before first SWB output", &
      TRIM(__FILE__),__LINE__)

    call Assert(iTempEndDate <= iSWBStatsEndDate, &
      "Your specified end date for data slicing ends after last SWB output", &
      TRIM(__FILE__),__LINE__)
    ! O.K. Dates pass the smell test and appear legitimate. Override default values.
    iSWBStatsEndDate = iTempEndDate
    iSWBStatsStartDate = iTempStartDate
  endif

  call gregorian_date(iSWBStatsStartDate, iSWBStatsStartYYYY, &
       iSWBStatsStartMM, iSWBStatsStartDD)

  call gregorian_date(iSWBStatsEndDate, iSWBStatsEndYYYY, &
       iSWBStatsEndMM, iSWBStatsEndDD)

  write(unit=LU_STD_OUT,fmt="(/,'  Analysis range from ',i02.2,'/',i02.2,'/',i04.4,"// &
      "' to ',i02.2,'/',i02.2,'/',i04.4)") iSWBStatsStartMM, iSWBStatsStartDD, iSWBStatsStartYYYY, &
      iSWBStatsEndMM, iSWBStatsEndDD, iSWBStatsEndYYYY

  if(lPERIOD_SLICE) then

    write(sStatsDescription,fmt="(i02.2,'-',i02.2,"// &
      "'_to_',i02.2,'-',i02.2,'_for_years_',i04.4,'-',i04.4)") &
      iSlcStartMM, iSlcStartDD, iSlcEndMM, iSlcEndDD, iSWBStatsStartYYYY, iSWBStatsEndYYYY

  else

    write(sStatsDescription,fmt="(i02.2,i02.2,i04.4,'-',i02.2,i02.2,i04.4)") &
      iSWBStatsStartMM, iSWBStatsStartDD, iSWBStatsStartYYYY, &
      iSWBStatsEndMM, iSWBStatsEndDD, iSWBStatsEndYYYY

  endif

  if(lCUMULATIVE) sStatsDescription = trim(sStatsDescription) // "_" &
    //"CUMULATIVE"

  if(lMASKSSF) sStatsDescription = trim(sStatsDescription) // "_" &
    //trim(sSiteNumber)

  ! output a summary of the calculations that will be generated
  do LU=LU_STD_OUT, LU_LOG

    write(unit=LU,fmt="(/,a,/)") "  Summary of output to be generated:"

    write(unit=LU,fmt="(t20,a,t28,a,t36,a,t44,a)") &
      "NONE","PLOT","GRID","STATS"

    k = STAT_INFO(iVariableNumber)%iDailyOutput
    if(k==iNONE) write(unit=LU,fmt="(t3,a,t20,a,t28,a,t36,a)") "Daily"," **","",""
    if(k==iGRID) write(unit=LU,fmt="(t3,a,t20,a,t28,a,t36,a)") "Daily","",""," **"
    if(k==iGRAPH) write(unit=LU,fmt="(t3,a,t20,a,t28,a,t36,a)") "Daily",""," **",""
    if(k==iBOTH) write(unit=LU,fmt="(t3,a,t20,a,t28,a,t36,a)") "Daily",""," **"," **"
    if(k==iSTATS) write(unit=LU,fmt="(t3,a,t20,a,t28,a,t36,a,t44,a)") "Daily",""," ",""," **"

    k = STAT_INFO(iVariableNumber)%iMonthlyOutput
    if(k==iNONE) write(unit=LU,fmt="(t3,a,t20,a,t28,a,t36,a)") "Monthly"," **","",""
    if(k==iGRID) write(unit=LU,fmt="(t3,a,t20,a,t28,a,t36,a)") "Monthly","",""," **"
    if(k==iGRAPH) write(unit=LU,fmt="(t3,a,t20,a,t28,a,t36,a)") "Monthly",""," **",""
    if(k==iBOTH) write(unit=LU,fmt="(t3,a,t20,a,t28,a,t36,a)") "Monthly",""," **"," **"
    if(k==iSTATS) write(unit=LU,fmt="(t3,a,t20,a,t28,a,t36,a,t44,a)") "Monthly",""," ",""," **"

    k = STAT_INFO(iVariableNumber)%iAnnualOutput
    if(k==iNONE) write(unit=LU,fmt="(t3,a,t20,a,t28,a,t36,a)") "Annual"," **","",""
    if(k==iGRID) write(unit=LU,fmt="(t3,a,t20,a,t28,a,t36,a)") "Annual","",""," **"
    if(k==iGRAPH) write(unit=LU,fmt="(t3,a,t20,a,t28,a,t36,a)") "Annual",""," **",""
    if(k==iBOTH) write(unit=LU,fmt="(t3,a,t20,a,t28,a,t36,a)") "Annual",""," **"," **"
    if(k==iSTATS) write(unit=LU,fmt="(t3,a,t20,a,t28,a,t36,a,t44,a)") "Annual",""," ",""," **"

  enddo

  !------------------------------------------------------------------------------------------
  ! BEGIN writing output
  !------------------------------------------------------------------------------------------

!  rewind(STAT_INFO(iVariableNumber)%iLU)
!  write(STAT_INFO(iVariableNumber)%iLU,POS=iENDHEADER_POS)

  write(LU_LOG, &
     fmt='("CALCULATING STATS for ",A)') &
       TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)
  flush(LU_LOG)

  pGrd%rData(:,:)= rZERO
  pMonthGrd%rData(:,:)= rZERO
  pYearGrd%rData(:,:)= rZERO
  pSummaryGrd%rData(:,:)= rZERO
  iMonthCount = 0
  iYearCount = 0
  iPeriodCount = 0

  do
    ! read in the current date
    read(UNIT=LU_SWBSTATS,iostat=iStat) &
       iCurrDD, iCurrMM, iCurrYYYY, iCurrDOY

    if(iStat /= 0)  exit

    write(sDateTxt,fmt="(i2.2,'/',i2.2,'/',i4.4)") &
      iCurrMM, iCurrDD, iCurrYYYY

    ! this boolean allows us to output results for a season or period each year
    lDATE_RANGE_ACTIVE = (iCurrMM > iSlcStartMM .and. iCurrMM < iSlcEndMM) &
             .or. (iCurrMM == iSlcStartMM .and. iCurrDD >= iSlcStartDD) &
             .or. (iCurrMM == iSlcEndMM .and. iCurrDD <= iSlcEndDD)

    ! figure out whether the current date is the end of a month or year
    iCurrJD = julian_day ( iCurrYYYY, iCurrMM, iCurrDD )
    iTomorrowJD = iCurrJD + 1
    call gregorian_date(iTomorrowJD, iTomorrowYYYY, &
      iTomorrowMM, iTomorrowDD)
    lYearEnd = (.not. iTomorrowYYYY == iCurrYYYY)
    call LookupMonth(iCurrMM, iCurrDD, iCurrYYYY,iCurrDOY, &
                   sMonthName, lMonthEnd)

    ! boolean triggering effects on the occasion of the first day of a new month
    lRESET = ( iCurrMM == 1 .and. iCurrDD == 1)

    pGrd%rData(:,:)= rZERO

    !> if current date does not fall within desired date range, keep
    !> reading data, or if current date is after the end of the
    !> desired date range, stop reading and get out
    if( (iCurrJD < iSWBStatsStartDate .or. .not. lDATE_RANGE_ACTIVE) .and. .not. lCUMULATIVE ) then

      do
        read(UNIT=LU_SWBSTATS,iostat=iStat) iTemp

        ! skip all of the RLE nonsense if we don't care about the contents
        call assert(iStat == 0, "Problem fast-forwarding binary file", &
          trim(__FILE__), __LINE__)
        ! EOF is poorly named. This value (EOF) is really just a marker for the end of the current DAY'S gridded data.
        ! By iterating through this file, we are just discarding all data until one value (iEOF) comes up. 
        ! At that point, the *next* value to be read in will be the next day's date header.
        if(iTemp == iEOF) then
          exit
        else
          cycle
        endif

      enddo

      !> 'cycle' again: return to top of external do loop and read in the
      !> next date header
      cycle

    elseif(iCurrJD > iSWBStatsEndDate) then
      exit
    endif

    !> we are currently dealing with results that fall within our
    !> daterange of interest...

    ! name "RLE_readByte" is misleading, since the return value (rVal)
    ! is actually a vector of all daily values with dimension (iNY*iNX)
    call RLE_readByte(LU_SWBSTATS,iRLE_MULT, rRLE_OFFSET, rVal, iNumGridCells,lEOF)
    if(lEOF) exit

    ! if user selected option "F_TO_C", perform a Fahrenheit to Celcius degree conversion
    if(lF_TO_C) then
      rValTmp = 5./9. * (rVal -32.)
      pGrd%rData(:,:)=RESHAPE(rValTmp,(/iNX,iNY/),PAD=rPad)
    else
      pGrd%rData(:,:)=RESHAPE(rVal,(/iNX,iNY/),PAD=rPad)
    endif
    
    ! if internal grid mask contains inactive cells, nuke any spurious results
    ! that may have been stored at these locations
    where (pGrd%iMask == iINACTIVE_CELL)
      pGrd%rData = rNO_DATA_NCDC
    endwhere

    ! keep track of how many days' worth of results are stored in the month grid
    if(STAT_INFO(iVariableNumber)%iMonthlyOutput /= iNONE) then
      where (pGrd%iMask == iACTIVE_CELL)
        pMonthGrd%rData = pMonthGrd%rData + pGrd%rData
      elsewhere
        pMonthGrd%rData = rNO_DATA_NCDC
      endwhere
      iMonthCount = iMonthCount + 1
    endif

    ! keep track of how many days' worth of results are stored in the year grid
    if(STAT_INFO(iVariableNumber)%iAnnualOutput /= iNONE) then
      where (pGrd%iMask == iACTIVE_CELL)
        pYearGrd%rData = pYearGrd%rData + pGrd%rData
      elsewhere
        pYearGrd%rData = rNO_DATA_NCDC
      endwhere
      iYearCount = iYearCount + 1
    endif

    ! store period data and keep track of number of days' worth of data contribute to current sum
    where (pGrd%iMask == iACTIVE_CELL)
      pSummaryGrd%rData = pSummaryGrd%rData + pGrd%rData
    endwhere
    iPeriodCount = iPeriodCount + 1

    if(lVERBOSE) then

      if(lCUMULATIVE) then
        call stats_WriteMinMeanMax(iLU=LU_STD_OUT, &
          sText=TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)//": "//trim(sDateTxt), &
          rData=pSummaryGrd%rData(:,:), &
          iMask=pGrd%iMask)
      else
        call stats_WriteMinMeanMax(iLU=LU_STD_OUT, &
          sText=TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)//": "//trim(sDateTxt), &
          rData=pGrd%rData(:,:), &
          iMask=pGrd%iMask)
      endif

    else

      write(LU_STD_OUT, fmt="(a)") TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)//": "//trim(sDateTxt)

    endif

    !> create output filename for DAILY GRID output
    if((STAT_INFO(iVariableNumber)%iDailyOutput==iGRID &
       .or. STAT_INFO(iVariableNumber)%iDailyOutput==iBOTH ) &
       .and. lDATE_RANGE_ACTIVE ) then

      write(sOutputFilename,FMT="(A,'_',i4.4,'_',i2.2,'_',i2.2,'.',a)") &
           TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
           iCurrYYYY,iCurrMM,iCurrDD, &
           trim(sOutputFileSuffix)

      if (lCUMULATIVE) then

        call grid_WriteGrid(sFilename=TRIM(sOutputFilename), &
                  pGrd=pSummaryGrd, &
                  iOutputFormat=iOutputFormat)
      else

        call grid_WriteGrid(sFilename=TRIM(sOutputFilename), &
                  pGrd=pGrd, &
                  iOutputFormat=iOutputFormat)

      endif

    endif

    if (STAT_INFO(iVariableNumber)%iDailyOutput==iSTATS) then

      write(sLabel,FMT="(i2.2,'/',i2.2'/',i4.4)") iCurrMM, iCurrDD, iCurrYYYY

!      if(lCUMULATIVE) then

!         if(lMASKSTATS) call CalcMaskStats(pSummaryGrd, pMaskGrd, pConfig, sVarName, sLabel)

!         if(lMASKSSF) call CalcMaskStatsSSF(pSummaryGrd, pMaskGrd, pConfig, sVarName, iGridCellValue, &
!            trim(sSiteNumber)//"   "//trim(sLabel)//"    00:00:00 ")

!       else

        if(lMASKSTATS) call CalcMaskStats(pGrd, pMaskGrd, pConfig, sVarName, sLabel)

        if(lMASKSSF) call CalcMaskStatsSSF(pGrd, pMaskGrd, pConfig, sVarName, iGridCellValue, &
           trim(sSiteNumber)//"   "//trim(sLabel)//"    00:00:00 ")

!       endif

    endif

    if((STAT_INFO(iVariableNumber)%iDailyOutput==iGRAPH &
       .or. STAT_INFO(iVariableNumber)%iDailyOutput==iBOTH) &
       .and. lDATE_RANGE_ACTIVE ) then

      write(sOutputFilename,FMT="(A,'_',i4.4,'_',i2.2,'_',i2.2,'.png')") &
        TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
        iCurrYYYY,iCurrMM,iCurrDD

      sTitleTxt = TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)// &
        '   '//sDateTxt

      if (lCUMULATIVE) then
        sTitleTxt = trim(sTitleTxt)//" (CUMULATIVE)"
        call make_shaded_contour(pSummaryGrd, TRIM(sOutputFilename), TRIM(sTitleTxt), &
          TRIM(STAT_INFO(iVariableNumber)%sUNITS))

      else

        call make_shaded_contour(pGrd, TRIM(sOutputFilename), TRIM(sTitleTxt), &
          TRIM(STAT_INFO(iVariableNumber)%sUNITS))

      endif

    end if

!------------------------- MONTHLY ANALYSIS
    if(lMonthEnd .and. lDATE_RANGE_ACTIVE) then

      where (pMonthGrd%iMask == iACTIVE_CELL)
        pMonthGrdMean%rData = pMonthGrd%rData / REAL(iMonthCount)
      elsewhere
        pMonthGrdMean%rData = rNO_DATA_NCDC
      endwhere

      if(lMASKSTATS .and. STAT_INFO(iVariableNumber)%iMonthlyOutput==iSTATS) then

        write(sLabel,FMT="(i2.2,'/',i4.4)") iCurrMM, iCurrYYYY

        if (lSUM) then
          call CalcMaskStats(pMonthGrd, pMaskGrd, pConfig, sVarName, sLabel)
        else
          call CalcMaskStats(pMonthGrdMean, pMaskGrd, pConfig, sVarName, sLabel, iMonthCount)
        endif
      endif

      if(STAT_INFO(iVariableNumber)%iMonthlyOutput==iGRID &
         .or. STAT_INFO(iVariableNumber)%iMonthlyOutput==iBOTH ) then

        if(lSUM) then

          write(sOutputFilename,FMT="(A,'_',i4.4,'_',i2.2,'.',a)") &
             "SUM_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
             iCurrYYYY,iCurrMM,trim(sOutputFileSuffix)

          call grid_WriteGrid(sFilename=TRIM(sOutputFilename), &
                    pGrd=pMonthGrd, &
                    iOutputFormat=iOutputFormat)

        else

          write(sOutputFilename,FMT="(A,'_',i4.4,'_',i2.2,'.',a)") &
             "MEAN_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
             iCurrYYYY,iCurrMM,trim(sOutputFileSuffix)

          call grid_WriteGrid(sFilename=TRIM(sOutputFilename), &
                    pGrd=pMonthGrdMean, &
                    iOutputFormat=iOutputFormat)

        endif

      end if

      if(STAT_INFO(iVariableNumber)%iMonthlyOutput==iGRAPH &
           .or. STAT_INFO(iVariableNumber)%iMonthlyOutput==iBOTH) then

        if(lSUM) then
          write(sOutputFilename,FMT="(A,'_',i4.4,'_',i2.2,'.png')") &
             "SUM_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
             iCurrYYYY,iCurrMM

          sTitleTxt = "SUM of daily "//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)// &
            ' for '//trim(sMonthName)//' '//trim(int2char(iCurrYYYY))

          call make_shaded_contour(pMonthGrd, TRIM(sOutputFilename), TRIM(sTitleTxt), &
            TRIM(STAT_INFO(iVariableNumber)%sUNITS))

        else

          ! now repeat for MEAN value reporting
          write(sOutputFilename,FMT="(A,'_',i4.4,'_',i2.2,'.png')") &
            "MEAN_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
            iCurrYYYY,iCurrMM

          sTitleTxt = "MEAN of daily "//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)// &
            ' for '//trim(sMonthName)//' '//trim(int2char(iCurrYYYY))

          call make_shaded_contour(pMonthGrdMean, TRIM(sOutputFilename), &
            TRIM(sTitleTxt), TRIM(STAT_INFO(iVariableNumber)%sUNITS))

        endif

      endif

      pMonthGrd%rData=rZERO
      iMonthCount = 0

    endif

  !------------------------- YEARLY ANALYSIS

    if ( lYearEnd .and. lDATE_RANGE_ACTIVE ) then

      where (pMonthGrd%iMask == iACTIVE_CELL)
        pYearGrdMean%rData = pYearGrd%rData / REAL(iYearCount)
      elsewhere
        pYearGrdMean%rData = rNO_DATA_NCDC
      endwhere

      if (lMASKSTATS .and. STAT_INFO(iVariableNumber)%iAnnualOutput==iSTATS) then
        write(sLabel,FMT="(i4.4)") iCurrYYYY
        if (lSUM) then
          call CalcMaskStats(pYearGrd, pMaskGrd, pConfig, sVarName, sLabel)
        else
          call CalcMaskStats(pYearGrdMean, pMaskGrd, pConfig, sVarName, sLabel, iYearCount)
        endif
      endif

      if(STAT_INFO(iVariableNumber)%iAnnualOutput==iGRID &
         .or. STAT_INFO(iVariableNumber)%iAnnualOutput==iBOTH ) then

        if (lSUM) then

          write(sOutputFilename,FMT="(A,'_',i4.4,'.',a)") &
             "SUM_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
             iCurrYYYY,trim(sOutputFileSuffix)

        call grid_WriteGrid(sFilename=TRIM(sOutputFilename), &
                  pGrd=pYearGrd, &
                  iOutputFormat=iOutputFormat)

        else

          ! now repeat for reporting of MEAN values
          write(sOutputFilename,FMT="(A,'_',i4.4,'.',a)") &
             "MEAN_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
             iCurrYYYY,trim(sOutputFileSuffix)

          call grid_WriteGrid(sFilename=TRIM(sOutputFilename), &
                    pGrd=pYearGrdMean, &
                    iOutputFormat=iOutputFormat)

        endif

      end if

      ! now produce ANNUAL PLOTS
      if(STAT_INFO(iVariableNumber)%iAnnualOutput==iGRAPH &
           .or. STAT_INFO(iVariableNumber)%iAnnualOutput==iBOTH) then

        if (lSUM) then

          write(sOutputFilename,FMT="(A,'_',i4.4,'.png')") &
             "SUM_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
             iCurrYYYY

          if(iYearCount < 365) then
            sTitleTxt = "SUM of daily "//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)// &
              " for "//trim(int2char(iCurrYYYY)//" ("//trim(int2char(iYearCount)) &
              //" days in calculation)")
          else
            sTitleTxt = "SUM of daily "//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)// &
              " for "//trim(int2char(iCurrYYYY))
          endif

          call make_shaded_contour(pYearGrd, TRIM(sOutputFilename), TRIM(sTitleTxt), &
            TRIM(STAT_INFO(iVariableNumber)%sUNITS))

        else

          ! now repeat for MEAN value reporting
          write(sOutputFilename,FMT="(A,'_',i4.4,'.png')") &
             "MEAN_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
             iCurrYYYY

          if(iYearCount < 365) then
            sTitleTxt = "MEAN of daily "//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)// &
              " for "//trim(int2char(iCurrYYYY)//" ("//trim(int2char(iYearCount)) &
              //" days in calculation)")
          else
            sTitleTxt = "MEAN of daily "//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)// &
              " for "//trim(int2char(iCurrYYYY))
          endif

          call make_shaded_contour(pYearGrdMean, TRIM(sOutputFilename), TRIM(sTitleTxt), &
            TRIM(STAT_INFO(iVariableNumber)%sUNITS))

        endif

      end if

      pYearGrd%rData=rZERO
      iYearCount = 0

    end if

  end do

  !------------------------- SUMMARY ANALYSIS

  if(lPERIOD_SLICE) then

    write(sLabel,fmt="(i02.2,'-',i02.2,"// &
      "'_to_',i02.2,'-',i02.2,'_for_years_',i04.4,'_to_',i04.4)") &
      iSlcStartMM, iSlcStartDD, iSlcEndMM, iSlcEndDD, &
      iSWBStatsStartYYYY, iSWBStatsEndYYYY

  else

    write(sLabel,fmt="(i02.2,'-',i02.2,'-',i04.4,"// &
        "'_to_',i02.2,'-',i02.2,'-',i04.4)") iSWBStatsStartMM, &
        iSWBStatsStartDD, iSWBStatsStartYYYY, &
        iSWBStatsEndMM, iSWBStatsEndDD, iSWBStatsEndYYYY

  endif

  lRESET = lFALSE

!   if( iSWBStatsOutputType == iSTATS .and. lMASKSTATS) then
!     if(iSWBStatsType == iMEAN) then
!       call CalcMaskStats(pSummaryGrd, pMaskGrd, pConfig, sVarName, sLabel, iPeriodCount)
!     else
!       call CalcMaskStats(pSummaryGrd, pMaskGrd, pConfig, sVarName, sLabel)
!     endif
!   endif

  write(sOutputFilename,FMT="(A,'_',a,'.',a)") &
     "SUM_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
     trim(sLabel),trim(sOutputFileSuffix)

  call grid_WriteGrid(sFilename=TRIM(sOutputFilename), &
            pGrd=pSummaryGrd, &
            iOutputFormat=iOutputFormat)

  ! now repeat for reporting of MEAN values
  write(sOutputFilename,FMT="(A,'_',a,'.',a)") &
    "MEAN_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
     trim(sLabel),trim(sOutputFileSuffix)

  if (iPeriodCount > 0) then

    where (pSummaryGrdMean%iMask == iACTIVE_CELL)
      pSummaryGrdMean%rData = pSummaryGrdMean%rData / REAL(iPeriodCount)
    elsewhere
      pSummaryGrdMean%rData = rNO_DATA_NCDC
    endwhere

    call grid_WriteGrid(sFilename=TRIM(sOutputFilename), &
            pGrd=pSummaryGrdMean, &
            iOutputFormat=iOutputFormat)

  endif

  write(sOutputFilename,FMT="(A,'_',a,'.png')") &
    "SUM_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), trim(sLabel)

  if(lPERIOD_SLICE) then

    write(sTitleTxt,fmt="('SUM of daily ',a,' for ',i02.2,'/',i02.2,"// &
      "' to ',i02.2,'/',i02.2,' for years ',i04.4,' to ',i04.4)") &
      TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), iSlcStartMM, &
      iSlcStartDD, iSlcEndMM, iSlcEndDD, iSWBStatsStartYYYY, iSWBStatsEndYYYY

  else

    write(sTitleTxt,fmt="('SUM of daily ',a,' for ',i02.2,'/',i02.2,'/',i04.4,"// &
        "' to ',i02.2,'/',i02.2,'/',i04.4)") &
        TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), iSWBStatsStartMM, &
        iSWBStatsStartDD, iSWBStatsStartYYYY, &
        iSWBStatsEndMM, iSWBStatsEndDD, iSWBStatsEndYYYY
  endif


  call make_shaded_contour(pSummaryGrd, TRIM(sOutputFilename), TRIM(sTitleTxt), &
    TRIM(STAT_INFO(iVariableNumber)%sUNITS))

  ! now repeat for MEAN value reporting
  write(sOutputFilename,FMT="(A,'_',a,'.png')") &
     "MEAN_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), trim(sLabel)

  if(lPERIOD_SLICE) then

    write(sTitleTxt,fmt="('MEAN of daily ',a,' for ',i02.2,'/',i02.2,"// &
      "' to ',i02.2,'/',i02.2,' for years ',i04.4,' to ',i04.4)") &
      TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), iSlcStartMM, &
      iSlcStartDD, iSlcEndMM, iSlcEndDD, iSWBStatsStartYYYY, iSWBStatsEndYYYY

  else

    write(sTitleTxt,fmt="('MEAN of daily ',a,' for ',i02.2,'/',i02.2,'/',i04.4,"// &
        "' to ',i02.2,'/',i02.2,'/',i04.4)") &
        TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), iSWBStatsStartMM, &
        iSWBStatsStartDD, iSWBStatsStartYYYY, &
        iSWBStatsEndMM, iSWBStatsEndDD, iSWBStatsEndYYYY
  endif

  if(iPeriodCount > 0) then

    where (pSummaryGrdMean %iMask == iACTIVE_CELL)
      pSummaryGrdMean%rData = pSummaryGrd%rData / REAL(iPeriodCount)
    elsewhere
      pSummaryGrdMean%rData = rNO_DATA_NCDC
    endwhere

  endif

  call make_shaded_contour(pSummaryGrdMean, TRIM(sOutputFilename), TRIM(sTitleTxt), &
    TRIM(STAT_INFO(iVariableNumber)%sUNITS))

  close(unit=LU_LOG)

end program swbstats
