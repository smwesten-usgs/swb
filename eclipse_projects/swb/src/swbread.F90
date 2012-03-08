program swbread

use types
use graph
use swb_stats
use swb_grid
use RLE
implicit none

  ! general temporary variables
  character (len=256)  :: sBinFile, sBuf, sBuf2, sBuf3 = ""
  character (len=256)  :: sOutputFilename = ""
  integer (kind=T_INT) :: iNumArgs
  integer (kind=T_INT) :: iNumGridCells
  integer (kind=T_INT) :: iStat
  integer (kind=T_INT) :: i, k
  integer (kind=T_INT) :: iDateNum = 0
  integer (kind=T_INT) :: iNumDaysInYear
  integer (kind=T_INT) :: iLen1, iLen2
  integer (kind=T_INT), dimension(2) :: iTempDate

  ! variables that are read in from the binary file header
  integer (kind=T_INT) :: iNX
  integer (kind=T_INT) :: iNY
  integer (kind=T_INT) :: iDataType
  real (kind=T_SGL)    :: rGridCellSize
  integer (kind=T_INT) :: iLengthUnits
  integer (kind=T_INT) :: iVariableNumber
  integer (kind=T_INT) :: iRLE_MULT
  real (kind=T_SGL)    :: rRLE_OFFSET
  real (kind=T_DBL)    :: rX0, rX1
  real (kind=T_DBL)    :: rY0, rY1
  integer (kind=T_INT) :: iStartMM, iStartDD, iStartYYYY
  integer (kind=T_INT) :: iEndMM, iEndDD, iEndYYYY

  integer (kind=T_INT) :: iCurrMM, iCurrDD, iCurrYYYY, iCurrDOY, iCurrJD
  integer (kind=T_INT) :: iTomorrowMM, iTomorrowDD, iTomorrowYYYY, iTomorrowDOY, iTomorrowJD
  integer (kind=T_INT) :: iTempStartDate, iTempEndDate
  character (len=256) :: sMonthName = ""
  logical (kind=T_LOGICAL) :: lMonthEnd
  logical (kind=T_LOGICAL) :: lYearEnd

  integer (kind=T_INT) :: LU_SWBREAD

  integer (kind=T_INT) :: iSWBReadStartDate, iSWBReadStartMM, &
                          iSWBReadStartDD,iSWBReadStartYYYY
  integer (kind=T_INT) :: iSWBReadEndDate, iSWBReadEndMM, &
                          iSWBReadEndDD,iSWBReadEndYYYY
  integer (kind=T_INT) :: iSWBReadOutputType = iBOTH

  character (len=256) :: sTitleTxt
  character (len=10) :: sDateTxt

  type ( T_GENERAL_GRID ),pointer :: pGrd
  type ( T_GENERAL_GRID ),pointer :: pMonthGrd
  type ( T_GENERAL_GRID ),pointer :: pYearGrd

  real(kind=T_SGL),dimension(:), allocatable :: rVal,rValSum,rPad

  character (len=8) :: sDate
  character (len=10) :: sTime
  character (len=5) :: sTZ

  character (len=1) :: sSlash = "/"
  character (len=20) :: sOutputFilePrefix = ""
  character (len=20) :: sOutputFileSuffix = "asc"
  logical (kind=T_LOGICAL) :: lVerbose = lFALSE
  integer (kind=T_INT) :: iOutputFormat = OUTPUT_ARC
  logical (kind=T_LOGICAL) :: lEOF
  logical (kind=T_LOGICAL) :: lPrematureEOF = lFALSE


  call date_and_time(sDate,sTime,sTZ)

  write(sBuf,FMT=*) "SWBREAD_LOGFILE_"//sDate//"_"//sTime(1:6)//".txt"

  ! open up the log file
  open(LU_LOG, file=TRIM(ADJUSTL(sBuf)),iostat=iStat,&
      status='REPLACE')
  call Assert( iStat == 0, "Problem opening log file file for output.")

  write(UNIT=LU_LOG,FMT=*) "Soil Water Balance Code BINARY FILE READER compiled on: "// &
    TRIM(__DATE__) //" "// TRIM(__TIME__)
  write(UNIT=LU_LOG,FMT=*) "SWB reader execution started: "// &
    sDate//"_"//sTime(1:6)

  ! warning - calling a Fortran 2003 extension function here
  iNumArgs = COMMAND_ARGUMENT_COUNT()

  if(iNumArgs < 1) then

    write(UNIT=*,FMT="(/,a,/)") &
      "Soil Water Balance Code - binary reader -- compiled on: "// &
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

    write(UNIT=*,FMT="(/,/,a,/)") &
      "Usage: swbread [binary file name] [GRID|PLOT|BOTH] {SURFER} {YEARLY|MONTHLY|DAILY} {start date} {end date} {VERBOSE}"
    write(UNIT=*,FMT="(/,t5,a)") "A filename and output type must be specified; all other arguments are OPTIONAL:"
    write(UNIT=*,FMT="(/,t7,a)") "1) output frequency will generate output at all frequencies less than"
    write(UNIT=*,FMT="(t10,a,/)") "that specified (i.e. MONTHLY will also generate YEARLY)"
    write(UNIT=*,FMT="(/,t7,a)") "2) start date and end date must be in MM/DD/YYYY format"
    write(UNIT=*,FMT="(/,t7,a)") "3) VERBOSE: writes daily min, mean, and max values to logfile"
    write(UNIT=*,FMT="(/,t7,a)") "4) SURFER: directs output to Surfer grids rather than Arc ASCII grids"
    stop

  end if

  call GET_COMMAND_ARGUMENT(1,sBinFile)

  open(LU_SWBREAD, FILE=TRIM(sBinFile),FORM='UNFORMATTED', &
       status='OLD',ACCESS='STREAM', IOSTAT=iStat )

  call Assert(iStat==0,"Failed to open input binary file: "//&
    TRIM(sBinFile),TRIM(__FILE__),__LINE__)

  read(UNIT=LU_SWBREAD) iNX             ! Number of cells in the x-direction
  read(UNIT=LU_SWBREAD) iNY             ! Number of cells in the y-direction
  read(UNIT=LU_SWBREAD) iDataType       ! Type of the grid
  read(UNIT=LU_SWBREAD) rGridCellSize   ! size of one side of a grid cell
  read(UNIT=LU_SWBREAD) iLengthUnits    ! length units code
  read(UNIT=LU_SWBREAD) iVariableNumber ! STAT_INFO variable number
  read(UNIT=LU_SWBREAD) iRLE_MULT       ! RLE Multiplier
  read(UNIT=LU_SWBREAD) rRLE_OFFSET     ! RLE Offset
  read(UNIT=LU_SWBREAD) rX0, rX1        ! World-coordinate range in X
  read(UNIT=LU_SWBREAD) rY0, rY1        ! World-coordinate range in Y
  read(UNIT=LU_SWBREAD) iStartMM, iStartDD, iStartYYYY
  read(UNIT=LU_SWBREAD) iEndMM, iEndDD, iEndYYYY

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
  iSWBReadStartDate = julian_day ( iStartYYYY, iStartMM, iStartDD)
  iSWBReadEndDate = julian_day ( iEndYYYY, iEndMM, iEndDD)
  STAT_INFO(iVariableNumber)%iDailyOutput = iNONE
  STAT_INFO(iVariableNumber)%iMonthlyOutput = iNONE
  STAT_INFO(iVariableNumber)%iAnnualOutput = iBOTH

  iNumGridCells = iNX * iNY

  if(iNumArgs == 1) then

    write(unit=LU_STD_OUT,fmt="(/,/,'Information about binary file ',a)") &
       TRIM(sBinFile)

    if(lPrematureEOF) then
      write(unit=LU_STD_OUT,fmt="(/,'  File ends prematurely - cannot determine the ending date ')")
      write(unit=LU_STD_OUT,fmt="(/,'  Starting date is ',i02.2,'/',i02.2,'/',i04.4)") &
        iStartMM, iStartDD, iStartYYYY
    else
      write(unit=LU_STD_OUT,fmt="(/,'  Dates range from ',i02.2,'/',i02.2,'/',i04.4,"// &
        "' to ',i02.2,'/',i02.2,'/',i04.4)") iStartMM, iStartDD, iStartYYYY, &
        iEndMM, iEndDD, iEndYYYY
    endif
    write(unit=LU_STD_OUT,fmt="(/,'  Contains SWB output for ',a,': ',a,' (',a,')')") &
      TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
      TRIM(STAT_INFO(iVariableNumber)%sLongName), &
      TRIM(STAT_INFO(iVariableNumber)%sUNITS)
    stop

  end if

  do i=2,iNumArgs

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

    if(TRIM(ADJUSTL(sBuf)) .eq. "GRID") then
      iSWBReadOutputType = iGRID
      STAT_INFO(iVariableNumber)%iAnnualOutput = iGRID
    elseif(TRIM(ADJUSTL(sBuf)) .eq. "PLOT" &
      .or. TRIM(ADJUSTL(sBuf)) .eq. "GRAPH") then
      iSWBReadOutputType = iGRAPH
      STAT_INFO(iVariableNumber)%iAnnualOutput = iGRAPH
    elseif(TRIM(ADJUSTL(sBuf)) .eq. "BOTH") then
      STAT_INFO(iVariableNumber)%iAnnualOutput = iBOTH
      iSWBReadOutputType = iBOTH
    elseif(TRIM(ADJUSTL(sBuf)) .eq. "DEFAULT") then
      iSWBReadStartDate = julian_day ( iStartYYYY, iStartMM, iStartDD)
      iSWBReadEndDate = julian_day ( iEndYYYY, iEndMM, iEndDD)
      STAT_INFO(iVariableNumber)%iDailyOutput = iNONE
      STAT_INFO(iVariableNumber)%iMonthlyOutput = iNONE
      STAT_INFO(iVariableNumber)%iAnnualOutput = iBOTH
    elseif(TRIM(ADJUSTL(sBuf)) .eq. "YEARLY" &
      .or. TRIM(ADJUSTL(sBuf)) .eq. "ANNUAL") then
      STAT_INFO(iVariableNumber)%iAnnualOutput = iSWBReadOutputType
    elseif(TRIM(ADJUSTL(sBuf)) .eq. "MONTHLY") then
      STAT_INFO(iVariableNumber)%iAnnualOutput = iSWBReadOutputType
      STAT_INFO(iVariableNumber)%iMonthlyOutput = iSWBReadOutputType
    elseif(TRIM(ADJUSTL(sBuf)) .eq. "DAILY") then
      STAT_INFO(iVariableNumber)%iAnnualOutput = iSWBReadOutputType
      STAT_INFO(iVariableNumber)%iMonthlyOutput = iSWBReadOutputType
      STAT_INFO(iVariableNumber)%iDailyOutput = iSWBReadOutputType
    elseif(TRIM(ADJUSTL(sBuf)) .eq. "VERBOSE") then
      lVerbose = lTRUE
    elseif(TRIM(ADJUSTL(sBuf)) .eq. "SURFER") then
      iOutputFormat = OUTPUT_SURFER
      sOutputFileSuffix = "grd"
    elseif(iLen1 /= iLen2) then  ! we have found forward slashes...probably a date
      iDateNum = iDateNum + 1
      call Assert(iDateNum <=2, "Too many dates entered on the command line", &
        TRIM(__FILE__),__LINE__)
      iTempDate(iDateNum) = mmddyyyy2julian(sBuf)
    else
      call Assert(lFALSE,"Unknown command line option: "//TRIM(sBuf), &
        TRIM(__FILE__),__LINE__)
    endif

  end do

  call Assert(iDateNum == 0 .or. iDateNum == 2, &
    "Two dates must be entered in order to perform analysis on a subset of the data", &
    TRIM(__FILE__),__LINE__)

  if(iDateNum ==2) then
    iTempStartDate = minval(iTempDate)
    iTempEndDate = maxval(iTempDate)

    call Assert(iTempStartDate >= iSWBReadStartDate, &
      "Your specified start date for data slicing begins before first SWB output", &
      TRIM(__FILE__),__LINE__)

    call Assert(iTempEndDate <= iSWBReadEndDate, &
      "Your specified end date for data slicing ends after last SWB output", &
      TRIM(__FILE__),__LINE__)
    ! O.K. Dates pass the smell test and appear legitimate. Override default values.
    iSWBReadEndDate = iTempEndDate
    iSWBReadStartDate = iTempStartDate
  endif

  call gregorian_date(iSWBReadStartDate, iSWBReadStartYYYY, &
       iSWBReadStartMM, iSWBReadStartDD)

  call gregorian_date(iSWBReadEndDate, iSWBReadEndYYYY, &
       iSWBReadEndMM, iSWBReadEndDD)

  write(unit=LU_STD_OUT,fmt="(/,'  Analysis range from ',i02.2,'/',i02.2,'/',i04.4,"// &
      "' to ',i02.2,'/',i02.2,'/',i04.4)") iSWBReadStartMM, iSWBReadStartDD, iSWBReadStartYYYY, &
      iSWBReadEndMM, iSWBReadEndDD, iSWBReadEndYYYY

  write(unit=LU_STD_OUT,fmt="(/,a,/)") "  Summary of output to be generated:"
  write(unit=LU_STD_OUT,fmt="(t20,a,t28,a,t36,a)") &
    "NONE","PLOT","GRID"

  k = STAT_INFO(iVariableNumber)%iDailyOutput
  if(k==iNONE) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Daily"," **","",""
  if(k==iGRID) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Daily","",""," **"
  if(k==iGRAPH) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Daily",""," **",""
  if(k==iBOTH) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Daily",""," **"," **"

  k = STAT_INFO(iVariableNumber)%iMonthlyOutput
  if(k==iNONE) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Monthly"," **","",""
  if(k==iGRID) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Monthly","",""," **"
  if(k==iGRAPH) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Monthly",""," **",""
  if(k==iBOTH) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Monthly",""," **"," **"

  k = STAT_INFO(iVariableNumber)%iAnnualOutput
  if(k==iNONE) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Annual"," **","",""
  if(k==iGRID) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Annual","",""," **"
  if(k==iGRAPH) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Annual",""," **",""
  if(k==iBOTH) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Annual",""," **"," **"

write(unit=LU_LOG,fmt="(/,a,/)") "  Summary of output to be generated:"
  write(unit=LU_LOG,fmt="(t20,a,t28,a,t36,a)") &
    "NONE","PLOT","GRID"

  k = STAT_INFO(iVariableNumber)%iDailyOutput
  if(k==iNONE) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Daily"," **","",""
  if(k==iGRID) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Daily","",""," **"
  if(k==iGRAPH) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Daily",""," **",""
  if(k==iBOTH) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Daily",""," **"," **"

  k = STAT_INFO(iVariableNumber)%iMonthlyOutput
  if(k==iNONE) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Monthly"," **","",""
  if(k==iGRID) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Monthly","",""," **"
  if(k==iGRAPH) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Monthly",""," **",""
  if(k==iBOTH) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Monthly",""," **"," **"

  k = STAT_INFO(iVariableNumber)%iAnnualOutput
  if(k==iNONE) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Annual"," **","",""
  if(k==iGRID) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Annual","",""," **"
  if(k==iGRAPH) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Annual",""," **",""
  if(k==iBOTH) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Annual",""," **"," **"

  write(unit=LU_LOG,fmt="(/)")

  !------------------------------------------------------------------------------------------
  ! BEGIN writing output
  !------------------------------------------------------------------------------------------

!  rewind(STAT_INFO(iVariableNumber)%iLU)
!  write(STAT_INFO(iVariableNumber)%iLU,POS=iENDHEADER_POS)

  allocate(rVal(iNX*iNY))
  allocate(rValSum(iNX*iNY))
  allocate(rPad(iNX*iNY))

  pGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, T_SGL_GRID)
  pMonthGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, T_SGL_GRID)
  pYearGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, T_SGL_GRID)

  rPad = -9999_T_SGL

  write(LU_LOG, &
     fmt='("CALCULATING STATS for ",A)') &
       TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)
  flush(LU_LOG)

  pGrd%rData(:,:)= rZERO
  pMonthGrd%rData(:,:)= rZERO
  pYearGrd%rData(:,:)= rZERO

  do
    ! read in the current date
    read(UNIT=LU_SWBREAD,iostat=iStat) &
       iCurrDD, iCurrMM, iCurrYYYY, iCurrDOY
    if(iStat /= 0) then
      exit
    end if

    write(sDateTxt,fmt="(i2.2,'/',i2.2,'/',i4.4)") &
      iCurrMM, iCurrDD, iCurrYYYY

    ! figure out whether the current date is the end of a month or year
    iCurrJD = julian_day ( iCurrYYYY, iCurrMM, iCurrDD )
    iTomorrowJD = iCurrJD + 1
    call gregorian_date(iTomorrowJD, iTomorrowYYYY, &
      iTomorrowMM, iTomorrowDD)
    lYearEnd = (.not. iTomorrowYYYY == iCurrYYYY)
    call LookupMonth(iCurrMM, iCurrDD, iCurrYYYY,iCurrDOY, &
                   sMonthName, lMonthEnd)
    pGrd%rData(:,:)= rZERO

    ! name "RLE_readByte" is misleading, since the return value (rVal)
    ! is actually a vector of all daily values with dimension (iNY*iNX)
    call RLE_readByte(LU_SWBREAD,iRLE_MULT, &
       rRLE_OFFSET, rVal,iNumGridCells,lEOF)
    if(lEOF) exit

    if(iCurrJD < iSWBReadStartDate) then
      cycle
    elseif(iCurrJD > iSWBReadEndDate) then
      exit
    endif

    pGrd%rData(:,:)=RESHAPE(rVal,(/iNX,iNY/),PAD=rPad)

    pMonthGrd%rData = pMonthGrd%rData + pGrd%rData
    pYearGrd%rData = pYearGrd%rData + pGrd%rData

    if(lVerbose) &
      call stats_WriteMinMeanMax(LU_LOG, &
         TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)//": "//trim(sDateTxt), &
         pGrd%rData(:,:))

    if(STAT_INFO(iVariableNumber)%iDailyOutput==iGRID &
       .or. STAT_INFO(iVariableNumber)%iDailyOutput==iBOTH) then

      write(sOutputFilename,FMT="(A,'_',i4.4,'_',i2.2,'_',i2.2,'.',a)") &
         TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
         iCurrYYYY,iCurrMM,iCurrDD, &
         trim(sOutputFileSuffix)

      if ( iOutputFormat == OUTPUT_SURFER ) then
         call grid_WriteSurferGrid(TRIM(sOutputFilename), &
           rX0,rX1,rY0,rY1,pGrd%rData(:,:))
      else
         call grid_WriteArcGrid(TRIM(sOutputFilename), &
            rX0,rX1,rY0,rY1,pGrd%rData(:,:))
      end if

    end if

    if(STAT_INFO(iVariableNumber)%iDailyOutput==iGRAPH &
       .or. STAT_INFO(iVariableNumber)%iDailyOutput==iBOTH) then

      write(sOutputFilename,FMT="(A,'_',i4.4,'_',i2.2,'_',i2.2,'.png')") &
        TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
        iCurrYYYY,iCurrMM,iCurrDD

      sTitleTxt = TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)// &
        '   '//sDateTxt

      call make_shaded_contour(pGrd, TRIM(sOutputFilename), TRIM(sTitleTxt), &
        TRIM(STAT_INFO(iVariableNumber)%sUNITS))

    end if

!------------------------- MONTHLY ANALYSIS
    if(lMonthEnd) then

      if((STAT_INFO(iVariableNumber)%iMonthlyOutput==iGRID &
         .or. STAT_INFO(iVariableNumber)%iMonthlyOutput==iBOTH)) then

        write(sOutputFilename,FMT="(A,'_',i4.4,'_',i2.2,'.',a)") &
           "SUM_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
           iCurrYYYY,iCurrMM,trim(sOutputFileSuffix)

        if ( iOutputFormat == OUTPUT_SURFER ) then
           call grid_WriteSurferGrid(TRIM(sOutputFilename), &
             rX0,rX1,rY0,rY1,pMonthGrd%rData(:,:))
        else
           call grid_WriteArcGrid(TRIM(sOutputFilename), &
              rX0,rX1,rY0,rY1,pMonthGrd%rData(:,:))
        end if

        write(sOutputFilename,FMT="(A,'_',i4.4,'_',i2.2,'.',a)") &
           "MEAN_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
           iCurrYYYY,iCurrMM,trim(sOutputFileSuffix)

        if ( iOutputFormat == OUTPUT_SURFER ) then
           call grid_WriteSurferGrid(TRIM(sOutputFilename), &
             rX0,rX1,rY0,rY1,&
               pMonthGrd%rData(:,:) / REAL(iCurrDD))
        else
           call grid_WriteArcGrid(TRIM(sOutputFilename), &
              rX0,rX1,rY0,rY1, &
               pMonthGrd%rData(:,:) / REAL(iCurrDD))
        end if

      end if

      if(STAT_INFO(iVariableNumber)%iMonthlyOutput==iGRAPH &
         .or. STAT_INFO(iVariableNumber)%iMonthlyOutput==iBOTH) then

        write(sOutputFilename,FMT="(A,'_',i4.4,'_',i2.2,'.png')") &
           "SUM_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
           iCurrYYYY,iCurrMM

        sTitleTxt = "SUM of daily "//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)// &
          ' for '//trim(sMonthName)//' '//trim(int2char(iCurrYYYY))

        call make_shaded_contour(pMonthGrd, TRIM(sOutputFilename), TRIM(sTitleTxt), &
          TRIM(STAT_INFO(iVariableNumber)%sUNITS))

        ! now repeat for MEAN value reporting
        write(sOutputFilename,FMT="(A,'_',i4.4,'_',i2.2,'.png')") &
           "MEAN_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
           iCurrYYYY,iCurrMM

        sTitleTxt = "MEAN of daily "//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)// &
          ' for '//trim(sMonthName)//' '//trim(int2char(iCurrYYYY))

        pMonthGrd%rData = pMonthGrd%rData / REAL(iCurrDD)

        call make_shaded_contour(pMonthGrd, TRIM(sOutputFilename), &
           TRIM(sTitleTxt), TRIM(STAT_INFO(iVariableNumber)%sUNITS))

      end if

      pMonthGrd%rData=rZERO

    end if

  !------------------------- YEARLY ANALYSIS

    if(lYearEnd) then

      if((STAT_INFO(iVariableNumber)%iAnnualOutput==iGRID &
         .or. STAT_INFO(iVariableNumber)%iAnnualOutput==iBOTH)) then

        iNumDaysInYear = num_days_in_year(iCurrYYYY)

        write(sOutputFilename,FMT="(A,'_',i4.4,'.',a)") &
           "SUM_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
           iCurrYYYY,trim(sOutputFileSuffix)

        if ( iOutputFormat == OUTPUT_SURFER ) then
           call grid_WriteSurferGrid(TRIM(sOutputFilename), &
             rX0,rX1,rY0,rY1,pYearGrd%rData(:,:))
        else
           call grid_WriteArcGrid(TRIM(sOutputFilename), &
              rX0,rX1,rY0,rY1,pYearGrd%rData(:,:))
        end if

        ! now repeat for reporting of MEAN values
        write(sOutputFilename,FMT="(A,'_',i4.4,'.',a)") &
           "MEAN_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
           iCurrYYYY,trim(sOutputFileSuffix)

        if ( iOutputFormat == OUTPUT_SURFER ) then
           call grid_WriteSurferGrid(TRIM(sOutputFilename), &
             rX0,rX1,rY0,rY1,pYearGrd%rData / REAL(iNumDaysInYear))
        else
           call grid_WriteArcGrid(TRIM(sOutputFilename), &
              rX0,rX1,rY0,rY1,pYearGrd%rData / REAL(iNumDaysInYear))
        end if


      end if

      ! now produce ANNUAL PLOTS
      if(STAT_INFO(iVariableNumber)%iAnnualOutput==iGRAPH &
         .or. STAT_INFO(iVariableNumber)%iAnnualOutput==iBOTH) then

        write(sOutputFilename,FMT="(A,'_',i4.4,'.png')") &
           "SUM_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
           iCurrYYYY

        sTitleTxt = "SUM of daily "//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)// &
          ' for '//trim(int2char(iCurrYYYY))

        call make_shaded_contour(pYearGrd, TRIM(sOutputFilename), TRIM(sTitleTxt), &
          TRIM(STAT_INFO(iVariableNumber)%sUNITS))

        ! now repeat for MEAN value reporting
        write(sOutputFilename,FMT="(A,'_',i4.4,'.png')") &
           "MEAN_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
           iCurrYYYY

        sTitleTxt = "MEAN of daily "//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)// &
          ' for '//trim(int2char(iCurrYYYY))

        pYearGrd%rData = pYearGrd%rData / REAL(iNumDaysInYear)

        call make_shaded_contour(pYearGrd, TRIM(sOutputFilename), TRIM(sTitleTxt), &
          TRIM(STAT_INFO(iVariableNumber)%sUNITS))

      end if

      pYearGrd%rData=rZERO

    end if

  end do

  close(unit=LU_LOG)

end program swbread
