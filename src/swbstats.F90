module swbstats_support

contains

subroutine CalcBasinStats(pGrd, pConfig, sVarName, sLabel, iNumDays)

  use types
  use graph
  use swb_grid

  implicit none

  type (T_GENERAL_GRID), pointer :: pGrd            ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  character(len=*) :: sVarName
  character (len=*) :: sLabel
  integer (kind=T_INT), optional :: iNumDays

  ![LOCALS]
  integer (kind=T_INT) :: j, k, iStat, iCount
  integer (kind=T_INT) ::   iNumGridCells
  real (kind=T_SGL) :: rSum, rAvg, rMin, rMax
  real (kind=T_SGL) :: rDenominator

  character (len=256) :: sBuf

  integer (kind=T_INT) :: iNumRecs

  type (T_GENERAL_GRID), pointer :: input_grd

  if(present(iNumDays)) then
    rDenominator = real(iNumDays, kind=T_SGL)
  else
    rDenominator = 1_T_SGL
  end if

  call assert(associated(pConfig%BMASK), "A basin mask list must be supplied in order " &
    //"to use the STATS option", trim(__FILE__), __LINE__)

  iNumRecs = size(pConfig%BMASK,1)

  if(pConfig%lFirstDayOfSimulation) then

    open(LU_PEST_STATS,FILE="SWB_PEST_STATS_"//trim(sVarName)//".txt", &
          iostat=iStat, STATUS='REPLACE')
    call Assert ( iStat == 0, &
      "Could not open PEST statistics file")

    write(UNIT=LU_PEST_STATS,FMT="(A,a)",advance='NO') "Period",sTAB

    do k=1,iNumRecs-1
      write(UNIT=LU_PEST_STATS,FMT="(A,a)",advance='NO') &
          ADJUSTL(TRIM(pConfig%BMASK(k)%sUSGS_UpstreamOrderID)),sTAB
    end do

    write(UNIT=LU_PEST_STATS,FMT="(A)") &
       ADJUSTL(TRIM(pConfig%BMASK(iNumRecs)%sUSGS_UpstreamOrderID))

    pConfig%lFirstDayOfSimulation = lFALSE

  else   ! append to files

    open(LU_PEST_STATS,FILE="SWB_PEST_STATS_"//trim(sVarName)//".txt",iostat=iStat, &
        POSITION='APPEND', STATUS='OLD')
    call Assert ( iStat == 0, &
      "Could not open PEST statistics file")

  end if

  write(UNIT=LU_PEST_STATS,FMT="(a,a)", advance='NO') TRIM(sLabel),sTAB

  do k = 1,iNumRecs

    iCount = COUNT(pConfig%BMASK(k)%pGrd%rData>0)

    ! sum of the sum of values within basin mask boundaries
    rSum = SUM(pGrd%rData,MASK=pConfig%BMASK(k)%pGrd%rData>0) / rDenominator
    rMax = MAXVAL(pGrd%rData,MASK=pConfig%BMASK(k)%pGrd%rData>0) / rDenominator
    rMin = MINVAL(pGrd%rData,MASK=pConfig%BMASK(k)%pGrd%rData>0) / rDenominator

    rAvg = rSum / iCount

    write(UNIT=LU_LOG,FMT="(A)") ""
    write(UNIT=LU_LOG,FMT="(5x,A)") TRIM(pConfig%BMASK(k)%sBasinDescription)
    write(UNIT=LU_LOG,FMT="(5x,A)") "==> "//TRIM(sLabel)
    write(UNIT=LU_LOG,FMT="(5x,'Drainage area (sq mi):', f14.2)") &
        pConfig%BMASK(k)%rDrainageArea
    write(UNIT=LU_LOG,FMT="(8x,A7,i12)") "count:",iCount
    write(UNIT=LU_LOG,FMT="(8x,A7,f14.2)") "sum:",rSum
    write(UNIT=LU_LOG,FMT="(8x,A7,f14.2)") "avg:",rAvg
!
    write(UNIT=LU_LOG,FMT="(A)") REPEAT("-",80)

    ! convert average value in inches to cubic feet
    rAvg = real(rAvg,kind=T_DBL) * real(pConfig%BMASK(k)%rDrainageArea,kind=T_DBL) &
               * 4.01449E9_T_DBL

    if( k < iNumRecs ) then

      write(UNIT=LU_PEST_STATS,FMT="(g16.8,a)", advance='NO') rAvg,sTAB

    else

      write(UNIT=LU_PEST_STATS,FMT="(g16.8)") rAvg

    endif

    close(UNIT=LU_MASK_FILE)

  end do

  flush(UNIT=LU_PEST_STATS)
  close(UNIT=LU_PEST_STATS)

end subroutine CalcBasinStats


subroutine ReadBasinMaskTable ( pConfig , pGrd)

  use types
  use graph
  use swb_grid
  implicit none

  !! reads the basin catchment data file for subsequent processing
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  type (T_GENERAL_GRID), pointer :: pGrd            ! pointer to model grid

  ! [ LOCALS ]
  integer (kind=T_INT) :: iStat, iNumMaskFiles, i, iRecNum, iSize
  character (len=256) :: sRecord                  ! Input file text buffer
  character (len=256) :: sItem                    ! Key word read from sRecord
  character (len=256) :: sBuf

  ! open basin mask file
  open ( LU_MASK, file=pConfig%sBasinMaskFilename, &
            status="OLD", iostat=iStat )
  call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
            "Open failed for file: " // pConfig%sBasinMaskFilename )

  ! read first line of file
  read ( unit=LU_MASK, fmt="(a256)", iostat=iStat ) sRecord
  call Assert( iStat == 0, &
     "Error reading first line of basin mask table" )

  ! read mask file to obtain expected number of basin mask files
  call chomp( sRecord, sItem, sTAB )
  call Uppercase( sItem )
  if ( sItem == "NUM_BASIN_MASK_FILES" ) then
    call chomp( sRecord, sItem, sTAB )
    read ( unit=sItem, fmt=*, iostat=iStat ) iNumMaskFiles
    call Assert( iStat == 0, "Failed to read number of basin mask files" )
    write(UNIT=LU_LOG,FMT=*)  "==> allocating memory for",iNumMaskFiles, &
       " landuse types within basin mask table"
  else
    call Assert( lFALSE, &
       "Unknown option in basin mask table; was expecting NUM_BASIN_MASK_FILES #")
  end if

  ! read (AND IGNORE) second line of file
  read ( unit=LU_MASK, fmt="(a256)", iostat=iStat ) sRecord
  call Assert( iStat == 0, &
     "Error reading second line of basin mask table" )

  ! now allocate memory for BASIN MASK table
  allocate ( pConfig%BMASK( iNumMaskFiles ), stat=iStat )
  call Assert ( iStat == 0, &
    "Could not allocate space for basin mask data structure" )

  iSize = size(pConfig%BMASK,1)

  iRecNum = 1

  BMASK: do

    read ( unit=LU_MASK, fmt="(a256)", iostat=iStat ) sRecord
    if ( iStat < 0 ) exit     ! EOF mark
    if ( sRecord(1:1) == "#" ) cycle      ! Ignore comment lines

    if(iRecNum > iSize) then
      write(UNIT=LU_LOG,FMT=*) ""
      write(UNIT=LU_LOG,FMT=*)  " *** The maximum number of basin mask table elements has"
      write(UNIT=LU_LOG,FMT=*)  "     been read in before reaching the end of the file."
      write(UNIT=LU_LOG,FMT=*) ""
      write(UNIT=LU_LOG,FMT=*)  "     size of allocated memory for BASIN MASK table: ",iSize
      write(UNIT=LU_LOG,FMT=*)  "     current record number: ", iRecNum
      exit
    end if

    write(UNIT=LU_LOG,FMT=*) ""
    write(UNIT=LU_LOG,FMT=*)  "-----------------------------------------------------------"
    write(UNIT=LU_LOG,FMT=*)  "Reading basin mask record number ",iRecNum, " of ",iNumMaskFiles
    write(UNIT=LU_LOG,FMT=*) ""

    call chomp( sRecord, sItem, sTAB )
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%BMASK(iRecNum)%sUSGS_UpstreamOrderID
    call Assert( iStat == 0, &
      "Error reading upstream order ID in basin mask table" )
    write(UNIT=LU_LOG,FMT=*)  "Upstream order ID = ",TRIM(pConfig%BMASK(iRecNum)%sUSGS_UpstreamOrderID)

    call chomp( sRecord, sItem, sTAB )
    call Uppercase(sItem)
    pConfig%BMASK(iRecNum)%sBasinDescription = TRIM(sItem)
    call Assert( iStat == 0, &
      "Error reading basin description in basin mask table" )
    write(UNIT=LU_LOG,FMT=*)  "Basin description = ",TRIM(pConfig%BMASK(iRecNum)%sBasinDescription)

    call chomp( sRecord, sItem, sTAB )
    pConfig%BMASK(iRecNum)%sPestGroup = TRIM(ADJUSTL(sItem))
    call Assert( iStat == 0, &
      "Error reading PEST group in basin mask table" )
    write(UNIT=LU_LOG,FMT=*)  "PEST group = ",TRIM(pConfig%BMASK(iRecNum)%sPestGroup)

    call chomp( sRecord, sItem, sTAB )
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%BMASK(iRecNum)%rPestWeight
    call Assert( iStat == 0, &
      "Error reading PEST observation weight in basin mask table" )
    write(sBuf,FMT="(F12.3)") pConfig%BMASK(iRecNum)%rPestWeight
    write(UNIT=LU_LOG,FMT=*)  "PEST weight = "//TRIM(sBuf)

    call chomp( sRecord, sItem, sTAB )
!    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%BMASK(iRecNum)%sBasinMaskFilename
    pConfig%BMASK(iRecNum)%sBasinMaskFilename = TRIM(ADJUSTL(sItem))
    call Assert( iStat == 0, &
      "Error reading basin mask filename in basin mask table" )
    write(UNIT=LU_LOG,FMT=*)  "Basin mask filename = ",TRIM(pConfig%BMASK(iRecNum)%sBasinMaskFilename)

    call chomp( sRecord, sItem, sTAB )
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%BMASK(iRecNum)%sFileType
    call Assert( iStat == 0, &
      "Error reading basin mask file type in basin mask table" )
    write(UNIT=LU_LOG,FMT=*)  "Basin mask filetype = ",TRIM(pConfig%BMASK(iRecNum)%sFileType)

    call chomp( sRecord, sItem, sTAB )
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%BMASK(iRecNum)%rQb
    call Assert( iStat == 0, &
      "Error reading baseflow estimate Qb in basin mask table" )
    write(UNIT=LU_LOG,FMT=*)  "Qb = ",pConfig%BMASK(iRecNum)%rQb

    call chomp( sRecord, sItem, sTAB )
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%BMASK(iRecNum)%rDrainageArea
    call Assert( iStat == 0, &
      "Error reading basin drainage area in basin mask table" )
    write(UNIT=LU_LOG,FMT=*)  "Drainage area = ", &
        pConfig%BMASK(iRecNum)%rDrainageArea

    write(UNIT=LU_LOG,FMT=*) " Attempting to read mask file: ", &
       TRIM(pConfig%BMASK(iRecNum)%sBasinMaskFilename)
    pConfig%BMASK(iRecNum)%pGrd => &
           grid_Read(TRIM(pConfig%BMASK(iRecNum)%sBasinMaskFilename), &
          "ARC_GRID", T_SGL_GRID )
    call Assert( grid_Conform( pGrd, pConfig%BMASK(iRecNum)%pGrd ), &
              "Non-conforming grid - filename: " &
              // TRIM(pConfig%BMASK(iRecNum)%sBasinMaskFilename), &
              TRIM(__FILE__),__LINE__)

    iRecNum = iRecNum + 1

  end do BMASK

  flush(UNIT=LU_LOG)

end subroutine ReadBasinMaskTable

subroutine ReadSimpleMaskTable ( pConfig , pGrd)

  use types
  use graph
  use swb_grid
  implicit none

  !! reads the mask data file for subsequent processing
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  type (T_GENERAL_GRID), pointer :: pGrd            ! pointer to model grid

  ! [ LOCALS ]
  integer (kind=T_INT) :: iStat, iNumMaskFiles, i, iRecNum, iSize
  character (len=256) :: sRecord                  ! Input file text buffer
  character (len=256) :: sItem                    ! Key word read from sRecord
  character (len=256) :: sBuf

  ! open basin mask file
  open ( LU_MASK, file=pConfig%sBasinMaskFilename, &
            status="OLD", iostat=iStat )
  call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
            "Open failed for file: " // pConfig%sBasinMaskFilename )

  ! read first line of file
  read ( unit=LU_MASK, fmt="(a256)", iostat=iStat ) sRecord
  call Assert( iStat == 0, &
     "Error reading first line of simple mask table" )

  ! read mask file to obtain expected number of mask files
  call chomp( sRecord, sItem, sTAB )
  call Uppercase( sItem )
  if ( sItem == "NUM_MASK_FILES" ) then
    call chomp( sRecord, sItem, sTAB )
    read ( unit=sItem, fmt=*, iostat=iStat ) iNumMaskFiles
    call Assert( iStat == 0, "Failed to read number of mask files" )
    write(UNIT=LU_LOG,FMT=*)  "==> allocating memory for",iNumMaskFiles, &
       " mask files within basin mask table"
  else
    call Assert( lFALSE, &
       "Unknown option in simple mask table; was expecting NUM_MASK_FILES #")
  end if

  ! read (AND IGNORE) second line of file
  read ( unit=LU_MASK, fmt="(a256)", iostat=iStat ) sRecord
  call Assert( iStat == 0, &
     "Error reading second line of simple mask table" )

  ! now allocate memory for MASK table
  allocate ( pConfig%BMASK( iNumMaskFiles ), stat=iStat )
  call Assert ( iStat == 0, &
    "Could not allocate space for simple mask data structure" )

  iSize = size(pConfig%BMASK,1)

  iRecNum = 1

  BMASK: do

    read ( unit=LU_MASK, fmt="(a256)", iostat=iStat ) sRecord
    if ( iStat < 0 ) exit     ! EOF mark
    if ( sRecord(1:1) == "#" ) cycle      ! Ignore comment lines

    if(iRecNum > iSize) then
      write(UNIT=LU_LOG,FMT=*) ""
      write(UNIT=LU_LOG,FMT=*)  " *** The maximum number of mask table elements has"
      write(UNIT=LU_LOG,FMT=*)  "     been read in before reaching the end of the file."
      write(UNIT=LU_LOG,FMT=*) ""
      write(UNIT=LU_LOG,FMT=*)  "     size of allocated memory for SIMPLE MASK table: ",iSize
      write(UNIT=LU_LOG,FMT=*)  "     current record number: ", iRecNum
      exit
    end if

    write(UNIT=LU_LOG,FMT=*) ""
    write(UNIT=LU_LOG,FMT=*)  "-----------------------------------------------------------"
    write(UNIT=LU_LOG,FMT=*)  "Reading mask record number ",iRecNum, " of ",iNumMaskFiles
    write(UNIT=LU_LOG,FMT=*) ""

    call chomp( sRecord, sItem, sTAB )
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%BMASK(iRecNum)%sUSGS_UpstreamOrderID
    call Assert( iStat == 0, &
      "Error reading ID number in mask table" )
    write(UNIT=LU_LOG,FMT=*)  "Upstream order ID = ",TRIM(pConfig%BMASK(iRecNum)%sUSGS_UpstreamOrderID)

    call chomp( sRecord, sItem, sTAB )
    call Uppercase(sItem)
    pConfig%BMASK(iRecNum)%sBasinDescription = TRIM(sItem)
    call Assert( iStat == 0, &
      "Error reading description in mask table" )
    write(UNIT=LU_LOG,FMT=*)  "Description = ",TRIM(pConfig%BMASK(iRecNum)%sBasinDescription)

    call chomp( sRecord, sItem, sTAB )
!    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%BMASK(iRecNum)%sBasinMaskFilename
    pConfig%BMASK(iRecNum)%sBasinMaskFilename = TRIM(ADJUSTL(sItem))
    call Assert( iStat == 0, &
      "Error reading filename in mask table" )
    write(UNIT=LU_LOG,FMT=*)  "Mask filename = ",TRIM(pConfig%BMASK(iRecNum)%sBasinMaskFilename)


    write(UNIT=LU_LOG,FMT=*) " Attempting to read mask file: ", &
       TRIM(pConfig%BMASK(iRecNum)%sBasinMaskFilename)
    pConfig%BMASK(iRecNum)%pGrd => &
           grid_Read(TRIM(pConfig%BMASK(iRecNum)%sBasinMaskFilename), &
          "ARC_GRID", T_SGL_GRID )
    call Assert( grid_Conform( pGrd, pConfig%BMASK(iRecNum)%pGrd ), &
              "Non-conforming grid - filename: " &
              // TRIM(pConfig%BMASK(iRecNum)%sBasinMaskFilename), &
              TRIM(__FILE__),__LINE__)

    iRecNum = iRecNum + 1

  end do BMASK

  flush(UNIT=LU_LOG)

end subroutine ReadSimpleMaskTable


end module swbstats_support

program swbstats

use types
use graph
use swb_stats
use swb_grid
use RLE
use swbstats_support
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

  integer (kind=T_INT) :: LU_SWBSTATS

  integer (kind=T_INT) :: iSWBStatsStartDate, iSWBStatsStartMM, &
                          iSWBStatsStartDD,iSWBStatsStartYYYY
  integer (kind=T_INT) :: iSWBStatsEndDate, iSWBStatsEndMM, &
                          iSWBStatsEndDD,iSWBStatsEndYYYY
  integer (kind=T_INT) :: iSWBStatsTotalNumDays
  integer (kind=T_INT) :: iSWBStatsOutputType = iBOTH
  integer (kind=T_INT) :: iSWBStatsType = iMEAN

  character (len=256) :: sTitleTxt
  character (len=10) :: sDateTxt

  type ( T_GENERAL_GRID ),pointer :: pGrd
  type ( T_GENERAL_GRID ),pointer :: pMonthGrd
  type ( T_GENERAL_GRID ),pointer :: pYearGrd
  type ( T_GENERAL_GRID ),pointer :: pSummaryGrd

  real(kind=T_SGL),dimension(:), allocatable :: rVal,rValSum,rPad

  character (len=8) :: sDate
  character (len=10) :: sTime
  character (len=5) :: sTZ

  character (len=1) :: sSlash = "/"
  character (len=20) :: sOutputFilePrefix = ""
  character (len=20) :: sOutputFileSuffix = "asc"
  character (len=256) :: sVarName
  character (len=256) :: sLabel = ""

  logical (kind=T_LOGICAL) :: lVerbose = lFALSE
  integer (kind=T_INT) :: iOutputFormat = OUTPUT_ARC
  logical (kind=T_LOGICAL) :: lEOF
  logical (kind=T_LOGICAL) :: lPrematureEOF = lFALSE
  integer (kind=T_INT) :: iMonthCount, iYearCount

  !> Global instantiation of a pointer of type T_MODEL_CONFIGURATION
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  type (T_GRID_COLLECTION), pointer :: mask_grd
  type (T_GENERAL_GRID), pointer :: input_grd

  ALLOCATE (pConfig, STAT=iStat)
  call Assert( iStat == 0, &
     "Could not allocate memory for model control data structure")

  call date_and_time(sDate,sTime,sTZ)

  write(sBuf,FMT=*) "SWBSTATS_LOGFILE_"//sDate//"_"//sTime(1:6)//".txt"

  ! open up the log file
  open(LU_LOG, file=TRIM(ADJUSTL(sBuf)),iostat=iStat,&
      status='REPLACE')
  call Assert( iStat == 0, "Problem opening log file file for output.")

  write(UNIT=LU_LOG,FMT=*) "Soil Water Balance Code BINARY FILE READER compiled on: "// &
    TRIM(__DATE__) //" "// TRIM(__TIME__)
  write(UNIT=LU_LOG,FMT=*) "SWB reader execution started: "// &
    sDate//"_"//sTime(1:6)

  flush(unit=LU_LOG)

  ! warning - calling a Fortran 2003 extension function here
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

    write(UNIT=*,FMT="(/,/,a)") &
      "Usage: swbstats [binary file name] [PERIOD|YEARLY|MONTHLY|DAILY]" &
        //" {SUM} {SURFER} {GRID|PLOT|BOTH|STATS} "
    write(UNIT=*,FMT="(t17,a,/)") "{start date (mm/dd/yyyy)} {end date (mm/dd/yyyy)} {VERBOSE} {basin_mask_filename}"

    write(UNIT=*,FMT="(/,t5,a)") "A filename and output frequency must be specified. All other arguments are OPTIONAL."
    write(UNIT=*,FMT="(/,t5,a)") "NOTES:"
    write(UNIT=*,FMT="(/,t7,a)") "1) output frequency will generate output at all frequencies less than"
    write(UNIT=*,FMT="(t10,a)") "that specified (i.e. MONTHLY will also generate YEARLY)"
    write(UNIT=*,FMT="(t12,a)") " - PERIOD: generates output for the entire period of"
    write(UNIT=*,FMT="(t15,a)") "simulation *or* the start and end dates entered on the command line"
    write(UNIT=*,FMT="(/,t7,a)") "2) If no output frequency is provided, a summary for the entire"
    write(UNIT=*,FMT="(t10,a)") "model simulation period is calculated"
    write(UNIT=*,FMT="(/,t7,a)") "3) SUM: will calculate statistics based on SUMS rather than MEAN values"
    write(UNIT=*,FMT="(/,t7,a)") "4) VERBOSE: writes daily min, mean, and max values to logfile"
    write(UNIT=*,FMT="(/,t7,a)") "5) SURFER: directs output to Surfer grids rather than Arc ASCII grids"
    write(UNIT=*,FMT="(/,t7,a)") "6) basin_mask_filename: specifies a list of basins for which stats will be calculated"

    stop

  end if

  call GET_COMMAND_ARGUMENT(1,sBinFile)

  open(nextunit(LU_SWBSTATS), FILE=TRIM(sBinFile),FORM='UNFORMATTED', &
       status='OLD',ACCESS='STREAM', ACTION='READ', IOSTAT=iStat )

  call Assert(iStat==0,"Failed to open input binary file: "//&
    TRIM(sBinFile),TRIM(__FILE__),__LINE__)

  read(UNIT=LU_SWBSTATS) iNX             ! Number of cells in the x-direction
  read(UNIT=LU_SWBSTATS) iNY             ! Number of cells in the y-direction
  read(UNIT=LU_SWBSTATS) iDataType       ! Type of the grid
  read(UNIT=LU_SWBSTATS) rGridCellSize   ! size of one side of a grid cell
  read(UNIT=LU_SWBSTATS) iLengthUnits    ! length units code
  read(UNIT=LU_SWBSTATS) iVariableNumber ! STAT_INFO variable number
  read(UNIT=LU_SWBSTATS) iRLE_MULT       ! RLE Multiplier
  read(UNIT=LU_SWBSTATS) rRLE_OFFSET     ! RLE Offset
  read(UNIT=LU_SWBSTATS) rX0, rX1        ! World-coordinate range in X
  read(UNIT=LU_SWBSTATS) rY0, rY1        ! World-coordinate range in Y
  read(UNIT=LU_SWBSTATS) iStartMM, iStartDD, iStartYYYY
  read(UNIT=LU_SWBSTATS) iEndMM, iEndDD, iEndYYYY

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

  pGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, T_SGL_GRID)
  pMonthGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, T_SGL_GRID)
  pYearGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, T_SGL_GRID)
  pSummaryGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, T_SGL_GRID)

  ! set default values for program options
  iSWBStatsStartDate = julian_day ( iStartYYYY, iStartMM, iStartDD)
  iSWBStatsEndDate = julian_day ( iEndYYYY, iEndMM, iEndDD)
  STAT_INFO(iVariableNumber)%iDailyOutput = iNONE
  STAT_INFO(iVariableNumber)%iMonthlyOutput = iNONE
  STAT_INFO(iVariableNumber)%iAnnualOutput = iNONE

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

  sVarName = TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)

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
      iSWBStatsOutputType = iGRID
      STAT_INFO(iVariableNumber)%iAnnualOutput = iGRID
    elseif(TRIM(ADJUSTL(sBuf)) .eq. "PLOT" &
      .or. TRIM(ADJUSTL(sBuf)) .eq. "GRAPH") then
      iSWBStatsOutputType = iGRAPH
      STAT_INFO(iVariableNumber)%iAnnualOutput = iGRAPH
    elseif(TRIM(ADJUSTL(sBuf)) .eq. "BOTH") then
      STAT_INFO(iVariableNumber)%iAnnualOutput = iBOTH
      iSWBStatsOutputType = iBOTH
    elseif(TRIM(ADJUSTL(sBuf)) .eq. "PERIOD") then

    elseif(TRIM(ADJUSTL(sBuf)) .eq. "STATS") then
      STAT_INFO(iVariableNumber)%iAnnualOutput = iSTATS
      iSWBStatsOutputType = iSTATS
    elseif(TRIM(ADJUSTL(sBuf)) .eq. "SUM") then
      iSWBStatsType = iSUM
    elseif(TRIM(ADJUSTL(sBuf)) .eq. "DEFAULT") then
      iSWBStatsStartDate = julian_day ( iStartYYYY, iStartMM, iStartDD)
      iSWBStatsEndDate = julian_day ( iEndYYYY, iEndMM, iEndDD)
      STAT_INFO(iVariableNumber)%iDailyOutput = iNONE
      STAT_INFO(iVariableNumber)%iMonthlyOutput = iNONE
      STAT_INFO(iVariableNumber)%iAnnualOutput = iBOTH
    elseif(TRIM(ADJUSTL(sBuf)) .eq. "YEARLY" &
      .or. TRIM(ADJUSTL(sBuf)) .eq. "ANNUAL") then
      STAT_INFO(iVariableNumber)%iAnnualOutput = iSWBStatsOutputType
    elseif(TRIM(ADJUSTL(sBuf)) .eq. "MONTHLY") then
      STAT_INFO(iVariableNumber)%iAnnualOutput = iSWBStatsOutputType
      STAT_INFO(iVariableNumber)%iMonthlyOutput = iSWBStatsOutputType
    elseif(TRIM(ADJUSTL(sBuf)) .eq. "DAILY") then
      STAT_INFO(iVariableNumber)%iAnnualOutput = iSWBStatsOutputType
      STAT_INFO(iVariableNumber)%iMonthlyOutput = iSWBStatsOutputType
      STAT_INFO(iVariableNumber)%iDailyOutput = iSWBStatsOutputType
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
      pConfig%sBasinMaskFilename = TRIM(ADJUSTL(sBuf))
      call ReadBasinMaskTable ( pConfig , pGrd)
      ALLOCATE (pConfig%SSF_FILES(size(pConfig%BMASK)), STAT=iStat)
    endif

  end do

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

  write(unit=LU_STD_OUT,fmt="(/,a,/)") "  Summary of output to be generated:"
  write(unit=LU_STD_OUT,fmt="(t20,a,t28,a,t36,a,t44,a)") &
    "NONE","PLOT","GRID","STATS"

  k = STAT_INFO(iVariableNumber)%iDailyOutput
  if(k==iNONE) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Daily"," **","",""
  if(k==iGRID) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Daily","",""," **"
  if(k==iGRAPH) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Daily",""," **",""
  if(k==iBOTH) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Daily",""," **"," **"
  if(k==iSTATS) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a,t44,a)") "Daily",""," ",""," **"

  k = STAT_INFO(iVariableNumber)%iMonthlyOutput
  if(k==iNONE) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Monthly"," **","",""
  if(k==iGRID) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Monthly","",""," **"
  if(k==iGRAPH) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Monthly",""," **",""
  if(k==iBOTH) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Monthly",""," **"," **"
  if(k==iSTATS) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a,t44,a)") "Monthly",""," ",""," **"

  k = STAT_INFO(iVariableNumber)%iAnnualOutput
  if(k==iNONE) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Annual"," **","",""
  if(k==iGRID) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Annual","",""," **"
  if(k==iGRAPH) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Annual",""," **",""
  if(k==iBOTH) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a)") "Annual",""," **"," **"
  if(k==iSTATS) write(unit=LU_STD_OUT,fmt="(t3,a,t20,a,t28,a,t36,a,t44,a)") "Annual",""," ",""," **"

write(unit=LU_LOG,fmt="(/,a,/)") "  Summary of output to be generated:"

  write(unit=LU_LOG,fmt="(t20,a,t28,a,t36,a,t44,a)") &
    "NONE","PLOT","GRID","STATS"

  k = STAT_INFO(iVariableNumber)%iDailyOutput
  if(k==iNONE) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Daily"," **","",""
  if(k==iGRID) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Daily","",""," **"
  if(k==iGRAPH) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Daily",""," **",""
  if(k==iBOTH) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Daily",""," **"," **"
  if(k==iSTATS) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a,t44,a)") "Daily",""," ",""," **"

  k = STAT_INFO(iVariableNumber)%iMonthlyOutput
  if(k==iNONE) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Monthly"," **","",""
  if(k==iGRID) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Monthly","",""," **"
  if(k==iGRAPH) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Monthly",""," **",""
  if(k==iBOTH) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Monthly",""," **"," **"
  if(k==iSTATS) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a,t44,a)") "Monthly",""," ",""," **"

  k = STAT_INFO(iVariableNumber)%iAnnualOutput
  if(k==iNONE) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Annual"," **","",""
  if(k==iGRID) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Annual","",""," **"
  if(k==iGRAPH) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Annual",""," **",""
  if(k==iBOTH) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a)") "Annual",""," **"," **"
  if(k==iSTATS) write(unit=LU_LOG,fmt="(t3,a,t20,a,t28,a,t36,a,t44,a)") "Annual",""," ",""," **"

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
  pSummaryGrd%rData(:,:)= rZERO
  iMonthCount = 0
  iYearCount = 0

  do
    ! read in the current date
    read(UNIT=LU_SWBSTATS,iostat=iStat) &
       iCurrDD, iCurrMM, iCurrYYYY, iCurrDOY
    if(iStat /= 0) then
      exit
    end if

    write(sDateTxt,fmt="(i2.2,'/',i2.2,'/',i4.4)") &
      iCurrMM, iCurrDD, iCurrYYYY

    write(*,fmt="(a,a)") "Processing: ",trim(sDateTxt)

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
    call RLE_readByte(LU_SWBSTATS,iRLE_MULT, &
       rRLE_OFFSET, rVal,iNumGridCells,lEOF)
    if(lEOF) exit

    !> if current date does not fall within desired date range, keep
    !> reading data, or if current date is after the end of the
    !> desired date range, stop reading and get out
    if(iCurrJD < iSWBStatsStartDate) then
      cycle
    elseif(iCurrJD > iSWBStatsEndDate) then
      exit
    endif

    iYearCount = iYearCount + 1

    pGrd%rData(:,:)=RESHAPE(rVal,(/iNX,iNY/),PAD=rPad)

    if(STAT_INFO(iVariableNumber)%iMonthlyOutput /= iNONE) then
        pMonthGrd%rData = pMonthGrd%rData + pGrd%rData
        iMonthCount = iMonthCount + 1
    endif

    if(STAT_INFO(iVariableNumber)%iAnnualOutput /= iNONE) then
        pYearGrd%rData = pYearGrd%rData + pGrd%rData
        iYearCount = iYearCount + 1
    endif

    pSummaryGrd%rData = pSummaryGrd%rData + pGrd%rData

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

    write(sLabel,FMT="(i2.2,'/',i2.2'/',i4.4)") iCurrMM, iCurrDD, iCurrYYYY
    if(STAT_INFO(iVariableNumber)%iDailyOutput==iSTATS) &
        call CalcBasinStats(pGrd, pConfig, sVarName, sLabel)

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

      if(STAT_INFO(iVariableNumber)%iMonthlyOutput==iSTATS) then

        write(sLabel,FMT="(i2.2,'/',i4.4)") iCurrMM, iCurrYYYY

        if(iSWBStatsType == iMEAN) then
          call CalcBasinStats(pMonthGrd, pConfig, sVarName, sLabel, iMonthCount)
        else
          call CalcBasinStats(pMonthGrd, pConfig, sVarName, sLabel)
        endif
      endif

      if((STAT_INFO(iVariableNumber)%iMonthlyOutput==iGRID &
         .or. STAT_INFO(iVariableNumber)%iMonthlyOutput==iBOTH)) then

        if(iSWBStatsType == iSUM) then

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

        else

          write(sOutputFilename,FMT="(A,'_',i4.4,'_',i2.2,'.',a)") &
             "MEAN_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
             iCurrYYYY,iCurrMM,trim(sOutputFileSuffix)

          if ( iOutputFormat == OUTPUT_SURFER ) then
             call grid_WriteSurferGrid(TRIM(sOutputFilename), &
               rX0,rX1,rY0,rY1,&
                 pMonthGrd%rData(:,:) / REAL(iMonthCount))
          else
             call grid_WriteArcGrid(TRIM(sOutputFilename), &
                rX0,rX1,rY0,rY1, &
                 pMonthGrd%rData(:,:) / REAL(iMonthCount))
          end if

        endif

      end if

      if(STAT_INFO(iVariableNumber)%iMonthlyOutput==iGRAPH &
         .or. STAT_INFO(iVariableNumber)%iMonthlyOutput==iBOTH) then

        if(iSWBStatsType == iSUM) then
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

          pMonthGrd%rData = pMonthGrd%rData / REAL(iMonthCount)

          call make_shaded_contour(pMonthGrd, TRIM(sOutputFilename), &
             TRIM(sTitleTxt), TRIM(STAT_INFO(iVariableNumber)%sUNITS))

        endif

      endif

      pMonthGrd%rData=rZERO
      iMonthCount = 0

    endif

  !------------------------- YEARLY ANALYSIS

    if(lYearEnd) then

      if(STAT_INFO(iVariableNumber)%iAnnualOutput == iSTATS) then
        write(sLabel,FMT="(i4.4)") iCurrYYYY
        if(iSWBStatsType == iMEAN) then
          call CalcBasinStats(pYearGrd, pConfig, sVarName, sLabel, iYearCount)
        else
          call CalcBasinStats(pYearGrd, pConfig, sVarName, sLabel)
        endif
      endif

      if((STAT_INFO(iVariableNumber)%iAnnualOutput == iGRID &
         .or. STAT_INFO(iVariableNumber)%iAnnualOutput == iBOTH)) then

        if(iSWBStatsType == iSUM) then

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

        else

          ! now repeat for reporting of MEAN values
          write(sOutputFilename,FMT="(A,'_',i4.4,'.',a)") &
             "MEAN_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
             iCurrYYYY,trim(sOutputFileSuffix)

          if ( iOutputFormat == OUTPUT_SURFER ) then
             call grid_WriteSurferGrid(TRIM(sOutputFilename), &
               rX0,rX1,rY0,rY1,pYearGrd%rData / REAL(iYearCount))
          else
             call grid_WriteArcGrid(TRIM(sOutputFilename), &
                rX0,rX1,rY0,rY1,pYearGrd%rData / REAL(iYearCount))
          end if

        endif

      end if

      ! now produce ANNUAL PLOTS
      if(STAT_INFO(iVariableNumber)%iAnnualOutput == iGRAPH &
         .or. STAT_INFO(iVariableNumber)%iAnnualOutput == iBOTH) then

        if(iSWBStatsType == iSUM) then

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

          pYearGrd%rData = pYearGrd%rData / REAL(iYearCount)

          call make_shaded_contour(pYearGrd, TRIM(sOutputFilename), TRIM(sTitleTxt), &
            TRIM(STAT_INFO(iVariableNumber)%sUNITS))

        endif

      end if

      pYearGrd%rData=rZERO
      iYearCount = 0

    end if

  end do

  !------------------------- SUMMARY ANALYSIS

  iSWBStatsTotalNumDays = iSWBStatsEndDate - iSWBStatsStartDate + 1

  write(sLabel,fmt="(i02.2,'_',i02.2,'_',i04.4,"// &
      "'_to_',i02.2,'_',i02.2,'_',i04.4)") iSWBStatsStartMM, &
      iSWBStatsStartDD, iSWBStatsStartYYYY, &
      iSWBStatsEndMM, iSWBStatsEndDD, iSWBStatsEndYYYY

  if( iSWBStatsOutputType == iSTATS ) then
    if(iSWBStatsType == iMEAN) then
      call CalcBasinStats(pSummaryGrd, pConfig, sVarName, sLabel, iSWBStatsTotalNumDays)
    else
      call CalcBasinStats(pSummaryGrd, pConfig, sVarName, sLabel)
    endif
  endif

  write(sOutputFilename,FMT="(A,'_',a,'.',a)") &
     "SUM_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
     trim(sLabel),trim(sOutputFileSuffix)

  if ( iOutputFormat == OUTPUT_SURFER ) then
    call grid_WriteSurferGrid(TRIM(sOutputFilename), &
       rX0,rX1,rY0,rY1,pSummaryGrd%rData(:,:))
  else
    call grid_WriteArcGrid(TRIM(sOutputFilename), &
       rX0,rX1,rY0,rY1,pSummaryGrd%rData(:,:))
  end if

  ! now repeat for reporting of MEAN values
  write(sOutputFilename,FMT="(A,'_',a,'.',a)") &
    "MEAN_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
     trim(sLabel),trim(sOutputFileSuffix)

  if ( iOutputFormat == OUTPUT_SURFER ) then
    call grid_WriteSurferGrid(TRIM(sOutputFilename), &
       rX0,rX1,rY0,rY1,pSummaryGrd%rData / REAL(iSWBStatsTotalNumDays))
  else
    call grid_WriteArcGrid(TRIM(sOutputFilename), &
       rX0,rX1,rY0,rY1,pSummaryGrd%rData / REAL(iSWBStatsTotalNumDays))
  end if

  write(sOutputFilename,FMT="(A,'_',a,'.png')") &
    "SUM_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), trim(sLabel)

  write(sTitleTxt,fmt="('SUM of daily ',a,' for ',i02.2,'/',i02.2,'/',i04.4,"// &
      "' to ',i02.2,'/',i02.2,'/',i04.4)") &
      TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), iSWBStatsStartMM, &
      iSWBStatsStartDD, iSWBStatsStartYYYY, &
      iSWBStatsEndMM, iSWBStatsEndDD, iSWBStatsEndYYYY

  call make_shaded_contour(pSummaryGrd, TRIM(sOutputFilename), TRIM(sTitleTxt), &
    TRIM(STAT_INFO(iVariableNumber)%sUNITS))

  ! now repeat for MEAN value reporting
  write(sOutputFilename,FMT="(A,'_',a,'.png')") &
     "MEAN_"//TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), trim(sLabel)

  write(sTitleTxt,fmt="('MEAN of daily ',a,' for ',i02.2,'/',i02.2,'/',i04.4,"// &
      "' to ',i02.2,'/',i02.2,'/',i04.4)") &
      TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), iSWBStatsStartMM, &
      iSWBStatsStartDD, iSWBStatsStartYYYY, &
      iSWBStatsEndMM, iSWBStatsEndDD, iSWBStatsEndYYYY

  pSummaryGrd%rData = pSummaryGrd%rData / REAL(iSWBStatsTotalNumDays)

  call make_shaded_contour(pSummaryGrd, TRIM(sOutputFilename), TRIM(sTitleTxt), &
    TRIM(STAT_INFO(iVariableNumber)%sUNITS))


  close(unit=LU_LOG)

end program swbstats
