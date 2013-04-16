module swbstats_support

  use types
  implicit none

  logical (kind=T_LOGICAL) :: lCUMULATIVE = lFALSE
  logical (kind=T_LOGICAL) :: lSUM = lFALSE
  logical (kind=T_LOGICAL) :: lPERIOD_SLICE = lFALSE
  logical (kind=T_LOGICAL) :: lRESET = lFALSE
  logical (kind=T_LOGICAL) :: lVERBOSE = lFALSE
  logical (kind=T_LOGICAL) :: lMASKSSF = lFALSE
  logical (kind=T_LOGICAL) :: lBASINSTATS = lFALSE
  logical (kind=T_LOGICAL) :: lMASKSTATS = lFALSE
  logical (kind=T_LOGICAL) :: lF_TO_C = lFALSE
  logical (kind=T_LOGICAL) :: lGAP_DIFFERENCE = lFALSE

  character (len=256) :: sStatsDescription = ""
  integer (kind=T_INT) :: LU_STATS

  integer (kind=T_INT) :: iSlcStartMM = 1
  integer (kind=T_INT) :: iSlcStartDD = 1
  integer (kind=T_INT) :: iSlcEndMM = 12
  integer (kind=T_INT) :: iSlcEndDD = 31
  logical (kind=T_LOGICAL) :: lPRINT

  character (len=78), dimension(34), parameter :: sUsageText = &
    [ "Usage: swbstats [binary file name]                                            ", &
      "                {YEARLY|MONTHLY|DAILY}                                        ", &
      "                {SUM}                                                         ", &
      "                {SURFER}                                                      ", &
      "                {GRID|PLOT|BOTH|STATS}                                        ", &
      "                {BASIN_MASK basin mask filename }                             ", &
      "                {MASK mask filename}                                          ", &
      "                {CUMULATIVE}                                                  ", &
      "                {PERIOD_SLICE start date (mm/dd) end date (mm/dd)}            ", &
      "                {start date (mm/dd/yyyy)}                                     ", &
      "                {end date (mm/dd/yyyy)}                                       ", &
      "                {VERBOSE}                                                     ", &
      "                                                                              ", &
      "A filename MUST be specified. All other arguments are OPTIONAL.               ", &
      "                                                                              ", &
      "NOTES:                                                                        ", &
      "                                                                              ", &
      "  1) SWBSTATS will generate output at all frequencies less than               ", &
      "     that specified; for example, MONTHLY will also generate YEARLY output    ", &
      "  2) If no output frequency is provided, a summary for the entire             ", &
      "     model simulation period is calculated                                    ", &
      "  3) SUM: will calculate statistics based on SUMS rather than MEAN values     ", &
      "  4) VERBOSE: writes daily min, mean, and max values to logfile               ", &
      "  5) SURFER: directs output to Surfer grids rather than Arc ASCII grids       ", &
      "  6) BASIN_MASK: specifies a list of basins for which stats will be calculated", &
      "     each entry in the basin list specifies an ARC ASCII basin mask for which ", &
      "     statistics will be calculated                                            ", &
      "  7) MASK: specifies a single mask file; stats are calculated for each        ", &
      "     distinct integer value present in the mask file                          ", &
      "  8) CUMULATIVE: specifies that MASK statistics are cumulative;               ", &
      "     these cumulative statistics are reset at the beginning of each year      ", &
      "  9) PERIOD_SLICE: calculates stats on a specified subset of days each year   ", &
      "     for example, 'PERIOD_SLICE 06/01 08/31' will report stats for the        ", &
      "     subset of output that occurs between June 1st and Aug. 31st of each year " ]

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

    open(newunit=LU_STATS,FILE="SWB_BASIN_STATS_"//trim(sVarName)//"_"//trim(sStatsDescription)//".txt", &
          iostat=iStat, STATUS='REPLACE')
    call Assert ( iStat == 0, &
      "Could not open BASIN statistics file")

    write(UNIT=LU_STATS,FMT="(A,a)",advance='NO') "Period",sTAB

    do k=1,iNumRecs-1
      write(UNIT=LU_STATS,FMT="(A,a)",advance='NO') &
          ADJUSTL(TRIM(pConfig%BMASK(k)%sUSGS_UpstreamOrderID)),sTAB
    end do

    write(UNIT=LU_STATS,FMT="(A)") &
       ADJUSTL(TRIM(pConfig%BMASK(iNumRecs)%sUSGS_UpstreamOrderID))

    pConfig%lFirstDayOfSimulation = lFALSE

  else   ! append to files

    open(LU_STATS,FILE="SWB_BASIN_STATS_"//trim(sVarName)//"_"//trim(sStatsDescription)//".txt", &
        POSITION='APPEND', STATUS='OLD')
    call Assert ( iStat == 0, &
      "Could not open BASIN statistics file")

  end if

  write(UNIT=LU_STATS,FMT="(a,a)", advance='NO') TRIM(sLabel),sTAB

  do k = 1,iNumRecs

    iCount = COUNT(pConfig%BMASK(k)%pGrd%rData>0)

    ! sum of the sum of values within basin mask boundaries
    rSum = SUM(pGrd%rData,MASK=pConfig%BMASK(k)%pGrd%rData>0) / rDenominator
    rMax = MAXVAL(pGrd%rData,MASK=pConfig%BMASK(k)%pGrd%rData>0) / rDenominator
    rMin = MINVAL(pGrd%rData,MASK=pConfig%BMASK(k)%pGrd%rData>0) / rDenominator

    rAvg = rSum / iCount

    if(lVERBOSE) then
      write(UNIT=LU_LOG,FMT="(A)") ""
      write(UNIT=LU_LOG,FMT="(5x,A)") TRIM(pConfig%BMASK(k)%sBasinDescription)
      write(UNIT=LU_LOG,FMT="(5x,A)") "==> "//TRIM(sLabel)
      write(UNIT=LU_LOG,FMT="(5x,'Drainage area (sq mi):', f14.2)") &
          pConfig%BMASK(k)%rDrainageArea
      write(UNIT=LU_LOG,FMT="(8x,A7,i12)") "count:",iCount
      write(UNIT=LU_LOG,FMT="(8x,A7,f14.2)") "sum:",rSum
      write(UNIT=LU_LOG,FMT="(8x,A7,f14.2)") "avg:",rAvg
      write(UNIT=LU_LOG,FMT="(A)") REPEAT("-",80)
    endif

    ! convert average value in inches to cubic feet
    rAvg = real(rAvg,kind=T_DBL) * real(pConfig%BMASK(k)%rDrainageArea,kind=T_DBL) &
               * 4.01449E9_T_DBL

    if( k < iNumRecs ) then

      write(UNIT=LU_STATS,FMT="(g16.8,a)", advance='NO') rAvg,sTAB

    else

      write(UNIT=LU_STATS,FMT="(g16.8)") rAvg

    endif

    close(UNIT=LU_MASK_FILE)

  end do

  flush(UNIT=LU_STATS)
  close(UNIT=LU_STATS)

end subroutine CalcBasinStats

!------------------------------------------------------------------------------

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
  integer (kind=T_INT), optional :: iNumDays

  ![LOCALS]
  integer (kind=T_INT) :: j, k, iStat, iCount, m, n
  integer (kind=T_INT) ::   iNumGridCells
  real (kind=T_SGL) :: rSum, rAvg, rMin, rMax
  real (kind=T_SGL) :: rBaseSum, rBaseAvg
  real (kind=T_DBL), dimension(:), allocatable, save :: rRunningSum
  integer (kind=T_INT), save :: i
  real (kind=T_DBL) :: rConversionFactor
  real (kind=T_SGL) :: rDenominator

  character (len=256) :: sBuf

  integer (kind=T_INT), save :: iNumRecs

  type (T_GENERAL_GRID), pointer :: input_grd

  if(present(iNumDays)) then
    rDenominator = real(iNumDays, kind=T_DBL)
  else
    rDenominator = 1_T_DBL
  end if

  if(pGrd%iLengthUnits == iGRID_LENGTH_UNITS_METERS) then
    rConversionFactor = 2589988.11_T_DBL
  else
    rConversionFactor = 27878400_T_DBL
  endif

  if(pConfig%lFirstDayOfSimulation) then

    iNumRecs = maxval(pMaskGrd%rData)
    if(lCUMULATIVE) then
      allocate(rRunningSum(iNumRecs))
      rRunningSum = 0_T_DBL
    endif


    sBuf = "SWB_"//trim(sVarName)//"_"//trim(sStatsDescription)//".txt"
    open(newunit=LU_STATS,FILE=trim(sBuf), &
          iostat=iStat, STATUS='REPLACE')
    call Assert ( iStat == 0, &
      "Could not open MASK statistics file "//dquote(sBuf))

    write(UNIT=LU_STATS,FMT="(A,a)",advance='NO') "Period",sTAB


    if(.not. lGAP_DIFFERENCE) then

      do k=1,iNumRecs - 1
        write(UNIT=LU_STATS,FMT="(A,a)",advance='NO') &
            trim(int2char(k)),sTAB
      end do

      write(UNIT=LU_STATS,FMT="(A)",advance='YES') trim(int2char(iNumRecs))

    else

      write(UNIT=LU_STATS,FMT="(A,a)",advance='NO') &
          trim(int2char(iNumRecs)),sTAB

      do k=1,iNumRecs - 2
        write(UNIT=LU_STATS,FMT="(A,a)",advance='NO') &
            trim(int2char(k)),sTAB
      end do

      write(UNIT=LU_STATS,FMT="(A)",advance='YES') &
          trim(int2char(iNumRecs - 1))

    endif

    pConfig%lFirstDayOfSimulation = lFALSE

  end if


  if(lPRINT) write(UNIT=LU_STATS,FMT="(a,a)", advance='NO') TRIM(sLabel),sTAB

  if(lRESET .and. lCUMULATIVE) rRunningSum = 0_T_DBL

  if(.not. lGAP_DIFFERENCE) then

    do k = 1,iNumRecs

      iCount = COUNT(pMaskGrd%rData == k)

      ! sum of the sum of values within basin mask boundaries
      rSum = SUM(pGrd%rData,MASK=pMaskGrd%rData == k) / rDenominator
      rMax = MAXVAL(pGrd%rData,MASK=pMaskGrd%rData == k) / rDenominator
      rMin = MINVAL(pGrd%rData,MASK=pMaskGrd%rData == k) / rDenominator

      rAvg = rSum / iCount

      if(lVERBOSE) then
        write(UNIT=LU_LOG,FMT="(A)") ""
        write(UNIT=LU_LOG,FMT="(5x,A)") "Summary for cells with mask value of: " &
           //trim(int2char(k) )
        write(UNIT=LU_LOG,FMT="(5x,A)") "==> "//TRIM(sLabel)
        write(UNIT=LU_LOG,FMT="(5x,'Grid cell area (sq mi):', f14.2)") &
            real(iCount, kind=T_DBL) * pGrd%rGridCellSize * pGrd%rGridCellSize / rConversionFactor
        write(UNIT=LU_LOG,FMT="(8x,A7,i12)") "count:",iCount
        write(UNIT=LU_LOG,FMT="(8x,A7,f14.2)") "sum:",rSum
        write(UNIT=LU_LOG,FMT="(8x,A7,f14.2)") "avg:",rAvg
        write(UNIT=LU_LOG,FMT="(A)") REPEAT("-",80)
      endif

      if(lCUMULATIVE) then

        rRunningSum(k) = rRunningSum(k) + rAvg

        if(lPRINT) then

          if( k < iNumRecs ) then

            write(UNIT=LU_STATS,FMT="(g16.8,a)", advance='NO') rRunningSum(k),sTAB

          else

            write(UNIT=LU_STATS,FMT="(g16.8)") rRunningSum(k)

          endif

        endif

      else

        if(lPRINT) then

          if( k < iNumRecs ) then

            write(UNIT=LU_STATS,FMT="(g16.8,a)", advance='NO') rAvg,sTAB

          else

            write(UNIT=LU_STATS,FMT="(g16.8)") rAvg

          endif

        endif

      endif

    enddo

  else

    iCount = COUNT(pMaskGrd%rData == iNumRecs)

    ! sum of the sum of values within basin mask boundaries
    rBaseSum = SUM(pGrd%rData,MASK=pMaskGrd%rData == iNumRecs) / rDenominator

    rBaseAvg = rBaseSum / iCount

    if(lPRINT) &
      write(UNIT=LU_STATS,FMT="(g16.8,a)", advance='NO') rBaseAvg,sTAB

    do k = 1,iNumRecs-1

      iCount = COUNT(pMaskGrd%rData == k)

      ! sum of the sum of values within basin mask boundaries
      rSum = SUM(pGrd%rData,MASK=pMaskGrd%rData == k) / rDenominator

      rAvg = rSum / iCount

      if(lVERBOSE) then
        write(UNIT=LU_LOG,FMT="(A)") ""
        write(UNIT=LU_LOG,FMT="(5x,A)") "Summary for cells with mask value of: " &
           //trim(int2char(k) )
        write(UNIT=LU_LOG,FMT="(5x,A)") "==> "//TRIM(sLabel)
        write(UNIT=LU_LOG,FMT="(5x,'Grid cell area (sq mi):', f14.2)") &
            real(iCount, kind=T_DBL) * pGrd%rGridCellSize * pGrd%rGridCellSize / rConversionFactor
        write(UNIT=LU_LOG,FMT="(8x,A7,i12)") "count:",iCount
        write(UNIT=LU_LOG,FMT="(8x,A7,f14.2)") "sum:",rSum
        write(UNIT=LU_LOG,FMT="(8x,A7,f14.2)") "avg:",rAvg
        write(UNIT=LU_LOG,FMT="(A)") REPEAT("-",80)
      endif

      if(lPRINT) then

        if( k < iNumRecs -1 ) then

          write(UNIT=LU_STATS,FMT="(g16.8,a)", advance='NO') rBaseAvg  - rAvg,sTAB

        else

          write(UNIT=LU_STATS,FMT="(g16.8)") rBaseAvg - rAvg

        endif

      endif

    enddo

  endif

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
  integer (kind=T_INT) :: iGridValue
  character (len=*) :: sLabel
  integer (kind=T_INT), optional :: iNumDays

  ![LOCALS]
  integer (kind=T_INT) :: j, iStat, iCount, m, n
  integer (kind=T_INT) ::   iNumGridCells
  real (kind=T_DBL) :: rSum, rAvg, rMin, rMax
  real (kind=T_DBL) :: rDenominator
  real (kind=T_DBL),save :: rConversionFactor
  real (kind=T_DBL),save :: rGridCellAreaSF

  character (len=256) :: sBuf

  integer (kind=T_INT), save :: iNumRecs

  type (T_GENERAL_GRID), pointer :: input_grd

  if(present(iNumDays)) then
    rDenominator = real(iNumDays, kind=T_DBL)
  else
    rDenominator = 1_T_DBL
  end if

  if(pConfig%lFirstDayOfSimulation) then

    ! conversion factor: grid cell sum to square feet
    if(pGrd%iLengthUnits == iGRID_LENGTH_UNITS_METERS) then
      rConversionFactor = 10.76391042_T_DBL
    else
      rConversionFactor = 1_T_DBL
    endif

    rGridCellAreaSF = real(pGrd%rGridCellSize, kind=T_DBL) &
      * real(pGrd%rGridCellSize, kind=T_DBL) * rConversionFactor

    sBuf = "SWB_"//trim(sVarName)//"_"//trim(sStatsDescription)//".ssf"
    open(newunit=LU_STATS,FILE=trim(sBuf), &
          iostat=iStat, STATUS='REPLACE')
    call Assert ( iStat == 0, &
      "Could not open PEST ssf file "//dquote(sBuf))

    pConfig%lFirstDayOfSimulation = lFALSE

  end if

  iCount = COUNT(pMaskGrd%rData == iGridValue)

  ! sum of the sum of values within basin mask boundaries
  rSum = SUM(pGrd%rData,MASK=pMaskGrd%rData == iGridValue) / rDenominator
  rMax = MAXVAL(pGrd%rData,MASK=pMaskGrd%rData == iGridValue) / rDenominator
  rMin = MINVAL(pGrd%rData,MASK=pMaskGrd%rData == iGridValue) / rDenominator

  rAvg = rSum / iCount

  if(lVERBOSE) then
    write(UNIT=LU_LOG,FMT="(A)") ""
    write(UNIT=LU_LOG,FMT="(5x,A)") "Summary for cells with mask value of: " &
       //trim(int2char(iGridValue) )
    write(UNIT=LU_LOG,FMT="(A)") "==> "//TRIM(sLabel)
    write(UNIT=LU_LOG,FMT="(2x,A,t30,i12)") "count:",iCount
    write(UNIT=LU_LOG,FMT="(2x,a,t30,f16.3)") "cell area:", &
       rGridCellAreaSF
    write(UNIT=LU_LOG,FMT="(2x,a,t30,f16.3)") "sum cell area:", &
      real(iCount, kind=T_DBL) * rGridCellAreaSF
    write(UNIT=LU_LOG,FMT="(2x,A,t30,f14.3)") "sum:",rSum
    write(UNIT=LU_LOG,FMT="(2x,A,t30,f14.3)") "avg:",rAvg
    write(UNIT=LU_LOG,FMT="(2x,'avg (cfs):',t30,g14.3)") rSum * rGridCellAreaSF / 86400_T_DBL / 12_T_DBL
    write(UNIT=LU_LOG,FMT="(A)") REPEAT("-",80)
  endif

  if(lPRINT) write(UNIT=LU_STATS,FMT="(a,3x,g16.8)") TRIM(sLabel), &
     rSum * rGridCellAreaSF / 86400_T_DBL / 12_T_DBL

  flush(UNIT=LU_STATS)

end subroutine CalcMaskStatsSSF

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

end module swbstats_support

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

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
  character (len=256)  :: sItem
  integer (kind=T_INT) :: iNumArgs
  integer (kind=T_INT) :: iNumGridCells
  integer (kind=T_INT) :: iStat
  integer (kind=T_INT) :: i, k
  integer (kind=T_INT) :: iDateNum = 0
  integer (kind=T_INT) :: iNumDaysInYear
  integer (kind=T_INT) :: iLen1, iLen2
  integer (kind=T_INT), dimension(2) :: iTempDate
  character (len=256) :: sSiteNumber
  character (len=7) :: sLengthUnits
  integer (kind=T_INT) :: iPos

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
  real (kind=T_DBL)    :: rX0_cntr, rX1_cntr
  real (kind=T_DBL)    :: rY0_cntr, rY1_cntr
  integer (kind=T_INT) :: iStartMM, iStartDD, iStartYYYY
  integer (kind=T_INT) :: iEndMM, iEndDD, iEndYYYY

  integer (kind=T_INT) :: iCurrMM, iCurrDD, iCurrYYYY, iCurrDOY, iCurrJD, iTemp
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
  integer (kind=T_INT) :: iSWBStatsOutputType = iBOTH
  integer (kind=T_INT) :: iSWBStatsType = iMEAN

  character (len=256) :: sTitleTxt
  character (len=10) :: sDateTxt

  type ( T_GENERAL_GRID ),pointer :: pGrd
  type ( T_GENERAL_GRID ),pointer :: pMaskGrd
  type ( T_GENERAL_GRID ),pointer :: pMonthGrd
  type ( T_GENERAL_GRID ),pointer :: pYearGrd
  type ( T_GENERAL_GRID ),pointer :: pSummaryGrd

  real(kind=T_SGL),dimension(:), allocatable :: rVal,rValSum,rPad, rValTmp

  character (len=8) :: sDate
  character (len=10) :: sTime
  character (len=5) :: sTZ

  character (len=1) :: sSlash = "/"
  character (len=20) :: sOutputFilePrefix = ""
  character (len=20) :: sOutputFileSuffix = "asc"
  character (len=256) :: sVarName
  character (len=256) :: sLabel = ""

  integer (kind=T_INT) :: iOutputFormat = OUTPUT_ARC
  logical (kind=T_LOGICAL) :: lYearBegin
  logical (kind=T_LOGICAL) :: lEOF
  logical (kind=T_LOGICAL) :: lPrematureEOF = lFALSE
  integer (kind=T_INT) :: iMonthCount, iYearCount, iPeriodCount
  integer (kind=T_INT) :: iGridCellValue

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

    do i=1,ubound(sUsageText,1)
      write(unit=*,fmt="(a)") sUsageText(i)
    enddo

    stop

  end if

  call GET_COMMAND_ARGUMENT(1,sBinFile)

  open(nextunit(LU_SWBSTATS), FILE=TRIM(sBinFile),FORM='UNFORMATTED', &
       status='OLD',ACCESS='STREAM', ACTION='READWRITE', IOSTAT=iStat )

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
    write(unit=LU_STD_OUT,fmt="(/,'  Grid dimensions (x,y): ',t28,a,', ',a)") &
      trim(int2char(iNX)), trim(int2char(iNY))
    write(sBuf,fmt="(f14.2)") rGridCellSize
    write(unit=LU_STD_OUT,fmt="('  Grid cell size: ',t28,a)") trim(adjustl(sBuf))
    write(unit=LU_STD_OUT,fmt="('  Grid cell units: ',t28)", advance="no")
    if(iLengthUnits == iGRID_LENGTH_UNITS_METERS) then
      write(unit=LU_STD_OUT,fmt="('meters',/)")
    else
      write(unit=LU_STD_OUT,fmt="('feet',/)")
    endif
    write(unit=LU_STD_OUT,fmt="('  Grid data type: ',t28,a)") trim(int2char(iDataType))
    write(unit=LU_STD_OUT,fmt="('  Length units code: ',t28,a)") trim(int2char(iLengthUnits))

    stop

  end if

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
    elseif(TRIM(ADJUSTL(sBuf)) .eq. "SUM") then
      iSWBStatsType = iSUM
      lSUM = lTRUE
    elseif(TRIM(ADJUSTL(sBuf)) .eq. "F_TO_C") then
      lF_TO_C = lTRUE
    elseif(TRIM(ADJUSTL(sBuf)) .eq. "GAP_DIFFERENCE") then
      lGAP_DIFFERENCE = lTRUE
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
      lVERBOSE = lTRUE
    elseif(TRIM(ADJUSTL(sBuf)) .eq. "CUMULATIVE") then
        lCUMULATIVE = lTRUE
        iSWBStatsType = iSUM
    elseif(TRIM(ADJUSTL(sBuf)) .eq. "SURFER") then
      iOutputFormat = OUTPUT_SURFER
      sOutputFileSuffix = "grd"
    elseif(iLen1 /= iLen2) then  ! we have found forward slashes...probably a date
      iDateNum = iDateNum + 1
      call Assert(iDateNum <=2, "Too many dates entered on the command line", &
        TRIM(__FILE__),__LINE__)
      iTempDate(iDateNum) = mmddyyyy2julian(sBuf)
    elseif(trim(adjustl(sBuf)) .eq. "PERIOD_SLICE") then
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

        print *, iSlcStartMM, iSlcStartDD
        print *, iSlcEndMM, iSlcEndDD

    elseif(TRIM(ADJUSTL(sBuf)) .eq. "BASIN_MASK") then
      i = i + 1
      lBASINSTATS = lTRUE
      STAT_INFO(iVariableNumber)%iAnnualOutput = iSTATS
      iSWBStatsOutputType = iSTATS
      call GET_COMMAND_ARGUMENT(i,sBuf)
      pConfig%sBasinMaskFilename = TRIM(ADJUSTL(sBuf))
      call ReadBasinMaskTable ( pConfig , pGrd)
      ALLOCATE (pConfig%SSF_FILES(size(pConfig%BMASK)), STAT=iStat)
    elseif(TRIM(ADJUSTL(sBuf)) .eq. "MASK") then
    lMASKSTATS = lTRUE
      STAT_INFO(iVariableNumber)%iAnnualOutput = iSTATS
      iSWBStatsOutputType = iSTATS
      i = i + 1
      call GET_COMMAND_ARGUMENT(i,sBuf)
      pMaskGrd => grid_Read(TRIM(sBuf), "ARC_GRID", T_SGL_GRID )
    elseif(TRIM(ADJUSTL(sBuf)) .eq. "MASK_SSF") then
        lMASKSSF = lTRUE
        STAT_INFO(iVariableNumber)%iDailyOutput = iSTATS
        iSWBStatsOutputType = iSTATS
        i = i + 1
        call GET_COMMAND_ARGUMENT(i,sBuf)
        pMaskGrd => grid_Read(TRIM(sBuf), "ARC_GRID", T_SGL_GRID )
        i = i + 1
        call GET_COMMAND_ARGUMENT(i,sBuf)
        sSiteNumber = trim(sBuf)
        i = i + 1
        call GET_COMMAND_ARGUMENT(i,sBuf)
        read(sBuf,*) iGridCellValue
    endif

    i = i + 1
    if (i > iNumArgs) exit

  end do

  call Assert(.not. (lBASINSTATS .and. lMASKSTATS), "Statistics may be generated" &
    //" for a set of basin masks OR a single mask file, not both!", &
    trim(__FILE__), __LINE__)

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
  allocate(rValTmp(iNX*iNY))
  allocate(rValSum(iNX*iNY))
  allocate(rPad(iNX*iNY))

  pGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, T_SGL_GRID)
  pMonthGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, T_SGL_GRID)
  pYearGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, T_SGL_GRID)
  pSummaryGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, T_SGL_GRID)

  ! copy some key information into the grid data structure
  pGrd%iDataType =iDataType              ! Type of the grid
  pGrd%rGridCellSize = rGridCellSize     ! size of one side of a grid cell
  pGrd%iLengthUnits = iLengthUnits       ! length units code

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
  iPeriodCount = 0

  do
    ! read in the current date
    read(UNIT=LU_SWBSTATS,iostat=iStat) &
       iCurrDD, iCurrMM, iCurrYYYY, iCurrDOY

    if(iStat /= 0) then
      exit
    end if

    write(sDateTxt,fmt="(i2.2,'/',i2.2,'/',i4.4)") &
      iCurrMM, iCurrDD, iCurrYYYY

    ! this logical allows us to output results for a season or period each year
    lPRINT = (iCurrMM > iSlcStartMM .and. iCurrMM < iSlcEndMM) &
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

    lRESET = ( iCurrMM == 1 .and. iCurrDD == 1)
    pGrd%rData(:,:)= rZERO

    !> if current date does not fall within desired date range, keep
    !> reading data, or if current date is after the end of the
    !> desired date range, stop reading and get out
    if( (iCurrJD < iSWBStatsStartDate .or. .not. lPRINT) .and. .not. lCUMULATIVE ) then

      do
        read(UNIT=LU_SWBSTATS,iostat=iStat) iTemp

        ! skip all of the RLE nonsense if we don't care about the contents
        ! note that we must subtract 4 to account for the fact that by this
        ! point in the program execution
        call assert(iStat == 0, "Problem fast-forwarding binary file", &
          trim(__FILE__), __LINE__)
        if(iTemp == iEOF) then
          exit
        else
          cycle
        endif

      enddo

      cycle

    elseif(iCurrJD > iSWBStatsEndDate) then
      exit
    endif

    ! name "RLE_readByte" is misleading, since the return value (rVal)
    ! is actually a vector of all daily values with dimension (iNY*iNX)
    call RLE_readByte(LU_SWBSTATS,iRLE_MULT, rRLE_OFFSET, rVal,iNumGridCells,lEOF)
    if(lEOF) exit

    if(lF_TO_C) then
      rValTmp = 5./9. * (rVal -32.)
      pGrd%rData(:,:)=RESHAPE(rValTmp,(/iNX,iNY/),PAD=rPad)
    else
      pGrd%rData(:,:)=RESHAPE(rVal,(/iNX,iNY/),PAD=rPad)
    endif

    ! keep track of how many days' worth of results are stored in the month grid
    if(STAT_INFO(iVariableNumber)%iMonthlyOutput /= iNONE) then
        pMonthGrd%rData = pMonthGrd%rData + pGrd%rData
        iMonthCount = iMonthCount + 1
    endif

    ! keep track of how many days' worth of results are stored in the year grid
    if(STAT_INFO(iVariableNumber)%iAnnualOutput /= iNONE) then
        pYearGrd%rData = pYearGrd%rData + pGrd%rData
        iYearCount = iYearCount + 1
    endif

    pSummaryGrd%rData = pSummaryGrd%rData + pGrd%rData
    iPeriodCount = iPeriodCount + 1

    if(lVERBOSE) then
      call stats_WriteMinMeanMax(LU_LOG, &
         TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)//": "//trim(sDateTxt), &
         pGrd%rData(:,:))

      if(lCUMULATIVE) then
        call stats_WriteMinMeanMax(LU_STD_OUT, &
          TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)//": "//trim(sDateTxt), &
          pSummaryGrd%rData(:,:))
      else
        call stats_WriteMinMeanMax(LU_STD_OUT, &
          TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)//": "//trim(sDateTxt), &
          pGrd%rData(:,:))
      endif
    else
      write(LU_STD_OUT, fmt="(a)") TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)//": "//trim(sDateTxt)
    endif

    if((STAT_INFO(iVariableNumber)%iDailyOutput==iGRID &
       .or. STAT_INFO(iVariableNumber)%iDailyOutput==iBOTH) &
       .and. lPRINT ) then

      write(sOutputFilename,FMT="(A,'_',i4.4,'_',i2.2,'_',i2.2,'.',a)") &
         TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
         iCurrYYYY,iCurrMM,iCurrDD, &
         trim(sOutputFileSuffix)

      if ( iOutputFormat == OUTPUT_SURFER ) then
         call grid_WriteSurferGrid(TRIM(sOutputFilename), &
            rX0_cntr,rX1_cntr,rY0_cntr,rY1_cntr,pGrd%rData(:,:))
      else
         call grid_WriteArcGrid(TRIM(sOutputFilename), &
            rX0,rX1,rY0,rY1,pGrd%rData(:,:))
      end if

    end if


    write(sLabel,FMT="(i2.2,'/',i2.2'/',i4.4)") iCurrMM, iCurrDD, iCurrYYYY

    if(lBASINSTATS) call CalcBasinStats(pGrd, pConfig, sVarName, sLabel)
    if(lMASKSTATS) call CalcMaskStats(pGrd, pMaskGrd, pConfig, sVarName, sLabel)
    if(lMASKSSF) call CalcMaskStatsSSF(pGrd, pMaskGrd, pConfig, sVarName, iGridCellValue, &
       trim(sSiteNumber)//"   "//trim(sLabel)//"    23:59:59 ")

    if((STAT_INFO(iVariableNumber)%iDailyOutput==iGRAPH &
       .or. STAT_INFO(iVariableNumber)%iDailyOutput==iBOTH) &
       .and. lPRINT ) then

      write(sOutputFilename,FMT="(A,'_',i4.4,'_',i2.2,'_',i2.2,'.png')") &
        TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME), &
        iCurrYYYY,iCurrMM,iCurrDD

      sTitleTxt = TRIM(STAT_INFO(iVariableNumber)%sVARIABLE_NAME)// &
        '   '//sDateTxt

      call make_shaded_contour(pGrd, TRIM(sOutputFilename), TRIM(sTitleTxt), &
        TRIM(STAT_INFO(iVariableNumber)%sUNITS))

    end if

!------------------------- MONTHLY ANALYSIS
    if(lMonthEnd .and. lPRINT) then

      if(lBASINSTATS .and. STAT_INFO(iVariableNumber)%iMonthlyOutput == iSTATS) then

        write(sLabel,FMT="(i2.2,'/',i4.4)") iCurrMM, iCurrYYYY

        if(iSWBStatsType == iMEAN) then
          call CalcBasinStats(pMonthGrd, pConfig, sVarName, sLabel, iMonthCount)
        else
          call CalcBasinStats(pMonthGrd, pConfig, sVarName, sLabel)
        endif
      endif

      if(lMASKSTATS .and. STAT_INFO(iVariableNumber)%iMonthlyOutput == iSTATS) then

        write(sLabel,FMT="(i2.2,'/',i4.4)") iCurrMM, iCurrYYYY

        if(iSWBStatsType == iMEAN) then
          call CalcMaskStats(pGrd, pMaskGrd, pConfig, sVarName, sLabel, iMonthCount)
        else
          call CalcMaskStats(pGrd, pMaskGrd, pConfig, sVarName, sLabel)
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
               rX0_cntr,rX1_cntr,rY0_cntr,rY1_cntr,pGrd%rData(:,:))
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
                rX0_cntr,rX1_cntr,rY0_cntr,rY1_cntr, &
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

    if( lYearEnd .and. lPRINT ) then

      if(lBASINSTATS .and. STAT_INFO(iVariableNumber)%iMonthlyOutput == iSTATS) then
        write(sLabel,FMT="(i4.4)") iCurrYYYY
        if(iSWBStatsType == iMEAN) then
          call CalcBasinStats(pYearGrd, pConfig, sVarName, sLabel, iYearCount)
        else
          call CalcBasinStats(pYearGrd, pConfig, sVarName, sLabel)
        endif
      endif

      if(lMASKSTATS .and. STAT_INFO(iVariableNumber)%iMonthlyOutput == iSTATS) then
        write(sLabel,FMT="(i4.4)") iCurrYYYY
        if(iSWBStatsType == iMEAN) then
          call CalcMaskStats(pGrd, pMaskGrd, pConfig, sVarName, sLabel, iYearCount)
        else
          call CalcMaskStats(pGrd, pMaskGrd, pConfig, sVarName, sLabel)
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
               rX0_cntr,rX1_cntr,rY0_cntr,rY1_cntr,pYearGrd%rData(:,:))
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
                rX0_cntr,rX1_cntr,rY0_cntr,rY1_cntr, &
                pYearGrd%rData / REAL(iYearCount))
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

  if( iSWBStatsOutputType == iSTATS .and. lBASINSTATS) then
    if(iSWBStatsType == iMEAN) then
      call CalcBasinStats(pSummaryGrd, pConfig, sVarName, sLabel, iPeriodCount)
    else
      call CalcBasinStats(pSummaryGrd, pConfig, sVarName, sLabel)
    endif
  endif

  lRESET = lFALSE

  if( iSWBStatsOutputType == iSTATS .and. lMASKSTATS) then
    if(iSWBStatsType == iMEAN) then
      call CalcMaskStats(pSummaryGrd, pMaskGrd, pConfig, sVarName, sLabel, iPeriodCount)
    else
      call CalcMaskStats(pSummaryGrd, pMaskGrd, pConfig, sVarName, sLabel)
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

  if ( iOutputFormat == OUTPUT_SURFER .and. iPeriodCount > 0) then
    call grid_WriteSurferGrid(TRIM(sOutputFilename), &
        rX0_cntr,rX1_cntr,rY0_cntr,rY1_cntr, &
        pSummaryGrd%rData / REAL(iPeriodCount))
  else
    if(iPeriodCount > 0) then
      call grid_WriteArcGrid(TRIM(sOutputFilename), &
        rX0,rX1,rY0,rY1,pSummaryGrd%rData / REAL(iPeriodCount))
    endif
  end if

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
    pSummaryGrd%rData = pSummaryGrd%rData / REAL(iPeriodCount)
  endif

  call make_shaded_contour(pSummaryGrd, TRIM(sOutputFilename), TRIM(sTitleTxt), &
    TRIM(STAT_INFO(iVariableNumber)%sUNITS))


  close(unit=LU_LOG)

end program swbstats
