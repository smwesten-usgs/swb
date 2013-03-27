!> @file
!> @brief Contains a single module, @ref model, which keeps track of the model date and executes
!>  necessary process modules.

!> @brief Allocates memory to store intermediate and final calculation results,
!> keeps track of the model date, reads tabular climate data, and calls the
!> necessary process modules in turn.
module model

  use types
  use data_factory
  use swb_grid
  use stats
  use runoff_curve_number
  use et_thornthwaite_mather
  use et_turc
  use et_hargreaves
  use et_jensen_haise
  use et_blaney_criddle
  use sm_thornthwaite_mather
  use snow
  use irrigation

#ifdef _OPENMP
  use omp_lib
#endif

#ifdef NETCDF_SUPPORT
  use netcdf4_support
#endif

  implicit none

  !! Counter for moving average water inputs
  integer (kind=T_INT) :: iDayCtr

  !! Generic grids used to shuffle data between subroutines
  type ( T_GENERAL_GRID ),pointer :: pGenericGrd_int
  type ( T_GENERAL_GRID ),pointer :: pGenericGrd_sgl

  !! For the "downhill" solution
  integer (kind=T_INT) :: iOrderCount
  integer (kind=T_INT), dimension(:), allocatable :: iOrderCol
  integer (kind=T_INT), dimension(:), allocatable :: iOrderRow
  real(kind=T_SGL) :: rStartTime,rEndTime

contains


!--------------------------------------------------------------------------
!!****s* model/model_Solve
! NAME
!   model_Solve - Reads and initializes model grids and executes process
!                 subroutines.
!
! SYNOPSIS
!   Reads and initializes model grids, reads climate data file, and handles
!   calls each process subroutine on a daily basis.
!
! INPUTS
!   pGrd - Pointer to the model grid data structure.
!   pConfig - Pointer to the model configuration data structure.
!
! OUTPUTS
!   NONE
!
!!***

subroutine model_Solve( pGrd, pConfig, pGraph, pLandUseGrid)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd               ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig      ! pointer to data structure that contains
    ! model options, flags, and other settings

  type (T_GRAPH_CONFIGURATION), dimension(:), pointer :: pGraph
    ! pointer to data structure that holds parameters for creating
    ! DISLIN plots
  type ( T_GENERAL_GRID ), pointer :: pLandUseGrid       ! pointer to land use grid

  ! [ LOCALS ]
  integer (kind=T_INT) :: i, j, k, iStat, iDayOfYear, iMonth
  integer (kind=T_INT) :: tj, ti
  integer (kind=T_INT) :: iTempDay, iTempMonth, iTempYear
  integer (kind=T_INT) :: iPos
  integer (kind=T_INT) :: jj, ii, iNChange, iUpstreamCount, iPasses, iTempval
  integer (kind=T_INT) :: iCol, iRow
  character(len=3) :: sMonthName
  logical (kind=T_LOGICAL) :: lMonthEnd
  integer (kind=T_INT),save :: iNumGridCells

  real (kind=T_SGL) :: rmin,ravg,rmax

  type (T_CELL),pointer :: cel
  character (len=256) :: sBuf

  type (T_TIME_SERIES_FILE), pointer :: pTS

  ! allocate memory for the time-series data pointer
  ALLOCATE (pTS, STAT=iStat)
  call Assert( iStat == 0, &
    "Could not allocate memory for time-series data structure", &
    TRIM(__FILE__),__LINE__)

!  call stats_OpenBinaryFiles(pConfig)

  FIRST_YEAR: if(pConfig%lFirstYearOfSimulation) then

  pGenericGrd_int => grid_Create ( pGrd%iNX, pGrd%iNY, pGrd%rX0, pGrd%rY0, &
     pGrd%rX1, pGrd%rY1, DATATYPE_INT )

  pGenericGrd_sgl => grid_Create ( pGrd%iNX, pGrd%iNY, pGrd%rX0, pGrd%rY0, &
     pGrd%rX1, pGrd%rY1, DATATYPE_REAL )

  call stats_OpenBinaryFiles(pConfig, pGrd)

  call stats_InitializeVolumeConversion(pGrd)

#ifdef DEBUG_PRINT
  call grid_WriteArcGrid("SSF_Grid_Cells."//trim(pConfig%sOutputFileSuffix), &
    pGrd%rX0, pGrd%rX1,pGrd%rY0,pGrd%rY1,REAL(pGrd%Cells(:,:)%iNumFilesSSF) )
#endif

  ! open file into which daily summaries of variables will be written
  if ( pConfig%lReportDaily ) call stats_OpenMSBReport()

  ! open CSV file for daily stats summary
  if ( pConfig%lReportDaily ) then
    open(LU_CSV_MIN, file='SWB_daily_MINIMUM_values.csv',iostat=iStat,&
      status='REPLACE')
    open(LU_CSV_MEAN, file='SWB_daily_MEAN_values.csv',iostat=iStat,&
      status='REPLACE')
    open(LU_CSV_MAX, file='SWB_daily_MAXIMUM_values.csv',iostat=iStat,&
      status='REPLACE')

    call Assert(iStat == 0, &
      "Problem opening CSV files for summary statistics output.")

    call stats_WriteDailyAccumulatorHeaderCSV(LU_CSV_MIN,iMIN)
    call stats_WriteDailyAccumulatorHeaderCSV(LU_CSV_MEAN,iMEAN)
    call stats_WriteDailyAccumulatorHeaderCSV(LU_CSV_MAX,iMAX)
  end if

  ! open CSV file for annual stats summary
  open(LU_CSV_ANNUAL, file='SWB_annual_statistics.csv',iostat=iStat,&
    status='REPLACE')
  call Assert(iStat == 0, &
    "Problem opening CSV file for summary annual statistics output.")
  call stats_WriteAnnualAccumulatorHeaderCSV(LU_CSV_ANNUAL)

  iNumGridCells = pGrd%iNX * pGrd%iNY

  ! Time the run
  call cpu_time(rStartTime)

  call DAT(FLOWDIR_DATA)%getvalues( pGrdBase=pGrd)
  pGrd%Cells%iFlowDir = pGrd%iData

  pGenericGrd_int%iData = pGrd%Cells%iFlowDir
  call grid_WriteGrid(sFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Flow_Direction_Grid" // &
    "."//trim(pConfig%sOutputFileSuffix), pGrd=pGenericGrd_int, pConfig=pConfig )

  call make_shaded_contour(pGrd=pGenericGrd_int, &
     sOutputFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Flow_Direction_Grid.png", &
     sTitleTxt="D8 Flow Direction Grid", &
     sAxisTxt="Flow Direction" )

!  call model_PopulateFlowDirectionArray(pConfig, pGrd, pFlowDirGrid)

  ! Are we solving using the downhill algorithm?
  if ( pConfig%iConfigureRunoffMode == CONFIG_RUNOFF_DOWNHILL ) then
    ! if a routing table exists, read it in; else initialize and
    ! save the routing table for future use
    write(UNIT=LU_LOG,FMT=*)  "model.f95: model_ConfigureRunoffDownhill"
    call model_ConfigureRunoffDownhill( pGrd, pConfig)
  end if

  ! Unless we are *not* routing water, we *must* call InitializeFlowDirection
  if( pConfig%iConfigureRunoffMode /= CONFIG_RUNOFF_NO_ROUTING) then
    write(UNIT=LU_LOG,FMT=*)  "model.f95: model_InitializeFlowDirection"
    call model_InitializeFlowDirection( pGrd , pConfig)
  end if

!  call model_PopulateSoilGroupArray(pConfig, pGrd, pSoilGroupGrid)
  call DAT(SOILS_GROUP_DATA)%getvalues( pGrdBase=pGrd)
  pGrd%Cells%iSoilGroup = pGrd%iData
!  where (pGrd%Cells%iSoilGroup == 0) pGrd%Cells%iSoilGroup = 1

  pGenericGrd_int%iData = pGrd%Cells%iSoilGroup
  call grid_WriteGrid(sFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Hydrologic_Soils_Group" // &
    "."//trim(pConfig%sOutputFileSuffix), pGrd=pGenericGrd_int, pConfig=pConfig )

  call make_shaded_contour(pGrd=pGenericGrd_int, &
      sOutputFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Hydrologic_Soils_Group.png", &
      sTitleTxt="Hydrologic Soils Group", &
      sAxisTxt="HSG" )

  call DAT(AWC_DATA)%getvalues( pGrdBase=pGrd)
  pGrd%Cells%rSoilWaterCapInput = pGrd%rData

  write(LU_LOG, fmt="(a, f14.3)") "  Minimum AWC: ", minval(pGrd%Cells%rSoilWaterCapInput)
  write(LU_LOG, fmt="(a, f14.3)") "  Maximum AWC: ", maxval(pGrd%Cells%rSoilWaterCapInput)

  pGenericGrd_sgl%rData = pGrd%Cells%rSoilWaterCapInput
  call grid_WriteGrid(sFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Available_Water_Capacity" // &
    "."//trim(pConfig%sOutputFileSuffix), pGrd=pGenericGrd_sgl, pConfig=pConfig )

  call make_shaded_contour(pGrd=pGenericGrd_sgl, &
     sOutputFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Available_Water_Capacity.png", &
     sTitleTxt="Available Water Capacity", &
     sAxisTxt="AWC (inches per inch)" )

!  call model_PopulateAvailableWaterCapacityArray(pConfig, pGrd, pSoilAWCGrid)

  call DAT(LANDUSE_DATA)%getvalues( pGrdBase=pGrd )
  pGrd%Cells%iLandUse = pGrd%iData
  pGenericGrd_int%iData = pGrd%Cells%iLandUse
  call grid_WriteGrid(sFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Landuse_Landcover" // &
    "."//trim(pConfig%sOutputFileSuffix), pGrd=pGenericGrd_int, pConfig=pConfig )
  call make_shaded_contour(pGrd=pGenericGrd_int, &
     sOutputFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Flow_Landuse_Landcover.png", &
     sTitleTxt="Landuse / Landcover", &
     sAxisTxt="LULC Code" )


  ! Initialize the model landuse-related parameters
  call model_InitializeLanduseRelatedParams( pGrd, pConfig )

  ! Initialize evapotranspiration
  call model_InitializeET( pGrd, pConfig )

  end if FIRST_YEAR

  ! close any existing open time-series files...
  close(LU_TS)

  if(.not. pConfig%lGriddedData) then
  ! Connect to the single-site time-series file
    open ( LU_TS, file=pConfig%sTimeSeriesFilename, &
      status="OLD", iostat=iStat )
    write(UNIT=LU_LOG,FMT=*)  "Opening time series file: ", &
      TRIM(pConfig%sTimeSeriesFilename)
    flush(LU_LOG)
    call Assert ( iStat == 0, &
      "Can't open time-series data file" )
    pConfig%iCurrentJulianDay = pConfig%iCurrentJulianDay + 1
    call gregorian_date(pConfig%iCurrentJulianDay, &
      iTempYear, iTempMonth, iTempDay)
    pConfig%iYear = iTempYear
    pConfig%iMonth = iTempMonth
    pConfig%iDay = iTempDay
  end if

  ! Zero out monthly and annual accumulators
  call stats_InitializeMonthlyAccumulators()
  call stats_InitializeAnnualAccumulators()

  ! ***************************
  ! ***** BEGIN MAIN LOOP *****
  ! ***************************

  MAIN_LOOP: do

  ! new day: initialize stats accumulators
  call stats_InitializeDailyAccumulators()

  ! blow away any remnant climate values
  pTS%rPrecip = iNO_DATA_NCDC
  pTS%rRH = iNO_DATA_NCDC
  pTS%rMaxT = iNO_DATA_NCDC
  pTS%rMinT = iNO_DATA_NCDC
  pTS%rWindSpd = iNO_DATA_NCDC
  pTS%rMinRH = iNO_DATA_NCDC
  pTS%rSunPct = iNO_DATA_NCDC
  pTS%lEOF = lFALSE

  if(.not. pConfig%lGriddedData) then
    call model_ReadTimeSeriesFile(pTS)
    if(pTS%lEOF) then

#ifdef STRICT_DATE_CHECKING
      if(.not. (pConfig%iMonth == 12 .and. pConfig%iDay == 31)) then
        write(unit=LU_LOG,FMT=*) "Time series file ends prematurely:"
        write(unit=LU_LOG,FMT=*) "  file = "//TRIM(pConfig%sTimeSeriesFilename)
        write(unit=sBuf,FMT=*) "Time series file ends prematurely: " &
           //TRIM(pConfig%sTimeSeriesFilename)
        call Assert(lFALSE,TRIM(sBuf),TRIM(__FILE__),__LINE__)
      end if
#endif
      close(unit=LU_TS)
      exit MAIN_LOOP
    end if

    ! check to ensure that we have not skipped a day
    if(.not. (pConfig%iYear == pTS%iYear &
      .and. pConfig%iMonth == pTS%iMonth &
      .and. pConfig%iDay == pTS%iDay)) then
      write(unit=LU_LOG,FMT=*) "Missing or out-of-order data in time-series file:"
      write(unit=LU_STD_OUT,FMT=*) "Missing or out-of-order data in time-series file"
      write(unit=LU_LOG,FMT=*) "  date (TS file)= "//TRIM(int2char(pTS%iMonth))//"/" &
        //TRIM(int2char(pTS%iDay))//"/" &
        //TRIM(int2char(pTS%iYear))
      write(unit=LU_LOG,FMT=*) "  date (SWB)= "//TRIM(int2char(pConfig%iMonth))//"/" &
        //TRIM(int2char(pConfig%iDay))//"/" &
        //TRIM(int2char(pConfig%iYear))
#ifdef STRICT_DATE_CHECKING
      call Assert(lFALSE,"",TRIM(__FILE__),__LINE__)
#else
      ! reset date to that of the input time-series data
      pConfig%iYear = pTS%iYear
      pConfig%iMonth = pTS%iMonth
      pConfig%iDay = pTS%iDay
      pConfig%iCurrentJulianDay = julian_day ( pConfig%iYear, pConfig%iMonth, pConfig%iDay )
#endif
    end if

    pConfig%iNumDaysInYear = num_days_in_year(pConfig%iYear)

  end if

  call LookupMonth(pConfig%iMonth,pConfig%iDay,pConfig%iYear, &
    pConfig%iDayOfYear,sMonthName,lMonthEnd)

  ! initialize landuse-associated variables; must be done each year if
  ! dynamic landuse is being used

  call DAT(LANDUSE_DATA)%getvalues( pGrdBase=pGrd )

  if ( DAT(LANDUSE_DATA)%lGridHasChanged ) then
    pGrd%Cells%iLandUse = pGrd%iData
    DAT(LANDUSE_DATA)%lGridHasChanged = lFALSE

    call sm_thornthwaite_mather_UpdatePctSM( pGrd )

    ! (Re)-initialize the model
    call model_InitializeLanduseRelatedParams( pGrd, pConfig )

  endif

  if(pConfig%lFirstDayOfSimulation) then
    ! scan through list of potential output variables; if any
    ! output is desired for a variable, note the current position
    ! within the file, move to the position reserved for the first day's
    ! date, write the date, and return to the position where the data
    ! for the first day will be written
    do k=1,iNUM_VARIABLES
      if(STAT_INFO(k)%iDailyOutput > iNONE &
        .or. STAT_INFO(k)%iMonthlyOutput > iNONE &
        .or. STAT_INFO(k)%iAnnualOutput > iNONE)  then
        inquire(UNIT=STAT_INFO(k)%iLU,POS=iPos)  ! establish location to return to
        write(UNIT=STAT_INFO(k)%iLU,POS=iSTARTDATE_POS) &
          pConfig%iMonth,pConfig%iDay, pConfig%iYear
        write(UNIT=STAT_INFO(k)%iLU,POS=iPos ) ! return to prior location in bin file
      end if
      pConfig%lFirstDayOfSimulation = lFALSE
    end do

  end if

  if(pConfig%lWriteToScreen) then
    write(UNIT=LU_STD_OUT,FMT=*)
    if(pConfig%lANSI_Colors) then
      write(UNIT=LU_STD_OUT,FMT="(1x,a7,a80,a7)") sBOLDWHITE,REPEAT('=',80),sWHITE
    else
      write(UNIT=LU_STD_OUT,FMT="(1x,a80)") REPEAT('=',80)
    end if
    write(UNIT=LU_STD_OUT,FMT="(1x,'DAY: ',i3,4x,A3,4x,i2,'/',i2,'/',i4)") &
      pConfig%iDayOfYear,sMonthName,pConfig%iMonth,pConfig%iDay,pConfig%iYear
!      write(UNIT=LU_STD_OUT,FMT="(1x,a80)") REPEAT('-',80)
    write(UNIT=LU_STD_OUT,FMT=*)
  end if

  ! write timestamp to the unformatted fortran file(s)
  do k=1,iNUM_VARIABLES
    if(STAT_INFO(k)%iDailyOutput > iNONE &
      .or. STAT_INFO(k)%iMonthlyOutput > iNONE &
      .or. STAT_INFO(k)%iAnnualOutput > iNONE)  then
    write(UNIT=STAT_INFO(k)%iLU) pConfig%iDay,pConfig%iMonth, &
      pConfig%iYear, pConfig%iDayOfYear
!    inquire(UNIT=STAT_INFO(k)%iLU, POS=STAT_INFO(k)%iPos)
!    write(UNIT=STAT_INFO(k)%iLU) iNO_DATA_NCDC  ! dummy value for now
    end if
  end do

!***********************************************************************

!  ! Initialize precipitation value for current day
  call model_GetDailyPrecipValue(pGrd, pConfig, pTS%rPrecip, &
    pConfig%iMonth, pConfig%iDay, pConfig%iYear, pConfig%iCurrentJulianDay)

  ! Initialize temperature values for current day
  call model_GetDailyTemperatureValue(pGrd, pConfig, &
    pTS%rAvgT, pTS%rMinT, pTS%rMaxT, pTS%rRH, &
    pConfig%iMonth, pConfig%iDay, pConfig%iYear, pConfig%iCurrentJulianDay)

  write(UNIT=LU_LOG,FMT="(1x,'Beginning calculations for day: '," &
    //"i3,4x,A3,4x,i2,'/',i2,'/',i4)") &
    pConfig%iDayOfYear,sMonthName,pConfig%iMonth,pConfig%iDay,pConfig%iYear

  if(pConfig%lWriteToScreen) then
    write(UNIT=LU_STD_OUT,FMT="(t39,a,t53,a,t69,a)") "min","mean","max"
    call stats_WriteMinMeanMax(LU_STD_OUT,"Gross Precipitation (in)" , pGrd%Cells(:,:)%rGrossPrecip )
    call stats_WriteMinMeanMax(LU_STD_OUT,"Minimum Temp (F)" , pGrd%Cells(:,:)%rTMin )
    call stats_WriteMinMeanMax(LU_STD_OUT,"Mean Temp (F)" , pGrd%Cells(:,:)%rTAvg )
    call stats_WriteMinMeanMax(LU_STD_OUT,"Maximum Temp (F)" , pGrd%Cells(:,:)%rTMax )
!      write(UNIT=LU_STD_OUT,FMT="(1x,a80)") REPEAT('-',80)
  write(UNIT=LU_STD_OUT,FMT=*)
end if

  call model_UpdateContinuousFrozenGroundIndex( pGrd , pConfig)

#ifdef IRRIGATION_MODULE
  call model_UpdateGrowingDegreeDay( pGrd , pConfig)
#endif

  ! Handle all the processes in turn

  if(pConfig%iConfigureSnow == CONFIG_SNOW_ORIGINAL_SWB) then
    call model_ProcessRain(pGrd, pConfig, pConfig%iDayOfYear, pConfig%iMonth)
  else if(pConfig%iConfigureSnow == CONFIG_SNOW_NEW_SWB) then
    call model_ProcessRainPRMS(pGrd, pConfig, pConfig%iDayOfYear, &
      pConfig%iMonth, pConfig%iNumDaysInYear)
  else
    call Assert(lFALSE,"Unhandled snow module option specified", &
      TRIM(__FILE__),__LINE__)
  end if

#ifdef IRRIGATION_MODULE
  call update_irrigation_amounts(pGrd, pConfig)
#endif

  call model_ProcessRunoff(pGrd, pConfig, pConfig%iDayOfYear, pConfig%iMonth)

  call model_ProcessET( pGrd, pConfig, pConfig%iDayOfYear, &
    pConfig%iNumDaysInYear, pTS%rRH, pTS%rMinRH, &
    pTS%rWindSpd, pTS%rSunPct )

  call model_ProcessSM( pGrd, pConfig, pConfig%iDayOfYear, &
    pConfig%iDay ,pConfig%iMonth, pConfig%iYear)

  ! if desired, output daily mass balance file and daily model grids
  if(pConfig%lWriteToScreen) then
    call stats_DumpDailyAccumulatorValues(LU_STD_OUT, pConfig)
  else
    write(UNIT=LU_STD_OUT,FMT="(a,i2.2,a,i2.2,a,i4.4)") &
      "Simulation day: ",pConfig%iMonth,"/", pConfig%iDay, "/", pConfig%iYear
  end if

  ! if desired, output daily mass balance file and daily model grids
  if ( pConfig%lReportDaily ) then

    call stats_WriteDailyAccumulatorValuesCSV(LU_CSV_MIN,pConfig%iMonth, &
      pConfig%iDay,pConfig%iYear,iMIN)
    call stats_WriteDailyAccumulatorValuesCSV(LU_CSV_MEAN,pConfig%iMonth, &
      pConfig%iDay,pConfig%iYear,iMEAN)
    call stats_WriteDailyAccumulatorValuesCSV(LU_CSV_MAX,pConfig%iMonth, &
      pConfig%iDay,pConfig%iYear,iMAX)
    call stats_WriteMSBReport(pGrd,pConfig%iMonth,pConfig%iDay, &
      pConfig%iYear,pConfig%iDayOfYear)

  end if

  ! write OFFSET VALUE to the unformatted fortran file(s)
!  do k=1,iNUM_VARIABLES
!    if(STAT_INFO(k)%iDailyOutput > iNONE &
!      .or. STAT_INFO(k)%iMonthlyOutput > iNONE &
!      .or. STAT_INFO(k)%iAnnualOutput > iNONE)  then
!      ! get current file position
!      inquire(UNIT=STAT_INFO(k)%iLU, POS=iTempval)
!      ! rewind to location of today's header, at the location
!      ! where the offset is to be written
!      write(UNIT=STAT_INFO(k)%iLU, POS=STAT_INFO(k)%iPos)
!      ! write an offset amount at the end of the current header
!      write(UNIT=STAT_INFO(k)%iLU) iTempval - STAT_INFO(k)%iPos
!      ! return to last file position
!      write(UNIT=STAT_INFO(k)%iLU, POS=iTempval)
!    end if
!  end do

  call model_WriteGrids(pGrd=pGrd, pConfig=pConfig, iOutputType=WRITE_ASCII_GRID_DAILY)

  ! Write the results at each month-end
  if ( lMonthEnd ) then

!      if ( pConfig%lReportDaily ) call stats_CalcMonthlyMeans(pConfig%iMonth, pConfig%iDay)
!      call stats_WriteMonthlyReport (LU_STD_OUT, pGrd, sMonthName, iMonth)

    call model_WriteGrids(pGrd=pGrd, pConfig=pConfig, iOutputType=WRITE_ASCII_GRID_MONTHLY)

  if ( pConfig%lWriteToScreen) call stats_DumpMonthlyAccumulatorValues(LU_STD_OUT, &
    pConfig%iMonth, sMonthName, pConfig)

    write(UNIT=LU_LOG,FMT="(A,i2,A,i4)") &
      "finished monthly calculations for: ", &
        pConfig%iMonth, "/", pConfig%iYear
    flush(UNIT=LU_LOG)

  end if

  !-------------------------------------------------------------------------
  ! time control block follows; if next day is part of a new year, exit loop
  !-------------------------------------------------------------------------
  call gregorian_date(pConfig%iCurrentJulianDay + 1, &
    iTempYear, iTempMonth, iTempDay)

  if(pConfig%iYear /= iTempYear) then
    close(unit=LU_TS)
    exit MAIN_LOOP
  else
    pConfig%iMonth = iTempMonth
    pConfig%iDay = iTempDay
    pConfig%iCurrentJulianDay = pConfig%iCurrentJulianDay + 1
  end if

  end do MAIN_LOOP

  call model_WriteGrids(pGrd=pGrd, pConfig=pConfig, iOutputType=WRITE_ASCII_GRID_ANNUAL)

  ! model_Solve has been called once... any further calls will not require
  !    re-initialization of data structures and data arrays
  pConfig%lFirstYearOfSimulation = lFALSE

  if(pConfig%lWriteToScreen) &
    call stats_DumpAnnualAccumulatorValues(LU_STD_OUT, pConfig, pConfig%iYear)

  call stats_WriteAnnualAccumulatorValuesCSV(LU_CSV_ANNUAL,pConfig%iYear)

  ! update value of last year
  if( .not. pConfig%lGriddedData) pConfig%iEndYear = pConfig%iYear

  DEALLOCATE(pTS, STAT=iStat)
  call Assert( iStat == 0, &
    "Could not deallocate memory for time-series data structure")

end subroutine model_Solve

!!***

!--------------------------------------------------------------------------
!!****s* model/model_EndOfRun
! NAME
!   model_EndOfRun - Closes fortran files units and prints out elapsed runtime.
!
! SYNOPSIS
!   Closes fortran files units and prints out elapsed runtime.
!
! INPUTS
!   pConfig - Pointer to the model configuration data structure.
!
! OUTPUTS
!   NONE
!
! NOTES
!   Code refers to parameters that are set within types.f95.
!
! SOURCE

subroutine model_EndOfRun(pGrd, pConfig, pGraph)

  ![ARGUMENTS]
  type (T_GENERAL_GRID),pointer :: pGrd      ! Grid of model cells
  type (T_MODEL_CONFIGURATION), pointer :: pConfig  ! pointer to data structure that contains
    ! model options, flags, and other settings
  type (T_GRAPH_CONFIGURATION), dimension(:), pointer :: pGraph
    ! pointer to data structure that holds parameters for creating
    ! DISLIN plots

  ![LOCALS]
  integer (kind=T_INT) :: k

  ! finalize and close any open NetCDF or binary output files
  do k=1,iNUM_VARIABLES

    ! write the end date of the simulation into the header of
    ! the binary file (*.bin)
    if(STAT_INFO(k)%iDailyOutput > iNONE &
      .or. STAT_INFO(k)%iMonthlyOutput > iNONE &
      .or. STAT_INFO(k)%iAnnualOutput > iNONE)  then
      write(UNIT=STAT_INFO(k)%iLU,POS=iENDDATE_POS) &
        pConfig%iMonth,pConfig%iDay, pConfig%iYear
    end if

  end do

  ! clean up
  close ( unit=LU_TS )
  if ( pConfig%lReportDaily ) then
    close ( unit=LU_MSB_REPORT )
    close ( unit=LU_CSV_MIN )
    close ( unit=LU_CSV_MEAN )
    close ( unit=LU_CSV_MAX )
    close ( unit=LU_CSV_ANNUAL )
  end if

  ! close any binary output files
  call stats_CloseBinaryFiles()

  ! trigger the call to reconstitute the output grids and plots from the
  ! compressed binary files, if desired
  if(.not. pConfig%lUseSWBRead) &
    call stats_RewriteGrids(pGrd%iNX, pGrd%iNY, pGrd%rX0, pGrd%rY0, pGrd%rX1, &
      pGrd%rY1, pConfig, pGraph)

  ! destroy model grid to free up memory
  call grid_Destroy(pGrd)
  call grid_Destroy(pGenericGrd_int)
  call grid_Destroy(pGenericGrd_sgl)
!  call grid_Destroy(pGenericGrd_short)

  ! how long did all this take, anyway?
  call cpu_time(rEndTime)
  print "(//1x,'SWB run completed in: ',f10.2,' minutes')", &
    (rEndTime - rStartTime) / 60.0_T_SGL
  write(unit=LU_LOG,fmt="(//1x,'SWB run completed in: ',f10.2, ' minutes')"), &
    (rEndTime - rStartTime) / 60.0_T_SGL

  return
end subroutine model_EndOfRun

!------------------------------------------------------------------------------

function if_GetDynamicLanduseValue( pGrd, pConfig, iYear)  result(iStat)
  !! Populates annual dynamic landuse value on a cell-by-cell basis
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  integer (kind=T_INT), intent(in) :: iYear
  integer (kind=T_INT) :: iStat
  ! [ LOCALS ]
  type (T_GENERAL_GRID),pointer :: input_grd      ! Pointer to temporary grid for I/O
  character (len=256) sBuf
  logical (kind=T_LOGICAL) :: lExists

  ! set to a nonzero value
  iStat = iEOF

  input_grd=>null()

  write(unit=LU_LOG,fmt="('Reading dynamic landuse data from file: '," &
    //"A,'_',i4,'.',A)") trim(pConfig%sDynamicLanduseFilePrefix), &
    iYear,trim(pConfig%sOutputFileSuffix)

  select case( pConfig%iConfigureLanduse )
    case( CONFIG_LANDUSE_DYNAMIC_ARC_GRID )
      write ( unit=sBuf, fmt='(A,"_",i4,".",A)' ) &
        trim(pConfig%sDynamicLanduseFilePrefix), iYear,trim(pConfig%sOutputFileSuffix)
          ! check to see if an existing downhill flow routing table exists
      inquire( file=trim(sBuf), EXIST=lExists)
      if(lExists) then
        input_grd => grid_Read( sBuf, "ARC_GRID", DATATYPE_INT )

        call model_PopulateLandUseArray(pConfig, pGrd, input_grd)

        iStat = 0
      endif
    case( CONFIG_LANDUSE_DYNAMIC_SURFER )
      write ( unit=sBuf, fmt='(A,"_",i4,".",A)' ) &
        trim(pConfig%sDynamicLanduseFilePrefix), iYear,trim(pConfig%sOutputFileSuffix)
      inquire( file=trim(sBuf), EXIST=lExists)
      if(lExists) then
        input_grd => grid_Read( sBuf, "SURFER", DATATYPE_INT )

        call model_PopulateLandUseArray(pConfig, pGrd, input_grd)

        iStat = 0
      endif

  case default
    call Assert ( lFALSE, "Internal error -- unknown landuse input type" )
end select

end function if_GetDynamicLanduseValue


!--------------------------------------------------------------------------
!!****s* model/model_GetDailyPrecipValue
! NAME
!   model_GetDailyPrecipValue - Copies precipitation values into
!                               the cell-by-cell data structure.
!
! SYNOPSIS
!   Populates the gross precipitation value on a cell-by-cell basis.
!   If gridded precipitation data are used, this subroutine reads in the
!   daily precipitation grid and copies the values into the cell-by-cell
!   data structure.
!
! INPUTS
!   pGrd - Pointer to the model grid data structure.
!   pConfig - Pointer to the model configuration data structure.
!   rPrecip - Daily precipitation amount read in by model_Solve.
!   iDayOfYear - Day of the current year (January 1 = 1).
!   iMonth - Month corresponding to the current model day (January = 1).
!
! OUTPUTS
!   NONE
!
! NOTES
!   ** Code refers to parameters that are set within types.f95.
!   ** Code directly modifies model grid values through the manipulation
!       of pointers.
!
! SOURCE

subroutine model_GetDailyPrecipValue( pGrd, pConfig, rPrecip, iMonth, iDay, iYear, iJulianDay)
  !! Populates Gross precipitation value on a cell-by-cell basis
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  real (kind=T_SGL), intent(in) :: rPrecip
  integer (kind=T_INT), intent(in) :: iMonth
  integer (kind=T_INT), intent(in) :: iDay
  integer (kind=T_INT), intent(in) :: iYear
  integer (kind=T_INT), intent(in) :: iJulianDay
  ! [ LOCALS ]
  real (kind=T_DBL) :: rMin, rMean, rMax, rSum
  integer (kind=T_INT) :: iCount, iNegCount
  character (len=256) sBuf


  call DAT(PRECIP_DATA)%set_constant(rPrecip)

  call DAT(PRECIP_DATA)%getvalues( pGrdBase=pGrd, &
      iMonth=iMonth, iDay=iDay, iYear=iYear, &
      iJulianDay=iJulianDay)

  pGrd%Cells%rGrossPrecip = pGrd%rData

  iNegCount = COUNT(pGrd%Cells%rGrossPrecip < pConfig%rMinValidPrecip)

  ! convert values less than the minimum valid amount to zero
  where (pGrd%Cells%rGrossPrecip < pConfig%rMinValidPrecip)
    pGrd%Cells%rGrossPrecip = rZERO
  end where

  if(pConfig%lHaltIfMissingClimateData) then
    call Assert(rMin >= rZERO,"Precipitation values less than " &
      //real2char(pConfig%rMinValidPrecip)//" are not allowed. " &
      //"("//trim(int2char(iNegCount))//" cells with values < " &
      //real2char(pConfig%rMinValidPrecip)//")",TRIM(__FILE__),__LINE__)
  elseif(iNegCount > 0) then
    write(sBuf,fmt="(a,i7,1x,a,1x,i2.2,'/',i2.2,'/',i4.4)") "*** ",iNegCount, &
      "Missing PRECIPITATION values detected: ", iMonth, iDay, iYear
    call echolog(sBuf)
    call echolog("  ==> Missing precipitation values will be set to zero")
  endif

  rMin = minval(pGrd%Cells%rGrossPrecip)
  rMax = maxval(pGrd%Cells%rGrossPrecip)
  rSum = sum(pGrd%Cells%rGrossPrecip)
  iCount = size(pGrd%Cells%rGrossPrecip)

  ! We are ignoring any missing or bogus values in this calculation
  rMean = rSum / iCount

  call stats_UpdateAllAccumulatorsByGrid(rMin,rMean,rMax,rSum,iGROSS_PRECIP,iMonth)

end subroutine model_GetDailyPrecipValue

!!***

!--------------------------------------------------------------------------
!!****s* model/model_GetDailyTemperatureValue
! NAME
!   model_GetDailyTemperatureValue - Copies precipitation values into
!                               the cell-by-cell data structure.
!
! SYNOPSIS
!   Populates the minimum, maximum, and average temperature values
!   on a cell-by-cell basis.
!
!   If gridded temperature data are used, this subroutine reads in the
!   daily minimum and maximum temperature grid and copies the values
!   into the cell-by-cell data structure.
!
! INPUTS
!   pGrd - Pointer to the model grid data structure.
!   pConfig - Pointer to the model configuration data structure.
!   rAvgT - Average daily temperature in Fahrenheit.
!   rMinT - Minimum daily temperature in Fahrenheit.
!   rMaxT - Maximum daily temperature in Fahrenheit.
!   iMonth - Month corresponding to the current model day (January = 1).
!   iDay - Day of month
!   iYear - Year
!
! OUTPUTS
!   NONE
!
! NOTES
!   ** Code refers to parameters that are set within types.f95.
!   ** Code directly modifies model grid values through the manipulation
!       of pointers.
!
! SOURCE

subroutine model_GetDailyTemperatureValue( pGrd, pConfig, rAvgT, rMinT, &
  rMaxT, rRH, iMonth, iDay, iYear, iJulianDay)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  real (kind=T_SGL), intent(in) :: rAvgT
  real (kind=T_SGL), intent(in) :: rMinT
  real (kind=T_SGL), intent(in) :: rMaxT
  real (kind=T_SGL), intent(in) :: rRH
  integer (kind=T_INT), intent(in) :: iMonth
  integer (kind=T_INT), intent(in) :: iDay
  integer (kind=T_INT), intent(in) :: iYear
  integer (kind=T_INT), intent(in) :: iJulianDay

  ! [ LOCALS ]
  real (kind=T_DBL) :: rMin, rMean, rMax, rSum, rTFactor, rTempVal, rMeanTMIN, rMeanTMAX
  integer (kind=T_INT) :: iNumGridCells
  integer (kind=T_INT) :: iRow,iCol, iCount, iCount_valid
  character (len=256) sBuf
  type (T_CELL),pointer :: cel

  iCount = 0

  call DAT(TMAX_DATA)%set_constant(rMaxT)
  call DAT(TMIN_DATA)%set_constant(rMinT)

  call DAT(TMAX_DATA)%getvalues( pGrdBase=pGrd, &
      iMonth=iMonth, iDay=iDay, iYear=iYear, &
      iJulianDay=iJulianDay, rValues=pGrd%Cells%rTMax)

  call DAT(TMIN_DATA)%getvalues( pGrdBase=pGrd, &
      iMonth=iMonth, iDay=iDay, iYear=iYear, &
      iJulianDay=iJulianDay, rValues=pGrd%Cells%rTMin)


#ifdef STREAM_INTERACTIONS
  !! Adjust cell-by-cell temperature
  if ( pconfig%lElevAdjustment ) then
    do iRow=1, pGrd%iNX
      do iCol=1, pGrd%iNY
        cel => pGrd%Cells(iCol,iRow)
        if ( pConfig%rElevHumidityThreshold > 9990.0_T_SGL .or. rRH < 0.0_T_SGL ) then
          rTFactor = pConfig%rElevDryFactor
        else if ( rRH < pConfig%rElevHumidityThreshold ) then
          rTFactor = pConfig%rElevDryFactor
        else
          rTFactor = pConfig%rElevHumidFactor
        end if
        cel%rTMin = cel%rTMin - rTFactor * (cel%rElevation - pconfig%rElevStationElevation)
        cel%rTMax = cel%rTMax - rTFactor * (cel%rElevation - pconfig%rElevStationElevation)
        cel%rTAvg = cel%rTAvg - rTFactor * (cel%rElevation - pconfig%rElevStationElevation)
      end do
    end do
  end if
#endif

  if(pConfig%lHaltIfMissingClimateData) then

    call Assert(iCount == 0,"Temperature values less than " &
      //real2char(pConfig%rMinValidTemp)//" are not allowed. " &
      //"("//trim(int2char(iCount) )//" cells with values < " &
      //real2char(pConfig%rMinValidTemp)//")",TRIM(__FILE__),__LINE__)

  elseif(iCount > 0) then

    write(sBuf,fmt="(a,i7,1x,a,1x,i2.2,'/',i2.2,'/',i4.4)") "*** ",iCount, &
      "Missing minimum or maximum TEMPERATURE values detected: ", iMonth, iDay, iYear
    call echolog(sBuf)
    write(sBuf,fmt="(a,f12.3,a)") "  ==> Missing TMIN values will be " &
    //"replaced with the mean of the remaining non-missing values (", &
    rMeanTMIN,")"

    call echolog(sBuf)

    write(sBuf,fmt="(a,f12.3,a)") "  ==> Missing TMAX values will be " &
    //"replaced with the mean of the remaining non-missing values (", &
    rMeanTMAX,")"

    call echolog(sBuf)

  endif

  pGrd%Cells%rTAvg = (pGrd%Cells%rTMax + pGrd%Cells%rTMin) / 2_T_SGL

  ! Scan through array of inputs looking for instances where the TMIN > TMAX
  ! (THIS CAN BE RE_WRITTEN USING MATRIX NOTATION)

  !$OMP DO

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX
      cel=>pGrd%Cells(iCol,iRow)

      if( cel%rTMax < cel%rTMin )then

        ! swap min and max values to maintain a positive delta T
         rTempVal = cel%rTMax
        cel%rTMax = cel%rTMin
        cel%rTMin = cel%rTMax

      end if

    end do
  end do

  !$OMP END DO

end subroutine model_GetDailyTemperatureValue
!!***
!--------------------------------------------------------------------------
!!****s* model/model_UpdateContinuousFrozenGroundIndex( pGrd )
! NAME
!   model_UpdateContinuousFrozenGroundIndex - Updates the continuous
!                               frozen ground index on a cell-by-cell basis.
! SYNOPSIS
!   Updates the continuous frozen ground index (CFGI)
!   on a cell-by-cell basis.
!
! INPUTS
!   pGrd - Pointer to the model grid data structure.
!
! OUTPUTS
!   NONE
!
! NOTES
!   ** Code refers to parameters that are set within types.f95.
!   ** Code directly modifies model grid values through the manipulation
!       of pointers.
!
! SOURCE

subroutine model_UpdateContinuousFrozenGroundIndex( pGrd , pConfig)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings

  ! [ LOCALS ]
  real (kind=T_SGL) :: A = 0.97             ! decay coefficient
  integer (kind=T_INT) :: iCol,iRow               ! temporary array indices
  type (T_CELL),pointer :: cel              ! pointer to a particular cell
  real (kind=T_SGL) :: rTAvg_C              ! temporary variable holding avg temp in C
  real (kind=T_SGL) :: rSnowDepthCM         ! snow depth in centimeters

  !$OMP DO

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX
      cel => pGrd%Cells(iCol,iRow)
      rTAvg_C = FtoC(cel%rTAvg)
      ! assuming snow depth is 10 times the water content of the snow in inches
      rSnowDepthCM = cel%rSnowCover * 10.0_T_SGL * rCM_PER_INCH

  if(cel%rTAvg > rFREEZING) then
    cel%rCFGI = max(A*cel%rCFGI - &
      (rTAvg_C * exp (-0.4_T_SGL * 0.5_T_SGL * rSnowDepthCM)),rZERO)
  else ! temperature is below freezing
    cel%rCFGI = max(A*cel%rCFGI - &
      (rTAvg_C * exp (-0.4_T_SGL * 0.08_T_SGL * rSnowDepthCM)),rZERO)
  end if

  end do
end do

  !$OMP END DO

!  write(UNIT=LU_LOG,FMT=*)  "=========CFGI CALCULATION==========="
!  write(UNIT=LU_STD_OUT,FMT="(A)") &
!      "                                 min          mean           max"
!  call stats_WriteMinMeanMax(LU_STD_OUT,"CFGI" , pGrd%Cells(:,:)%rCFGI )
!
!  write(UNIT=LU_LOG,FMT=*)  "=========CFGI CALCULATION==========="



end subroutine model_UpdateContinuousFrozenGroundIndex


!--------------------------------------------------------------------------
!!****s* model/model_UpdateGrowingDegreeDay( pGrd )
! NAME
!   model_UpdateGrowingDegreeDay - Updates the growing degree-day
!                                  on a cell-by-cell basis.
! SYNOPSIS
!   Updates the growing degree-day
!   on a cell-by-cell basis.
!
! INPUTS
!   pGrd - Pointer to the model grid data structure.
!
! OUTPUTS
!   NONE
!
! NOTES
!   ** Code refers to parameters that are set within types.f95.
!   ** Code directly modifies model grid values through the manipulation
!       of pointers.
!
! SOURCE

#ifdef IRRIGATION_MODULE

subroutine model_UpdateGrowingDegreeDay( pGrd , pConfig)

  implicit none

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  ! [ LOCALS ]
  real (kind=T_SGL) :: rDD                        ! daily departure from TBase
  type (T_CELL),pointer :: cel                      ! pointer to a particular cell
  real (kind=T_SGL) :: rA, rAt
  real (kind=T_SGL) :: rTMax
  real (kind=T_SGL) :: rW
  integer (kind=T_INT) :: iCol,iRow

  ! zero out growing degree day at start of calendar year
  if(pConfig%iDayOfYear == 1) pGrd%Cells%rGDD = 0.

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX  ! last subscript in a Fortran array should be the slowest-changing
      cel => pGrd%Cells(iCol,iRow)

  ! cap the maximum value used in GDD calculations on the basis of the value
  ! provided by user...
  rTMax = min(cel%rGDD_TMax, cel%rTMax)

  if(rTMax <= cel%rGDD_TBase) then

  rDD = 0.

  elseif(cel%rTMin >= cel%rGDD_TBase) then

  rDD = cel%rTAvg - cel%rGDD_TBase

  else

  rW = (rTMax - cel%rTMin) / 2.

  rAt = ( cel%rGDD_TBase - cel%rTAvg) / rW

  if(rAt > 1) rAt = 1.
  if(rAt < -1) rAt = -1.

  rA = asin(rAt)

  rDD = (( rW * cos(rA)) - ((cel%rGDD_TBase - cel%rTAvg) &
    * ((dpPI / 2.) - rA))) / dpPI

  end if

  cel%rGDD = cel%rGDD + rDD

  end do

  end do

end subroutine model_UpdateGrowingDegreeDay

!--------------------------------------------------------------------------
!!****s* model/model_InitializeGrowingDegreeDay( pGrd )
! NAME
!   model_InitializeGrowingDegreeDay - Initializes the growing degree-day
!                                  on a cell-by-cell basis.
! SYNOPSIS
!   Initializes the growing degree-day
!   on a cell-by-cell basis.
!
! INPUTS
!   pGrd - Pointer to the model grid data structure.
!
! OUTPUTS
!   NONE
!
! NOTES
!   ** Code refers to parameters that are set within types.f95.
!   ** Code directly modifies model grid values through the manipulation
!       of pointers.
!
! SOURCE

subroutine model_InitializeGrowingDegreeDay( pGrd , pConfig)

  implicit none

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  ! [ LOCALS ]
  real (kind=T_SGL) :: rDD                        ! daily departure from TBase
  type (T_CELL),pointer :: cel                      ! pointer to a particular cell
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  real (kind=T_SGL) :: rA, rAt
  real (kind=T_SGL) :: rTMax
  real (kind=T_SGL) :: rW
  integer (kind=T_INT) :: iCol,iRow

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX  ! last subscript in a Fortran array should be the slowest-changing
      cel => pGrd%Cells(iCol,iRow)
      pIRRIGATION => pConfig%IRRIGATION(cel%iLandUseIndex)

  ! transfer table values to the grid cell
  cel%rGDD_TMax = pIRRIGATION%rGDD_MaxTemp
  cel%rGDD_TBase = pIRRIGATION%rGDD_BaseTemp

  end do

  end do

end subroutine model_InitializeGrowingDegreeDay

#endif

!!***
!--------------------------------------------------------------------------
!!****s* model/model_ProcessRain
! NAME
!   model_ProcessRain - Processes the daily rainfall. Upon return, the
!                       daily precipitation is corrected to account for
!                       snow accumulation / snowmelt.
!
! SYNOPSIS
!   Processes daily precipitation values. Daily precipitation values
!   are altered to account for interception and with regard to the
!   form that the precipitation takes (rain or snow). If the form of the
!   precipitation is snow, the net precipitation value is zeroed out, and
!   the precipitation value is moved into the snowfall value.
!
! INPUTS
!   pGrd - Pointer to the model grid data structure.
!   pConfig - Pointer to the model configuration data structure.
!   iDayOfYear - Day of the current year (January 1 = 1).
!   iMonth - Month corresponding to the current model day (January = 1).
!
! OUTPUTS
!   NONE
!
! NOTES
!   ** Code refers to parameters that are set within types.f95.
!   ** Code directly modifies model grid values through the manipulation
!       of pointers.
!
! SOURCE

subroutine model_ProcessRain( pGrd, pConfig, iDayOfYear, iMonth)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  integer (kind=T_INT),intent(in) :: iDayOfYear  ! Day of the year
  integer (kind=T_INT), intent(in) :: iMonth     ! Integer month value (1-12)
  ! [ LOCALS ]
  real (kind=T_DBL) :: dpPotentialMelt,dpPotentialInterception,dpInterception
  real (kind=T_DBL) :: dpPreviousSnowCover,dpChgInSnowCover, dpSnowCover
  real (kind=T_DBL) :: dpNetPrecip, dpNetRainfall
  integer (kind=T_INT) :: iRow,iCol
  type (T_CELL),pointer :: cel
  integer (kind=T_INT) :: iNumGridCells
  real (kind=T_DBL) :: rMin, rMean, rMax, rSum, rSum2
  integer (kind=T_INT) :: iRowCount
  real (kind=T_SGL) ::  rMonthlySnowRunoff
  logical (kind=T_LOGICAL) :: lFREEZING

  ! [ LOCAL PARAMETERS ]
  real (kind=T_SGL), parameter :: rMELT_INDEX = 1.5_T_SGL

  ! set snowmelt to zero uniformly across model grid
  pGrd%Cells(:,:)%rSnowMelt = rZERO

  ! set snowfall to zero uniformly across model grid
  pGrd%Cells(:,:)%rSnowFall_SWE = rZERO

  ! calculate number of cells in model grid
  iNumGridCells = pGrd%iNX * pGrd%iNY

!  pGrd%Cells(:,:)%rPrevious_SnowCover = pGrd%Cells(:,:)%rSnowCover

  ! Use "potential interception" for each cell to compute net precip
  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX
      cel => pGrd%Cells(iCol,iRow)

    ! allow for correction factor to be applied to precip gage input data
    if ( cel%rTAvg - (cel%rTMax-cel%rTMin)/3.0_T_SGL <= rFREEZING ) then
      lFREEZING = lTRUE
      cel%rGrossPrecip = cel%rGrossPrecip * pConfig%rSnowFall_SWE_Corr_Factor
    else
      lFREEZING = lFALSE
      cel%rGrossPrecip = cel%rGrossPrecip * pConfig%rRainfall_Corr_Factor
    end if

    dpPotentialInterception = rf_model_GetInterception(pConfig,cel%iLandUse,iDayOfYear)

    dpPreviousSnowCover = real(cel%rSnowCover, kind=T_DBL)
    dpSnowCover = real(cel%rSnowCover, kind=T_DBL)

    dpNetPrecip = real(cel%rGrossPrecip, kind=T_DBL) - dpPotentialInterception
!      cel%dpNetPrecip = cel%rGrossPrecip-dpPotentialInterception
!      if ( cel%dpNetPrecip < rZERO ) cel%dpNetPrecip = rZERO
    if ( dpNetPrecip < dpZERO ) dpNetPrecip = dpZERO
!      dpInterception = cel%rGrossPrecip - cel%dpNetPrecip
    dpInterception = real(cel%rGrossPrecip, kind=T_DBL) - dpNetPrecip

    if(dpInterception < dpZERO) &
      call Assert(lFALSE, &
        "Negative value for interception was calculated on day " &
        //int2char(iDayOfYear)//" iRow: "//trim(int2char(iRow)) &
        //"  iCol: "//trim(int2char(iCol)), &
        trim(__FILE__), __LINE__)

    call stats_UpdateAllAccumulatorsByCell(dpInterception, &
      iINTERCEPTION,iMonth,iZERO)

    if(STAT_INFO(iINTERCEPTION)%iDailyOutput > iNONE &
      .or. STAT_INFO(iINTERCEPTION)%iMonthlyOutput > iNONE &
      .or. STAT_INFO(iINTERCEPTION)%iAnnualOutput > iNONE)  then
        call RLE_writeByte(STAT_INFO(iINTERCEPTION)%iLU, &
          real(dpInterception, kind=T_SGL), pConfig%iRLE_MULT, &
          pConfig%rRLE_OFFSET, iNumGridCells, iINTERCEPTION)
    end if

!      cel%rAnnualInterception = cel%rAnnualInterception + cel%dpInterception
!      rMonthlyInterception = rMonthlyInterception + cel%dpInterception

  ! NET PRECIP = GROSS PRECIP - INTERCEPTION
    dpNetRainfall = dpNetPrecip

  ! Is it snowing?
    if (lFREEZING ) then
!        cel%rSnowCover = cel%rSnowCover + cel%dpNetPrecip
    dpSnowCover = dpSnowCover + dpNetPrecip
!         rMonthlySnowFall = rMonthlySnowFall + sum(pGrd%Cells(:,:)%dpNetPrecip)
    cel%rSnowFall_SWE = dpNetPrecip
    dpNetRainfall = dpZERO      ! For now -- if there is snowmelt, we do it next
  end if

  ! Is there any melting?
  if(cel%rTAvg>rFREEZING) then
    dpPotentialMelt = rMELT_INDEX * ( cel%rTMax-rFREEZING )*dpC_PER_F / rMM_PER_INCH

  if(dpSnowCover > dpPotentialMelt) then
    cel%rSnowMelt = dpPotentialMelt
    dpSnowCover = dpSnowCover - dpPotentialMelt
  else
    cel%rSnowMelt = dpSnowCover
    dpSnowCover = dpZERO
  end if

  end if

  dpChgInSnowCover = dpSnowCover - dpPreviousSnowCover

  call stats_UpdateAllAccumulatorsByCell( &
    REAL(dpChgInSnowCover,kind=T_DBL), iCHG_IN_SNOW_COV,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell( &
    dpNetRainfall,iNET_PRECIP,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell( &
    REAL(cel%rSnowMelt,kind=T_DBL),iSNOWMELT,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell( &
    REAL(cel%rSnowFall_SWE,kind=T_DBL),iSNOWFALL_SWE,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell( &
    dpSnowCover,iSNOWCOVER,iMonth,iZERO)

  ! copy temporary double-precision values back to single-precision
  cel%rSnowCover = real(dpSnowCover, kind=T_SGL)
  cel%rNetPrecip = real(dpNetRainfall, kind=T_SGL)

  end do
end do

  ! a call to the UpdateAllAccumulatorsByCell subroutine with a value of "iNumGridCalls"
  ! as the final argument triggers the routine to update monthly and annual stats
  call stats_UpdateAllAccumulatorsByCell(dpZERO,iCHG_IN_SNOW_COV,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO,iINTERCEPTION,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO,iNET_PRECIP,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO,iSNOWMELT,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO,iSNOWFALL_SWE,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO,iSNOWCOVER,iMonth,iNumGridCells)

end subroutine model_ProcessRain

!----------------------------------------------------------------------

subroutine model_ProcessRainPRMS( pGrd, pConfig, iDayOfYear, iMonth, iNumDaysInYear)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  integer (kind=T_INT),intent(in) :: iDayOfYear  ! Day of the year
  integer (kind=T_INT), intent(in) :: iMonth     ! Integer month value (1-12)
  integer (kind=T_INT), intent(in) :: iNumDaysInYear

  ! [ LOCALS ]
  real (kind=T_DBL) :: rPotentialMelt,rPotentialInterception,rInterception
  real (kind=T_SGL) :: rPreviousSnowCover,rChgInSnowCover
  integer (kind=T_INT) :: iCol,iRow
  type (T_CELL),pointer :: cel
  integer (kind=T_INT) :: iNumGridCells
  real (kind=T_DBL) :: rMin, rMean, rMax, rSum, rSum2
  integer (kind=T_INT) :: iCount
  real (kind=T_SGL) ::  rMonthlySnowRunoff
  real (kind=T_SGL) :: rFracRain
  real (kind=T_SGL) :: rTd, rTempDifference
  real (kind=T_SGL) :: rDelta,rOmega_s,rD_r, rRa, rRs, rRn_mean, rN
  real (kind=T_SGL) :: rRso, rRns, rRnl, rRn, rZenithAngle
  real (kind=T_SGL) :: rLatitude
  real (kind=T_SGL) :: rTempComp, rRadComp
  logical (kind=T_LOGICAL), parameter :: lENERGY_BALANCE = lFALSE

  ! [ LOCAL PARAMETERS ]
  ! from eqn 2, Kustas and Rango, 1994
  ! value of 0.2 for A_sub_r from Brubaker et al 1996 (Snowmelt Runoff Model)
  real (kind=T_SGL), parameter :: rA_sub_r = 0.2_T_SGL   ! cm per degree C
  real (kind=T_SGL), parameter :: rM_sub_Q = 0.026_T_SGL  ! cm/day per W/m**2

  ! Bastardized values below....
!  real (kind=T_SGL), parameter :: rA_sub_r = 0.27_T_SGL   ! cm per degree C
!  real (kind=T_SGL), parameter :: rM_sub_Q = 0.0040_T_SGL  ! cm/day per W/m**2

  real (kind=T_SGL), parameter :: rAlbedoInit = 0.965   ! Kustas et al
  real (kind=T_SGL), parameter :: rElevation = 1500
  real (kind=T_SGL), parameter :: rMeltInitTemperature = 31.5_T_SGL

  ! set snowmelt to zero uniformly across model grid
!  pGrd%Cells(:,:)%rSnowMelt = rZERO

  ! set snowfall to zero uniformly across model grid
! pGrd%Cells(:,:)%rSnowFall_SWE = rZERO

  ! calculate number of cells in model grid
  iNumGridCells = pGrd%iNX * pGrd%iNY

!  pGrd%Cells(:,:)%rPrevious_SnowCover = pGrd%Cells(:,:)%rSnowCover

  rD_r =rel_Earth_Sun_dist(iDayOfYear,iNumDaysInYear)
  rDelta = solar_declination(iDayOfYear, iNumDaysInYear)

  do iRow=1,pGrd%iNY

  rLatitude = row_latitude(pConfig%rNorthernLatitude, &
    pConfig%rSouthernLatitude, pGrd%iNY, iRow)
  rOmega_s = sunset_angle(rLatitude, rDelta)
  rN = daylight_hours(rOmega_s)

  ! NOTE that the following equation returns extraterrestrial radiation in
  ! MJ / m**2 / day.
  rRa = extraterrestrial_radiation_Ra(rLatitude,rDelta,rOmega_s,rD_r)
  rRso = clear_sky_solar_radiation_Rso(rRa)
  rZenithAngle = zenith_angle(rLatitude, rDelta)

  !$OMP DO

  ! determine the fraction of precip that falls as snow
  do iCol=1,pGrd%iNX

  cel => pGrd%Cells(iCol,iRow)

  ! initialize accumulators for this cell
  cel%rSnowMelt = rZERO
  cel%rSnowFall_SWE = rZERO
  cel%rSnowFall = rZERO

  if(cel%rTMin > pConfig%rTMaxAllSnow &
    .or. cel%rTMax > pConfig%rTMaxAllRain) then

  rFracRain = rONE
  cel%iDaysSinceLastSnow = cel%iDaysSinceLastSnow + 1

  else if(cel%rTMax < pConfig%rTMaxAllSnow) then

  rFracRain = rZERO
  cel%iDaysSinceLastSnow = 0

  else

  ! this is straight from MMS/PRMS
  rFracRain = ((cel%rTMax - pConfig%rTMaxAllSnow) &
    / (cel%rTMax - cel%rTMin))
  cel%iDaysSinceLastSnow = 0

  end if

  cel%rGrossPrecip = rFracRain * cel%rGrossPrecip * pConfig%rRainfall_Corr_Factor &
    + (rONE - rFracRain) * cel%rGrossPrecip * pConfig%rSnowFall_SWE_Corr_Factor

  rPotentialInterception = rf_model_GetInterception(pConfig,cel%iLandUse,iDayOfYear)

  rPreviousSnowCover = cel%rSnowCover

  cel%rNetPrecip = MAX(cel%rGrossPrecip-rPotentialInterception,rZERO)
  rInterception = MAX(cel%rGrossPrecip - cel%rNetPrecip,rZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(rInterception,kind=T_DBL), &
    iINTERCEPTION,iMonth,iZERO)

  if(STAT_INFO(iINTERCEPTION)%iDailyOutput > iNONE &
    .or. STAT_INFO(iINTERCEPTION)%iMonthlyOutput > iNONE &
    .or. STAT_INFO(iINTERCEPTION)%iAnnualOutput > iNONE)  then
      call RLE_writeByte(STAT_INFO(iINTERCEPTION)%iLU, &
        REAL(rInterception,kind=T_SGL), pConfig%iRLE_MULT, pConfig%rRLE_OFFSET, &
        iNumGridCells, iINTERCEPTION)
  end if

!      cel%rAnnualInterception = cel%rAnnualInterception + cel%rInterception
!      rMonthlyInterception = rMonthlyInterception + cel%rInterception

  cel%rSnowFall_SWE = cel%rNetPrecip * (rONE - rFracRain)
  cel%rSnowFall = cel%rSnowFall_SWE * snow_depth_Hedstrom(cel%rTAvg, pConfig)
  cel%rSnowCover = cel%rSnowCover + cel%rSnowFall_SWE
  cel%rNetPrecip = cel%rNetPrecip - cel%rSnowFall_SWE

  if(cel%rSnowCover > rNEAR_ZERO) then  ! no point in calculating all this
    ! unless there is snowcover present

  rRs = solar_radiation_Hargreaves_Rs(rRa, cel%rTMin, cel%rTMax) ! &

  cel%rSnowAlbedo = snow_albedo(rAlbedoInit, cel%iDaysSinceLastSnow, &
    rZenithAngle)

  if(lENERGY_BALANCE) then

  call snow_energy_balance(cel%rTMin, cel%rTMax, &
    cel%rTAvg, rRs, rRso, cel%rSnowAlbedo, cel%rSnowCover, &
    cel%rNetPrecip, cel%rSnowTemperature, cel%rSnowMelt, iCol,iRow)

  else

  ! amount average temperature exceeds freezing point
  rTempDifference = FtoC(cel%rTAvg) - FtoC(rMeltInitTemperature)
  rTd = max(rTempDifference,rZERO)

  rRns = net_shortwave_radiation_Rns(rRs, cel%rSnowAlbedo)

  rRnl = net_longwave_radiation_Rnl(cel%rTMin, cel%rTMax, rRs, rRso)

  rRn = rRns - rRnl

  rTempComp = rA_sub_r * rTd / rCM_PER_INCH

  rRadComp = max(rM_sub_Q * rRn * 11.57  / rCM_PER_INCH,rZERO)

  rPotentialMelt = rTempComp + rRadComp
#ifdef DEBUG_PRINT
  if(iCol > 3 .and. iCol < 5 .and. iRow > 20 .and. iRow < 22) then
    write(*,FMT="('Snow albedo:',t32,F14.3)") cel%rSnowAlbedo
    write(*,FMT="('Extraterrestrial radiation (Ra):',t32,F14.3)") rRa
    write(*,FMT="('Incoming shortwave (Rs):',t32,F14.3)") rRs
    write(*,FMT="('Clear sky shortwave (Rso):',t32,F14.3)") rRso
    write(*,FMT="('Zenith angle :',t32,F14.3)") rZenithAngle
    write(*,FMT="('Net shortwave (Rns):',t32,F14.3)") rRns
    write(*,FMT="('Net longwave (Rnl):',t32,F14.3)") rRnl
    write(*,FMT="('Net shortwave + longwave (Rn):',t32,F14.3)") rRn
    write(*,FMT="('Amount temp > 0 (rTd):',t32,F14.3)") rTd
    write(*,FMT="('Average temp  (rTAvg):',t32,F14.3)") cel%rTAvg
    write(*,FMT="('Temp Difference:',t32,F14.3)") rTempDifference
    write(*,FMT="('Snowcover (SWE, inches):',t32,F14.3)") cel%rSnowCover
    write(*,FMT="('Potential snowmelt (temp):',t32,F14.3)") rTempComp
    write(*,FMT="('Potential snowmelt (rad):',t32,F14.3)") rRadComp
    write(*,FMT="('Potential snowmelt:',t32,F14.3)") rPotentialMelt
    write(*,FMT="('----------------------------------------------------')")
  end if
#endif

  if(cel%rSnowCover > rPotentialMelt) then
    cel%rSnowMelt = rPotentialMelt
    cel%rSnowCover = cel%rSnowCover - rPotentialMelt
  else
    cel%rSnowMelt = cel%rSnowCover
    cel%rSnowCover = rZERO
  end if
end if
end if

  rChgInSnowCover = cel%rSnowCover - rPreviousSnowCover

  call stats_UpdateAllAccumulatorsByCell( &
    REAL(rChgInSnowCover,kind=T_DBL), iCHG_IN_SNOW_COV,iMonth,iZERO)
  call stats_UpdateAllAccumulatorsByCell( &
    REAL(cel%rNetPrecip,kind=T_DBL),iNET_PRECIP,iMonth,iZERO)
  call stats_UpdateAllAccumulatorsByCell( &
    REAL(cel%rSnowMelt,kind=T_DBL),iSNOWMELT,iMonth,iZERO)
  call stats_UpdateAllAccumulatorsByCell( &
    REAL(cel%rSnowFall_SWE,kind=T_DBL),iSNOWFALL_SWE,iMonth,iZERO)
  call stats_UpdateAllAccumulatorsByCell( &
    REAL(cel%rSnowCover,kind=T_DBL),iSNOWCOVER,iMonth,iZERO)


  end do

  !$OMP END DO

  end do

  ! a call to the UpdateAllAccumulatorsByCell subroutine with a value of "iNumGridCalls"
  ! as the final argument triggers the routine to update monthly and annual stats
  call stats_UpdateAllAccumulatorsByCell(dpZERO,iCHG_IN_SNOW_COV,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO,iINTERCEPTION,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO,iNET_PRECIP,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO,iSNOWMELT,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO,iSNOWFALL_SWE,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO,iSNOWCOVER,iMonth,iNumGridCells)

  return

end subroutine model_ProcessRainPRMS

!!***

!--------------------------------------------------------------------------
!!****s* model/model_ProcessRunoff
! NAME
!   model_ProcessRunoff - Calls method-specific subroutines to handle
!                         surface runoff calculation.
!
! SYNOPSIS
!   This subroutine calls the appropriate subroutine for calculating
!   surface runoff based on whether the user has selected the iterative
!   or downhill solution.
!
! INPUTS
!   pGrd - Pointer to the model grid data structure.
!   pConfig - Pointer to the model configuration data structure.
!   iDayOfYear - Day of the current year (January 1 = 1).
!   iMonth - Month corresponding to the current model day (January = 1).
!
! OUTPUTS
!   NONE
!
! NOTES
!   ** Code refers to parameters that are set within types.f95.
!   ** Code directly modifies model grid values through the manipulation
!       of pointers.
!
! SOURCE

subroutine model_ProcessRunoff(pGrd, pConfig, iDayOfYear, iMonth)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd          ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  integer (kind=T_INT),intent(in) :: iDayOfYear    ! day of current year (January 1 = 1)
  ! [ LOCALS ]
  integer (kind=T_INT) :: iCount
  integer (kind=T_INT) :: j, i
  real (kind=T_DBL) :: xmin, xmax, ymin, ymax
  integer (kind=T_INT), intent(in) :: iMonth     ! Integer month value (1-12)
  integer (kind=T_INT) :: iNumGridCells

  ! calculate number of cells in model grid
  iNumGridCells = pGrd%iNX * pGrd%iNY

  ! Iteratively processes the runoff event; first initialize the upstream flows
  pGrd%Cells(:,:)%rInFlow = rZERO
  pGrd%Cells(:,:)%rOutFlow = rZERO
  pGrd%Cells(:,:)%rFlowOutOfGrid = rZERO

  if ( pConfig%iConfigureRunoffMode == CONFIG_RUNOFF_ITERATIVE ) then
    do
      iCount = if_model_RunoffIteration( pGrd, pConfig, iDayOfYear, iMonth )
      if ( iCount == 0 ) then
        exit
      endif
    end do

  else if (pConfig%iConfigureRunoffMode == CONFIG_RUNOFF_DOWNHILL ) then
    call model_RunoffDownhill( pGrd, pConfig, iDayOfYear, iMonth )

  else if (pConfig%iConfigureRunoffMode == CONFIG_RUNOFF_NO_ROUTING ) then
    call model_Runoff_NoRouting( pGrd, pConfig, iDayOfYear, iMonth )

  else
    call Assert(lFALSE,"Internal error selecting a runoff routing module" &
      //" runoff mode = "//TRIM(int2char(pConfig%iConfigureRunoffMode)), &
      TRIM(__FILE__),__LINE__)
  end if

  ! Update the moving average counter
  iDayCtr = iDayCtr + 1
  if ( iDayCtr > iMOVING_AVG_TERMS ) iDayCtr = 1

  ! Update the inflow buffer (used to determine antecedent runoff conditions)
  pGrd%Cells(:,:)%rNetInflowBuf(iDayCtr) = pGrd%Cells(:,:)%rNetPrecip &
    + pGrd%Cells(:,:)%rSnowMelt + pGrd%Cells(:,:)%rInflow

  return
end subroutine model_ProcessRunoff

!!***

!--------------------------------------------------------------------------
!!****s* model/model_ConfigureRunoffDownhill
! NAME
!   model_ConfigureRunoffDownhill - Establishes sorted list of grid cells
!                                   (upstream-to-downstream) for use in the
!                                   downhill solution method.
!
! SYNOPSIS
!   This subroutine is only called if the user selects the downhill surface
!   runoff solution method. This routine systematically combs the model domain
!   looking for cells which either receive no runoff, or receive runoff from
!   cells that are already in the downhill runoff routing table.
!
!   The routing table is saved in a file named 'swb_routing.bin', which
!   is an unformatted Fortran data file.  Once this table exists, the
!   code will take routing information from this table first before
!   running through the process of determining a routing table from scratch.
!
! INPUTS
!   pGrd - Pointer to the model grid data structure.
!   pConfig - Pointer to the model configuration data structure.
!
! OUTPUTS
!   NONE
!
! NOTES
!   ** Code refers to parameters that are set within types.f95.
!   ** Code directly modifies model grid values through the manipulation
!       of pointers.
!
! SOURCE

subroutine model_ConfigureRunoffDownhill( pGrd, pConfig)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd          ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  ! [ LOCALS ]
  integer (kind=T_INT) :: iCol, iRow, iStat, tj, ti, iTgt_Row, iTgt_Col,k,iCumlCount,iCount
  integer (kind=T_INT) :: iRowSub, iColSub, iNChange, iUpstreamCount, iPasses
  integer (kind=T_INT) :: ic
  integer (kind=T_INT) :: iNumGridCells
  integer (kind=T_INT) :: iNumIterationsNochange
  logical (kind=T_LOGICAL) :: lExist
  logical (kind=T_LOGICAL) :: lCircular = lFALSE
  type( T_GENERAL_GRID ), pointer :: pTempGrid
  type (T_CELL),pointer :: cel

  ! calculate number of gridcells in model domain
  iNumGridCells = pGrd%iNY * pGrd%iNX

  ! set iteration counter
  iNumIterationsNochange = 0

  pTempGrid=>grid_Create( pGrd%iNX, pGrd%iNY, pGrd%rX0, pGrd%rY0, &
    pGrd%rX1, pGrd%rY1, DATATYPE_INT )

  allocate(iOrderCol(pGrd%iNY*pGrd%iNX), iOrderRow(pGrd%iNY*pGrd%iNX), stat=iStat)
  call Assert( iStat == 0, &
    "Could not allocate order of solution vectors for downhill procedure")

  INQUIRE( FILE='swb_routing_table.bin', EXIST=lExist)
  EXISTS: if (.not. lExist) then

  iPasses = 0
  write(UNIT=LU_LOG,FMT=*) "Configuring the downhill routing table..."
  flush(UNIT=LU_LOG)
  iOrderCount = 0
  pGrd%Cells%lDownhillMarked = lFALSE

  do
    iNChange = 0
    do iRow=1,pGrd%iNY
      do iCol=1,pGrd%iNX  ! last index in a Fortan array should be the slowest changing
        cel => pGrd%Cells(iCol,iRow)
        if ( cel%lDownhillMarked ) cycle
        ! Count upstream cells
        iUpstreamCount = 0

  ! now search all adjacent cells which have current cell
  ! as their target

  lCircular = lFALSE

  do iRowSub=iRow-1,iRow+1
    if (iRowSub>=1 .and. iRowSub<=pGrd%iNY) then     ! we're within array bounds
      do iColSub=iCol-1,iCol+1
        if (iColSub>=1 .and. iColSub<=pGrd%iNX) then  ! we're within array bounds
          if (iRow==iRowSub .and. iCol==iColSub) cycle  ! Skip current inquiry cell
            if (pGrd%Cells(iColSub,iRowSub)%lDownhillMarked) cycle  ! Don't count marked neighbors
            call model_DownstreamCell(pGrd,iRowSub,iColSub,tj,ti)
              if (tj==iRow .and. ti==iCol ) then
                iUpstreamCount = iUpstreamCount+1
                ! we've found a cell that points to the current model
                ! cell; does our current model cell point back at it?
                ! if so, we have circular flow
                call model_DownstreamCell(pGrd,iRow,iCol,iTgt_Row,iTgt_Col)
                if (iTgt_Row==iRowSub .and. iTgt_Col==iColSub ) lCircular = lTRUE
              end if
        end if
      end do
    end if
  end do

  ! If there are none, we can mark this cell
  ! If we have circular flow (a points to b, b points to a),
  ! we can mark the current cell; both a and b will be set to
  ! closed depressions in subsequent processing
  if ( iUpstreamCount == 0  &
    .or. (iUpstreamCount == 1 .and. lCircular)) then
    iNChange = iNChange+1
    cel%lDownhillMarked = lTRUE
    iOrderCount = iOrderCount+1
    iOrderCol(iOrderCount) = iCol
    iOrderRow(iOrderCount) = iRow
    !write(UNIT=LU_LOG,FMT=*) 'found ',iOrderCount, iRow, iCol
  elseif ( iNumIterationsNochange > 10 ) then
    ! convert offending cell into a depression
    ! we've gotten to this point because flow paths are circular;
    ! this is likely in a flat area of the DEM, and is in reality
    ! likely to be a depression
    iNChange = iNChange+1
    cel%lDownhillMarked = lTRUE
    cel%iFlowDir = 0
    iOrderCount = iOrderCount+1
    iOrderCol(iOrderCount) = iCol
    iOrderRow(iOrderCount) = iRow
    !write(UNIT=LU_LOG,FMT=*) 'found ',iOrderCount, iRow, iCol
  end if

  end do  ! loop over rows
end do  ! loop over columns

  if ( iNChange==0 ) then

  iNumIterationsNochange = iNumIterationsNochange + 1

  iCumlCount = 0
  write(LU_LOG,"(/,1x,'Summary of remaining unmarked cells')")

  ! loop over possible (legal) values of ther flow direction grid
  do k=0,128
    iCount=COUNT(.not. pGrd%Cells%lDownHillMarked &
      .and.pGrd%Cells%iFlowDir==k)
    if(iCount>0) then
      iCumlCount = iCumlCount + iCount
      write(LU_LOG,FMT="(3x,i8,' unmarked grid cells have flowdir value: ',i8)") &
        iCount, k
    end if
  end do

  write(LU_LOG,FMT="(3x,a)") repeat("-",60)
  write(LU_LOG,FMT="(3x,i8,' Total cells with nonzero flow " &
    //"direction values')") iCumlCount

  where( pGrd%Cells%lDownHillMarked )
    pTempGrid%iData = iROUTE_CELL_MARKED
  elsewhere
    pTempGrid%iData = pGrd%Cells%iFlowDir
  end where

!#ifdef GRAPHICS_SUPPORT
!        call genericgraph(pTempGrid)
!#endif

#ifdef DEBUG_PRINT

  call grid_WriteArcGrid("iteration"//TRIM(int2char(iPasses))// &
    "problem_gridcells.asc", &
    pTempGrid%rX0,pTempGrid%rX1,pTempGrid%rY0,pTempGrid%rY1, &
    REAL(pTempGrid%iData,kind=T_SGL))

#endif

  else
    ! reset iteration counter
    iNumIterationsNochange = 0
  end if

  if(iOrderCount == iNumGridCells) exit
  iPasses = iPasses+1
  write(UNIT=LU_LOG,FMT=*) 'Iteration ',iPasses,'  ',iOrderCount,&
    ' of ',iNumGridCells,' cells have been configured'
end do

  write(UNIT=LU_LOG,FMT=*) "  Number of passes required: ",iPasses
  write(UNIT=LU_LOG,FMT=*) ""
  flush(UNIT=LU_LOG)

  open( LU_ROUTING, FILE='swb_routing.bin',FORM='UNFORMATTED', &
    status='REPLACE',ACCESS='STREAM')
  write(LU_ROUTING) iOrderCount

  do ic=1,iOrderCount
    write(LU_ROUTING) iOrderCol(ic),iOrderRow(ic)
  end do
  flush(UNIT=LU_ROUTING)
  close(UNIT=LU_ROUTING)

  else ! routing table already exists

  pGrd%Cells%lDownhillMarked = lTRUE
  open(LU_ROUTING, FILE='swb_routing.bin',FORM='UNFORMATTED', &
    ACCESS='STREAM')
  read(LU_ROUTING) iOrderCount

  ! crude error checking to see whether the routing table has the right
  ! number of elements
  call Assert(LOGICAL(iOrderCount==iNumGridCells,kind=T_LOGICAL), &
    'Problem with existing routing file.  Delete swb_routing.bin and rerun')

  do ic=1,iOrderCount
    read(LU_ROUTING) iOrderCol(ic),iOrderRow(ic)
  end do

  close(UNIT=LU_ROUTING)

  write(UNIT=LU_LOG,FMT=*) ""
  write(UNIT=LU_LOG,FMT=*)  "*****************************************************************************"
  write(UNIT=LU_LOG,FMT=*)  "NOTE: Read in downhill routing information from existing swb_routing.bin file"
  write(UNIT=LU_LOG,FMT=*)  "*****************************************************************************"
  write(UNIT=LU_LOG,FMT=*) ""
  flush(UNIT=LU_LOG)

  write(UNIT=LU_STD_OUT,FMT=*)  "*****************************************************************************"
  write(UNIT=LU_STD_OUT,FMT=*)  "NOTE: Read in downhill routing information from existing swb_routing.bin file"
  write(UNIT=LU_STD_OUT,FMT=*)  "*****************************************************************************"

  end if EXISTS

  return

  end subroutine model_ConfigureRunoffDownhill

!!***

!--------------------------------------------------------------------------
!!****s* model/model_RunoffDownhill
! NAME
!   model_RunoffDownhill - Performs a "downhill" one-pass solution for
!                          the surface water runoff calculation.
!
! SYNOPSIS
!   This subroutine makes a single pass through all grid cells in the
!   model domain in order to calculate surface water runoff. The grid
!   cells farthest upstream are solved first. Calculation of runoff amounts
!   proceeds from upstream to downstream until a calculation has been made
!   for all grid cells.
!
! INPUTS
!   pGrd - Pointer to the model grid data structure.
!   pConfig - Pointer to the model configuration data structure.
!   iDayOfYear - Day of the current year (January 1 = 1).
!   iMonth - Month corresponding to the current model day (January = 1).
!
! OUTPUTS
!   NONE
!
! NOTES
!   ** Code refers to parameters that are set within types.f95.
!   ** Code directly modifies model grid values through the manipulation
!       of pointers.
!
! SOURCE

subroutine model_RunoffDownhill(pGrd, pConfig, iDayOfYear, iMonth)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd          ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  integer (kind=T_INT),intent(in) :: iDayOfYear
  integer (kind=T_INT), intent(in) :: iMonth       ! Integer month value (1-12)
  ! [ LOCALS ]
  integer (kind=T_INT) :: ic,iTgt_Col,iTgt_Row,iFrac
  real (kind=T_SGL) :: rP,rR,rDelta
  type (T_CELL),pointer :: cel
  type (T_CELL),pointer :: target_cel


  ! Reset the upstream flows (note that iOrderCount, iOrderCol, and iOrderRow are globals)
  do ic=1,iOrderCount

  cel => pGrd%Cells(iOrderCol(ic),iOrderRow(ic))
  call model_DownstreamCell(pGrd,iOrderRow(ic),iOrderCol(ic),iTgt_Row,iTgt_Col)

#ifdef STREAM_INTERACTIONS
  cel%rStreamCapture = rZERO
#endif

  ! Compute the runoff
  cel%rOutFlow = rf_model_CellRunoff(pConfig, cel, iDayOfYear)

  ! Now, route the water
  if ( iTgt_Row == iROUTE_LEFT_GRID ) then
    cel%rFlowOutOfGrid = cel%rOutflow
    cel%rOutFlow = rZERO
    cycle
  elseif ( iTgt_Row == iROUTE_DEPRESSION ) then
    ! Don't route any further; the water pools here.
    cel%rOutFlow = rZERO
    cycle
  endif

  ! MUST screen target values to ensure we don't start attempting
  ! manipulation of memory that is out of bounds!!
  call Assert(LOGICAL(iTgt_Row>0 .and. iTgt_Row <= pGrd%iNY,kind=T_LOGICAL), &
    "iTgt_Row out of bounds: Row = "//int2char(iOrderRow(ic)) &
    //"  Col = "//int2char(iOrderCol(ic)), &
    trim(__FILE__),__LINE__)
  call Assert(LOGICAL(iTgt_Col>0 .and. iTgt_Col <= pGrd%iNX,kind=T_LOGICAL), &
    "iTgt_Col out of bounds: Row = "//int2char(iOrderRow(ic)) &
    //"  Col = "//int2char(iOrderCol(ic)), &
    trim(__FILE__),__LINE__)

  target_cel => pGrd%Cells(iTgt_Col,iTgt_Row)

#ifdef STREAM_INTERACTIONS

  if(target_cel%iStreamIndex > 0) then
    ! route outflow to a specific stream or fracture ID
    cel%rStreamCapture = cel%rOutFlow
    cel%rOutFlow = rZERO
  else if &
#else
  if &
#endif
  (target_cel%iLandUse == pConfig%iOPEN_WATER_LU &
  .or. target_cel%rSoilWaterCap<rNEAR_ZERO) then
  ! Don't route any further; the water has joined a generic
  ! surface water feature. We assume that once the water hits a
  ! surface water feature that the surface water drainage
  ! network transports the bulk of it
  ! out of the model domain quite rapidly
  cel%rFlowOutOfGrid = cel%rOutflow
  cel%rOutFlow = rZERO

  else
    ! add cell outflow to target cell inflow
      target_cel%rInFlow = &
        target_cel%rInFlow + cel%rOutFlow * cel%rRouteFraction
      cel%rFlowOutOfGrid = cel%rOutflow * &
        (rONE - cel%rRouteFraction)
      cel%rOutflow = cel%rOutflow * cel%rRouteFraction
  end if

  end do

end subroutine model_RunoffDownhill

!!***

!--------------------------------------------------------------------------
!!****s* model/model_Runoff_NoRouting
! NAME
!   model_Runoff_NoRouting - Removes any computed runoff directly from the
!                            model domain.
! SYNOPSIS
!   This subroutine makes a single pass through all grid cells in the
!   model domain in order to calculate surface water runoff. The grid
!   cells farthest upstream are solved first. Calculation of runoff amounts
!   proceeds from upstream to downstream until a calculation has been made
!   for all grid cells.
!
!   NOTE that this version performs *NO* routing, but simply removes any
!   runoff directly from the model domain.
!
! INPUTS
!   pGrd - Pointer to the model grid data structure.
!   pConfig - Pointer to the model configuration data structure.
!   iDayOfYear - Day of the current year (January 1 = 1).
!   iMonth - Month corresponding to the current model day (January = 1).
!
! OUTPUTS
!   NONE
!
! NOTES
!   ** Code refers to parameters that are set within types.f95.
!   ** Code directly modifies model grid values through the manipulation
!       of pointers.
!
! SOURCE

subroutine model_Runoff_NoRouting(pGrd, pConfig, iDayOfYear, iMonth)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd          ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  integer (kind=T_INT),intent(in) :: iDayOfYear
  integer (kind=T_INT), intent(in) :: iMonth       ! Integer month value (1-12)
  ! [ LOCALS ]
  integer (kind=T_INT) :: iCol,iRow, iFrac
  real (kind=T_SGL) :: rR
  type (T_CELL),pointer :: cel
  ! [ CONSTANTS ]

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX
      cel => pGrd%Cells(iCol,iRow)

  ! Compute the runoff for each cell
  rR = rf_model_CellRunoff(pConfig, cel, iDayOfYear)

  ! Now, remove any runoff from the model grid
!      call stats_UpdateAllAccumulatorsByCell(REAL(rR,kind=T_DBL), &
!         iRUNOFF_OUTSIDE,iMonth,iZERO)

  cel%rFlowOutOfGrid = rR
!       cel%rOutFlow = rR

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! What is the point of this? If we aren't routing,
!! only a small amount of water (generated from a
!! cell directly beneath a stream segment) will
!! be captured...
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef STREAM_INTERACTIONS
  ! Capture into streams or fractures
  cel%rStreamCapture = rZERO
  if ( cel%iStreamIndex /= 0 ) then
    ! Compute the amount of fracture recharge
    cel%rStreamCapture = cel%rInFlow * pconfig%rStreamMaxCapture(cel%iStreamIndex) &
      / pconfig%rStreamMaxInflow(cel%iStreamIndex)
    if (cel%rStreamCapture < rZERO) then
      print *, "Negative!", cel%rInFlow, cel%rStreamCapture
    endif
    cel%rOutFlow = cel%rOutFlow - cel%rStreamCapture
  end if
#endif

  ! we've removed the water from the grid; it shouldn't be included in
  ! "outflow" water
  cel%rOutFlow = rZERO

  end do
end do

  return

end subroutine model_Runoff_NoRouting

!!***

!--------------------------------------------------------------------------
!!****f* model/if_model_RunoffIteration
! NAME
!   if_model_RunoffIteration -
!
!
! SYNOPSIS
!
! INPUTS
!   pGrd - Pointer to the model grid data structure.
!   pConfig - Pointer to the model configuration data structure.
!   iDayOfYear - Day of the current year (January 1 = 1).
!   iMonth - Month corresponding to the current model day (January = 1).
!
! OUTPUTS
!   NONE
!
! NOTES
!   ** Code refers to parameters that are set within types.f95.
!   ** Code directly modifies model grid values through the manipulation
!       of pointers.
!
! SOURCE

function if_model_RunoffIteration(pGrd, pConfig, iDayOfYear, iMonth) result(iCount)
  !! Performs one runoff iteration for the specified amount of precip.
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd          ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  integer (kind=T_INT),intent(in) :: iDayOfYear
  integer (kind=T_INT), intent(in) :: iMonth       ! Integer month value (1-12)
  ! [ RETURN VALUE ]
  integer (kind=T_INT) :: iCount
  ! [ LOCALS ]
  integer (kind=T_INT) :: iCol,iRow,iTgt_Row,iTgt_Col
  real (kind=T_SGL) :: rR,rDelta
  type (T_CELL),pointer :: cel,tcel
  real (kind=T_DBL) :: xmin, xmax, ymin, ymax
!  real (kind=T_SGL) :: rMonthlyRunoffOutside, rDailyRunoffOutside

  ! [ CONSTANTS ]

  ! reset the term for tracking outflow from one iteration to the next
  rDelta = rZERO

  ! Reset the upstream flows
  iCount = 0

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX
      cel => pGrd%Cells(iCol,iRow)
      call model_DownstreamCell(pGrd,iRow,iCol,iTgt_Row,iTgt_Col)

  if ( iTgt_Row == iROUTE_DEPRESSION ) then
    ! Don't route any further; the water pools here.
    cel%rOutFlow = rZERO
!        call stats_UpdateAllAccumulatorsByCell(dpZERO, &
!            iRUNOFF_OUTSIDE,iMonth,iZERO)
  cycle
end if

  ! Compute the runoff
  rR = rf_model_CellRunoff(pConfig, cel, iDayOfYear)

  if( iTgt_Row == iROUTE_LEFT_GRID ) then
    rDelta = rR - cel%rFlowOutOfGrid
    cel%rFlowOutOfGrid = cel%rFlowOutOfGrid + rDelta
    cycle
  end if

  call Assert(LOGICAL(iTgt_Row>0 .and. iTgt_Row <= pGrd%iNY,kind=T_LOGICAL), &
    "iTgt_Row out of bounds: iRow= "//int2char(iRow)//"  iCol= "//int2char(iCol), &
    trim(__FILE__),__LINE__)
  call Assert(LOGICAL(iTgt_Col>0 .and. iTgt_Col <= pGrd%iNX,kind=T_LOGICAL), &
    "iTgt_Col out of bounds: iRow= "//int2char(iRow)//"  iCol= "//int2char(iCol), &
    trim(__FILE__),__LINE__)

  tcel => pGrd%Cells(iTgt_Row,iTgt_Col)

  if(tcel%iLandUse == pConfig%iOPEN_WATER_LU) then

  rDelta = rR - cel%rFlowOutOfGrid
  cel%rFlowOutOfGrid = cel%rFlowOutOfGrid + rDelta

  else  ! route water normally

  rDelta = rR - cel%rOutFlow
  tcel%rInFlow = tcel%rInFlow + rDelta
  cel%rOutFlow = cel%rOutFlow + rDelta
end if

  ! Did we make a change?
  if ( rDelta > pConfig%rIterationTolerance ) then
    iCount = iCount+1
  end if

  end do
end do

  return
end function if_model_RunoffIteration

!!***

!--------------------------------------------------------------------------
!!****f* model/rf_model_CellRunoff
! NAME
!   rf_model_CellRunoff - Calculate runoff for a cell.
!
! SYNOPSIS
!   This subroutine determines the runoff (outflow) from a given cell
!   by calling the appropriate runoff calculation function. Currently the
!   function calls either a curve-number based runoff calculation function,
!   or a Green-Ampt based runoff calculation function (unimplemented).
!
! INPUTS
!   pConfig - Pointer to the model configuration data structure.
!   cel - Pointer to the grid cell for which runoff (outflow) calculation
!         should occur.
!   iDayOfYear - Integer value of the day number of the current year (1-366).
!
! OUTPUTS
!   rOutFlow - Runoff (outflow) value in inches.
!
! SOURCE

function rf_model_CellRunoff(pConfig, cel, iDayOfYear) result(rOutFlow)
  !! Calculates a single cell's runoff
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  type (T_CELL),pointer :: cel
  integer (kind=T_INT),intent(in) :: iDayOfYear
  ! [ RETURN VALUE ]
  real (kind=T_SGL) :: rOutFlow
  ! [ LOCALS ]

  if (pConfig%iConfigureRunoff == CONFIG_RUNOFF_CURVE_NUMBER) then
    rOutFlow = runoff_CellRunoff_CurveNumber(pConfig, cel, iDayOfYear)
    call Assert(rOutFlow >= rZERO,"CN outflow is negative", &
      TRIM(__FILE__),__LINE__)
  else if (pConfig%iConfigureRunoff == CONFIG_RUNOFF_GREEN_AMPT) then
    rOutFlow = rf_model_CellRunoff_GreenAmpt(pConfig, cel, iDayOfYear)
    call Assert(rOutFlow >= rZERO,"Green-Ampt outflow is negative", &
      TRIM(__FILE__),__LINE__)
  end if

  return
end function rf_model_CellRunoff

!!***
!--------------------------------------------------------------------------
!!****f* model/rf_model_CellRunoff_GreenAmpt
! NAME
!   rf_model_CellRunoff_GreenAmpt - Calculate runoff for a cell using a
!      Green-Ampt based calculation method.
!
! SYNOPSIS
!   This function will (when implemented) return the value of runoff for
!   the current grid cell based on a Green-Ampt based calculation method.
!   The function currently throws an error if called.
!
! INPUTS
!   pConfig - Pointer to the model configuration data structure.
!   cel - Pointer to the grid cell for which runoff (outflow) calculation
!         should occur.
!   iDayOfYear - Integer value of the day number of the current year (1-366).
!
! OUTPUTS
!   rOutFlow - Runoff (outflow) value in inches.
!
! SOURCE

function rf_model_CellRunoff_GreenAmpt(pConfig, cel, iDayOfYear) result(rOutFlow)
  !! Calculates a single cell's runoff using the modified Green-Ampt model
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  type (T_CELL),pointer :: cel
  integer (kind=T_INT),intent(in) :: iDayOfYear
  ! [ RETURN VALUE ]
  real (kind=T_SGL) :: rOutFlow
  ! [ LOCALS ]

  call Assert( lFALSE, "Steve needs to put some code in here!" )
  !!!STEVE
  !!!PUT SINGLE_CELL G-A _RUNOFF_ CALCULATION IN HERE

  rOutFlow = -9999.

  return
end function rf_model_CellRunoff_GreenAmpt

!!***
!--------------------------------------------------------------------------
!!****s* model/model_InitializeFlowDirection
! NAME
!   model_InitializeFlowDirection - Scans the flow direction grid for
!     closed depressions and marks them.
!
! SYNOPSIS
!   This subroutine makes a single pass through the FLOW DIRECTION grid
!   input and assigns flow directions to the model grid based on that
!   input.  If the cyclic flow routing is detected (a => b; b => a),
!   both cells will be marked as closed depressions.  If the target of the
!   current grid cell is outside the model domain, the target cell is
!   identified as having left the grid.
!
! INPUTS
!   pGrd - Pointer to the model grid data structure.
!   pConfig - Pointer to the model configuration data structure.
!
! OUTPUTS
!   NONE
!
! NOTES
!   ** Code refers to parameters that are set within types.f95.
!   ** Code directly modifies model grid values through the manipulation
!       of pointers.
!
! SOURCE

subroutine model_InitializeFlowDirection( pGrd , pConfig)
  !! Scans the flow direction grid for closed depressions and marks them
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd         ! pointer to model grid

  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings

  integer (kind=T_INT) :: iRow,iCol
  integer (kind=T_INT) :: iTgt_Row,iTgt_Col
  ! [ PARAMETERS ]
  integer (kind=T_SHORT),parameter :: DIR_DEPRESSION=0
  integer (kind=T_SHORT),parameter :: DIR_RIGHT=1
  integer (kind=T_SHORT),parameter :: DIR_DOWN_RIGHT=2
  integer (kind=T_SHORT),parameter :: DIR_DOWN=4
  integer (kind=T_SHORT),parameter :: DIR_DOWN_LEFT=8
  integer (kind=T_SHORT),parameter :: DIR_LEFT=16
  integer (kind=T_SHORT),parameter :: DIR_UP_LEFT=32
  integer (kind=T_SHORT),parameter :: DIR_UP=64
  integer (kind=T_SHORT),parameter :: DIR_UP_RIGHT=128
  character (len=256) :: sBuf

  ! no point in doing these calculations unless we're really going to
  ! route water
  if(pConfig%iConfigureRunoffMode==CONFIG_RUNOFF_NO_ROUTING) return


  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX

  select case (pGrd%Cells(iCol,iRow)%iFlowDir)
    case ( DIR_DEPRESSION )
      iTgt_Col = iROUTE_DEPRESSION               ! added Jan 2009 SMW
      iTgt_Row = iROUTE_DEPRESSION               ! added Jan 2009 SMW
      continue
    case ( DIR_RIGHT )
      iTgt_Row = iRow
      iTgt_Col = iCol+1
      if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
        iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
      if ( pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir == DIR_LEFT ) then
        pGrd%Cells(iCol,iRow)%iFlowDir = DIR_DEPRESSION
        pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir = DIR_DEPRESSION
        write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
      end if
      end if
    case ( DIR_DOWN_RIGHT )
      iTgt_Row = iRow+1
      iTgt_Col = iCol+1
      if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
        iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
      if ( pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir == DIR_UP_LEFT ) then
        pGrd%Cells(iCol,iRow)%iFlowDir = DIR_DEPRESSION
        pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir = DIR_DEPRESSION
        write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
      end if
      end if
    case ( DIR_DOWN )
      iTgt_Row = iRow+1
      iTgt_Col = iCol
      if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
        iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
      if ( pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir == DIR_UP ) then
        pGrd%Cells(iCol,iRow)%iFlowDir = DIR_DEPRESSION
        pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir = DIR_DEPRESSION
        write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
      end if
      end if
    case ( DIR_DOWN_LEFT )
      iTgt_Row = iRow+1
      iTgt_Col = iCol-1
      if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
        iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
      if ( pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir == DIR_UP_RIGHT ) then
        pGrd%Cells(iCol,iRow)%iFlowDir = DIR_DEPRESSION
        pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir = DIR_DEPRESSION
        write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
      end if
      end if
    case ( DIR_LEFT )
      iTgt_Row = iRow
      iTgt_Col = iCol-1
      if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
        iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
      if ( pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir == DIR_RIGHT) then
        pGrd%Cells(iCol,iRow)%iFlowDir = DIR_DEPRESSION
        pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir = DIR_DEPRESSION
        write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
      end if
      end if
    case ( DIR_UP_LEFT )
      iTgt_Row = iRow-1
      iTgt_Col = iCol-1
      if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
        iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
      if ( pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir == DIR_DOWN_RIGHT) then
        pGrd%Cells(iCol,iRow)%iFlowDir = DIR_DEPRESSION
        pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir = DIR_DEPRESSION
        write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
      end if
      end if
    case ( DIR_UP )
      iTgt_Row = iRow-1
      iTgt_Col = iCol
      if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
        iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
      if ( pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir == DIR_DOWN) then
        pGrd%Cells(iCol,iRow)%iFlowDir = DIR_DEPRESSION
        pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir = DIR_DEPRESSION
        write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
      end if
      end if
    case ( DIR_UP_RIGHT )
      iTgt_Row = iRow-1
      iTgt_Col = iCol+1
      if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
        iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
      if ( pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir == DIR_DOWN_LEFT) then
        pGrd%Cells(iCol,iRow)%iFlowDir = DIR_DEPRESSION
        pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir = DIR_DEPRESSION
        write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
      end if
      end if
    case default  !! flow direction indeterminate
    !!
    !!  NOTE: This may not be the correct way to deal with indeterminate
    !!        flow directions!!
    !!
      write ( unit=sBuf, fmt='("Flow direction grid element (",i5,",",i5,' &
        // '") contains undefined flow direction with integer value: ",i4)' ) &
          iCol,iRow,pGrd%Cells(iCol,iRow)%iFlowDir
      write(UNIT=LU_LOG,FMT=*)  sBuf
      pGrd%Cells(iCol,iRow)%iFlowDir = DIR_DEPRESSION
      iTgt_Col = iROUTE_DEPRESSION
      iTgt_Row = iROUTE_DEPRESSION
  end select

  ! does the current value of either target point outside of the grid?
  if ( iTgt_Row == 0 .or. iTgt_Row > pGrd%iNY .or. &
    iTgt_Col == 0 .or. iTgt_Col > pGrd%iNX ) then
      iTgt_Row = iROUTE_LEFT_GRID
      iTgt_Col = iROUTE_LEFT_GRID
  end if

  ! now assign the value of the targets to the iTgt element of the
  ! grid data structure
  pGrd%Cells(iCol,iRow)%iTgt_Row = iTgt_Row
  pGrd%Cells(iCol,iRow)%iTgt_Col = iTgt_Col
end do
end do

#ifdef DEBUG_PRINT

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX
      if(pGrd%Cells(iCol,iRow)%iTgt_Col==iCol .and. pGrd%Cells(iCol,iRow)%iTgt_Row==iRow) then
        write(unit=LU_LOG,FMT=*) 'ALERT** target is the same as the originating cell'
        write(unit=LU_LOG,FMT=*) '  ORIG   (iRow, iCol) : ',iRow, iCol
        write(unit=LU_LOG,FMT=*) '  ==> FLOWDIR: ',pGrd%Cells(iCol,iRow)%iFlowDir
        write(unit=LU_LOG,FMT=*) '  TARGET (iRow, iCol) : ',pGrd%Cells(iCol,iRow)%iTgt_Row, &
          pGrd%Cells(iCol,iRow)%iTgt_Col
        write(unit=LU_LOG,FMT=*) '  ==> FLOWDIR: ' , &
          pGrd%Cells(pGrd%Cells(iCol,iRow)%iTgt_Row,pGrd%Cells(iCol,iRow)%iTgt_Col)%iFlowDir
      end if
    end do
end do

#endif

  return
end subroutine model_InitializeFlowDirection

!!***

!--------------------------------------------------------------------------

subroutine model_DownstreamCell(pGrd,iRow,iCol,iTgt_Row,iTgt_Col)
  !! Determines the "downstream" cell for cell (iRow,iCol) and returns the index in
  !! (iTgt_Row,iTgt_Col)
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  integer (kind=T_INT),intent(in) :: iRow,iCol
  integer (kind=T_INT),intent(out) :: iTgt_Row,iTgt_Col
  ! [ PARAMETERS ]
  integer (kind=T_SHORT),parameter :: DIR_DEPRESSION=0
  integer (kind=T_SHORT),parameter :: DIR_RIGHT=1
  integer (kind=T_SHORT),parameter :: DIR_DOWN_RIGHT=2
  integer (kind=T_SHORT),parameter :: DIR_DOWN=4
  integer (kind=T_SHORT),parameter :: DIR_DOWN_LEFT=8
  integer (kind=T_SHORT),parameter :: DIR_LEFT=16
  integer (kind=T_SHORT),parameter :: DIR_UP_LEFT=32
  integer (kind=T_SHORT),parameter :: DIR_UP=64
  integer (kind=T_SHORT),parameter :: DIR_UP_RIGHT=128

  select case (pGrd%Cells(iCol,iRow)%iFlowDir)
    case ( DIR_DEPRESSION )
      iTgt_Row = iROUTE_DEPRESSION  ! value is -999
      iTgt_Col = iROUTE_DEPRESSION  ! value is -999
    case ( DIR_RIGHT )
      iTgt_Row = iRow
      iTgt_Col = iCol+1
    case ( DIR_DOWN_RIGHT )
      iTgt_Row = iRow+1
      iTgt_Col = iCol+1
    case ( DIR_DOWN )
      iTgt_Row = iRow+1
      iTgt_Col = iCol
    case ( DIR_DOWN_LEFT )
      iTgt_Row = iRow+1
      iTgt_Col = iCol-1
    case ( DIR_LEFT )
      iTgt_Row = iRow
      iTgt_Col = iCol-1
    case ( DIR_UP_LEFT )
      iTgt_Row = iRow-1
      iTgt_Col = iCol-1
    case ( DIR_UP )
      iTgt_Row = iRow-1
      iTgt_Col = iCol
    case ( DIR_UP_RIGHT )
      iTgt_Row = iRow-1
      iTgt_Col = iCol+1
  end select

  ! the following code was trapping all 'iROUTE_DEPRESSION' values and
  ! converting them to iROUTE_LEFT_GRID values
  !
  ! changed test from "iTgt_Row < 1" to "iTgt_Row == 0"

  if ( iTgt_Row == 0 .or. iTgt_Row > pGrd%iNY .or. &
    iTgt_Col == 0 .or. iTgt_Col > pGrd%iNX ) then    ! Left the grid?
  iTgt_Row = iROUTE_LEFT_GRID
  iTgt_Col = iROUTE_LEFT_GRID
  end if

  return
end subroutine model_DownstreamCell

!--------------------------------------------------------------------------

subroutine model_ReadBasinMaskTable ( pConfig )
  !! reads the basin cacthment data file for subsequent processing
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
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

  iRecNum = iRecNum + 1

  end do BMASK

  flush(UNIT=LU_LOG)

end subroutine model_ReadBasinMaskTable

!--------------------------------------------------------------------------

subroutine model_ReadLanduseLookupTable( pConfig )
  !! Reads the landuse data from pConfig%sLanduseLookupFilename
  ! [ ARGUMENTS ]
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  ! [ LOCALS ]
  integer (kind=T_INT) :: iStat, iNumLandUses, i, iType, iRecNum, iSize
  integer (kind=T_INT) :: iNumSoilTypes
  character (len=1024) :: sRecord                  ! Input file text buffer
  character (len=256) :: sItem                    ! Key word read from sRecord

  ! open landuse file
  open ( LU_LOOKUP, file=pConfig%sLanduseLookupFilename, &
    status="OLD", iostat=iStat )
  call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
    "Open failed for file: " // pConfig%sLanduseLookupFilename )

  ! read first line of file
  read ( unit=LU_LOOKUP, fmt="(a)", iostat=iStat ) sRecord
  call Assert( iStat == 0, &
    "Error reading first line of landuse lookup table" )

  ! read landuse file to obtain expected number of landuse types
  call chomp(sRecord, sItem , sTAB )
!  call chomp_tab( sRecord, sItem )
  if ( str_compare(sItem,"NUM_LANDUSE_TYPES") ) then
    call chomp(sRecord, sItem , sTAB )
    read ( unit=sItem, fmt=*, iostat=iStat ) iNumLandUses
    call Assert( iStat == 0, "Failed to read number of landuse types" )
    write(UNIT=LU_LOG,FMT=*)  "==> allocating memory for",iNumLandUses,"landuse types within lookup table"
  else
    call Assert( lFALSE, &
      "Unknown option in landuse lookup table; was expecting NUM_LANDUSE_TYPES #")
  end if

  ! read second line of file
  read ( unit=LU_LOOKUP, fmt="(a)", iostat=iStat ) sRecord
  call Assert( iStat == 0, &
    "Error reading second line of landuse lookup table" )

  ! read landuse file to obtain expected number of soil types
  call chomp(sRecord, sItem , sTAB )
  call Uppercase( sItem )
  if ( sItem == "NUM_SOIL_TYPES" ) then
    call chomp(sRecord, sItem , sTAB )
    read ( unit=sItem, fmt=*, iostat=iStat ) iNumSoilTypes
    call Assert( iStat == 0, "Failed to read number of soil types" )
    write(UNIT=LU_LOG,FMT=*)  "==> allocating memory for",iNumSoilTypes,"soil types within lookup table"
  else
    call Assert( lFALSE, &
      "Unknown option in landuse lookup table; was expecting NUM_SOIL_TYPES #")
  end if

  ! now allocate memory for landuse table
  allocate ( pConfig%LU( iNumLandUses ), stat=iStat )
  call Assert ( iStat == 0, &
    "Could not allocate space for landuse data structure" )

  iSize = size(pConfig%LU,1)

  ! now allocate memory for SOILS subtable within landuse table
  allocate ( pConfig%CN( iNumLandUses,iNumSoilTypes ), stat=iStat )
  call Assert ( iStat == 0, &
    "Could not allocate space for CN subtable within landuse data structure" )

  ! now allocate memory for ROOTING DEPTH subtable within landuse table
  allocate ( pConfig%ROOTING_DEPTH( iNumLandUses,iNumSoilTypes ), stat=iStat )
  call Assert ( iStat == 0, &
    "Could not allocate space for ROOTING_DEPTH subtable within landuse data structure" )

  ! now allocate memory for MAX_RECHARGE subtable within landuse table
  allocate ( pConfig%MAX_RECHARGE( iNumLandUses,iNumSoilTypes ), stat=iStat )
  call Assert ( iStat == 0, &
    "Could not allocate space for MAX_RECHARGE subtable within landuse data structure" )

#ifdef IRRIGATION_MODULE
  ! now allocate memory for IRRIGATION subtable within landuse table
  ! note that this table is NOT populated in this subroutine, but in "readIrrgationLookupTable"
  allocate ( pConfig%IRRIGATION( iNumLandUses ), stat=iStat )
  call Assert ( iStat == 0, &
    "Could not allocate space for IRRIGATION data structure" )
#endif

  iRecNum = 1

  LU_READ: do

  read ( unit=LU_LOOKUP, fmt="(a)", iostat=iStat ) sRecord
  if ( iStat < 0 ) exit     ! EOF mark
  if ( sRecord(1:1) == "#" ) cycle      ! Ignore comment lines

  if(iRecNum > iSize) then
    write(UNIT=LU_LOG,FMT=*) ""
    write(UNIT=LU_LOG,FMT=*)  " *** The maximum number of landuse lookup table elements has"
    write(UNIT=LU_LOG,FMT=*)  "     been read in before reaching the end of the file."
    write(UNIT=LU_LOG,FMT=*) ""
    write(UNIT=LU_LOG,FMT=*)  "     size of allocated memory for LU table: ",iSize
    write(UNIT=LU_LOG,FMT=*)  "     current record number: ", iRecNum
    exit
  end if

  write(UNIT=LU_LOG,FMT=*) ""
  write(UNIT=LU_LOG,FMT=*)  "-----------------------------------------------------------"
  write(UNIT=LU_LOG,FMT=*)  "Reading landuse record number ",iRecNum, " of ",iNumLandUses
  write(UNIT=LU_LOG,FMT=*) ""

  call chomp(sRecord, sItem, sTAB)
  read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%LU(iRecNum)%iLandUseType
  call Assert( iStat == 0, "Error reading land use type in landuse lookup table" )
  write(UNIT=LU_LOG,FMT=*)  "  landuse type = ",pConfig%LU(iRecNum)%iLandUseType

  call chomp( sRecord, pConfig%LU(iRecNum)%sLandUseDescription, sTAB)
  write(UNIT=LU_LOG,FMT=*)  "  landuse description = ", &
    TRIM(pConfig%LU(iRecNum)%sLandUseDescription)

  call chomp( sRecord, pConfig%LU(iRecNum)%sAssumedPercentImperviousness, sTAB)
  write(UNIT=LU_LOG,FMT=*)  "  assumed % imperviousness = ", &
    TRIM(pConfig%LU(iRecNum)%sAssumedPercentImperviousness)

  do i=1,iNumSoilTypes
  call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%CN(iRecNum,i)
    call Assert( iStat == 0, &
      "Error reading curve number for soil group "//trim(int2char(i))//" in landuse lookup table" )
    write(UNIT=LU_LOG,FMT=*)  "  curve number for soil group",i,": ",pConfig%CN(iRecNum,i)
  end do

  do i=1,iNumSoilTypes
  call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%MAX_RECHARGE(iRecNum,i)
    call Assert( iStat == 0, &
      "Error reading maximum recharge for soil group "//trim(int2char(i))//" in landuse lookup table" )
    write(UNIT=LU_LOG,FMT=*)  "  MAXIMUM RECHARGE for soil group",i,": ",pConfig%MAX_RECHARGE(iRecNum,i)
  end do

  call chomp(sRecord, sItem, sTAB)
  read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%LU(iRecNum)%rIntercept_GrowingSeason
  call Assert( iStat == 0, "Error reading interception values in landuse file" )
  write(UNIT=LU_LOG,FMT=*)  "  Interception value for growing season = ",pConfig%LU(iRecNum)%rIntercept_GrowingSeason

  call chomp(sRecord, sItem, sTAB)
read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%LU(iRecNum)%rIntercept_NonGrowingSeason
  call Assert( iStat == 0, "Error reading interception values in landuse file" )
  write(UNIT=LU_LOG,FMT=*)  "  Interception value for non-growing season = ", &
    pConfig%LU(iRecNum)%rIntercept_NonGrowingSeason

  ! now read in a rooting depth for each landuse/soil type combination
  do i=1,iNumSoilTypes
    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%ROOTING_DEPTH(iRecNum,i)
    call Assert( iStat == 0, &
      "Error reading rooting depth for soil group "//trim(int2char(i))//" in landuse lookup table" )
    write(UNIT=LU_LOG,FMT=*)  "  ROOTING DEPTH for soil group",i,": ",pConfig%ROOTING_DEPTH(iRecNum,i)
  end do

  iRecNum = iRecNum + 1

  end do LU_READ

#ifdef IRRIGATION_MODULE
  pConfig%IRRIGATION%iLandUseType = pConfig%LU%iLandUseType
#endif

  ! That's all!
  close ( unit=LU_LOOKUP )

  return
end subroutine model_ReadLanduseLookupTable

!--------------------------------------------------------------------------

#ifdef IRRIGATION_MODULE

subroutine model_ReadIrrigationLookupTable( pConfig )
  !! Reads the irrigation data from pConfig%sIrrigationLookupFilename
  ! [ ARGUMENTS ]
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  ! [ LOCALS ]
  integer (kind=T_INT) :: iStat, iNumLandUses, i, iType, iRecNum, iSize
  integer (kind=T_INT) :: iLandUseType
  integer (kind=T_INT) :: iNumSoilTypes
  character (len=1024) :: sRecord                  ! Input file text buffer
  character (len=256) :: sItem                    ! Key word read from sRecord

  ! open landuse file
  open ( LU_LOOKUP, file=pConfig%sIrrigationLookupFilename, &
    status="OLD", iostat=iStat )
  call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
    "Open failed for file: " // pConfig%sIrrigationLookupFilename )

  ! read first line of file
  read ( unit=LU_LOOKUP, fmt="(a)", iostat=iStat ) sRecord
  call Assert( iStat == 0, &
    "Error reading first line of irrigation lookup table" )

  ! read landuse file to obtain expected number of landuse types
  call chomp(sRecord, sItem, sTAB)
  call Uppercase( sItem )
  if ( sItem == "NUM_LANDUSE_TYPES" ) then
    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) iNumLandUses
    call Assert( iStat == 0, "Failed to read number of landuse types" )
    call Assert(iNumLandUses == size(pConfig%IRRIGATION), &
      "Number of landuses in IRRIGATION table is unequal to number in LANDUSE lookup table", &
      trim(__FILE__),__LINE__)
    write(UNIT=LU_LOG,FMT=*)  "==> allocating memory for",iNumLandUses,"landuse types within lookup table"
  else
    call Assert( lFALSE, &
      "Unknown option in irrigation lookup table; was expecting NUM_LANDUSE_TYPES #")
  end if

  call Assert(associated(pConfig%LU), "The landuse lookup table must be read in " &
    //"before the irrigation lookup table may be read.", trim(__FILE__),__LINE__)

  iSize = size(pConfig%LU,1)

  iRecNum = 1

  LU_READ: do

  read ( unit=LU_LOOKUP, fmt="(a)", iostat=iStat ) sRecord
  if ( iStat < 0 ) exit     ! EOF mark
  if ( sRecord(1:1) == "#" ) cycle      ! Ignore comment lines

  if(iRecNum > iSize) then
    write(UNIT=LU_LOG,FMT=*) ""
    write(UNIT=LU_LOG,FMT=*)  " *** The maximum number of irrigation lookup table elements has"
    write(UNIT=LU_LOG,FMT=*)  "     been read in before reaching the end of the file."
    write(UNIT=LU_LOG,FMT=*) ""
    write(UNIT=LU_LOG,FMT=*)  "     size of allocated memory for IRRIGATION table: ",iSize
    write(UNIT=LU_LOG,FMT=*)  "     current record number: ", iRecNum
    exit
  end if

  write(UNIT=LU_LOG,FMT=*) ""
  write(UNIT=LU_LOG,FMT=*)  "-----------------------------------------------------------"
  write(UNIT=LU_LOG,FMT=*)  "Reading irrigation table record number ",iRecNum, " of ",iNumLandUses
  write(UNIT=LU_LOG,FMT=*) ""

  call chomp(sRecord, sItem, sTAB)
  read ( unit=sItem, fmt=*, iostat=iStat ) iLandUseType
  call Assert( iStat == 0, "Error reading land use type in irrigation lookup table" )
  call Assert(iLandUseType == pConfig%LU(iRecNum)%iLandUseType, &
    "Landuse types in the irrigation lookup table must match those in the landuse lookup table", &
      trim(__FILE__), __LINE__)
  pConfig%IRRIGATION(iRecNum)%iLandUseType = iLandUseType

  call chomp(sRecord, sItem, sTAB)
  read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iRecNum)%rKc_Max
  call Assert( iStat == 0, &
    "Error reading maximum crop coefficient in irrigation lookup table" )
  write(UNIT=LU_LOG,FMT=*)  "  maximum crop coefficient ", &
    pConfig%IRRIGATION(iRecNum)%rKc_Max

  call chomp(sRecord, sItem, sTAB)
  read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iRecNum)%rK0
  call Assert( iStat == 0, &
    "Error reading first phenological stage crop coefficient in irrigation lookup table" )
  write(UNIT=LU_LOG,FMT=*)  "   first phenological stage crop coefficient ", &
    pConfig%IRRIGATION(iRecNum)%rK0

  call chomp(sRecord, sItem, sTAB)
  read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iRecNum)%rAlpha1
  call Assert( iStat == 0, &
    "Error reading shape coefficient (Alpha1) in irrigation lookup table" )
  write(UNIT=LU_LOG,FMT=*)  "   shape coefficient (Alpha1) ", &
    pConfig%IRRIGATION(iRecNum)%rAlpha1

  call chomp(sRecord, sItem, sTAB)
  read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iRecNum)%rGDD_Kc_Max
  call Assert( iStat == 0, &
    "Error reading GDD associated with max crop coefficient in irrigation lookup table" )
  write(UNIT=LU_LOG,FMT=*)  "   GDD associated with max crop coefficient ", &
    pConfig%IRRIGATION(iRecNum)%rGDD_Kc_Max

  call chomp(sRecord, sItem, sTAB)
  read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iRecNum)%rGDD_Death
  call Assert( iStat == 0, &
    "Error reading GDD associated with plant death in irrigation lookup table" )
  write(UNIT=LU_LOG,FMT=*)  "   GDD associated with plant death ", &
    pConfig%IRRIGATION(iRecNum)%rGDD_Death

  call chomp(sRecord, sItem, sTAB)
  read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iRecNum)%rGDD_BaseTemp
  call Assert( iStat == 0, &
    "Error reading GDD base temperature in irrigation lookup table" )
  write(UNIT=LU_LOG,FMT=*)  "   GDD base temperature ", &
    pConfig%IRRIGATION(iRecNum)%rGDD_BaseTemp

  call chomp(sRecord, sItem, sTAB)
  read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iRecNum)%rGDD_MaxTemp
  call Assert( iStat == 0, &
    "Error reading GDD max temperature in irrigation lookup table" )
  write(UNIT=LU_LOG,FMT=*)  "   GDD max temperature ", &
    pConfig%IRRIGATION(iRecNum)%rGDD_MaxTemp

  call chomp(sRecord, sItem, sTAB)
  read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iRecNum)%rMAD
  call Assert( iStat == 0, &
    "Error reading management allowable deficit (MAD) in irrigation lookup table" )
  write(UNIT=LU_LOG,FMT=*)  "   management allowable deficit (MAD) ", &
    pConfig%IRRIGATION(iRecNum)%rMAD

  call chomp(sRecord, sItem, sTAB)
  pConfig%IRRIGATION(iRecNum)%iBeginIrrigation = mmddyyyy2doy(sItem)
  write(UNIT=LU_LOG,FMT=*)  "   irrigation starts on or after day ", &
    pConfig%IRRIGATION(iRecNum)%iBeginIrrigation

  call chomp(sRecord, sItem, sTAB)
  pConfig%IRRIGATION(iRecNum)%iEndIrrigation = mmddyyyy2doy(sItem)
  write(UNIT=LU_LOG,FMT=*)  "   irrigation ends on day ", &
    pConfig%IRRIGATION(iRecNum)%iEndIrrigation

  iRecNum = iRecNum + 1

  end do LU_READ

  ! That's all!
  close ( unit=LU_LOOKUP )

  return
end subroutine model_ReadIrrigationLookupTable

#endif

!--------------------------------------------------------------------------

subroutine model_PopulateLandUseArray(pConfig, pGrd, pLandUseGrid)

  ! [ ARGUMENTS ]
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings

  type ( T_GENERAL_GRID ),pointer :: pGrd            ! pointer to model grid
  type ( T_GENERAL_GRID ),pointer :: pLandUseGrid     ! pointer to model grid

  if( len_trim( pConfig%sLandUse_PROJ4 ) > 0 ) then

    call echolog(" ")
    call echolog("Transforming gridded data in file: "//dquote(pLandUseGrid%sFilename) )
    call echolog("  FROM: "//squote(pConfig%sLandUse_PROJ4) )
    call echolog("  TO:   "//squote(pConfig%sBase_PROJ4) )

    call grid_Transform(pGrd=pLandUseGrid, &
                        sFromPROJ4=pConfig%sLandUse_PROJ4, &
                        sToPROJ4=pConfig%sBase_PROJ4 )

    call Assert( grid_CompletelyCover( pGrd, pLandUseGrid ), &
      "Transformed grid doesn't completely cover your model domain.")

    call grid_gridToGrid(pGrdFrom=pLandUseGrid,&
                            iArrayFrom=pLandUseGrid%iData, &
                            pGrdTo=pGrd, &
                            iArrayTo=pGrd%Cells%iLandUse )

  else

    call Assert( grid_Conform( pGrd, pLandUseGrid ), &
                      "Non-conforming LANDUSE grid. Filename: " &
                      //dquote(pLandUseGrid%sFilename) )
    pGrd%Cells%iLandUse = pLandUseGrid%iData

  endif

  call grid_Destroy(pLandUseGrid)

  pGenericGrd_int%iData = pGrd%Cells%iLandUse
  call grid_WriteGrid(sFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Land_Use_Land_Cover" // &
    "."//trim(pConfig%sOutputFileSuffix), pGrd=pGenericGrd_int, pConfig=pConfig )

end subroutine model_PopulateLandUseArray

!--------------------------------------------------------------------------

subroutine model_PopulateSoilGroupArray(pConfig, pGrd, pSoilGroupGrid)

  ! [ ARGUMENTS ]
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings

  type ( T_GENERAL_GRID ),pointer :: pGrd            ! pointer to model grid
  type ( T_GENERAL_GRID ),pointer :: pSoilGroupGrid     ! pointer to model grid

  if( len_trim( pConfig%sSoilGroup_PROJ4 ) > 0 ) then

    call echolog(" ")
    call echolog("Transforming gridded data in file: "//dquote(pSoilGroupGrid%sFilename) )
    call echolog("  FROM: "//squote(pConfig%sSoilGroup_PROJ4) )
    call echolog("  TO:   "//squote(pConfig%sBase_PROJ4) )

    call grid_Transform(pGrd=pSoilGroupGrid, &
                        sFromPROJ4=pConfig%sSoilGroup_PROJ4, &
                        sToPROJ4=pConfig%sBase_PROJ4 )

    call Assert( grid_CompletelyCover( pGrd, pSoilGroupGrid ), &
      "Transformed grid doesn't completely cover your model domain.")

    call grid_gridToGrid(pGrdFrom=pSoilGroupGrid,&
                            iArrayFrom=pSoilGroupGrid%iData, &
                            pGrdTo=pGrd, &
                            iArrayTo=pGrd%Cells%iSoilGroup )

  else

    call Assert( grid_Conform( pGrd, pSoilGroupGrid ), &
                      "Non-conforming HYDROLOGIC SOILS grid. Filename: " &
                      //dquote(pSoilGroupGrid%sFilename) )
    pGrd%Cells%iSoilGroup = pSoilGroupGrid%iData

  endif

  call grid_Destroy(pSoilGroupGrid)

  !********************************************************
  !*** HACK ALERT !!!
  !********************************************************
  where (pGrd%Cells%iSoilGroup <= 0) pGrd%Cells%iSoilGroup = 4

  pGenericGrd_int%iData = pGrd%Cells%iSoilGroup

  call grid_WriteGrid(sFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Soil_Group" // &
    "."//trim(pConfig%sOutputFileSuffix), pGrd=pGenericGrd_int, pConfig=pConfig )

end subroutine model_PopulateSoilGroupArray

!--------------------------------------------------------------------------

subroutine model_PopulateFlowDirectionArray(pConfig, pGrd, pFlowDirGrid)

  ! [ ARGUMENTS ]
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings

  type ( T_GENERAL_GRID ),pointer :: pGrd            ! pointer to model grid
  type ( T_GENERAL_GRID ),pointer :: pFlowDirGrid     ! pointer to model grid

  if( len_trim( pConfig%sFlowDir_PROJ4 ) > 0 ) then

    call echolog(" ")
    call echolog("Transforming gridded data in file: "//dquote(pFlowDirGrid%sFilename) )
    call echolog("  FROM: "//squote(pConfig%sFlowDir_PROJ4) )
    call echolog("  TO:   "//squote(pConfig%sBase_PROJ4) )

    call grid_Transform(pGrd=pFlowDirGrid, &
                        sFromPROJ4=pConfig%sFlowDir_PROJ4, &
                        sToPROJ4=pConfig%sBase_PROJ4 )

    call Assert( grid_CompletelyCover( pGrd, pFlowDirGrid ), &
      "Transformed grid doesn't completely cover your model domain.")

    call grid_gridToGrid(pGrdFrom=pFlowDirGrid,&
                            iArrayFrom=pFlowDirGrid%iData, &
                            pGrdTo=pGrd, &
                            iArrayTo=pGrd%Cells%iFlowDir )

  else

    call Assert( grid_Conform( pGrd, pFlowDirGrid ), &
                      "Non-conforming FLOW_DIRECTION grid. Filename: " &
                      //dquote(pFlowDirGrid%sFilename) )
    pGrd%Cells%iFlowDir = pFlowDirGrid%iData

  endif

  call grid_Destroy(pFlowDirGrid)

  pGenericGrd_int%iData = pGrd%Cells%iFlowDir
  call grid_WriteGrid(sFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_D8_Flow_Direction" // &
    "."//trim(pConfig%sOutputFileSuffix), pGrd=pGenericGrd_int, pConfig=pConfig )

end subroutine model_PopulateFlowDirectionArray

!--------------------------------------------------------------------------

function rf_model_GetInterception( pConfig, iType, iDayOfYear ) result(rIntRate)
  !! Looks up the interception value for land-use type iType.
  ! [ ARGUMENTS ]
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  integer (kind=T_INT), intent(in) :: iType
  integer (kind=T_INT), intent(in) :: iDayOfYear
  ! [ RETURN VALUE ]
  real (kind=T_SGL) :: rIntRate
  ! [ LOCALS ]
  integer ( kind=T_INT ) :: i
  type ( T_LANDUSE_LOOKUP ),pointer :: pLU

  ! Default is zero
  rIntRate = rZERO
  ! Search the default values
  do i = 1,size(pConfig%LU,1)
    pLU => pConfig%LU(i)
    if ( pLU%iLandUseType == iType ) then
      if ( lf_model_GrowingSeason(pConfig, iDayOfYear) ) then
        rIntRate = pLU%rIntercept_GrowingSeason
      else
        rIntRate = pLU%rIntercept_NonGrowingSeason
      end if
      exit
    end if
  end do

  call Assert(LOGICAL(rIntRate >= rZERO,kind=T_LOGICAL), &
    "Negative value was determined for interception. Check your lookup tables.")

  return
end function rf_model_GetInterception

!--------------------------------------------------------------------------

subroutine model_InitializeLanduseRelatedParams( pGrd, pConfig )

  type ( T_GENERAL_GRID ),pointer :: pGrd          ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings

  write(UNIT=LU_LOG,FMT=*) "model.f95: calling model_InitializeSM"
  flush(unit=LU_LOG)
  call model_InitializeSM(pGrd, pConfig)

  write(UNIT=LU_LOG,FMT=*)  "model.f95: runoff_InitializeCurveNumber"
  flush(unit=LU_LOG)
  call runoff_InitializeCurveNumber( pGrd ,pConfig)

  write(UNIT=LU_LOG,FMT=*)  "model.f95: model_InitialMaxInfil"
  flush(unit=LU_LOG)
  call model_InitializeMaxInfil(pGrd, pConfig )


end subroutine model_InitializeLanduseRelatedParams

!--------------------------------------------------------------------------

subroutine model_InitializeET( pGrd, pConfig )
  !! Depending on the ET model in use, initializes the values for ET
  !! calculations.
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd          ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings


  write(UNIT=LU_LOG,FMT=*)  "model_InitializeET : "
  write(UNIT=LU_LOG,FMT=*)  "  filename = ", TRIM(pConfig%sTimeSeriesFilename)

  select case ( pConfig%iConfigureET )
    case ( CONFIG_ET_NONE )
      call Assert( .false._T_LOGICAL, "No ET configuration was specified" )
    case ( CONFIG_ET_THORNTHWAITE_MATHER )
      call et_tm_initialize ( pGrd, pConfig, pConfig%sTimeSeriesFilename)
    case ( CONFIG_ET_TURC )
      call et_turc_initialize ( pGrd, pConfig%sTimeSeriesFilename)
    case ( CONFIG_ET_JENSEN_HAISE )
      call et_jh_initialize ( pGrd, pConfig%sTimeSeriesFilename)
    case ( CONFIG_ET_BLANEY_CRIDDLE )
      call et_bc_initialize ( pGrd, pConfig%sTimeSeriesFilename)
    case ( CONFIG_ET_HARGREAVES )

  end select

end subroutine model_InitializeET

!--------------------------------------------------------------------------

subroutine model_ProcessET( pGrd, pConfig, iDayOfYear, iNumDaysInYear, &
  rRH, rMinRH, rWindSpd, rSunPct )
!! Depending on the ET model in use, computes the potential ET for each
!! cell, based on the meteorological data given. Stores cell-by-cell PET
!! values in the model grid.
!!
! [ ARGUMENTS ]
type ( T_GENERAL_GRID ),pointer :: pGrd          ! pointer to model grid
type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
  ! model options, flags, and other settings
integer (kind=T_INT),intent(in) :: iDayOfYear, iNumDaysInYear
real (kind=T_SGL),intent(in) :: rRH,rMinRH,rWindSpd,rSunPct

  ! [ LOCALS ]
  type (T_CELL),pointer :: cel                      ! pointer to a particular cell
  integer (kind=T_INT) :: iCol, iRow
#ifdef IRRIGATION_MODULE
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
#endif

  select case ( pConfig%iConfigureET )
    case ( CONFIG_ET_NONE )
      call Assert( .false._T_LOGICAL, "No ET configuration was specified" )
    case ( CONFIG_ET_THORNTHWAITE_MATHER )
      call et_tm_ComputeET ( pGrd, pConfig, iDayOfYear, rRH, rMinRH, &
        rWindSpd, rSunPct)
    case ( CONFIG_ET_TURC )
      call et_turc_ComputeET ( pGrd, iDayOfYear, rRH, rSunPct)
    case ( CONFIG_ET_JENSEN_HAISE )
      call et_jh_ComputeET ( pGrd, iDayOfYear, rRH, rMinRH, &
        rWindSpd, rSunPct)
    case ( CONFIG_ET_BLANEY_CRIDDLE )
      call et_bc_ComputeET ( pGrd, iDayOfYear, rRH, rMinRH, &
        rWindSpd, rSunPct)
    case ( CONFIG_ET_HARGREAVES )
      call et_hargreaves_ComputeET ( pGrd, pConfig, iDayOfYear, iNumDaysInYear)
  end select

#ifdef IRRIGATION_MODULE

  ! modify potential ET by the crop coefficient
  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX  ! last index in a Fortan array should be the slowest changing
      cel => pGrd%Cells(iCol,iRow)
      pIRRIGATION => pConfig%IRRIGATION(cel%iLandUseIndex)

  cel%rSM_PotentialET = cel%rSM_PotentialET &
    * calc_crop_coefficient(pIRRIGATION%rKc_Max, pIRRIGATION%rK0, &
      pIRRIGATION%rGDD_Kc_Max, pIRRIGATION%rGDD_Death, pIRRIGATION%rAlpha1, cel%rGDD)

  enddo
enddo

#endif

! if the ground is still frozen, we're not going to consider ET to be
! possible.
where (pGrd%Cells%rCFGI > rNEAR_ZERO)
  pGrd%Cells%rSM_PotentialET = rZERO
endwhere


!  call stats_WriteMinMeanMax( LU_STD_OUT, "POTENTIAL ET", pGrd%Cells%rSM_PotentialET )

  return
end subroutine model_ProcessET

!--------------------------------------------------------------------------

subroutine model_ProcessSM( pGrd, pConfig, iDayOfYear, iDay, iMonth, iYear)
  !! Depending on the SM model in use, computes the change in soil moisture
  !! and also the recharge (if any) for each cell in the grid, given the
  !! precipitation rPrecip and the snow melt rSnowMelt
  !!
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd         ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings

  integer (kind=T_INT),intent(in) :: iDayOfYear
  integer (kind=T_INT),intent(in) :: iDay
  integer (kind=T_INT),intent(in) :: iMonth
  integer (kind=T_INT),intent(in) :: iYear

  select case ( pConfig%iConfigureSM )
    case ( CONFIG_SM_NONE )
      call Assert( .false._T_LOGICAL, "No soil moisture calculation method was specified" )
    case ( CONFIG_SM_THORNTHWAITE_MATHER )
      call sm_thornthwaite_mather_UpdateSM (pGrd, pConfig, &
        iDayOfYear, iDay, iMonth,iYear)
  end select

  return
end subroutine model_ProcessSM

!--------------------------------------------------------------------------

subroutine model_InitializeSM(pGrd, pConfig )
  !! Depending on the SM model in use, computes the change in soil moisture
  !! and also the recharge (if any) for each cell in the grid, given the
  !! precipitation rPrecip and the snow melt rSnowMelt
  !!
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd         ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  ! [ LOCALS ]
  integer (kind=T_INT) :: iCol,iRow,k
  type ( T_CELL ),pointer :: cel            ! pointer to cell data structure
  type ( T_LANDUSE_LOOKUP ),pointer :: pLU  ! pointer to landuse data structure
  logical ( kind=T_LOGICAL ) :: lMatch

  ! [ LOCAL PARAMETERS ]

  if(pConfig%iConfigureSMCapacity==CONFIG_SM_CAPACITY_CALCULATE) then
    ! Update the soil-water capacity based on land-cover and soil type
    do iRow=1,pGrd%iNY
      do iCol=1,pGrd%iNX

        lMatch = lFALSE
        cel => pGrd%Cells(iCol,iRow)
        ! loop over all LAND USE types...
        do k=1,size(pConfig%LU,1)
          pLU => pConfig%LU(k)
          if ( pLU%iLandUseType == cel%iLandUse ) then

            cel%rSoilWaterCap = cel%rSoilWaterCapInput * &
               pConfig%ROOTING_DEPTH(k,cel%iSoilGroup)
            lMatch=lTRUE
            exit
          end if
        end do

        if(.not. lMATCH) then
          call Assert(lFALSE,&
            "Failed to match landuse grid with landuse table during soil moisture initialization~" &
            //" Row: "//trim(int2char(iRow))//"  Col: "//trim(int2char(iCol)) &
            //"  cell LU: "//trim(int2char(int(cel%iLandUse, kind=T_INT) ) ) )
        endif
      end do
    end do
  end if

  select case ( pConfig%iConfigureSM )
    case ( CONFIG_SM_NONE )
      call Assert( lFALSE, "No soil moisture calculation method was specified" )
    case ( CONFIG_SM_THORNTHWAITE_MATHER )
      call sm_thornthwaite_mather_Initialize ( pGrd, pConfig )
  end select

  return
end subroutine model_InitializeSM

!--------------------------------------------------------------------------

subroutine model_InitializeMaxInfil(pGrd, pConfig )
  !!
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd         ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  ! [ LOCALS ]
  integer (kind=T_INT) :: iCol,iRow,k
  type ( T_CELL ),pointer :: cel            ! pointer to cell data structure
  type ( T_LANDUSE_LOOKUP ),pointer :: pLU  ! pointer to landuse data structure
  logical ( kind=T_LOGICAL ) :: lMatch

  ! Update the MAXIMUM RECHARGE RATE based on land-cover and soil type
  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX

      lMatch = lFALSE
      cel => pGrd%Cells(iCol,iRow)
      do k=1,size(pConfig%LU,1)
        pLU => pConfig%LU(k)
        if ( pLU%iLandUseType == cel%iLandUse ) then
          ! save index of matching landuse for ease of processing land use properties later
          cel%iLandUseIndex = k
          ! need to ensure that the soil type doesn't exceed
          ! the max number of soil types or we get a core dump
          call Assert(INT(cel%iSoilGroup, kind=T_INT) &
            <= size(pConfig%MAX_RECHARGE,2), &
            "Value in soil type grid exceeds the maximum " &
            // "number of soil types in the land use lookup table.")
          cel%rMaxRecharge = pConfig%MAX_RECHARGE(k,INT(cel%iSoilGroup,kind=T_INT))
          lMatch=lTRUE
          exit
        end if
      end do
      if(.not. lMATCH) then
        write(UNIT=LU_LOG,FMT=*) "iRow: ",iRow,"  iCol: ",iCol,"  cell LU: ", cel%iLandUse
        call Assert(lFALSE,&
          "Failed to match landuse grid with landuse table during maximum infiltration initialization")
      endif
    end do
  end do

end subroutine model_InitializeMaxInfil

!--------------------------------------------------------------------------

subroutine model_WriteGrids(pGrd, pConfig, iOutputType)
!! Writes the monthly output arrays in the proper grid format
! [ ARGUMENTS ]
type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
  ! model options, flags, and other settings
integer (kind=T_INT), intent(in) :: iOutputType

  ! [ LOCALS ]
  real (kind=T_DBL) :: xmin,xmax,ymin,ymax
  character (len=256) sBufOut,sBufFuture,sBufSuffix,sDayText,sMonthText, &
    sYearText

  sBufOut = trim(pConfig%sOutputFilePrefix)//trim( YEAR_INFO(pConfig%iMonth)%sName )
  sBufFuture = trim(pConfig%sFutureFilePrefix)
  sBufSuffix = trim(pConfig%sOutputFileSuffix)

  write(sDayText,fmt="(a1,i2.2,a1,i2.2,a1,i4)") "_",pConfig%iMonth,"_",pConfig%iDay,"_",pConfig%iYear
  write(sMonthText,fmt="(a1,i2.2,a1,i4)") "_",pConfig%iMonth,"_",pConfig%iYear
  write(sYearText,fmt="(a1,i4)") "_",pConfig%iYear

  xmin = pGrd%rX0
  xmax = pGrd%rX1
  ymin = pGrd%rY0
  ymax = pGrd%rY1

   if(MAXVAL(pGrd%Cells%rMSB) > 0.1 .or. MINVAL(pGrd%Cells%rMSB) < -0.1) then

     pGenericGrd_sgl%rData = pGrd%Cells%rMSB
     call grid_WriteGrid( &
       sFilename="MASS_BALANCE"//trim(sDayText)//"."//trim(sBufSuffix), &
       pGrd=pGenericGrd_sgl, pConfig=pConfig)

   elseif ( iOutputType == WRITE_ASCII_GRID_ANNUAL ) then

     pGenericGrd_sgl%rData = pGrd%Cells%rSoilMoisturePct
     call grid_WriteGrid(sFilename=trim(sBufFuture) // "final_pct_sm" // &
     trim(sYearText) // "." //trim(sBufSuffix), &
       pGrd=pGenericGrd_sgl, pConfig=pConfig)

     pGenericGrd_sgl%rData = pGrd%Cells%rSnowCover
     call grid_WriteGrid(sFilename=trim(sBufFuture) // "final_snow_cover" // &
       trim(sYearText) // "." //trim(sBufSuffix), &
       pGrd=pGenericGrd_sgl, pConfig=pConfig )

   elseif ( iOutputType == WRITE_ASCII_GRID_DAILY ) then

   elseif ( iOutputType == WRITE_ASCII_GRID_MONTHLY ) then

   elseif ( iOutputType == WRITE_ASCII_GRID_DIAGNOSTIC ) then

   elseif ( iOutputType == WRITE_ASCII_GRID_DEBUG ) then

   end if

end subroutine model_WriteGrids

! read a single line from the time-series file and return a pointer to the values
subroutine model_ReadTimeSeriesFile(pTS)

  type (T_TIME_SERIES_FILE), pointer :: pTS

  ! [ LOCALS ]
  character(len=256) :: sBuf
  integer (kind=T_INT) :: iStat
  real (kind=T_SGL) :: rTemp

  do

    read ( unit=LU_TS, fmt="(a256)", iostat=iStat ) sBuf
#ifdef DEBUG_PRINT
    print *, trim(sBuf)
#endif
    if ( iStat<0 ) then
      pTS%lEOF = lTRUE
      exit ! END OF FILE
    end if

    call Assert ( iStat == 0, &
      "Cannot read record from time-series file", TRIM(__FILE__),__LINE__)

    if ( sBuf(1:1) == '#' ) cycle      ! Ignore comment statements

    call CleanUpCsv ( sBuf )

    read ( unit=sBuf, fmt=*, iostat=iStat ) pTS%iMonth, pTS%iDay, &
      pTS%iYear, pTS%rAvgT, pTS%rPrecip, pTS%rRH, pTS%rMaxT, pTS%rMinT, &
      pTS%rWindSpd, pTS%rMinRH, pTS%rSunPct

    ! on read error, skip onto the next line
    if (iStat/=0) then
      write(UNIT=LU_LOG,FMT=*) "Skipping: ",trim(sBuf)
      write(UNIT=LU_LOG,FMT=*)
      cycle
    end if

    ! check for ridiculous PRECIP values; halt if such values are found
    if( pTS%rPrecip < 0.) then
      call echolog("Missing or corrupt precipitation data in climate file. " &
        //"~Offending line: "//TRIM(sBuf) )
      call Assert(lFALSE, "")
    end if

    ! check for ridiculous TMAX values; halt if such values are found
    if(pTS%rMaxT < -100 .or. pTS%rMaxT > 150. ) then
      call echolog("Missing or corrupt maximum air value (TMAX) in climate file. " &
        //"~Offending line: "//TRIM(sBuf) )
      call Assert(lFALSE, "")
    end if

    ! check for ridiculous values; halt if such values are found
    if(pTS%rMinT > 120. .or. pTS%rMinT < -100. ) then
        call echolog("Missing or corrupt minimum air value (TMIN) in climate file. " &
          //"~Offending line: "//TRIM(sBuf) )
      call Assert(lFALSE, "")
    end if

    ! if MAXIMUM temperature is less than the MINIMUM, assume that they
    ! were inadvertently swapped; swap them back and keep running
    if(pTS%rMaxT < pTS%rMinT) then
      rTemp = pTS%rMinT
      pTS%rMinT = pTS%rMaxT
      pTS%rMaxT = rTemp
      call echolog(repeat("*", 40)//"~ALERT: TMAX value is less than TMIN. Swapping values. " &
        //"~Offending line: "//TRIM(sBuf)//"~"//repeat("*", 40) )
    endif

    exit

  end do

end subroutine model_ReadTimeSeriesFile

end module model
