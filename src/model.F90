!> @file
!> Contains a single module, @ref model, which keeps track of the model date and executes
!>  necessary process modules.

!> Allocates memory to store intermediate and final calculation results,
!> keeps track of the model date, reads tabular climate data, and calls the
!> necessary process modules in turn.
module model

  use iso_c_binding, only : c_short, c_int, c_float, c_double, c_long_long
  use types
  use data_factory
  use datetime
  use swb_grid
  use stats
  use runoff_curve_number
  use et_crop_coefficients
  use et_thornthwaite_mather
  use et_turc
  use et_hargreaves
  use et_jensen_haise
  use et_blaney_criddle
  use irrigation
  use netcdf4_support
  use sm_thornthwaite_mather
  use water_balance

   implicit none

  !! Counter for moving average water inputs
  integer (kind=c_int) :: iDayCtr

  !! Generic grids used to shuffle data between subroutines
  type ( T_GENERAL_GRID ),pointer :: pGenericGrd_int
  type ( T_GENERAL_GRID ),pointer :: pGenericGrd_sgl

  !! For the "downhill" solution
  integer (kind=c_int) :: iOrderCount
  integer (kind=c_int), dimension(:), allocatable :: iOrderCol
  integer (kind=c_int), dimension(:), allocatable :: iOrderRow
  real(kind=c_float) :: rStartTime,rEndTime

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

subroutine model_Solve( pGrd, pConfig, pGraph)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd               ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig      ! pointer to data structure that contains
    ! model options, flags, and other settings

  type (T_GRAPH_CONFIGURATION), dimension(:), pointer :: pGraph
    ! pointer to data structure that holds parameters for creating
    ! DISLIN plots

  ! [ LOCALS ]
  integer (kind=c_int) :: i, j, k, iStat, iDayOfYear, iMonth
  integer (kind=c_int) :: tj, ti
  integer (kind=c_int) :: iTempDay, iTempMonth, iTempYear
  integer (kind=c_long_long) :: iPos
  integer (kind=c_int) :: iIndex
  integer (kind=c_int) :: jj, ii, iNChange, iUpstreamCount, iPasses, iTempval
  integer (kind=c_int) :: iCol, iRow
  character(len=3) :: sMonthName
  logical (kind=c_bool) :: lMonthEnd

  real (kind=c_float) :: rmin,ravg,rmax

  type (T_CELL),pointer :: cel
  character (len=256) :: sBuf

  type (T_TIME_SERIES_FILE), pointer :: pTS

  ! allocate memory for the time-series data pointer
  ALLOCATE (pTS, STAT=iStat)
  call Assert( iStat == 0, &
    "Could not allocate memory for time-series data structure", &
    TRIM(__FILE__),__LINE__)

  pConfig%iNumDaysInYear = num_days_in_year(pConfig%iYear)

  FIRST_YEAR_pt_1: if(pConfig%lFirstYearOfSimulation) then

    pGenericGrd_int => grid_Create ( pGrd%iNX, pGrd%iNY, pGrd%rX0, pGrd%rY0, &
       pGrd%rX1, pGrd%rY1, DATATYPE_INT )

    pGenericGrd_sgl => grid_Create ( pGrd%iNX, pGrd%iNY, pGrd%rX0, pGrd%rY0, &
       pGrd%rX1, pGrd%rY1, DATATYPE_REAL )

    ! perform some basic sanity checking on the specified model options
    call model_CheckConfigurationSettings( pGrd, pConfig )

  endif FIRST_YEAR_pt_1

  ! close any existing open time-series files...
  close(LU_TS)

  if( .not. pConfig%lGriddedData ) then
  ! Connect to the single-site time-series file
    open ( LU_TS, file=pConfig%sTimeSeriesFilename, &
      status="OLD", action='READ', iostat=iStat )
    write(UNIT=LU_LOG,FMT=*)  "Opening time series file: ", &
      TRIM(pConfig%sTimeSeriesFilename)
    flush(LU_LOG)
    call Assert ( iStat == 0, "Can't open time-series data file" )
!    pConfig%iCurrentJulianDay = pConfig%iCurrentJulianDay + 1
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

  ! if we are not using gridded climate data, here is where we read in
  ! the current days' values from the single-site time series file.
  if(.not. pConfig%lGriddedData) then

    call model_ReadTimeSeriesFile(pConfig, pTS)
    if(pTS%lEOF) then
      close(unit=LU_TS)
      exit MAIN_LOOP
    end if

  end if

  call LookupMonth(pConfig%iMonth,pConfig%iDay,pConfig%iYear, &
    pConfig%iDayOfYear,sMonthName,lMonthEnd)

  ! initialize (or re-initialize) landuse-associated variables;
  ! must be done whenever a new landuse grid is provided

  call DAT(LANDUSE_DATA)%getvalues( pGrdBase=pGrd, iMonth=pConfig%iMonth, &
    iDay=pConfig%iDay, iYear=pConfig%iYear )

  ! after the first call to getvalues, allow for landuse files to be missing.
  ! if files are missing, the existing landuse values are retained.
  DAT(LANDUSE_DATA)%lMissingFilesAreAllowed = lTRUE

  ! if a new landuse grid is found, then we need to perform a basic reinitialization
  ! of active/inactive cells, total soil water capacity, etc.
  if ( DAT(LANDUSE_DATA)%lGridHasChanged ) then

    do

      ! if user has supplied a constant landuse data value, we don't want to
      ! continue to update data structures and inactivate cells, etc.
      if (      DAT(LANDUSE_DATA)%iSourceDataForm == CONSTANT_VALUE     &
          .and. DAT(LANDUSE_DATA)%iNumberOfGetCalls > 1 ) exit

      pGrd%Cells%iLandUse = pGrd%iData
      DAT(LANDUSE_DATA)%lGridHasChanged = lFALSE

      pGenericGrd_int%iData = pGrd%Cells%iLandUse
      pGenericGrd_int%iMask = pGrd%iMask

      call grid_WriteGrid(sFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Landuse_Grid_" // &
        trim(asCharacter(pConfig%iYear))//"_"//trim(asCharacter(pConfig%iMonth)) &
        //"_"//trim(asCharacter(pConfig%iYear))// &
        "."//trim(pConfig%sOutputFileSuffix), pGrd=pGenericGrd_int, iOutputFormat=pConfig%iOutputFormat )

      call make_shaded_contour(pGrd=pGenericGrd_int, &
        sOutputFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Landuse_Grid_" // &
        trim(asCharacter(pConfig%iYear))//"_"//trim(asCharacter(pConfig%iMonth)) &
        //"_"//trim(asCharacter(pConfig%iYear))//".png", &
        sTitleTxt="Landuse Grid", &
        sAxisTxt="Landuse Code",  &
        rMinZVal=0.0 )

      if (pConfig%lFirstYearOfSimulation) then
        ! read in flow direction, soil group, and AWC grids
        call model_InitializeDataStructures( pGrd, pConfig )
      endif

      call model_setInactiveCells( pGrd, pConfig )

      ! (Re)-initialize the model
      ! calls the following:
      ! 1) create landuse index values
      ! 2) calculate soil moisture
      ! 3) calculate runoff curve numbers
      call model_InitializeLanduseRelatedParams( pGrd, pConfig )
      call sm_thornthwaite_mather_UpdatePctSM( pGrd )
      call model_InitializeMaxRecharge(pGrd, pConfig )

      if (pConfig%iConfigureFAO56 /= CONFIG_FAO56_NONE ) then
        write(UNIT=LU_LOG,FMT=*)  "model.F90: reinitializing irrigation table " &
          //"-- calling model_CreateIrrigationTableIndex"
        call model_CreateIrrigationTableIndex(pGrd, pConfig )
      endif

      write(UNIT=LU_LOG,FMT=*)  "model.F90: model_InitializeET"
      flush(unit=LU_LOG)
      call model_InitializeET( pGrd, pConfig )

      exit

    enddo

  endif

  FIRST_YEAR_pt_2: if(pConfig%lFirstYearOfSimulation) then

!
!    %% This block should be superfluous, as CreateIrrigationTableIndex should be called
!       above after a new landuse grid is read in
!
!     if (pConfig%iConfigureFAO56 /= CONFIG_FAO56_NONE .and. &
!        ( pConfig%iConfigureLanduse == CONFIG_LANDUSE_STATIC_GRID &
!         .or. pConfig%iConfigureLanduse == CONFIG_LANDUSE_CONSTANT) ) then
!       call model_CreateIrrigationTableIndex(pGrd, pConfig )
!     endif

    ! initialize binary and stats output files
    call model_InitializeInputAndOutput( pGrd, pConfig )

   ! Are we solving using the downhill algorithm?
    if ( pConfig%iConfigureRunoffMode == CONFIG_RUNOFF_DOWNHILL ) then
      ! if a routing table exists, read it in; else initialize and
      ! save the routing table for future use
      write(UNIT=LU_LOG,FMT=*) "Configuring runoff for the downhill flow routing option..."
      call model_ConfigureRunoffDownhill( pGrd, pConfig)
    end if

    ! Unless we are *not* routing water, we *must* call InitializeFlowDirection
    if( pConfig%iConfigureRunoffMode /= CONFIG_RUNOFF_NO_ROUTING) then
      write(UNIT=LU_LOG,FMT=*) "Initializing flow direction..."
      call model_InitializeFlowDirection( pGrd , pConfig)
    end if

    pConfig%lFirstYearOfSimulation = lFALSE

  end if FIRST_YEAR_pt_2

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
!  call model_GetDailyPrecipValue(pGrd, pConfig, pTS%rPrecip, &
!    pConfig%iMonth, pConfig%iDay, pConfig%iYear, pConfig%iCurrentJulianDay)

  ! Initialize temperature values for current day
!  call model_GetDailyTemperatureValue(pGrd, pConfig, &
!    pTS%rAvgT, pTS%rMinT, pTS%rMaxT, pTS%rRH, &
!    pConfig%iMonth, pConfig%iDay, pConfig%iYear, pConfig%iCurrentJulianDay)

  call model_GetDailyPrecipAndTemperatureValue(pGrd, pConfig, pTS%rPrecip, &
    pTS%rAvgT, pTS%rMinT, pTS%rMaxT, pConfig%iMonth, pConfig%iDay, &
    pConfig%iYear, pConfig%iCurrentJulianDay)

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

  call model_UpdateGrowingSeason( pGrd, pConfig )

  call model_ProcessET( pGrd, pConfig, pConfig%iDayOfYear, &
    pConfig%iNumDaysInYear, pTS%rRH, pTS%rMinRH, &
    pTS%rWindSpd, pTS%rSunPct )

  call model_ProcessRain(pGrd, pConfig, pConfig%iDayOfYear, pConfig%iMonth)

  ! call model_ProcessET( pGrd, pConfig, pConfig%iDayOfYear, &
  !   pConfig%iNumDaysInYear, pTS%rRH, pTS%rMinRH, &
  !   pTS%rWindSpd, pTS%rSunPct )

  if(pConfig%iConfigureFAO56 /= CONFIG_FAO56_NONE ) then
    call model_UpdateGrowingDegreeDay( pGrd , pConfig)
    call et_kc_ApplyCropCoefficients(pGrd, pConfig)
  endif

  if( pConfig%lEnableIrrigation )  call irrigation_UpdateAmounts(pGrd, pConfig)

  call model_ProcessRunoff(pGrd, pConfig, pConfig%iDayOfYear, pConfig%iMonth)

  call calculate_water_balance( pGrd, pConfig, pConfig%iDayOfYear, &
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

  call model_WriteGrids(pGrd=pGrd, pConfig=pConfig, iOutputType=WRITE_ASCII_GRID_DAILY)

  call model_dumpvals( pGrd=pGrd, pConfig=pConfig )

  ! Write the results at each month-end
  if ( lMonthEnd ) then

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

  call MODEL_SIM%addDay()
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

  do iIndex=1,iNUM_VARIABLES

    ! write the end date of the simulation (up to this point) into the header of
    ! the binary file (*.bin)
    if(STAT_INFO(iIndex)%iDailyOutput > iNONE &
      .or. STAT_INFO(iIndex)%iMonthlyOutput > iNONE &
      .or. STAT_INFO(iIndex)%iAnnualOutput > iNONE)  then

      inquire(UNIT=STAT_INFO(iIndex)%iLU,POS=iPos)  ! establish location to return to

      write(UNIT=STAT_INFO(iIndex)%iLU,POS=iENDDATE_POS) &
        pConfig%iMonth,pConfig%iDay, pConfig%iYear

      write(UNIT=STAT_INFO(iIndex)%iLU,POS=iPos ) ! return to prior location in bin file

    end if

  end do

  ! update current date so all is well when next years file is opened
  pConfig%iMonth = iTempMonth
  pConfig%iDay = iTempDay
  pConfig%iCurrentJulianDay = pConfig%iCurrentJulianDay + 1


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
  integer (kind=c_int) :: iIndex

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
    call stats_RewriteGrids(pGrd, pConfig, pGraph)

  ! destroy model grid to free up memory
  call grid_Destroy(pGrd)
  call grid_Destroy(pGenericGrd_int)
  call grid_Destroy(pGenericGrd_sgl)

  do iIndex=1,size(DAT, 1)
    if (DAT(iIndex)%iNC_ARCHIVE_STATUS == NETCDF_FILE_OPEN ) then
      call netcdf_close_file(NCFILE=DAT(iIndex)%NCFILE_ARCHIVE)
      call netcdf_deallocate_data_struct(NCFILE=DAT(iIndex)%NCFILE_ARCHIVE)
    endif
  enddo

  ! how long did all this take, anyway?
  call cpu_time(rEndTime)
  print "(//1x,'SWB run completed in: ',f10.2,' minutes')", &
    (rEndTime - rStartTime) / 60.0_c_float
  write(unit=LU_LOG,fmt="(//1x,'SWB run completed in: ',f10.2, ' minutes')"), &
    (rEndTime - rStartTime) / 60.0_c_float

end subroutine model_EndOfRun

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
  real (kind=c_float), intent(in) :: rPrecip
  integer (kind=c_int), intent(in) :: iMonth
  integer (kind=c_int), intent(in) :: iDay
  integer (kind=c_int), intent(in) :: iYear
  integer (kind=c_int), intent(in) :: iJulianDay
  ! [ LOCALS ]
  real (kind=c_double) :: rMin, rMean, rMax, rSum
  integer (kind=c_int) :: iCount, iNegCount
  character (len=256) sBuf


  call DAT(PRECIP_DATA)%set_constant(rPrecip)

  call DAT(PRECIP_DATA)%getvalues( pGrdBase=pGrd, &
      iMonth=iMonth, iDay=iDay, iYear=iYear, &
      iJulianDay=iJulianDay, &
      rValues=pGrd%Cells%rGrossPrecip)

  write(sBuf, fmt="(i4.4,'_',i2.2,'_',i2.2)") iYear, iMonth, iDay

  iNegCount = COUNT(pGrd%Cells%rGrossPrecip < pConfig%rMinValidPrecip &
                .and. pGrd%iMask == iACTIVE_CELL )

  ! convert values less than the minimum valid amount to zero
  where (pGrd%Cells%rGrossPrecip < pConfig%rMinValidPrecip)
    pGrd%Cells%rGrossPrecip = rZERO
  end where

  rMin = minval(pGrd%Cells%rGrossPrecip, pGrd%iMask == iACTIVE_CELL )
  rMax = maxval(pGrd%Cells%rGrossPrecip, pGrd%iMask == iACTIVE_CELL)
  rSum = sum(pGrd%Cells%rGrossPrecip, pGrd%iMask == iACTIVE_CELL)
  iCount = count( pGrd%iMask == iACTIVE_CELL )

  call assert(iCount > 0, "Cannot continue -- there are no active cells", &
    __FILE__, __LINE__)
  ! We are ignoring any missing or bogus values in this calculation
  rMean = rSum / iCount

  if(pConfig%lHaltIfMissingClimateData) then
    call Assert(rMin >= pConfig%rMinValidPrecip,"Precipitation values less than " &
      //trim(real2char(pConfig%rMinValidPrecip))//" are not allowed. " &
      //"("//trim(int2char(iNegCount))//" cells with values < " &
      //trim(real2char(pConfig%rMinValidPrecip))//")",TRIM(__FILE__),__LINE__)
  elseif(iNegCount > 0) then
    write(sBuf,fmt="(a,i7,1x,a,1x,i2.2,'/',i2.2,'/',i4.4)") "*** ",iNegCount, &
      "Missing PRECIPITATION values detected: ", iMonth, iDay, iYear
    call echolog(sBuf)
    call echolog("  ==> Missing precipitation values will be set to zero")
  endif

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

subroutine model_GetDailyPrecipAndTemperatureValue( pGrd, pConfig, rPrecip, &
    rAvgT, rMinT, rMaxT, iMonth, iDay, iYear, iJulianDay)


  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  real (kind=c_float), intent(in) :: rPrecip
  real (kind=c_float), intent(in) :: rAvgT
  real (kind=c_float), intent(in) :: rMinT
  real (kind=c_float), intent(in) :: rMaxT
  integer (kind=c_int), intent(in) :: iMonth
  integer (kind=c_int), intent(in) :: iDay
  integer (kind=c_int), intent(in) :: iYear
  integer (kind=c_int), intent(in) :: iJulianDay

  ! [ LOCALS ]
  real (kind=c_double) :: rMin, rMean, rMax, rSum, rTempVal
  integer (kind=c_int) :: iNumGridCells
  integer (kind=c_int) :: iRow,iCol, iCount, iCount_valid, iNegCount
  character (len=256) sBuf
  type (T_CELL),pointer :: cel

  iCount = 0

  call DAT(TMAX_DATA)%set_constant(rMaxT)

  call DAT(TMAX_DATA)%getvalues( pGrdBase=pGrd, &
      iMonth=iMonth, iDay=iDay, iYear=iYear, &
      iJulianDay=iJulianDay, rValues=pGrd%Cells%rTMax)

  call DAT(TMIN_DATA)%set_constant(rMinT)
  call DAT(TMIN_DATA)%getvalues( pGrdBase=pGrd, &
      iMonth=iMonth, iDay=iDay, iYear=iYear, &
      iJulianDay=iJulianDay, rValues=pGrd%Cells%rTMin)

  call DAT(PRECIP_DATA)%set_constant(rPrecip)
  call DAT(PRECIP_DATA)%getvalues( pGrdBase=pGrd, &
      iMonth=iMonth, iDay=iDay, iYear=iYear, &
      iJulianDay=iJulianDay, &
      rValues=pGrd%Cells%rGrossPrecip)

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX
      cel=>pGrd%Cells(iCol,iRow)

      if( cel%rTMax < cel%rTMin )then
        ! swap min and max values to maintain a positive delta T
        rTempVal = cel%rTMax
        cel%rTMax = cel%rTMin
        cel%rTMin = cel%rTMax

      end if

    enddo
  enddo

  pGrd%Cells%rTAvg = (pGrd%Cells%rTMax + pGrd%Cells%rTMin) / 2_c_float


  rMin = minval(pGrd%Cells%rGrossPrecip, pGrd%iMask == iACTIVE_CELL )
  rMax = maxval(pGrd%Cells%rGrossPrecip, pGrd%iMask == iACTIVE_CELL)
  rSum = sum(pGrd%Cells%rGrossPrecip, pGrd%iMask == iACTIVE_CELL)
  iCount = count( pGrd%iMask == iACTIVE_CELL )

  iNegCount = COUNT(pGrd%Cells%rGrossPrecip < pConfig%rMinValidPrecip &
                .and. pGrd%iMask == iACTIVE_CELL )

  call assert(iCount > 0, "Cannot continue -- there are no active cells", &
    __FILE__, __LINE__)
  ! We are ignoring any missing or bogus values in this calculation
  rMean = rSum / iCount

  if(pConfig%lHaltIfMissingClimateData) then
    call Assert(rMin >= pConfig%rMinValidPrecip,"Precipitation values less than " &
      //trim(real2char(pConfig%rMinValidPrecip))//" are not allowed. " &
      //"("//trim(int2char(iNegCount))//" cells with values < " &
      //trim(real2char(pConfig%rMinValidPrecip))//")",TRIM(__FILE__),__LINE__)
  elseif(iNegCount > 0) then
    write(sBuf,fmt="(a,i7,1x,a,1x,i2.2,'/',i2.2,'/',i4.4)") "*** ",iNegCount, &
      "Missing PRECIPITATION values detected: ", iMonth, iDay, iYear
    call echolog(sBuf)
    call echolog("  ==> Missing precipitation values will be set to zero")
  endif

  call stats_UpdateAllAccumulatorsByGrid(rMin,rMean,rMax,rSum,iGROSS_PRECIP,iMonth)

end subroutine model_GetDailyPrecipAndTemperatureValue




subroutine model_GetDailyTemperatureValue( pGrd, pConfig, rAvgT, rMinT, &
  rMaxT, rRH, iMonth, iDay, iYear, iJulianDay)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  real (kind=c_float), intent(in) :: rAvgT
  real (kind=c_float), intent(in) :: rMinT
  real (kind=c_float), intent(in) :: rMaxT
  real (kind=c_float), intent(in) :: rRH
  integer (kind=c_int), intent(in) :: iMonth
  integer (kind=c_int), intent(in) :: iDay
  integer (kind=c_int), intent(in) :: iYear
  integer (kind=c_int), intent(in) :: iJulianDay

  ! [ LOCALS ]
  real (kind=c_double) :: rMin, rMean, rMax, rSum, rTFactor, rTempVal, rMeanTMIN, rMeanTMAX
  integer (kind=c_int) :: iNumGridCells
  integer (kind=c_int) :: iRow,iCol, iCount, iCount_valid
  character (len=256) sBuf
  type (T_CELL),pointer :: cel

  iCount = 0

!!!   *$OMP PARALLEL SECTIONS
!!!   *$OMP SECTION
  call DAT(TMAX_DATA)%set_constant(rMaxT)
  call DAT(TMAX_DATA)%getvalues( pGrdBase=pGrd, &
      iMonth=iMonth, iDay=iDay, iYear=iYear, &
      iJulianDay=iJulianDay, rValues=pGrd%Cells%rTMax)
!OMP SECTION
  call DAT(TMIN_DATA)%set_constant(rMinT)
  call DAT(TMIN_DATA)%getvalues( pGrdBase=pGrd, &
      iMonth=iMonth, iDay=iDay, iYear=iYear, &
      iJulianDay=iJulianDay, rValues=pGrd%Cells%rTMin)
!!!   *$OMP END PARALLEL SECTIONS

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

  pGrd%Cells%rTAvg = (pGrd%Cells%rTMax + pGrd%Cells%rTMin) / 2_c_float

  ! Scan through array of inputs looking for instances where the TMIN > TMAX
  ! (THIS CAN BE RE_WRITTEN USING MATRIX NOTATION)

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX
      cel=>pGrd%Cells(iCol,iRow)

      if( cel%rTMax < cel%rTMin )then

        ! swap min and max values to maintain a positive delta T
        rTempVal = cel%rTMax
        cel%rTMax = cel%rTMin
        cel%rTMin = rTempVal

      end if

    end do
  end do

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
  real (kind=c_float) :: A = 0.97             ! decay coefficient
  integer (kind=c_int) :: iCol,iRow               ! temporary array indices
  type (T_CELL),pointer :: cel              ! pointer to a particular cell
  real (kind=c_float) :: rTAvg_C              ! temporary variable holding avg temp in C
  real (kind=c_float) :: rSnowDepthCM         ! snow depth in centimeters

  !!!   *$OMP DO ORDERED PRIVATE(iRow, iCol, cel)

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX
      cel => pGrd%Cells(iCol,iRow)

      if (pGrd%iMask(iCol, iRow) == iINACTIVE_CELL) cycle

      rTAvg_C = FtoC(cel%rTAvg)
      ! assuming snow depth is 10 times the water content of the snow in inches
      rSnowDepthCM = cel%rSnowCover * 10.0_c_float * rCM_PER_INCH

  if(cel%rTAvg > rFREEZING) then
    cel%rCFGI = max(A*cel%rCFGI - &
      (rTAvg_C * exp (-0.4_c_float * 0.5_c_float * rSnowDepthCM)),rZERO)
  else ! temperature is below freezing
    cel%rCFGI = max(A*cel%rCFGI - &
      (rTAvg_C * exp (-0.4_c_float * 0.08_c_float * rSnowDepthCM)),rZERO)
  end if

  end do
end do

  !!!   *$OMP END DO

!  write(UNIT=LU_LOG,FMT=*)  "=========CFGI CALCULATION==========="
!  write(UNIT=LU_STD_OUT,FMT="(A)") &
!      "                                 min          mean           max"
!  call stats_WriteMinMeanMax(LU_STD_OUT,"CFGI" , pGrd%Cells(:,:)%rCFGI )
!
!  write(UNIT=LU_LOG,FMT=*)  "=========CFGI CALCULATION==========="



end subroutine model_UpdateContinuousFrozenGroundIndex


subroutine model_UpdateGrowingSeason( pGrd, pConfig )

  implicit none

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer        :: pGrd
  type (T_MODEL_CONFIGURATION), pointer  :: pConfig

  ! [ LOCALS ]
  type (T_CELL),pointer :: cel              ! pointer to a particular cell
  integer (kind=c_int) :: iCol, iRow



  if (pConfig%iConfigureGrowingSeason == CONFIG_GROWING_SEASON_CONTROL_FILE ) then

    pGrd%Cells%iGrowingSeason = if_model_GrowingSeason( pConfig, pConfig%iDayOfYear )

  else

    do iRow=1,pGrd%iNY
      do iCol=1,pGrd%iNX
        cel => pGrd%Cells(iCol, iRow)

        ! update season-specific GDD
        if (cel%rTMin > pConfig%fGrowingSeasonEnd_KillingFrostTemp ) then
          cel%rGDD_GrowingSeason = cel%rGDD_GrowingSeason   &
              + (cel%rTAvg - pConfig%fGrowingSeasonEnd_KillingFrostTemp )
        else
          cel%rGDD_GrowingSeason = 0.
        endif

        ! is this cell in active growing season?
        if ( cel%iGrowingSeason == iTRUE ) then

          ! check for killing frost. if minimum temp is below killing frost temp, shut down growing season
          if ( cel%rTMin <= pConfig%fGrowingSeasonEnd_KillingFrostTemp ) cel%iGrowingSeason = iFALSE

        else  ! it is NOT currently growing season; should it be?

          if ( cel%rGDD_GrowingSeason > pConfig%fGrowingSeasonStart_Minimum_GDD ) cel%iGrowingSeason = iTRUE

        endif

      enddo
    enddo

  endif

end subroutine model_UpdateGrowingSeason


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
subroutine model_UpdateGrowingDegreeDay( pGrd , pConfig)

  implicit none

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  ! [ LOCALS ]
  real (kind=c_float) :: rDD                        ! daily departure from TBase
  type (T_CELL),pointer :: cel                      ! pointer to a particular cell
  real (kind=c_float) :: rA, rAt
  real (kind=c_float) :: rTMax
  real (kind=c_float) :: rW
  integer (kind=c_int) :: iCol,iRow
  real (kind=c_float) :: rGDD_BaseTemp, rGDD_MaxTemp
  logical (kind=c_bool) :: lAssertTest

  ! zero out growing degree day at start of calendar year
  if(pConfig%iDayOfYear == 1) pGrd%Cells%rGDD = 0.

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX  ! last subscript in a Fortran array should be the slowest-changing
      cel => pGrd%Cells(iCol,iRow)

      if (pGrd%iMask(iCol, iRow) == iINACTIVE_CELL) cycle

      ! cap the maximum value used in GDD calculations on the basis of the value
      ! provided by user...

      lAssertTest = cel%iLandUseIndex >= 1 .and. cel%iLandUseIndex <= pConfig%iNumberOfLanduses

      if(.not. lAssertTest) &
        call assert(lAssertTest, &
        "Array index out of bounds. Variable is iLandUseIndex with a value of " &
        //trim(int2char(cel%iLandUseIndex)), trim(__FILE__),__LINE__)

      rGDD_BaseTemp = pConfig%IRRIGATION(cel%iLandUseIndex)%rGDD_BaseTemp
      rGDD_MaxTemp = pConfig%IRRIGATION(cel%iLandUseIndex)%rGDD_MaxTemp

      rTMax = min(rGDD_MaxTemp, cel%rTMax)

      !> @todo This routine is more complex than it needs to be. Convert to simpler GDD estimation function.
      if(rTMax <= rGDD_BaseTemp) then

        rDD = 0.

      elseif(cel%rTMin >= rGDD_BaseTemp) then

        rDD = cel%rTAvg - rGDD_BaseTemp

      else

        rW = (rTMax - cel%rTMin) / 2.

        rAt = ( rGDD_BaseTemp - cel%rTAvg) / rW

        if(rAt > 1) rAt = 1.
        if(rAt < -1) rAt = -1.

        rA = asin(rAt)

        rDD = (( rW * cos(rA)) - ((rGDD_BaseTemp - cel%rTAvg) &
               * ((dpPI / 2.) - rA))) / dpPI

      end if

      cel%rGDD = cel%rGDD + rDD

    end do

  end do

end subroutine model_UpdateGrowingDegreeDay

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
  integer (kind=c_int),intent(in) :: iDayOfYear  ! Day of the year
  integer (kind=c_int), intent(in) :: iMonth     ! Integer month value (1-12)
  ! [ LOCALS ]
  real (kind=c_double) :: dpPotentialMelt,dpPotentialInterception,dpInterception
  real (kind=c_double) :: dpPreviousSnowCover,dpChgInSnowCover, dpSnowCover
  real (kind=c_double) :: dpNetPrecip    ! all forms of precip, after interception
  real (kind=c_double) :: dpNetRainfall  ! precip as RAINFALL, after interception
  integer (kind=c_int) :: iRow, iCol
  type (T_CELL),pointer :: cel
  integer (kind=c_int) :: iNumGridCells
  real (kind=c_double) :: rMin, rMean, rMax, rSum, rSum2
  integer (kind=c_int) :: iRowCount
  real (kind=c_float) ::  rMonthlySnowRunoff
  logical (kind=c_bool) :: lFREEZING
  real (kind=c_float)   :: rPotential_Evaporated_Interception
  real (kind=c_float)   :: rPrevious_Interception_Storage
  real (kind=c_float)   :: rMAXIMUM_INTERCEPTION_STORAGE
  real (kind=c_float)   :: rFraction_Wet


  ! [ LOCAL PARAMETERS ]
  real (kind=c_float), parameter :: rMELT_INDEX = 1.5_c_float

  ! set snowmelt to zero uniformly across model grid
  pGrd%Cells(:,:)%rSnowMelt = rZERO

  ! set snowfall to zero uniformly across model grid
  pGrd%Cells(:,:)%rSnowFall_SWE = rZERO

  ! calculate number of cells in model grid
  iNumGridCells = pGrd%iNumGridCells

  ! Use "potential interception" for each cell to compute net precip

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX
      cel => pGrd%Cells(iCol,iRow)

      if (pGrd%iMask(iCol, iRow) == iINACTIVE_CELL) then

        dpChgInSnowCover = dpZERO
        dpSnowCover = dpZERO
        dpPreviousSnowCover = dpZERO
        dpNetRainfall = dpZERO

      else

        ! allow for correction factor to be applied to precip gage input data
        if ( cel%rTAvg - (cel%rTMax-cel%rTMin) / 3.0_c_float <= rFREEZING ) then
          lFREEZING = lTRUE
          cel%rGrossPrecip = cel%rGrossPrecip * pConfig%rSnowFall_SWE_Corr_Factor
        else
          lFREEZING = lFALSE
          cel%rGrossPrecip = cel%rGrossPrecip * pConfig%rRainfall_Corr_Factor
        end if

        ! this simply retrieves the table value for the given landuse, with the amount
        ! zeroed out in the event that interception storage is already maxed out
        dpPotentialInterception = max( rf_model_GetInterception(pConfig,cel), 0.0_c_double )   !  &
!                                     - real( cel%rInterceptionStorage, kind=c_double ),  &
!                                       0.0_c_double )

        ! save the current snowcover value, create local copy as well
        dpPreviousSnowCover = real(cel%rSnowCover, kind=c_double)
        dpSnowCover = real(cel%rSnowCover, kind=c_double)

        ! calculate NET PRECIPITATION; assign value of zero if all of the
        ! GROSS PRECIP is captured by the INTERCEPTION process
        dpNetPrecip = real(cel%rGrossPrecip, kind=c_double) - dpPotentialInterception

        if ( dpNetPrecip < dpZERO ) dpNetPrecip = dpZERO

        dpInterception = real(cel%rGrossPrecip, kind=c_double) - dpNetPrecip

        ! negative interception can only be generated if the user has supplied
        ! *negative* values for GROSS PRECIPITATION; this has happened,
        ! mostly due to interpolation schemes that generate pockets
        ! of negative precip values
        if(dpInterception < dpZERO) then

          print *, cel%iLanduse, cel%iSoilGroup
          print *, dpInterception, cel%rGrossPrecip, dpPotentialInterception, dpNetPrecip

          call Assert(lFALSE, &
            "Negative value for interception was calculated on day " &
            //int2char(iDayOfYear)//" iRow: "//trim(int2char(iRow)) &
            //"  iCol: "//trim(int2char(iCol)), &
            trim(__FILE__), __LINE__)
        endif

        cel%rInterception = real(dpInterception, kind=c_double)
        cel%rInterceptionStorage = cel%rInterceptionStorage + cel%rInterception

        rMAXIMUM_INTERCEPTION_STORAGE = pConfig%LU( cel%iLandUseIndex )%rMax_Interception_Storage

        if ( cel%rInterceptionStorage > 0.0_c_float ) then

          rPrevious_Interception_Storage = cel%rInterceptionStorage
          rFraction_Wet = ( cel%rInterceptionStorage / ( rMAXIMUM_INTERCEPTION_STORAGE + 1.0e-6) )**0.66666667_c_float
          rPotential_Evaporated_Interception = min( cel%rReferenceET0, rFraction_Wet * cel%rInterceptionStorage )
          cel%rInterceptionStorage = max( cel%rInterceptionStorage - rPotential_Evaporated_Interception, 0.0_c_float )
          cel%rActual_ET_interception = max( rPrevious_Interception_Storage - cel%rInterceptionStorage, 0.0_c_float )
            ! modify ReferenceET0_adj if evaporation of interception is considered to decrease reference ET0
          if ( pConfig%iConfigureActET_Interception == CONFIG_INTERCEPTION_IS_PART_OF_ACTET ) &
            cel%rReferenceET0_adj = max( cel%rReferenceET0 - cel%rActual_ET_interception, 0.0_c_float)

        else

          cel%rActual_ET_interception = 0.0_c_float
          cel%rReferenceET0_adj = cel%rReferenceET0

        endif

        ! NOW we're through with INTERCEPTION calculations
        ! Next, we partition between snow and rain

        ! Assume that all precipitation is rain, for now
        dpNetRainfall = dpNetPrecip

        ! Is it snowing?
        if (lFREEZING ) then
          dpSnowCover = dpSnowCover + dpNetPrecip
          cel%rSnowFall_SWE = dpNetPrecip
          dpNetRainfall = dpZERO      ! For now -- if there is snowmelt, we do it next
        end if

        ! Is there any melting?
        if ( cel%rTAvg > rFREEZING ) then
          dpPotentialMelt = rMELT_INDEX * ( cel%rTMax - rFREEZING ) &
                            * dpC_PER_F / rMM_PER_INCH

          if(dpSnowCover > dpPotentialMelt) then
            cel%rSnowMelt = dpPotentialMelt
            dpSnowCover = dpSnowCover - dpPotentialMelt
          else   ! not enough snowcover to satisfy the amount that *could* melt
            cel%rSnowMelt = dpSnowCover
            dpSnowCover = dpZERO
          end if

        end if

        dpChgInSnowCover = dpSnowCover - dpPreviousSnowCover

        ! copy temporary double-precision values back to single-precision
        cel%rSnowCover = real(dpSnowCover, kind=c_float)
        cel%rNetRainfall = real(dpNetRainfall, kind=c_float)

      endif

      call stats_UpdateAllAccumulatorsByCell( &
        REAL(dpChgInSnowCover,kind=c_double), iCHG_IN_SNOW_COV,iMonth,iZERO)

      call stats_UpdateAllAccumulatorsByCell( &
        dpNetRainfall,iNET_RAINFALL,iMonth,iZERO)

      call stats_UpdateAllAccumulatorsByCell( &
        REAL(cel%rSnowMelt,kind=c_double),iSNOWMELT,iMonth,iZERO)

      call stats_UpdateAllAccumulatorsByCell( &
        REAL(cel%rSnowFall_SWE,kind=c_double),iSNOWFALL_SWE,iMonth,iZERO)

      call stats_UpdateAllAccumulatorsByCell( &
        dpSnowCover,iSNOWCOVER,iMonth,iZERO)

    end do

  end do

  ! a call to the UpdateAllAccumulatorsByCell subroutine with a value of "iNumGridCalls"
  ! as the final argument triggers the routine to update monthly and annual stats
  call stats_UpdateAllAccumulatorsByCell(dpZERO,iCHG_IN_SNOW_COV,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO,iNET_RAINFALL,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO,iSNOWMELT,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO,iSNOWFALL_SWE,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO,iSNOWCOVER,iMonth,iNumGridCells)

end subroutine model_ProcessRain

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
  integer (kind=c_int),intent(in) :: iDayOfYear    ! day of current year (January 1 = 1)
  ! [ LOCALS ]
  integer (kind=c_int) :: iCount
  integer (kind=c_int) :: j, i
  real (kind=c_double) :: xmin, xmax, ymin, ymax
  integer (kind=c_int), intent(in) :: iMonth     ! Integer month value (1-12)
  integer (kind=c_int) :: iNumGridCells

  ! calculate number of cells in model grid
  iNumGridCells = pGrd%iNX * pGrd%iNY

  ! Iteratively processes the runoff event; first initialize the upstream flows
  pGrd%Cells(:,:)%rInFlow = rZERO
  pGrd%Cells(:,:)%rOutFlow = rZERO
  pGrd%Cells(:,:)%rFlowOutOfGrid = rZERO

  if (pConfig%iConfigureRunoffMode == CONFIG_RUNOFF_DOWNHILL ) then

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
  pGrd%Cells(:,:)%rNetInflowBuf(iDayCtr) = pGrd%Cells(:,:)%rNetRainfall &
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
  integer (kind=c_int) :: iCol, iRow, iStat, tj, ti, iTgt_Row, iTgt_Col,k,iCumlCount,iCount
  integer (kind=c_int) :: iRowSub, iColSub, iNChange, iUpstreamCount, iPasses
  integer (kind=c_int) :: ic
  integer (kind=c_int) :: iNumGridCells, iNumActiveGridCells
  integer (kind=c_int) :: iNumIterationsNochange
  integer (kind=c_int) :: LU_TEMP
  logical (kind=c_bool) :: lExist
  logical (kind=c_bool) :: lCircular = lFALSE
  type( T_GENERAL_GRID ), pointer :: pTempGrid
  type (T_CELL),pointer :: cel

  ! calculate number of gridcells in model domain
  iNumGridCells = pGrd%iNY * pGrd%iNX

  iNumActiveGridCells = count(pGrd%iMask == iACTIVE_CELL)

  ! set iteration counter
  iNumIterationsNochange = 0

  pTempGrid=>grid_Create( pGrd%iNX, pGrd%iNY, pGrd%rX0, pGrd%rY0, &
    pGrd%rX1, pGrd%rY1, DATATYPE_INT )

  allocate(iOrderCol(iNumActiveGridCells), iOrderRow(iNumActiveGridCells), stat=iStat)
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
          if (pGrd%iMask(iCol, iRow) == iINACTIVE_CELL) cycle
          if ( cel%lDownhillMarked ) cycle
          ! Count upstream cells
          iUpstreamCount = 0

          cel%iSumUpslopeCells = 0
          cel%iNumUpslopeConnections = 0

          ! now search all adjacent cells which have current cell
          ! as their target

          lCircular = lFALSE

          do iRowSub=iRow-1,iRow+1
            if (iRowSub>=1 .and. iRowSub<=pGrd%iNY) then     ! we're within array bounds
              do iColSub=iCol-1,iCol+1
                if (iColSub>=1 .and. iColSub<=pGrd%iNX) then              ! we're within array bounds
                  if (iRow==iRowSub .and. iCol==iColSub) cycle            ! Skip current inquiry cell
                  if (pGrd%iMask(iColSub, iRowSub) == iINACTIVE_CELL ) cycle     ! Don't count inactive neighbors
                  call model_DownstreamCell(pGrd,iRowSub,iColSub,iTgt_Row,iTgt_Col)

                  if (iTgt_Row==iRow .and. iTgt_Col==iCol ) then          ! target cell points at current inquiry cell
                    if (pGrd%Cells(iColSub,iRowSub)%lDownhillMarked) then

                      cel%iSumUpslopeCells = cel%iSumUpslopeCells &
                         + pGrd%Cells(iColSub,iRowSub)%iSumUpslopeCells + 1
                       cel%iNumUpslopeConnections = cel%iNumUpslopeConnections + 1

                    else

                      iUpstreamCount = iUpstreamCount+1
                      ! we've found a cell that points to the current model
                      ! cell; does our current model cell point back at it?
                      ! if so, we have circular flow
                      call model_DownstreamCell(pGrd,iRow,iCol,iTgt_Row,iTgt_Col)
                      if (iTgt_Row==iRowSub .and. iTgt_Col==iColSub )  lCircular = lTRUE

                    endif

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

        ! loop over possible (legal) values of the flow direction grid
        do k=0,128
          iCount=COUNT(.not. pGrd%Cells%lDownHillMarked &
            .and.pGrd%Cells%iFlowDir==k .and. pGrd%iMask == iACTIVE_CELL)
          if(iCount>0) then
            iCumlCount = iCumlCount + iCount
            write(LU_LOG,FMT="(3x,i8,' unmarked grid cells have flowdir value: ',i8)") &
              iCount, k
          end if
        end do

        write(LU_LOG,FMT="(3x,a)") repeat("-",60)
        write(LU_LOG,FMT="(3x,i8,' Total cells with nonzero flow " &
          //"direction values')") iCumlCount

#ifdef DEBUG_PRINT

        where( pGrd%Cells%lDownHillMarked .or.  pGrd%iMask /=  iACTIVE_CELL )
          pTempGrid%iData = iROUTE_CELL_MARKED
        elsewhere
          pTempGrid%iData = pGrd%Cells%iFlowDir
        end where

!        call genericgraph(pTempGrid)
#endif

        call grid_WriteArcGrid("iteration"//TRIM(int2char(iPasses))// &
          "problem_gridcells.asc", pTempGrid)

      else
        ! reset iteration counter
        iNumIterationsNochange = 0
      end if

      if(iOrderCount == iNumActiveGridCells) exit
      iPasses = iPasses+1
      write(UNIT=LU_LOG,FMT=*) 'Iteration ',iPasses,'  ',iOrderCount,&
        ' of ',iNumGridCells,' cells have been configured'

    end do

    write(UNIT=LU_LOG,FMT=*) "  Number of passes required: ",iPasses
    write(UNIT=LU_LOG,FMT=*) ""
    flush(UNIT=LU_LOG)

    open ( LU_ROUTING, FILE='swb_routing.bin',FORM='UNFORMATTED', &
      status='REPLACE',ACCESS='STREAM')

    open (unit=newunit(LU_TEMP), FILE='swb_routing_log.csv', FORM='FORMATTED', &
      status='REPLACE')
    write (LU_TEMP,fmt="(a)") "Row number, Col number, Num contributing cells, Num upslope connections"

    write(LU_ROUTING) iOrderCount
    write(LU_TEMP, fmt="(i12)") iOrderCount

    do ic=1,iOrderCount
      write(LU_ROUTING) iOrderCol(ic),iOrderRow(ic)
      write(LU_TEMP, fmt="(i12,',',i12,',',i12,',',i12)") iOrderRow(ic),iOrderCol(ic), &
         pGrd%Cells(iOrderCol(ic),iOrderRow(ic))%iSumUpslopeCells, &
         pGrd%Cells(iOrderCol(ic),iOrderRow(ic))%iNumUpslopeConnections
    end do
    flush(UNIT=LU_ROUTING)
    close(UNIT=LU_ROUTING)
    close(UNIT=LU_TEMP)

    pTempGrid%iData = pGrd%Cells%iSumUpslopeCells

    call make_shaded_contour(pGrd=pTempGrid, &
      sOutputFilename=trim(pConfig%sOutputFilePrefix) // "CALC_Upslope_Contributing_Area.png", &
      sTitleTxt="Upslope Contributing Area", &
      sAxisTxt="Number of Cells" )

    pTempGrid%iData = pGrd%Cells%iNumUpslopeConnections

    call make_shaded_contour(pGrd=pTempGrid, &
      sOutputFilename=trim(pConfig%sOutputFilePrefix) // "CALC_Num_Upslope_Connections.png", &
      sTitleTxt="Number of Upslope Connecting Cells", &
      sAxisTxt="Number of Cells" )

  else ! routing table already exists

    pGrd%Cells%lDownhillMarked = lTRUE
    open(LU_ROUTING, FILE='swb_routing.bin',FORM='UNFORMATTED', ACCESS='STREAM')
    read(LU_ROUTING) iOrderCount

    ! crude error checking to see whether the routing table has the right
    ! number of elements
    call Assert(LOGICAL(iOrderCount==iNumActiveGridCells,kind=c_bool), &
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
  integer (kind=c_int),intent(in) :: iDayOfYear
  integer (kind=c_int), intent(in) :: iMonth       ! Integer month value (1-12)
  ! [ LOCALS ]
  integer (kind=c_int) :: ic,iTgt_Col,iTgt_Row,iFrac
  real (kind=c_float) :: rP,rR,rDelta
  type (T_CELL),pointer :: cel
  type (T_CELL),pointer :: target_cel

  ! Reset the upstream flows (note that iOrderCount, iOrderCol, and iOrderRow are globals)
  do ic=1,iOrderCount

    cel => pGrd%Cells(iOrderCol(ic),iOrderRow(ic))

    if (pGrd%iMask(iOrderCol(ic),iOrderRow(ic)) == iINACTIVE_CELL) cycle

    call model_DownstreamCell(pGrd,iOrderRow(ic),iOrderCol(ic),iTgt_Row,iTgt_Col)

    ! Compute the runoff
    cel%rRunoff = rf_model_CellRunoff(pConfig, cel, iDayOfYear)

    ! Now, route the water
    if ( iTgt_Row == iROUTE_LEFT_GRID .or. iTgt_Col == iROUTE_LEFT_GRID ) then
      cel%rFlowOutOfGrid = cel%rRunoff
      cel%rOutFlow = rZERO
      cycle
    elseif ( iTgt_Row == iROUTE_DEPRESSION  .or. iTgt_Col == iROUTE_DEPRESSION ) then
      ! Don't route any further; the water pools here.
      cel%rOutFlow = rZERO
      cycle
    endif

    ! MUST screen target values to ensure we don't start attempting
    ! manipulation of memory that is out of bounds!!
    call Assert(LOGICAL(iTgt_Row>0 .and. iTgt_Row <= pGrd%iNY,kind=c_bool), &
      "iTgt_Row out of bounds: Row = "//int2char(iOrderRow(ic)) &
      //"  Col = "//int2char(iOrderCol(ic)), &
      trim(__FILE__),__LINE__)
    call Assert(LOGICAL(iTgt_Col>0 .and. iTgt_Col <= pGrd%iNX,kind=c_bool), &
      "iTgt_Col out of bounds: Row = "//int2char(iOrderRow(ic)) &
      //"  Col = "//int2char(iOrderCol(ic)), &
      trim(__FILE__),__LINE__)

    target_cel => pGrd%Cells(iTgt_Col,iTgt_Row)

    !> if target cell is inactive, assume that the water should be tracked
    !> as flow out of grid
    if ( pGrd%iMask(iTgt_Col,iTgt_Row) == iINACTIVE_CELL) then

      cel%rFlowOutOfGrid = cel%rRunoff
      cel%rOutFlow = rZERO
      cycle

    endif

    if ( target_cel%iLandUse == pConfig%iOPEN_WATER_LU                   &
        .or. target_cel%rSoilWaterCap < rNEAR_ZERO ) then
      ! Don't route any further; the water has joined a generic
      ! surface water feature. We assume that once the water hits a
      ! surface water feature that the surface water drainage
      ! network transports the bulk of it
      ! out of the model domain quite rapidly
      cel%rFlowOutOfGrid = cel%rRunoff
      cel%rOutFlow = rZERO

    else
      ! add cell outflow to target cell inflow; ROUTING FRACTION represents the fraction of
      ! cell outflow that makes it to the downslope cell.
      cel%rFlowOutOfGrid = cel%rRunoff * (rONE - cel%rRouteFraction)
      cel%rOutflow = cel%rRunoff * cel%rRouteFraction
      target_cel%rInFlow = cel%rOutFlow
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
  integer (kind=c_int),intent(in) :: iDayOfYear
  integer (kind=c_int), intent(in) :: iMonth       ! Integer month value (1-12)
  ! [ LOCALS ]
  integer (kind=c_int) :: iCol,iRow, iFrac
  type (T_CELL),pointer :: cel
  ! [ CONSTANTS ]

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX
      cel => pGrd%Cells(iCol,iRow)

    ! Compute the runoff for each cell
    cel%rRunoff = rf_model_CellRunoff(pConfig, cel, iDayOfYear)

    ! Now, remove any runoff from the model grid
    cel%rFlowOutOfGrid = cel%rRunoff

    ! we've removed the water from the grid; it shouldn't be included in
    ! "outflow" water
    cel%rOutFlow = rZERO

    end do
  end do

end subroutine model_Runoff_NoRouting


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
  integer (kind=c_int),intent(in) :: iDayOfYear
  ! [ RETURN VALUE ]
  real (kind=c_float) :: rOutFlow
  ! [ LOCALS ]

  if (pConfig%iConfigureRunoff == CONFIG_RUNOFF_CURVE_NUMBER) then
    rOutFlow = runoff_CellRunoff_CurveNumber(pConfig, cel, iDayOfYear)
  else if (pConfig%iConfigureRunoff == CONFIG_RUNOFF_GREEN_AMPT) then
    rOutFlow = rf_model_CellRunoff_GreenAmpt(pConfig, cel, iDayOfYear)
  end if

  if (rOutFlow < rZERO) then
    call echolog("gross precip: " &
      //trim(asCharacter(cel%rGrossPrecip))//"~outflow: " &
      //trim(asCharacter(rOutflow)))
    call assert(lFALSE, "Negative outflow calculated for cell!", &
      trim(__FILE__), __LINE__)
  endif

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
  integer (kind=c_int),intent(in) :: iDayOfYear
  ! [ RETURN VALUE ]
  real (kind=c_float) :: rOutFlow
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

  ! [ LOCALS ]
  integer (kind=c_int) :: iRow,iCol
  integer (kind=c_int) :: iTgt_Row,iTgt_Col
  type (T_CELL),pointer :: cel
  character (len=256) :: sBuf

  ! [ PARAMETERS ]
  integer (kind=c_short),parameter :: DIR_DEPRESSION=0
  integer (kind=c_short),parameter :: DIR_RIGHT=1
  integer (kind=c_short),parameter :: DIR_DOWN_RIGHT=2
  integer (kind=c_short),parameter :: DIR_DOWN=4
  integer (kind=c_short),parameter :: DIR_DOWN_LEFT=8
  integer (kind=c_short),parameter :: DIR_LEFT=16
  integer (kind=c_short),parameter :: DIR_UP_LEFT=32
  integer (kind=c_short),parameter :: DIR_UP=64
  integer (kind=c_short),parameter :: DIR_UP_RIGHT=128

  ! no point in doing these calculations unless we're really going to
  ! route water
  if(pConfig%iConfigureRunoffMode==CONFIG_RUNOFF_NO_ROUTING) return

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX

      cel => pGrd%Cells(iCol,iRow)

      if (pGrd%iMask(iCol, iRow) == iINACTIVE_CELL) then

       cel%iTgt_Row = iROUTE_LEFT_GRID
       cel%iTgt_Col = iROUTE_LEFT_GRID
       cycle

     endif

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
  integer (kind=c_int),intent(in) :: iRow,iCol
  integer (kind=c_int),intent(out) :: iTgt_Row,iTgt_Col
  ! [ PARAMETERS ]
  integer (kind=c_short),parameter :: DIR_DEPRESSION=0
  integer (kind=c_short),parameter :: DIR_RIGHT=1
  integer (kind=c_short),parameter :: DIR_DOWN_RIGHT=2
  integer (kind=c_short),parameter :: DIR_DOWN=4
  integer (kind=c_short),parameter :: DIR_DOWN_LEFT=8
  integer (kind=c_short),parameter :: DIR_LEFT=16
  integer (kind=c_short),parameter :: DIR_UP_LEFT=32
  integer (kind=c_short),parameter :: DIR_UP=64
  integer (kind=c_short),parameter :: DIR_UP_RIGHT=128

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
  integer (kind=c_int) :: iStat, iNumMaskFiles, i, iRecNum, iSize
  character (len=256) :: sRecord                  ! Input file text buffer
  character (len=256) :: sItem                    ! Key word read from sRecord
  character (len=256) :: sBuf

  ! open basin mask file
  open ( LU_MASK, file=pConfig%sBasinMaskFilename, &
    status="OLD", iostat=iStat )
  call Assert( LOGICAL( iStat == 0,kind=c_bool), &
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
  integer (kind=c_int) :: iStat, iNumLandUses, i, iType, iRecNum, iSize
  integer (kind=c_int) :: iNumSoilTypes
  character (len=1024) :: sRecord                  ! Input file text buffer
  character (len=256)  :: sItem                    ! Key word read from sRecord
  real (kind=c_float)  :: fTempVal

  ! open landuse file
  open ( LU_LOOKUP, file=pConfig%sLanduseLookupFilename, &
    status="OLD", iostat=iStat )
  call Assert( LOGICAL( iStat == 0,kind=c_bool), &
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

  !> keep track of number of landuses for use throughout code
  pConfig%iNumberOfLanduses = iNumLandUses

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

  pConfig%iNumberOfSoilTypes = iNumSoilTypes

  ! now allocate memory for landuse table
  allocate ( pConfig%LU( iNumLandUses ), stat=iStat )
  call Assert ( iStat == 0, &
    "Could not allocate space for landuse data structure" )

  ! now allocate memory for IRRIGATION, TEW, and REW subtables
  ! even if irrigation is not actively being used, this must be in place so that
  ! the default growing-degree day baseline temps are available elsewhere in the code
  allocate ( pConfig%IRRIGATION( pConfig%iNumberOfLanduses ), stat=iStat )
  call Assert ( iStat == 0, &
    "Could not allocate space for IRRIGATION data structure", trim(__FILE__), __LINE__ )

  ! now allocate memory for READILY_EVAPORABLE_WATER subtable
  allocate ( pConfig%READILY_EVAPORABLE_WATER( pConfig%iNumberOfLanduses, &
    pConfig%iNumberOfSoilTypes), stat=iStat )
  call Assert ( iStat == 0, &
    "Could not allocate space for READILY_EVAPORABLE_WATER  data structure", &
    trim(__FILE__), __LINE__ )

   ! assign a reasonable default value
   pConfig%READILY_EVAPORABLE_WATER = 0.3

  ! now allocate memory for TOTAL_EVAPORABLE_WATER subtable
  allocate ( pConfig%TOTAL_EVAPORABLE_WATER( pConfig%iNumberOfLanduses, &
    pConfig%iNumberOfSoilTypes), stat=iStat )
  call Assert ( iStat == 0, &
    "Could not allocate space for TOTAL_EVAPORABLE_WATER  data structure", &
    trim(__FILE__), __LINE__ )

  ! assign another reasonable default value
  pConfig%TOTAL_EVAPORABLE_WATER = 0.6

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

  ! now allocate memory for IRRIGATION subtable within landuse table
  ! note that this table is NOT populated in this subroutine, but in "readIrrgationLookupTable"
  allocate ( pConfig%IRRIGATION( iNumLandUses ), stat=iStat )
  call Assert ( iStat == 0, &
    "Could not allocate space for IRRIGATION data structure" )

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
  read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%LU(iRecNum)%rIntercept_GrowingSeason_a
  call Assert( iStat == 0, "Error reading interception values in landuse file" )
  write(UNIT=LU_LOG,FMT=*)  "  Interception value ('a' coefficient) for growing season = ",    &
    pConfig%LU(iRecNum)%rIntercept_GrowingSeason_a

  ! extra parameters needed if we are running with the Horton option
  if ( pConfig%iConfigureInterception == CONFIG_INTERCEPTION_HORTON ) then

    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%LU(iRecNum)%rIntercept_GrowingSeason_b
    call Assert( iStat == 0, "Error reading interception values in landuse file" )
    write(UNIT=LU_LOG,FMT=*)  "  Interception value ('b' coefficient) for growing season = ", &
      pConfig%LU(iRecNum)%rIntercept_GrowingSeason_b

    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%LU(iRecNum)%rIntercept_GrowingSeason_n
    call Assert( iStat == 0, "Error reading interception values in landuse file" )
    write(UNIT=LU_LOG,FMT=*)  "  Interception precipitation exponent ('n' coefficient) for growing season = ", &
      pConfig%LU(iRecNum)%rIntercept_GrowingSeason_n

  endif

  call chomp(sRecord, sItem, sTAB)
  read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%LU(iRecNum)%rIntercept_NonGrowingSeason_a
  call Assert( iStat == 0, "Error reading interception values in landuse file" )
  write(UNIT=LU_LOG,FMT=*)  "  Interception value ('a' coefficient) for non-growing season = ", &
    pConfig%LU(iRecNum)%rIntercept_NonGrowingSeason_a

  if ( pConfig%iConfigureInterception == CONFIG_INTERCEPTION_HORTON ) then

    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%LU(iRecNum)%rIntercept_NonGrowingSeason_b
    call Assert( iStat == 0, "Error reading interception values in landuse file" )
    write(UNIT=LU_LOG,FMT=*)  "  Interception value ('b' coefficient) for non-growing season = ", &
      pConfig%LU(iRecNum)%rIntercept_NonGrowingSeason_b

    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%LU(iRecNum)%rIntercept_NonGrowingSeason_n
    call Assert( iStat == 0, "Error reading interception values in landuse file" )
    write(UNIT=LU_LOG,FMT=*)  "  Interception precipitation exponent ('n' coefficient) for non-growing season = ", &
      pConfig%LU(iRecNum)%rIntercept_NonGrowingSeason_n

  endif

  if ( pConfig%iConfigureActET_Interception == CONFIG_INTERCEPTION_IS_PART_OF_ACTET ) then
    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%LU(iRecNum)%rMax_Interception_Storage
    call Assert( iStat == 0, "Error reading maximum interception storage values in landuse file" )
    write(UNIT=LU_LOG,FMT=*)  "  Maximum interception storage value = ", &
      pConfig%LU(iRecNum)%rMax_Interception_Storage
  endif

  ! now read in a rooting depth for each landuse/soil type combination
  do i=1,iNumSoilTypes
    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%ROOTING_DEPTH(iRecNum,i)
    call Assert( iStat == 0, &
      "Error reading rooting depth for soil group "//trim(int2char(i))//" in landuse lookup table" )
    write(UNIT=LU_LOG,FMT=*)  "  ROOTING DEPTH for soil group",i,": ",pConfig%ROOTING_DEPTH(iRecNum,i)
  end do

  call chomp(sRecord, sItem, sTAB)

  if (len_trim(sItem) > 0 ) then
    read( sItem, fmt=*, iostat=iStat ) fTempVal
    ! test to see whether fTempVal was read correcly as a *REAL* value. if it is, we have extra data
    ! columns present. the HORTON interception routine introduces 4 new columns of data that might be read
    ! in as rooting depths if the user is not careful.
    call assert( iStat /= 0, "Read a real value from a column of the landuse lookup table following what "   &
      //"was supposed~to be the final ROOTING DEPTH column. This file may contain HORTON interception" &
      //" values,~or other unknown data columns. Please check the landuse lookup table and rerun.", __FILE__, __LINE__)
  endif

  iRecNum = iRecNum + 1

  end do LU_READ

  pConfig%IRRIGATION%iLandUseType = pConfig%LU%iLandUseType

  ! That's all!
  close ( unit=LU_LOOKUP )

  return
end subroutine model_ReadLanduseLookupTable

!--------------------------------------------------------------------------

subroutine model_ReadIrrigationLookupTable( pConfig, pGrd )
  !! Reads the irrigation data from pConfig%sIrrigationLookupFilename
  ! [ ARGUMENTS ]
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  type ( T_GENERAL_GRID ),pointer :: pGrd          ! pointer to model grid

  ! [ LOCALS ]
  integer (kind=c_int) :: iStat, iNumLandUses, i, j, iType, iRecNum, iSize
  integer (kind=c_int) :: iLandUseType
  integer (kind=c_int) :: iLandUseIndex
  integer (kind=c_int) :: iNumSoilTypes
  logical (kind=c_bool) :: lFound
  real (kind=c_float) :: rTempValue
  character (len=1024) :: sRecord                  ! Input file text buffer
  character (len=256) :: sItem                    ! Key word read from sRecord
  character (len=256) :: sBuf
	integer (kind=c_int) :: iCol,iRow
  type ( T_CELL ),pointer :: cel            ! pointer to cell data structure

  ! open IRRIGATION file
  open ( LU_LOOKUP, file=pConfig%sIrrigationLookupFilename, &
    status="OLD", iostat=iStat )
  call Assert( LOGICAL( iStat == 0,kind=c_bool), &
    "Open failed for file: " // pConfig%sIrrigationLookupFilename )

  call Assert(associated(pConfig%LU), "The landuse lookup table must be read in " &
    //"before the irrigation lookup table may be read.", trim(__FILE__),__LINE__)

  ! set the configuration option; if we're reading in the irrigation table,
  ! it is assumed that the crop coefficients should be applied and
  ! irrigation amounts calculated

  iSize = size(pConfig%LU,1)
  iNumSoilTypes = size(pConfig%CN,2)

  iRecNum = 1

  LU_READ: do

    read ( unit=LU_LOOKUP, fmt="(a)", iostat=iStat ) sRecord
    if ( iStat < 0 ) exit     ! EOF mark
    if ( sRecord(1:1) == "#" ) cycle      ! Ignore comment lines

    write(UNIT=LU_LOG,FMT=*) ""
    write(UNIT=LU_LOG,FMT=*)  "-----------------------------------------------------------"
    write(UNIT=LU_LOG,FMT=*)  "Reading irrigation table record number ",iRecNum
    write(UNIT=LU_LOG,FMT=*) ""

    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) iLandUseType
    call Assert( iStat == 0, "Error reading land use type in irrigation lookup table" )

    ! scan the landuse codes already supplied in the landuse lookup table and
    ! find a match
    lFound = lFALSE
    do j=1,pConfig%iNumberOfLanduses
      if(iLandUseType == pConfig%LU(j)%iLandUseType) then
        iLandUseIndex = j
        lFound = lTRUE
        exit
      endif
    enddo

    call assert(lFound, "Unknown landuse code found while reading from the " &
      //"crop coefficient and irrigation parameters table.~Landuse specified "&
      //"in the irrigation table but not found in the landuse table: " &
      //trim(int2char(iLandUseType)),trim(__FILE__), __LINE__)

    call assert(iLandUseIndex >= 1 .and. iLandUseIndex <= pConfig%iNumberOfLanduses, &
      "Array index out of bounds. Variable is iLandUseIndex with a value of " &
      //trim(int2char(iLandUseIndex)), trim(__FILE__),__LINE__)

    ! store index value for future reference; allows us to easily match up
    ! records in the landuse lookup table even if the irrigation table
    ! is supplied in a different order
    pConfig%IRRIGATION(iLandUseIndex)%iLandUseType = iLandUseType

    call chomp(sRecord, sItem, sTAB)
    pConfig%IRRIGATION(iLandUseIndex)%sLandUseDescription = trim(sItem)
    write(UNIT=LU_LOG,FMT=*)  "  landuse "//trim(asCharacter(iLandUseType))//" : ", &
      dQuote(pConfig%IRRIGATION(iLandUseIndex)%sLandUseDescription)

    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iLandUseIndex)%rMeanPlantHeight
    call Assert( iStat == 0, &
      "Error reading mean plant height " &
        //"from irrigation lookup table", trim(__FILE__), __LINE__ )
    write(UNIT=LU_LOG,FMT=*)  "  mean plant height (feet) ", &
      pConfig%IRRIGATION(iLandUseIndex)%rMeanPlantHeight

    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iLandUseIndex)%rKcb_ini
    call Assert( iStat == 0, &
      "Error reading initial basal crop coefficient (Kcb_ini) " &
        //"from irrigation lookup table", trim(__FILE__), __LINE__ )
    write(UNIT=LU_LOG,FMT=*)  "  initial basal crop coefficient (Kcb_ini) ", &
      pConfig%IRRIGATION(iLandUseIndex)%rKcb_ini

    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iLandUseIndex)%rKcb_mid
    call Assert( iStat == 0, &
      "Error reading mid-growth basal crop coefficient (Kcb_mid) " &
        //"from irrigation lookup table", trim(__FILE__), __LINE__ )
    write(UNIT=LU_LOG,FMT=*)  "  mid-growth basal crop coefficient (Kcb_ini) ", &
      pConfig%IRRIGATION(iLandUseIndex)%rKcb_mid

    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iLandUseIndex)%rKcb_end
    call Assert( iStat == 0, &
      "Error reading end-growth phase basal crop coefficient (Kcb_end) " &
         //"from irrigation lookup table", trim(__FILE__), __LINE__ )
    write(UNIT=LU_LOG,FMT=*)  "  end-growth basal crop coefficient (Kcb_end) ", &
      pConfig%IRRIGATION(iLandUseIndex)%rKcb_end

    pConfig%IRRIGATION(iLandUseIndex)%rKcb_max = &
       max( pConfig%IRRIGATION(iLandUseIndex)%rKcb_ini, &
       pConfig%IRRIGATION(iLandUseIndex)%rKcb_mid, &
       pConfig%IRRIGATION(iLandUseIndex)%rKcb_end )

    write(UNIT=LU_LOG,FMT=*)  "  maximum basal crop coefficient (Kcb_max) ", &
      pConfig%IRRIGATION(iLandUseIndex)%rKcb_max

    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iLandUseIndex)%rKcb_min
    call Assert( iStat == 0, &
      "Error reading minimum allowable crop coefficient (Kcb_min) " &
         //"from irrigation lookup table", trim(__FILE__), __LINE__ )
    write(UNIT=LU_LOG,FMT=*)  "  end-growth basal crop coefficient (Kcb_min) ", &
      pConfig%IRRIGATION(iLandUseIndex)%rKcb_min
      ! initialize rKcb to the minimum value
      pConfig%IRRIGATION(iLandUseIndex)%rKcb = pConfig%IRRIGATION(iLandUseIndex)%rKcb_min

!    call chomp(sRecord, sItem, sTAB)
!    pConfig%IRRIGATION(iLandUseIndex)%iBeginIrrigation = mmddyyyy2doy(sItem)
!    write(UNIT=LU_LOG,FMT=*)  "   irrigation starts on or after day ", &
!      pConfig%IRRIGATION(iLandUseIndex)%iBeginIrrigation

    call chomp(sRecord, sItem, sTAB)
      if ( scan(sItem, "/") /= 0 ) then
        pConfig%IRRIGATION(iLandUseIndex)%iL_plant = mmdd2doy(sItem)
      else
        read ( unit=sItem, fmt=*, iostat=iStat ) rTempValue
        call Assert( iStat == 0, &
          "Error reading day of year (or GDD) of initial planting from " &
            //"irrigation lookup table" , trim(__FILE__), __LINE__ )
        pConfig%IRRIGATION(iLandUseIndex)%iL_plant = int(rTempValue)
      endif
    write(UNIT=LU_LOG,FMT=*)  "  day of year (or GDD) of initial planting ", &
      pConfig%IRRIGATION(iLandUseIndex)%iL_plant

    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) rTempValue
    call Assert( iStat == 0, &
      "Error reading number of days (or GDD) until end of initial plant growth " &
        //"phase from irrigation lookup table" , trim(__FILE__), __LINE__ )
    pConfig%IRRIGATION(iLandUseIndex)%iL_ini = int(rTempValue) &
		  + pConfig%IRRIGATION(iLandUseIndex)%iL_plant
    write(UNIT=LU_LOG,FMT=*)  "  length (in days or GDD) until end of " &
	    //"initial plant growth phase ", int(rTempValue)
    write(UNIT=LU_LOG,FMT=*)  "  day of year (or GDD) for end of " &
      //"initial plant growth phase ", pConfig%IRRIGATION(iLandUseIndex)%iL_ini

    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) rTempValue
    call Assert( iStat == 0, &
      "Error reading length (days or GDD) until end of plant development " &
        //"phase from irrigation lookup table" , trim(__FILE__), __LINE__ )
    pConfig%IRRIGATION(iLandUseIndex)%iL_dev = int(rTempValue) &
				  + pConfig%IRRIGATION(iLandUseIndex)%iL_ini
    write(UNIT=LU_LOG,FMT=*)  "  length (in days or GDD) until end of " &
	    //"plant development phase ", int(rTempValue)
    write(UNIT=LU_LOG,FMT=*)  "  day of year (or GDD) for end of " &
      //"plant development ", pConfig%IRRIGATION(iLandUseIndex)%iL_dev

    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) rTempValue
    call Assert( iStat == 0, &
      "Error reading length (days or GDD) until end of mid-season growth " &
        //"phase from irrigation lookup table" , trim(__FILE__), __LINE__ )
    pConfig%IRRIGATION(iLandUseIndex)%iL_mid = int(rTempValue) &
		      + pConfig%IRRIGATION(iLandUseIndex)%iL_dev
    write(UNIT=LU_LOG,FMT=*)  "  length (in days or GDD) until end of " &
	    //"mid-season growth phase ", int(rTempValue)
    write(UNIT=LU_LOG,FMT=*)  "  day of year (or GDD) for end of " &
      //"mid-season growth phase ", pConfig%IRRIGATION(iLandUseIndex)%iL_mid

    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) rTempValue
    call Assert( iStat == 0, &
      "Error reading length (days or GDD) until end of late-season growth " &
        //"phase from irrigation lookup table" , trim(__FILE__), __LINE__ )
    pConfig%IRRIGATION(iLandUseIndex)%iL_late = int(rTempValue) &
		      + pConfig%IRRIGATION(iLandUseIndex)%iL_mid
    write(UNIT=LU_LOG,FMT=*)  "  length (in days or GDD) until end of " &
	    //"late-season growth phase ", int(rTempValue)
    write(UNIT=LU_LOG,FMT=*)  "  day of year (or GDD) for end of " &
      //"late-season growth phase ", pConfig%IRRIGATION(iLandUseIndex)%iL_late

    call chomp(sRecord, sItem, sTAB)
    if(str_compare(sItem,"GDD") ) then
      pConfig%IRRIGATION(iLandUseIndex)%lUnitsAreDOY = lFALSE
    elseif(str_compare(sItem,"DOY") ) then
      pConfig%IRRIGATION(iLandUseIndex)%lUnitsAreDOY = lTRUE
    else
      call Assert( lFALSE, &
      "Error reading units label from irrigation lookup table.~ Valid" &
        //" entries are 'GDD' or 'DOY'", trim(__FILE__), __LINE__ )
    endif
    write(UNIT=LU_LOG,FMT=*)  "  growth targets given in units of ", &
      dquote(sItem)

    do i=1,iNumSoilTypes
      call chomp(sRecord, sItem, sTAB)
      ! READILY_EVAPORABLE_WATER(# LU, #Soil Types)
      read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%READILY_EVAPORABLE_WATER(iLandUseIndex,i)
      call Assert( iStat == 0, &
        "Error reading readily evaporable water for soil group " &
          //trim(int2char(i))//" and landuse " &
          //trim(int2char(pConfig%IRRIGATION(iLandUseIndex)%iLandUseType) ) &
          //" in landuse lookup table" , trim(__FILE__), __LINE__ )
      write(UNIT=LU_LOG,FMT=*)  "  readily evaporable water for soil group",i,": ", &
        pConfig%READILY_EVAPORABLE_WATER(iLandUseIndex,i)
    end do

    do i=1,iNumSoilTypes
      call chomp(sRecord, sItem, sTAB)
      ! TOTAL_EVAPORABLE_WATER(# LU, #Soil Types)
      read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%TOTAL_EVAPORABLE_WATER(iLandUseIndex,i)
      call Assert( iStat == 0, &
        "Error reading total evaporable water for soil group " &
          //trim(int2char(i))//" and landuse " &
          //trim(int2char(pConfig%IRRIGATION(iLandUseIndex)%iLandUseType) ) &
          //" in landuse lookup table" , trim(__FILE__), __LINE__ )
      write(UNIT=LU_LOG,FMT=*)  "  total evaporable water for soil group",i,": ", &
        pConfig%TOTAL_EVAPORABLE_WATER(iLandUseIndex,i)
    end do

    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iLandUseIndex)%rDepletionFraction
    call Assert( iStat == 0, &
      "Error reading plant stress depletion fraction in irrigation lookup table" )
    write(UNIT=LU_LOG,FMT=*)  "   plant stress depletion fraction: ", &
      pConfig%IRRIGATION(iLandUseIndex)%rDepletionFraction

    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iLandUseIndex)%rGDD_BaseTemp
    call Assert( iStat == 0, &
      "Error reading GDD base temperature in irrigation lookup table" )
    write(UNIT=LU_LOG,FMT=*)  "   GDD base temperature ", &
      pConfig%IRRIGATION(iLandUseIndex)%rGDD_BaseTemp

    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iLandUseIndex)%rGDD_MaxTemp
    call Assert( iStat == 0, &
      "Error reading GDD max temperature in irrigation lookup table" )
    write(UNIT=LU_LOG,FMT=*)  "   GDD max temperature ", &
      pConfig%IRRIGATION(iLandUseIndex)%rGDD_MaxTemp

    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iLandUseIndex)%rMAD
    call Assert( iStat == 0, &
      "Error reading management allowable deficit (MAD) in irrigation lookup table" )
    write(UNIT=LU_LOG,FMT=*)  "   management allowable deficit (MAD) ", &
      pConfig%IRRIGATION(iLandUseIndex)%rMAD

    call chomp(sRecord, sItem, sTAB)
    pConfig%IRRIGATION(iLandUseIndex)%iBeginIrrigation = mmdd2doy(sItem)
    write(UNIT=LU_LOG,FMT=*)  "   irrigation starts on or after day ", &
      pConfig%IRRIGATION(iLandUseIndex)%iBeginIrrigation

    call chomp(sRecord, sItem, sTAB)
    pConfig%IRRIGATION(iLandUseIndex)%iEndIrrigation = mmdd2doy(sItem)
    write(UNIT=LU_LOG,FMT=*)  "   irrigation ends on day ", &
      pConfig%IRRIGATION(iLandUseIndex)%iEndIrrigation


    call chomp(sRecord, sItem, sTAB)
    call uppercase( sItem )
    if ( ( index( sItem, "CAPACITY PLUS") > 0 ) .or. ( index( sItem, "CAPACITY ORIGINAL") > 0 ) )  then
      pConfig%IRRIGATION(iLandUseIndex)%iApplication_Scheme = CONFIG_IRRIGATION_APPLICATION_FIELD_CAPACITY_RZ
    elseif ( index( sItem, "CAPACITY") > 0 ) then
      pConfig%IRRIGATION(iLandUseIndex)%iApplication_Scheme = CONFIG_IRRIGATION_APPLICATION_FIELD_CAPACITY
!      elseif ( str_buffer .contains. "deficit") then
      !   pConfig%IRRIGATION(iLandUseIndex)%iApplication_Scheme = APP_DEFINED_DEFICIT
    elseif ( index( sItem, "CONSTANT") > 0 ) then
      pConfig%IRRIGATION(iLandUseIndex)%iApplication_Scheme = CONFIG_IRRIGATION_APPLICATION_CONSTANT_AMNT
      ! elseif ( str_buffer .contains. "demand") then
      !   pConfig%IRRIGATION(iLandUseIndex)%iApplication_Scheme = APP_HWB_DEMAND_BASED
    else
      pConfig%IRRIGATION(iLandUseIndex)%iApplication_Scheme = CONFIG_IRRIGATION_APPLICATION_NONE
    endif
    select case ( pConfig%IRRIGATION(iLandUseIndex)%iApplication_Scheme )
      case ( CONFIG_IRRIGATION_APPLICATION_NONE )
        sBuf = "'none'"
      case ( CONFIG_IRRIGATION_APPLICATION_CONSTANT_AMNT )
        sBuf = "'constant application amount'"
      case ( CONFIG_IRRIGATION_APPLICATION_FIELD_CAPACITY )
        sBuf = "'replenish soil to field capacity, inefficiencies assumed lost from mass balance'"
      case ( CONFIG_IRRIGATION_APPLICATION_FIELD_CAPACITY_RZ )
        sBuf = "'replenish soil to field capacity, inefficiencies delivered to root zone'"
      case default
        call assert(lFALSE, "Unhandled configuration file option associated with "                   &
          //"'Irrigation Application Scheme' choice", trim(__FILE__), __LINE__ )
    end select
    write(unit=LU_LOG, fmt=*) "  irrigation application scheme is "//trim( sBuf )

    call chomp(sRecord, sItem, sTAB)
    read( unit=sItem, fmt=*, iostat=iStat) pConfig%IRRIGATION(iLandUseIndex)%rIrrigationAmount
    call Assert( iStat == 0, &
      "Error reading irrigation amount in irrigation lookup table" )
    write(unit=LU_LOG, fmt=*) "  irrigation amount per event ", &
      pConfig%IRRIGATION(iLandUseIndex)%rIrrigationAmount


    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) &
      pConfig%IRRIGATION(iLandUseIndex)%rFractionOfIrrigationFromGW
    call Assert( iStat == 0, &
      "Error reading the fraction of irrigation water obtained from groundwater " &
      //"from the irrigation lookup table" )
    write(UNIT=LU_LOG,FMT=*)  "  fraction of irrigation water obtained from groundwater ", &
      pConfig%IRRIGATION(iLandUseIndex)%rFractionOfIrrigationFromGW

    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iLandUseIndex)%rIrrigationEfficiency_GW
    call Assert( iStat == 0, &
      "Error reading the irrigation application efficiency when making use of groundwater " &
      //"from the irrigation lookup table" )
    call assert(pConfig%IRRIGATION(iLandUseIndex)%rIrrigationEfficiency_GW > 0., &
      "Fractional irrigation efficiency for groundwater sources must be greater than 0.", &
      trim(__FILE__),__LINE__)
    call assert(pConfig%IRRIGATION(iLandUseIndex)%rIrrigationEfficiency_GW <= 1., &
      "Fractional irrigation efficiency for groundwater sources must be less than or equal to 1.", &
      trim(__FILE__),__LINE__)
    write(UNIT=LU_LOG,FMT=*)  "  fractional irrigation efficiency for groundwater sources ", &
      pConfig%IRRIGATION(iLandUseIndex)%rIrrigationEfficiency_GW

    call chomp(sRecord, sItem, sTAB)
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iLandUseIndex)%rIrrigationEfficiency_SW
    call Assert( iStat == 0, &
      "Error reading the irrigation application efficiency when making use of surface water " &
      //"from the irrigation lookup table" )
    call assert(pConfig%IRRIGATION(iLandUseIndex)%rIrrigationEfficiency_SW > 0., &
      "Fractional irrigation efficiency for surface-water sources must be greater than 0.", &
      trim(__FILE__),__LINE__)
    call assert(pConfig%IRRIGATION(iLandUseIndex)%rIrrigationEfficiency_SW <= 1., &
      "Fractional irrigation efficiency for surface-water sources must be less than or equal to 1.", &
      trim(__FILE__),__LINE__)
    write(UNIT=LU_LOG,FMT=*)  "  fractional irrigation efficiency for surface-water sources ", &
      pConfig%IRRIGATION(iLandUseIndex)%rIrrigationEfficiency_SW

    iRecNum = iRecNum + 1

  end do LU_READ

  ! That's all!
  close ( unit=LU_LOOKUP )

end subroutine model_ReadIrrigationLookupTable

!--------------------------------------------------------------------------

function rf_model_GetInterception( pConfig, cel ) result(rIntRate)
  !! Looks up the interception value for land-use type iType.

  ! [ ARGUMENTS ]
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  type (T_CELL),pointer :: cel

  ! [ RETURN VALUE ]
  real (kind=c_float) :: rIntRate

  ! [ LOCALS ]
  integer ( kind=c_int )             :: i
  logical ( kind=c_bool )            :: lAssertTest
  type ( T_LANDUSE_LOOKUP ),pointer  :: pLU
  real (kind=c_float)                :: fTempPrecip

  lAssertTest = cel%iLandUseIndex >= 1 .and. cel%iLandUseIndex <= pConfig%iNumberOfLanduses

  if(.not. lAssertTest) &
    call assert(lAssertTest, &
      "Array index out of bounds. Variable is iLandUseIndex with a value of " &
      //trim(int2char(cel%iLandUseIndex)), trim(__FILE__),__LINE__)

  pLU => pConfig%LU(cel%iLandUseIndex)

  fTempPrecip = cel%rGrossPrecip

  ! Default is zero
  rIntRate = rZERO
  if ( cel%iGrowingSeason == iTRUE ) then
    if ( pLU%rIntercept_GrowingSeason_n < 1.0_c_float )  fTempPrecip = fTempPrecip ** pLU%rIntercept_GrowingSeason_n
    rIntRate = pLU%rIntercept_GrowingSeason_a + pLU%rIntercept_GrowingSeason_b * fTempPrecip
  else
    if ( pLU%rIntercept_NonGrowingSeason_n < 1.0_c_float )  fTempPrecip = fTempPrecip ** pLU%rIntercept_NonGrowingSeason_n
    rIntRate = pLU%rIntercept_NonGrowingSeason_a + pLU%rIntercept_NonGrowingSeason_b * fTempPrecip
  end if

  if (rIntRate < rZero) then

    call echolog("Negative interception value encountered. Check your lookup tables." &
      //"~landuse code: "//trim(asCharacter(pLU%iLanduseType)) &
      //"~landuse description: "//trim(pLU%sLanduseDescription) )

    call Assert(lFALSE, "")
  endif

end function rf_model_GetInterception

!--------------------------------------------------------------------------

subroutine model_CheckConfigurationSettings( pGrd, pConfig )

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd               ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig      ! pointer to data structure that contains


  call assert(DAT(FLOWDIR_DATA)%iSourceDataType /= DATATYPE_NA, &
    "No flow direction information has been specified. If you are not" &
    //"~routing flow, add a directive such as 'FLOW_DIRECTION CONSTANT 1'" &
    //" to your ~control file." )

  call assert(DAT(LANDUSE_DATA)%iSourceDataType /= DATATYPE_NA, &
    "No landuse data grid has been specified. If you have only" &
    //"~a single landuse type, add a directive such as 'LANDUSE CONSTANT 21'" &
    //"~to your control file." )

  call assert(DAT(AWC_DATA)%iSourceDataType /= DATATYPE_NA, &
    "No available water capacity grid has been specified. If you do not presently" &
    //"~have an AWC grid, you may add a directive such as 'WATER_CAPACITY CONSTANT 2.6'" &
    //"~to your control file in order to run SWB." )

  call assert(DAT(SOILS_GROUP_DATA)%iSourceDataType /= DATATYPE_NA, &
    "No hydrologic soils group grid has been specified. If you do not presently" &
    //"~have an HSG grid, you may add a directive such as 'HYDROLOGIC_SOIL_GROUP CONSTANT 1'" &
    //"~to your control file in order to run SWB." )

  if (pConfig%lEnableIrrigation .and. pConfig%iConfigureFAO56 == CONFIG_FAO56_NONE ) then
    call assert( lFALSE, "The irrigation module must be used with one of the FAO-56 crop~" &
      //"coefficient submodels enabled. These can be enabled by adding one of the following~" &
      //"to your control file:~"//sTAB//"~" &
      //sTAB//"FAO56 CROP_COEFFICIENTS_ONE_FACTOR_STANDARD~" &
      //sTAB//"FAO56 CROP_COEFFICIENTS_TWO_FACTOR_STANDARD~" &
      //sTAB//"FAO56 CROP_COEFFICIENTS_ONE_FACTOR_NONSTANDARD~" &
      //sTAB//"FAO56 CROP_COEFFICIENTS_TWO_FACTOR_NONSTANDARD~")
  endif

  if ( pConfig%lGriddedData ) then

    call assert( pConfig%iConfigureTemperature /= CONFIG_NONE, &
      "No temperature data have been specified. A data source for both the minumum~" &
      //"and maximum air temperature must be specified in order to run SWB." )

    call assert( pConfig%iConfigurePrecip /= CONFIG_NONE, &
      "No precipitation data have been specified. A data source for ~" &
      //"daily precipitation must be specified in order to run SWB." )

  endif

end subroutine model_CheckConfigurationSettings

!--------------------------------------------------------------------------

subroutine model_setInactiveCells( pGrd, pConfig )

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd               ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig      ! pointer to data structure that contains

  where ( pGrd%Cells%iSoilGroup <= 0 .or. &
          pGrd%Cells%iFlowDir < 0 .or. &
          pGrd%Cells%iLandUse <= 0)

    pGrd%iMask = iINACTIVE_CELL

  elsewhere

    pGrd%iMask = iACTIVE_CELL

  endwhere

  pGenericGrd_sgl%rData = real( pGrd%iMask, kind=c_float )
  call grid_WriteGrid( &
   sFilename="ACTIVE_MODEL_CELLS"//".asc", pGrd=pGenericGrd_sgl, iOutputFormat=OUTPUT_ARC)

  call echolog("Finished converting cells with missing data (negative values) to inactive cells." &
    //"~ A total of "//trim(asCharacter(count(pGrd%iMask==iINACTIVE_CELL))) &
    //" cells were inactivated out of "//trim(asCharacter(pConfig%iNumGridCells))//" cells.")

end subroutine model_setInactiveCells

!--------------------------------------------------------------------------

subroutine model_InitializeDataStructures( pGrd, pConfig )

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd               ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig      ! pointer to data structure that contains

  call DAT(FLOWDIR_DATA)%getvalues( pGrdBase=pGrd)
  pGrd%Cells%iFlowDir = pGrd%iData

  pGenericGrd_int%iData = pGrd%Cells%iFlowDir

  where(pGenericGrd_int%iData< 0)
    pGenericGrd_int%iMask = iINACTIVE_CELL
  elsewhere
    pGenericGrd_int%iMask = iACTIVE_CELL
  endwhere

  call grid_WriteGrid(sFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Flow_Direction_Grid" // &
    "."//trim(pConfig%sOutputFileSuffix), pGrd=pGenericGrd_int, iOutputFormat=pConfig%iOutputFormat )

  call make_shaded_contour(pGrd=pGenericGrd_int, &
     sOutputFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Flow_Direction_Grid.png", &
     sTitleTxt="D8 Flow Direction Grid", &
     sAxisTxt="Flow Direction" )

  call DAT(SOILS_GROUP_DATA)%getvalues( pGrdBase=pGrd)
  pGrd%Cells%iSoilGroup = pGrd%iData

  where(pGenericGrd_int%iData< 0)
    pGenericGrd_int%iMask = iINACTIVE_CELL
  elsewhere
    pGenericGrd_int%iMask = iACTIVE_CELL
  endwhere

  pGenericGrd_int%iData = pGrd%Cells%iSoilGroup
  call grid_WriteGrid(sFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Hydrologic_Soils_Group" // &
    "."//trim(pConfig%sOutputFileSuffix), pGrd=pGenericGrd_int, iOutputFormat=pConfig%iOutputFormat )

  call make_shaded_contour(pGrd=pGenericGrd_int, &
      sOutputFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Hydrologic_Soils_Group.png", &
      sTitleTxt="Hydrologic Soils Group", &
      sAxisTxt="HSG", &
      rMinZVal=0.0 )

  call DAT(AWC_DATA)%getvalues( pGrdBase=pGrd)
  pGrd%Cells%rSoilWaterCapInput = pGrd%rData

  write(LU_LOG, fmt="(a, f14.3)") "  Minimum AWC: ", minval(pGrd%Cells%rSoilWaterCapInput)
  write(LU_LOG, fmt="(a, f14.3)") "  Maximum AWC: ", maxval(pGrd%Cells%rSoilWaterCapInput)

  pGenericGrd_sgl%rData = pGrd%Cells%rSoilWaterCapInput

  where(pGenericGrd_sgl%rData< 0)
    pGenericGrd_sgl%iMask = iINACTIVE_CELL
  elsewhere
    pGenericGrd_sgl%iMask = iACTIVE_CELL
  endwhere

  call grid_WriteGrid(sFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Available_Water_Capacity" // &
    "."//trim(pConfig%sOutputFileSuffix), pGrd=pGenericGrd_sgl, iOutputFormat=pConfig%iOutputFormat )

  call make_shaded_contour(pGrd=pGenericGrd_sgl, &
     sOutputFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Available_Water_Capacity.png", &
     sTitleTxt="Available Water Capacity", &
     sAxisTxt="AWC (inches per foot)" )

  if (DAT(ROUTING_FRAC_DATA)%iSourceDataType /= DATATYPE_NA) then

    call DAT(ROUTING_FRAC_DATA)%getvalues( pGrdBase=pGrd)
    pGrd%Cells%rRouteFraction = pGrd%rData

    write(LU_LOG, fmt="(a, f14.3)") "  Minimum routing fraction: ", minval(pGrd%Cells%rRouteFraction)
    write(LU_LOG, fmt="(a, f14.3)") "  Maximum routing fraction: ", maxval(pGrd%Cells%rRouteFraction)

    pGenericGrd_sgl%rData = pGrd%Cells%rRouteFraction

    where(pGenericGrd_sgl%rData< 0)
      pGenericGrd_sgl%iMask = iINACTIVE_CELL
    elsewhere
      pGenericGrd_sgl%iMask = iACTIVE_CELL
    endwhere

    call grid_WriteGrid(sFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Routing_Fraction" // &
      "."//trim(pConfig%sOutputFileSuffix), pGrd=pGenericGrd_sgl, iOutputFormat=pConfig%iOutputFormat )

    call make_shaded_contour(pGrd=pGenericGrd_sgl, &
       sOutputFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Routing_Fraction.png", &
       sTitleTxt="Routing Fraction", &
       sAxisTxt="Routing Fraction (unitless)" )

  endif

  if (DAT(IRRIGATED_LAND_MASK_DATA)%iSourceDataType == DATATYPE_NA) then

    call DAT(IRRIGATED_LAND_MASK_DATA)%initialize(sDescription="Irrigated land mask data", &
      iConstant=1_c_int )

  endif


  if (DAT(MASK_DATA)%iSourceDataType /= DATATYPE_NA) then

    call DAT(MASK_DATA)%getvalues( pGrdBase=pGrd )

    where ( pGrd%iData > 0 )
      pGrd%iMask = iACTIVE_CELL
    elsewhere
      pGrd%iMask = iINACTIVE_CELL
    endwhere

    pGenericGrd_int%iData = pGrd%iData
    call grid_WriteGrid(sFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Basin_Mask" // &
      "."//trim(pConfig%sOutputFileSuffix), pGrd=pGenericGrd_int, iOutputFormat=pConfig%iOutputFormat )

    call make_shaded_contour(pGrd=pGenericGrd_int, &
       sOutputFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Basin_mask.png", &
       sTitleTxt="Input Basin Mask", &
       sAxisTxt="Basin Mask (unitless)" )

  endif

end subroutine model_InitializeDataStructures

!--------------------------------------------------------------------------

subroutine model_InitializeInputAndOutput( pGrd, pConfig )

    ! [ ARGUMENTS ]
    type ( T_GENERAL_GRID ),pointer :: pGrd               ! pointer to model grid
    type (T_MODEL_CONFIGURATION), pointer :: pConfig      ! pointer to data structure that contains

   ! [ LOCALS ]
   integer (kind=c_int) :: iStat

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

end subroutine model_InitializeInputAndOutput

!----------------------------------------------------------------------

subroutine model_InitializeRunoff( pGrd, pConfig )

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd               ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig      ! pointer to data structure that contains


  ! If we are routing water, we *must* call InitializeFlowDirection
  if( pConfig%iConfigureRunoffMode /= CONFIG_RUNOFF_NO_ROUTING) then
    write(UNIT=LU_LOG,FMT=*)  "model.F90: model_InitializeFlowDirection"
    call model_InitializeFlowDirection( pGrd , pConfig)
  end if

  ! Are we solving using the downhill algorithm?
  if ( pConfig%iConfigureRunoffMode == CONFIG_RUNOFF_DOWNHILL ) then
    ! if a routing table exists, read it in; else initialize and
    ! save the routing table for future use
    write(UNIT=LU_LOG,FMT=*)  "model.F90: model_ConfigureRunoffDownhill"
    call model_ConfigureRunoffDownhill( pGrd, pConfig)
  end if

end subroutine model_InitializeRunoff

!----------------------------------------------------------------------

subroutine model_InitializeLanduseRelatedParams( pGrd, pConfig )

  type ( T_GENERAL_GRID ),pointer :: pGrd          ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings

  write(UNIT=LU_LOG,FMT=*)  "model.F90: model_CreateLanduseIndex"
  flush(unit=LU_LOG)
  call model_CreateLanduseIndex(pGrd, pConfig )

  write(UNIT=LU_LOG,FMT=*) "model.F90: calling model_InitializeSM"
  flush(unit=LU_LOG)
  call model_InitializeSM(pGrd, pConfig)

  write(UNIT=LU_LOG,FMT=*)  "model.F90: runoff_InitializeCurveNumber"
  flush(unit=LU_LOG)
  call runoff_InitializeCurveNumber( pGrd ,pConfig)

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
      call Assert( .false._c_bool, "No ET configuration was specified" )
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
integer (kind=c_int),intent(in) :: iDayOfYear, iNumDaysInYear
real (kind=c_float),intent(in) :: rRH,rMinRH,rWindSpd,rSunPct

  ! [ LOCALS ]
  type (T_CELL),pointer :: cel                      ! pointer to a particular cell
  integer (kind=c_int) :: iCol, iRow
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry

  select case ( pConfig%iConfigureET )
    case ( CONFIG_ET_NONE )
      call Assert( .false._c_bool, "No ET configuration was specified" )
    case ( CONFIG_ET_THORNTHWAITE_MATHER )
      call et_tm_ComputeET ( pGrd, pConfig, iDayOfYear )
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


! if the ground is still frozen, we're not going to consider ET to be
! possible.
!where (pGrd%Cells%rCFGI > rNEAR_ZERO)
!  pGrd%Cells%rReferenceET0 = rZERO
!endwhere

! in order to integrate Thornthwaite-Mather approach with FAO56 approach,
! an adjusted reference ET0 is now defined... must populate this
pGrd%Cells%rReferenceET0_adj = pGrd%Cells%rReferenceET0

end subroutine model_ProcessET

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
  integer (kind=c_int) :: iCol,iRow,k
  type ( T_CELL ),pointer :: cel            ! pointer to cell data structure
  type ( T_LANDUSE_LOOKUP ),pointer :: pLU  ! pointer to landuse data structure
  logical ( kind=c_bool ) :: lMatch

  ! [ LOCAL PARAMETERS ]

  if(pConfig%iConfigureSMCapacity==CONFIG_SM_CAPACITY_CALCULATE) then
    ! Update the soil-water capacity based on land-cover and soil type
    do iRow=1,pGrd%iNY
      do iCol=1,pGrd%iNX

        lMatch = lFALSE
        cel => pGrd%Cells(iCol,iRow)

        if (pGrd%iMask(iCol, iRow) == iINACTIVE_CELL) cycle

        ! loop over all LAND use types...
        do k=1,size(pConfig%LU,1)
          pLU => pConfig%LU(k)
          if ( pLU%iLandUseType == cel%iLandUse ) then

            !> must guard against segfaulting due to illegal values of
            !> soil type
            if (cel%iSoilGroup > uBound(pConfig%ROOTING_DEPTH,2) &
                 .or. cel%iSoilGroup < lBound(pConfig%ROOTING_DEPTH,2) ) then

              call assert(lFALSE, &
                 "Soil group value is out of bounds: " &
                 //"col: "//trim(asCharacter(iCol)) &
                 //"  row: "//trim(asCharacter(iRow)) &
                 //"  value: "//trim(asCharacter(cel%iSoilGroup)), &
                 trim(__FILE__), __LINE__)
            endif

            cel%rSoilWaterCap = cel%rSoilWaterCapInput * pConfig%ROOTING_DEPTH(k,cel%iSoilGroup)
            lMatch=lTRUE
            exit
          end if
        end do

        if(.not. lMATCH) then
          call Assert(lFALSE,&
            "Failed to match landuse grid with landuse table during soil moisture initialization~" &
            //" Row: "//trim(int2char(iRow))//"  Col: "//trim(int2char(iCol)) &
            //"  cell LU: "//trim(int2char(int(cel%iLandUse, kind=c_int) ) ) )
        endif
      end do
    end do
  end if

  select case ( pConfig%iConfigureSM )

    case ( CONFIG_SM_NONE )
      call Assert( lFALSE, "No soil moisture calculation method was specified" )
    case ( CONFIG_SM_TM_LOOKUP_TABLE )
      call sm_thornthwaite_mather_Initialize ( pGrd, pConfig )
    case ( CONFIG_SM_TM_EQUATIONS )
      call sm_thornthwaite_mather_Initialize ( pGrd, pConfig )
    case default

  end select

end subroutine model_InitializeSM

!--------------------------------------------------------------------------

subroutine model_InitializeMaxRecharge(pGrd, pConfig )

  type ( T_GENERAL_GRID ),pointer :: pGrd         ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings

  ! [ LOCALS ]
  integer (kind=c_int) :: iCol,iRow,k
  type ( T_CELL ),pointer :: cel            ! pointer to cell data structure
  type ( T_LANDUSE_LOOKUP ),pointer :: pLU  ! pointer to landuse data structure
  logical ( kind=c_bool ) :: lMatch

  if(pConfig%iConfigureMaxRecharge == CONFIG_MAX_RECHARGE_TABLE ) then
    ! Update the maximum recharge based on land-cover and soil type
    do iRow=1,pGrd%iNY
      do iCol=1,pGrd%iNX

        lMatch = lFALSE
        cel => pGrd%Cells(iCol,iRow)

        if (pGrd%iMask(iCol, iRow) == iINACTIVE_CELL) cycle

        ! loop over all LAND use types...
        do k=1,size(pConfig%LU,1)
          pLU => pConfig%LU(k)
          if ( pLU%iLandUseType == cel%iLandUse ) then

            !> must guard against segfaulting due to illegal values of
            !> soil type
            if (cel%iSoilGroup > uBound(pConfig%MAX_RECHARGE,2) &
                 .or. cel%iSoilGroup < lBound(pConfig%MAX_RECHARGE,2) ) then

              call assert(lFALSE, &
                 "Soil group value is out of bounds: " &
                 //"col: "//trim(asCharacter(iCol)) &
                 //"  row: "//trim(asCharacter(iRow)) &
                 //"  value: "//trim(asCharacter(cel%iSoilGroup)), &
                 trim(__FILE__), __LINE__)
            endif

            cel%rMaximumRechargeRate = pConfig%MAX_RECHARGE(k,cel%iSoilGroup)
            lMatch=lTRUE
            exit
          end if
        end do

        if(.not. lMATCH) then
          call Assert(lFALSE,&
            "Failed to match landuse grid with landuse table during maximum recharge rate initialization~" &
            //" Row: "//trim(int2char(iRow))//"  Col: "//trim(int2char(iCol)) &
            //"  cell LU: "//trim(int2char(int(cel%iLandUse, kind=c_int) ) ) )
        endif
      end do
    end do

  elseif (DAT(MAXIMUM_RECHARGE_RATE_DATA)%iSourceDataType /= DATATYPE_NA) then

    call DAT(MAXIMUM_RECHARGE_RATE_DATA)%getvalues( pGrdBase=pGrd)
    pGrd%Cells%rMaximumRechargeRate = pGrd%rData

  endif

  ! regardless of source of maximum recharge rate values, we want to summarize the values
  ! that SWB will be using
  write(LU_LOG, fmt="(a, f14.3)") "  Minimum maximum recharge rate: ", minval(pGrd%Cells%rMaximumRechargeRate)
  write(LU_LOG, fmt="(a, f14.3)") "  Maximum maximum recharge rate: ", maxval(pGrd%Cells%rMaximumRechargeRate)

  pGenericGrd_sgl%rData = pGrd%Cells%rMaximumRechargeRate

  where(pGenericGrd_sgl%rData< 0)
    pGenericGrd_sgl%iMask = iINACTIVE_CELL
  elsewhere
    pGenericGrd_sgl%iMask = iACTIVE_CELL
  endwhere

  call grid_WriteGrid(sFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Maximum_Recharge_Rate" // &
    "."//trim(pConfig%sOutputFileSuffix), pGrd=pGenericGrd_sgl, iOutputFormat=pConfig%iOutputFormat )

  call make_shaded_contour(pGrd=pGenericGrd_sgl, &
     sOutputFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Maximum_Recharge_Rate.png", &
     sTitleTxt="Maximum Recharge Rate", &
     sAxisTxt="Maximum Recharge Rate (inches)" )

end subroutine model_InitializeMaxRecharge



subroutine model_CreateLanduseIndex(pGrd, pConfig )

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd         ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  ! [ LOCALS ]
  integer (kind=c_int) :: iCol,iRow,k
  type ( T_CELL ),pointer :: cel            ! pointer to cell data structure

  type ( T_LANDUSE_LOOKUP ),pointer :: pLU  ! pointer to landuse data structure
  logical ( kind=c_bool ) :: lMatch

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX

      if (pGrd%iMask(iCol, iRow) == iINACTIVE_CELL) cycle

      lMatch = lFALSE
      cel => pGrd%Cells(iCol,iRow)

      do k=1,size(pConfig%LU,1)
        pLU => pConfig%LU(k)
        if ( pLU%iLandUseType == cel%iLandUse ) then
          ! save index of matching landuse for ease of processing land use properties later
          cel%iLandUseIndex = k
 !         ! need to ensure that the soil type doesn't exceed
 !         ! the max number of soil types or we get a core dump
          call Assert(cel%iSoilGroup <= size(pConfig%MAX_RECHARGE,2), &
             "Value in soil type grid exceeds the maximum " &
             // "number of soil types in the land use lookup table.", &
             trim(__FILE__),__LINE__)
 !         cel%rMaxRecharge = pConfig%MAX_RECHARGE(k,INT(cel%iSoilGroup,kind=c_int))
          lMatch=lTRUE
          exit
        end if
      end do
      if(.not. lMATCH) then
        call echolog ("iRow: "//trim(asCharacter(iRow))//"  iCol: "//trim(asCharacter(iCol)) &
          //"  cell LU: "//trim(asCharacter( cel%iLandUse )) )
        call Assert(lFALSE,&
          "Failed to match landuse grid with landuse table during creation of landuse indices", &
          trim(__FILE__),__LINE__)
      endif
    end do
  end do

end subroutine model_CreateLanduseIndex

!--------------------------------------------------------------------------------------------

subroutine model_CreateIrrigationTableIndex(pGrd, pConfig )

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd         ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  ! [ LOCALS ]
  integer (kind=c_int) :: iCol,iRow,j
  type ( T_CELL ),pointer :: cel            ! pointer to cell data structure
  logical ( kind=c_bool ) :: lMatch

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX

      cel => pGrd%Cells(iCol,iRow)

      if ( pGrd%iMask(iCol, iRow) == iINACTIVE_CELL ) cycle

      lMatch = lFALSE
      do j=1,size(pConfig%IRRIGATION,1)
        if(cel%iLanduse == pConfig%IRRIGATION(j)%iLandUseType) then
          cel%iIrrigationTableIndex = j
          lMatch = lTRUE
          exit
        endif
      enddo

      call assert(lMatch, "Unknown landuse code found while reading from the " &
        //"crop coefficient and irrigation parameters table.~Landuse specified "&
        //"in the landuse grid but not found in the irrigation table.~ " &
        //"  Landuse grid value: "//trim(int2char(cel%iLanduse)),trim(__FILE__), __LINE__)

		enddo
	enddo

end subroutine model_CreateIrrigationTableIndex

!--------------------------------------------------------------------------

subroutine model_dumpvals(pGrd, pConfig)

  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains

  ! [ LOCALS ]
  type ( T_CELL ),pointer :: cel            ! pointer to cell data structure
  logical                 :: file_is_open
  integer (kind=c_int)    :: idx

  do idx=1, ubound( DUMP_VARS, 1)

    inquire(unit=DUMP_VARS( idx )%file_unit, opened=file_is_open )

    if (       file_is_open                                                     &
         .and. ( DUMP_VARS( idx )%column_num > 0 )                              &
         .and. ( DUMP_VARS( idx )%column_num <= ubound( pGrd%Cells, 1) )        &
         .and. ( DUMP_VARS( idx )%row_num > 0 )                                 &
         .and. ( DUMP_VARS( idx )%row_num <= ubound( pGrd%Cells, 2) )  ) then

      cel => pGrd%Cells( DUMP_VARS( idx )%column_num, DUMP_VARS( idx )%row_num )

      write( DUMP_VARS( idx )%file_unit, "(i2,',',i2,',',i4,',',5(i12,','),39(f12.3,','),f12.3 )") pConfig%iMonth, &
        pConfig%iDay,                                                                                              &
        pConfig%iYear, cel%iLandUse, cel%iLandUseIndex, cel%iSoilGroup, cel%iNumUpslopeConnections,                &
        cel%iSumUpslopeCells, cel%rTMin, cel%rTMax, cel%rTAvg,                                                     &
        cel%rCFGI, cel%rGDD, cel%rCurrentRootingDepth, cel%rGrossPrecip, cel%rInterception,        &
        cel%rNetRainfall, cel%rSnowCover,                                                                          &
        cel%rSnowMelt, cel%rIrrigationAmount, cel%rIrrigationFromGW, cel%rIrrigationFromSW,                        &
        cel%rKcb, cel%rCropETc, cel%rBareSoilEvap, cel%rTotalAvailableWater, cel%rReadilyAvailableWater,           &
        cel%rReferenceET0,                                                                                         &
        cel%rActualET, cel%rReferenceET0_adj,                                                                      &
        cel%rKe, cel%rKs, cel%rKr,                                                                                 &
        cel%rSoilWaterCap, cel%rSoilMoisture, cel%rAdjCN, cel%rSMax, cel%rInflow, cel%rRunoff, &
        cel%rOutflow, cel%rFlowOutOfGrid,           &
        cel%rDailyRecharge, cel%rRejectedRecharge, cel%rNetInflowBuf(1), cel%rNetInflowBuf(2),                     &
        cel%rNetInflowBuf(3), cel%rNetInflowBuf(4), cel%rNetInflowBuf(5)

      flush( DUMP_VARS( idx )%file_unit )

    endif

  enddo

end subroutine model_dumpvals


subroutine model_WriteGrids(pGrd, pConfig, iOutputType)
!! Writes the monthly output arrays in the proper grid format
! [ ARGUMENTS ]
type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
  ! model options, flags, and other settings
integer (kind=c_int), intent(in) :: iOutputType

  ! [ LOCALS ]
  real (kind=c_double) :: xmin,xmax,ymin,ymax
  character (len=256) sBufOut,sBufFuture,sBufSuffix,sDayText,sMonthText, &
    sYearText, sDateText

  sBufOut = trim(pConfig%sOutputFilePath)//trim(pConfig%sOutputFilePrefix) &
       //trim( YEAR_INFO(pConfig%iMonth)%sName )
  sBufFuture = trim(pConfig%sFutureFilePath)//trim(pConfig%sFutureFilePrefix)
  sBufSuffix = trim(pConfig%sOutputFileSuffix)

  write(sDateText,fmt="(i4,'/',i2.2,'/',i2.2)") pConfig%iYear,pConfig%iMonth,pConfig%iDay
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
       pGrd=pGenericGrd_sgl, iOutputFormat=pConfig%iOutputFormat)
     call make_shaded_contour(pGrd=pGenericGrd_sgl, &
           sOutputFilename="MASS_BALANCE"//trim(sDayText)//".png", &
           sTitleTxt="MASS BALANCE ERROR AMOUNTS: "//trim(sDateText), &
           sAxisTxt="INCHES" )


   elseif ( iOutputType == WRITE_ASCII_GRID_ANNUAL ) then

     pGenericGrd_sgl%rData = pGrd%Cells%rSoilMoisturePct
     call grid_WriteGrid(sFilename=trim(sBufFuture) // "final_pct_sm" // &
     trim(sYearText) // "." //trim(sBufSuffix), &
       pGrd=pGenericGrd_sgl, iOutputFormat=pConfig%iOutputFormat)

     pGenericGrd_sgl%rData = pGrd%Cells%rSnowCover
     call grid_WriteGrid(sFilename=trim(sBufFuture) // "final_snow_cover" // &
       trim(sYearText) // "." //trim(sBufSuffix), &
       pGrd=pGenericGrd_sgl, iOutputFormat=pConfig%iOutputFormat )

   elseif ( iOutputType == WRITE_ASCII_GRID_DAILY ) then

   elseif ( iOutputType == WRITE_ASCII_GRID_MONTHLY ) then

   elseif ( iOutputType == WRITE_ASCII_GRID_DIAGNOSTIC ) then

   elseif ( iOutputType == WRITE_ASCII_GRID_DEBUG ) then

   end if

end subroutine model_WriteGrids

!> This subroutine reads a single line from a single-station
!> climate data file, parses the values, and returns a pointer to a
!> time-series data object.
!> @todo Make logic at end of routine more robust; currently the logic
!> to test for the presence of a header could mask errors reading in
!> values from the data file.
subroutine model_ReadTimeSeriesFile(pConfig, pTS)

  type (T_MODEL_CONFIGURATION), pointer :: pConfig      ! pointer to data structure that contains
  type (T_TIME_SERIES_FILE), pointer :: pTS

  ! [ LOCALS ]
  character(len=256) :: sBuf
  integer (kind=c_int) :: iStat
  real (kind=c_float) :: rMaxRH

  do

    ! read line from the time series file
    read ( unit=LU_TS, fmt="(a256)", iostat=iStat ) sBuf

    ! check for end-of-file condition
    if ( is_iostat_end(iStat) ) then
      pTS%lEOF = lTRUE
      ! if we have enabled STRICT_DATE_CHECKING, terminate the run

#ifdef STRICT_DATE_CHECKING
      if(.not. (pConfig%iMonth == 12 .and. pConfig%iDay == 31)) then
        write(unit=LU_LOG,FMT=*) "Time series file ends prematurely:"
        write(unit=LU_LOG,FMT=*) "  file = "//TRIM(pConfig%sTimeSeriesFilename)
        write(unit=sBuf,FMT=*) "Time series file ends prematurely: " &
           //TRIM(pConfig%sTimeSeriesFilename)
        call Assert(lFALSE,TRIM(sBuf),TRIM(__FILE__),__LINE__)
      end if
#endif

      exit ! END OF FILE; exit main do loop
    end if

    ! check for errors
    call Assert ( iStat == 0, "Problems reading time series file: " &
           //TRIM(pConfig%sTimeSeriesFilename), TRIM(__FILE__),__LINE__)

    ! Ignore comment statements and blank lines
    if ( sBuf(1:1) == '#' ) cycle
    if (len_trim(sBuf) == 0 ) cycle

    ! eliminate punctuation
    call CleanUpCsv ( sBuf )
    read ( unit=sBuf, fmt=*, iostat=iStat ) pTS%iMonth, pTS%iDay, &
      pTS%iYear, pTS%rAvgT, pTS%rPrecip, pTS%rRH, pTS%rMaxT, pTS%rMinT, &
      pTS%rWindSpd, pTS%rMinRH, pTS%rSunPct
    if (iStat /= 0) then  ! this is a sloppy way of getting past the header
      write(UNIT=LU_LOG,FMT=*) "Skipping: ",trim(sBuf)
      write(UNIT=LU_LOG,FMT=*)
      cycle
    end if

    if ( pTS%rMaxT< -100. .or. pTS%rMinT < -100. ) then
      call echolog( "Suspiciously low air temperature data detected in climate file. ~" &
        //"Input: "//TRIM(sBuf))
      call Assert(lFALSE, "",TRIM(__FILE__),__LINE__)
    end if

    if ( pTS%rMaxT < pTS%rMinT ) then
      call echolog( "Maximum daily air temperature less than the minimum air temperature in climate file. ~" &
        //"Input: "//TRIM(sBuf))
      call Assert(lFALSE, "",TRIM(__FILE__),__LINE__)
    end if

    if( pTS%rPrecip < 0.) then
      call echolog( "Negative precipitation value in climate file. ~" &
        //"Input: "//TRIM(sBuf))
      call Assert(lFALSE, "",TRIM(__FILE__),__LINE__)
    end if

    if(.not. pConfig%lHaltIfMissingClimateData) then
      if(pTS%rMinRH < 0.0 .or. pTS%rMinRH > 100.0) then
        pTS%rMinRH = minimum_rel_hum(pTS%rMinT, pTS%rMaxT)
      endif

      if(pTS%rRH < 0.0 .or. pTS%rRH > 100.0) then
        rMaxRH = maximum_rel_hum(pTS%rMinT)
        pTS%rRH = ( rMaxRH + pTS%rMinRH ) / 2_c_float
      endif

     if(pTS%rSunPct < 0.0 .or. pTS%rSunPct > 100.0) then
        pTS%rSunPct = estimate_percent_of_possible_sunshine(pTS%rMaxT, pTS%rMinT)
     endif

     if(pTS%rWindSpd < 0.0 .or. pTS%rWindSpd > 50.) then
       pTS%rWindSpd = 2_c_float
     endif

    endif

    ! Check to ensure that we have not skipped a day
    ! we have to ignore the very first day because when running while using
    ! a single-site file, the pConfig values are populated with the
    ! values read from the time series file. Thus, this test will always
    ! be true on the first day of the simulation when reading from a
    ! single-site file.

    if (.not. pConfig%lFirstDayOfSimulation) then

      if(.not. ( (pConfig%iYear == pTS%iYear) &
         .and.   (pConfig%iMonth == pTS%iMonth) &
         .and.   (pConfig%iDay == pTS%iDay) ) ) then
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
      endif

    endif

    exit

  end do

end subroutine model_ReadTimeSeriesFile

end module model
