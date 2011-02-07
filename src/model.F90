module model
!!****h* SWB/model
! NAME
!
!   model.f95 - Main program module - Handles initialization and execution
!               of process modules.
!
! SYNOPSIS
!   Module model coordinates execution of all process modules that make up
!   the SWB model. Initializes model configuration parameters, allocates memory to store
!   intermediate calculation results, and handles execution of
!   process modules.
!
! NOTES
!   The first section of this code reads the model control file and
!   sets model configuration parameters. Program control shifts to the
!   model_Solve subroutine once the code encounters a SOLVE statement
!   in the control file.
!
!!***

  use types
  use swb_grid
  use swb_stats
  use runoff_curve_number
  use et_thornthwaite_mather
  use et_turc
  use et_hargreaves
  use et_jensen_haise
  use et_blaney_criddle
  use sm_thornthwaite_mather
  use snow
  use irrigation

#ifdef NETCDF_SUPPORT
  use netcdf_support
#endif

  implicit none

  !! Counter for moving average water inputs
  integer (kind=T_INT) :: iDayCtr

  !! For the "downhill" solution
  integer (kind=T_INT) :: iOrderCount
  integer (kind=T_INT), dimension(:), allocatable :: iOrderCol
  integer (kind=T_INT), dimension(:), allocatable :: iOrderRow
  real(kind=T_SGL) :: rStartTime,rEndTime

contains

!--------------------------------------------------------------------------
!!****s* model/model_Run
! NAME
!   model_Run - Reads model control file and initializes model configuration
!               parameters and flags.
!
! SYNOPSIS
!   Reads model control file and initializes model configuration
!   parameters and flags. Multiple calls to model_Solve are accomodated
!   for multiple-year model runs.
!
! INPUTS
!    sControlFile - Name of the model control file; passed from main.f95.
!
! OUTPUTS
!   NONE
!
!!***

subroutine model_Run(sControlFile)

  ! [ARGUMENTS]
  character (len=*), intent(in) :: sControlFile
  ! [LOCALS]
  type (T_GENERAL_GRID),pointer :: input_grd      ! Temporary grid for I/O
  type (T_GENERAL_GRID),pointer :: pGrd      ! Grid of model cells
  type (T_GRAPH_CONFIGURATION), dimension(:),pointer :: pGraph
                                            ! pointer to data structure that
                                                   ! holds parameters for creating
                                                   ! DISLIN plots
#ifdef NETCDF_SUPPORT
  type (T_NETCDF_FILE), pointer :: pNC            ! pointer to struct containing NetCDF info
#endif

  type (T_TIME_SERIES_FILE), pointer :: pTSt

  type (T_SSF_FILES), dimension(:), pointer :: pSSF      ! pointer to struct containing SSF file info
  logical (kind=t_LOGICAL), save :: lNO_SSF_FILES = lTRUE
  logical (kind=T_LOGICAL) :: lOpened
  integer (kind=T_INT) :: iOldSize, iNewSize
  integer (kind=T_INT) :: iRowNum, iColNum
  logical (kind=T_LOGICAL) :: lMatch

  character (len=256) :: sRecord                  ! Input file text buffer
  character (len=256) :: sItem                    ! Key word read from sRecord
  character (len=256) :: sOption                  ! Key word read from sRecord
  character (len=256) :: sArgument                ! Key word read from sRecord
  character (len=256) :: sDateStr, sDateStrPretty ! hold date and time of initiation of run
  character(len=256) :: sBuf
  integer (kind=T_INT) :: iNX                     ! Number of cells in the x-direction
  integer (kind=T_INT) :: iNY                     ! Number of cells in the y-direction
  integer (kind=T_INT) :: iValue                  ! Temporary value for 'CONSTANT'
  integer (kind=T_INT) :: iStat                   ! For 'iostat=' checks
  integer (kind=T_INT) :: i,iVarNum,iTimeFrame    ! loop counters
  integer (kind=T_INT) :: idx                    ! index counter for STREAM_CAPTURE routines
  integer (kind=T_INT) :: iLU_SSF = 300          ! last LU for SSF file
  real (kind=T_DBL) :: rX0, rY0                  ! Model grid extent (lower-left)
  real (kind=T_DBL) :: rX1, rY1                  ! Model grid extent (upper-right)
  real (kind=T_DBL) :: rXp, rYp                  ! Coordinates at a point within a grid
  real (kind=T_SGL) :: rGridCellSize             ! Model grid cell size
  real (kind=T_SGL) :: rValue                    ! Temporary value for 'CONSTANT'
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  character (len=8) :: sDate
  character (len=10) :: sTime
  character (len=5) :: sTZ

  call date_and_time(sDate,sTime,sTZ)

  write(sBuf,FMT=*) "SWB_LOGFILE_"//sDate//"_"//sTime(1:6)//".txt"

  ! open up the log file
  open(unit=LU_LOG, file=TRIM(ADJUSTL(sBuf)),iostat=iStat,&
      status='REPLACE')
  call Assert(iStat == 0, &
      "Problem opening log file file for output.")

  write(UNIT=LU_LOG,FMT=*) "Soil Water Balance Code compiled on: "// &
    TRIM(__DATE__) //" "// TRIM(__TIME__)
  write(UNIT=LU_LOG,FMT=*) "Model execution started: "// &
    sDate//"_"//sTime(1:6)

  ALLOCATE (pConfig, STAT=iStat)
  call Assert( iStat == 0, &
     "Could not allocate memory for model control data structure")

  ALLOCATE (pGraph(iNUM_VARIABLES), STAT=iStat)
  call Assert( iStat == 0, &
     "Could not allocate memory for graph control data structure")

#ifdef NETCDF_SUPPORT
  ALLOCATE (pConfig%NETCDF_FILE(iNUM_VARIABLES,3), STAT=iStat)
  call Assert( iStat == 0, &
     "Could not allocate memory for NETCDF file info data structure")
#endif

  ALLOCATE (pTSt, STAT=iStat)
  call Assert( iStat == 0, &
     "Could not allocate memory for time-series data structure")

  write (UNIT=LU_LOG,FMT=*) "Running ",trim(sControlFile)

  ! Initialize the module variables - set default program options

  pConfig%sOutputFilePrefix = "swb"
  pConfig%sFutureFilePrefix = "swb_future"
  pConfig%sTimeSeriesFilename = ""
  pConfig%sLanduseLookupFilename = ""
  pConfig%iOutputFormat = OUTPUT_ARC
  iDayCtr = 0
  pConfig%iConfigureET = CONFIG_ET_NONE
  pConfig%iConfigureRunoff = CONFIG_RUNOFF_CURVE_NUMBER
  pConfig%iConfigureRunoffMode = CONFIG_RUNOFF_DOWNHILL
  pConfig%iConfigurePrecip = CONFIG_PRECIP_SINGLE_STATION
  pConfig%iConfigureTemperature = CONFIG_TEMPERATURE_SINGLE_STATION
  pConfig%iConfigureSMCapacity = CONFIG_SM_CAPACITY_CALCULATE
  pConfig%iConfigureSnow = CONFIG_SNOW_ORIGINAL_SWB
  pConfig%iConfigureInitialAbstraction = CONFIG_SM_INIT_ABSTRACTION_TR55
  pConfig%iConfigureMissingData = CONFIG_END_IF_MISSING_DATA
  pConfig%sOutputFileSuffix = "grd"
  pConfig%iHeaderPrintInterval = 7
  pConfig%lWriteToScreen = lTRUE
  pConfig%lReportDaily = lTRUE

#ifdef STREAM_INTERACTIONS
  pConfig%rStreamMaxInflow = rBIGVAL
  pConfig%rStreamMaxCapture = rBIGVAL
#endif

  iVarNum = -99999

  ! Attempt to open CONTROL file
  open ( unit=LU_CONTROL, file=sControlFile, status="OLD", iostat=iStat )
  call Assert( iStat == 0, "Cannot open control file " // sControlFile )

  ! Make sure the grids have not been initialized...
  pGrd => null()
  input_grd => null()

  ! check to see if an existing downhill flow routing table exists
  INQUIRE( FILE='swb_routing.bin', EXIST=pConfig%lDownhillRoutingTableExists)
  if(pConfig%lDownhillRoutingTableExists) then
    write(UNIT=LU_LOG,FMT=*)  "A downhill routing table exists from a previous run..."
  end if
  flush(UNIT=LU_LOG)

  CTL_READ: do

    read ( unit=LU_CONTROL, fmt="(a256)", iostat=iStat ) sRecord
    if ( iStat < 0 ) exit     ! EOF mark
    call Assert( iStat == 0, &
       "Terminating due to read error" )
    if ( sRecord(1:1) == "#" ) cycle      ! Ignore comment lines
    call Assert( iStat == 0, &
        "Terminating due to read error" )
    write (UNIT=LU_LOG,FMT=*) ">> " // trim(sRecord)
    flush(UNIT=LU_LOG)

    call Chomp( sRecord, sItem )
    call Uppercase( sItem )

    if ( sItem == "GRID" ) then
      write(UNIT=LU_LOG,FMT=*) "Reading in grid dimensions"
      ! Read NX
      call Chomp ( sRecord, sItem )
      read ( unit=sItem, fmt=*, iostat=iStat ) iNX
      call Assert( iStat == 0, "Failed to read NX" )
      write(UNIT=LU_LOG,FMT="('    NX: ',i6)") iNX
      ! Read NY
      call Chomp ( sRecord, sItem )
      read ( unit=sItem, fmt=*, iostat=iStat ) iNY
      call Assert ( iStat == 0, "Failed to read NY" )
      write(UNIT=LU_LOG,FMT="('    NY: ',i6)") iNY
      ! Read lower-left
      call Chomp ( sRecord, sItem )
      read ( unit=sItem, fmt=*, iostat=iStat ) rX0
      call Assert ( iStat == 0, "Failed to read X0" )
      write(UNIT=LU_LOG,FMT="('    X0: ',f14.4)") rX0
      call Chomp ( sRecord, sItem )
      read ( unit=sItem, fmt=*, iostat=iStat ) rY0
      call Assert ( iStat == 0, "Failed to read Y0" )
      write(UNIT=LU_LOG,FMT="('    Y0: ',f14.4)") rY0
      ! Read upper-right
      call Chomp ( sRecord, sItem )
      read ( unit=sItem, fmt=*, iostat=iStat ) rX1
      call Assert ( iStat == 0, "Failed to read X1" )
      write(UNIT=LU_LOG,FMT="('    X1: ',f14.4)") rX1
      call Chomp ( sRecord, sItem )
      read ( unit=sItem, fmt=*, iostat=iStat ) rY1
      call Assert ( iStat == 0, "Failed to read Y1" )
      write(UNIT=LU_LOG,FMT="('    Y1: ',f14.4)") rY1
      ! read grid cell size
      call Chomp ( sRecord, sItem )
      read ( unit=sItem, fmt=*, iostat=iStat ) rGridCellSize
      call Assert ( iStat == 0, "Failed to read grid cell size" )
      write(UNIT=LU_LOG,FMT="('    grid cell size: ',f8.1)") rGridCellSize
      ! Now, build the grid
      pGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, T_CELL_GRID)
      pGrd%rGridCellSize = rGridCellSize
      flush(UNIT=LU_LOG)

#ifdef DEBUG_PRINT
    else if ( sItem == "MEM_TEST" ) then

      rX0 = 10000.
      rY0 = 10000.
      rGridCellSize = 30.0

      do iNX = 10,10000,10
        iNY = iNX
        pGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, T_CELL_GRID)
        call grid_Destroy( pGrd )
      end do
#endif

    else if ( sItem == "GROWING_SEASON" ) then
      write(UNIT=LU_LOG,FMT=*) "Reading growing season"
      call Chomp ( sRecord, sArgument )
      read ( sArgument, fmt=*, iostat=iStat ) rValue
      pConfig%iDayOfFirstFrost = INT(rValue,kind=T_INT)
      call Assert ( iStat == 0, "Could not read start of growing season" )
      call Chomp ( sRecord, sArgument )
      read ( sArgument, fmt=*, iostat=iStat ) rValue
      pConfig%iDayOfLastFrost = INT(rValue,kind=T_INT)
      call Assert ( iStat == 0, "Could not read end of growing season" )
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      if (trim(sOption) == "TRUE" ) then
        pConfig%lNorthernHemisphere = lTRUE
      else
        pConfig%lNorthernHemisphere = lFALSE
      end if
      write(UNIT=LU_LOG,FMT=*) "Northern Hemisphere = ",pConfig%lNorthernHemisphere
      flush(UNIT=LU_LOG)

    else if ( sItem == "PRECIPITATION" ) then
      write(UNIT=LU_LOG,FMT=*) "Configuring precipitation data input"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      if ( trim(sOption) == "SINGLE_STATION" ) then
        pConfig%iConfigurePrecip = CONFIG_PRECIP_SINGLE_STATION
        write(UNIT=LU_LOG,FMT=*) "  Precip data will be read for a single station"
      else
        if ( trim(sOption) == "ARC_GRID" ) then
          pConfig%iConfigurePrecip = CONFIG_PRECIP_ARC_GRID
          write(UNIT=LU_LOG,FMT=*) "Precip data will be read as a series of ARC grids"
          pConfig%sPrecipFilePrefix = sArgument
          write(UNIT=LU_LOG,FMT=*)  "  grid file prefix for PRECIP data: ", &
          TRIM(pConfig%sPrecipFilePrefix)
        else if ( trim(sOption) == "SURFER" ) then
          pConfig%iConfigurePrecip = CONFIG_PRECIP_SURFER_GRID
          write(UNIT=LU_LOG,FMT=*) "Precip data will be read as a series of SURFER grids"
          write(UNIT=LU_LOG,FMT=*)  "  grid file prefix for PRECIP data: ", &
          TRIM(pConfig%sPrecipFilePrefix)
#ifdef NETCDF_SUPPORT
        else if( trim(sOption) == "NETCDF" ) then
          pConfig%iConfigurePrecip = CONFIG_PRECIP_NETCDF
          write(UNIT=LU_LOG,FMT=*) &
            "Precip data will be read from a NetCDF file"
          pConfig%NETCDF_FILE(iGROSS_PRECIP, iNC_INPUT)%iNCID = &
              netcdf_open(TRIM(sArgument))
          pConfig%NETCDF_FILE(iGROSS_PRECIP, iNC_INPUT)%sFilename = &
              TRIM(sArgument)
          call Chomp ( sRecord, sArgument )
          call netcdf_info(pConfig, iGROSS_PRECIP,iNC_INPUT, TRIM(sArgument))
          pConfig%NETCDF_FILE(iGROSS_PRECIP,iNC_INPUT)%sVarName = TRIM(sArgument)
          call netcdf_chk_extent(pConfig,iGROSS_PRECIP,iNC_INPUT,pGrd)
#endif
        else
          call Assert( .false._T_LOGICAL, "Illegal precipitation input format specified" )
        end if
        pConfig%lGriddedData = lTRUE
      end if
      flush(UNIT=LU_LOG)

    else if ( sItem == "TEMPERATURE" ) then
      write(UNIT=LU_LOG,FMT=*) "Configuring temperature data input"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      if ( trim(sOption) == "SINGLE_STATION" ) then
        pConfig%iConfigureTemperature = CONFIG_TEMPERATURE_SINGLE_STATION
        write(UNIT=LU_LOG,FMT=*) "  Temperature data will be read for a single station"
      else
        if ( trim(sOption) == "ARC_GRID" ) then
          pConfig%iConfigureTemperature = CONFIG_TEMPERATURE_ARC_GRID
          write(UNIT=LU_LOG,FMT=*) "Temperature data will be read as a series of ARC grids"
          pConfig%sTMAXFilePrefix = sArgument
          write(UNIT=LU_LOG,FMT=*)  "  grid file prefix for Max Temperature data: ", &
             TRIM(pConfig%sTMAXFilePrefix)
          call Chomp ( sRecord, sArgument )
          pConfig%sTMINFilePrefix = sArgument
          write(UNIT=LU_LOG,FMT=*)  "  grid file prefix for Min Temperature data: ", &
             TRIM(pConfig%sTMINFilePrefix)
          pConfig%lGriddedData = lTRUE
        else if ( trim(sOption) == "SURFER" ) then
          pConfig%iConfigureTemperature = CONFIG_TEMPERATURE_SURFER_GRID
          write(UNIT=LU_LOG,FMT=*) "Temperature data will be read as a series of SURFER grids"
          pConfig%sTMAXFilePrefix = sArgument
          write(UNIT=LU_LOG,FMT=*)  "  grid file prefix for Max Temperature data: ", &
             TRIM(pConfig%sTMAXFilePrefix)
          call Chomp ( sRecord, sArgument )
          pConfig%sTMINFilePrefix = sArgument
          write(UNIT=LU_LOG,FMT=*)  "  grid file prefix for Min Temperature data: ", &
             TRIM(pConfig%sTMINFilePrefix)
          pConfig%lGriddedData = lTRUE
#ifdef NETCDF_SUPPORT
        else if( trim(sOption) == "NETCDF" ) then
          pConfig%iConfigureTemperature = CONFIG_TEMPERATURE_NETCDF
          write(UNIT=LU_LOG,FMT=*) &
            "Temperature data will be read from a NetCDF file"

          ! first open and check extents for the TMAX file
          pConfig%NETCDF_FILE(iMAX_TEMP,iNC_INPUT)%iNCID = &
              netcdf_open(TRIM(sArgument))
          pConfig%NETCDF_FILE(iMAX_TEMP,iNC_INPUT)%sFilename = &
              TRIM(sArgument)
          ! read in the NetCDF variable that corresponds to TMAX
          call Chomp ( sRecord, sArgument )
          call netcdf_info(pConfig, iMAX_TEMP,iNC_INPUT, TRIM(sArgument))
          pConfig%NETCDF_FILE(iMAX_TEMP,iNC_INPUT)%sVarName = TRIM(sArgument)
          call netcdf_chk_extent(pConfig,iMAX_TEMP,iNC_INPUT,pGrd)

          ! now repeat for TMIN file
          call Chomp ( sRecord, sArgument )
          pConfig%NETCDF_FILE(iMIN_TEMP,iNC_INPUT)%iNCID = &
              netcdf_open(TRIM(sArgument))
          pConfig%NETCDF_FILE(iMIN_TEMP,iNC_INPUT)%sFilename = &
              TRIM(sArgument)
          ! read in the NetCDF variable that corresponds to TMIN
          call Chomp ( sRecord, sArgument )
          call netcdf_info(pConfig, iMIN_TEMP, iNC_INPUT, TRIM(sArgument))
          pConfig%NETCDF_FILE(iMIN_TEMP, iNC_INPUT)%sVarName = TRIM(sArgument)
          call netcdf_chk_extent(pConfig,iMIN_TEMP,iNC_INPUT,pGrd)
#endif
        else
          call Assert( lFALSE, "Illegal temperature input format specified", &
            TRIM(__FILE__),__LINE__)
        endif
      end if
      flush(UNIT=LU_LOG)

    else if ( sItem == "LAND_USE" ) then
      write(UNIT=LU_LOG,FMT=*) "Populating land use grid"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      if ( trim(sOption) == "CONSTANT" ) then
        pConfig%iConfigureLanduse = CONFIG_LANDUSE_CONSTANT
        read ( unit=sArgument, fmt=*, iostat=iStat ) iValue
        call Assert( iStat == 0, "Cannot read integer data value" )
        pGrd%Cells%iLandUse = iValue
      elseif(trim(sOption) == "DYNAMIC" ) then
        ! make room for another option and read in the proper value of sArgument
        sOption = sArgument
        call Uppercase( sOption )
        call Chomp ( sRecord, sArgument )

        if ( trim(sOption) == "ARC_GRID" ) then
          pConfig%iConfigureLanduse = CONFIG_LANDUSE_DYNAMIC_ARC_GRID
          write(UNIT=LU_LOG,FMT=*) "Landuse data will be read as a series of ARC grids"
          pConfig%sDynamicLanduseFilePrefix = sArgument
          write(UNIT=LU_LOG,FMT=*)  "  grid file prefix for dynamic landuse data: ", &
             TRIM(pConfig%sDynamicLanduseFilePrefix)
        else if ( trim(sOption) == "SURFER" ) then
          pConfig%iConfigureLanduse = CONFIG_LANDUSE_DYNAMIC_SURFER
          write(UNIT=LU_LOG,FMT=*) "Landuse data will be read as a series of SURFER grids"
          pConfig%sDynamicLanduseFilePrefix = sArgument
          write(UNIT=LU_LOG,FMT=*)  "  grid file prefix for dynamic landuse data: ", &
             TRIM(pConfig%sDynamicLanduseFilePrefix)
!#ifdef NETCDF_SUPPORT
!        else if( trim(sOption) == "NETCDF" ) then
!          pConfig%iConfigureLanduse = CONFIG_LANDUSE_DYNAMIC_NETCDF
!          pConfig%sDynamicLanduseFilePrefix = "none"
!          write(UNIT=LU_LOG,FMT=*) &
!            "Dynamic landuse data will be read from a NetCDF file"
!                 ! first open and check extents for the landuse file
!          pConfig%NETCDF_FILE(iMAX_TEMP,iNC_INPUT)%iNCID = &
!              netcdf_open(TRIM(sArgument))
!          pConfig%NETCDF_FILE(iMAX_TEMP,iNC_INPUT)%sFilename = &
!              TRIM(sArgument)
!#endif
        endif
      elseif( trim(sOption) == "ARC_GRID" &
         .or. trim(sOption) == "SURFER" ) then   ! read in a static gridded landuse file
        pConfig%iConfigureLanduse = CONFIG_LANDUSE_STATIC_GRID
        pConfig%sDynamicLanduseFilePrefix = "none"
        input_grd => grid_Read( sArgument, sOption, T_INT_GRID )
        call Assert( grid_Conform( pGrd, input_grd ),  &
                      "Non-conforming grid" )
        pGrd%Cells%iLandUse = input_grd%iData
        call grid_Destroy( input_grd )
      else
        call Assert( lFALSE, "Illegal landuse input option or format specified", &
          TRIM(__FILE__),__LINE__)
      endif
      flush(UNIT=LU_LOG)

    else if ( sItem == "FLOW_DIRECTION" ) then
      write(UNIT=LU_LOG,FMT=*) "Populating flow direction grid"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      if ( trim(sOption) == "CONSTANT" ) then
        read ( unit=sArgument, fmt=*, iostat=iStat ) iValue
        call Assert( iStat == 0, "Cannot read integer data value" )
        pGrd%Cells%iFlowDir = iValue
      else
        input_grd => grid_Read( sArgument, sOption, T_INT_GRID )
        call Assert( grid_Conform( pGrd, input_grd ), &
                      "Non-conforming grid" )
        pGrd%Cells%iFlowDir = input_grd%iData
        call grid_Destroy( input_grd )
      end if
      flush(UNIT=LU_LOG)

    else if ( sItem == "ROUTING_FRACTION" ) then
      write(UNIT=LU_LOG,FMT=*) "Populating routing fraction grid"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      if ( trim(sOption) == "CONSTANT" ) then
        read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
        call Assert( iStat == 0, &
          "Cannot read real data value" )
        pGrd%Cells%rRouteFraction = rValue
      else
        input_grd => grid_Read( sArgument, sOption, T_SGL_GRID )
        call Assert( grid_Conform( pGrd, input_grd ), &
                      "Non-conforming grid" )
        pGrd%Cells%rRouteFraction = input_grd%rData
        call grid_Destroy( input_grd )
        call Assert(LOGICAL(MAXVAL(pGrd%Cells%rRouteFraction)<=rONE,&
            kind=T_LOGICAL), &
            "One or mode values in routing fraction grid exceed 1.0")
        call Assert(LOGICAL(MINVAL(pGrd%Cells%rRouteFraction)>=rZERO,&
            kind=T_LOGICAL), &
            "One or mode values in routing fraction grid are less than 0.0")
      end if
      write(UNIT=LU_LOG, &
        FMT="('  Number of cells with normal routing: ',i8)") &
        COUNT(pGrd%Cells%rRouteFraction > &
        (rONE - rNEAR_ZERO))
      write(UNIT=LU_LOG, &
        FMT="('  Number of cells with fractional routing: ',i8)") &
        COUNT(pGrd%Cells%rRouteFraction <= &
        (rONE - rNEAR_ZERO))
      flush(UNIT=LU_LOG)

    else if ( sItem == "INITIAL_FROZEN_GROUND_INDEX" ) then
      write(UNIT=LU_LOG,FMT=*) "Initializing continuous frozen ground index"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      if ( trim(sOption) == "CONSTANT" ) then
        read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
        call Assert( iStat == 0, "Cannot read real data value" )
        pGrd%Cells%rCFGI = rValue
      else
        input_grd => grid_Read( sArgument, sOption, T_SGL_GRID )
        call Assert( grid_Conform( pGrd, input_grd ), &
                      "Non-conforming grid" )
        pGrd%Cells%rCFGI = input_grd%rData
        call grid_Destroy( input_grd )
      end if
      flush(UNIT=LU_LOG)

    else if ( sItem == "PATH_DELIMITER" ) then
      write(UNIT=LU_LOG,FMT=*) &
        "Setting delimiter character for referencing pathnames"
      call Chomp ( sRecord, sArgument )
      pConfig%sSlash = TRIM(ADJUSTL(sArgument))
      write(UNIT=LU_LOG,FMT=*)  &
        "Delimiter character for referencing pathnames set to: '" &
        //TRIM(pConfig%sSlash)//"'"
      flush(UNIT=LU_LOG)

    else if ( sItem == "WRITE_EXTRA_PEST_FILES" ) then
      write(UNIT=LU_LOG,FMT=*) &
        "SWB will write an observations snippet and a *.ins file"
      pConfig%lWriteExtraPestFiles = lTRUE
      flush(UNIT=LU_LOG)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    else if ( sItem == "WRITE_SSF" ) then
      write(UNIT=LU_LOG,FMT=*) &
        "Adding to the list of SSF files to be written out"
      if(lNO_SSF_FILES) then   ! allocate SSF_FILES for first time
        ALLOCATE (pConfig%SSF_FILES(1), STAT=iStat)
        call Assert( iStat == 0, &
           "Could not allocate memory for SSF file information structure")
        lNO_SSF_FILES = lFALSE
        iNewSize = size(pConfig%SSF_FILES)
      else                     ! create more room for SSF_FILES
        iOldSize = size(pConfig%SSF_FILES)
        iNewSize = iOldSize + 1

        ! allocate space for temporary data structure
        ALLOCATE (pSSF(1:iNewSize), STAT=iStat)
        call Assert( iStat == 0, &
           "Could not allocate memory for new temporary SSF file information structure")
        ! copy contents of old data struct to temporary struct
        pSSF(1:iOldSize) = pConfig%SSF_FILES(1:iOldSize)

        ! eliminate old data struct
        DEALLOCATE (pConfig%SSF_FILES, STAT=iStat)
        call Assert( iStat == 0, &
           "Could not deallocate memory for SSF file information structure")

        ! allocate space for new, larger data struct
        ALLOCATE (pConfig%SSF_FILES(1:iNewSize), STAT=iStat)
        call Assert( iStat == 0, &
           "Could not allocate memory for RESIZED SSF file information structure")

        ! now copy data back to larger data structure
        pConfig%SSF_FILES(1:iOldSize) = pSSF(1:iOldSize)

        ! finally, deallocate temporary data struct
        DEALLOCATE (pSSF, STAT=iStat)
        call Assert( iStat == 0, &
           "Could not deallocate memory for temporary SSF file information structure")
      end if

      ! determine which swb variable is to be written to an SSF file
      call Chomp ( sRecord, sArgument )
      lMatch = lFALSE
      do i=1,iNUM_VARIABLES
        if(TRIM(sArgument) == TRIM(STAT_INFO(i)%sVARIABLE_NAME)) then
          pConfig%SSF_FILES(iNewSize)%iVarNum = i
          lMatch = lTRUE
          exit
        end if
      end do

      call Assert(lMatch,"Could not find matching variable name in " &
        //"WRITE_SSF block of control file: "//TRIM(sArgument), &
        TRIM(__FILE__),__LINE__)

      call Chomp ( sRecord, sArgument )

      if(TRIM(sArgument)=="COORDS") then
        call Chomp ( sRecord, sArgument )
        read ( unit=sArgument, fmt=*, iostat=iStat ) rXp
        iColNum = grid_GetGridColNum(pGrd,rXp)
        write(UNIT=LU_LOG,FMT="('   X-coord: ',F14.3,' <===> Column #: ',i6)") rXp,iColNum
        call Chomp ( sRecord, sArgument )
        read ( unit=sArgument, fmt=*, iostat=iStat ) rYp
        iRowNum = grid_GetGridRowNum(pGrd,rYp)
        write(UNIT=LU_LOG,FMT="('   Y-coord: ',F14.3,' <===> Row #: ',i6)") rYp,iRowNum

      else

        read ( unit=sArgument, fmt=*, iostat=iStat ) iColNum
        call Assert( iStat == 0, "Cannot read integer data value" )

        call Chomp ( sRecord, sArgument )
        read ( unit=sArgument, fmt=*, iostat=iStat ) iRowNum
        call Assert( iStat == 0, "Cannot read integer data value" )

      end if

      call Assert(iColNum <= pGrd%iNX .and. iColNum > 0, &
         "Column number is outside of grid extent:" &
         //TRIM(int2char(iColNum)), &
         TRIM(__FILE__),__LINE__)
      call Assert(iRowNum <= pGrd%iNY .and. iRowNum > 0, &
         "Row number is outside of grid extent" &
         //TRIM(int2char(iRowNum)), &
         TRIM(__FILE__),__LINE__)

      pConfig%SSF_FILES(iNewSize)%iColNum = iColNum
      pConfig%SSF_FILES(iNewSize)%iRowNum = iRowNum

      ! increment SSF file counter for this grid cell
      pGrd%Cells(iRowNum,iColNum)%iNumFilesSSF = &
        pGrd%Cells(iRowNum,iColNum)%iNumFilesSSF + 1

      ! check to ensure the file unit is not already open
      do
        iLU_SSF = iLU_SSF + 1
        inquire(iLU_SSF, OPENED=lOpened, NAME=sBuf)
        if(.not. lOpened) exit
        write(unit=LU_LOG,fmt=*) ' *** FOUND AN EXISTING FILE: ' &
          //TRIM(sBuf)
      end do

     ! reuse sBuf to hold the name of the new output file
      sBuf = TRIM(STAT_INFO(pConfig%SSF_FILES(iNewSize)%iVarNum)%sVARIABLE_NAME) &
        //"_"//TRIM(int2char(pConfig%SSF_FILES(iNewSize)%iColNum)) &
        //"_"//TRIM(int2char(pConfig%SSF_FILES(iNewSize)%iRowNum)) &
        //".ssf"

      ! opening and closing file to erase contents left over from previous runs
      open(unit=iLU_SSF, file=TRIM(sBuf),status='REPLACE', iostat=iStat)
      call Assert(iStat==0,"Error opening file on unit " &
        //TRIM(int2char(iLU_SSF))//"; filename = "//TRIM(sBuf), &
        TRIM(__FILE__),__LINE__)
      pConfig%SSF_FILES(iNewSize)%iLU = iLU_SSF
      pConfig%SSF_FILES(iNewSize)%sFileName = TRIM(sBuf)
      close(unit=iLU_SSF, iostat=iStat)
      call Assert(iStat==0,"Error closing file on unit " &
        //TRIM(int2char(iLU_SSF))//"; filename = "//TRIM(sBuf), &
        TRIM(__FILE__),__LINE__)

      write(UNIT=LU_LOG,FMT=*)  &
        "SSF file has been opened for: " &
        //TRIM(STAT_INFO(pConfig%SSF_FILES(iNewSize)%iVarNum)%sVARIABLE_NAME) &
        //"  col: "//TRIM(int2char(pConfig%SSF_FILES(iNewSize)%iColNum)) &
        //"  row: "//TRIM(int2char(pConfig%SSF_FILES(iNewSize)%iRowNum))
      flush(UNIT=LU_LOG)

    else if ( sItem == "STATS_START_YEAR" ) then
      write(UNIT=LU_LOG,FMT=*) &
        "Setting starting year for calculation of recharge statistics"
      call Chomp ( sRecord, sArgument )
      read ( unit=sArgument, fmt=*, iostat=iStat ) iValue
      call Assert( iStat == 0, "Cannot read integer data value" )
      pConfig%iStartYearforCalculation = iValue
      write(UNIT=LU_LOG,FMT=*)  &
        "First year for recharge stats calculations:", iValue
      flush(UNIT=LU_LOG)

    else if ( sItem == "STATS_END_YEAR" ) then
      write(UNIT=LU_LOG,FMT=*) &
        "Setting ending year for calculation of recharge statistics"
      call Chomp ( sRecord, sArgument )
      read ( unit=sArgument, fmt=*, iostat=iStat ) iValue
      call Assert( iStat == 0, "Cannot read integer data value" )
      pConfig%iEndYearforCalculation = iValue
      write(UNIT=LU_LOG,FMT=*)  &
        "Last year for recharge stats calculations:", iValue
      flush(UNIT=LU_LOG)

    else if ( sItem == "MINIMUM_VALID_PRECIP_VALUE" ) then
      write(UNIT=LU_LOG,FMT=*) &
        "Setting value for minimum valid precip amount." &
        //" Values less than this will be forced to zero."
      call Chomp ( sRecord, sArgument )
      read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
      call Assert( iStat == 0, "Cannot read real data value" )
      pConfig%rMinValidPrecip = rValue
      write(UNIT=LU_LOG,FMT=*)  &
        "Minimum valid precipitation value has been reset to:", rValue
      flush(UNIT=LU_LOG)

    else if ( sItem == "LOWER_LIMIT_CFGI" ) then
      write(UNIT=LU_LOG,FMT=*) &
        "Initializing lower boundary for continuous frozen ground index threshold"
      call Chomp ( sRecord, sArgument )
      read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
      call Assert( iStat == 0, "Cannot read real data value" )
      pConfig%rLL_CFGI = rValue
      write(UNIT=LU_LOG,FMT=*)  &
        "Ground will be considered unfrozen at a CFGI value of: ", rValue
      flush(UNIT=LU_LOG)

    else if ( sItem == "UPPER_LIMIT_CFGI" ) then
      write(UNIT=LU_LOG,FMT=*) &
        "Initializing upper boundary for continuous frozen ground index threshold"
      call Chomp ( sRecord, sArgument )
      read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
      call Assert( iStat == 0, "Cannot read real data value" )
      pConfig%rUL_CFGI = rValue
      write(UNIT=LU_LOG,FMT=*)  "Ground will be considered frozen at a CFGI value of: ", rValue
      flush(UNIT=LU_LOG)

    else if ( sItem == "GRID_LENGTH_UNITS" ) then
      write(UNIT=LU_LOG,FMT=*) "Setting grid length units"
      call Chomp ( sRecord, sArgument )
      if(TRIM(sArgument)=="FEET") then
        pGrd%iLengthUnits = iGRID_LENGTH_UNITS_FEET
      elseif(TRIM(sArgument)=="METERS") then
        pGrd%iLengthUnits = iGRID_LENGTH_UNITS_METERS
      else
        call Assert(lFALSE , "Grid units may only be set to METERS or FEET" )
      endif
      flush(UNIT=LU_LOG)

    else if ( sItem == "REFERENCE_ET_SLOPE" ) then
      write(UNIT=LU_LOG,FMT=*) "Initializing slope factor for reference ET method"
      call Chomp ( sRecord, sArgument )
      read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
      call Assert( iStat == 0, "Cannot read real data value" )
      pConfig%rET_Slope = rValue
      write(UNIT=LU_LOG,FMT=*)  "Reference ET slope set to: ", rValue
      flush(UNIT=LU_LOG)

    else if ( sItem == "REFERENCE_ET_EXPONENT" ) then
      write(UNIT=LU_LOG,FMT=*) "Initializing exponent for reference ET method"
      call Chomp ( sRecord, sArgument )
      read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
      call Assert( iStat == 0, "Cannot read real data value" )
      pConfig%rET_Exponent = rValue
      write(UNIT=LU_LOG,FMT=*)  "Reference ET exponent set to: ", rValue
      flush(UNIT=LU_LOG)

    else if ( sItem == "REFERENCE_ET_CONSTANT" ) then
      write(UNIT=LU_LOG,FMT=*) "Initializing constant for reference ET method"
      call Chomp ( sRecord, sArgument )
      read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
      call Assert( iStat == 0, "Cannot read real data value" )
      pConfig%rET_Constant = rValue
      write(UNIT=LU_LOG,FMT=*)  "Reference ET constant set to: ", rValue
      flush(UNIT=LU_LOG)

    else if ( sItem == "SNWD_SLOPE" ) then
      write(UNIT=LU_LOG,FMT=*) "Initializing parameter SNWD_SLOPE"
      call Chomp ( sRecord, sArgument )
      read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
      call Assert( iStat == 0, "Cannot read real data value" )
      pConfig%rSNWD_slp1 = rValue
      write(UNIT=LU_LOG,FMT=*)  "SNWD_SLOPE set to: ", rValue
      flush(UNIT=LU_LOG)

    else if ( sItem == "SNWD_DENOMINATOR" ) then
      write(UNIT=LU_LOG,FMT=*) "Initializing parameter SNWD_DENOM"
      call Chomp ( sRecord, sArgument )
      read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
      call Assert( iStat == 0, "Cannot read real data value" )
      pConfig%rSNWD_denom = rValue
      write(UNIT=LU_LOG,FMT=*)  "SNWD_DENOM set to: ", rValue
      flush(UNIT=LU_LOG)

    else if ( sItem == "SNWD_INTERCEPT" ) then
      write(UNIT=LU_LOG,FMT=*) "Initializing parameter SNWD_INTERCEPT"
      call Chomp ( sRecord, sArgument )
      read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
      call Assert( iStat == 0, "Cannot read real data value" )
      pConfig%rSNWD_intcp1 = rValue
      write(UNIT=LU_LOG,FMT=*)  "SNWD_INTERCEPT set to: ", rValue
      flush(UNIT=LU_LOG)

    else if ( sItem == "TMAX_ALLSNOW" ) then
      write(UNIT=LU_LOG,FMT=*) "Initializing parameter TMAX_ALLSNOW"
      call Chomp ( sRecord, sArgument )
      read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
      call Assert( iStat == 0, "Cannot read real data value" )
      pConfig%rTMaxAllSnow = rValue
      write(UNIT=LU_LOG,FMT=*)  "TMAX_ALLSNOW set to: ", rValue
      flush(UNIT=LU_LOG)

    else if ( sItem == "TMAX_ALLRAIN" ) then
      write(UNIT=LU_LOG,FMT=*) "Initializing parameter TMAX_ALLRAIN"
      call Chomp ( sRecord, sArgument )
      read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
      call Assert( iStat == 0, "Cannot read real data value" )
      pConfig%rTMaxAllRain = rValue
      write(UNIT=LU_LOG,FMT=*)  "TMAX_ALLRAIN set to: ", rValue
      flush(UNIT=LU_LOG)

    else if ( sItem == "RAINFALL_CORRECTION_FACTOR" ) then
      write(UNIT=LU_LOG,FMT=*) "Initializing rainfall correction factor"
      call Chomp ( sRecord, sArgument )
      read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
      call Assert( iStat == 0, "Cannot read real data value" )
      pConfig%rRainfall_Corr_Factor = rValue
      write(UNIT=LU_LOG,FMT=*)  "Rainfall correction factor set to: ", rValue
      flush(UNIT=LU_LOG)

    else if ( sItem == "SNOWFALL_CORRECTION_FACTOR" ) then
      write(UNIT=LU_LOG,FMT=*) "Initializing snowfall correction factor"
      call Chomp ( sRecord, sArgument )
      read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
      call Assert( iStat == 0, "Cannot read real data value" )
      pConfig%rSnowWaterEquiv_Corr_Factor = rValue
      write(UNIT=LU_LOG,FMT=*)  "Snowfall correction factor set to: ", rValue
      flush(UNIT=LU_LOG)

    else if ( sItem == "OPEN_WATER_LAND_USE" ) then
      write(UNIT=LU_LOG,FMT=*) "Setting open water land use type"
      call Chomp ( sRecord, sArgument )
      read ( unit=sArgument, fmt=*, iostat=iStat ) iValue
      call Assert( iStat == 0, "Cannot read integer data value" )
      pConfig%iOPEN_WATER_LU = iValue
      write(UNIT=LU_LOG,FMT=*)  "Cell will be considered to be open water given" &
        // " a land use code of ", iValue
      flush(UNIT=LU_LOG)

    else if ( sItem == "ELEVATION" ) then
      write(unit=LU_LOG, fmt=*) "Reading elevation grid"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      if ( trim(sOption) == "CONSTANT" ) then
        read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
        call Assert( iStat == 0, &
          "Cannot read floating point data value" )
        pGrd%Cells%rElevation = rValue
      else
        input_grd => grid_Read( sArgument, sOption, T_SGL_GRID )
        call Assert( grid_Conform( pGrd, input_grd ), &
                      "Non-conforming grid" )
        pGrd%Cells%rElevation = input_grd%rData
        call grid_Destroy( input_grd )
      end if

    else if ( sItem == "SOIL_GROUP" ) then
      write(UNIT=LU_LOG,FMT=*) "Populating soil group grid"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      if ( trim(sOption) == "CONSTANT" ) then
        read ( unit=sArgument, fmt=*, iostat=iStat ) iValue
        call Assert( iStat == 0, &
          "Cannot read integer data value" )
        pGrd%Cells%iSoilGroup = iValue
      else
        input_grd => grid_Read( sArgument, sOption, T_INT_GRID )
        call Assert( grid_Conform( pGrd, input_grd ), &
                      "Non-conforming grid" )
        pGrd%Cells%iSoilGroup = input_grd%iData
        call grid_Destroy( input_grd )
      end if
      flush(UNIT=LU_LOG)

    else if ( sItem == "LAND_USE_LOOKUP_TABLE" ) then
      write(UNIT=LU_LOG,FMT=*) "Reading land-use lookup table"
      call Chomp ( sRecord, pConfig%sLandUseLookupFilename )
      if ( len_trim(pConfig%sLandUseLookupFilename) == 0 ) then
        call Assert( .false._T_LOGICAL, "No land use lookup table specified" )
      end if
      call model_ReadLanduseLookupTable( pConfig )
      flush(UNIT=LU_LOG)

#ifdef IRRIGATION_MODULE

    else if ( sItem == "IRRIGATION_LOOKUP_TABLE" ) then
      write(UNIT=LU_LOG,FMT=*) "Reading irrigation lookup table"
      call Chomp ( sRecord, pConfig%sIrrigationLookupFilename )
      if ( len_trim(pConfig%sIrrigationLookupFilename) == 0 ) then
        call Assert( .false._T_LOGICAL, "No irrigation lookup table specified" )
      end if
      call model_ReadIrrigationLookupTable( pConfig )
      flush(UNIT=LU_LOG)

#endif

    else if ( sItem == "BASIN_MASK_LOOKUP_TABLE" ) then
      write(UNIT=LU_LOG,FMT=*) "Reading basin mask lookup table"
      call Chomp ( sRecord, pConfig%sBasinMaskFilename )
      if ( len_trim(pConfig%sBasinMaskFilename) == 0 ) then
        call Assert( .false._T_LOGICAL, "No basin mask table specified" )
      end if
      call model_ReadBasinMaskTable( pConfig )
      flush(UNIT=LU_LOG)

    else if ( sItem == "ADJUSTED_WATER_CAPACITY" ) then
      write(UNIT=LU_LOG,FMT=*) "Populating adjusted water capacity grid"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      if ( trim(sOption) == "CONSTANT" ) then
        pConfig%iConfigureSMCapacity = CONFIG_SM_CAPACITY_FM_TABLE
        read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
        call Assert( iStat == 0, "Cannot read real data value" )
        pGrd%Cells%rSoilWaterCap = rValue
        write(UNIT=LU_LOG,FMT=*)  "using a constant value for the maximum soil water capacity"
        write(UNIT=LU_LOG,FMT=*)  "NOTE: any option specified with the 'WATER_CAPACITY' option will be ignored"
      else
        pConfig%iConfigureSMCapacity = CONFIG_SM_CAPACITY_FM_TABLE
        input_grd => grid_Read( sArgument, sOption, T_SGL_GRID )
        call Assert( grid_Conform( pGrd, input_grd ), &
                      "Non-conforming grid" )
        pGrd%Cells%rSoilWaterCap = input_grd%rData
        call grid_Destroy( input_grd )
        write(UNIT=LU_LOG,FMT=*)  "reading an ASCII grid to obtain the maximum soil water capacity"
        write(UNIT=LU_LOG,FMT=*)  "NOTE: any option specified with the 'WATER_CAPACITY' option will be ignored"
      end if

      write(UNIT=LU_LOG,FMT=*)  "Total water capacity grid MINIMUM VALUE:", &
         minval(pGrd%Cells%rSoilWaterCap)
      write(UNIT=LU_LOG,FMT=*)  "Total water capacity grid MAXIMUM VALUE:", &
         maxval(pGrd%Cells%rSoilWaterCap)

      call Assert(maxval(pGrd%Cells%rSoilWaterCap) < 25.0 &
         .and. minval(pGrd%Cells%rSoilWaterCap) >= 0., &
         "Total water capacity must be in the range" &
         //" from 0 to 17.5 inches [of water].",TRIM(__FILE__),__LINE__)
      flush(UNIT=LU_LOG)

    else if ( sItem == "WATER_CAPACITY" ) then
      write(UNIT=LU_LOG,FMT=*) "Populating water capacity grid"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      if ( trim(sOption) == "CONSTANT" ) then
        read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
        call Assert( iStat == 0, "Cannot read real data value" )
        pGrd%Cells%rSoilWaterCapInput = rValue
      else
        input_grd => grid_Read( sArgument, sOption, T_SGL_GRID )
        call Assert( grid_Conform( pGrd, input_grd ), &
                      "Non-conforming grid" )
        pGrd%Cells%rSoilWaterCapInput = input_grd%rData
        call grid_Destroy( input_grd )
      end if
      write(UNIT=LU_LOG,FMT=*)  "Water capacity grid MINIMUM VALUE:", &
         minval(pGrd%Cells%rSoilWaterCapInput)
      write(UNIT=LU_LOG,FMT=*)  "Water capacity grid MAXIMUM VALUE:", &
         maxval(pGrd%Cells%rSoilWaterCapInput)

      call Assert(maxval(pGrd%Cells%rSoilWaterCapInput) <= 12.0 &
         .and. maxval(pGrd%Cells%rSoilWaterCapInput) >= 0., &
         "Available water capacity must be in the range" &
         //" from 0 to 12 inches [of water] per foot [of soil].",TRIM(__FILE__),__LINE__)
      flush(UNIT=LU_LOG)

    else if ( sItem == "INITIAL_SOIL_MOISTURE" ) then
      write(UNIT=LU_LOG,FMT=*) "Populating initial moisture grid"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      if ( trim(sOption) == "CONSTANT" ) then
        read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
        call Assert( iStat == 0, "Cannot read real data value" )
        pGrd%Cells%rSoilMoisturePct = rValue
      else
        input_grd => grid_Read( sArgument, sOption, T_SGL_GRID )
        call Assert( grid_Conform( pGrd, input_grd ), &
                      "Non-conforming grid" )
        pGrd%Cells%rSoilMoisturePct = input_grd%rData
        call grid_Destroy( input_grd )
      end if
      write(UNIT=LU_LOG,FMT=*)  "Initial soil moisture grid MINUMUM VALUE:", &
         minval(pGrd%Cells%rSoilMoisturePct)
      write(UNIT=LU_LOG,FMT=*)  "Initial soil moisture grid MAXIMUM VALUE:", &
         maxval(pGrd%Cells%rSoilMoisturePct)
      call Assert( LOGICAL( minval(pGrd%Cells%rSoilMoisturePct) >= 0, &
         kind=T_LOGICAL), "Negative values are not allowed in the" &
         //" initial soil moisture grid.")

      flush(UNIT=LU_LOG)

    else if ( sItem == "ELEVATION_ADJUSTMENT" ) then
      !! This expects to read four real values:
      !!
      !! rStationElevation -- The elevation of the weather station
      !! rDryFactor -- The factor (per foot of difference between the station and ground
      !!               surface for temperature adjustment when conditions are "dry"
      !! rHumidFactor -- The factor (per foot of difference between the station and ground
      !!               surface for temperature adjustment when conditions are "humid"
      !! rHumidityThreshold -- the minimum relative humidity for "humid" conditions.
      !!                If this value is 9999, the "dry" factor is always used (this is
      !!                useful for simulations where humidity data are not available)
      call Chomp( sRecord, sArgument )
      read(unit=sArgument, fmt=*, iostat=iStat) pconfig%rElevStationElevation
      call Assert( logical(iStat==0, kind=T_LOGICAL), &
        "Failed to read station elevation " // sArgument )
      call Chomp( sRecord, sArgument )
      read(unit=sArgument, fmt=*, iostat=iStat) pconfig%rElevDryFactor
      call Assert( logical(iStat==0, kind=T_LOGICAL), &
        "Failed to read dry elevation factor " // sArgument )
      call Chomp( sRecord, sArgument )
      read(unit=sArgument, fmt=*, iostat=iStat) pconfig%rElevHumidFactor
      call Assert( logical(iStat==0, kind=T_LOGICAL), &
        "Failed to read humid elevation factor " // sArgument )
      call Chomp( sRecord, sArgument )
      read(unit=sArgument, fmt=*, iostat=iStat) pconfig%rElevHumidityThreshold
      call Assert( logical(iStat==0, kind=T_LOGICAL), &
        "Failed to read humidity threshold " // sArgument )
      pconfig%lElevAdjustment = .true.

    else if ( sItem == "RLE_MULTIPLIER" ) then
      call Chomp ( sRecord, sArgument )
      write(UNIT=LU_LOG,FMT=*) "Setting the value for the RLE multiplier"
      read ( unit=sArgument, fmt=*, iostat=iStat ) iValue
      call Assert( iStat == 0, "Cannot read integer data value" )
      pConfig%iRLE_MULT = iValue
      flush(UNIT=LU_LOG)

    else if ( sItem == "OUTPUT_OPTIONS" ) then
      lMatch = lFALSE
      call Chomp ( sRecord, sArgument )
      write(UNIT=LU_LOG,FMT=*) "Setting the output options"
      do i=1,iNUM_VARIABLES
        if(TRIM(sArgument) == TRIM(STAT_INFO(i)%sVARIABLE_NAME)) then
          lMatch = lTRUE
          iVarNum = i
          exit
        end if
      end do

      call Assert(lMatch,"Output requested for unknown variable: "//TRIM(sArgument), &
        TRIM(__FILE__),__LINE__)

      ! set daily output options
      call Chomp ( sRecord, sArgument )
      if (TRIM(sArgument)== "GRID") then
        STAT_INFO(iVarNum)%iDailyOutput = iGRID
#ifdef GRAPHICS_SUPPORT
      else if (TRIM(sArgument)== "GRAPH" .or. TRIM(sArgument)== "PLOT") then
        STAT_INFO(iVarNum)%iDailyOutput = iGRAPH
      else if (TRIM(sArgument)== "BOTH") then
        STAT_INFO(iVarNum)%iDailyOutput = iBOTH
#endif
      else
        STAT_INFO(iVarNum)%iDailyOutput = iNONE
      end if

      ! now repeat for monthly output options
      call Chomp ( sRecord, sArgument )
      if (TRIM(sArgument)== "GRID") then
        STAT_INFO(iVarNum)%iMonthlyOutput = iGRID
#ifdef GRAPHICS_SUPPORT
      else if (TRIM(sArgument)== "GRAPH" .or. TRIM(sArgument)== "PLOT") then
        STAT_INFO(iVarNum)%iMonthlyOutput = iGRAPH
      else if (TRIM(sArgument)== "BOTH") then
        STAT_INFO(iVarNum)%iMonthlyOutput = iBOTH
#endif
      else
        STAT_INFO(iVarNum)%iMonthlyOutput = iNONE
      end if

      ! repeat for annual output options
      call Chomp ( sRecord, sArgument )
      if (TRIM(sArgument)== "GRID") then
        STAT_INFO(iVarNum)%iAnnualOutput = iGRID
#ifdef GRAPHICS_SUPPORT
      else if (TRIM(sArgument)== "GRAPH" .or. TRIM(sArgument)== "PLOT") then
        STAT_INFO(iVarNum)%iAnnualOutput = iGRAPH
      else if (TRIM(sArgument)== "BOTH") then
        STAT_INFO(iVarNum)%iAnnualOutput = iBOTH
#endif
      else
        STAT_INFO(iVarNum)%iAnnualOutput = iNONE
      end if

#ifdef NETCDF_SUPPORT
      ! finally repeat for NetCDF output options
      call Chomp ( sRecord, sArgument )
      if (TRIM(sArgument)== "NETCDF") then
        STAT_INFO(iVarNum)%iNetCDFOutput = iGRID
      else
        STAT_INFO(iVarNum)%iNetCDFOutput = iNONE
      end if
#endif

      write(UNIT=LU_LOG,FMT=*)  "Options have been set for: ", &
         TRIM(STAT_INFO(iVarNum)%sVARIABLE_NAME)
      write(UNIT=LU_LOG,FMT=*)  "STAT_INFO(iVarNum)%iAnnualOutput: ",&
         STAT_INFO(iVarNum)%iAnnualOutput
      write(UNIT=LU_LOG,FMT=*)  "STAT_INFO(iVarNum)%iMonthlyOutput: ",&
         STAT_INFO(iVarNum)%iMonthlyOutput
      write(UNIT=LU_LOG,FMT=*)  "STAT_INFO(iVarNum)%iDailyOutput: ",&
         STAT_INFO(iVarNum)%iDailyOutput

#ifdef NETCDF_SUPPORT
      write(UNIT=LU_LOG,FMT=*)  "STAT_INFO(iVarNum)%iNetCDFOutput: ",&
         STAT_INFO(iVarNum)%iNetCDFOutput
#endif

#ifdef GRAPHICS_SUPPORT
    else if ( sItem == "DISLIN_PARAMETERS" ) then
      call Chomp ( sRecord, sArgument )
      write(UNIT=LU_LOG,FMT=*) "Setting the values for DISLIN parameters"
      do i=1,iNUM_VARIABLES
        if(TRIM(sArgument) == TRIM(STAT_INFO(i)%sVARIABLE_NAME)) then
          iVarNum = i
          exit
        end if
      end do

    else if ( TRIM(sItem) == "SET_PAGE" ) then
      if(iVarNum>0 .and. iVarNum <= iNUM_VARIABLES) then
         call Chomp ( sRecord, sArgument )
         pGraph(iVarNum)%cPAPER_SIZE = TRIM(sArgument)
      else
         write(UNIT=LU_LOG,FMT=*)  "FAILED to set DISLIN page size; make sure that the "
         write(UNIT=LU_LOG,FMT=*)   "  DISLIN variable name has been set and is valid"
      end if

    else if ( TRIM(sItem) == "SET_DEVICE" ) then
      if(iVarNum>0 .and. iVarNum <= iNUM_VARIABLES) then
         call Chomp ( sRecord, sArgument )
         pGraph(iVarNum)%cCDEV = TRIM(sArgument)
      else
         write(UNIT=LU_LOG,FMT=*)  "FAILED to set DISLIN output device; make sure that the "
         write(UNIT=LU_LOG,FMT=*)   "  DISLIN variable name has been set and is valid"
      end if

    else if ( TRIM(sItem) == "SET_COLOR_TABLE" ) then
      if(iVarNum>0 .and. iVarNum <= iNUM_VARIABLES) then
         call Chomp ( sRecord, sArgument )
         pGraph(iVarNum)%cCOLOR_TABLE = TRIM(sArgument)
      else
         write(UNIT=LU_LOG,FMT=*)  "FAILED to set DISLIN color table; make sure that the "
         write(UNIT=LU_LOG,FMT=*)   "  DISLIN variable name has been set and is valid"
      end if

    else if ( TRIM(sItem) == "SET_FONT" ) then
      if(iVarNum>0 .and. iVarNum <= iNUM_VARIABLES) then
         call Chomp ( sRecord, sArgument )
         pGraph(iVarNum)%cFONT_NAME = TRIM(sArgument)
      else
         write(UNIT=LU_LOG,FMT=*)  "FAILED to set DISLIN font name; make sure that the "
         write(UNIT=LU_LOG,FMT=*)   "  DISLIN variable name has been set and is valid"
      end if

    else if ( TRIM(sItem) == "Z_AXIS_TITLE" ) then
      if(iVarNum>0 .and. iVarNum <= iNUM_VARIABLES) then
!         call Chomp ( sRecord, sArgument )
         pGraph(iVarNum)%cZ_AXIS_TITLE = TRIM(sRecord)
      else
         write(UNIT=LU_LOG,FMT=*)  "FAILED to set DISLIN parameter; make sure that the "
         write(UNIT=LU_LOG,FMT=*)   "  DISLIN variable name has been set and is valid"
      end if

    else if ( TRIM(sItem) == "SET_Z_AXIS_RANGE" ) then
      if(iVarNum>0 .and. iVarNum <= iNUM_VARIABLES) then
         call Chomp ( sRecord, sArgument )
         if(TRIM(sArgument)=="DAILY") then
           iTimeFrame = iDAILY
         else if(TRIM(sArgument)=="MONTHLY") then
           iTimeFrame = iMONTHLY
         else
           iTimeFrame = iANNUAL
         end if

         call Chomp ( sRecord, sArgument )
         read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
         call Assert( iStat == 0, &
            "Cannot read real data value" )
         pGraph(iVarNum)%rZA(iTimeFrame) = rValue

         call Chomp ( sRecord, sArgument )
         read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
         call Assert( iStat == 0, &
            "Cannot read real data value" )
         pGraph(iVarNum)%rZE(iTimeFrame) = rValue

         call Chomp ( sRecord, sArgument )
         read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
         call Assert( iStat == 0, &
            "Cannot read real data value" )
         pGraph(iVarNum)%rZSTEP(iTimeFrame) = rValue

         pGraph(iVarNum)%rZOR(iTimeFrame) = pGraph(iVarNum)%rZA(iTimeFrame)

      else
         write(UNIT=LU_LOG,FMT=*)  "FAILED to set DISLIN parameter; make sure that the "
         write(UNIT=LU_LOG,FMT=*)   "  DISLIN variable name has been set and is valid"
      end if
      flush(UNIT=LU_LOG)

    else if ( TRIM(sItem) == "SET_WINDOW_SIZE" ) then
      if(iVarNum>0 .and. iVarNum <= iNUM_VARIABLES) then
         call Chomp ( sRecord, sArgument )
         read ( unit=sArgument, fmt=*, iostat=iStat ) iValue
         call Assert( iStat == 0, &
            "Cannot read integer data value" )
         pGraph(iVarNum)%iWinSizeX = iValue

         call Chomp ( sRecord, sArgument )
         read ( unit=sArgument, fmt=*, iostat=iStat ) iValue
         call Assert( iStat == 0, &
            "Cannot integer data value" )
         pGraph(iVarNum)%iWinSizeY = iValue

      else
         write(UNIT=LU_LOG,FMT=*)  "FAILED to set DISLIN parameter; make sure that the "
         write(UNIT=LU_LOG,FMT=*)   "  DISLIN variable name has been set and is valid"
      end if
      flush(UNIT=LU_LOG)
#endif

    else if ( sItem == "DRIPPS_COMPATIBILITY" ) then
      write(UNIT=LU_LOG,FMT=*) &
        "DRIPPS_COMPATIBILITY is no longer a valid directive. Ignoring."

    else if ( sItem == "OUTPUT_OPTIONS" ) then
      call Chomp ( sRecord, sArgument )
      write(UNIT=LU_LOG,FMT=*) "Setting the output options (daily, monthly, annual)"
      do i=1,iNUM_VARIABLES
        if(TRIM(sArgument) == TRIM(STAT_INFO(i)%sVARIABLE_NAME)) then
          iVarNum = i
          exit
        end if
      end do

    else if ( sItem == "INITIAL_ABSTRACTION_METHOD" ) then
      write(UNIT=LU_LOG,FMT=*) "Setting initial abstraction method"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      if ( trim(sOption) == "TR55" ) then
        pConfig%iConfigureInitialAbstraction = CONFIG_SM_INIT_ABSTRACTION_TR55
      else if (trim(sOption) == "HAWKINS" ) then
        pConfig%iConfigureInitialAbstraction = CONFIG_SM_INIT_ABSTRACTION_HAWKINS
      else
        call Assert( .false._T_LOGICAL, "Illegal initial abstraction method specified" )
      end if
      flush(UNIT=LU_LOG)

    else if ( sItem == "ANSI_COLORS" ) then
      write(UNIT=LU_LOG,FMT=*) "Setting ANSI color screen output option"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      if ( trim(sOption) == "TRUE" ) then
        pConfig%lANSI_Colors = lTRUE
      else
        pConfig%lANSI_Colors = lFALSE
      end if
      flush(UNIT=LU_LOG)

    else if ( sItem == "INITIAL_SNOW_COVER" ) then
      write(UNIT=LU_LOG,FMT=*) "Reading initial snow cover"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      if ( trim(sOption) == "CONSTANT" ) then
        read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
        call Assert( iStat == 0, "Cannot read real data value for initial snow cover" )
        pGrd%Cells%rSnowCover = rValue
      else if ( trim(sOption) == "ARC_GRID" ) then
        write(UNIT=LU_LOG,FMT=*) "Snow cover data will be read as an ARC grid"
        input_grd => grid_Read( sArgument, sOption, T_SGL_GRID )
        call Assert( grid_Conform( pGrd, input_grd ), &
                      "Non-conforming grid" )
        pGrd%Cells%rSnowCover = input_grd%rData
        call grid_Destroy( input_grd )
      else if ( trim(sOption) == "SURFER" ) then
        write(UNIT=LU_LOG,FMT=*) "Snow cover data will be read as a SURFER grid"
        input_grd => grid_Read( sArgument, sOption, T_SGL_GRID )
        call Assert( grid_Conform( pGrd, input_grd ), &
                      "Non-conforming grid" )
        pGrd%Cells%rSnowCover = input_grd%rData
        call grid_Destroy( input_grd )
      else
        call Assert( .false._T_LOGICAL, "Illegal snow cover input format specified" )
      end if
      flush(UNIT=LU_LOG)

    else if ( sItem == "RUNOFF" ) then
      write(UNIT=LU_LOG,FMT=*) "Configuring runoff options"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      if ( trim(sOption) == "C-N" ) then
        pConfig%iConfigureRunoff = CONFIG_RUNOFF_CURVE_NUMBER
        write(UNIT=LU_LOG,FMT=*) "Configuring the curve number runoff model"
      else if ( trim(sOption) == "G-A" ) then
        pConfig%iConfigureRunoff = CONFIG_RUNOFF_GREEN_AMPT
        write(UNIT=LU_LOG,FMT=*) "Configuring the modified Green-Ampt infiltration model"
        !!!STEVE
        !!!PUT INITIALIZATION CODE FOR G-A IN HERE (IF NEEDED)
      else
        call Assert( .false._T_LOGICAL, "Illegal runoff option specified" )
      end if
      ! Look for a solution mode?
      if (len_trim(sRecord) > 0) then
        call Chomp ( sRecord, sOption )
        call Uppercase ( sOption )
        if ( trim(sOption) == "ITERATIVE" ) then
          pConfig%iConfigureRunoffMode = CONFIG_RUNOFF_ITERATIVE
          write(UNIT=LU_LOG,FMT=*) "Configuring the iterative runoff procedure"
        else if ( trim(sOption) == "DOWNHILL" ) then
          pConfig%iConfigureRunoffMode = CONFIG_RUNOFF_DOWNHILL
          write(UNIT=LU_LOG,FMT=*) "Configuring the downhill runoff procedure"
        else if ( trim(sOption) == "NO_ROUTING" ) then
          pConfig%iConfigureRunoffMode = CONFIG_RUNOFF_NO_ROUTING
          write(UNIT=LU_LOG,FMT=*) "NO FLOW ROUTING WILL BE PERFORMED"
        else
          call Assert( .false._T_LOGICAL, "Illegal runoff procedure option specified" )
        end if
      end if
      flush(UNIT=LU_LOG)

    else if ( sItem == "SNOW" ) then
      write(UNIT=LU_LOG,FMT=*) "Configuring snow module options"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      if ( trim(sOption) == "ORIGINAL_SNOW_MODULE" ) then
        pConfig%iConfigureSnow = CONFIG_SNOW_ORIGINAL_SWB
        write(UNIT=LU_LOG,FMT=*) "Snow calculations will be made using " &
                       //"the original SWB formulations"
      else if ( trim(sOption) == "NEW_SNOW_MODULE" ) then
        pConfig%iConfigureSnow = CONFIG_SNOW_NEW_SWB
        write(UNIT=LU_LOG,FMT=*) "Snow calculations will be made using " &
                       //"*NEW* SWB formulations"
      else
        call Assert( .false._T_LOGICAL, "Illegal snow module option specified" )
      end if
      flush(UNIT=LU_LOG)

    else if ( sItem == "ITERATIVE_METHOD_TOLERANCE" ) then
      write(UNIT=LU_LOG,FMT=*) "Setting iterative method solution tolerance"
      call Chomp ( sRecord, sOption )
      read ( unit=sOption, fmt=*, iostat=iStat ) rValue
      pConfig%rIterationTolerance = rValue
      flush(UNIT=LU_LOG)

    else if ( sItem == "ET" ) then
      write(UNIT=LU_LOG,FMT=*) "Configuring ET options"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      if ( trim(sOption) == "T-M" ) then
        pConfig%iConfigureET = CONFIG_ET_THORNTHWAITE_MATHER
        call et_tm_configure( sRecord )
      else if ( trim(sOption) == "TURC" ) then
        pConfig%iConfigureET = CONFIG_ET_TURC
        call et_turc_configure( sRecord )
      else if ( trim(sOption) == "J-H" ) then
        pConfig%iConfigureET = CONFIG_ET_JENSEN_HAISE
        call et_jh_configure( sRecord )
      else if ( trim(sOption) == "B-C" ) then
        pConfig%iConfigureET = CONFIG_ET_BLANEY_CRIDDLE
        call et_bc_configure( sRecord )
      else if ( trim(sOption) == "HARGREAVES" ) then
        pConfig%iConfigureET = CONFIG_ET_HARGREAVES
        call et_hargreaves_configure( pConfig, sRecord )
      else
        call Assert( .false._T_LOGICAL, "Illegal ET option specified" )
      end if
      flush(UNIT=LU_LOG)

    else if ( sItem == "SM" ) then
      write(UNIT=LU_LOG,FMT=*) "Configuring soil-moisture options"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      if ( trim(sOption) == "T-M" ) then
        pConfig%iConfigureSM = CONFIG_SM_THORNTHWAITE_MATHER
        call sm_thornthwaite_mather_Configure( sRecord )
      else
        call Assert( .false._T_LOGICAL, "Illegal soil-moisture option specified" )
      end if
      flush(UNIT=LU_LOG)

    else if ( sItem == "OUTPUT_FORMAT" ) then
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      if ( trim(sOption) == "ARC_GRID" ) then
        pConfig%iOutputFormat = OUTPUT_ARC
        write(UNIT=LU_LOG,FMT=*) "Selecting ARC output format"
      else if ( trim(sOption) == "SURFER" ) then
        pConfig%iOutputFormat = OUTPUT_SURFER
        write(UNIT=LU_LOG,FMT=*) "Selecting SURFER output format"
      else
        call Assert( .false._T_LOGICAL, "Illegal output format specified" )
      end if
      flush(UNIT=LU_LOG)

    else if ( sItem == "OUTPUT_GRID_SUFFIX" ) then
      write(UNIT=LU_LOG,FMT=*) "Setting output grid file suffix"
      call Chomp ( sRecord, pConfig%sOutputFileSuffix )
      if ( len_trim(pConfig%sOutputFileSuffix) == 0 ) then
        write(UNIT=LU_LOG,FMT=*)  "[No output grid file suffix specified.  Default is 'grd']"
        pConfig%sOutputFileSuffix = "grd"
      endif
      flush(UNIT=LU_LOG)

    else if ( sItem == "OUTPUT_GRID_PREFIX" ) then
      write(UNIT=LU_LOG,FMT=*) "Setting output grid file prefix"
      call Chomp ( sRecord, pConfig%sOutputFilePrefix )
      if ( len_trim(pConfig%sOutputFileSuffix) == 0 ) then
        write(UNIT=LU_LOG,FMT=*)  "[No output grid file prefix specified.  Default is NULL]"
        pConfig%sOutputFilePrefix = ""
      endif
      flush(UNIT=LU_LOG)

    else if ( sItem == "SOLVE" ) then
      write(UNIT=LU_LOG,FMT=*) "Solving the model"
      flush(UNIT=LU_LOG)
      call Chomp ( sRecord, sArgument )
      call Assert (LOGICAL(len_trim(sArgument)>0,kind=T_LOGICAL), &
         "No time-series data file specified" )
      pConfig%sTimeSeriesFilename = trim(sArgument)
      ! if first time through, open file and grab the year so we know
      ! when the simulation starts
      if(pConfig%lFirstYearOfSimulation) then
        open ( unit=LU_TS, file=pConfig%sTimeSeriesFilename, status="OLD", iostat=iStat )
        call Assert ( iStat == 0, &
         "Can't open time-series data file" )
        call model_ReadTimeSeriesFile(pTSt)
        pConfig%iStartYear = pTSt%iYear
        pConfig%iStartJulianDay = julian_day ( pConfig%iStartYear, 1, 1)
        ! current julian day will be incremented in model_Solve
        pConfig%iCurrentJulianDay = pConfig%iStartJulianDay - 1
        close( unit=LU_TS )
      end if
      ! actual call to "model_Solve" subroutine
      call model_Solve( pGrd, pConfig, pGraph)

    else if ( sItem == "SOLVE_NO_TS_DATA" ) then
      write(UNIT=LU_LOG,FMT=*) &
        "Solving the model - no single-station time series data will be read"
      flush(UNIT=LU_LOG)
      call Chomp ( sRecord, sArgument )
      call Assert (LOGICAL(len_trim(sArgument)>0,kind=T_LOGICAL), &
         "Must specify the year to begin simulation" )
      read(sArgument,FMT=*,iostat=iStat) pConfig%iStartYear
      call Assert( iStat == 0, &
         "Cannot read integer data value for beginning year of simulation" )
      pConfig%iStartJulianDay = julian_day ( pConfig%iStartYear, 1, 1)

      call Chomp ( sRecord, sArgument )
      call Assert (LOGICAL(len_trim(sArgument)>0,kind=T_LOGICAL), &
         "Must specify the year to end simulation" )
      read(sArgument,FMT=*,iostat=iStat) pConfig%iEndYear
      call Assert( iStat == 0, &
         "Cannot read integer data value for ending year of simulation" )
      call Assert( LOGICAL(pConfig%iStartYear<=pConfig%iEndYear,kind=T_LOGICAL), &
         "Ending year must be equal to or greater than the beginning year" )
      pConfig%iEndJulianDay = julian_day ( pConfig%iEndYear, 12, 31)
      pConfig%iCurrentJulianDay = pConfig%iStartJulianDay
      call gregorian_date(pConfig%iStartJulianDay, pConfig%iYear, &
         pConfig%iMonth, pConfig%iDay)

      do i=pConfig%iStartYear,pConfig%iEndYear
        pConfig%iYear = i
        pConfig%iCurrentJulianDay = julian_day ( i, 1, 1)
      call gregorian_date(pConfig%iCurrentJulianDay, pConfig%iYear, &
         pConfig%iMonth, pConfig%iDay)
        pConfig%iNumDaysInYear = num_days_in_year(pConfig%iYear)
        write(UNIT=LU_LOG,FMT="(a,i4.4)") "Calling model_Solve." &
          // "  Current year = ",i
        flush(UNIT=LU_LOG)
        ! actual call to "model_Solve" subroutine
        call model_Solve( pGrd, pConfig, pGraph)
      end do

    else if ( trim(sItem) == "CALC_BASIN_STATS" ) then
      call stats_CalcBasinStats(pGrd, pConfig, pGraph)

    else if (trim(sItem) == "SUPPRESS_SCREEN_OUTPUT" ) then
        pConfig%lWriteToScreen = lFALSE

    else if (trim(sItem) == "SUPPRESS_INTEGRATED_OUTPUT" ) then
        pConfig%lUseSWBRead = lTRUE

    else if (trim(sItem) == "SUPPRESS_DISLIN_MESSAGES" ) then
        pGraph%lEnableDislinMessages = lFALSE

    else if (trim(sItem) == "SUPPRESS_DAILY_FILES" ) then
        pConfig%lReportDaily = lFALSE

#ifdef STREAM_INTERACTIONS
    else if (trim(sItem) == "STREAM_CONFIG" ) then
        call chomp(sRecord, sOption)
        read (unit=sOption, fmt=*, iostat=istat) idx
        call Assert(LOGICAL(idx <= STREAM_INTERACTIONS_MAX, kind=T_LOGICAL), &
           "STREAM_CONFIG index cannot exceed the value of STREAM_INTERACTIONS_MAX", &
           TRIM(__FILE__),__LINE__)

        ! Get the daily cell maximum inflow
        call chomp(sRecord, sOption)
        read (unit=sOption, fmt=*, iostat=istat) pconfig%rStreamMaxInflow(idx)
        call Assert(pconfig%rStreamMaxInflow(idx) > rZERO, &
                    "Maximum STREAM_CONFIG cell daily inflow must be positive")
        ! Get the daily cell maximum recharge
        call chomp(sRecord, sOption)
        read (unit=sOption, fmt=*, iostat=istat) pconfig%rStreamMaxCapture(idx)
        call Assert(pconfig%rStreamMaxCapture(idx) > rZERO, &
                    "Maximum STREAM_CONFIG cell daily capture must be positive")

    else if ( sItem == "STREAM_CAPTURE" ) then
      write(UNIT=LU_LOG,FMT=*) "Populating stream capture index grid"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      if ( trim(sOption) == "CONSTANT" ) then
        read ( unit=sArgument, fmt=*, iostat=iStat ) iValue
        call Assert( iStat == 0, "Cannot read integer data value" )
        pGrd%Cells%iStreamIndex = iValue
      else
        input_grd => grid_Read( sArgument, sOption, T_INT_GRID )
        call Assert( grid_Conform( pGrd, input_grd ), &
                      "Non-conforming grid" )
        pGrd%Cells%iStreamIndex = input_grd%iData
        call grid_Destroy( input_grd )
      end if
      flush(UNIT=LU_LOG)

#endif

    else if ( trim(sItem) == "EOJ" ) then
      call stats_CalcMeanRecharge(pGrd, pConfig, pGraph)
      call stats_CalcMeanRechargebyLU(pGrd, pConfig, pGraph)
      write(UNIT=LU_LOG,FMT=*) "Job complete."
      call model_EndOfRun(pGrd, pConfig, pGraph)
      close(unit=LU_LOG)
      exit

    else
      call Assert( lFALSE, "Illegal directive was detected - " // sItem )
      end if

  end do CTL_READ  ! end of control file read loop

  DEALLOCATE(pConfig, STAT=iStat)
  call Assert( iStat == 0, &
     "Could not deallocate memory for model control data structure")

  DEALLOCATE(pGraph, STAT=iStat)
  call Assert( iStat == 0, &
     "Could not deallocate memory for plot control data structure")

  DEALLOCATE(pTSt, STAT=iStat)
  call Assert( iStat == 0, &
     "Could not deallocate memory for time-series data structure", &
     TRIM(__FILE__),__LINE__)

  close(UNIT=LU_CONTROL)

  return

end subroutine model_Run

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

subroutine model_Solve( pGrd, pConfig, pGraph )

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd, pTempGrid    ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig      ! pointer to data structure that contains
                                                        ! model options, flags, and other settings

  type (T_GRAPH_CONFIGURATION), dimension(:), pointer :: pGraph
       ! pointer to data structure that holds parameters for creating
       ! DISLIN plots

#ifdef NETCDF_SUPPORT
  type (T_NETCDF_FILE), pointer :: pNC
#endif

  ! [ LOCALS ]
  integer (kind=T_INT) :: i, j, k, iStat, iDayOfYear, iMonth
!  integer (kind=T_INT) :: iDay, iYear, tj, ti
  integer (kind=T_INT) :: tj, ti
  integer (kind=T_INT) :: iTempDay, iTempMonth, iTempYear
  integer (kind=T_INT) :: iPos
  integer (kind=T_INT) :: jj, ii, iNChange, iUpstreamCount, iPasses
  character(len=3) :: sMonthName
  logical (kind=T_LOGICAL) :: lMonthEnd
!  real (kind=T_SGL) :: rAvgT,rMinT,rMaxT,rPrecip,rRH,rMinRH,rWindSpd,rSunPct
  integer (kind=T_INT),save :: iNumGridCells
!  integer (kind=T_INT) :: iNumDaysInYear
!  integer (kind=T_INT) :: iEndOfYearJulianDay

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

    call stats_OpenBinaryFiles(pConfig, pGrd)

    call stats_InitializeVolumeConversion(pGrd)

#ifdef DEBUG_PRINT
      call grid_WriteArcGrid("SSF_Grid_Cells."//trim(pConfig%sOutputFileSuffix), &
          pGrd%rX0, pGrd%rX1,pGrd%rY0,pGrd%rY1,REAL(pGrd%Cells(:,:)%iNumFilesSSF) )
#endif

    ! open file into which daily summaries of variables will be written
    if ( pConfig%lReportDaily ) then
      open(unit=LU_MSB_REPORT, file='recharge_daily_report.csv',iostat=iStat,&
         status='REPLACE')
      call Assert(iStat == 0, &
         "Problem opening mass balance report file for output.")

      write(LU_MSB_REPORT,FMT="(300A)") 'Month,Day,Year,Date,Day of Year,' &
      // 'Mean Avg Temp,' &
      // 'Mean Min Temp,' &
      // 'Mean Max Temp,' &
      // 'CFGI,' &
      // 'Min Adj CN,' &
      // 'Mean Adj CN,' &
      // 'Max Adj CN,' &
      // 'Gross Precip, Interception,' &
      // 'Net Precip,' &
      // 'Snowfall,' &
      // 'Net Rainfall,' &
      // 'TOTAL Surface Storage (snow),' &
      // 'Change in Surface Storage (snow),' &
      // 'Snowmelt,' &
      // 'TOTAL Soil Moisture Storage,' &
      // 'Change in Soil Moisture Storage, Surface Flow Out of Grid,' &
      // 'Rejected Recharge,' &
      // 'Actual Evapotranspiration,' &
      // 'Recharge,' &
#ifdef STREAM_INTERACTIONS
      // 'Stream Capture,' &
#endif
#ifdef IRRIGATION_MODULE
      // 'Irrigation applied,' &
#endif
      // 'MASS BALANCE'

    end if

    ! open CSV file for daily stats summary
    if ( pConfig%lReportDaily ) then
      open(unit=LU_CSV, file='recharge_daily_statistics.csv',iostat=iStat,&
         status='REPLACE')
      call Assert(iStat == 0, &
         "Problem opening CSV file for summary statistics output.")
      call stats_WriteDailyAccumulatorHeaderCSV(LU_CSV)
    end if

    ! open CSV file for annual stats summary
    open(unit=LU_CSV_ANNUAL, file='recharge_annual_statistics.csv',iostat=iStat,&
       status='REPLACE')
    call Assert(iStat == 0, &
       "Problem opening CSV file for summary annual statistics output.")
    call stats_WriteAnnualAccumulatorHeaderCSV(LU_CSV_ANNUAL)

    iNumGridCells = pGrd%iNX * pGrd%iNY

    ! Time the run
    call cpu_time(rStartTime)

     !!! TESTING BLOCK - NetCDF output
!    pTempGrid=>grid_Create( pGrd%iNX, pGrd%iNY, pGrd%rX0, pGrd%rY0, &
!             pGrd%rX1, pGrd%rY1, T_SGL_GRID )
!   pTempGrid%rData = pGrd%Cells%rSoilMoisture
!    pNC => pConfig%NETCDF_FILE(iSOIL_MOISTURE,iNC_OUTPUT)
!    pNC%sVarName = "soil_moist"
!    pNC%iNCID = netcdf_create("Initial_Soil_Moisture.nc")
!    call netcdf_write_attributes(iSOIL_MOISTURE, iNC_OUTPUT, pConfig, pGrd)
!    call netcdf_write_variable(iSOIL_MOISTURE, iNC_OUTPUT, pConfig, &
!        pTempGrid, 1)
!    call grid_destroy(pTempGrid)
!    call netcdf_check(nf90_close(pNC%iNCID),TRIM(__FILE__),__LINE__)
    !!! END OF TESTING BLOCK - NetCDF output

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

    write(UNIT=LU_LOG,FMT=*)  "model.f95: model_InitializeET"
    flush(unit=LU_LOG)
    call model_InitializeET( pGrd, pConfig )

  end if FIRST_YEAR

  ! close any existing open time-series files...
  close(LU_TS)

  if(.not. pConfig%lGriddedData) then
  ! Connect to the single-site time-series file
    open ( unit=LU_TS, file=pConfig%sTimeSeriesFilename, &
      status="OLD", iostat=iStat )
    write(UNIT=LU_LOG,FMT=*)  "Opening time series file: ", &
      TRIM(pConfig%sTimeSeriesFilename)
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
          write(unit=sBuf,FMT=*) "Time series file ends prematurely: "//TRIM(pConfig%sTimeSeriesFilename)
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
    FIRST_YEAR_OR_DYN_LU: if(pConfig%lFirstDayOfSimulation &
      .or. (pConfig%iMonth == 1 .and. pConfig%iDay == 1 &
       .and. (pConfig%iConfigureLanduse == CONFIG_LANDUSE_DYNAMIC_ARC_GRID &
       .or. pConfig%iConfigureLanduse == CONFIG_LANDUSE_DYNAMIC_SURFER) )) then

      if( pConfig%iConfigureLanduse == CONFIG_LANDUSE_DYNAMIC_ARC_GRID &
       .or. pConfig%iConfigureLanduse == CONFIG_LANDUSE_DYNAMIC_SURFER) then

        call model_GetDynamicLanduseValue( pGrd, pConfig, pConfig%iYear)

      endif

      ! Initialize the model
      write(UNIT=LU_LOG,FMT=*) "model.f95: calling model_InitializeSM"
      flush(unit=LU_LOG)
      call model_InitializeSM(pGrd, pConfig)

      write(UNIT=LU_LOG,FMT=*)  "model.f95: runoff_InitializeCurveNumber"
      flush(unit=LU_LOG)
      call runoff_InitializeCurveNumber( pGrd ,pConfig)

      write(UNIT=LU_LOG,FMT=*)  "model.f95: model_InitialMaxInfil"
      flush(unit=LU_LOG)
      call model_InitializeMaxInfil(pGrd, pConfig )

    end if FIRST_YEAR_OR_DYN_LU


    if(pConfig%lFirstDayOfSimulation) then
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

#ifdef NETCDF_SUPPORT
      call model_write_NetCDF_attributes(pConfig, pGrd)
#endif
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
      end if
    end do

    ! Initialize precipitation value for current day
    call model_GetDailyPrecipValue(pGrd, pConfig, pTS%rPrecip, &
       pConfig%iMonth, pConfig%iDay, pConfig%iYear)

    ! Initialize temperature values for current day
    call model_GetDailyTemperatureValue(pGrd, pConfig, &
       pTS%rAvgT, pTS%rMinT, pTS%rMaxT, pTS%rRH, &
       pConfig%iMonth, pConfig%iDay, pConfig%iYear)

    write(UNIT=LU_LOG,FMT="(1x,'Beginning calculations for day: '," &
         //"i3,4x,A3,4x,i2,'/',i2,'/',i4)") &
         pConfig%iDayOfYear,sMonthName,pConfig%iMonth,pConfig%iDay,pConfig%iYear

#ifdef DEBUG_PRINT
      call stats_WriteMinMeanMax (LU_LOG, "Precip: ", pGrd%Cells%rGrossPrecip)
#endif

    if(pConfig%lWriteToScreen) then
      write(UNIT=LU_STD_OUT,FMT="(t39,a,t53,a,t69,a)") "min","mean","max"
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

      call stats_WriteDailyAccumulatorValuesCSV(LU_CSV,pConfig%iMonth, &
         pConfig%iDay,pConfig%iYear)

      call stats_WriteMSBReport(pGrd,pConfig%iMonth,pConfig%iDay, &
         pConfig%iYear,pConfig%iDayOfYear)

    end if

    write ( unit=sBuf, fmt='("day",i3.3)' ) pConfig%iDayOfYear
    call model_WriteGrids(pGrd, pConfig, sBuf, pConfig%iDay, pConfig%iMonth, &
         pConfig%iYear, pConfig%iDayOfYear)

    ! Write the results at each month-end
    if ( lMonthEnd ) then

      if ( pConfig%lReportDaily ) call stats_CalcMonthlyMeans(pConfig%iMonth, pConfig%iDay)
!      call stats_WriteMonthlyReport (LU_STD_OUT, pGrd, sMonthName, iMonth)
      call model_WriteGrids(pGrd, pConfig, sMonthName,  pConfig%iDay, &
         pConfig%iMonth, pConfig%iYear, pConfig%iDayOfYear)

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

  call model_WriteGrids(pGrd, pConfig, "ANNUAL", pConfig%iDay, &
     pConfig%iMonth, pConfig%iYear, pConfig%iDayOfYear)

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
#ifdef NETCDF_SUPPORT
  type (T_NETCDF_FILE), pointer :: pNC
#endif
  integer (kind=T_INT) :: iNX, iNY        ! Grid dimensions
  real (kind=T_DBL) :: rX0, rY0          ! Lower-left corner (world coords)
  real (kind=T_DBL) :: rX1, rY1          ! Upper-right corner (world coords)
  integer (kind=T_INT) :: k

  ! trigger the call to reconstitute the output grids and plots from the
  ! compressed binary files, if desired
  if(.not. pConfig%lUseSWBRead) &
    call stats_RewriteGrids(pGrd%iNX, pGrd%iNY, pGrd%rX0, pGrd%rY0, pGrd%rX1, &
        pGrd%rY1, pConfig, pGraph)

  ! preserve grid dimensions before destroying model grid data structure
  iNX = pGrd%iNX
  iNY = pGrd%iNY
  rX0 = pGrd%rX0
  rY0 = pGrd%rY0
  rX1 = pGrd%rX1
  rY1 = pGrd%rY1

  ! destroy model grid to free up memory
  call grid_Destroy(pGrd)

  ! finalize and close any open NetCDF or binary output files
  do k=1,iNUM_VARIABLES

#ifdef NETCDF_SUPPORT
    ! close any NetCDF files that have been opened for output
    if(STAT_INFO(k)%iNetCDFOutput > iNONE ) then

      pNC => pConfig%NETCDF_FILE(k,iNC_OUTPUT)
      call netcdf_check(nf90_close(pNC%iNCID))
      write(UNIT=LU_LOG,FMT=*)  "model.f95: closed NetCDF file "// &
         TRIM(STAT_INFO(k)%sVARIABLE_NAME)//".nc"
      flush(unit=LU_LOG)

    end if

#endif

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
    close ( unit=LU_CSV )
    close ( unit=LU_CSV_ANNUAL )
  end if

  ! close any remaining binary output files
  call stats_CloseBinaryFiles()

  ! how long did all this take, anyway?
  call cpu_time(rEndTime)
  print "(//1x,'SWB run completed in: ',f10.2,' minutes')", &
    (rEndTime-rStartTime)/60.0_T_SGL
  write(unit=LU_LOG,fmt="(//1x,'SWB run completed in: ',f10.2, ' minutes')"), &
    (rEndTime-rStartTime)/60.0_T_SGL

  return
end subroutine model_EndOfRun

!!***


subroutine model_GetDynamicLanduseValue( pGrd, pConfig, iYear)
  !! Populates annual dynamic landuse value on a cell-by-cell basis
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  integer (kind=T_INT), intent(in) :: iYear
  ! [ LOCALS ]
  type (T_GENERAL_GRID),pointer :: input_grd      ! Pointer to temporary grid for I/O
  character (len=256) sBuf

  input_grd=>null()

  write(unit=LU_LOG,fmt="('Reading dynamic landuse data from file: '," &
     //"A,'_',i4,'.',A)") trim(pConfig%sDynamicLanduseFilePrefix), &
     iYear,trim(pConfig%sOutputFileSuffix)

  select case( pConfig%iConfigureLanduse )
    case( CONFIG_LANDUSE_DYNAMIC_ARC_GRID )
      write ( unit=sBuf, fmt='(A,"_",i4,".",A)' ) &
         trim(pConfig%sDynamicLanduseFilePrefix), iYear,trim(pConfig%sOutputFileSuffix)
      input_grd => grid_Read( sBuf, "ARC_GRID", T_INT_GRID )
      call Assert( grid_Conform( pGrd, input_grd ), &
                      "Non-conforming grid - filename: " // sBuf )
      pGrd%Cells%iLanduse = input_grd%iData
      call grid_Destroy( input_grd )
    case( CONFIG_LANDUSE_DYNAMIC_SURFER )
      write ( unit=sBuf, fmt='(A,"_",i4,".",A)' ) &
         trim(pConfig%sDynamicLanduseFilePrefix), iYear,trim(pConfig%sOutputFileSuffix)
      input_grd => grid_Read( sBuf, "SURFER", T_INT_GRID )
      call Assert( grid_Conform( pGrd, input_grd ), &
                      "Non-conforming grid - filename: " // sBuf )
      pGrd%Cells%iLanduse = input_grd%iData
      call grid_Destroy( input_grd )
!#ifdef NETCDF_SUPPORT
!    case( CONFIG_LANDUSE_DYNAMIC_NETCDF )
!call netcdf_read( iGROSS_PRECIP, iNC_INPUT, pConfig, pGrd, input_grd, &
!         JULIAN_DAY(iYear, iMonth, iDay))
!      pGrd%Cells%rGrossPrecip = input_grd%rData
!      call grid_Destroy( input_grd )
!#endif

    case default
        call Assert ( lFALSE, "Internal error -- unknown landuse input type" )
  end select

end subroutine model_GetDynamicLanduseValue


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

subroutine model_GetDailyPrecipValue( pGrd, pConfig, rPrecip, iMonth, iDay, iYear)
  !! Populates Gross precipitation value on a cell-by-cell basis
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  real (kind=T_SGL), intent(in) :: rPrecip
  integer (kind=T_INT), intent(in) :: iMonth
  integer (kind=T_INT), intent(in) :: iDay
  integer (kind=T_INT), intent(in) :: iYear
  ! [ LOCALS ]
  type (T_GENERAL_GRID),pointer :: input_grd      ! Pointer to temporary grid for I/O
  real (kind=T_DBL) :: rMin, rMean, rMax, rSum
  integer (kind=T_INT) :: iCount, iNegCount
  character (len=256) sBuf

  input_grd=>null()

  select case( pConfig%iConfigurePrecip )
    case( CONFIG_PRECIP_SINGLE_STATION)
      pGrd%Cells(:,:)%rGrossPrecip = rPrecip  ! use a single value for entire grid
    case( CONFIG_PRECIP_ARC_GRID )
      write ( unit=sBuf, fmt='(A,"_",i4,"_",i2.2,"_",i2.2,".",A)' ) &
         trim(pConfig%sPrecipFilePrefix), iYear,iMonth,iDay,trim(pConfig%sOutputFileSuffix)
      input_grd => grid_Read( sBuf, "ARC_GRID", T_SGL_GRID )
      call Assert( grid_Conform( pGrd, input_grd ), &
                      "Non-conforming grid - filename: " // sBuf )
      pGrd%Cells%rGrossPrecip = input_grd%rData
      call grid_Destroy( input_grd )
    case( CONFIG_PRECIP_SURFER_GRID )
      write ( unit=sBuf, fmt='(A,"_",i4,"_",i2.2,"_",i2.2,".",A)' ) &
         trim(pConfig%sPrecipFilePrefix), iYear,iMonth,iDay,trim(pConfig%sOutputFileSuffix)
      input_grd => grid_Read( sBuf, "SURFER", T_SGL_GRID )
      call Assert( grid_Conform( pGrd, input_grd ), &
                      "Non-conforming grid - filename: " // sBuf )
      pGrd%Cells%rGrossPrecip = input_grd%rData
      call grid_Destroy( input_grd )
#ifdef NETCDF_SUPPORT
    case( CONFIG_PRECIP_NETCDF )
call netcdf_read( iGROSS_PRECIP, iNC_INPUT, pConfig, pGrd, input_grd, &
         JULIAN_DAY(iYear, iMonth, iDay))
      pGrd%Cells%rGrossPrecip = input_grd%rData
      call grid_Destroy( input_grd )
#endif

    case default
        call Assert ( lFALSE, "Internal error -- unknown precipitation input type" )
  end select

  where (pGrd%Cells%rGrossPrecip < pConfig%rMinValidPrecip)
    pGrd%Cells%rGrossPrecip = rZERO
  end where

  rMin = minval(pGrd%Cells%rGrossPrecip)
  rMax = maxval(pGrd%Cells%rGrossPrecip)
  rSum = sum(pGrd%Cells%rGrossPrecip)
  iCount = size(pGrd%Cells%rGrossPrecip)
  iNegCount = COUNT(pGrd%Cells%rGrossPrecip<rZERO)

  call Assert(LOGICAL(rMin >= rZERO,T_LOGICAL), &
    "Negative or missing precipitation values are not allowed. " &
    //"("//trim(int2char(iNegCount))//" cells with values < 0.0)")

  if(iCount>0) then
    rMean = rSum / iCount
  else
    rMean = -9999.
  end if

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
     rMaxT, rRH, iMonth, iDay, iYear)

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
  ! [ LOCALS ]
  type (T_GENERAL_GRID),pointer :: input_grd      ! Pointer to temporary grid for I/O
  real (kind=T_DBL) :: rMin, rMean, rMax, rSum, rTFactor, rTempVal
  integer (kind=T_INT) :: iNumGridCells
  integer (kind=T_INT) :: iRow,iCol
  character (len=256) sBuf
  type (T_CELL),pointer :: cel

  input_grd=>null()

  select case( pConfig%iConfigureTemperature )
    case( CONFIG_TEMPERATURE_SINGLE_STATION)
      pGrd%Cells(:,:)%rTMin = rMinT  ! use a single value for entire grid
      pGrd%Cells(:,:)%rTMax = rMaxT
      pGrd%Cells(:,:)%rTAvg = rAvgT

      !! Adjust cell-by-cell temperature
      if ( pconfig%lElevAdjustment ) then
        do iCol=1, pGrd%iNY
          do iRow=1, pGrd%iNX
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

    case( CONFIG_TEMPERATURE_ARC_GRID )
      write ( unit=sBuf, fmt='(A,"_",i4,"_",i2.2,"_",i2.2,".",A)' ) &
         trim(pConfig%sTMINFilePrefix), iYear,iMonth,iDay,trim(pConfig%sOutputFileSuffix)
      input_grd => grid_Read( sBuf, "ARC_GRID", T_SGL_GRID )
      call Assert( grid_Conform( pGrd, input_grd ), &
                      "Non-conforming grid - filename: " // sBuf )
      pGrd%Cells%rTMin = input_grd%rData
      call grid_Destroy( input_grd )
      input_grd=>null()

      write ( unit=sBuf, fmt='(A,"_",i4,"_",i2.2,"_",i2.2,".",A)' ) &
         trim(pConfig%sTMAXFilePrefix), iYear,iMonth,iDay,trim(pConfig%sOutputFileSuffix)
      input_grd => grid_Read( sBuf, "ARC_GRID", T_SGL_GRID )
      call Assert( grid_Conform( pGrd, input_grd ), &
                      "Non-conforming grid - filename: " // sBuf )
      pGrd%Cells%rTMax = input_grd%rData
      call grid_Destroy( input_grd )

      pGrd%Cells%rTAvg = (pGrd%Cells%rTMax + pGrd%Cells%rTMin) / 2_T_SGL

    case( CONFIG_TEMPERATURE_SURFER_GRID )
      write ( unit=sBuf, fmt='(A,"_",i2.2,"_",i2.2,"_",i4,".",A)' ) &
         trim(pConfig%sTMINFilePrefix), iMonth,iDay,iYear,trim(pConfig%sOutputFileSuffix)
      input_grd => grid_Read( sBuf, "SURFER", T_SGL_GRID )
      call Assert( grid_Conform( pGrd, input_grd ), &
                      "Non-conforming grid - filename: " // sBuf )
      pGrd%Cells%rTMin = input_grd%rData
      call grid_Destroy( input_grd )
      input_grd=>null()

      write ( unit=sBuf, fmt='(A,"_",i2.2,"_",i2.2,"_",i4,".",A)' ) &
         trim(pConfig%sTMAXFilePrefix), iMonth,iDay,iYear,trim(pConfig%sOutputFileSuffix)
      input_grd => grid_Read( sBuf, "SURFER", T_SGL_GRID )
      call Assert( grid_Conform( pGrd, input_grd ), &
                      "Non-conforming grid - filename: " // sBuf )
      pGrd%Cells%rTMax = input_grd%rData
      call grid_Destroy( input_grd )

      pGrd%Cells%rTAvg = (pGrd%Cells%rTMax + pGrd%Cells%rTMin) / 2_T_SGL

#ifdef NETCDF_SUPPORT
    case( CONFIG_TEMPERATURE_NETCDF )
      call netcdf_read( iMAX_TEMP, iNC_INPUT, pConfig, pGrd, input_grd, JULIAN_DAY(iYear, iMonth, iDay))
      pGrd%Cells%rTMax = input_grd%rData
      call grid_Destroy( input_grd )

      call netcdf_read( iMIN_TEMP, iNC_INPUT, pConfig, pGrd, input_grd, JULIAN_DAY(iYear, iMonth, iDay))
      pGrd%Cells%rTMin = input_grd%rData
      call grid_Destroy( input_grd )

      pGrd%Cells%rTAvg = (pGrd%Cells%rTMax + pGrd%Cells%rTMin) / 2_T_SGL
#endif

    case default
        call Assert ( lFALSE, "Internal error -- unknown temperature input type" )
  end select


  ! calculate number of cells in model grid
  iNumGridCells = pGrd%iNX * pGrd%iNY

  ! Scan through array of inputs looking for instances where the TMIN > TMAX
  ! (THIS CAN BE RE_WRITTEN USING MATRIX NOTATION)

  !$OMP DO


  do iCol=1,pGrd%iNX
    do iRow=1,pGrd%iNY

      cel=>pGrd%Cells(iRow,iCol)

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

  do iCol=1,pGrd%iNX
    do iRow=1,pGrd%iNY
      cel => pGrd%Cells(iRow,iCol)
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

  do iCol=1,pGrd%iNX  ! last subscript in a Fortran array should be the slowest-changing
    do iRow=1,pGrd%iNY
      cel => pGrd%Cells(iRow,iCol)

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
                          * ((rPI / 2.) - rA))) / rPI

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

  do iCol=1,pGrd%iNX  ! last subscript in a Fortran array should be the slowest-changing
    do iRow=1,pGrd%iNY
      cel => pGrd%Cells(iRow,iCol)
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
  real (kind=T_SGL) :: rPotentialMelt,rPotentialInterception,rInterception
  real (kind=T_SGL) :: rPreviousSnowCover,rChgInSnowCover
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
  pGrd%Cells(:,:)%rSnowWaterEquiv = rZERO

  ! calculate number of cells in model grid
  iNumGridCells = pGrd%iNX * pGrd%iNY

!  pGrd%Cells(:,:)%rPrevious_SnowCover = pGrd%Cells(:,:)%rSnowCover

  ! Use "potential interception" for each cell to compute net precip
  do iCol=1,pGrd%iNX
    do iRow=1,pGrd%iNY

     cel => pGrd%Cells(iRow,iCol)

     ! allow for correction factor to be applied to precip gage input data
     if ( cel%rTAvg - (cel%rTMax-cel%rTMin)/3.0_T_SGL <= rFREEZING ) then
       lFREEZING = lTRUE
       cel%rGrossPrecip = cel%rGrossPrecip * pConfig%rSnowWaterEquiv_Corr_Factor
     else
       lFREEZING = lFALSE
       cel%rGrossPrecip = cel%rGrossPrecip * pConfig%rRainfall_Corr_Factor
     end if

      rPotentialInterception = rf_model_GetInterception(pConfig,cel%iLandUse,iDayOfYear)

      rPreviousSnowCover = cel%rSnowCover

      cel%rNetPrecip = cel%rGrossPrecip-rPotentialInterception
      if ( cel%rNetPrecip < rZERO ) cel%rNetPrecip = rZERO
      rInterception = cel%rGrossPrecip - cel%rNetPrecip

      call Assert(LOGICAL(rInterception >= rZERO,kind=T_LOGICAL), &
         "Negative value for interception was calculated on day " &
         //int2char(iDayOfYear)//" iRow: "//trim(int2char(iRow))//"  iCol: "//trim(int2char(iCol)))

      call stats_UpdateAllAccumulatorsByCell(REAL(rInterception,kind=T_DBL), &
          iINTERCEPTION,iMonth,iZERO)

     if(STAT_INFO(iINTERCEPTION)%iDailyOutput > iNONE &
       .or. STAT_INFO(iINTERCEPTION)%iMonthlyOutput > iNONE &
       .or. STAT_INFO(iINTERCEPTION)%iAnnualOutput > iNONE)  then
        call RLE_writeByte(STAT_INFO(iINTERCEPTION)%iLU, &
           rInterception, pConfig%iRLE_MULT, pConfig%rRLE_OFFSET, &
           iNumGridCells, iINTERCEPTION)
     end if

!      cel%rAnnualInterception = cel%rAnnualInterception + cel%rInterception
!      rMonthlyInterception = rMonthlyInterception + cel%rInterception

      ! Is it snowing?
      if (lFREEZING ) then
        cel%rSnowCover = cel%rSnowCover + cel%rNetPrecip
!         rMonthlySnowFall = rMonthlySnowFall + sum(pGrd%Cells(:,:)%rNetPrecip)
        cel%rSnowWaterEquiv = cel%rNetPrecip
        cel%rNetPrecip = rZERO      ! For now -- if there is snowmelt, we do it next
      end if
      ! Is there any melting?
      if(cel%rTAvg>rFREEZING) then
        rPotentialMelt = rMELT_INDEX * ( cel%rTMax-rFREEZING )*rC_PER_F / rMM_PER_INCH
        if(cel%rSnowCover > rPotentialMelt) then
          cel%rSnowMelt = rPotentialMelt
          cel%rSnowCover = cel%rSnowCover - rPotentialMelt
        else
          cel%rSnowMelt = cel%rSnowCover
          cel%rSnowCover = rZERO
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
        REAL(cel%rSnowWaterEquiv,kind=T_DBL),iSNOWFALL,iMonth,iZERO)
      call stats_UpdateAllAccumulatorsByCell( &
        REAL(cel%rSnowCover,kind=T_DBL),iSNOWCOVER,iMonth,iZERO)

    end do
  end do

  ! a call to the UpdateAllAccumulatorsByCell subroutine with a value of "iNumGridCalls"
  ! as the final argument triggers the routine to update monthly and annual stats
  call stats_UpdateAllAccumulatorsByCell(rD_ZERO,iCHG_IN_SNOW_COV,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(rD_ZERO,iINTERCEPTION,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(rD_ZERO,iNET_PRECIP,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(rD_ZERO,iSNOWMELT,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(rD_ZERO,iSNOWFALL,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(rD_ZERO,iSNOWCOVER,iMonth,iNumGridCells)

  return

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
 ! pGrd%Cells(:,:)%rSnowWaterEquiv = rZERO

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

      cel => pGrd%Cells(iRow,iCol)

      ! initialize accumulators for this cell
      cel%rSnowMelt = rZERO
      cel%rSnowWaterEquiv = rZERO
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
       + (rONE - rFracRain) * cel%rGrossPrecip * pConfig%rSnowWaterEquiv_Corr_Factor

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

       cel%rSnowWaterEquiv = cel%rNetPrecip * (rONE - rFracRain)
       cel%rSnowFall = cel%rSnowWaterEquiv * snow_depth_Hedstrom(cel%rTAvg, pConfig)
       cel%rSnowCover = cel%rSnowCover + cel%rSnowWaterEquiv
       cel%rNetPrecip = cel%rNetPrecip - cel%rSnowWaterEquiv

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
        REAL(cel%rSnowWaterEquiv,kind=T_DBL),iSNOWFALL,iMonth,iZERO)
      call stats_UpdateAllAccumulatorsByCell( &
        REAL(cel%rSnowCover,kind=T_DBL),iSNOWCOVER,iMonth,iZERO)


    end do

    !$OMP END DO

  end do

  ! a call to the UpdateAllAccumulatorsByCell subroutine with a value of "iNumGridCalls"
  ! as the final argument triggers the routine to update monthly and annual stats
  call stats_UpdateAllAccumulatorsByCell(rD_ZERO,iCHG_IN_SNOW_COV,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(rD_ZERO,iINTERCEPTION,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(rD_ZERO,iNET_PRECIP,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(rD_ZERO,iSNOWMELT,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(rD_ZERO,iSNOWFALL,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(rD_ZERO,iSNOWCOVER,iMonth,iNumGridCells)

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
             pGrd%rX1, pGrd%rY1, T_INT_GRID )

  allocate(iOrderCol(pGrd%iNY*pGrd%iNX), iOrderRow(pGrd%iNY*pGrd%iNX), stat=iStat)
  call Assert( iStat == 0, &
     "Could not allocate order of solution vectors for downhill procedure")

  INQUIRE( FILE='swb_routing.bin', EXIST=lExist)
  EXISTS: if (.not. lExist) then

    iPasses = 0
    write(UNIT=LU_LOG,FMT=*) "Configuring the downhill routing table..."
    flush(UNIT=LU_LOG)
    iOrderCount = 0
    pGrd%Cells%lDownhillMarked = lFALSE

    do
      iNChange = 0
      do iCol=1,pGrd%iNX  ! last index in a Fortan array should be the slowest changing
        do iRow=1,pGrd%iNY

          cel => pGrd%Cells(iRow,iCol)
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
                    if (pGrd%Cells(iRowSub,iColSub)%lDownhillMarked) cycle  ! Don't count marked neighbors
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

    open(UNIT=LU_ROUTING, FILE='swb_routing.bin',FORM='UNFORMATTED', &
      status='REPLACE',ACCESS='STREAM')
    write(LU_ROUTING) iOrderCount

    do ic=1,iOrderCount
      write(LU_ROUTING) iOrderCol(ic),iOrderRow(ic)
    end do
    flush(UNIT=LU_ROUTING)
    close(UNIT=LU_ROUTING)

  else ! routing table already exists

    pGrd%Cells%lDownhillMarked = lTRUE
    open(UNIT=LU_ROUTING, FILE='swb_routing.bin',FORM='UNFORMATTED', &
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


  ! Reset the upstream flows (note that iOrderCount, iOrderCol, and iOrderRow are globals)
  do ic=1,iOrderCount

    cel => pGrd%Cells(iOrderRow(ic),iOrderCol(ic))
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

#ifdef STREAM_INTERACTIONS

    if(pGrd%Cells(iTgt_Row,iTgt_Col)%iStreamIndex > 0) then
      ! route outflow to a specific stream or fracture ID
      cel%rStreamCapture = cel%rOutFlow
      cel%rOutFlow = rZERO
    else if &
#else
    if &
#endif
      (pGrd%Cells(iTgt_Row,iTgt_Col)%iLandUse == pConfig%iOPEN_WATER_LU &
      .or. pGrd%Cells(iTgt_Row,iTgt_Col)%rSoilWaterCap<rNEAR_ZERO) then
      ! Don't route any further; the water has joined a generic
      ! surface water feature. We assume that once the water hits a
      ! surface water feature that the surface water drainage
      ! network transports the bulk of it
      ! out of the model domain quite rapidly
      cel%rFlowOutOfGrid = cel%rOutflow
      cel%rOutFlow = rZERO

    else
      ! add cell outflow to target cell inflow
!      tcel%rInFlow = tcel%rInFlow + cel%rOutFlow
       pGrd%Cells(iTgt_Row,iTgt_Col)%rInFlow = &
          pGrd%Cells(iTgt_Row,iTgt_Col)%rInFlow + cel%rOutFlow * cel%rRouteFraction
       cel%rFlowOutOfGrid = cel%rOutflow * &
                              (rONE - cel%rRouteFraction)
       cel%rOutflow = cel%rOutflow * cel%rRouteFraction
    end if

  end do

  return
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

  do iCol=1,pGrd%iNX
    do iRow=1,pGrd%iNY

      cel => pGrd%Cells(iRow,iCol)

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

  do iCol=1,pGrd%iNX
    do iRow=1,pGrd%iNY
      cel => pGrd%Cells(iRow,iCol)
      call model_DownstreamCell(pGrd,iRow,iCol,iTgt_Row,iTgt_Col)

      if ( iTgt_Row == iROUTE_DEPRESSION ) then
        ! Don't route any further; the water pools here.
        cel%rOutFlow = rZERO
!        call stats_UpdateAllAccumulatorsByCell(rD_ZERO, &
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

  do iCol=1,pGrd%iNX
    do iRow=1,pGrd%iNY

      select case (pGrd%Cells(iRow,iCol)%iFlowDir)
        case ( DIR_DEPRESSION )
          iTgt_Col = iROUTE_DEPRESSION               ! added Jan 2009 SMW
          iTgt_Row = iROUTE_DEPRESSION               ! added Jan 2009 SMW
          continue
        case ( DIR_RIGHT )
          iTgt_Row = iRow
          iTgt_Col = iCol+1
          if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
               iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
            if ( pGrd%Cells(iTgt_Row,iTgt_Col)%iFlowDir == DIR_LEFT ) then
              pGrd%Cells(iRow,iCol)%iFlowDir = DIR_DEPRESSION
              pGrd%Cells(iTgt_Row,iTgt_Col)%iFlowDir = DIR_DEPRESSION
              write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
            end if
          end if
        case ( DIR_DOWN_RIGHT )
          iTgt_Row = iRow+1
          iTgt_Col = iCol+1
          if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
               iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
            if ( pGrd%Cells(iTgt_Row,iTgt_Col)%iFlowDir == DIR_UP_LEFT ) then
              pGrd%Cells(iRow,iCol)%iFlowDir = DIR_DEPRESSION
              pGrd%Cells(iTgt_Row,iTgt_Col)%iFlowDir = DIR_DEPRESSION
              write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
            end if
          end if
        case ( DIR_DOWN )
          iTgt_Row = iRow+1
          iTgt_Col = iCol
          if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
               iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
            if ( pGrd%Cells(iTgt_Row,iTgt_Col)%iFlowDir == DIR_UP ) then
              pGrd%Cells(iRow,iCol)%iFlowDir = DIR_DEPRESSION
              pGrd%Cells(iTgt_Row,iTgt_Col)%iFlowDir = DIR_DEPRESSION
              write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
            end if
          end if
        case ( DIR_DOWN_LEFT )
          iTgt_Row = iRow+1
          iTgt_Col = iCol-1
          if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
               iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
            if ( pGrd%Cells(iTgt_Row,iTgt_Col)%iFlowDir == DIR_UP_RIGHT ) then
              pGrd%Cells(iRow,iCol)%iFlowDir = DIR_DEPRESSION
              pGrd%Cells(iTgt_Row,iTgt_Col)%iFlowDir = DIR_DEPRESSION
              write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
            end if
          end if
        case ( DIR_LEFT )
          iTgt_Row = iRow
          iTgt_Col = iCol-1
          if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
               iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
            if ( pGrd%Cells(iTgt_Row,iTgt_Col)%iFlowDir == DIR_RIGHT) then
              pGrd%Cells(iRow,iCol)%iFlowDir = DIR_DEPRESSION
              pGrd%Cells(iTgt_Row,iTgt_Col)%iFlowDir = DIR_DEPRESSION
              write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
            end if
          end if
        case ( DIR_UP_LEFT )
          iTgt_Row = iRow-1
          iTgt_Col = iCol-1
          if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
               iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
            if ( pGrd%Cells(iTgt_Row,iTgt_Col)%iFlowDir == DIR_DOWN_RIGHT) then
              pGrd%Cells(iRow,iCol)%iFlowDir = DIR_DEPRESSION
              pGrd%Cells(iTgt_Row,iTgt_Col)%iFlowDir = DIR_DEPRESSION
              write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
            end if
          end if
        case ( DIR_UP )
          iTgt_Row = iRow-1
          iTgt_Col = iCol
          if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
               iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
            if ( pGrd%Cells(iTgt_Row,iTgt_Col)%iFlowDir == DIR_DOWN) then
              pGrd%Cells(iRow,iCol)%iFlowDir = DIR_DEPRESSION
              pGrd%Cells(iTgt_Row,iTgt_Col)%iFlowDir = DIR_DEPRESSION
              write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
            end if
          end if
        case ( DIR_UP_RIGHT )
          iTgt_Row = iRow-1
          iTgt_Col = iCol+1
          if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
               iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
            if ( pGrd%Cells(iTgt_Row,iTgt_Col)%iFlowDir == DIR_DOWN_LEFT) then
              pGrd%Cells(iRow,iCol)%iFlowDir = DIR_DEPRESSION
              pGrd%Cells(iTgt_Row,iTgt_Col)%iFlowDir = DIR_DEPRESSION
              write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
            end if
          end if
        case default  !! flow direction indeterminate
        !!
        !!  NOTE: This may not be the correct way to deal with indeterminate
        !!        flow directions!!
        !!
          write ( unit=sBuf, fmt='("Flow direction grid element (",i3,",",i3,' &
             // '") contains undefined flow direction with integer value: ",i4)' ) &
                   iCol,iRow,pGrd%Cells(iRow,iCol)%iFlowDir
          write(UNIT=LU_LOG,FMT=*)  sBuf
          pGrd%Cells(iRow,iCol)%iFlowDir = DIR_DEPRESSION
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
      pGrd%Cells(iRow,iCol)%iTgt_Row = iTgt_Col
      pGrd%Cells(iRow,iCol)%iTgt_Col = iTgt_Row
    end do
  end do

#ifdef DEBUG_PRINT

  do iCol=1,pGrd%iNX
    do iRow=1,pGrd%iNY
      if(pGrd%Cells(iRow,iCol)%iTgt_Col==iCol .and. pGrd%Cells(iRow,iCol)%iTgt_Row==iRow) then
       write(unit=LU_LOG,FMT=*) 'ALERT** target is the same as the originating cell'
       write(unit=LU_LOG,FMT=*) '  ORIG   (iRow, iCol) : ',iRow, iCol
       write(unit=LU_LOG,FMT=*) '  ==> FLOWDIR: ',pGrd%Cells(iRow,iCol)%iFlowDir
       write(unit=LU_LOG,FMT=*) '  TARGET (iRow, iCol) : ',pGrd%Cells(iRow,iCol)%iTgt_Row, &
         pGrd%Cells(iRow,iCol)%iTgt_Col
       write(unit=LU_LOG,FMT=*) '  ==> FLOWDIR: ' , &
         pGrd%Cells(pGrd%Cells(iRow,iCol)%iTgt_Row,pGrd%Cells(iRow,iCol)%iTgt_Col)%iFlowDir
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

  select case (pGrd%Cells(iRow,iCol)%iFlowDir)
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
  open ( unit=LU_MASK, file=pConfig%sBasinMaskFilename, &
            status="OLD", iostat=iStat )
  call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
            "Open failed for file: " // pConfig%sBasinMaskFilename )

  ! read first line of file
  read ( unit=LU_MASK, fmt="(a256)", iostat=iStat ) sRecord
  call Assert( iStat == 0, &
     "Error reading first line of basin mask table" )

  ! read mask file to obtain expected number of basin mask files
  call Chomp_tab( sRecord, sItem )
  call Uppercase( sItem )
  if ( sItem == "NUM_BASIN_MASK_FILES" ) then
    call Chomp_tab ( sRecord, sItem )
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

    call Chomp_tab ( sRecord, sItem )
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%BMASK(iRecNum)%sUSGS_UpstreamOrderID
    call Assert( iStat == 0, &
      "Error reading upstream order ID in basin mask table" )
    write(UNIT=LU_LOG,FMT=*)  "Upstream order ID = ",TRIM(pConfig%BMASK(iRecNum)%sUSGS_UpstreamOrderID)

    call Chomp_tab ( sRecord, sItem )
    call Uppercase(sItem)
    pConfig%BMASK(iRecNum)%sBasinDescription = TRIM(sItem)
    call Assert( iStat == 0, &
      "Error reading basin description in basin mask table" )
    write(UNIT=LU_LOG,FMT=*)  "Basin description = ",TRIM(pConfig%BMASK(iRecNum)%sBasinDescription)

    call Chomp_tab ( sRecord, sItem )
    pConfig%BMASK(iRecNum)%sPestGroup = TRIM(ADJUSTL(sItem))
    call Assert( iStat == 0, &
      "Error reading PEST group in basin mask table" )
    write(UNIT=LU_LOG,FMT=*)  "PEST group = ",TRIM(pConfig%BMASK(iRecNum)%sPestGroup)

    call Chomp_tab ( sRecord, sItem )
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%BMASK(iRecNum)%rPestWeight
    call Assert( iStat == 0, &
      "Error reading PEST observation weight in basin mask table" )
    write(sBuf,FMT="(F12.3)") pConfig%BMASK(iRecNum)%rPestWeight
    write(UNIT=LU_LOG,FMT=*)  "PEST weight = "//TRIM(sBuf)

    call Chomp_tab ( sRecord, sItem )
!    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%BMASK(iRecNum)%sBasinMaskFilename
    pConfig%BMASK(iRecNum)%sBasinMaskFilename = TRIM(ADJUSTL(sItem))
    call Assert( iStat == 0, &
      "Error reading basin mask filename in basin mask table" )
    write(UNIT=LU_LOG,FMT=*)  "Basin mask filename = ",TRIM(pConfig%BMASK(iRecNum)%sBasinMaskFilename)

    call Chomp_tab ( sRecord, sItem )
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%BMASK(iRecNum)%sFileType
    call Assert( iStat == 0, &
      "Error reading basin mask file type in basin mask table" )
    write(UNIT=LU_LOG,FMT=*)  "Basin mask filetype = ",TRIM(pConfig%BMASK(iRecNum)%sFileType)

    call Chomp_tab ( sRecord, sItem )
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%BMASK(iRecNum)%rQb
    call Assert( iStat == 0, &
      "Error reading baseflow estimate Qb in basin mask table" )
    write(UNIT=LU_LOG,FMT=*)  "Qb = ",pConfig%BMASK(iRecNum)%rQb

    call Chomp_tab ( sRecord, sItem )
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
  open ( unit=LU_LOOKUP, file=pConfig%sLanduseLookupFilename, &
            status="OLD", iostat=iStat )
  call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
            "Open failed for file: " // pConfig%sLanduseLookupFilename )

  ! read first line of file
  read ( unit=LU_LOOKUP, fmt="(a)", iostat=iStat ) sRecord
  call Assert( iStat == 0, &
     "Error reading first line of landuse lookup table" )

  ! read landuse file to obtain expected number of landuse types
  call Chomp_tab( sRecord, sItem )
  call Uppercase( sItem )
  if ( sItem == "NUM_LANDUSE_TYPES" ) then
    call Chomp_tab ( sRecord, sItem )
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
  call Chomp_tab( sRecord, sItem )
  call Uppercase( sItem )
  if ( sItem == "NUM_SOIL_TYPES" ) then
    call Chomp_tab ( sRecord, sItem )
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

    call Chomp_tab ( sRecord, sItem )
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%LU(iRecNum)%iLandUseType
    call Assert( iStat == 0, "Error reading land use type in landuse lookup table" )
    write(UNIT=LU_LOG,FMT=*)  "  landuse type = ",pConfig%LU(iRecNum)%iLandUseType

    call Chomp_tab ( sRecord, pConfig%LU(iRecNum)%sLandUseDescription )
    write(UNIT=LU_LOG,FMT=*)  "  landuse description = ", &
      TRIM(pConfig%LU(iRecNum)%sLandUseDescription)

    call Chomp_tab ( sRecord, pConfig%LU(iRecNum)%sAssumedPercentImperviousness )
    write(UNIT=LU_LOG,FMT=*)  "  assumed % imperviousness = ", &
      TRIM(pConfig%LU(iRecNum)%sAssumedPercentImperviousness)

    do i=1,iNumSoilTypes
      call Chomp_tab ( sRecord, sItem )
      read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%CN(iRecNum,i)
      call Assert( iStat == 0, &
        "Error reading curve number for soil group in landuse lookup table" )
      write(UNIT=LU_LOG,FMT=*)  "  curve number for soil group",i,": ",pConfig%CN(iRecNum,i)
    end do

    do i=1,iNumSoilTypes
      call Chomp_tab ( sRecord, sItem )
      read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%MAX_RECHARGE(iRecNum,i)
      call Assert( iStat == 0, &
        "Error reading maximum recharge for soil group in landuse lookup table" )
      write(UNIT=LU_LOG,FMT=*)  "  MAXIMUM RECHARGE for soil group",i,": ",pConfig%MAX_RECHARGE(iRecNum,i)
    end do

    call Chomp_tab ( sRecord, sItem )
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%LU(iRecNum)%rIntercept_GrowingSeason
    call Assert( iStat == 0, "Error reading interception values in landuse file" )
    write(UNIT=LU_LOG,FMT=*)  "  Interception value for growing season = ",pConfig%LU(iRecNum)%rIntercept_GrowingSeason

    call Chomp_tab ( sRecord, sItem )
   read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%LU(iRecNum)%rIntercept_NonGrowingSeason
    call Assert( iStat == 0, "Error reading interception values in landuse file" )
    write(UNIT=LU_LOG,FMT=*)  "  Interception value for non-growing season = ", &
        pConfig%LU(iRecNum)%rIntercept_NonGrowingSeason

    ! now read in a rooting depth for each landuse/soil type combination
    do i=1,iNumSoilTypes
      call Chomp_tab ( sRecord, sItem )
      read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%ROOTING_DEPTH(iRecNum,i)
      call Assert( iStat == 0, &
        "Error reading rooting depth for soil group in landuse lookup table" )
      write(UNIT=LU_LOG,FMT=*)  "  ROOTING DEPTH for soil group",i,": ",pConfig%ROOTING_DEPTH(iRecNum,i)
    end do

!    call Chomp_tab ( sRecord, sItem )
!    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%LU(iRecNum)%lCONSTANT_ROOT_ZONE_DEPTH
!    call Assert( iStat == 0, "Error reading root-zone depth constant in landuse file" )
!    write(UNIT=LU_LOG,FMT=*)  "  Root zone treated as a constant value?  ", &
!        pConfig%LU(iRecNum)%lCONSTANT_ROOT_ZONE_DEPTH

!    do i=1,iNUM_ROOT_ZONE_PAIRS
!
!      call Chomp_tab ( sRecord, sItem )
!      read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%LU(iRecNum)%rX_ROOT_ZONE(i)
!      call Assert( iStat == 0, &
!       "Error reading root zone X value number in landuse file" )
!      write(UNIT=LU_LOG,FMT=*)  "    Root zone available water capacity (in/ft): X(",i,") = ", &
!          pConfig%LU(iRecNum)%rX_ROOT_ZONE(i)
!
!    end do
!
!    do i=1,iNUM_ROOT_ZONE_PAIRS
!
!      call Chomp_tab ( sRecord, sItem )
!      read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%LU(iRecNum)%rY_ROOT_ZONE(i)
!      call Assert( iStat == 0, "Error reading root zone Y values in landuse file" )
!      write(UNIT=LU_LOG,FMT=*)  "    Root zone root zone depth (ft):             Y(",i,") = ", &
!          pConfig%LU(iRecNum)%rY_ROOT_ZONE(i)
!
!    end do

    iRecNum = iRecNum + 1

  end do LU_READ

  pConfig%IRRIGATION%iLandUseType = pConfig%LU%iLandUseType

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
  open ( unit=LU_LOOKUP, file=pConfig%sIrrigationLookupFilename, &
            status="OLD", iostat=iStat )
  call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
            "Open failed for file: " // pConfig%sIrrigationLookupFilename )

  ! read first line of file
  read ( unit=LU_LOOKUP, fmt="(a)", iostat=iStat ) sRecord
  call Assert( iStat == 0, &
     "Error reading first line of irrigation lookup table" )

  ! read landuse file to obtain expected number of landuse types
  call Chomp_tab( sRecord, sItem )
  call Uppercase( sItem )
  if ( sItem == "NUM_LANDUSE_TYPES" ) then
    call Chomp_tab ( sRecord, sItem )
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

    call Chomp_tab ( sRecord, sItem )
    read ( unit=sItem, fmt=*, iostat=iStat ) iLandUseType
    call Assert( iStat == 0, "Error reading land use type in irrigation lookup table" )
    call Assert(iLandUseType == pConfig%LU(iRecNum)%iLandUseType, &
      "Landuse types in the irrigation lookup table must match those in the landuse lookup table", &
        trim(__FILE__), __LINE__)
    pConfig%IRRIGATION(iRecNum)%iLandUseType = iLandUseType

    call Chomp_tab ( sRecord, sItem )
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iRecNum)%rKc_Max
    call Assert( iStat == 0, &
      "Error reading maximum crop coefficient in irrigation lookup table" )
    write(UNIT=LU_LOG,FMT=*)  "  maximum crop coefficient ", &
        pConfig%IRRIGATION(iRecNum)%rKc_Max

    call Chomp_tab ( sRecord, sItem )
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iRecNum)%rK0
    call Assert( iStat == 0, &
      "Error reading first phenological stage crop coefficient in irrigation lookup table" )
    write(UNIT=LU_LOG,FMT=*)  "   first phenological stage crop coefficient ", &
        pConfig%IRRIGATION(iRecNum)%rK0

    call Chomp_tab ( sRecord, sItem )
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iRecNum)%rAlpha1
    call Assert( iStat == 0, &
      "Error reading shape coefficient (Alpha1) in irrigation lookup table" )
    write(UNIT=LU_LOG,FMT=*)  "   shape coefficient (Alpha1) ", &
        pConfig%IRRIGATION(iRecNum)%rAlpha1

    call Chomp_tab ( sRecord, sItem )
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iRecNum)%rGDD_Kc_Max
    call Assert( iStat == 0, &
      "Error reading GDD associated with max crop coefficient in irrigation lookup table" )
    write(UNIT=LU_LOG,FMT=*)  "   GDD associated with max crop coefficient ", &
        pConfig%IRRIGATION(iRecNum)%rGDD_Kc_Max

    call Chomp_tab ( sRecord, sItem )
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iRecNum)%rGDD_Death
    call Assert( iStat == 0, &
      "Error reading GDD associated with plant death in irrigation lookup table" )
    write(UNIT=LU_LOG,FMT=*)  "   GDD associated with plant death ", &
        pConfig%IRRIGATION(iRecNum)%rGDD_Death

    call Chomp_tab ( sRecord, sItem )
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iRecNum)%rGDD_BaseTemp
    call Assert( iStat == 0, &
      "Error reading GDD base temperature in irrigation lookup table" )
    write(UNIT=LU_LOG,FMT=*)  "   GDD base temperature ", &
        pConfig%IRRIGATION(iRecNum)%rGDD_BaseTemp

    call Chomp_tab ( sRecord, sItem )
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iRecNum)%rGDD_MaxTemp
    call Assert( iStat == 0, &
      "Error reading GDD max temperature in irrigation lookup table" )
    write(UNIT=LU_LOG,FMT=*)  "   GDD max temperature ", &
        pConfig%IRRIGATION(iRecNum)%rGDD_MaxTemp

    call Chomp_tab ( sRecord, sItem )
    read ( unit=sItem, fmt=*, iostat=iStat ) pConfig%IRRIGATION(iRecNum)%rMAD
    call Assert( iStat == 0, &
      "Error reading management allowable deficit (MAD) in irrigation lookup table" )
    write(UNIT=LU_LOG,FMT=*)  "   management allowable deficit (MAD) ", &
        pConfig%IRRIGATION(iRecNum)%rMAD

    call Chomp_tab ( sRecord, sItem )
    pConfig%IRRIGATION(iRecNum)%iBeginIrrigation = mmddyyyy2doy(sItem)
    write(UNIT=LU_LOG,FMT=*)  "   irrigation starts on or after day ", &
        pConfig%IRRIGATION(iRecNum)%iBeginIrrigation

    call Chomp_tab ( sRecord, sItem )
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

function rf_model_GetInterception( pConfig, iType, iDayOfYear ) result(rIntRate)
  !! Looks up the interception value for land-use type iType.
  ! [ ARGUMENTS ]
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  integer (kind=T_SHORT), intent(in) :: iType
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
      call et_hargreaves_initialize ( pGrd, pConfig%sTimeSeriesFilename)
  end select

  return
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
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  integer (kind=T_INT) :: iCol, iRow

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
  do iCol=1,pGrd%iNX  ! last index in a Fortan array should be the slowest changing
    do iRow=1,pGrd%iNY
      cel => pGrd%Cells(iRow,iCol)
      pIRRIGATION => pConfig%IRRIGATION(cel%iLandUseIndex)

      cel%rSM_PotentialET = cel%rSM_PotentialET &
          * calc_crop_coefficient(pIRRIGATION%rKc_Max, pIRRIGATION%rK0, &
               pIRRIGATION%rGDD_Kc_Max, pIRRIGATION%rGDD_Death, pIRRIGATION%rAlpha1, cel%rGDD)

      ! if the ground is still frozen, we're not going to consider ET to be
      ! possible.
      if(cel%rCFGI > rNEAR_ZERO)  cel%rSM_PotentialET = rZERO

    enddo
  enddo

#endif

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
    do iCol=1,pGrd%iNX
      do iRow=1,pGrd%iNY

        lMatch = lFALSE
        cel => pGrd%Cells(iRow,iCol)
        ! loop over all LAND USE types...
        do k=1,size(pConfig%LU,1)
          pLU => pConfig%LU(k)
          if ( pLU%iLandUseType == cel%iLandUse ) then
!            if( pLU%lCONSTANT_ROOT_ZONE_DEPTH ) then

              cel%rSoilWaterCap = cel%rSoilWaterCapInput * &
                pConfig%ROOTING_DEPTH(k,cel%iSoilGroup)

!            else
!              cel%rSoilWaterCap = cel%rSoilWaterCapInput * &
!                interpolate(pLU%rX_ROOT_ZONE,pLU%rY_ROOT_ZONE,cel%rSoilWaterCapInput)
!            end if
!            if(cel%iLandUse == 82) then
!             write(UNIT=LU_LOG,FMT=*)  "LU:", cel%iLandUse,"  SOIL:",cel%iSoilGroup,"  out:", &
!              "  in:",cel%rSoilWaterCapInput,"  product:",cel%rSoilWaterCap
!             write(UNIT=LU_LOG,FMT=*)  "   ",pLU%rX_ROOT_ZONE
!             write(UNIT=LU_LOG,FMT=*)  "   ",pLU%rY_ROOT_ZONE
!            endif
            lMatch=lTRUE
            exit
          end if
        end do
!      write(UNIT=LU_LOG,FMT=*)  iRow,iCol," table: ",pLU%iLandUseType," cell: ",cel%iLandUse
        if(.not. lMATCH) then
          write(UNIT=LU_LOG,FMT=*)  "iRow: ",iRow,"  iCol: ",iCol,"  cell LU: ", cel%iLandUse
          call Assert(lFALSE,&
          "Failed to match landuse grid with landuse table during soil moisture initialization")
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
  do iCol=1,pGrd%iNX
    do iRow=1,pGrd%iNY

      lMatch = lFALSE
      cel => pGrd%Cells(iRow,iCol)
      do k=1,size(pConfig%LU,1)
        pLU => pConfig%LU(k)
        if ( pLU%iLandUseType == cel%iLandUse ) then
          ! save index of matching landuse for ease of processing land use properties later
          cel%iLandUseIndex = k
          ! need to ensure that the soil type doesn't exceed
          ! the max number of soil types or we get a core dump
          call Assert(LOGICAL(INT(cel%iSoilGroup,kind=T_INT) &
            <= size(pConfig%MAX_RECHARGE,2),kind=T_LOGICAL), &
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

  return
end subroutine model_InitializeMaxInfil

!--------------------------------------------------------------------------

subroutine model_WriteGrids(pGrd, pConfig, sMonthName, iDay,iMonth, &
    iYear, iDayOfYear)
  !! Writes the monthly output arrays in the proper grid format
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  character (len=*),intent(in) :: sMonthName
  integer (kind=T_INT), intent(in) :: iDayOfYear, iMonth, iDay, iYear

  ! [ LOCALS ]
  real (kind=T_DBL) :: xmin,xmax,ymin,ymax
  character (len=256) sBufOut,sBufFuture,sBufSuffix,sDayText,sMonthText, &
                         sYearText

  sBufOut = "output"//pConfig%sSlash//trim(sMonthName)
  sBufFuture = "output"//pConfig%sSlash//"future"//pConfig%sSlash
  sBufSuffix = trim(pConfig%sOutputFileSuffix)

  write(sDayText,fmt="(a1,i2.2,a1,i2.2,a1,i4)") "_",iMonth,"_",iDay,"_",iYear
  write(sMonthText,fmt="(a1,i2.2,a1,i4)") "_",iMonth,"_",iYear
  write(sYearText,fmt="(a1,i4)") "_",iYear

  xmin = pGrd%rX0
  xmax = pGrd%rX1
  ymin = pGrd%rY0
  ymax = pGrd%rY1

  if ( pConfig%iOutputFormat == OUTPUT_ARC ) then
    if(MAXVAL(pGrd%Cells%rMSB)>0.1) then
      call grid_WriteArcGrid("MASS_BALANCE" // &
        trim(sDayText) // "." //trim(sBufSuffix), &
        xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rMSB )
    elseif ( trim(sMonthName) == "ANNUAL" ) then
!      call grid_WriteArcGrid( trim(sBufOut) // "_rch." // trim(sBufSuffix), &
!               xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rAnnualRecharge )
!      call grid_WriteArcGrid(trim(sBufOut) // "_pot_et." // trim(sBufSuffix), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rAnnualPotET )
!      call grid_WriteArcGrid(trim(sBufOut) // "_act_et." // trim(sBufSuffix), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rAnnualActET )
      call grid_WriteArcGrid(trim(sBufFuture) // "final_pct_sm" // &
        trim(sYearText) // "." //trim(sBufSuffix), &
          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rSoilMoisturePct )
      call grid_WriteArcGrid(trim(sBufFuture) // "final_snow_cover" // &
        trim(sYearText) // "." //trim(sBufSuffix), &
          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rSnowCover )
!      call grid_WriteArcGrid(trim(sBufOut) // "_soil_water_cap" // &
!        trim(sYearText) // "." //trim(sBufSuffix), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rSoilWaterCap )
    else if ( sMonthName(1:3) == 'day' ) then
!      call grid_WriteArcGrid("precip." // sMonthName(4:6), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rGrossPrecip )
!      call grid_WriteArcGrid("output/daily/ALT_recharge" // trim(sDayText) // &
!          "." // sBufSuffix, &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rDailyRecharge )

!        call grid_WriteArcGrid("output\\daily\\pot_et" // trim(sDayText) // &
!          "." // sBufSuffix, &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rSM_PotentialET )
!        call grid_WriteArcGrid("output\\act_et." // sMonthName(4:6), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rSM_ActualET )
!        call grid_WriteArcGrid("output\\net_infil." // sMonthName(4:6), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rNetInfil )
!        call grid_WriteArcGrid("output\\daily\\inflow" // trim(sDayText) // &
!          "." // sBufSuffix, &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rInFlow )
!        call grid_WriteArcGrid("output\\daily\\outflow" // trim(sDayText) // &
!          "." // sBufSuffix, &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rOutFlow )
!        call grid_WriteArcGrid("output\\daily\\soil_mois" // trim(sDayText) // &
!          "." // sBufSuffix, &
!            xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rSoilMoisture )
!        call grid_WriteArcGrid("output\\daily\\curve_num" // trim(sDayText) // &
!          "." // sBufSuffix, &
!            xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rAdjCN )
!        call grid_WriteArcGrid("output\\daily\\snow_cov" // trim(sDayText) // &
!          "." // sBufSuffix, &
!            xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rSnowCover )

    else

!      call grid_WriteArcGrid(trim(sBufOut) // "_rch"// trim(sMonthText) // &
!          "." // sBufSuffix, &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rMonthlyRecharge )
!      call grid_WriteArcGrid(trim(sBufOut) // "_pot_et." // trim(sBufSuffix), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rMonthlyPotET )
!      call grid_WriteArcGrid(trim(sBufOut) // "_act_et." // trim(sBufSuffix), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rMonthlyActET )
!      call grid_WriteArcGrid(trim(sBufOut) // "_inflow." // trim(sBufSuffix), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rMonthlyInFlow )
!      call grid_WriteArcGrid(trim(sBufOut) // "_outflow." // trim(sBufSuffix), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rMonthlyOutFlow )
!        call grid_WriteArcGrid(trim(sBufOut) // "_sm." // trim(sBufSuffix), &
!            xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rSoilMoisture )
    end if
  else if ( pConfig%iOutputFormat == OUTPUT_SURFER ) then
    if(MAXVAL(pGrd%Cells%rMSB)>0.1) then
      call grid_WriteSurferGrid("MASS_BALANCE" // &
        trim(sDayText) // "." //trim(sBufSuffix), &
        xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rMSB )
    elseif ( trim(sMonthName) == "ANNUAL" ) then
!      call grid_WriteSurferGrid(trim(sBufOut) // "_rch." // trim(sBufSuffix), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rAnnualRecharge )
!      call grid_WriteSurferGrid(trim(sBufOut) // "_pot_et." // trim(sBufSuffix), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rAnnualPotET )
!      call grid_WriteSurferGrid(trim(sBufOut) // "_act_et." // trim(sBufSuffix), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rAnnualActET )
      call grid_WriteSurferGrid(trim(sBufFuture) // "final_pct_sm" // &
        trim(sYearText) // "." //trim(sBufSuffix), &
          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rSoilMoisturePct )
      call grid_WriteSurferGrid(trim(sBufFuture) // "final_snow_cover" // &
        trim(sYearText) // "." //trim(sBufSuffix), &
          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rSnowCover )
!      call grid_WriteSurferGrid(trim(sBufOut) // "_soil_water_cap" // &
!        trim(sYearText) // "." //trim(sBufSuffix), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rSoilWaterCap )

    else if ( sMonthName(1:3) == 'day' ) then

!      call grid_WriteSurferGrid("output\\recharge." // sMonthName(4:6), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rDailyRecharge )

!        call grid_WriteSurferGrid("output\\daily\\pot_et." // sMonthName(4:6), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rSM_PotentialET )
!        call grid_WriteSurferGrid("output\\act_et." // sMonthName(4:6), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rSM_ActualET )
!        call grid_WriteSurferGrid("output\\net_infil." // sMonthName(4:6), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rNetInfil )
!        call grid_WriteSurferGrid("output\\daily\\inflow." // sMonthName(4:6), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rInFlow )
!        call grid_WriteSurferGrid("output\\daily\\outflow." // sMonthName(4:6), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rOutFlow )

!        call grid_WriteSurferGrid("output\\daily\\soil_mois." // sMonthName(4:6), &
!            xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rSoilMoisture )
!        call grid_WriteSurferGrid("output\\daily\\curve_num." // sMonthName(4:6), &
!            xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rAdjCN )
!        call grid_WriteSurferGrid("output\\daily\\snow_cov." // sMonthName(4:6), &
!            xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rSnowCover )

    else
!      call grid_WriteSurferGrid(trim(sBufOut) // "_rch." // trim(sBufSuffix), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rMonthlyRecharge )
!      call grid_WriteSurferGrid(trim(sBufOut) // "_pot_et." // trim(sBufSuffix), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rMonthlyPotET )
!      call grid_WriteSurferGrid(trim(sBufOut) // "_act_et." // trim(sBufSuffix), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rMonthlyActET )
!      call grid_WriteSurferGrid(trim(sBufOut) // "_inflow." // trim(sBufSuffix), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rMonthlyInFlow )
!      call grid_WriteSurferGrid(trim(sBufOut) // "_outflow." // trim(sBufSuffix), &
!          xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rMonthlyOutFlow )
!        call grid_WriteSurferGrid(trim(sBufOut) // "_sm." // trim(sBufSuffix), &
!            xmin,xmax,ymin,ymax,pGrd%Cells(:,:)%rSoilMoisture )
    end if
  else
    call Assert( .false._T_LOGICAL, "Illegal output format specified" )
  end if

  return
end subroutine model_WriteGrids

#ifdef NETCDF_SUPPORT

subroutine model_write_NetCDF_attributes(pConfig, pGrd)
  ! this code block initializes NetCDF output files for any
  ! valid SWB variable, as specified in the OUTPUT_OPTIONS
  ! input block

    type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
    type (T_MODEL_CONFIGURATION), pointer :: pConfig      ! pointer to data structure that contains
                                                        ! model options, flags, and other settings
    ! [ LOCALS ]
    integer (kind=T_INT) :: k
    type (T_NETCDF_FILE), pointer :: pNC

    do k=1,iNUM_VARIABLES

      if(STAT_INFO(k)%iNetCDFOutput > iNONE ) then

        pNC => pConfig%NETCDF_FILE(k,iNC_OUTPUT)
        pNC%sVarName = TRIM(STAT_INFO(k)%sVARIABLE_NAME)
        pNC%sUnits =  TRIM(STAT_INFO(k)%sUNITS)
        pNC%rScaleFactor = STAT_INFO(k)%rNC_MultFactor
        pNC%rAddOffset = STAT_INFO(k)%rNC_AddOffset
        pNC%iNCID = netcdf_create(TRIM(pNC%sVarName)//".nc")
        call netcdf_write_attributes(k, iNC_OUTPUT, pConfig, pGrd)
        write(unit=LU_LOG,FMT="('Wrote attributes to NetCDF file--')")
        write(unit=LU_LOG,FMT="('      k: ',i4,'  (',a,')')") k,TRIM(pNC%sVarName)
        write(unit=LU_LOG,FMT="('      NDIC: ',i4)") pNC%iNCID
      end if

    end do

    return

end subroutine model_write_NetCDF_attributes

#endif

! read a single line from the time-series file and return a pointer to the values
subroutine model_ReadTimeSeriesFile(pTS)

  type (T_TIME_SERIES_FILE), pointer :: pTS

  ! [ LOCALS ]
  character(len=256) :: sBuf
  integer (kind=T_INT) :: iStat

  do

    read ( unit=LU_TS, fmt="(a256)", iostat=iStat ) sBuf
    if ( iStat<0 ) then
      pTS%lEOF = lTRUE
      exit ! END OF FILE
    end if
    call Assert ( iStat == 0, &
              "Cannot read record from time-series file", &
              TRIM(__FILE__),__LINE__)
    if ( sBuf(1:1) == '#' ) cycle      ! Ignore comment statements
    call CleanUpCsv ( sBuf )
    read ( unit=sBuf, fmt=*, iostat=iStat ) pTS%iMonth, pTS%iDay, &
       pTS%iYear, pTS%rAvgT, pTS%rPrecip, pTS%rRH, pTS%rMaxT, pTS%rMinT, &
       pTS%rWindSpd, pTS%rMinRH, pTS%rSunPct
    if (iStat/=0) then
      write(UNIT=LU_LOG,FMT=*) "Skipping: ",trim(sBuf)
      write(UNIT=LU_LOG,FMT=*)
      cycle
    end if

    if(pTS%rMaxT< -100 .or. pTS%rMinT < -100 .or. pTS%rMaxT < pTS%rMinT &
       .or. pTS%rPrecip < 0.) then
      write(UNIT=LU_LOG,fmt=*) &
        "Missing or corrupt data in climate file"
      call Assert(lFALSE, &
        "Input: "//TRIM(sBuf),TRIM(__FILE__),__LINE__)
    end if

    exit

  end do

  return

end subroutine model_ReadTimeSeriesFile

end module model
