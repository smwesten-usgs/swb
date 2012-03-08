!> @file
!> @brief Contains a single module, @ref control, which reads program options from
!> an SWB control file and sets program configuration flags

!> @brief Reads program options from an SWB control file and sets program configuration flags.
module control

  use types
  use model
  use swb_grid
  use swb_stats

  contains

!> @brief  Reads model control file and initializes model configuration
!>         parameters and flags.
!>
!> Reads model control file and initializes model configuration
!>         parameters and flags. Multiple calls to model_Main are accomodated
!>         for multiple-year model runs.
!>
!> @param[in]  sControlFile  Name of the control file to use for the current simulation.
!> @retval NONE
subroutine control_setModelOptions(sControlFile)

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
  integer (kind=T_INT) :: iCurrentLineNumber
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
  open(LU_LOG, file=TRIM(ADJUSTL(sBuf)),iostat=iStat, &
      status = "REPLACE")
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
  pConfig%iConfigureLanduse = CONFIG_NONE
  pConfig%iConfigureET = CONFIG_ET_NONE
  pConfig%iConfigureRunoff = CONFIG_RUNOFF_CURVE_NUMBER
  pConfig%iConfigureRunoffMode = CONFIG_RUNOFF_DOWNHILL
  pConfig%iConfigurePrecip = CONFIG_PRECIP_SINGLE_STATION
  pConfig%iConfigureTemperature = CONFIG_TEMPERATURE_SINGLE_STATION
  pConfig%iConfigureSMCapacity = CONFIG_SM_CAPACITY_CALCULATE
  pConfig%iConfigureSnow = CONFIG_SNOW_ORIGINAL_SWB
  pConfig%iConfigureInitialAbstraction = CONFIG_SM_INIT_ABSTRACTION_TR55
  pConfig%sOutputFileSuffix = "grd"
  pConfig%iHeaderPrintInterval = 7
  pConfig%lWriteToScreen = lTRUE
  pConfig%lReportDaily = lTRUE
  iCurrentLineNumber = 0

#ifdef STREAM_INTERACTIONS
  pConfig%rStreamMaxInflow = rBIGVAL
  pConfig%rStreamMaxCapture = rBIGVAL
#endif

  iVarNum = -99999

  ! Attempt to open CONTROL file
  open ( LU_CONTROL, file=sControlFile, status="OLD", iostat=iStat )
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
    iCurrentLineNumber = iCurrentLineNumber + 1
    if ( sRecord(1:1) == "#" ) cycle     ! ignore comment lines
    if (len_trim(sRecord) == 0) cycle    ! ignore blank lines
    call Assert( iStat == 0, &
        "Terminating due to read error" )
    write (UNIT=LU_LOG,FMT=*) ">> " // trim(sRecord)
    flush(UNIT=LU_LOG)

    call Chomp( sRecord, sItem )
    call uppercase(sItem)

    if ( str_compare(sItem,"GRID") ) then
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
      call assert(pGrd%rGridCellSize == rGridCellSize, "Grid cell size entered in the " &
        //"control file ("//trim(real2char(rGridCellSize))//")~does not" &
        //" match calculated grid cell size ("//trim(real2char(pGrd%rGridCellSize)) &
        //"). ~Check the control file.", &
        trim(__FILE__), __LINE__)
      pGrd%iNumGridCells = iNX * iNY
      write(UNIT=LU_LOG,FMT="('    total number of gridcells: ',i8)") pGrd%iNumGridCells
      flush(UNIT=LU_LOG)

#ifdef DEBUG_PRINT
    elseif ( str_compare(sItem,"MEM_TEST") ) then

      rX0 = 10000.
      rY0 = 10000.
      rGridCellSize = 30.0

      do iNX = 10,10000,10
        iNY = iNX
        pGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, T_CELL_GRID)
        call grid_Destroy( pGrd )
      end do
#endif

    elseif ( str_compare(sItem,"GROWING_SEASON") ) then
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
      if (trim(sOption) == "TRUE" .or. trim(sOption) == "T") then
        pConfig%lNorthernHemisphere = lTRUE
      else
        pConfig%lNorthernHemisphere = lFALSE
      end if
      write(UNIT=LU_LOG,FMT=*) "Northern Hemisphere = ",pConfig%lNorthernHemisphere
      flush(UNIT=LU_LOG)

    else if ( str_compare(sItem,"PRECIPITATION") ) then
      write(UNIT=LU_LOG,FMT=*) "Configuring precipitation data input"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      if ( str_compare(sOption,"SINGLE_STATION") ) then
        pConfig%iConfigurePrecip = CONFIG_PRECIP_SINGLE_STATION
        write(UNIT=LU_LOG,FMT=*) "  Precip data will be read for a single station"
        pConfig%lGriddedData = lFALSE
      else
        if ( str_compare(sOption,"ARC_GRID") ) then
          pConfig%iConfigurePrecip = CONFIG_PRECIP_ARC_GRID
          write(UNIT=LU_LOG,FMT=*) "Precip data will be read as a series of ARC grids"
          pConfig%sPrecipFilePrefix = sArgument
          write(UNIT=LU_LOG,FMT=*)  "  grid file prefix for PRECIP data: ", &
          TRIM(pConfig%sPrecipFilePrefix)
          pConfig%lGriddedData = lTRUE
        else if ( str_compare(sOption,"SURFER") ) then
          pConfig%iConfigurePrecip = CONFIG_PRECIP_SURFER_GRID
          write(UNIT=LU_LOG,FMT=*) "Precip data will be read as a series of SURFER grids"
          write(UNIT=LU_LOG,FMT=*)  "  grid file prefix for PRECIP data: ", &
          TRIM(pConfig%sPrecipFilePrefix)
          pConfig%lGriddedData = lTRUE
#ifdef NETCDF_SUPPORT
        else if( str_compare(sOption,"NETCDF") ) then
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
          pConfig%lGriddedData = lTRUE
#endif
        else
          call Assert( .false._T_LOGICAL, "Illegal precipitation input format specified" )
        end if
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

    else if ( sItem == "IGNORE_MISSING_CLIMATE_DATA" ) then
      write(UNIT=LU_LOG,FMT=*) &
        "SWB will ignore missing climate data. Missing precip will be set to zero."
      write(UNIT=LU_LOG,FMT=*) &
        "  Missing temperature data will be filled with the values from the previous day."
      pConfig%lHaltIfMissingClimateData = lFALSE
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
      open(iLU_SSF, file=TRIM(sBuf),status='REPLACE', iostat=iStat)
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
      pConfig%rSnowFall_SWE_Corr_Factor = rValue
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

    else if ( sItem == "IRRIGATION_LOOKUP_TABLE" ) then
      write(UNIT=LU_LOG,FMT=*) "Reading irrigation lookup table"
      call Chomp ( sRecord, pConfig%sIrrigationLookupFilename )
      call assert(len_trim(pConfig%sIrrigationLookupFilename) > 0, &
        "No irrigation lookup table specified", trim(__FILE__), __LINE__ )
      call assert(len_trim(pConfig%sLandUseLookupFilename) > 0, &
         "The irrigation module cannot be activated before specifying " &
         //"a land use lookup table", trim(__FILE__), __LINE__ )
      call model_ReadIrrigationLookupTable( pConfig )
      flush(UNIT=LU_LOG)

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
      write(UNIT=LU_LOG,FMT=*)  "Initial soil moisture grid MINIMUM VALUE:", &
         minval(pGrd%Cells%rSoilMoisturePct)
      write(UNIT=LU_LOG,FMT=*)  "Initial soil moisture grid MAXIMUM VALUE:", &
         maxval(pGrd%Cells%rSoilMoisturePct)
      call Assert( LOGICAL( minval(pGrd%Cells%rSoilMoisturePct) >= 0, &
         kind=T_LOGICAL), "Negative values are not allowed in the" &
         //" initial soil moisture grid.")

      flush(UNIT=LU_LOG)

#ifdef STREAM_INTERACTIONS

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

#endif

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

      call assert(STAT_INFO(iVarNum)%lActive, "The variable "//squote(STAT_INFO(i)%sVARIABLE_NAME) &
        //" is not accessible given the current compilation options.~" &
        //"GDD (growing-degree days), for example, is only available if the~" &
        //"code has been compiled with irrigation module support.",trim(__FILE__), __LINE__)

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
        open ( LU_TS, file=pConfig%sTimeSeriesFilename, status="OLD", iostat=iStat )
        call Assert ( iStat == 0, &
         "Can't open time-series data file" )
        call model_ReadTimeSeriesFile(pConfig, pTSt)
        pConfig%iStartYear = pTSt%iYear
        pConfig%iStartJulianDay = julian_day ( pConfig%iStartYear, 1, 1)
        ! current julian day will be incremented in model_Main
        pConfig%iCurrentJulianDay = pConfig%iStartJulianDay - 1
        close( unit=LU_TS )
      end if

      pConfig%lGriddedData = lFALSE
      ! actual call to "model_Main" subroutine
      call model_Main( pGrd, pConfig, pGraph)

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
        write(UNIT=LU_LOG,FMT="(a,i4.4)") "Calling model_Main." &
          // "  Current year = ",i
        flush(UNIT=LU_LOG)
        ! actual call to "model_Main" subroutine
        call model_Main( pGrd, pConfig, pGraph)
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
      exit

    else
      call Assert( lFALSE, "Illegal directive: "//squote(sItem) &
        // "~found on line number "//trim(int2char(iCurrentLineNumber)) &
        //" of control file "//squote(sControlfile) )
    end if

  end do CTL_READ  ! end of control file read loop

  DEALLOCATE(pConfig, STAT=iStat)
  call Assert( iStat == 0, &
     "Could not deallocate memory for model control data structure")

  DEALLOCATE(pGraph, STAT=iStat)
  call Assert( iStat == 0, &
     "Could not deallocate memory for plot control data structure")

  if(associated(pTst) ) then
    DEALLOCATE(pTSt, STAT=iStat)
    call Assert( iStat == 0, &
       "Could not deallocate memory for time-series data structure", &
       TRIM(__FILE__),__LINE__)
  endif

  close(UNIT=LU_CONTROL)
  return

end subroutine control_setModelOptions

end module control
