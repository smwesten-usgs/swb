!> @file
!> Contains a single module, @ref control, which reads program options from
!> an SWB control file and sets program configuration flags

!> Reads program options from an SWB control file and sets program configuration flags.
module control

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use types
  use model
  use swb_grid
  use stats
  use data_factory
  use datetime

  implicit none

  contains

!>  Reads model control file and initializes model configuration
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
  type (T_GENERAL_GRID),pointer :: input_grd         ! Temporary grid for I/O
  type (T_GENERAL_GRID),pointer :: pGrd              ! Grid of model cells
!  type (T_GENERAL_GRID),pointer :: pLandUseGrid      ! Landuse input grid
!  type (T_GENERAL_GRID),pointer :: pSoilGroupGrid    ! Soil HSG input grid
!  type (T_GENERAL_GRID),pointer :: pFlowDirGrid      ! Flow direction input grid
!  type (T_GENERAL_GRID),pointer :: pSoilAWCGrid      ! Available Water Capacity input grid
  real (kind=c_float), dimension(:,:), pointer :: pArray_sgl


  type (T_GRAPH_CONFIGURATION), dimension(:),pointer :: pGraph
                                            ! pointer to data structure that
                                                   ! holds parameters for creating
                                                   ! DISLIN plots

  type (T_TIME_SERIES_FILE), pointer :: pTSt

  type (T_SSF_FILES), dimension(:), pointer :: pSSF   ! pointer to struct containing SSF file info
  logical (kind=c_bool), save :: lNO_SSF_FILES = lTRUE
  logical (kind=c_bool) :: lOpened
  integer (kind=c_int) :: iOldSize, iNewSize
  integer (kind=c_int) :: iRowNum, iColNum
  logical (kind=c_bool) :: lMatch

  character (len=256) :: sRecord                  ! Input file text buffer
  character (len=256) :: sItem                    ! Key word read from sRecord
  character (len=256) :: sOption                  ! Key word read from sRecord
  character (len=256) :: sArgument                ! Key word read from sRecord
  character (len=256) :: sArgument2               ! Key word read from sRecord
  character (len=256) :: sArgument3               ! Key word read from sRecord
  character (len=256) :: sArgument4               ! Key word read from sRecord
  character (len=256) :: sArgument5               ! Key word read from sRecord
  character (len=256) :: sDateStr, sDateStrPretty ! hold date and time of initiation of run
  character(len=256) :: sBuf
  integer (kind=c_int) :: iNX                     ! Number of cells in the x-direction
  integer (kind=c_int) :: iNY                     ! Number of cells in the y-direction
  integer (kind=c_int) :: iValue                  ! Temporary value for 'CONSTANT'
  integer (kind=c_int) :: iRetVal
  integer (kind=c_int) :: iStat                   ! For 'iostat=' checks
  integer (kind=c_int) :: i,iVarNum,iTimeFrame    ! loop counters
  integer (kind=c_int) :: idx                     ! index counter for STREAM_CAPTURE routines
  integer (kind=c_int) :: iLU_SSF = 300           ! last LU for SSF file
  integer (kind=c_int) :: iCurrentLineNumber
  real (kind=c_double), dimension(0:1) :: rX_LL, rY_LL ! Model grid extent (lower-left)
  real (kind=c_double) :: rX0, rY0                     ! Model grid extent (lower-left)
  real (kind=c_double) :: rX1, rY1                     ! Model grid extent (upper-right)
  real (kind=c_double) :: rXp, rYp                     ! Coordinates at a point within a grid
  real (kind=c_double) :: rTemp
  real (kind=c_double) :: rGridCellSize             ! Model grid cell size
  real (kind=c_float) :: rValue                    ! Temporary value for 'CONSTANT'
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


  ALLOCATE (pTSt, STAT=iStat)
  call Assert( iStat == 0, &
     "Could not allocate memory for time-series data structure")

  write (UNIT=LU_LOG,FMT=*) "Running ",trim(sControlFile)

  ! Initialize the module variables - set default program options

  DAT(PRECIP_DATA)%sVariableName_z = "prcp"
  DAT(TMIN_DATA)%sVariableName_z = "tmin"
  DAT(TMAX_DATA)%sVariableName_z = "tmax"

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
  pConfig%iConfigureInitialAbstraction = CONFIG_SM_INIT_ABSTRACTION_TR55
  pConfig%sOutputFileSuffix = "asc"
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

      call Chomp ( sRecord, sItem )
      read ( unit=sItem, fmt=*, iostat=iStat ) rTemp
      call Assert ( iStat == 0, "Failed to read X1 (or grid cell size)" )

      !> if there is no additional information in this line, we assume that
      !> the user wants to skip defining the upper right coordinates
      if( len_trim(sRecord) == 0 ) then

        rGridCellSize = rTemp
        pGrd => grid_Create(iNX, iNY, rX0, rY0, rGridCellSize, GRID_DATATYPE_ALL)

      else

        rX1 = rTemp

        call Chomp ( sRecord, sItem )
        read ( unit=sItem, fmt=*, iostat=iStat ) rY1
        call Assert ( iStat == 0, "Failed to read Y1" )

        call Chomp ( sRecord, sItem )
        read ( unit=sItem, fmt=*, iostat=iStat ) rGridCellSize
        call Assert ( iStat == 0, "Failed to read grid cell size" )
        ! Now, build the grid

        pGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, GRID_DATATYPE_ALL)
        call assert(pGrd%rGridCellSize == rGridCellSize, "Grid cell size entered in the " &
          //"control file ("//trim(asCharacter(rGridCellSize))//")~does not" &
          //" match calculated grid cell size ("//trim(asCharacter(pGrd%rGridCellSize)) &
          //"). ~Check the control file.", &
          trim(__FILE__), __LINE__)

        write(UNIT=LU_LOG,FMT="('    X1: ',f14.4)") rX1
        write(UNIT=LU_LOG,FMT="('    Y1: ',f14.4)") rY1

      endif

      write(UNIT=LU_LOG,FMT="('    grid cell size: ',f8.1)") rGridCellSize

      pConfig%iNumGridCells = iNX * iNY
      pConfig%iNX = pGrd%iNX; pConfig%iNY = pGrd%iNY
      pConfig%rX0 = pGrd%rX0; pConfig%rX1 = pGrd%rX1
      pConfig%rY0 = pGrd%rY0; pConfig%rY1 = pGrd%rY1
      rX_LL(0) = pConfig%rX0; rX_LL(1) = pConfig%rX1
      rY_LL(0) = pConfig%rY0; rY_LL(1) = pConfig%rY1
      pConfig%rGridCellSize = pGrd%rGridCellSize
      pGrd%sFilename = "[none: base grid]"
      pGrd%sPROJ4_string = ""
      DAT(:)%sSourcePROJ4_string = ""

    else if (sItem == "BASE_PROJECTION_DEFINITION") then
      pConfig%sBase_PROJ4 = trim(sRecord)
      call Assert(associated(pGrd), "The project grid must be specified " &
        //"before the base grid projection information can be specified.")
      pGrd%sPROJ4_string = trim(sRecord)
      DAT(:)%sSourcePROJ4_string=trim(sRecord)

      iRetVal = pj_init_and_transform(pGrd%sPROJ4_string//C_NULL_CHAR, &
                trim(sPROJ4_LatLon)//C_NULL_CHAR, &
                2_c_long, rX_LL, rY_LL)
      call grid_CheckForPROJ4Error(iRetVal, pGrd%sPROJ4_string, sPROJ4_LatLon)

      pConfig%rX0_LL = rX_LL(0); pConfig%rX1_LL = rX_LL(1)
      pConfig%rY0_LL = rY_LL(0); pConfig%rY1_LL = rY_LL(1)
      pConfig%rSouthernLatitude = rY_LL(0)
      pConfig%rNorthernLatitude = rY_LL(1)

      call echolog("Northern latitude has been set to " &
        //trim(asCharacter(pConfig%rNorthernLatitude*360./dpTWOPI)) &
        //" on the basis of the specified grid extents")
      call echolog("Southern latitude has been set to " &
        //trim(asCharacter(pConfig%rSouthernLatitude*360./dpTWOPI)) &
        //" on the basis of the specified grid extents")

#ifdef DEBUG_PRINT
    elseif ( str_compare(sItem,"MEM_TEST") ) then

      rX0 = 10000.
      rY0 = 10000.
      rGridCellSize = 30.0

      do iNX = 10,10000,10
        iNY = iNX
        pGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, DATATYPE_CELL_GRID)
        call grid_Destroy( pGrd )
      end do
#endif

    elseif ( str_compare(sItem,"GROWING_SEASON") ) then
      write(UNIT=LU_LOG,FMT=*) "Reading growing season"
      call Chomp ( sRecord, sArgument )
      read ( sArgument, fmt=*, iostat=iStat ) rValue
      pConfig%iDayOfFirstFrost = INT(rValue,kind=c_int)
      call Assert ( iStat == 0, "Could not read start of growing season" )
      call Chomp ( sRecord, sArgument )
      read ( sArgument, fmt=*, iostat=iStat ) rValue
      pConfig%iDayOfLastFrost = INT(rValue,kind=c_int)
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
        write(UNIT=LU_LOG,FMT=*) "  Precip data will be read for a single station"
        pConfig%lGriddedData = lFALSE
        pConfig%iConfigurePrecip = CONFIG_PRECIP_SINGLE_STATION
        call DAT(PRECIP_DATA)%initialize(sDescription=trim(sItem), &
           rConstant=0.0 )

      elseif(str_compare(sOption,"ARC_GRID") ) then
        pConfig%iConfigurePrecip = CONFIG_PRECIP_ARC_GRID
        call DAT(PRECIP_DATA)%initialize(sDescription=trim(sItem), &
          sFileType=trim(sOption), &
          sFilenameTemplate=trim(sArgument), &
          iDataType=DATATYPE_REAL )
          pConfig%lGriddedData = lTRUE

      elseif(str_compare(sOption,"SURFER") ) then
        pConfig%iConfigurePrecip = CONFIG_PRECIP_SURFER_GRID
        call DAT(PRECIP_DATA)%initialize(sDescription=trim(sItem), &
          sFileType=trim(sOption), &
          sFilenameTemplate=trim(sArgument), &
          iDataType=DATATYPE_REAL )
          pConfig%lGriddedData = lTRUE

      else if( str_compare(sOption,"NETCDF") ) then
        pConfig%iConfigurePrecip = CONFIG_PRECIP_NETCDF
        ! initialize DAT object for precip data
        call DAT(PRECIP_DATA)%initialize_netcdf( &
          sDescription=trim(sItem), &
            sFilenameTemplate = trim(sArgument), &
            iDataType=DATATYPE_REAL, &
            pGrdBase=pGrd)
          pConfig%lGriddedData = lTRUE
      else
        call Assert( lFALSE , "Illegal precipitation input format specified" )
      end if
      flush(UNIT=LU_LOG)

    else if ( str_compare(sItem,"PRECIPITATION_SCALE") ) then
      call Chomp ( sRecord, sArgument )
      call DAT(PRECIP_DATA)%set_scale(asReal(sArgument))

    else if ( str_compare(sItem,"PRECIPITATION_OFFSET") ) then
      call Chomp ( sRecord, sArgument )
      call DAT(PRECIP_DATA)%set_offset(asReal(sArgument))

    else if ( str_compare(sItem,"PRECIPITATION_CONVERSION_FACTOR") ) then
      call Chomp ( sRecord, sArgument )
      call DAT(PRECIP_DATA)%set_conversion_factor(asReal(sArgument))

    else if ( str_compare(sItem,"NETCDF_PRECIP_X_VAR") ) then
      call Chomp ( sRecord, sArgument )
      DAT(PRECIP_DATA)%sVariableName_x = trim(sArgument)

    else if ( str_compare(sItem,"NETCDF_PRECIP_Y_VAR") ) then
      call Chomp ( sRecord, sArgument )
      DAT(PRECIP_DATA)%sVariableName_y = trim(sArgument)

    else if ( str_compare(sItem,"NETCDF_PRECIP_Z_VAR") ) then
      call Chomp ( sRecord, sArgument )
      DAT(PRECIP_DATA)%sVariableName_z = trim(sArgument)

    else if ( str_compare(sItem,"NETCDF_PRECIP_TIME_VAR") ) then
      call Chomp ( sRecord, sArgument )
      DAT(PRECIP_DATA)%sVariableName_time = trim(sArgument)

    else if ( str_compare(sItem,"NETCDF_PRECIP_VARIABLE_ORDER") ) then
      call Chomp ( sRecord, sArgument )
      call DAT(PRECIP_DATA)%set_variable_order(lowercase_fn(trim(sArgument)))

    else if ( str_compare(sItem,"NETCDF_PRECIP_FLIP_VERTICAL") ) then
      call DAT(PRECIP_DATA)%set_grid_flip_vertical()

    else if ( str_compare(sItem,"NETCDF_PRECIP_FLIP_HORIZONTAL") ) then
      call DAT(PRECIP_DATA)%set_grid_flip_horizontal()

    elseif (str_compare(sItem, "NETCDF_PRECIP_MAKE_LOCAL_ARCHIVE") ) then
      call DAT(PRECIP_DATA)%set_make_local_archive(lTRUE)

    else if (sItem == "PRECIPITATION_GRID_PROJECTION_DEFINITION") then
      call DAT(PRECIP_DATA)%set_PROJ4( trim(sRecord) )

    elseif (sItem == "PRECIPITATION_MINIMUM_ALLOWED_VALUE") then
      call Chomp ( sRecord, sArgument )
      DAT(PRECIP_DATA)%rMinAllowedValue = asReal(sArgument)

    elseif (sItem == "PRECIPITATION_MAXIMUM_ALLOWED_VALUE") then
      call Chomp ( sRecord, sArgument )
      DAT(PRECIP_DATA)%rMaxAllowedValue = asReal(sArgument)

    elseif (sItem == "PRECIPITATION_MISSING_VALUES_CODE") then
      call Chomp ( sRecord, sArgument )
      DAT(PRECIP_DATA)%rMissingValuesCode = asReal(sArgument)

    elseif (sItem == "PRECIPITATION_MISSING_VALUES_OPERATOR") then
      call Chomp ( sRecord, sArgument )
      DAT(PRECIP_DATA)%sMissingValuesOperator = trim(sArgument)

    elseif (sItem == "PRECIPITATION_MISSING_VALUES_ACTION") then
      call Chomp ( sRecord, sArgument )
      if (sArgument == "ZERO") then
        DAT(PRECIP_DATA)%iMissingValuesAction = MISSING_VALUES_ZERO_OUT
      elseif (sArgument == "MEAN" ) then
        DAT(PRECIP_DATA)%iMissingValuesAction = MISSING_VALUES_REPLACE_WITH_MEAN
      else
        call assert(lFALSE, "Unknown missing value action supplied for" &
          //" precipitation data: "//dquote(sArgument) )
      endif

    else if ( sItem == "TEMPERATURE" ) then
      write(UNIT=LU_LOG,FMT=*) "Configuring temperature data input"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      if ( trim(sOption) == "SINGLE_STATION" ) then
        pConfig%iConfigureTemperature = CONFIG_TEMPERATURE_SINGLE_STATION
        write(UNIT=LU_LOG,FMT=*) "  Temperature data will be read for a single station"
        call DAT(TMIN_DATA)%initialize(sDescription=trim(sItem), &
           rConstant=65.0 )
        call DAT(TMAX_DATA)%initialize(sDescription=trim(sItem), &
           rConstant=65.0 )
      else

        if ( trim(sOption) == "ARC_GRID" ) then

          pConfig%iConfigureTemperature = CONFIG_TEMPERATURE_ARC_GRID

          call DAT(TMAX_DATA)%initialize(sDescription=trim(sItem), &
            sFileType=trim(sOption), &
            sFilenameTemplate=trim(sArgument), &
            iDataType=DATATYPE_REAL )

          call Chomp ( sRecord, sArgument )

          call DAT(TMIN_DATA)%initialize(sDescription=trim(sItem), &
            sFileType=trim(sOption), &
            sFilenameTemplate=trim(sArgument), &
            iDataType=DATATYPE_REAL )

          pConfig%lGriddedData = lTRUE

        elseif ( trim(sOption) == "SURFER" ) then

          pConfig%iConfigureTemperature = CONFIG_TEMPERATURE_SURFER_GRID

          call DAT(TMAX_DATA)%initialize(sDescription=trim(sItem), &
            sFileType=trim(sOption), &
            sFilenameTemplate=trim(sArgument), &
            iDataType=DATATYPE_REAL )

          call Chomp ( sRecord, sArgument )

          call DAT(TMIN_DATA)%initialize(sDescription=trim(sItem), &
            sFileType=trim(sOption), &
            sFilenameTemplate=trim(sArgument), &
            iDataType=DATATYPE_REAL )

          pConfig%lGriddedData = lTRUE

        else if( trim(sOption) == "NETCDF" ) then

          pConfig%iConfigureTemperature = CONFIG_TEMPERATURE_NETCDF

          call DAT(TMAX_DATA)%initialize_netcdf( &
            sDescription=trim(sItem), &
            sFilenameTemplate = trim(sArgument), &
            iDataType=DATATYPE_REAL, &
            pGrdBase=pGrd )

          ! read in the NetCDF variable that corresponds to TMIN
          call Chomp ( sRecord, sArgument )

          call DAT(TMIN_DATA)%initialize_netcdf( &
            sDescription=trim(sItem), &
            sFilenameTemplate = trim(sArgument), &
            iDataType=DATATYPE_REAL, &
            pGrdBase=pGrd)

        else
          call Assert( lFALSE, "Illegal temperature input format specified", &
            TRIM(__FILE__),__LINE__)
        endif
      end if
      flush(UNIT=LU_LOG)

    else if (sItem == "TEMPERATURE_GRID_PROJECTION_DEFINITION") then
      call DAT(TMAX_DATA)%set_PROJ4( trim(sRecord) )
      call DAT(TMIN_DATA)%set_PROJ4( trim(sRecord) )

    else if ( str_compare(sItem,"NETCDF_TMAX_X_VAR") ) then
      call Chomp ( sRecord, sArgument )
      DAT(TMAX_DATA)%sVariableName_x = trim(sArgument)

    else if ( str_compare(sItem,"NETCDF_TMAX_Y_VAR") ) then
      call Chomp ( sRecord, sArgument )
      DAT(TMAX_DATA)%sVariableName_y = trim(sArgument)

    else if ( str_compare(sItem,"NETCDF_TMAX_Z_VAR") ) then
      call Chomp ( sRecord, sArgument )
      DAT(TMAX_DATA)%sVariableName_z = trim(sArgument)

    else if ( str_compare(sItem,"NETCDF_TMAX_TIME_VAR") ) then
      call Chomp ( sRecord, sArgument )
      DAT(TMAX_DATA)%sVariableName_time = trim(sArgument)

    else if ( str_compare(sItem,"NETCDF_TMIN_X_VAR") ) then
      call Chomp ( sRecord, sArgument )
      DAT(TMIN_DATA)%sVariableName_x = trim(sArgument)

    else if ( str_compare(sItem,"NETCDF_TMIN_Y_VAR") ) then
      call Chomp ( sRecord, sArgument )
      DAT(TMIN_DATA)%sVariableName_y = trim(sArgument)

    else if ( str_compare(sItem,"NETCDF_TMIN_Z_VAR") ) then
      call Chomp ( sRecord, sArgument )
      DAT(TMIN_DATA)%sVariableName_z = trim(sArgument)

    else if ( str_compare(sItem,"NETCDF_TMIN_TIME_VAR") ) then
      call Chomp ( sRecord, sArgument )
      DAT(TMIN_DATA)%sVariableName_time = trim(sArgument)

    else if ( str_compare(sItem,"NETCDF_TMIN_VARIABLE_ORDER") ) then
      call Chomp ( sRecord, sArgument )
      call DAT(TMIN_DATA)%set_variable_order(lowercase_fn(trim(sArgument)))

    else if ( str_compare(sItem,"NETCDF_TMAX_VARIABLE_ORDER") ) then
      call Chomp ( sRecord, sArgument )
      call DAT(TMAX_DATA)%set_variable_order(lowercase_fn(trim(sArgument)))

    else if ( str_compare(sItem,"NETCDF_TMAX_FLIP_VERTICAL") ) then
      call DAT(TMAX_DATA)%set_grid_flip_vertical()

    else if ( str_compare(sItem,"NETCDF_TMAX_FLIP_HORIZONTAL") ) then
      call DAT(TMAX_DATA)%set_grid_flip_horizontal()

    else if ( str_compare(sItem,"NETCDF_TMIN_FLIP_VERTICAL") ) then
      call DAT(TMIN_DATA)%set_grid_flip_vertical()

    else if ( str_compare(sItem,"NETCDF_TMIN_FLIP_HORIZONTAL") ) then
      call DAT(TMIN_DATA)%set_grid_flip_horizontal()

    elseif (str_compare(sItem, "NETCDF_TMAX_MAKE_LOCAL_ARCHIVE") ) then
      call DAT(TMAX_DATA)%set_make_local_archive(lTRUE)

    elseif (str_compare(sItem, "NETCDF_TMIN_MAKE_LOCAL_ARCHIVE") ) then
      call DAT(TMIN_DATA)%set_make_local_archive(lTRUE)

    else if (sItem == "TMAX_GRID_PROJECTION_DEFINITION") then
      call DAT(TMAX_DATA)%set_PROJ4( trim(sRecord) )

    else if ( str_compare(sItem,"TMAX_SCALE") ) then
      call Chomp ( sRecord, sArgument )
      call DAT(TMAX_DATA)%set_scale(asReal(sArgument))

    else if ( str_compare(sItem,"TMAX_OFFSET") ) then
      call Chomp ( sRecord, sArgument )
      call DAT(TMAX_DATA)%set_offset(asReal(sArgument))

    else if ( str_compare(sItem,"TMAX_CONVERSION_FACTOR") ) then
      call Chomp ( sRecord, sArgument )
      call DAT(TMAX_DATA)%set_conversion_factor(asReal(sArgument))

    elseif (sItem == "TMAX_MINIMUM_ALLOWED_VALUE") then
      call Chomp ( sRecord, sArgument )
      DAT(TMAX_DATA)%rMinAllowedValue = asReal(sArgument)

    elseif (sItem == "TMAX_MAXIMUM_ALLOWED_VALUE") then
      call Chomp ( sRecord, sArgument )
      DAT(TMAX_DATA)%rMaxAllowedValue = asReal(sArgument)

    elseif (sItem == "TMAX_MISSING_VALUES_CODE") then
      call Chomp ( sRecord, sArgument )
      DAT(TMAX_DATA)%rMissingValuesCode = asReal(sArgument)

    elseif (sItem == "TMAX_MISSING_VALUES_OPERATOR") then
      call Chomp ( sRecord, sArgument )
      DAT(TMAX_DATA)%sMissingValuesOperator = trim(sArgument)

    elseif (sItem == "TMAX_MISSING_VALUES_ACTION") then
      call Chomp ( sRecord, sArgument )
      if (sArgument == "ZERO") then
        DAT(TMAX_DATA)%iMissingValuesAction = MISSING_VALUES_ZERO_OUT
      elseif (sArgument == "MEAN" ) then
        DAT(TMAX_DATA)%iMissingValuesAction = MISSING_VALUES_REPLACE_WITH_MEAN
      else
        call assert(lFALSE, "Unknown missing value action supplied for" &
          //" TMAX data: "//dquote(sArgument) )
      endif

    else if (sItem == "TMIN_GRID_PROJECTION_DEFINITION") then
      call DAT(TMIN_DATA)%set_PROJ4( trim(sRecord) )

    else if ( str_compare(sItem,"TMIN_SCALE") ) then
      call Chomp ( sRecord, sArgument )
      call DAT(TMIN_DATA)%set_scale(asReal(sArgument))

    else if ( str_compare(sItem,"TMIN_OFFSET") ) then
      call Chomp ( sRecord, sArgument )
      call DAT(TMIN_DATA)%set_offset(asReal(sArgument))

    else if ( str_compare(sItem,"TMIN_CONVERSION_FACTOR") ) then
      call Chomp ( sRecord, sArgument )
      call DAT(TMIN_DATA)%set_conversion_factor(asReal(sArgument))

    elseif (sItem == "TMIN_MINIMUM_ALLOWED_VALUE") then
      call Chomp ( sRecord, sArgument )
      DAT(TMIN_DATA)%rMinAllowedValue = asReal(sArgument)

    elseif (sItem == "TMIN_MAXIMUM_ALLOWED_VALUE") then
      call Chomp ( sRecord, sArgument )
      DAT(TMIN_DATA)%rMaxAllowedValue = asReal(sArgument)

    elseif (sItem == "TMIN_MISSING_VALUES_CODE") then
      call Chomp ( sRecord, sArgument )
      DAT(TMIN_DATA)%rMissingValuesCode = asReal(sArgument)

    elseif (sItem == "TMIN_MISSING_VALUES_OPERATOR") then
      call Chomp ( sRecord, sArgument )
      DAT(TMIN_DATA)%sMissingValuesOperator = trim(sArgument)

    elseif (sItem == "TMIN_MISSING_VALUES_ACTION") then
      call Chomp ( sRecord, sArgument )
      if (sArgument == "ZERO") then
        DAT(TMIN_DATA)%iMissingValuesAction = MISSING_VALUES_ZERO_OUT
      elseif (sArgument == "MEAN" ) then
        DAT(TMIN_DATA)%iMissingValuesAction = MISSING_VALUES_REPLACE_WITH_MEAN
      else
        call assert(lFALSE, "Unknown missing value action supplied for" &
          //" TMIN data: "//dquote(sArgument) )
      endif


    else if ( sItem == "LAND_USE" .or. sItem == "LANDUSE" ) then
      write(UNIT=LU_LOG,FMT=*) "Populating land use grid"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      if ( trim(sOption) == "CONSTANT" ) then
!        pConfig%iConfigureLanduse = CONFIG_LANDUSE_CONSTANT
        read ( unit=sArgument, fmt=*, iostat=iStat ) iValue
        call Assert( iStat == 0, "Cannot read integer data value" )
!        pLandUseGrid => grid_Create ( pGrd%iNX, pGrd%iNY, pGrd%rX0, pGrd%rY0, &
!          pGrd%rX1, pGrd%rY1, DATATYPE_INT )
!        pLandUseGrid%iData = iValue
!        pLandUseGrid%sFilename = "Constant-value grid"

        call DAT(LANDUSE_DATA)%initialize(sDescription=sItem, &
           iConstant=iValue )

      elseif(trim(sOption) == "DYNAMIC" ) then
        ! make room for another option and read in the proper value of sArgument
        sOption = sArgument
        call Uppercase( sOption )
        call Chomp ( sRecord, sArgument )

        call DAT(LANDUSE_DATA)%initialize(sDescription=sItem, &
        sFileType=trim(sOption), &
        sFilenameTemplate=trim(sArgument), &
        iDataType=DATATYPE_INT )

      elseif( trim(sOption) == "ARC_GRID" &
         .or. trim(sOption) == "SURFER" ) then   ! read in a static gridded landuse file

        call DAT(LANDUSE_DATA)%initialize(sDescription=sItem, &
        sFileType=trim(sOption), &
        sFilename=trim(sArgument), &
        iDataType=DATATYPE_INT )

      else
        call Assert( lFALSE, "Illegal landuse input option or format specified", &
          TRIM(__FILE__),__LINE__)
      endif
      flush(UNIT=LU_LOG)

    else if (sItem == "LANDUSE_PROJECTION_DEFINITION") then
      call DAT(LANDUSE_DATA)%set_PROJ4( trim(sRecord) )

    elseif (sItem == "LANDUSE_SET_MINIMUM_ALLOWED" ) then
      call DAT(LANDUSE_DATA)%set_valid_minimum(asInt(sRecord))

    elseif (sItem == "LANDUSE_SET_MAXIMUM_ALLOWED" ) then
      call DAT(LANDUSE_DATA)%set_valid_maximum(asInt(sRecord))

    else if ( sItem == "FLOW_DIRECTION" ) then
      write(UNIT=LU_LOG,FMT=*) "Populating flow direction grid"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      if ( trim(sOption) == "CONSTANT" ) then
        read ( unit=sArgument, fmt=*, iostat=iStat ) iValue
        call Assert( iStat == 0, "Cannot read integer data value" )
!        pFlowDirGrid => grid_Create ( pGrd%iNX, pGrd%iNY, pGrd%rX0, pGrd%rY0, &
!          pGrd%rX1, pGrd%rY1, DATATYPE_INT )
!        pFlowDirGrid%iData = iValue
!        pFlowDirGrid%sFilename = "Constant-value grid"

        call DAT(FLOWDIR_DATA)%initialize(sDescription=trim(sItem), &
           iConstant=iValue )

      else
!        pFlowDirGrid => grid_Read( sArgument, sOption, DATATYPE_INT )
!        pFlowDirGrid%sFilename = trim(sArgument)
        call DAT(FLOWDIR_DATA)%initialize(sDescription=trim(sItem), &
          sFileType=trim(sOption), &
          sFilename=trim(sArgument), &
          iDataType=DATATYPE_INT )

      end if
      flush(UNIT=LU_LOG)

    else if (sItem == "FLOW_DIRECTION_PROJECTION_DEFINITION") then
      call DAT(FLOWDIR_DATA)%set_PROJ4( trim(sRecord) )

    elseif (sItem == "FLOW_DIRECTION_SET_MINIMUM_ALLOWED" ) then
      call DAT(FLOWDIR_DATA)%set_valid_minimum(asInt(sRecord))

    elseif (sItem == "FLOW_DIRECTION_SET_MAXIMUM_ALLOWED" ) then
      call DAT(FLOWDIR_DATA)%set_valid_maximum(asInt(sRecord))


    else if ( sItem == "ROUTING_FRACTION" ) then
      write(UNIT=LU_LOG,FMT=*) "Populating routing fraction grid"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      if ( trim(sOption) == "CONSTANT" ) then
        read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
        call Assert( iStat == 0, "Cannot read real data value" )

        call DAT(ROUTING_FRAC_DATA)%initialize(sDescription=trim(sItem), &
           rConstant=rValue )

      else

        call DAT(ROUTING_FRAC_DATA)%initialize(sDescription=trim(sItem), &
          sFileType=trim(sOption), &
          sFilename=trim(sArgument), &
          iDataType=DATATYPE_REAL )

      endif

    else if (sItem == "ROUTING_FRACTION_PROJECTION_DEFINITION") then
      call DAT(ROUTING_FRAC_DATA)%set_PROJ4( trim(sRecord) )

    elseif (sItem == "ROUTING_FRACTION_SET_MINIMUM_ALLOWED" ) then
      call DAT(ROUTING_FRAC_DATA)%set_valid_minimum(asReal(sRecord))

    elseif (sItem == "ROUTING_FRACTION_SET_MAXIMUM_ALLOWED" ) then
      call DAT(ROUTING_FRAC_DATA)%set_valid_maximum(asReal(sRecord))

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
        input_grd => grid_Read( sArgument, sOption, DATATYPE_REAL )
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

    else if ( sItem == "OUTPUT_PATH" ) then
      write(UNIT=LU_LOG,FMT=*) &
        "Setting output pathname"
      call Chomp ( sRecord, sArgument )
      pConfig%sOutputFilePath = TRIM(ADJUSTL(sArgument))
      write(UNIT=LU_LOG,FMT=*)  &
        "Output pathname set to: '" &
        //TRIM(pConfig%sOutputFilePath)//"'"
      flush(UNIT=LU_LOG)

    else if ( sItem == "OUTPUT_DAILY_PATH" ) then
      write(UNIT=LU_LOG,FMT=*) &
        "Setting output pathname for daily output"
      call Chomp ( sRecord, sArgument )
      pConfig%sOutputFilePathDaily = TRIM(ADJUSTL(sArgument))
      write(UNIT=LU_LOG,FMT=*)  &
        "Output pathname for daily output set to: '" &
        //TRIM(pConfig%sOutputFilePathDaily)//"'"
      flush(UNIT=LU_LOG)

    else if ( sItem == "OUTPUT_MONTHLY_PATH" ) then
      write(UNIT=LU_LOG,FMT=*) &
        "Setting output pathname for monthly output"
      call Chomp ( sRecord, sArgument )
      pConfig%sOutputFilePathMonthly = TRIM(ADJUSTL(sArgument))
      write(UNIT=LU_LOG,FMT=*)  &
        "Output pathname for monthly output set to: '" &
        //TRIM(pConfig%sOutputFilePathMonthly)//"'"
      flush(UNIT=LU_LOG)

    else if ( sItem == "OUTPUT_ANNUAL_PATH" ) then
      write(UNIT=LU_LOG,FMT=*) &
        "Setting output pathname for annual output"
      call Chomp ( sRecord, sArgument )
      pConfig%sOutputFilePathAnnual = TRIM(ADJUSTL(sArgument))
      write(UNIT=LU_LOG,FMT=*)  &
        "Output pathname for annual output set to: '" &
        //TRIM(pConfig%sOutputFilePathAnnual)//"'"
      flush(UNIT=LU_LOG)

    else if ( sItem == "OUTPUT_FUTURE_PATH" ) then
      write(UNIT=LU_LOG,FMT=*) &
        "Setting pathname for future output"
      call Chomp ( sRecord, sArgument )
      pConfig%sFutureFilePath = TRIM(ADJUSTL(sArgument))
      write(UNIT=LU_LOG,FMT=*)  &
        "Pathname for future output set to: '" &
        //TRIM(pConfig%sFutureFilePath)//"'"
      flush(UNIT=LU_LOG)

    else if ( sItem == "IMAGE_PATH" ) then
      write(UNIT=LU_LOG,FMT=*) &
        "Setting image pathname"
      call Chomp ( sRecord, sArgument )
      pConfig%sImageFilePath = TRIM(ADJUSTL(sArgument))
      write(UNIT=LU_LOG,FMT=*)  &
        "Image pathname set to: '" &
        //TRIM(pConfig%sImageFilePath)//"'"
      flush(UNIT=LU_LOG)

    else if ( sItem == "IMAGE_DAILY_PATH" ) then
      write(UNIT=LU_LOG,FMT=*) &
        "Setting pathname for daily image output"
      call Chomp ( sRecord, sArgument )
      pConfig%sImageFilePathDaily = TRIM(ADJUSTL(sArgument))
      write(UNIT=LU_LOG,FMT=*)  &
        "Pathname for daily image output set to: '" &
        //TRIM(pConfig%sImageFilePathDaily)//"'"
      flush(UNIT=LU_LOG)

    else if ( sItem == "IMAGE_MONTHLY_PATH" ) then
      write(UNIT=LU_LOG,FMT=*) &
        "Setting pathname for monthly image output"
      call Chomp ( sRecord, sArgument )
      pConfig%sImageFilePathMonthly = TRIM(ADJUSTL(sArgument))
      write(UNIT=LU_LOG,FMT=*)  &
        "Pathname for monthly image output set to: '" &
        //TRIM(pConfig%sImageFilePathMonthly)//"'"
      flush(UNIT=LU_LOG)

    else if ( sItem == "IMAGE_ANNUAL_PATH" ) then
      write(UNIT=LU_LOG,FMT=*) &
        "Setting pathname for annual image output"
      call Chomp ( sRecord, sArgument )
      pConfig%sImageFilePathAnnual = TRIM(ADJUSTL(sArgument))
      write(UNIT=LU_LOG,FMT=*)  &
        "Pathname for annual image output set to: '" &
        //TRIM(pConfig%sImageFilePathAnnual)//"'"
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
        "  Missing temperature data will be replaced with the mean value of the non-missing values."
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
      pGrd%Cells(iColNum,iRowNum)%iNumFilesSSF = &
        pGrd%Cells(iColNum,iRowNum)%iNumFilesSSF + 1

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
        input_grd => grid_Read( sArgument, sOption, DATATYPE_REAL )
        call Assert( grid_Conform( pGrd, input_grd ), &
                      "Non-conforming grid" )
        pGrd%Cells%rElevation = input_grd%rData
        call grid_Destroy( input_grd )
      end if

    else if ( sItem == "SOIL_GROUP" .or. sItem == "HYDROLOGIC_SOIL_GROUP") then
      write(UNIT=LU_LOG,FMT=*) "Populating hydrologic soil group grid"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      if ( trim(sOption) == "CONSTANT" ) then
        read ( unit=sArgument, fmt=*, iostat=iStat ) iValue
        call Assert( iStat == 0, &
          "Cannot read integer data value" )
!        pSoilGroupGrid => grid_Create ( pGrd%iNX, pGrd%iNY, pGrd%rX0, pGrd%rY0, &
!          pGrd%rX1, pGrd%rY1, DATATYPE_INT )
!        pSoilGroupGrid%iData = iValue
!        pSoilGroupGrid%sFilename = "Constant-value grid"
        !pGrd%Cells%iSoilGroup = iValue

        call DAT(SOILS_GROUP_DATA)%initialize(sDescription=trim(sItem), &
           iConstant=iValue )

      else
!        pSoilGroupGrid => grid_Read( sArgument, sOption, DATATYPE_INT )
!        pSoilGroupGrid%sFilename = trim(sArgument)

        call DAT(SOILS_GROUP_DATA)%initialize(sDescription=trim(sItem), &
          sFileType=trim(sOption), &
          sFilename=trim(sArgument), &
          iDataType=DATATYPE_INT )

      end if
      flush(UNIT=LU_LOG)

    else if (sItem == "SOIL_GROUP_PROJECTION_DEFINITION") then
!      pConfig%sSoilGroup_PROJ4 = trim(sRecord)
      call DAT(SOILS_GROUP_DATA)%set_PROJ4( trim(sRecord) )

    elseif (sItem == "SOIL_GROUP_SET_MINIMUM_ALLOWED" ) then
      call DAT(SOILS_GROUP_DATA)%set_valid_minimum(asInt(sRecord))

    elseif (sItem == "SOIL_GROUP_SET_MAXIMUM_ALLOWED" ) then
      call DAT(SOILS_GROUP_DATA)%set_valid_maximum(asInt(sRecord))


    else if ( sItem == "LAND_USE_LOOKUP_TABLE" ) then
      write(UNIT=LU_LOG,FMT=*) "Reading land-use lookup table"
      call Chomp ( sRecord, pConfig%sLandUseLookupFilename )
      if ( len_trim(pConfig%sLandUseLookupFilename) == 0 ) then
        call Assert( .false._c_bool, "No land use lookup table specified" )
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
      call model_ReadIrrigationLookupTable( pConfig, pGrd )
      flush(UNIT=LU_LOG)

    else if ( sItem == "BASIN_MASK" ) then
      write(UNIT=LU_LOG,FMT=*) "Populating basin mask grid"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      if ( trim(sOption) == "CONSTANT" ) then
        read ( unit=sArgument, fmt=*, iostat=iStat ) iValue
        call Assert( iStat == 0, "Cannot read integer data value" )

        call DAT(MASK_DATA)%initialize(sDescription=trim(sItem), &
           iConstant=iValue )

      else

        call DAT(MASK_DATA)%initialize(sDescription=trim(sItem), &
          sFileType=trim(sOption), &
          sFilename=trim(sArgument), &
          iDataType=DATATYPE_INT )

      end if
      flush(UNIT=LU_LOG)

    else if ( sItem == "BASIN_MASK_LOOKUP_TABLE" ) then
      write(UNIT=LU_LOG,FMT=*) "Reading basin mask lookup table"
      call Chomp ( sRecord, pConfig%sBasinMaskFilename )
      if ( len_trim(pConfig%sBasinMaskFilename) == 0 ) then
        call Assert( lFALSE, "No basin mask table specified" )
      end if
      call model_ReadBasinMaskTable( pConfig )
      flush(UNIT=LU_LOG)

    else if (sItem == "BASIN_MASK_PROJECTION_DEFINITION") then
      write(UNIT=LU_LOG,FMT=*) "Reading basin mask projection definition"
      if (DAT(MASK_DATA)%iSourceDataType /= DATATYPE_NA) then
        call DAT(MASK_DATA)%set_PROJ4( trim(sRecord) )
      endif
      pConfig%sBasinMaskPROJ4String = trim( sRecord )
      if ( len_trim(pConfig%sBasinMaskPROJ4String) == 0 ) then
        call Assert( lFALSE, "No basin mask projection definition specified" )
      end if

    else if ( sItem == "ADJUSTED_WATER_CAPACITY" ) then
      write(UNIT=LU_LOG,FMT=*) "Populating adjusted water capacity grid"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      if ( trim(sOption) == "CONSTANT" ) then
        pConfig%iConfigureSMCapacity = CONFIG_SM_CAPACITY_CONSTANT
        read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
        call Assert( iStat == 0, "Cannot read real data value" )
        pGrd%Cells%rSoilWaterCap = rValue
        write(UNIT=LU_LOG,FMT=*)  "using a constant value for the maximum soil water capacity"
        write(UNIT=LU_LOG,FMT=*)  "NOTE: any option specified with the 'WATER_CAPACITY' option will be ignored"
      else
        pConfig%iConfigureSMCapacity = CONFIG_SM_CAPACITY_FM_TABLE
        input_grd => grid_Read( sArgument, sOption, DATATYPE_REAL )
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

    else if ( sItem == "WATER_CAPACITY" .or. sItem == "AVAILABLE_WATER_CAPACITY") then
      write(UNIT=LU_LOG,FMT=*) "Populating available water capacity grid"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
!      allocate(pSoilAWCGrid, stat=iStat)
!      call assert(iStat==0,"Problem allocating memory for the " &
!        //"Soil Available Water Capacity data object", &
!        trim(__FILE__), __LINE__)

      if ( trim(sOption) == "CONSTANT" ) then

        read ( unit=sArgument, fmt=*, iostat=iStat ) rValue
        call Assert( iStat == 0, "Cannot read real data value" )
!        pSoilAWCGrid => grid_Create ( pGrd%iNX, pGrd%iNY, pGrd%rX0, pGrd%rY0, &
!          pGrd%rX1, pGrd%rY1, DATATYPE_REAL )
!        pSoilAWCGrid%rData = rValue

        call DAT(AWC_DATA)%initialize(sDescription=trim(sItem), &
           rConstant=rValue )

      else

        call DAT(AWC_DATA)%initialize(sDescription=trim(sItem), &
          sFileType=trim(sOption), &
          sFilename=trim(sArgument), &
          iDataType=DATATYPE_REAL )

      end if

    else if (sItem == "WATER_CAPACITY_PROJECTION_DEFINITION") then
      call DAT(AWC_DATA)%set_PROJ4( trim(sRecord) )

    elseif (sItem == "WATER_CAPACITY_SET_MINIMUM_ALLOWED" ) then
      call DAT(AWC_DATA)%set_valid_minimum(asReal(sRecord))

    elseif (sItem == "WATER_CAPACITY_SET_MAXIMUM_ALLOWED" ) then
      call DAT(AWC_DATA)%set_valid_maximum(asReal(sRecord))


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
        input_grd => grid_Read( sArgument, sOption, DATATYPE_REAL )
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
         kind=c_bool), "Negative values are not allowed in the" &
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
      call Assert( logical(iStat==0, kind=c_bool), &
        "Failed to read station elevation " // sArgument )
      call Chomp( sRecord, sArgument )
      read(unit=sArgument, fmt=*, iostat=iStat) pconfig%rElevDryFactor
      call Assert( logical(iStat==0, kind=c_bool), &
        "Failed to read dry elevation factor " // sArgument )
      call Chomp( sRecord, sArgument )
      read(unit=sArgument, fmt=*, iostat=iStat) pconfig%rElevHumidFactor
      call Assert( logical(iStat==0, kind=c_bool), &
        "Failed to read humid elevation factor " // sArgument )
      call Chomp( sRecord, sArgument )
      read(unit=sArgument, fmt=*, iostat=iStat) pconfig%rElevHumidityThreshold
      call Assert( logical(iStat==0, kind=c_bool), &
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

      else if (TRIM(sArgument)== "GRAPH" .or. TRIM(sArgument)== "PLOT") then
        STAT_INFO(iVarNum)%iDailyOutput = iGRAPH
      else if (TRIM(sArgument)== "BOTH") then
        STAT_INFO(iVarNum)%iDailyOutput = iBOTH
      else
        STAT_INFO(iVarNum)%iDailyOutput = iNONE
      end if

      ! now repeat for monthly output options
      call Chomp ( sRecord, sArgument )
      if (TRIM(sArgument)== "GRID") then
        STAT_INFO(iVarNum)%iMonthlyOutput = iGRID
      else if (TRIM(sArgument)== "GRAPH" .or. TRIM(sArgument)== "PLOT") then
        STAT_INFO(iVarNum)%iMonthlyOutput = iGRAPH
      else if (TRIM(sArgument)== "BOTH") then
        STAT_INFO(iVarNum)%iMonthlyOutput = iBOTH

      else
        STAT_INFO(iVarNum)%iMonthlyOutput = iNONE
      end if

      ! repeat for annual output options
      call Chomp ( sRecord, sArgument )
      if (TRIM(sArgument)== "GRID") then
        STAT_INFO(iVarNum)%iAnnualOutput = iGRID
      else if (TRIM(sArgument)== "GRAPH" .or. TRIM(sArgument)== "PLOT") then
        STAT_INFO(iVarNum)%iAnnualOutput = iGRAPH
      else if (TRIM(sArgument)== "BOTH") then
        STAT_INFO(iVarNum)%iAnnualOutput = iBOTH
      else
        STAT_INFO(iVarNum)%iAnnualOutput = iNONE
      end if

      ! finally repeat for NetCDF output options
      call Chomp ( sRecord, sArgument )
      if (TRIM(sArgument)== "NETCDF") then
        STAT_INFO(iVarNum)%iNetCDFOutput = iGRID
      else
        STAT_INFO(iVarNum)%iNetCDFOutput = iNONE
      end if

      write(UNIT=LU_LOG,FMT=*)  "Options have been set for: ", &
         TRIM(STAT_INFO(iVarNum)%sVARIABLE_NAME)
      write(UNIT=LU_LOG,FMT=*)  "STAT_INFO(iVarNum)%iAnnualOutput: ",&
         STAT_INFO(iVarNum)%iAnnualOutput
      write(UNIT=LU_LOG,FMT=*)  "STAT_INFO(iVarNum)%iMonthlyOutput: ",&
         STAT_INFO(iVarNum)%iMonthlyOutput
      write(UNIT=LU_LOG,FMT=*)  "STAT_INFO(iVarNum)%iDailyOutput: ",&
         STAT_INFO(iVarNum)%iDailyOutput

      write(UNIT=LU_LOG,FMT=*)  "STAT_INFO(iVarNum)%iNetCDFOutput: ",&
         STAT_INFO(iVarNum)%iNetCDFOutput

    else if ( sItem == "DISLIN_PARAMETERS" ) then
      call Chomp ( sRecord, sArgument )
      write(UNIT=LU_LOG,FMT=*) "Setting the values for DISLIN parameters"
      lMatch = lFALSE
      do i=1,iNUM_VARIABLES
        if(TRIM(sArgument) == TRIM(STAT_INFO(i)%sVARIABLE_NAME)) then
          iVarNum = i
          lMatch = lTRUE
          exit
        end if
      end do
      call assert( lMatch, "DISLIN parameters were specified for an unknown SWB parameter" &
          //"( "//dquote(sArgument)//" )")

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
        call Assert( .false._c_bool, "Illegal initial abstraction method specified" )
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

    else if ( sItem == "GRID_TEST" ) then
      write(UNIT=LU_LOG,FMT=*) "Testing the value obtained when reading a real valued grid"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      call Chomp ( sRecord, sArgument )
      write(UNIT=LU_LOG,FMT=*) "Testing the ability to read from grid: "//dquote(sArgument)
      input_grd => grid_Read( sArgument, sOption, DATATYPE_REAL )
      if(.not. grid_Conform( pGrd, input_grd ) ) &
        write(UNIT=LU_LOG,FMT=*) "INPUT GRID DOES NOT ALIGN WITH PROJECT COORDINATES..."
      call stats_WriteMinMeanMax( LU_LOG, dquote(sArgument), input_grd%rData)
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
        input_grd => grid_Read( sArgument, sOption, DATATYPE_REAL )
        call Assert( grid_Conform( pGrd, input_grd ), &
                      "Non-conforming grid" )
        pGrd%Cells%rSnowCover = input_grd%rData
        call grid_Destroy( input_grd )
      else if ( trim(sOption) == "SURFER" ) then
        write(UNIT=LU_LOG,FMT=*) "Snow cover data will be read as a SURFER grid"
        input_grd => grid_Read( sArgument, sOption, DATATYPE_REAL )
        call Assert( grid_Conform( pGrd, input_grd ), &
                      "Non-conforming grid" )
        pGrd%Cells%rSnowCover = input_grd%rData
        call grid_Destroy( input_grd )
      else
        call Assert( .false._c_bool, "Illegal snow cover input format specified" )
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
        call Assert( .false._c_bool, "Illegal runoff option specified" )
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
          call Assert( .false._c_bool, "Illegal runoff procedure option specified" )
        end if
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
!        pConfig%iConfigureET = CONFIG_ET_BLANEY_CRIDDLE
        call assert(lFALSE, "The Blaney-Criddle method has been removed from this " &
         //"version of swb.", trim(__FILE__),__LINE__)
!        call et_bc_configure( sRecord )
      else if ( trim(sOption) == "HARGREAVES" ) then
        pConfig%iConfigureET = CONFIG_ET_HARGREAVES
        call et_hargreaves_configure( pConfig, sRecord )
      else
        call Assert( .false._c_bool, "Illegal ET option specified" )
      end if
      flush(UNIT=LU_LOG)

    else if ( sItem == "SM" ) then
      write(UNIT=LU_LOG,FMT=*) "Configuring soil-moisture options"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      if ( trim(sOption) == "T-M" .or. trim(sOption) == "THORNTHWAITE-MATHER") then
        call Chomp ( sRecord, sOption )

        if ( trim(sOption) == "EQUATIONS" .or. trim(sOption) == "EQUATION") then
          pConfig%iConfigureSM = CONFIG_SM_TM_EQUATIONS
        elseif (len_trim(sRecord) == 0) then
          pConfig%iConfigureSM = CONFIG_SM_TM_LOOKUP_TABLE
          call sm_thornthwaite_mather_Configure( sOption )
        elseif (trim(sOption) == "TABLE" ) then
          pConfig%iConfigureSM = CONFIG_SM_TM_LOOKUP_TABLE
          call sm_thornthwaite_mather_Configure( sRecord )
        else
          call Assert( lFALSE, "Illegal Thornthwaite-Mather soil-moisture retention option specified" )
        endif
      else
        call Assert( lFALSE, "Illegal soil-moisture option specified" )
      end if
      flush(UNIT=LU_LOG)

    else if ( sItem == "FAO56" ) then
      write(UNIT=LU_LOG,FMT=*) "Configuring swb to use FAO56 crop coefficients"
      call Chomp ( sRecord, sOption )
      call Uppercase ( sOption )
      if ( trim(sOption) == "CROP_COEFFICIENTS_ONE_FACTOR_STANDARD") then
        pConfig%iConfigureFAO56 = CONFIG_FAO56_ONE_FACTOR_STANDARD
      elseif ( trim(sOption) == "CROP_COEFFICIENTS_ONE_FACTOR_NONSTANDARD") then
        pConfig%iConfigureFAO56 = CONFIG_FAO56_ONE_FACTOR_NONSTANDARD
      elseif ( trim(sOption) == "CROP_COEFFICIENTS_TWO_FACTOR_STANDARD") then
        pConfig%iConfigureFAO56 = CONFIG_FAO56_TWO_FACTOR_STANDARD
      elseif ( trim(sOption) == "CROP_COEFFICIENTS_TWO_FACTOR_NONSTANDARD") then
        pConfig%iConfigureFAO56 = CONFIG_FAO56_TWO_FACTOR_NONSTANDARD

      else
        call Assert( lFALSE, "Illegal FAO56 option specified" )
      end if
      flush(UNIT=LU_LOG)

    else if ( sItem == "ENABLE_IRRIGATION" ) then
      write(UNIT=LU_LOG,FMT=*) "Allowing swb to supplement soil moisture by means of irrigation"
      pConfig%lEnableIrrigation = lTRUE

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
        call Assert( .false._c_bool, "Illegal output format specified" )
      end if
      flush(UNIT=LU_LOG)

    else if ( sItem == "OUTPUT_GRID_SUFFIX" ) then
      write(UNIT=LU_LOG,FMT=*) "Setting output grid file suffix"
      call Chomp ( sRecord, sArgument )
      if ( len_trim(sArgument) == 0 ) then
        write(UNIT=LU_LOG,FMT=*)  "[No output grid file suffix specified.  Default is 'asc']"
        pConfig%sOutputFileSuffix = "asc"
      else
        pConfig%sOutputFileSuffix = trim(sArgument)
      endif
      flush(UNIT=LU_LOG)

    else if ( sItem == "OUTPUT_GRID_PREFIX" ) then
      write(UNIT=LU_LOG,FMT=*) "Setting output grid file prefix"
      call Chomp ( sRecord, sArgument )
      if ( len_trim(sArgument) == 0 ) then
        write(UNIT=LU_LOG,FMT=*)  "[No output grid file prefix specified.  Default is 'output/swb_']"
      else
        pConfig%sOutputFilePrefix = trim(sArgument)
      endif
      flush(UNIT=LU_LOG)

    else if ( sItem == "FUTURE_GRID_PREFIX" ) then
      write(UNIT=LU_LOG,FMT=*) "Setting future grid file prefix"
      call Chomp ( sRecord, sArgument )
      if ( len_trim(sArgument) == 0 ) then
        write(UNIT=LU_LOG,FMT=*)  "[No future output grid file prefix specified.  Default is 'output/future/swb_future_']"
      else
        pConfig%sFutureFilePrefix = trim(sArgument)
      endif
      flush(UNIT=LU_LOG)

    else if ( sItem == "SOLVE" ) then
      write(UNIT=LU_LOG,FMT=*) "Solving the model"
      flush(UNIT=LU_LOG)
      call Chomp ( sRecord, sArgument )
      call Assert (LOGICAL(len_trim(sArgument)>0,kind=c_bool), &
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
!        pConfig%iCurrentJulianDay = pConfig%iStartJulianDay - 1
         pConfig%iCurrentJulianDay = pConfig%iStartJulianDay
        close( unit=LU_TS )
      end if
      pConfig%lGriddedData = lFALSE
      ! actual call to "model_Solve" subroutine
      call model_Solve( pGrd, pConfig, pGraph)

    else if ( sItem == "SOLVE_NO_TS_DATA" .or. sItem == "SOLVE_NO_TS_FILE" ) then
      pConfig%lGriddedData = lTRUE
      write(UNIT=LU_LOG,FMT=*) &
        "Solving the model - no single-station time series data will be read"
      flush(UNIT=LU_LOG)
      call Chomp ( sRecord, sArgument )
      call Assert (LOGICAL(len_trim(sArgument)>0,kind=c_bool), &
         "Must specify the year to begin simulation" )
      read(sArgument,FMT=*,iostat=iStat) pConfig%iStartYear
      call Assert( iStat == 0, &
         "Cannot read integer data value for beginning year of simulation" )
      pConfig%iStartJulianDay = julian_day ( pConfig%iStartYear, 1, 1)
      call MODEL_SIM%tStartDate%calcJulianDay(iMonth=1, iDay=1, iYear=pConfig%iStartYear)

      call Chomp ( sRecord, sArgument )
      call Assert ( len_trim(sArgument) > 0, &
         "Must specify the year to end simulation" )
      read(sArgument,FMT=*,iostat=iStat) pConfig%iEndYear
      call Assert( iStat == 0, &
         "Cannot read integer data value for ending year of simulation" )
      call Assert( pConfig%iStartYear <= pConfig%iEndYear, &
         "Ending year must be equal to or greater than the beginning year" )
      call MODEL_SIM%tEndDate%calcJulianDay(iMonth=1, iDay=1, iYear=pConfig%iStartYear)
      pConfig%iEndJulianDay = julian_day ( pConfig%iEndYear, 12, 31)
      MODEL_SIM%iJulianDay = MODEL_SIM%tStartDate%iJulianDay
      call MODEL_SIM%calcGregorianDate()
      pConfig%iCurrentJulianDay = pConfig%iStartJulianDay
      call gregorian_date(pConfig%iStartJulianDay, pConfig%iYear, &
         pConfig%iMonth, pConfig%iDay)

      DAT(:)%iStartYear = pConfig%iStartYear
      DAT(:)%iEndYear = pConfig%iEndYear

      !> ** THIS IS ESSENTIALLY THE TIME CONTROL LOOP FOR USE
      !>    WITH GRIDDED DATA
      !> @todo incorporate this into a proper time control module
      do i=pConfig%iStartYear,pConfig%iEndYear
        pConfig%iYear = i
        pConfig%iCurrentJulianDay = julian_day ( i, 1, 1)
      call gregorian_date(pConfig%iCurrentJulianDay, pConfig%iYear, &
         pConfig%iMonth, pConfig%iDay)
        pConfig%iNumDaysInYear = num_days_in_year(pConfig%iYear)
        write(UNIT=LU_LOG,FMT="(a,i4.4)") "Calling model_Main." &
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
        call Assert(LOGICAL(idx <= STREAM_INTERACTIONS_MAX, kind=c_bool), &
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
        input_grd => grid_Read( sArgument, sOption, DATATYPE_INT )
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
      pConfig%lEOJ_IsPresent = lTRUE
      call model_EndOfRun(pGrd, pConfig, pGraph)
      exit

    else
      call Assert( lFALSE, "Illegal directive: "//squote(sItem) &
        // "~found on line number "//trim(int2char(iCurrentLineNumber)) &
        //" of control file "//squote(sControlfile) )
    end if

  end do CTL_READ  ! end of control file read loop

  if ( .not. pConfig%lEOJ_IsPresent ) &
    call warn("No end-of-job directive was found; some cleanup steps may be skipped, ~" &
    //"and certain output files and/or images may be missing.")

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
