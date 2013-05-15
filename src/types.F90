!> @file
!> @brief Contains a single module, @ref types, which contains types definitions
!>  and general utility functions used by the SWB code.

!> @brief Contains types definitions and general utility functions used by the SWB code.
!>
!> This module defines parameters and derived data types for use throughout
!> the SWB code.  This module also contains a number of utility functions
!> that perform string manipulations, convert Celcius to Fahrenheit, and
!> convert between Julian days and Gregorian dates.
module types

#ifdef __INTEL_COMPILER
  use ifport
#endif

  use iso_c_binding

  implicit none

  !> @defgroup types types
  !> @{

  character(len=45), public, parameter :: &
      SWB_VERSION = "1.2 BETA (geographic transformations enabled)"

  !> @name Variables: Fortran logical unit numbers
  !> Global logical unit numbers for input and output
  !> @{
  integer (kind=c_int), parameter :: LU_STD_OUT = 6
  integer (kind=c_int), parameter :: LU_CONTROL = 12
  integer (kind=c_int), parameter :: LU_GRID = 14
  integer (kind=c_int), parameter :: LU_LOOKUP = 16
  integer (kind=c_int), parameter :: LU_TS = 18
  integer (kind=c_int), parameter :: LU_TEMP = 20
  integer (kind=c_int), parameter :: LU_MSB_REPORT = 22
  integer (kind=c_int), parameter :: LU_BIN_FILE = 24
  integer (kind=c_int), parameter :: LU_ROUTING = 26
  integer (kind=c_int), parameter :: LU_CSV_MIN = 28
  integer (kind=c_int), parameter :: LU_CSV_MEAN = 30
  integer (kind=c_int), parameter :: LU_CSV_MAX = 32
  integer (kind=c_int), parameter :: LU_CSV_ANNUAL = 34
  integer (kind=c_int), parameter :: LU_MASK = 36
  integer (kind=c_int), parameter :: LU_MASK_FILE = 38
  integer (kind=c_int), parameter :: LU_MASK_STATS_CSV = 40
  integer (kind=c_int), parameter :: LU_PEST_STATS = 42
  integer (kind=c_int), parameter :: LU_PEST_OBS = 44
  integer (kind=c_int), parameter :: LU_PEST_INS = 46
  integer (kind=c_int), parameter :: LU_LOG = 48
  !> @}

  !> @name Constants: General conversion factors and flags
  !> Some useful and common conversion factors, defined here
  !> to ensure consistency throughout the code
  !> @{
  real (kind=c_float), parameter :: rZERO = 0.0_c_float
  real (kind=c_double), parameter :: dpZERO = 0.0_c_double
  real (kind=c_float), parameter :: rNEAR_ZERO = 1E-9_c_float
  real (kind=c_float), parameter :: rPOINT2 = 0.2_c_float
  real (kind=c_float), parameter :: rHALF = 0.5_c_float
  real (kind=c_float), parameter :: dpHALF = 0.5_c_double
  real (kind=c_float), parameter :: rPOINT8 = 0.8_c_float
  real (kind=c_float), parameter :: rONE = 1.0_c_float
  real (kind=c_double), parameter :: dpONE = 1.0_c_double
  real (kind=c_float), parameter :: rFREEZING = 32.0_c_float
  real (kind=c_double), parameter :: dpFREEZING = 32.0_c_double
  real (kind=c_float), parameter :: rTEN = 10.0_c_float
  real (kind=c_float), parameter :: rHUNDRED = 100.0_c_float
  real (kind=c_float), parameter :: rTHOUSAND = 1000.0_c_float
  real (kind=c_double), parameter :: dpC_PER_F = 5.0_c_double / 9.0_c_double
  real (kind=c_double), parameter :: dpF_PER_C = 9.0_c_double / 5.0_c_double
  real (kind=c_float), parameter :: rM_PER_FOOT = 0.3048_c_float
  real (kind=c_float), parameter :: rMM_PER_INCH = 25.4_c_float
  real (kind=c_float), parameter :: rCM_PER_INCH = 2.54_c_float
  real (kind=c_double), parameter :: dpPI = 3.141592653589793_c_double
  real (kind=c_double), parameter :: dpPI_OVER_180 = dpPI / 180_c_double
  real (kind=c_double), parameter :: dpTWOPI = 2.0_c_double * dpPI
  real (kind=c_double), parameter :: dpSQM_to_SQFT = 10.76391_c_double
  real (kind=c_float), parameter :: rMINIMUM_SOIL_MOISTURE = 0.51_c_float
  integer (kind=c_short), parameter :: iACTIVE_CELL = 1
  integer (kind=c_short), parameter :: iINACTIVE_CELL = 0
  integer (kind=c_int), parameter :: iMOVING_AVG_TERMS = 5
  integer (kind=c_int), parameter :: iROUTE_CELL_MARKED = -1
  integer (kind=c_int), parameter :: iROUTE_DEPRESSION = -999
  integer (kind=c_int), parameter :: iROUTE_LEFT_GRID = -1000
  integer (kind=c_int), parameter :: iNO_DATA_NCDC = -99999
  integer (kind=c_int), parameter :: iNUM_DIGITS = 3
  integer (kind=c_int), parameter :: iFIELD_WIDTH = 10
  integer (kind=c_int), parameter :: iZERO = 0
  logical (kind=c_bool), parameter :: lTRUE = .true._c_bool
  logical (kind=c_bool), parameter :: lFALSE = .false._c_bool
  integer(kind=c_int), parameter :: iEOF = HUGE(iZERO)
  real (kind=c_float), parameter :: rBIGVAL = HUGE(rZERO)
  character (len=1), parameter :: sTAB = achar(9)
  character (len=2), parameter :: sWHITESPACE = achar(9)//" "
  character (len=1), parameter :: sBACKSLASH = achar(92)
  character (len=1), parameter :: sFORWARDSLASH = achar(47)
  character (len=1), parameter :: sRETURN = achar(13)
  !> @}

  !> @name Constants (flow direction): General conversion factors and flags
  !> Some useful and common conversion factors, defined here
  !> to ensure consistency throughout the code
  !> @{
  integer (kind=c_short),parameter :: iDIR_DEPRESSION=0
  integer (kind=c_short),parameter :: iDIR_RIGHT=1
  integer (kind=c_short),parameter :: iDIR_DOWN_RIGHT=2
  integer (kind=c_short),parameter :: iDIR_DOWN=4
  integer (kind=c_short),parameter :: iDIR_DOWN_LEFT=8
  integer (kind=c_short),parameter :: iDIR_LEFT=16
  integer (kind=c_short),parameter :: iDIR_UP_LEFT=32
  integer (kind=c_short),parameter :: iDIR_UP=64
  integer (kind=c_short),parameter :: iDIR_UP_RIGHT=128
  !> @}

  !> @name Globals: Variables for run-length encoding operation
  !> These variables store the position of the start date and
  !> end date within the binary (stream) output file.
  !> @{
  integer(kind=c_int) :: iSTARTDATE_POS
  integer(kind=c_int) :: iENDDATE_POS
  integer(kind=c_int) :: iENDHEADER_POS
  !> @}


  !> @name Constants: Definitions for ANSI.sys-like text colors
  !> @{
  character (len=7), parameter :: sRED = char(27)//'[1;31m'
  character (len=7), parameter :: sGREEN =char(27)//'[1;32m'
  character (len=7), parameter :: sYELLOW = char(27)//'[1;33m'
  character (len=7), parameter :: sBLUE = char(27)//'[1;34m'
  character (len=7), parameter :: sCYAN = char(27)//'[1;36m'
  character (len=7), parameter :: sWHITE = char(27)//'[0;37m'
  character (len=7), parameter :: sBOLDWHITE = char(27)//'[1;37m'
  character (len=7), parameter :: sCLRSCR = char(27)//'[2J'
  !> @}


!> @brief Type that contains the data for an individual grid cell.
!>
!> Type that contains data for an individual grid cell. The model uses
!> a grid of T_CELL types. Each variable added to this data type
!> consumes Ny * Nx * size(c_float) bytes.
  type T_CELL
      integer (kind=c_int) :: iFlowDir = iZERO    ! Flow direction from flow-dir grid
      integer (kind=c_int) :: iSoilGroup = iZERO  ! Soil type from soil-type grid
      integer (kind=c_int) :: iLandUseIndex       ! Index (row num) of land use table
      integer (kind=c_int) :: iLandUse = iZERO    ! Land use from land-use grid
      integer (kind=c_int) :: iIrrigationTableIndex = iZERO  ! Index (row num) of irrigation table
      real (kind=c_float) :: rElevation =rZERO            ! Ground elevation
      real (kind=c_float) :: rSoilWaterCapInput = rZERO   ! Soil water capacity from grid file
      real (kind=c_float) :: rSoilWaterCap =rZERO         ! Soil water capacity adjusted for LU/LC
      real (kind=c_float) :: rSoilMoisture = rZERO        ! Soil moisture in inches of water
			real (kind=c_float) :: rCurrentRootingDepth = 0.2   ! Current rooting depth for use w FAO56 calculations
			real (kind=c_float) :: rKcb                         ! crop coefficient for this cell
			real (kind=c_float) :: rTotalAvailableWater = rZERO
			real (kind=c_float) :: rReadilyAvailableWater = rZERO

      real (kind=c_float) :: rSoilMoisturePct = rZERO        ! Soil moisture as percentage of water capacity
      real (kind=c_float) :: rSM_AccumPotentWatLoss = rZERO  ! Accumulated potential water loss

!      real (kind=c_float) :: rMaxRecharge             ! Maximum groundwater recharge rate

      real (kind=c_float) :: rReferenceET0 = rZERO       ! Reference ET0, presumably alfalfa
      real (kind=c_float) :: rReferenceET0_adj = rZERO        ! ADJUSTED crop ET
      real (kind=c_float) :: rCropETc = rZERO            ! unadjusted crop ET
      real (kind=c_float) :: rBareSoilEvap = rZERO
      real (kind=c_float) :: rActualET = rZERO           ! Actual Evapotranspiration

!if_defined STREAM_INTERACTIONS
      integer (kind=c_int) :: iStreamIndex = iZERO  ! ID of the fracture capture lookup table
      real (kind=c_float) :: rStreamCapture           ! Amount of water captured by the stream (or fracture)
!end_if

      integer (kind=c_int) :: iTgt_Row   ! Row: "i" index of target cell into which runoff flows
      integer (kind=c_int) :: iTgt_Col   ! Col: "j" index of target cell into which runoff flows

      real (kind=c_float) :: rBaseCN                 ! Curve number from landuse/soil group
      real (kind=c_float) :: rAdjCN                  ! Curve number, adjusted for antecedent moisture
!      real (kind=c_float) :: rSMax                   ! S_max parameter from runoff calculation
      real (kind=c_float) :: rInFlow = rZERO         ! flow in from uphill
      real (kind=c_float) :: rOutFlow = rZERO        ! flow out downhill
      real (kind=c_float) :: rFlowOutOfGrid = rZERO  ! flow that leaves the grid
      real (kind=c_float) :: rRouteFraction = rONE   ! Fraction of outflow to route downslope
      real (kind=c_float) :: rGrossPrecip = rZERO    ! Precip - no interception applied
      real (kind=c_float) :: rNetPrecip
      real (kind=c_float) :: rInterception = rZERO   ! Interception term
      real (kind=c_float) :: rInterceptionStorage = rZERO ! This is a reservoir to hold intercepted moisture
      real (kind=c_float) :: rNetRainfall = rZERO    ! Net precipitation - precip minus interception
      real (kind=c_float) :: rSnowFall_SWE = rZERO   ! precipitation that falls as snow (in SWE)
      real (kind=c_float) :: rSnowFall = rZERO       ! snowfall in inches as SNOW
      real (kind=c_float) :: rSnowCover = rZERO      ! snowcover expressed as inches of water
      real (kind=c_float) :: rSnowTemperature = 23. ! snow temperature
!      real (kind=c_float) :: rPrevious_SnowCover     ! Previous day's snow cover
      real (kind=c_float) :: rSnowMelt = rZERO       ! snowmelt in inches of water
      real (kind=c_float) :: rTMin = rZERO           ! Minimum daily temperature
      real (kind=c_float) :: rTMax = rZERO           ! Maximum daily temperature
      real (kind=c_float) :: rTAvg = rZERO           ! Average daily temperature
      real (kind=c_float) :: rCFGI = rZERO           ! Continuous Frozen Ground Index

      real (kind=c_float) :: rGDD = rZERO            ! Growing Degree Day
      real (kind=c_float) :: rIrrigationAmount = rZERO ! total amount of any irrigation
      real (kind=c_float) :: rIrrigationFromGW = rZERO ! term to hold irrigation term, if any
      real (kind=c_float) :: rIrrigationFromSW = rZERO ! term to hold irrigation term, if any
!      real (kind=c_float) :: rMaximumAllowableDepletion = 100_c_float ! by default, no irrigation
                                                                  ! will be performed

      real (kind=c_float) :: rSnowAlbedo             ! Snow albedo value
      integer (kind=c_int) :: iDaysSinceLastSnow = iZERO  ! Number of days since last snowfall
!      real (kind=c_float) :: rNetInfil               ! NetPrecip + InFlow + SnowMelt - OutFlow
      real (kind=c_float),dimension(iMOVING_AVG_TERMS) :: rNetInflowBuf  ! Inflow buffer for moving avg
      real (kind=c_float) :: rDailyRecharge = rZERO  ! Daily recharge
      real (kind=c_float) :: rSUM_Recharge = rZERO   ! SUM of all daily recharge values for entire run
      real (kind=c_float) :: rSUM_RejectedRecharge = rZERO   ! SUM of all daily rejected recharge values for entire run
      real (kind=c_float) :: rMSB = rZERO            ! cellular mass balance
      integer(kind=c_short) :: iNumFilesSSF = 0    ! number of SSF files associated with grid cell

      logical (kind=c_bool) :: lDownhillMarked = lFALSE  ! Has been marked for downhill solution
  end type T_CELL

  ! Generic grid data type identifier constants
  integer (kind=c_int), parameter :: CONSTANT_GRID = 0
  integer (kind=c_int), parameter :: STATIC_GRID = 1
  integer (kind=c_int), parameter :: STATIC_NETCDF_GRID = 2
  integer (kind=c_int), parameter :: DYNAMIC_GRID = 3
  integer (kind=c_int), parameter :: DYNAMIC_NETCDF_GRID = 4

  integer (kind=c_int), parameter :: DATATYPE_INT = 0
  integer (kind=c_int), parameter :: DATATYPE_REAL = 1
  integer (kind=c_int), parameter :: DATATYPE_CELL_GRID = 2
  integer (kind=c_int), parameter :: DATATYPE_SHORT = 3
  integer (kind=c_int), parameter :: DATATYPE_DOUBLE = 4

  integer (kind=c_int), parameter :: FILETYPE_ARC_ASCII = 0
  integer (kind=c_int), parameter :: FILETYPE_SURFER = 1
  integer (kind=c_int), parameter :: FILETYPE_NETCDF = 2
  integer (kind=c_int), parameter :: FILETYPE_NONE = 3

  integer (kind=c_int), parameter :: GRID_DATATYPE_INT = 0
  integer (kind=c_int), parameter :: GRID_DATATYPE_REAL = 1
  integer (kind=c_int), parameter :: GRID_DATATYPE_CELL_GRID = 2
  integer (kind=c_int), parameter :: GRID_DATATYPE_ALL = 3

!> @brief Type that contains the data for a grid.
!>
!>   Type that contains the data for a grid. Depending on the need of the
!>   application code, the grid may contain integer, real, or cell-by-cell
!>   data. This 'generic' data type makes many data validation problems
!>   simpler, but at the cost of two unpopulated pointer variables per grid.
!>   This implements a coding mechanism comparable to templates in C++.!
  type T_GENERAL_GRID
!      integer (kind=c_int) :: iGridType            ! One of the grid type options above
      integer (kind=c_int) :: iNX                   ! Number of cells in the x-direction
      integer (kind=c_int) :: iNY                   ! Number of cells in the y-direction
      integer (kind=c_int) :: iNumGridCells         ! Total number of grid cells
      integer (kind=c_int) :: iDataType             ! Data type contained in the grid (integer, real, SWB cell)
      character (len=256)  :: sProj4_string         ! proj4 string defining coordinate system of grid
      character (len=256)  :: sFilename             ! original file name that the data was read from
      real (kind=c_double)    :: rGridCellSize         ! size of one side of a grid cell
      integer (kind=c_int) :: iLengthUnits= -99999  ! length units code
      real (kind=c_double)    :: rX0, rX1              ! World-coordinate range in X
      real (kind=c_double)    :: rY0, rY1              ! World-coordinate range in Y
      integer (kind=c_int), dimension(:,:), pointer :: iData ! Integer data
      integer (kind=c_int) :: iNoDataValue = -9999
      real (kind=c_float), dimension(:,:), pointer :: rData    ! Real data
      real (kind=c_float) :: rNoDataValue
      real (kind=c_double), dimension(:,:), allocatable :: rX    ! x coordinate associated with data
      real (kind=c_double), dimension(:,:), allocatable :: rY    ! y coordinate associated with data
      type (T_CELL), dimension(:,:), pointer :: Cells        ! T_CELL objects
  end type T_GENERAL_GRID

  !> define parameter values for working with type T_GENERAL_GRID
  integer (kind=c_int), parameter :: iGRID_LENGTH_UNITS_METERS = 0
  integer (kind=c_int), parameter :: iGRID_LENGTH_UNITS_FEET = 1

  type T_GRID_BOUNDS
    real (kind=c_double) :: rXll, rYll
    real (kind=c_double) :: rXul, rYul
    real (kind=c_double) :: rXlr, rYlr
    real (kind=c_double) :: rXur, rYur
    character (len=256) :: sPROJ4_string
  end type T_GRID_BOUNDS

!> @brief Type that contains pointers to one or more grid data structures.
!>
!> Type that contains pointers to one or more grid data structures.
  type T_GRID_COLLECTION
    type (T_GENERAL_GRID), dimension(:), pointer :: Grids
  end type T_GRID_COLLECTION

  !> @brief Container for land-use related data
  !>
  !> Container for land-use related data. Includes curve number
  !> and rooting-depth generalizations.
  type T_LANDUSE_LOOKUP

    !> Land use type; values are expected to correspond to those provided
    !> by the user in the input landuse grid.
	integer (kind=c_int) :: iLandUseType

    !> Land use description
    character (len=256) :: sLandUseDescription

    !> Assumed percent imperviousness (not used in any calculations)
	character (len=256) :: sAssumedPercentImperviousness

    !> Interception value (inches per day) during growing season
	real (kind=c_float) :: rIntercept_GrowingSeason

    !> Interception value (inches per day) outside of growing season
	real (kind=c_float) :: rIntercept_NonGrowingSeason

  end type T_LANDUSE_LOOKUP

  !> @brief Type that contains information needed to calculate irrigation for
  !> each land use.
  !>
  !> Type that contains information needed to estimate crop coefficients and
  !> calculate irrigation for each land use.
  type T_IRRIGATION_LOOKUP

    !> Landuse code corresponding to the codes specified in landuse grid
 	integer (kind=c_int) :: iLandUseType

    !> Land use description
    character (len=256) :: sLandUseDescription

    !> Plant or crop description
    character (len=256) :: sCropDescription

    !> Mean plant or crop height, feet
    real (kind=c_float) :: rMeanPlantHeight = 3.0

    !> Crop coefficient, basal, for a given day (calculated in code)
    real (kind=c_float) :: rKcb

    !> Crop coefficient, basal, initial growth phase (Kcb_ini)
    real (kind=c_float) :: rKcb_ini = 0.25

    !> Crop coefficient, basal, mid-growth phase (Kcb_mid)
    real (kind=c_float) :: rKcb_mid = 1.3

    !> Crop coefficient, basal, end-growth phase (Kcb_end)
    real (kind=c_float) :: rKcb_end = 0.7

    !> Crop coefficient, MINIMUM allowed value (Kc_min)
    real (kind=c_float) :: rKcb_min = 0.1

    !> Day of year (or GDD) for initial planting
    integer (kind=c_int) :: iL_plant = 50

    !> Day of year (or GDD) for end of initial growth phase
    integer (kind=c_int) :: iL_ini = 300

    !> Day of year (or GDD) for end of development phase
    integer (kind=c_int) :: iL_dev = 650

    !> Day of year (or GDD) for end of mid-season growth phase
    integer (kind=c_int) :: iL_mid = 1300

    !> Day of year (or GDD) for end of late season growth phase
    integer (kind=c_int) :: iL_late = 1800

    !> How should the growth phase identifiers be treated (DOY or GDD)
    logical (kind=c_bool) :: lUnitsAreDOY = lFALSE

    !> Growing degree-day base temperature (10 degrees C for corn)
    real (kind=c_float) :: rGDD_BaseTemp  = 50.

    !> Growing degree-day maximum temperature (cutoff; 30 degrees C for corn)
    real (kind=c_float) :: rGDD_MaxTemp   = 130.

    ! Depletion fraction: fraction of soil moisture depletion beyond which
    ! significant plant stress results
    real (kind=c_float) :: rDepletionFraction = 0.2

    ! Maximum allowable depletion (MAD) = maximum allowed soil water depletion (as fraction)
    real (kind=c_float) :: rMAD           = 1.

    !> Day of year before which no irrigation is assumed to take place
    integer (kind=c_int) :: iBeginIrrigation = 120

    !> Day of year after which no irrigation is assumed to take place
    integer (kind=c_int) :: iEndIrrigation = 240

    !> Fraction of irrigation water obtained from GW rather than surface water
    real (kind=c_float) :: rFractionOfIrrigationFromGW = rONE

    !> Irrigation efficiency, surface-water sources
    real (kind=c_float) :: rIrrigationEfficiency_SW = 1.0 / 0.8

    !> Irrigation efficiency, groundwater sources
    real (kind=c_float) :: rIrrigationEfficiency_GW = 1.0 / 0.8

  end type T_IRRIGATION_LOOKUP

  !> container for basin mask table data
  type T_BASIN_MASK
    character(len=256) :: sUSGS_UpstreamOrderID
    character(len=256) :: sBasinDescription
    character(len=256) :: sBasinMaskFilename
    character(len=256) :: sFileType
    real (kind=c_float) :: rPestWeight = 1.0
    character (len=256) :: sPestGroup

    type(T_GENERAL_GRID), pointer :: pGrd

    real (kind=c_float) :: rQb
    real (kind=c_float) :: rDrainageArea
    integer (kind=c_int) :: iLENGTH
    real (kind=c_float) :: rMIN
    real (kind=c_float) :: rMAX
    real (kind=c_float) :: rMEAN
  end type T_BASIN_MASK

  !> container for SSF file information
  type T_SSF_FILES
    integer (kind=c_int) :: iLU      ! Fortran logical unit #
    character (len=128) :: sFileName
    integer (kind=c_int) :: iRowNum
    integer (kind=c_int) :: iColNum
    integer (kind=c_int) :: iVarNum  ! T_STATS variable number
  end type T_SSF_FILES


  !> Container for calendar lookup information
  type T_MONTH
    ! Container for calendar lookup information
    character (len=3) :: sName          ! Abbreviated name
	character (len=9) :: sFullName      ! Full month name
    integer (kind=c_int) :: iStart      ! Starting (Julian) date
    integer (kind=c_int) :: iEnd        ! Ending (Julian) date
	integer (kind=c_int) :: iMonth      ! Month number (1-12)
    integer (kind=c_int) :: iNumDays    ! Max number of days in month
  end type T_MONTH

  !> Month information
  type ( T_MONTH ),dimension(12),target :: YEAR_INFO = (/ &
      T_MONTH( 'JAN','JANUARY  ',   1,  31, 1, 31), &
      T_MONTH( 'FEB','FEBRUARY ',  32,  59, 2, 29), &
      T_MONTH( 'MAR','MARCH    ',  60,  90, 3, 31), &
      T_MONTH( 'APR','APRIL    ',  91, 120, 4, 30), &
      T_MONTH( 'MAY','MAY      ', 121, 151, 5, 31), &
      T_MONTH( 'JUN','JUNE     ', 152, 181, 6, 30), &
      T_MONTH( 'JUL','JULY     ', 182, 212, 7, 31), &
      T_MONTH( 'AUG','AUGUST   ', 213, 243, 8, 31), &
      T_MONTH( 'SEP','SEPTEMBER', 244, 273, 9, 30), &
      T_MONTH( 'OCT','OCTOBER  ', 274, 304, 10, 31), &
      T_MONTH( 'NOV','NOVEMBER ', 305, 334, 11, 30), &
      T_MONTH( 'DEC','DECEMBER ', 335, 365, 12, 31) /)

  ! Global buffer for writing log messages
  character (len=256), public :: sLogBuffer

  !> @brief Type that contains parameters for graphings and stats calculations.
  !>
  !> This type contains parameters that determine how the main state variables
  !> and other important ancillary variables are tabulated and graphed.

  type T_STATS
    !> Short version of the variable name
    character (len=20) :: sVARIABLE_NAME
    !> Fortran logical unit for binary file writes
    integer(kind=c_int) :: iLU
    !> Integer number of characters to indent (on-screen display)
    integer (kind=c_int) :: iIndent
    !> Multiplier for calculating mass balances (1=source, 0=NA, -1=sink)
    integer(kind=c_int) :: iMassBalanceConst
    !> Does it make sense to show the sum of this accumulator?
    logical(kind=c_bool) :: lShowSum
    !> Does it make sense to show the daily sum of this accumulator?
    logical(kind=c_bool) :: lShowDailySum
    !> Is this variable active given the compiler options?
    logical (kind=c_bool) :: lActive
    !> Text describing the unit of measure (i.e. inches)
    character (len=20) :: sUnits
    !> Long form of the variable name
    character(len=64) :: sLongName
    !> Multiplication factor when writing to a NetCDF file
    real (kind=c_double) :: rNC_MultFactor
    !> Offset when writing to a NetCDF file
    real (kind=c_double) :: rNC_AddOffset
    !> Type of daily output desired (0=none; 1=grid; 2=plot; 3=both; 4=stats)
    integer(kind=c_int) :: iDailyOutput
    !> Type of monthly output desired (0=none; 1=grid; 2=plot; 3=both; 4=stats)
    integer(kind=c_int) :: iMonthlyOutput
    !> Type of annual output desired (0=none; 1=grid; 2=plot; 3=both; 4=stats)
    integer(kind=c_int) :: iAnnualOutput
    !> Is output to be written to a NetCDF file?
    integer(kind=c_int) :: iNetCDFOutput
    !> Description of utility of variable in calculating mass balance
    character (len=24) :: sMSB_Note
    !> Offset value; contains number of bytes written for a given day
    integer (kind=c_int) :: iOffset
    !> Position value; location of current day's position marker
    integer (kind=c_int) :: iPos
  end type T_STATS

  !> Global parameter defining the number of elements in the YEAR_INFO array.
  integer (kind=c_int), parameter :: iNUM_MONTHS = 12
  integer(kind=c_int), parameter :: iNUM_VARIABLES = 36

  ! constants defining T_STATS output types
  integer(kind=c_int), parameter :: iNONE = 0
  integer(kind=c_int), parameter :: iGRID = 1
  integer(kind=c_int), parameter :: iGRAPH = 2
  integer(kind=c_int), parameter :: iBOTH = 3
  integer(kind=c_int), parameter :: iSTATS = 4

  !> Define parameter values for working with accumulator arrays
  type ( T_STATS ), dimension(iNUM_VARIABLES) :: STAT_INFO = [ &

    ! FIRST come the SOURCES (+) in the mass balance...
    T_STATS ('GROSS_PRECIP',0,0,1,lTRUE,lTRUE, lTRUE, &
      'inches','gross precipitation', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'source',0,0), &

    T_STATS ('SNOWMELT',0,0,1,lTRUE,lTRUE, lTRUE, &
       'inches','snowmelt', &
        1.,0.0,iNONE,iNONE,iNONE,iNONE,'source',0,0), &

    T_STATS ('INFLOW',0,0,1,lTRUE,lTRUE, lTRUE, &
      'inches','incoming flow from adjacent cells', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'source',0,0), &

    T_STATS ('IRRIGATION_AMOUNT',0,0,1,lTRUE,lTRUE, &
      lTRUE, 'inches','daily estimated irrigation amount', &
        1.,0.0,iNONE,iNONE,iNONE,iNONE,'source',0,0), &

    ! NOW make room for the SINKS (-) in the mass balance...
    T_STATS ('SNOWFALL',0,0,-1,lTRUE,lTRUE, lTRUE, &
      'inches','precipitation falling as snow (SWE)', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'sink',0,0), &

    T_STATS ('INTERCEPTION',0,0,-1,lTRUE,lTRUE, lTRUE, &
      'inches','interception', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'sink',0,0), &

    T_STATS ('OUTFLOW',0,0,-1,lTRUE,lTRUE, lTRUE, &
      'inches','outgoing flow to adjacent cells', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'sink',0,0), &

    T_STATS ('RUNOFF_OUTSIDE',0,0,-1,lTRUE,lTRUE, lTRUE, &
      'inches','runoff leaving model domain', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'sink',0,0), &

    T_STATS ('ACT_ET',0,0,-1,lTRUE,lTRUE, lTRUE, &
      'inches','actual evapotranspiration', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'sink',0,0), &

    T_STATS ('CHG_IN_SOIL_MOIS',0,0,-1,lTRUE,lTRUE, lTRUE, &
      'inches','daily change in soil moisture', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'sink',0,0), &

    T_STATS ('RECHARGE',0,0,-1,lTRUE,lTRUE, lTRUE, &
      'inches','daily potential recharge', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'sink',0,0), &

    T_STATS ('REJECTED_RECHARGE',0,0,-1,lTRUE,lTRUE, lTRUE, &
      'inches','recharge exceeding max recharge rate', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'sink',0,0), &

    T_STATS ('STREAM_CAPTURE',0,0,-1,lTRUE,lTRUE, &
#ifdef STREAM_INTERACTIONS
      lTRUE, &
#else
      lFALSE, &
#endif
        'inches','runoff or recharge captured by a stream or fracture', &
        1.,0.0,iNONE,iNONE,iNONE,iNONE,'sink',0,0), &

    ! The following items are tracked and provided as outputs
    ! but are not part of the mass balance calculation...
    T_STATS ('SNOWCOVER',0,2,0,lFALSE,lTRUE, lTRUE, &
      'inches','water equivalent of snow cover', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('CFGI',0,2,0,lFALSE,lFALSE, lTRUE, &
      'unitless','continuous frozen ground index', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('MIN_TEMP',0,2,0,lFALSE,lFALSE, lTRUE, &
      'degrees F','minimum daily air temperature', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('MAX_TEMP',0,2,0,lFALSE,lFALSE, lTRUE, &
      'degrees F','maximum daily air temperature', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('AVG_TEMP',0,2,0,lFALSE,lFALSE, lTRUE, &
      'degrees F','mean daily air temperature', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('CHG_IN_SNOW_COV',0,2,0,lFALSE,lTRUE, lTRUE, &
      'inches','snowfall minus snowmelt', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('NET_RAINFALL',0,2,0,lTRUE,lTRUE, lTRUE, &
      'inches','gross precipitation minus interception and snowfall', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('NET_INFLOW',0,2,0,lTRUE,lTRUE, lTRUE, &
      'inches','sum of net precip and inflow', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('NET_INFIL',0,2,0,lTRUE,lTRUE, lTRUE, &
      'inches','precip and inflow minus outflow', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('REFERENCE_ET',0,2,0,lTRUE,lTRUE, lTRUE, &
      'inches','reference evapotranspiration (ET0)', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('REFERENCE_ET_ADJ',0,2,0,lTRUE,lTRUE, lTRUE, &
      'inches','adjusted reference evapotranspiration (ET0_adj)', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('CROP_ET',0,2,0,lTRUE,lTRUE, lTRUE, &
      'inches','crop evapotranspiration (ETc)', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('BARE_SOIL_EVAP',0,2,0,lTRUE,lTRUE, lTRUE, &
      'inches','evaporation from bare soil', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('P_MINUS_PET',0,2,0,lTRUE,lFALSE, lTRUE, &
      'inches','net inflow minus potential et', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('SM_DEFICIT',0,2,0,lFALSE,lFALSE, lTRUE, &
      'inches','daily soil moisture deficit', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('SM_SURPLUS',0,2,0,lFALSE,lFALSE, lTRUE, &
      'inches','daily soil moisture surplus', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('SM_APWL',0,2,0,lFALSE,lFALSE, lTRUE, &
      'inches','accumulated potential water loss', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('SOIL_MOISTURE',0,2,0,lFALSE,lTRUE, lTRUE, &
      'inches','daily soil moisture', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('GDD',0,2,0,lFALSE,lFALSE,lTRUE, &
      'degree-day','growing degree day', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('ROOTING_DEPTH',0,2,0,lFALSE,lFALSE,lTRUE, &
      'feet','current rooting depth', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('CROP_COEFFICIENT',0,2,0,lFALSE,lFALSE,lTRUE, &
      'unitless','crop coefficient (Kcb)', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('IRRIGATION_FROM_GW',0,2,0,lFALSE,lFALSE, lTRUE, &
      'inches','amount of water required from groundwater for irrigation', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('IRRIGATION_FROM_SW',0,2,0,lFALSE,lFALSE, lTRUE, &
      'inches','amount of water required from surface water for irrigation', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0) &
    ]

  !> @anchor const_stat
  !> @name Constants: Statistics that SWB knows how to calculate and store
  !> Index values for members of the rMonthly, rDaily, and rAnnual arrays
  !> that are global variables within module stats
  !> @{
  integer (kind=c_int), parameter :: iNUM_STATS = 5
  integer (kind=c_int), parameter :: iMIN = 1
  integer (kind=c_int), parameter :: iMEAN = 2
  integer (kind=c_int), parameter :: iMAX = 3
  integer (kind=c_int), parameter :: iSUM = 4
  integer (kind=c_int), parameter :: iLENGTH = 5
  character(len=7), dimension(iNUM_STATS), parameter :: STAT_NAMES = &
    [ "Minimum", "Mean   ", "Maximum", "Sum    ", "Length " ]
  !> @}

  !> @name Constants: Variables that SWB can output or summarize
  !> Index values for members of the rMonthly, rDaily, and rAnnual arrays
  !> that are global variables within module stats
  !> @note These constants are arranged in the desired order of output
  !> for daily and annual reporting: sources first, then the sinks,
  !> and then all other informational items
  !> @{
  integer (kind=c_int), parameter :: iGROSS_PRECIP = 1
  integer (kind=c_int), parameter :: iSNOWMELT = 2
  integer (kind=c_int), parameter :: iINFLOW = 3
  integer (kind=c_int), parameter :: iIRRIGATION = 4
  integer (kind=c_int), parameter :: iSNOWFALL_SWE = 5
  integer (kind=c_int), parameter :: iINTERCEPTION = 6
  integer (kind=c_int), parameter :: iOUTFLOW = 7
  integer (kind=c_int), parameter :: iRUNOFF_OUTSIDE = 8
  integer (kind=c_int), parameter :: iACT_ET = 9
  integer (kind=c_int), parameter :: iCHG_IN_SOIL_MOIST = 10
  integer (kind=c_int), parameter :: iRECHARGE = 11
  integer (kind=c_int), parameter :: iREJECTED_RECHARGE = 12
  integer (kind=c_int), parameter :: iSTREAM_CAPTURE = 13
  integer (kind=c_int), parameter :: iSNOWCOVER = 14
  integer (kind=c_int), parameter :: iCFGI = 15
  integer (kind=c_int), parameter :: iMIN_TEMP = 16
  integer (kind=c_int), parameter :: iMAX_TEMP = 17
  integer (kind=c_int), parameter :: iAVG_TEMP = 18
  integer (kind=c_int), parameter :: iCHG_IN_SNOW_COV = 19
  integer (kind=c_int), parameter :: iNET_RAINFALL = 20
  integer (kind=c_int), parameter :: iNET_INFLOW = 21
  integer (kind=c_int), parameter :: iNET_INFIL = 22
  integer (kind=c_int), parameter :: iREFERENCE_ET = 23
  integer (kind=c_int), parameter :: iREFERENCE_ET_ADJ = 24
  integer (kind=c_int), parameter :: iCROP_ET = 25
  integer (kind=c_int), parameter :: iBARE_SOIL_EVAP = 26
  integer (kind=c_int), parameter :: iP_MINUS_PET = 27
  integer (kind=c_int), parameter :: iSM_DEFICIT = 28
  integer (kind=c_int), parameter :: iSM_SURPLUS = 29
  integer (kind=c_int), parameter :: iSM_APWL = 30
  integer (kind=c_int), parameter :: iSOIL_MOISTURE = 31
  integer (kind=c_int), parameter :: iGDD = 32
  integer (kind=c_int), parameter :: iROOTING_DEPTH = 33
  integer (kind=c_int), parameter :: iCROP_COEFFICIENT = 34
  integer (kind=c_int), parameter :: iIRRIGATION_FROM_GW = 35
  integer (kind=c_int), parameter :: iIRRIGATION_FROM_SW = 36

#ifdef STREAM_INTERACTIONS
  ! The maximum number of fracture recharge entries
  integer (kind=c_int), parameter :: STREAM_INTERACTIONS_MAX = 100
#endif

  !> @anchor const_runoffCalc
  !> @name Constants: Runoff calculation
  !> Options controlling the selection of a runoff calculation algorithm
  !> @{
  integer (kind=c_int),parameter :: CONFIG_RUNOFF_CURVE_NUMBER = 0
  integer (kind=c_int),parameter :: CONFIG_RUNOFF_GREEN_AMPT = 1
  !> @}

  !> @anchor const_runoffSoln
  !> @name Constants: Runoff routing calculation
  !> Options for routing mechanism selection
  !> @{
  integer (kind=c_int),parameter :: CONFIG_RUNOFF_ITERATIVE = 0
  integer (kind=c_int),parameter :: CONFIG_RUNOFF_DOWNHILL = 1
  integer (kind=c_int),parameter :: CONFIG_RUNOFF_NO_ROUTING = 2
  !> @}


  !> @name Constants: Evapotranspiration algorithm
  !> Options for specifying the choice of evapotranspiration algorithm
  !> @{
  integer (kind=c_int),parameter :: CONFIG_ET_NONE = 0
  integer (kind=c_int),parameter :: CONFIG_ET_THORNTHWAITE_MATHER = 1
  integer (kind=c_int),parameter :: CONFIG_ET_TURC = 2
  integer (kind=c_int),parameter :: CONFIG_ET_JENSEN_HAISE = 3
  integer (kind=c_int),parameter :: CONFIG_ET_BLANEY_CRIDDLE = 4
  integer (kind=c_int),parameter :: CONFIG_ET_HARGREAVES = 5
  !> @}

  !> @name Constants: Precipitation data format
  !> Options for specifying the method of input for precipitation data
  !> @{
  integer (kind=c_int),parameter :: CONFIG_PRECIP_SINGLE_STATION = 0
  integer (kind=c_int),parameter :: CONFIG_PRECIP_ARC_GRID = 1
  integer (kind=c_int),parameter :: CONFIG_PRECIP_SURFER_GRID = 2
  integer (kind=c_int),parameter :: CONFIG_PRECIP_NETCDF = 3
  !> @}

  !> @name Constants: Temperature data format
  !> Options for specifying the method of input for temperature data
  !> @{
  integer (kind=c_int),parameter :: CONFIG_TEMPERATURE_SINGLE_STATION = 0
  integer (kind=c_int),parameter :: CONFIG_TEMPERATURE_ARC_GRID = 1
  integer (kind=c_int),parameter :: CONFIG_TEMPERATURE_SURFER_GRID = 2
  integer (kind=c_int),parameter :: CONFIG_TEMPERATURE_NETCDF = 3
  !> @}

  !> @name Constants: Landuse input data format
  !> Options for specifying the method of input for temperature data
  !> @{
  integer (kind=c_int),parameter :: CONFIG_LANDUSE_CONSTANT = 0
  integer (kind=c_int),parameter :: CONFIG_LANDUSE_DYNAMIC_ARC_GRID = 1
  integer (kind=c_int),parameter :: CONFIG_LANDUSE_DYNAMIC_SURFER = 2
  integer (kind=c_int),parameter :: CONFIG_LANDUSE_DYNAMIC_NETCDF = 3
  integer (kind=c_int),parameter :: CONFIG_LANDUSE_STATIC_GRID = 4
  !> @}

  !> @name Constants: Snow module
  !> Configuration for selection of snowfall and snowmelt modules
  !> @{
  integer (kind=c_int),parameter :: CONFIG_SNOW_ORIGINAL_SWB = 0
  integer (kind=c_int),parameter :: CONFIG_SNOW_NEW_SWB = 1
  !> @}

  !> @name Constants: Soil-moisture input data format
  !> Configuration information for soil-moisture capacity calculations
  !> @{
  integer (kind=c_int),parameter :: CONFIG_SM_CAPACITY_CALCULATE = 0
  integer (kind=c_int),parameter :: CONFIG_SM_CAPACITY_CONSTANT = 1
  integer (kind=c_int),parameter :: CONFIG_SM_CAPACITY_FM_TABLE = 2
  !> @}

  !> @name Constants: Soil-moisture calculation
  !> Configuration information for soil-moisture retention calculations
  !> @{
  integer (kind=c_int),parameter :: CONFIG_SM_NONE = 0
  integer (kind=c_int),parameter :: CONFIG_SM_THORNTHWAITE_MATHER = 1
  !> @}

  !> @name Constants: Thornthwaite-Mather implementation method
  !> Configuration information for Thornthwaite-Mather SM retention
  !> @{
  integer (kind=c_int),parameter :: CONFIG_TM_NONE = 0
  integer (kind=c_int),parameter :: CONFIG_TM_LOOKUP_TABLE = 1
  integer (kind=c_int),parameter :: CONFIG_TM_EQUATIONS = 2
  !> @}

  !> @name Constants: FAO56 module
  !> Configuration information for FAO56 calculations
  !> @{
  integer (kind=c_int), parameter :: CONFIG_FAO56_NONE = 0
  integer (kind=c_int), parameter :: CONFIG_FAO56_ONE_FACTOR_STANDARD = 1
  integer (kind=c_int), parameter :: CONFIG_FAO56_ONE_FACTOR_NONSTANDARD = 2
  integer (kind=c_int), parameter :: CONFIG_FAO56_TWO_FACTOR_STANDARD = 3
  integer (kind=c_int), parameter :: CONFIG_FAO56_TWO_FACTOR_NONSTANDARD = 4

  !> @}

  !> @name Constants: SCS curve number
  !> Configuration information for initial abstraction assumptions
  !> @{
  integer (kind=c_int), parameter :: CONFIG_SM_INIT_ABSTRACTION_TR55 = 0
  integer (kind=c_int), parameter :: CONFIG_SM_INIT_ABSTRACTION_HAWKINS = 1
  !> @}

  ! Define behavior in the case of missing data [UNIMPLEMENTED]
  integer (kind=c_int), parameter :: CONFIG_ESTIMATE_MISSING_DATA = 0
  integer (kind=c_int), parameter :: CONFIG_END_IF_MISSING_DATA = 1

  !> @name Constants: Output grid format
  !> Options for output formats
  !> @{
  integer (kind=c_int),parameter :: OUTPUT_SURFER = 0
  integer (kind=c_int),parameter :: OUTPUT_ARC = 1
  !> @}

  ! Options for ASCII grid output
  integer (kind=c_int), parameter :: WRITE_ASCII_GRID_DAILY = 0
  integer (kind=c_int), parameter :: WRITE_ASCII_GRID_MONTHLY = 1
  integer (kind=c_int), parameter :: WRITE_ASCII_GRID_ANNUAL = 2
  integer (kind=c_int), parameter :: WRITE_ASCII_GRID_DEBUG = 3
  integer (kind=c_int), parameter :: WRITE_ASCII_GRID_DIAGNOSTIC = 4

  !> generic configuration
  integer (kind=c_int), parameter :: CONFIG_NONE = 0

  !> @}

  !> @brief Type that contains model configuration data.
  !>
  !> The configuration data structure is passed by pointer throughout
  !> the SWB model modules. Most of the functioning of SWB may be controlled
  !> by changing the configuration options prior to a run.
  type T_MODEL_CONFIGURATION

      !> Runoff calculation method @ref const_runoffCalc "(\em see defined constants)"
      integer (kind=c_int) :: iConfigureRunoff = CONFIG_NONE

      !> Runoff routing solution mode @ref const_runoffSoln "(\em see defined constants)"
      integer (kind=c_int) :: iConfigureRunoffMode = CONFIG_NONE

      !> Reference evapotranspiration calculation method
      integer (kind=c_int) :: iConfigureET = CONFIG_NONE

      !> Precipitation input option
      integer (kind=c_int) :: iConfigurePrecip = CONFIG_NONE

      !> Temperature data input option
      integer (kind=c_int) :: iConfigureTemperature = CONFIG_NONE

      !> Landuse data input option
      integer (kind=c_int) :: iConfigureLanduse = CONFIG_NONE

      !> Soil moisture calculation option
      integer (kind=c_int) :: iConfigureSM = CONFIG_NONE

      !> Thornthwaite-Mather soil moisture retention method
      integer (kind=c_int) :: iSoilMoistureRetentionMethod = CONFIG_TM_EQUATIONS

      !> Snowfall and snowmelt option
      integer (kind=c_int) :: iConfigureSnow = CONFIG_NONE

      !> Maximum soil water capacity option
      integer (kind=c_int) :: iConfigureSMCapacity = CONFIG_NONE

      !> FAO56 module options calculation option
      integer (kind=c_int) :: iConfigureFAO56 = CONFIG_NONE

      !> Initial abstraction method: use 0.2S or 0.05S as estimate of initial abstraction
      integer (kind=c_int) :: iConfigureInitialAbstraction = CONFIG_NONE

			!> Enable irrigation calculations?
			logical (kind=c_bool) ::lEnableIrrigation = lFALSE

      !> Option to write extra files when using PEST
      logical (kind=c_bool) :: lWriteExtraPestFiles = lFALSE

      !> flag indicating whether or not screen output should include
      !> ANSI.sys-like colors
      logical (kind=c_bool) :: lANSI_Colors = lFALSE

      !> allow for alternate methods of delimiting subdirectories;
      !> needed for operation on Windows *and* Linux platforms
      character(len=1) :: sSlash = sBACKSLASH

      integer(kind=c_int) :: iRLE_MULT = 10000
      real (kind=c_float) :: rRLE_OFFSET = 0.

      !> flag indicating whether this is the first year of a
      !> multiple-year simulation
      logical (kind=c_bool) :: lFirstYearOfSimulation  = lTRUE
      logical (kind=c_bool) :: lFirstDayOfSimulation  = lTRUE
      integer (kind=c_int) :: iStartYear
      integer (kind=c_int) :: iEndYear
      integer (kind=c_int) :: iYear           ! keep track of current
      integer (kind=c_int) :: iMonth          ! simulation date by
      integer (kind=c_int) :: iDay            ! updating these values
      integer (kind=c_int) :: iDayOfYear
      integer (kind=c_int) :: iNumDaysInYear
      integer (kind=c_int) :: iStartJulianDay
      integer (kind=c_int) :: iCurrentJulianDay
      integer (kind=c_int) :: iEndJulianDay
      integer (kind=c_int) :: iStartYearforCalculation = -99999
      integer (kind=c_int) :: iEndYearforCalculation = 99999
      integer (kind=c_int) :: iNumberOfLanduses
      integer (kind=c_int) :: iNumberOfSoilTypes
      logical (kind=c_bool) :: lGriddedData = lFALSE
      logical (kind=c_bool) :: lUseSWBRead = lFALSE
      logical (kind=c_bool) :: lHaltIfMissingClimateData = lTRUE

      ! flag indicating whether a previous downhill routing table exists
      logical (kind=c_bool) :: lDownhillRoutingTableExists = lFALSE

      ! Prefix of input ARC or SURFER gridded precip time-series files
      character (len=256) :: sPrecipFilePrefix = repeat(" ", 256)

      ! Prefix of input ARC or SURFER gridded temperature time-series files
      character (len=256) :: sTMAXFilePrefix = repeat(" ", 256)
      character (len=256) :: sTMINFilePrefix = repeat(" ", 256)

      ! Prefix of the input ARC of SURFER gridded DYNAMIC LANDUSE files
      character (len=256) :: sDynamicLanduseFilePrefix = repeat(" ", 256)

      ! ET parameters
      real (kind=c_float) :: rET_Slope = 0.0023    ! default is for Hargreaves (1985) method
      real (kind=c_float) :: rET_Exponent = 0.5
      real (kind=c_float) :: rET_Constant = 17.8

      ! precip correction factors
      real (kind=c_float) :: rRainfall_Corr_Factor = 1.0
      real (kind=c_float) :: rSnowFall_SWE_Corr_Factor = 1.0

      ! minimum value for valid precip  and temperature data
      real (kind=c_float) :: rMinValidPrecip = 0.
      real (kind=c_float) :: rMinValidTemp = -100.0

      ! define temperature values at which precip is all rain or all snow
      real (kind=c_float) :: rTMaxAllSnow = 30.5
      real (kind=c_float) :: rTMaxAllRain = 40.0

      ! SNOW DEPTH parameters for fresh snow
      real (kind=c_float) :: rSNWD_slp1 = 51.3
      real (kind=c_float) :: rSNWD_intcp1 = 67.9
      real (kind=c_float) :: rSNWD_denom = 2.6

      ! Filename for standard (single-station) time-series file
      character (len=256) :: sTimeSeriesFilename = repeat(" ", 256)

      ! Filename for land use lookup table
      character (len=256) :: sLanduseLookupFilename = repeat(" ", 256)

      ! Filename for irrigation lookup table
      character (len=256) :: sIrrigationLookupFilename = repeat(" ", 256)

      ! PROJ4 string for BASE Grid
      character (len=256) :: sBASE_PROJ4 = repeat(" ", 256)

      !> project grid definitions
      real (kind=c_double) :: rX0, rY0          ! Lower-left corner (world coords)
      real (kind=c_double) :: rX1, rY1          ! Upper-right corner (world coords)
      integer (kind=c_int) :: iNumGridCells  !
      real (kind=c_float) :: rGridCellSize     !
      integer (kind=c_int) :: iNX            !
      integer (kind=c_int) :: iNY            !

      ! PROJ4 string for Landuse Grid
      character (len=256) :: sLandUse_PROJ4 = repeat(" ", 256)

      ! PROJ4 string for Soil Group Grid
      character (len=256) :: sSoilGroup_PROJ4 = repeat(" ", 256)

      ! PROJ4 string for Soil Available Water Capacity  Grid
      character (len=256) :: sSoilAWC_PROJ4 = repeat(" ", 256)

      ! PROJ4 string for Flow Direction Grid
      character (len=256) :: sFlowDir_PROJ4 = repeat(" ", 256)

      ! PROJ4 string for PRECIPITATION Grid
      character (len=256) :: sPrecipitationGrid_PROJ4 = repeat(" ", 256)

      ! PROJ4 string for AIR TEMPERATURE Grid
      character (len=256) :: sTemperatureGrid_PROJ4 = repeat(" ", 256)

      ! Filename for basin mask table
      character (len=256) :: sBasinMaskFilename = repeat(" ", 256)

      ! Target prefixes for output files
      character (len=256) :: sOutputFilePath = "output/"
      character (len=256) :: sOutputFilePathDaily = "output/daily/"
      character (len=256) :: sOutputFilePathMonthly = "output/monthly/"
      character (len=256) :: sOutputFilePathAnnual = "output/annual/"

      character (len=256) :: sFutureFilePath = "output/future/"

      character (len=256) :: sImageFilePath = "images/"
      character (len=256) :: sImageFilePathDaily = "images/daily/"
      character (len=256) :: sImageFilePathMonthly = "images/monthly/"
      character (len=256) :: sImageFilePathAnnual = "images/annual/"

      ! Target prefixes for output files
      character (len=256) :: sOutputFilePrefix = "swb_"
      character (len=256) :: sFutureFilePrefix = "swb_future_"

      ! Target suffix for output files
      character (len=256) :: sOutputFileSuffix = "asc"

      ! Precipitation amounts describing antecedent runoff conditions
      real (kind=c_float) :: rDRY_DORMANT = 0.50_c_float   ! shift to Type I
      real (kind=c_float) :: rWET_DORMANT = 1.10_c_float   ! shift to Type II
      real (kind=c_float) :: rDRY_GROWING = 1.40_c_float   ! shift to Type I
      real (kind=c_float) :: rWET_GROWING = 2.10_c_float   ! shift to Type II

      ! Configuration flag for type of grid output to write to disk
      ! output file is either ARC_GRID or SURFER
      integer (kind=c_int) :: iOutputFormat

      ! Growing season configuration
      integer (kind=c_int) :: iDayOfFirstFrost
      integer (kind=c_int) :: iDayOfLastFrost
      logical (kind=c_bool) :: lNorthernHemisphere = lTRUE

      ! flags to control output
      logical (kind=c_bool) :: lReportDaily       !
      logical (kind=c_bool) :: lWriteToScreen     !

      integer (kind=c_int) :: iHeaderPrintInterval ! frequency with which to reprint
                                                   ! detailed output header

      ! Define how low iteration-to-iteration variance must be
      ! before the iterative approach is determined to have converged
      real (kind=c_float) :: rIterationTolerance = 1.0E-6

      ! define a pointer to the landuse lookup table
      type (T_LANDUSE_LOOKUP), dimension(:), pointer :: LU  ! T_LANDUSE_LOOKUP objects

      ! define a pointer to the IRRIGATION lookup table
      type (T_IRRIGATION_LOOKUP), dimension(:), pointer :: IRRIGATION

      ! define a pointer to the BASIN MASK lookup table
      type (T_BASIN_MASK), dimension(:), pointer :: BMASK  ! T_BASIN_MASK objects

      ! define a pointer to the CURVE NUMBER lookup table
      real (kind=c_float), dimension(:,:), pointer :: CN

      ! define a pointer to the MAXIMUM RECHARGE RATE lookup table
      real (kind=c_float), dimension(:,:),pointer :: MAX_RECHARGE

      ! define a pointer to the ROOTING DEPTH lookup table
      real (kind=c_float), dimension(:,:),pointer :: ROOTING_DEPTH

      ! define a pointer to the READILY_EVAPORABLE_WATER lookup table
      real (kind=c_float), dimension(:,:),pointer :: READILY_EVAPORABLE_WATER

      ! define a pointer to the TOTAL_EVAPORABLE_WATER lookup table
      real (kind=c_float), dimension(:,:),pointer :: TOTAL_EVAPORABLE_WATER

      ! define threshold value for CFGI below which ground is considered unfrozen
      real (kind=c_float) :: rLL_CFGI = 9999.0
      ! define threshold value for CFGI above which ground is considered frozen
      real (kind=c_float) :: rUL_CFGI = 9999.0

      ! define the land use category associated with open water
      integer(kind=c_int) :: iOPEN_WATER_LU = -99999

      ! define southern and northern latitude values bounding the grid
	  real (kind=c_float) :: rSouthernLatitude
 	  real (kind=c_float) :: rNorthernLatitude

#ifdef STREAM_INTERACTIONS
 	    ! Data for the elevation corrections on temperature
 	    logical (kind=c_bool) :: lElevAdjustment
 	    real (kind=c_float) :: rElevStationElevation
 	    real (kind=c_float) :: rElevDryFactor
 	    real (kind=c_float) :: rElevHumidFactor
 	    real (kind=c_float) :: rElevHumidityThreshold
#endif

       ! data structure to hold information about which cells we
       ! want to write our SSF files for
       type (T_SSF_FILES), dimension(:), pointer :: SSF_FILES

#ifdef STREAM_INTERACTIONS
 	  !! Added by Vic Kelson, February 2008
     !!
 	  !! These arrays manage the "Stream Interactions" option. This option uses
 	  !! special Curve Number values to remove water from the grid, e.g. to a
     !! surface stream or to a fracture network. A grid of "stream" information
     !! is read; non-zero entries in the grid capture water based on the index
     !! number in the cell. For example, if the grid cell stream value is 3,
     !! index 3 in the arrays below are used.
     !!
     !! Influent runoff water (IR) is captured by the stream capture code according
     !! to the values below. For cells with non-zero stream index,
     !!
     !! If 0 < IR < rStreamMaxInflow, the cell captures proportionally
     !!       IR = IR * (1 - rStreamMaxCapture / rStreamMaxInflow)
     !! If IR >= rStreamMaxInflow the cell captures the cell captures its maximum
     !!       IR = IR - rStreamMaxCapture
     !!
     !! The code defaults both values to a very large number, thus any amount of
     !! inflow into a cell where the stream index is non-zero will be captured.
     !! For most "surface water" applications, these values need not be changed
     !! unless there are special cases. For "fracture recharge" problems, these
     !! values allow the modeler to set the maximum amount that a fracture can
     !! capture.
     !!
 	  !! The constant STREAM_INTERACTIONS_MAX is the maximum number of curve
 	  !! number entries allowed for the fracture recharge option.
	  real (kind=c_float), dimension(STREAM_INTERACTIONS_MAX) :: rStreamMaxInflow
 	  real (kind=c_float), dimension(STREAM_INTERACTIONS_MAX) :: rStreamMaxCapture
#endif

  end type T_MODEL_CONFIGURATION

  !> data structure to hold a line of input from the classic time-series file
  type T_TIME_SERIES_FILE
     integer (kind=c_int) :: iMonth
     integer (kind=c_int) :: iDay
     integer (kind=c_int) :: iYear
     real (kind=c_float) :: rAvgT
     real (kind=c_float) :: rPrecip
     real (kind=c_float) :: rRH
     real (kind=c_float) :: rMaxT
     real (kind=c_float) :: rMinT
     real (kind=c_float) :: rWindSpd
     real (kind=c_float) :: rMinRH
     real (kind=c_float) :: rSunPct
     logical (kind=c_bool) :: lEOF = lFALSE
  end type T_TIME_SERIES_FILE

  !> Type that holds parameters used to create graphics with the DISLIN
  !> library.
  !>
  !> HEADING
  !> ------------------
  !>
  !>  1. List
  !>  2. List, pt 2
  !>  3. List, pt 3
  !>
  !>
  !>
  type T_GRAPH_CONFIGURATION

    character(len=256) :: cSETFIL = "DEFAULT_FILENAME"
    character(len=256) :: cCDEV = "PNG"
    character(len=256) :: cPAPER_SIZE = "USAP"
    character(len=256) :: cTITLE = "NO TITLE"
    character(len=256) :: cZ_AXIS_TITLE = "Z-AXIS TITLE"
    character(len=256) :: cX_AXIS_TITLE	= "X-AXIS TITLE"
    character(len=256) :: cY_AXIS_TITLE = "Y-AXIS TITLE"
    character(len=256) :: cFONT_NAME = "HELVE"
    character(len=256) :: cCOLOR_TABLE = "SPEC"

    integer(kind=c_int) :: iWinSizeX = 768
    integer(kind=c_int) :: iWinSizeY = 1024

    integer(kind=c_int) :: iTimeFrame = 1

    real(kind=c_float),dimension(3) :: rZA = 0.
    real(kind=c_float),dimension(3) :: rZE = 10.
    real(kind=c_float),dimension(3) :: rZOR = 0.
    real(kind=c_float),dimension(3) :: rZSTEP = 1.

    logical (kind=c_bool) :: lEnableDislinMessages = lTRUE

  end type T_GRAPH_CONFIGURATION

  integer(kind=c_int),parameter :: iDAILY = 1
  integer(kind=c_int),parameter :: iMONTHLY = 2
  integer(kind=c_int),parameter :: iANNUAL = 3

!**********************************************************************
!! GENERIC interfaces
!**********************************************************************

  interface approx_equal
    module procedure :: approx_equal_sgl
    module procedure :: approx_equal_dbl
  end interface approx_equal

  interface assert
    module procedure :: assert_simple_sub
    module procedure :: assert_simple_fl_sub
    module procedure :: assert_module_details_sub
    module procedure :: assert_module_details_fl_sub
  end interface assert

  interface asCharacter
    module procedure int2char
    module procedure c_size_t2char
    module procedure real2char
    module procedure dbl2char
  end interface asCharacter

  interface asReal
    module procedure char2real
    module procedure int2real
    module procedure dbl2real
  end interface asReal

  interface chomp
    module procedure chomp_delim_sub
    module procedure chomp_default_sub
  end interface chomp

  interface CtoF
    module procedure CtoF_sgl_fn
    module procedure CtoF_dbl_fn
  end interface CtoF

  interface FtoC
    module procedure FtoC_sgl_fn
    module procedure FtoC_dbl_fn
  end interface FtoC

  interface FtoK
    module procedure FtoK_sgl_fn
    module procedure FtoK_dbl_fn
  end interface FtoK


contains


function nextunit(iLU)  result(iUnit)

  integer (kind=c_int), intent(out), optional :: iLU
  integer (kind=c_int) :: iUnit

  ! [ LOCALS ]
  logical (kind=c_bool) :: lOpened
  integer (kind=c_int) :: iStartLU = 201
  integer (kind=c_int) :: iStopLU = 10000

  do iUnit=iStartLU, iStopLU
    inquire(unit = iUnit, opened = lOpened )
    if(.not. lOpened) exit

  enddo

  call assert(iUnit < iStopLU, "No available logical unit number could " &
    //"be found.",trim(__FILE__),__LINE__)

  if(present(iLU) ) iLU = iUnit

end function nextunit

!------------------------------------------------------------------------------

function squote(sString)                                   result(sQuotedString)

  character (len=*), intent(in) :: sString
  character (len=len_trim(adjustl(sString))+2) :: sQuotedString

  sQuotedString = "'"//trim(adjustl(sString))//"'"

end function squote

!------------------------------------------------------------------------------

!> @brief Function 'dquote' returns a character string with double quotes
!> prepended and appended to the string that is passed.
function dquote(sString)                                   result(sQuotedString)

  character (len=*), intent(in) :: sString
  character (len=len_trim(adjustl(sString))+2) :: sQuotedString

  sQuotedString = '"'//trim(adjustl(sString))//'"'

end function dquote

!------------------------------------------------------------------------------

function str_compare(sString1, sString2)                   result(lBool)

  character(len=*), intent(in) :: sString1
  character(len=*), intent(in) :: sString2
  logical (kind=c_bool) :: lBool

#ifdef DEBUG_PRINT
  write(*,fmt="(/,a)") trim(__FILE__)//":"//trim(int2char(__LINE__))
  write(*,fmt="(a)") "Incoming sString1: "//dquote(sString1)
  write(*,fmt="(a)") "Incoming sString2: "//dquote(sString2)
  write(*,fmt="(a)") "Munged sString1: "//dquote(adjustl(uppercase_fn(sString1)) )
  write(*,fmt="(a)") "Munged sString2: "//dquote(adjustl(uppercase_fn(sString2)) )
#endif

  if(trim(adjustl(uppercase_fn(sString1))) .eq. trim(adjustl(uppercase_fn(sString2)))) then
    lBool = lTRUE
  else
    lBool = lFALSE
  end if

end function str_compare

!------------------------------------------------------------------------------

subroutine assert_simple_sub(lCondition,sErrorMessage)

  ! ARGUMENTS
  logical (kind=c_bool), intent(in) :: lCondition
  character (len=*), intent(in) :: sErrorMessage

  ! [ LOCALS ]
  integer (kind=c_int) :: i
  integer (kind=c_int), dimension(2) :: iLU

  iLU = [ LU_STD_OUT, LU_LOG ]

  if ( .not. lCondition ) then

    do i=1,2
      write(UNIT=iLU(i),FMT="(/,a,/)") '** FATAL ERROR - HALTING SWB **'
      call writeMultiLine(trim(sErrorMessage), iLU(i))
    enddo

    stop

  endif

end subroutine assert_simple_sub

!--------------------------------------------------------------------------

subroutine assert_simple_fl_sub(lCondition,sErrorMessage)

  ! ARGUMENTS
  logical, intent(in) :: lCondition
  character (len=*), intent(in) :: sErrorMessage

  call assert_simple_sub(logical(lCondition, kind=c_bool), &
      sErrorMessage)

end subroutine assert_simple_fl_sub

!--------------------------------------------------------------------------

subroutine assert_module_details_sub(lCondition,sErrorMessage,sFilename,iLineNum)

  ! ARGUMENTS
  logical (kind=c_bool), intent(in) :: lCondition
  character (len=*), intent(in) :: sErrorMessage
  character (len=*) :: sFilename
  integer (kind=c_int) :: iLineNum

  ! [ LOCALS ]
  integer (kind=c_int) :: i
  integer (kind=c_int), dimension(2) :: iLU
  character (len=12) :: sLineNum

  iLU = [ LU_STD_OUT, LU_LOG ]

  if ( .not. lCondition ) then

    write(sLineNum,fmt="(i12)") iLineNum

    do i=1,2
      write(UNIT=iLU(i),FMT="(/,a,/)") '** FATAL ERROR - HALTING SWB **'
      call writeMultiLine(trim(sErrorMessage), iLU(i))
      write(UNIT=iLU(i),FMT="(/,'   fortran module:',t27, a)") trim(sFilename)
      write(UNIT=iLU(i),FMT="('   module line number:',t27,a)") trim(adjustl(sLineNum))
    enddo

    stop

  endif

end subroutine assert_module_details_sub


subroutine assert_module_details_fl_sub(lCondition,sErrorMessage,sFilename,iLineNum)

  ! ARGUMENTS
  logical, intent(in) :: lCondition
  character (len=*), intent(in) :: sErrorMessage
  character (len=*) :: sFilename
  integer (kind=c_int) :: iLineNum

  call assert_module_details_sub(logical(lCondition, kind=c_bool), &
    sErrorMessage,sFilename,iLineNum)

end subroutine assert_module_details_fl_sub

!-------------------------------------------------------------------------------

!> @brief echo to screen AND write to logfile
  subroutine echolog(sMessage)

    character(len=*), intent(in)             :: sMessage

    call writeMultiLine(sMessage, LU_STD_OUT )
    call writeMultiLine(sMessage, LU_LOG )
    flush(LU_LOG)

  end subroutine echolog

!-------------------------------------------------------------------------------

subroutine Chomp_delim_sub(sRecord, sItem, sDelimiters)

  ! ARGUMENTS
  character (len=*), intent(inout)                 :: sRecord
  character (len=256), intent(out)   :: sItem
  character (len=*), intent(in)                    :: sDelimiters
  ! LOCALS
  integer (kind=c_int) :: iR                      ! Index in sRecord
  integer (kind=c_int) :: iB                      !
  integer (kind=c_int) :: iLen

  iB = 0

#ifdef DEBUG_PRINT
  write(*,fmt="(/,a)") trim(__FILE__)//":"//trim(int2char(__LINE__))
  write(*,fmt="(a)") "Incoming sRecord: "//dquote(sRecord)
#endif

  ! eliminate any leading spaces
  sRecord = adjustl(sRecord)
  ! find the end position of 'sRecord'
  iLen = len_trim(sRecord)

  ! find the POSITION of the first delimiter found
  iR = SCAN(trim(sRecord),sDelimiters)

  if(iR==0) then
    sItem = trim(sRecord)   ! no delimiters found; return entirety of sRecord
    sRecord = ""            ! as sItem
  else
    sItem = trim(sRecord(1:iR-1))
    sRecord = trim( adjustl(sRecord(iR+1:)) )
  end if

#ifdef DEBUG_PRINT
  write(*,fmt="(a)") "Exit sRecord: "//dquote(sRecord)
  write(*,fmt="(a)") "Exit sItem: "//dquote(sItem)
  write(*,fmt="(a,i3)") "  iR: ", iR
  write(*,fmt="(a,i3)") "  iB: ", iB
#endif


end subroutine Chomp_delim_sub

!------------------------------------------------------------------------------

subroutine Chomp_default_sub(sRecord, sItem)

  ! ARGUMENTS
  character (len=*), intent(inout)                 :: sRecord
  character (len=256), intent(out)   :: sItem

  ! LOCALS
  integer (kind=c_int) :: iR                      ! Index in sRecord
  integer (kind=c_int) :: iB                      !
  integer (kind=c_int) :: iLen

#ifdef DEBUG_PRINT
  write(*,fmt="(/,a)") trim(__FILE__)//":"//trim(int2char(__LINE__))
  write(*,fmt="(a)") "Incoming sRecord: "//dquote(sRecord)
#endif

  ! eliminate any leading spaces
  sRecord = adjustl(sRecord)
  ! find the end position of 'sRecord'
!  iLen = len_trim(sRecord)

  ! find the POSITION of the first delimiter found
  iR = SCAN(trim(sRecord),sWHITESPACE)

  if(iR==0) then
    sItem = trim(sRecord)   ! no delimiters found; return entirety of sRecord
    sRecord = ""            ! as sItem
  else
    sItem = trim(adjustl(sRecord(1:iR-1)))
    sRecord = trim(adjustl(sRecord(iR+1:)))
  end if

#ifdef DEBUG_PRINT
  write(*,fmt="(a)") "Exit sRecord: "//dquote(sRecord)
  write(*,fmt="(a)") "Exit sItem: "//dquote(sItem)
  write(*,fmt="(a,i3)") "  iR: ", iR
#endif

end subroutine Chomp_default_sub

! !--------------------------------------------------------------------------
! !****s* types/Chomp
! ! NAME
! !   Chomp - Chomps the first space-delimited word from the
! !           beginning of a text string.
! !
! ! SYNOPSIS
! !   Chomps the first space-delimited word from the the text string in
! !   sRecord and returns it in sItem; leaves the remaining text in sRecord.
! !
! ! INPUTS
! !   sRecord - Character string to operate on.
! !
! ! OUTPUTS
! !   sRecord - Character string to operate on.
! !   sItem - Character string to operate on.
! !
! ! EXAMPLE
! !   input:   sRecord = "THIS IS THE TIME"    sItem = ""
! !   output:  sRecord = "IS THE TIME"         sItem = "THIS"
! !
! ! SOURCE
!
! subroutine Chomp(sRecord, sItem)
!
!   ! ARGUMENTS
!   character (len=*), intent(inout) :: sRecord
!   character (len=256), intent(out) :: sItem
!   ! LOCALS
!   integer (kind=c_int) :: iR                      ! Index in sRecord
!   integer (kind=c_int) :: iS                      ! Index in sItem
!   logical (kind=c_bool) :: lSkip               ! TRUE while skipping spaces
!
!   ! Set my pointers and remove leading and trailing spaces
!   iR = 1
!   iS = 1
!   sItem = ""
!   lSkip = lTRUE
!   do iR=1,len_trim(sRecord)
!       if ( lSkip .and. sRecord(iR:iR) == " " ) then
!           cycle
!       else if ( .not. lSkip .and. sRecord(iR:iR) == " " ) then
!           exit
!       else
!           lSkip = lFALSE
!           sItem(iS:iS) = sRecord(iR:iR)
!           iS = iS+1
!       end if
!   end do
!   sRecord = sRecord(iR:)
!
!   return
! end subroutine Chomp
!
!
 subroutine GetSysTimeDate(sDateStr,sDateStrPretty)

   character(len=256), intent(out) :: sDateStr, sDateStrPretty

   character (len=256) :: sRecord
   character (len=256) :: sItem
   character (len=256) :: sDay
   character (len=256) :: sMon
   character (len=256) :: sDD
   character (len=8) :: sHH
   character (len=8) :: sMM
   character (len=8) :: sSS
   character (len=256) :: sTime
   character (len=256) :: sYear

   sRecord = FDATE()

   call chomp(sRecord,sDay)
   call chomp(sRecord,sMon)
   call chomp(sRecord,sDD)
   call chomp(sRecord,sTime)
   call chomp(sRecord,sYear)

   sHH = sTime(1:2)
   sMM = sTime(4:5)
   sSS = sTime(7:8)

   sDateStr = TRIM(sDD)//"_"//TRIM(sMon)//"_"//TRIM(sYear)//"__"//&
     TRIM(sHH)//"_"//TRIM(sMM)
   sDateStrPretty = &
     TRIM(sDay)//" "//TRIM(sMon)//" "//TRIM(sDD)//" "//TRIM(sYear)//" " &
      //TRIM(sHH)//":"//TRIM(sMM)

   return

 end subroutine GetSysTimeDate

!
! !--------------------------------------------------------------------------
! !****s* types/Chomp_tab
! ! NAME
! !   Chomp_tab - Chomps all text up to the first tab character from a text string.
! !
! ! SYNOPSIS
! !   Chomps all text up to the first tab character from the text string in
! !   sRecord and returns it in sItem; leaves the remaining text in sRecord.
! !
! ! INPUTS
! !   sRecord - Character string to operate on.
! !
! ! OUTPUTS
! !   sRecord - Character string to operate on.
! !   sItem - Character string to operate on.
! !
! ! EXAMPLE
! !   input:   sRecord = "THIS IS<tab> THE TIME"    sItem = ""
! !   output:  sRecord = " THE TIME"                sItem = "THIS IS"
! !
! ! SOURCE
!
 subroutine Chomp_tab(sRecord, sItem)

   ! ARGUMENTS
   character (len=*), intent(inout)                :: sRecord
   character (len=256), intent(out)   :: sItem
   ! LOCALS
   integer (kind=c_int) :: iR                      ! Index in sRecord
   character (len=1), parameter :: cTab = ACHAR(9) ! ASCII tab character

   iR = SCAN(sRecord,cTab)

   if(iR==0) then
     sItem = trim(sRecord)      ! no tab found; return entirety of sRecord
 	sRecord = ""			   ! as sItem
   else
     sItem = trim(sRecord(1:iR-1))
 	sRecord = trim(sRecord(iR+1:))
     do iR=1,len_trim(sRecord)
 	  if (sRecord(iR:iR) == " " ) then
         cycle
 	  else
 	    exit
 	  end if
 	end do
     sRecord = sRecord(iR:)
   end if

 end subroutine Chomp_tab

!--------------------------------------------------------------------------
!****s* types/Chomp_slash
! NAME
!   Chomp_slash - Chomps all text up to the first slash (/) character from a text string.
!
! SYNOPSIS
!   Chomps all text up to the first slash character from the text string in
!   sRecord and returns it in sItem; leaves the remaining text in sRecord.
!
! INPUTS
!   sRecord - Character string to operate on.
!
! OUTPUTS
!   sRecord - Character string to operate on.
!   sItem - Character string to operate on.
!
! EXAMPLE
!   input:   sRecord = "03/27/1998"    sItem = ""
!   output:  sRecord = "27/1998"       sItem = "03"
!
! SOURCE

subroutine Chomp_slash(sRecord, sItem)

  ! ARGUMENTS
  character (len=*), intent(inout)                 :: sRecord
  character (len=256), intent(out)   :: sItem
  ! LOCALS
  integer (kind=c_int) :: iR                      ! Index in sRecord
  character (len=1), parameter :: cSlash = "/"

  iR = SCAN(sRecord,cSlash)

  if(iR==0) then
    sItem = trim(sRecord)  ! no slash found; return entirety of sRecord
	 sRecord = ""			   ! as sItem
  else
    sItem = trim(sRecord(1:iR-1))
    sRecord = trim(sRecord(iR+1:))

    do iR=1,len_trim(sRecord)
	   if (sRecord(iR:iR) == " " ) then
        cycle
	   else
	     exit
      end if
	 end do
    sRecord = sRecord(iR:)
  end if

  return
end subroutine Chomp_slash

!--------------------------------------------------------------------------

function mmddyyyy2julian(sMMDDYYYY)  result(iJD)

  character (len=*) :: sMMDDYYYY
  integer (kind=c_int) :: iJD

  ! [ LOCALS ]
  integer (kind=c_int) :: iMonth
  integer (kind=c_int) :: iDay
  integer (kind=c_int) :: iYear
  character (len=256) :: sItem, sBuf
  integer (kind=c_int) :: iStat

  sItem = sMMDDYYYY

  ! parse month value
  call Chomp_slash(sItem, sBuf)
  read(sBuf,*,iostat = iStat) iMonth
  call Assert(iStat==0, "Problem reading month value from string "//TRIM(sMMDDYYYY), &
    TRIM(__FILE__),__LINE__)

  ! parse day value
  call Chomp_slash(sItem, sBuf)
  read(sBuf,*,iostat=iStat) iDay
  call Assert(iStat==0, "Problem reading day value from string "//TRIM(sMMDDYYYY), &
    TRIM(__FILE__),__LINE__)

  ! parse year value
  call Chomp_slash(sItem, sBuf)
  read(sBuf,*,iostat=iStat) iYear
  call Assert(iStat==0, "Problem reading year value from string "//TRIM(sMMDDYYYY), &
    TRIM(__FILE__),__LINE__)

  iJD = julian_day ( iYear, iMonth, iDay)

  return

end function mmddyyyy2julian

!--------------------------------------------------------------------------

function mmdd2doy(sMMDD)  result(iDOY)

  character (len=*) :: sMMDD
  integer (kind=c_int) :: iDOY

  ! [ LOCALS ]
  integer (kind=c_int) :: iMonth
  integer (kind=c_int) :: iDay
  integer (kind=c_int) :: iYear
  character (len=256) :: sItem, sBuf
  integer (kind=c_int) :: iStat
  integer (kind=c_int) :: iJD
  integer (kind=c_int) :: iStartingJD

  sItem = sMMDD

  ! parse month value
  call Chomp_slash(sItem, sBuf)
  read(sBuf,*,iostat = iStat) iMonth
  call Assert(iStat==0, "Problem reading month value from string "//TRIM(sMMDD), &
    TRIM(__FILE__),__LINE__)

  ! parse day value
  call Chomp_slash(sItem, sBuf)
  read(sBuf,*,iostat=iStat) iDay
  call Assert(iStat==0, "Problem reading day value from string "//TRIM(sMMDD), &
    TRIM(__FILE__),__LINE__)


  iStartingJD = julian_day ( 1999, 1, 1)
  iJD = julian_day ( 1999, iMonth, iDay)

  iDOY = iJD - iStartingJD + 1

  return

end function mmdd2doy

!--------------------------------------------------------------------------

function mmddyyyy2doy(sMMDDYYYY)  result(iDOY)

  character (len=*) :: sMMDDYYYY
  integer (kind=c_int) :: iDOY

  ! [ LOCALS ]
  integer (kind=c_int) :: iMonth
  integer (kind=c_int) :: iDay
  integer (kind=c_int) :: iYear
  character (len=256) :: sItem, sBuf
  integer (kind=c_int) :: iStat
  integer (kind=c_int) :: iJD
  integer (kind=c_int) :: iStartingJD

  sItem = sMMDDYYYY

  ! parse month value
  call Chomp_slash(sItem, sBuf)
  read(sBuf,*,iostat = iStat) iMonth
  call Assert(iStat==0, "Problem reading month value from string "//TRIM(sMMDDYYYY), &
    TRIM(__FILE__),__LINE__)

  ! parse day value
  call Chomp_slash(sItem, sBuf)
  read(sBuf,*,iostat=iStat) iDay
  call Assert(iStat==0, "Problem reading day value from string "//TRIM(sMMDDYYYY), &
    TRIM(__FILE__),__LINE__)

  ! parse year value
  call Chomp_slash(sItem, sBuf)
  read(sBuf,*,iostat=iStat) iYear
  call Assert(iStat==0, "Problem reading year value from string "//TRIM(sMMDDYYYY), &
    TRIM(__FILE__),__LINE__)


  iStartingJD = julian_day ( iYear, 1, 1)
  iJD = julian_day ( iYear, iMonth, iDay)

  iDOY = iJD - iStartingJD + 1

  return

end function mmddyyyy2doy

!----------------------------------------------------------------------------

function uppercase_fn ( s )                               result(sOut)

  ! ARGUMENTS
  character (len=*), intent(in) :: s
  character(len=len(s)) :: sOut
  ! LOCALS
  integer (kind=c_int) :: i    ! do loop index
  ! CONSTANTS
  integer (kind=c_int), parameter :: LOWER_TO_UPPER = -32

!  LOWER_TO_UPPER = ichar( "A" ) - ichar( "a" )

  sOut = s

  do i=1,len_trim(sOut)
      if ( ichar(sOut(i:i) ) >= ichar("a") .and. ichar(sOut(i:i)) <= ichar("z") ) then
          sOut(i:i) = char( ichar( sOut(i:i) ) + LOWER_TO_UPPER )
      end if
  end do

  return
end function uppercase_fn

!--------------------------------------------------------------------------

function lowercase_fn ( s )                               result(sOut)

  ! ARGUMENTS
  character (len=*), intent(in) :: s
  character(len=len(s)) :: sOut
  ! LOCALS
  integer (kind=c_int) :: i    ! do loop index
  ! CONSTANTS
  integer (kind=c_int), parameter :: UPPER_TO_LOWER = 32

!  UPPER_TO_LOWER = ichar( "a" ) - ichar( "A" )

  sOut = s

  do i=1,len_trim(sOut)
      if ( ichar(sOut(i:i) ) >= ichar("A") .and. ichar(sOut(i:i)) <= ichar("Z") ) then
          sOut(i:i) = char( ichar( sOut(i:i) ) + UPPER_TO_LOWER )
      end if
  end do

end function lowercase_fn

!--------------------------------------------------------------------------

subroutine uppercase ( s )

  ! ARGUMENTS
  character (len=*), intent(inout) :: s
  ! LOCALS
  integer (kind=c_int) :: i    ! do loop index
  ! CONSTANTS
  integer (kind=c_int) :: LOWER_TO_UPPER = ichar( "A" ) - ichar( "a" )

  do i=1,len_trim(s)
      if ( ichar(s(i:i) ) >= ichar("a") .and. ichar(s(i:i)) <= ichar("z") ) then
          s(i:i) = char( ichar( s(i:i) ) + LOWER_TO_UPPER )
      end if
  end do

  return
end subroutine uppercase

!--------------------------------------------------------------------------
!****s* types/CleanUpCsv
! NAME
!   CleanUpCsv - Strips punctuation from a string, making it Fortran-friendly.
!
! SYNOPSIS
!   Strips commas, tabs, and double-quotes from a text string.
!
! INPUTS
!   s - Character string to operate on.
!
! OUTPUTS
!   s - Character string to operate on.
! SOURCE

subroutine CleanUpCsv ( s )

  ! ARGUMENTS
  character (len=*), intent(inout) :: s
  ! LOCALS
  integer (kind=c_int) :: i,j
  ! LOCAL PARAMETERS
  character (len=1),dimension(3),parameter :: &
                REPLACE_CHARS = (/ ',', achar(9), '"' /)

  do i=1,size(REPLACE_CHARS)
    do j=1,len(s)
      if ( s(j:j) == REPLACE_CHARS(i) ) s(j:j) = ' '
    end do
  end do

  return
end subroutine CleanUpCsv

function clean(sRecord,sDelimiters)                       result(sItem)

  ! ARGUMENTS
  character (len=*), intent(inout)           :: sRecord
  character (len=*), intent(in), optional    :: sDelimiters

  ! LOCALS
  character (len=256) :: sItem
  integer (kind=c_int) :: iR                 ! Index in sRecord
  integer (kind=c_int) :: i, j

  ! eliminate any leading spaces
  sRecord = adjustl(sRecord)
  sItem = ""
  j = 0

  do i = 1,len_trim(sRecord)

    if(present(sDelimiters)) then
      iR = SCAN(sRecord(i:i),sDelimiters)
    else
      iR = SCAN(sRecord(i:i),":/;,")
    endif

    if(iR==0) then
      j = j + 1
      sItem(j:j) = sRecord(i:i)
    end if

  enddo

end function clean


!--------------------------------------------------------------------------
!****s* types/CleanUpSlash
! NAME
!   CleanUpCsv - Strips punctuation from a string, including slashes,
!       making it Fortran-friendly.
!
! SYNOPSIS
!   Strips commas, tabs, slashes, and double-quotes from a text string.
!
! INPUTS
!   s - Character string to operate on.
!
! OUTPUTS
!   s - Character string to operate on.
! SOURCE

subroutine CleanUpSlash ( s )

  ! ARGUMENTS
  character (len=*), intent(inout) :: s
  ! LOCALS
  integer (kind=c_int) :: i,j
  ! LOCAL PARAMETERS
  character (len=1),dimension(5),parameter :: &
                REPLACE_CHARS = [ ',', sTAB, '"', &
                  sBACKSLASH, sFORWARDSLASH ]

  do i=1,size(REPLACE_CHARS)
    do j=1,len(s)
      if ( s(j:j) == REPLACE_CHARS(i) ) s(j:j) = ' '
    end do
  end do

end subroutine CleanUpSlash



! This is a simple function to search for an available unit.
! LUN_MIN and LUN_MAX define the range of possible LUNs to check.
! The UNIT value is returned by the function, and also by the optional
! argument. This allows the function to be used directly in an OPEN
! statement, and optionally save the result in a local variable.
! If no units are available, -1 is returned.
integer function newunit(unit)
  integer, intent(out), optional :: unit
! local
  integer, parameter :: LUN_MIN=10, LUN_MAX=1000
  logical :: opened
  integer :: lun
! begin
  newunit=-1
  do lun=LUN_MIN,LUN_MAX
    inquire(unit=lun,opened=opened)
    if (.not. opened) then
      newunit=lun
      exit
    end if
  end do
  if (present(unit)) unit=newunit
end function newunit

!--------------------------------------------------------------------------
!****f* types/polynomial
! NAME
!   polynomial - Evaluates a polynomial function.
! SYNOPSIS
!   Computes the value of a polynomial function with an arbitrary number
!   of coefficients.
! INPUTS
!   x - x value of the polynomial.
!   a - Array of coefficients that make up the polynomial expression.
!
! OUTPUTS
!   result - Value of the polynomial evaluated at x.
!
! SOURCE

function polynomial(x,a) result(rV)
  ! Computes a polynomial in x, with coefficients in a.
  ! [ ARGUMENTS ]
  real (kind=c_double),intent(in) :: x
  real (kind=c_double),dimension(:) :: a
  ! [ RETURN VALUE ]
  real (kind=c_double) :: rV
  ! [ LOCALS ]
  integer (kind=c_int) :: i

  rV =a(1)
  do i=2,size(a)
    rV = rV*x + a(i)
  end do

!  rV = dpZERO

!  do i=1,size(a)

!    rV = rV + a(i) * x**(i-1)

!  enddo


end function polynomial


!--------------------------------------------------------------------------
!****s* types/LookUpMonth
! NAME
!   LookUpMonth - Returns the month name and integer index given day of year.
!
! SYNOPSIS
!   This subroutine returns the name and integer index of the month (1-12) that
!   corresponds to the current integer day of the year (1-365). Also indicates
!   whether the current day is the last day in the current month. Does *not*
!   account for leap years.
!
! INPUTS
!   iMonth - Month index (1-12) corresponding to the current day of the year.
!   iDay - Day index (1-31) corresponding to the current day within the
!          present month.
!   iYear - Current Gregorian year (4-digit).
!
! OUTPUTS
!   iDayOfYear - Integer value corresponding to the number of days since
!                January 1 of the current year (e.g. January 1 = day 1).
!   sMonthName - character string representing the name of the current month.
!   lMonthEnd -  .true. if the current day of the month is the last day of
!                the month, .false. otherwise.
!
! NOTES
!   see http://daac.gsfc.nasa.gov/julian_calendar.shtml for tables relating
!   the day of year to calendar month
!
! SOURCE

subroutine LookupMonth(iMonth, iDay, iYear,iDayOfYear, &
                       sMonthName,lMonthEnd)

  ! [ ARGUMENTS ]
  integer (kind=c_int),intent(in) :: iMonth
  integer (kind=c_int),intent(in) :: iDay
  integer (kind=c_int),intent(in) :: iYear
  integer (kind=c_int),intent(out) :: iDayOfYear
  character (len=3),intent(out) :: sMonthName
  logical (kind=c_bool),intent(out) :: lMonthEnd
  ! [ LOCALS ]
  integer (kind=c_int) :: i
  type ( T_MONTH ),pointer :: mo
  integer (kind=c_int) :: iEpochMonth = 1
  integer (kind=c_int) :: iEpochDay = 1
  integer (kind=c_int) :: iEpochJulianDay
  integer (kind=c_int) :: iDummy1, iDummy2
  integer (kind=c_int) :: iJulianDay
  integer (kind=c_int) :: iTomorrowsMonth

  ! get the Julian Day corresponding to the first day of the
  ! current calendar year
  iEpochJulianDay = julian_day( iYear, iEpochMonth, iEpochDay )

  ! get the Julian Day corresponding to the current day of the
  ! current calendar year
  iJulianDay = julian_day (iYear, iMonth, iDay)

  ! calculate day of year as the difference between today's Julian
  ! Day Number and the JDN for Jan 1 of the current year.
  iDayOfYear = iJulianDay - iEpochJulianDay + 1

  call gregorian_date(iJulianDay + 1, iDummy1, iTomorrowsMonth, iDummy2)

  lMonthEnd = .not. (iTomorrowsMonth == iMonth)

  mo => YEAR_INFO(iMonth)

  sMonthName = mo%sName

  return
end subroutine LookupMonth

!----------------------------------------------------------------------

function approx_equal_dbl(rA, rB, rTol)  result(lTest)

   real(kind=c_double) :: rA
   real(kind=c_double) :: rB
   real(kind=c_double), optional :: rTol
   logical(kind=c_bool) :: lTest

   ! [ LOCALS ]
   real (kind=c_double) :: rDiff
   integer (kind=c_int) :: iDiff
   real(kind=c_double) :: rMultiplier

   if(present(rTol) ) then
     rMultiplier = rTol
   else
     rTol = 10000.
   endif

   rDiff = ABS(rA - rB)
   iDiff = int(rDiff * rMultiplier, kind=c_int)

   if(iDiff == 0) then
     lTest = lTRUE
   else
     lTest = lFALSE
   endif

   return

end function approx_equal_dbl


function approx_equal_sgl(rA, rB, rTol)  result(lTest)

   real(kind=c_float) :: rA
   real(kind=c_float) :: rB
   real(kind=c_float), optional :: rTol
   logical(kind=c_bool) :: lTest

   ! [ LOCALS ]
   real (kind=c_double) :: rDiff
   integer (kind=c_int) :: iDiff
   real(kind=c_float) :: rMultiplier

   if(present(rTol) ) then
     rMultiplier = rTol
   else
     rTol = 10000.
   endif

   rDiff = ABS(rA - rB)
   iDiff = int(rDiff * rMultiplier, kind=c_int)

   if(iDiff == 0) then
     lTest = lTRUE
   else
     lTest = lFALSE
   endif

   return

end function approx_equal_sgl


!***
!--------------------------------------------------------------------------
!****f* types/lf_model_GrowingSeason
! NAME
!   lf_model_GrowingSeason - Returns .true. if iDayOfYear is inside the
!                            growing season window.
!
! SYNOPSIS
!   Returns .true. if iDayOfYear is within the inside the growing season
!   window. Returns .false. otherwise.
!
! INPUTS
!   pConfig - pointer to the model configuration data structure.
!   iDayOfYear - integer day of the year (1-365).
!
! OUTPUTS
!   lGrowing - .true. if iDayOfYear is inside the growing season window;
!              .false. otherwise.
!
! NOTES
!   The model configuration data structure must be iniitialized before calling
!   this function. The function return value changes based on whether we
!   are in the northern or suthern hemisphere.
!
! SOURCE

function lf_model_GrowingSeason( pConfig, iDayOfYear ) result ( lGrowing )

  ! [ ARGUMENTS ]
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  integer (kind=c_int),intent(in) :: iDayOfYear
  ! [ RETURN VALUE ]
  logical (kind=c_bool) :: lGrowing

  lGrowing = ( iDayOfYear > pConfig%iDayOfFirstFrost .and. &
               iDayOfYear < pConfig%iDayOfLastFrost )

  if(.not. pConfig%lNorthernHemisphere) then
    lGrowing = .not. lGrowing
  end if

  return
end function lf_model_GrowingSeason


!--------------------------------------------------------------------------
!****f* types/FtoC
! NAME
!   FtoC - Converts degrees Fahrenheit to degrees Celsius.
!
! SYNOPSIS
!   Converts degrees Fahrenheit to degrees Celsius.
!
! INPUTS
!   rF - Temperature in degrees Fahrenheit.
!
! OUTPUTS
!   rC - Temperature in degrees Celcius.
!
! SOURCE

 function FtoC_sgl_fn(rF)   result(rC)
  ! Converts degrees Fahrenheit to degrees Celsius
  ! [ ARGUMENTS ]
  real (kind=c_float),intent(in) :: rF
  ! [ RETURN VALUE ]
  real (kind=c_float) :: rC

  rC = (rF - rFREEZING) * dpC_PER_F

end function FtoC_sgl_fn


 function FtoC_dbl_fn(rF)   result(rC)
  ! Converts degrees Fahrenheit to degrees Celsius
  ! [ ARGUMENTS ]
  real (kind=c_double),intent(in) :: rF
  ! [ RETURN VALUE ]
  real (kind=c_double) :: rC

  rC = (rF - dpFREEZING) * dpC_PER_F

end function FtoC_dbl_fn

!--------------------------------------------------------------------------
!****f* types/CtoF
! NAME
!   CtoF - Converts degrees Celsius to degrees Fahrenheit.
!
! SYNOPSIS
!   Converts degrees Celsius to degrees Fahrenheit.
!
! INPUTS
!   rC - Temperature in degrees Celsius.
!
! OUTPUTS
!   rF - Temperature in degrees Fahrenheit.
!
! SOURCE

 function CtoF_sgl_fn(rC)   result(rF)
  ! Converts degrees Celsius to degrees Fahrenheit
  ! [ ARGUMENTS ]
  real (kind=c_float),intent(in) :: rC
  ! [ RETURN VALUE ]
  real (kind=c_float) :: rF

  rF = rC * dpF_PER_C + rFREEZING

end function CtoF_sgl_fn


 function CtoF_dbl_fn(rC)   result(rF)
  ! Converts degrees Celsius to degrees Fahrenheit
  ! [ ARGUMENTS ]
  real (kind=c_double),intent(in) :: rC
  ! [ RETURN VALUE ]
  real (kind=c_double) :: rF

  rF = rC * dpF_PER_C + dpFREEZING

end function CtoF_dbl_fn

!--------------------------------------------------------------------------
!****f* types/FtoK
! NAME
!   FtoC - Converts degrees Fahrenheit to kelvins.
!
! SYNOPSIS
!   Converts degrees Fahrenheit to kelvins.
!
! INPUTS
!   rF - Temperature in degrees Fahrenheit.
!
! OUTPUTS
!   rK - Temperature in kelvins.
!
! SOURCE

 function FtoK_sgl_fn(rF)    result(rK)
  ! Converts degrees Fahrenheit to kelvins
  ! [ ARGUMENTS ]
  real (kind=c_float),intent(in) :: rF
  ! [ RETURN VALUE ]
  real (kind=c_float) :: rK

  rK = (rF - rFREEZING) * dpC_PER_F + 273.15_c_float

end function FtoK_sgl_fn

 function FtoK_dbl_fn(rF)    result(rK)
  ! Converts degrees Fahrenheit to kelvins
  ! [ ARGUMENTS ]
  real (kind=c_double),intent(in) :: rF
  ! [ RETURN VALUE ]
  real (kind=c_double) :: rK

  rK = (rF - dpFREEZING) * dpC_PER_F + 273.15_c_double

end function FtoK_dbl_fn

!***
!--------------------------------------------------------------------------
function count_fields(sRecord) result(iNumFields)

  character (len=*), intent(inout) :: sRecord

  character (len=256) :: sItem
  integer(kind=c_int) :: iNumFields, i

  i=1

  do
    call chomp(sRecord,sItem, sTAB)

    if(LEN_TRIM(sRecord)==0) then
      exit
    end if
    i=i+1

  end do

  iNumFields = i

  return

end function count_fields

!--------------------------------------------------------------------------
!****f* types/mm_to_in
! NAME
!   FtoC - Converts millimeters to inches.
!
! SYNOPSIS
!   Converts measurements in millimeters to inches.
!
! INPUTS
!   r_mm - Value in millimeters.
!
! OUTPUTS
!   r_in - Value in inches.
!
! SOURCE

function mm_to_in(r_mm) result(r_in)
  ! Converts value in millimeters to inches
  ! [ ARGUMENTS ]
  real (kind=c_float),intent(in) :: r_mm
  ! [ RETURN VALUE ]
  real (kind=c_float) :: r_in

  r_in = r_mm / rMM_PER_INCH

  return

end function mm_to_in

!***
!--------------------------------------------------------------------------
!****f* types/interpolate
! NAME
!   interpolate - Finds an interpolated value from rXT, rYT, for the ordinate value rX.
!
! SYNOPSIS
!   Finds an interpolated value from rXT, rYT, for the ordinate value rX.
!   Expects the rXT and rYT to make a table of points, with the
!   ordinate values in rXT and the abscissa values in rYT. Retains
!   the end-member values for out-of-range rX values.
!
! INPUTS
!   rXT - array holding X-values of the table to be interpolated
!   rYT - array holding Y-values of the table to be interpolated
!   rX - X value for which the function will return a corresponding
!        interpolated Y-value
!
! OUTPUTS
!   rY - Y-value interpolated from the table and provided X-value
!
! NOTES
!
! SOURCE

function interpolate(rXT, rYT, rX) result(rY)

  ! [ ARGUMENTS ]
  real (kind=c_float), dimension(:), intent(in) :: rXT, rYT
  real (kind=c_float), intent(in) :: rX
  ! [ RETURN VALUE ]
  real (kind=c_float) :: rY
  ! [ LOCALS ]
  integer (kind=c_int) :: i,n

  call Assert(LOGICAL(size(rXT) == size(rYT),kind=c_bool), &
     "Interpolation tables don't conform")
  n = size(rXT)
  if (rX <= rXT(1)) then
    rY = rYT(1)
  else if (rX >= rXT(n)) then
    rY = rYT(n)
  else
    do i=2,n
      if (rXT(i) > rX) then
        rY = rYT(i-1) + (rX-rXT(i-1)) * (rYT(i)-rYT(i-1)) / (rXT(i)-rXT(i-1))
        exit
      end if
    end do
  end if

  return
end function interpolate

!--------------------------------------------------------------------------
!****f* types/julian_day
! NAME
!   julian_day - Convert from a Gregorian calendar date to a Julian day number.
!
! SYNOPSIS
!   Conversion from a Gregorian calendar date to a Julian day number.
!   Valid for any Gregorian calendar date producing a Julian day
!   greater than zero.
!
! INPUTS
!   iYear   4-digit year
!   iMonth  2-digit month (1-12)
!   iDay    2-digit day (1-31)
!
! OUTPUTS
!   iJD     integer number of days that have elapsed since noon
!           Greenwich Mean Time (UT or TT) Monday, January 1, 4713 BC
!
! SOURCE

function julian_day ( iYear, iMonth, iDay, iOrigin ) result(iJD)

  ! [ ARGUMENTS ]
  integer (kind=c_int), intent(in) :: iYear, iMonth, iDay
  integer (kind=c_int), optional :: iOrigin

  ! [ LOCALS ]
  integer (kind=c_int) i,j,k
  integer (kind=c_int) :: iOffset

  ! [ RETURN VALUE ]
  integer (kind=c_int) :: iJD

  call Assert(iMonth > 0 .and. iMonth <= 12, &
    "Illegal month value supplied: "//TRIM(int2char(iMonth)), &
    TRIM(__FILE__),__LINE__)

  call Assert(iDay > 0 .and. iDay <= YEAR_INFO(iMonth)%iNumDays, &
    "Illegal day value supplied: "//TRIM(int2char(iDay)), &
    TRIM(__FILE__),__LINE__)

  i= iYear
  j= iMonth
  k= iDay

  if(present(iOrigin)) then
    iOffset = iOrigin
  else
    iOffset = 0
  endif

  iJD= ( k-32075_c_int + 1461_c_int * (i + 4800_c_int + (j - 14_c_int) / 12_c_int) &
        /4_c_int + 367_c_int * (j - 2_c_int - (j - 14_c_int)/ 12_c_int * 12_c_int) &
        /12_c_int - 3_c_int *((i + 4900_c_int + (j - 14_c_int) &
        /12_c_int)/100_c_int)/4_c_int ) - iOffset

end function julian_day

!--------------------------------------------------------------------------
!****f* types/solstice
! NAME
!   solstice - Returns 0 normally, or a value >0 during solstice or equinox.
!
! SYNOPSIS
!    Returns the following:
!      0: non-solstice and non-equinox day
!      1: Vernal equinox
!      2: Summer Solstice
!      3: Autumnal equinox
!      4: Winter solstice
!
! INPUTS
!   iJD     Julian day value
!
! OUTPUTS
!   iSol    Code as described above
!
! SOURCE

function solstice (iJD)  result (iSol)

  ! [ ARGUMENTS ]
  integer (kind=c_int), intent(in) :: iJD

  ! [ LOCALS ]
  integer (kind=c_int) iMonth, iDay, iYear


  ! [ RETURN VALUE ]
  integer (kind=c_int) :: iSol

  call gregorian_date(iJD, iYear, iMonth, iDay)

  if(iMonth==3 .and. iDay == 20) then
    iSol = 1
  elseif(iMonth==6 .and. iDay == 21) then
    iSol = 2
  elseif(iMonth==9 .and. iDay == 22) then
    iSol = 3
  elseif(iMonth==12 .and. iDay == 21) then
    iSol = 4
  else
    iSol = 0
  endif

  return

end function solstice

!--------------------------------------------------------------------------

!> @brief Write multiple lines of output to Fortran logical unit
!> @details Writes one or more lines of an input text string to a Fortran
!> logical unit number. To output multiple lines, insert a tilde (~) at
!> each point in the text string where a carriage return is desired.
!> @param[in] sMessageText Character string that contains the message to be written.
!> @param[in] iLU Integer value of the Fortran logical unit number to write to.
subroutine writeMultiLine(sMessageText, iLU)

  ! [ ARGUMENTS ]
  character (len=*) :: sMessageText
  integer (kind=c_int) :: iLU

  ! [ LOCALS ]
  character (len=len(sMessageText) ) :: sRecord
  character (len=256) :: sItem
  logical (kind=c_bool) :: lFileOpen

  inquire (unit=iLU, opened=lFileOpen)

  sRecord = trim(sMessageText)

  if (lFileOpen) then

    do

      ! break up string with '~' as delimiter
      call chomp(sRecord, sItem, "~")
      if(len_trim(sItem) == 0) exit
      write(UNIT=iLU,FMT="(a)") trim(sItem)
    enddo

  endif

end subroutine writeMultiLine

!--------------------------------------------------------------------------

!> Convert an integer value into a formatted character string
function int2char(iValue)  result(sBuf)

  integer (kind=c_int) :: iValue
  character (len=256) :: sBuf

  write(UNIT=sBuf,FMT="(i14)") iValue
  sBuf = ADJUSTL(sBuf)

end function int2char

!--------------------------------------------------------------------------

!> Convert an character value into a real
function char2real(sValue)  result(rValue)

  character (len=256) :: sValue
  real (kind=c_float) :: rValue

  read(UNIT=sValue,FMT=*) rValue

end function char2real

!--------------------------------------------------------------------------

!> Convert an int value into a real
function int2real(iValue)  result(rValue)

  integer (kind=c_int) :: iValue
  real (kind=c_float) :: rValue

  rValue = real(iValue, kind=c_float)

end function int2real

!--------------------------------------------------------------------------

!> Convert an dbl value into a real
function dbl2real(dpValue)  result(rValue)

  real (kind=c_double) :: dpValue
  real (kind=c_float) :: rValue

  rValue = real(dpValue, kind=c_float)

end function dbl2real

!--------------------------------------------------------------------------

!> Convert an integer value into a formatted character string
function c_size_t2char(iValue)  result(sBuf)

  integer (kind=c_size_t) :: iValue
  character (len=256) :: sBuf

  write(UNIT=sBuf,FMT="(i14)") iValue
  sBuf = ADJUSTL(sBuf)

end function c_size_t2char

!--------------------------------------------------------------------------

!> Convert a real value into a formatted character string
function real2char(rValue, sFmt)  result(sBuf)

  real (kind=c_float) :: rValue
  character (len=*), optional :: sFmt

  ![ LOCALS ]
  character(len=64) :: sBuf, sFormat

  if (present(sFmt) ) then
    sFormat = "("//sFmt//")"
  else
    sFormat = "(F16.4)"
  endif

  write(UNIT=sBuf,FMT=TRIM(sFormat) ) rValue
  sBuf = ADJUSTL(sBuf)

end function real2char

!> Convert a double precision real value into a formatted character string
function dbl2char(rValue, sFmt)  result(sBuf)

  real (kind=c_double) :: rValue
  character (len=*), optional :: sFmt

  ![ LOCALS ]
  character(len=64) :: sBuf, sFormat

  if (present(sFmt) ) then
    sFormat = "("//sFmt//")"
  else
    sFormat = "(F16.4)"
  endif

  write(UNIT=sBuf,FMT=TRIM(sFormat) ) rValue
  sBuf = ADJUSTL(sBuf)

end function dbl2char

!--------------------------------------------------------------------------

elemental function deg2rad(rDeg) result(rRad)

  ! [ ARGUMENTS ]
  real (kind=c_double), intent(in) :: rDeg

  ! [ LOCALS ]
  real (kind=c_double) :: rRad

  rRad = dpPI / 180_c_double * rDeg

end function deg2rad

!--------------------------------------------------------------------------

elemental function rad2deg(rRad) result(rDeg)

  ! [ ARGUMENTS ]
  real (kind=c_double), intent(in) :: rRad

  ! [ LOCALS ]
  real (kind=c_double) :: rDeg

  rDeg = rRad * 180_c_double / dpPI

end function rad2deg

!--------------------------------------------------------------------------

!> @brief Return the number of days in the given year.
!>
!> This function simply returns the number of days given the current year.
function num_days_in_year(iYear) result(iNumDaysInYear)

  integer (kind=c_int), intent(in) :: iYear

  ! [ LOCALS ]
  integer (kind=c_int) :: iFirstDay, iLastDay, iNumDaysInYear

  iFirstDay = julian_day ( iYear, 1, 1 )
  iLastDay = julian_day ( iYear, 12, 31 )
  iNumDaysInYear = iLastDay - iFirstDay + 1

  return

end function num_days_in_year

!--------------------------------------------------------------------------

function day_of_year(iJD) result(iDOY)

  integer (kind=c_int), intent(in) :: iJD

  ! [ LOCALS ]
  integer (kind=c_int) :: iFirstDay, iLastDay, iDOY
  integer (kind=c_int) :: iMonth, iDay, iYear


  call gregorian_date(iJD, iYear, iMonth, iDay)
  iFirstDay = julian_day ( iYear, 1, 1 )

  iDOY = iJD - iFirstDay + 1

end function day_of_year

!--------------------------------------------------------------------------

function isLeap(iYear)   result(lResult)

  integer (kind=c_int), intent(in)     :: iYear
  logical (kind=c_bool) :: lResult

  lResult = ( mod(iYear, 4) == 0 .and. mod(iYear, 100) /= 0 ) .or. &
                 ( mod(iYear, 400) == 0 .and. iYear /= 0 )

end function isLeap

!--------------------------------------------------------------------------
! NAME
!   gregorian_date - Convert from a Julian day number to a Gregorian date.
!
! SYNOPSIS
!   Conversion to a Gregorian calendar date from a Julian date.
!   Valid for any Gregorian calendar date producing a Julian day number
!   greater than zero.
!
! INPUTS
!   iJD     integer number of days that have elapsed since noon
!           Greenwich Mean Time (UT or TT) Monday, January 1, 4713 BC
! OUTPUTS
!   iYear   4-digit year
!   iMonth  2-digit month (1-12)
!   iDay    2-digit day (1-31)
!
! NOTES
!   Reference: Fliegel, H. F. and van Flandern, T. C. (1968).
!   Communications of the ACM, Vol. 11, No. 10 (October, 1968).
!   Modified from code found at:
!       http://aa.usno.navy.mil/faq/docs/JD_Formula.html


!> Convert from a Julian day number to a Gregorian date.
!>
!> Converts from a Julian day number to a Gregorian date.
!> Valid for any Gregorian calendar date producing a Julian day number
!> greater than zero.
!>
!> Reference: Fliegel, H. F. and van Flandern, T. C. (1968).
!>            Communications of the ACM, Vol. 11, No. 10 (October, 1968).
!>
!>    Modified from code found at:
!>            http://aa.usno.navy.mil/faq/docs/JD_Formula.html
subroutine gregorian_date(iJD, iYear, iMonth, iDay, iOrigin)

  ! [ ARGUMENTS ]
  integer (kind=c_int) :: iJD
  integer (kind=c_int), intent(inout) :: iYear, iMonth, iDay
  integer (kind=c_int), optional :: iOrigin
  ! [ LOCALS ]
  integer (kind=c_int) iI,iJ,iK,iL,iN
  integer (kind=c_int) :: iOffset

  if(present(iOrigin)) then
    iOffset = iOrigin
  else
    iOffset = 0
  endif

  ! allow for an alternate "origin" to be specified... technically,
  ! this is no longer a "Julian" day, but alas... This modification
  ! was required in order to process the "time" variables from global
  ! climate models, which seem to be defined as something like this:
  ! time:units = "days since 1960-01-01 00:00:00"
  !
  ! for the above example, JD = 2436935 on the first day; the NetCDF "time"
  ! variable will be equal to 0.  Thus, in order to get the conversion
  ! right, we must add 0 + 2436935 to yield a true Julian Day.

  iJD = iJD + iOffset

  iL= iJD + 68569_c_int
  iN= 4*iL / 146097_c_int
  iL= iL - (146097_c_int * iN + 3_c_int)/4_c_int
  iI= 4000_c_int * (iL + 1_c_int) / 1461001_c_int
  iL= iL - 1461_c_int * iI / 4_c_int + 31_c_int
  iJ= 80_c_int * iL / 2447_c_int
  iK= iL - 2447_c_int * iJ / 80_c_int
  iL= iJ / 11_c_int
  iJ= iJ + 2_c_int - 12_c_int * iL
  iI= 100_c_int * (iN - 49_c_int) + iI + iL

  iYear = iI
  iMonth = iJ
  iDay = iK

end subroutine gregorian_date

  !> @}

function asCSV(sText)  result(sResult)

  character (len=*) :: sText
  character (len=256) :: sResult

   character (len=256 ) :: sRecord
  character (len=256) :: sItem


  sRecord = trim(adjustl(sText))

  sResult = ""
  sItem = ""

  do

    call chomp(sRecord, sItem)
    sResult = trim(sResult)//" "//trim(sItem)

    if(len_trim(sRecord) == 0 ) exit

    sResult = trim(sResult)//", "

  enddo

  sResult = adjustl(sResult)

end function asCSV

function char_ptr_to_fortran_string( cpCharacterPtr )  result(sText)

  use iso_c_binding
  implicit none

  type(c_ptr) :: cpCharacterPtr
  character(len=256) :: sText
  character (kind=c_char), pointer, dimension(:) :: fptr
  integer (kind=c_int) :: iCount

    sText = repeat(" ", 256)

    call c_f_pointer(cpCharacterPtr, fptr, [256])
    iCount = 0
    do
      iCount = iCount + 1
      if(index(string=fptr(iCount), substring=c_null_char) /= 0) exit
      sText(iCount:iCount) = fptr(iCount)

    enddo

end function char_ptr_to_fortran_string

function c_to_fortran_string( cCharacterString )  result(sText)

  use iso_c_binding
  implicit none

  character (len=*) :: cCharacterString
  character(len=256) :: sText
  integer (kind=c_int) :: iIndex

  sText = repeat(" ", 256)

  iIndex = index(string=cCharacterString, substring=c_null_char)

  if(iIndex == 0) then

    sText = trim(adjustl(cCharacterString))

  else

    sText = cCharacterString(1:iIndex-1)

  endif

end function c_to_fortran_string

function fortran_to_c_string( sText )  result(cCharacterString)

  use iso_c_binding
  implicit none

  character (len=*) :: sText
  character(len=len_trim(sText)+1) :: cCharacterString
  integer (kind=c_int) :: iIndex

  iIndex = index(string=sText, substring=c_null_char)

  if (iIndex == 0) then
    cCharacterString = trim(sText)//c_null_char
  else
    cCharacterString  = sText(1:iIndex)
  endif

end function fortran_to_c_string


end module types
