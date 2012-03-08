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

  implicit none

  character(len=16), public, parameter :: &
      SWB_VERSION = "1.0.1"

  !> Define the sizes of base types used in the model
  integer, public, parameter :: T_LOGICAL = 4
  integer, public, parameter :: T_INT = 4
  integer, public, parameter :: T_SHORT = 2
  integer, public, parameter :: T_BYTE = 1
  integer, public, parameter :: T_SGL_DISLIN = 4

  !> Define machine-independent sizes for base types
  !  integer*2, public, parameter :: T_SGL = SELECTED_REAL_KIND(p=6,r=37)
  integer, public, parameter :: T_SGL = SELECTED_REAL_KIND(p=6,r=37)
  integer, public, parameter :: T_DBL = SELECTED_REAL_KIND(p=13,r=200)

  !> @name Variables: Fortran logical unit numbers
  !> Global logical unit numbers for input and output
  !> @{
  integer (kind=T_INT), parameter :: LU_STD_OUT = 6
  integer (kind=T_INT), parameter :: LU_CONTROL = 12
  integer (kind=T_INT), parameter :: LU_GRID = 14
  integer (kind=T_INT), parameter :: LU_LOOKUP = 16
  integer (kind=T_INT), parameter :: LU_TS = 18
  integer (kind=T_INT), parameter :: LU_TEMP = 20
  integer (kind=T_INT), parameter :: LU_MSB_REPORT = 22
  integer (kind=T_INT), parameter :: LU_BIN_FILE = 24
  integer (kind=T_INT), parameter :: LU_ROUTING = 26
  integer (kind=T_INT), parameter :: LU_CSV_MIN = 28
  integer (kind=T_INT), parameter :: LU_CSV_MEAN = 30
  integer (kind=T_INT), parameter :: LU_CSV_MAX = 32
  integer (kind=T_INT), parameter :: LU_CSV_ANNUAL = 34
  integer (kind=T_INT), parameter :: LU_MASK = 36
  integer (kind=T_INT), parameter :: LU_MASK_FILE = 38
  integer (kind=T_INT), parameter :: LU_MASK_STATS_CSV = 40
  integer (kind=T_INT), parameter :: LU_PEST_STATS = 42
  integer (kind=T_INT), parameter :: LU_PEST_OBS = 44
  integer (kind=T_INT), parameter :: LU_PEST_INS = 46
  integer (kind=T_INT), parameter :: LU_LOG = 48
  !> @}

  !> @name Constants: General conversion factors and flags
  !> Some useful and common conversion factors, defined here
  !> to ensure consistency throughout the code
  !> @{
  real (kind=T_SGL), parameter :: rZERO = 0.0_T_SGL
  real (kind=T_DBL), parameter :: dpZERO = 0.0_T_DBL
  real (kind=T_SGL), parameter :: rNEAR_ZERO = 1E-9_T_SGL
  real (kind=T_SGL), parameter :: rPOINT2 = 0.2_T_SGL
  real (kind=T_SGL), parameter :: rHALF = 0.5_T_SGL
  real (kind=T_SGL), parameter :: rPOINT8 = 0.8_T_SGL
  real (kind=T_SGL), parameter :: rONE = 1.0_T_SGL
  real (kind=T_DBL), parameter :: dpONE = 1.0_T_DBL
  real (kind=T_SGL), parameter :: rFREEZING = 32.0_T_SGL
  real (kind=T_SGL), parameter :: rTEN = 10.0_T_SGL
  real (kind=T_SGL), parameter :: rHUNDRED = 100.0_T_SGL
  real (kind=T_SGL), parameter :: rTHOUSAND = 1000.0_T_SGL
  real (kind=T_DBL), parameter :: dpC_PER_F = 5.0_T_DBL / 9.0_T_DBL
  real (kind=T_DBL), parameter :: dpF_PER_C = 9.0_T_DBL / 5.0_T_DBL
  real (kind=T_SGL), parameter :: rM_PER_FOOT = 0.3048_T_SGL
  real (kind=T_SGL), parameter :: rMM_PER_INCH = 25.4_T_SGL
  real (kind=T_SGL), parameter :: rCM_PER_INCH = 2.54_T_SGL
  real (kind=T_DBL), parameter :: dpPI = 3.14159265_T_DBL
  real (kind=T_DBL), parameter :: dpTWOPI = 2.0_T_DBL * dpPI
  real (kind=T_DBL), parameter :: dpSQM_to_SQFT = 10.76391_T_DBL
  integer (kind=T_INT), parameter :: iMOVING_AVG_TERMS = 5
  integer (kind=T_INT), parameter :: iROUTE_CELL_MARKED = -1
  integer (kind=T_INT), parameter :: iROUTE_DEPRESSION = -999
  integer (kind=T_INT), parameter :: iROUTE_LEFT_GRID = -1000
  integer (kind=T_INT), parameter :: iNO_DATA_NCDC = -99999
  integer (kind=T_INT), parameter :: iNUM_DIGITS = 3
  integer (kind=T_INT), parameter :: iFIELD_WIDTH = 10
  integer (kind=T_INT), parameter :: iZERO = 0
  logical (kind=T_LOGICAL), parameter :: lTRUE = .true._T_LOGICAL
  logical (kind=T_LOGICAL), parameter :: lFALSE = .false._T_LOGICAL
  integer(kind=T_INT), parameter :: iEOF = HUGE(iZERO)
  real (kind=T_SGL), parameter :: rBIGVAL = HUGE(rZERO)
  character (len=1), parameter :: sTAB = achar(9)
  character (len=2), parameter :: sWHITESPACE = achar(9)//" "
  character (len=1), parameter :: sBACKSLASH = achar(92)
  character (len=1), parameter :: sFORWARDSLASH = achar(47)
  !> @}

  !> @name Globals: Variables for run-length encoding operation
  !> These variables store the position of the start date and
  !> end date within the binary (stream) output file.
  !> @{
  integer(kind=T_INT) :: iSTARTDATE_POS
  integer(kind=T_INT) :: iENDDATE_POS
  integer(kind=T_INT) :: iENDHEADER_POS
  !> @}


  integer (kind=T_INT), parameter :: iNC_INPUT = 1
  integer (kind=T_INT), parameter :: iNC_OUTPUT = 2
  integer (kind=T_INT), parameter :: iNC_OBSERVATION = 3


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
!> consumes Ny * Nx * size(T_SGL) bytes.
  type T_CELL
      integer (kind=T_SHORT) :: iFlowDir = iZERO    ! Flow direction from flow-dir grid
      integer (kind=T_SHORT) :: iSoilGroup = iZERO  ! Soil type from soil-type grid
      integer (kind=T_SHORT) :: iLandUseIndex       ! Index (row num) of land use table
      integer (kind=T_SHORT) :: iLandUse = iZERO    ! Land use from land-use grid
      real (kind=T_SGL) :: rElevation =rZERO        ! Ground elevation
      real (kind=T_SGL) :: rSoilWaterCapInput = rZERO   ! Soil water capacity from grid file
      real (kind=T_SGL) :: rSoilWaterCap =rZERO     ! Soil water capacity adjusted for LU/LC
      real (kind=T_SGL) :: rSoilMoisture = rZERO    ! Soil moisture in inches of water
      real (kind=T_SGL) :: rREW
      real (kind=T_SGL) :: rTEW

      real (kind=T_SGL) :: rSoilMoisturePct         ! Soil moisture as percentage of water capacity
      real (kind=T_SGL) :: rSM_AccumPotentWatLoss   ! Accumulated potential water loss

      real (kind=T_SGL) :: rMaxRecharge             ! Maximum groundwater recharge rate

      real (kind=T_SGL) :: rSM_PotentialET          ! Potential ET
!      real (kind=T_SGL) :: rSM_ActualET            ! Actual Evapotranspiration

!if_defined STREAM_INTERACTIONS
      integer (kind=T_INT) :: iStreamIndex = iZERO  ! ID of the fracture capture lookup table
      real (kind=T_SGL) :: rStreamCapture           ! Amount of water captured by the stream (or fracture)
!end_if

      integer (kind=T_INT) :: iTgt_Row   ! Row: "i" index of target cell into which runoff flows
      integer (kind=T_INT) :: iTgt_Col   ! Col: "j" index of target cell into which runoff flows

      real (kind=T_SGL) :: rBaseCN                 ! Curve number from landuse/soil group
      real (kind=T_SGL) :: rAdjCN                  ! Curve number from landuse/soil group
      real (kind=T_SGL) :: rSMax                   ! S_max parameter from runoff calculation
      real (kind=T_SGL) :: rInFlow                 ! flow in from uphill
      real (kind=T_SGL) :: rOutFlow                ! flow out downhill
      real (kind=T_SGL) :: rFlowOutOfGrid          ! flow that leaves the grid
      real (kind=T_SGL) :: rRouteFraction = rONE   ! Fraction of outflow to route downslope
      real (kind=T_SGL) :: rGrossPrecip            ! Precip - no interception applied
!      real (kind=T_SGL) :: rInterception           ! Interception term
      real (kind=T_SGL) :: rNetPrecip              ! Net precipitation - precip minus interception
      real (kind=T_SGL) :: rSnowFall_SWE           ! precipitation that falls as snow (in SWE)
      real (kind=T_SGL) :: rSnowFall               ! snowfall in inches as SNOW
      real (kind=T_SGL) :: rSnowCover              ! snowcover expressed as inches of water
      real (kind=T_SGL) :: rSnowTemperature = 23. ! snow temperature
!      real (kind=T_SGL) :: rPrevious_SnowCover     ! Previous day's snow cover
      real (kind=T_SGL) :: rSnowMelt               ! snowmelt in inches of water
      real (kind=T_SGL) :: rTMin                   ! Minimum daily temperature
      real (kind=T_SGL) :: rTMax                   ! Maximum daily temperature
      real (kind=T_SGL) :: rTAvg                   ! Average daily temperature
      real (kind=T_SGL) :: rCFGI = rZERO           ! Continuous Frozen Ground Index

      real (kind=T_SGL) :: rGDD_TBase = 50.        !
      real (kind=T_SGL) :: rGDD_TMax = 150.        !
      real (kind=T_SGL) :: rGDD = rZERO            ! Growing Degree Day
      real (kind=T_SGL) :: rIrrigationAmount       ! total amount of any irrigation
      real (kind=T_SGL) :: rIrrigationFromGW = rZERO ! term to hold irrigation term, if any
      real (kind=T_SGL) :: rIrrigationFromSW = rZERO ! term to hold irrigation term, if any
      real (kind=T_SGL) :: rMaximumAllowableDepletion = 100_T_SGL ! by default, no irrigation
                                                                  ! will be performed

      real (kind=T_SGL) :: rSnowAlbedo             ! Snow albedo value
      integer (kind=T_INT) :: iDaysSinceLastSnow = 0  ! Number of days since last snowfall
!      real (kind=T_SGL) :: rNetInfil               ! NetPrecip + InFlow + SnowMelt - OutFlow
      real (kind=T_SGL),dimension(iMOVING_AVG_TERMS) :: rNetInflowBuf  ! Inflow buffer for moving avg
      real (kind=T_SGL) :: rDailyRecharge          ! Daily recharge
      real (kind=T_SGL) :: rSUM_Recharge = rZERO   ! SUM of all daily recharge values for entire run
      real (kind=T_SGL) :: rSUM_RejectedRecharge = rZERO   ! SUM of all daily rejected recharge values for entire run
      real (kind=T_SGL) :: rMSB                    ! cellular mass balance
      integer(kind=T_SHORT) :: iNumFilesSSF = 0    ! number of SSF files associated with grid cell

      logical (kind=T_LOGICAL) :: lDownhillMarked   ! Has been marked for downhill solution
  end type T_CELL

  ! Generic grid data type identifier constants
  integer (kind=T_INT), public, parameter :: T_INT_GRID = 0       ! Integer data
  integer (kind=T_INT), public, parameter :: T_SGL_GRID = 1       ! Real data
  integer (kind=T_INT), public, parameter :: T_CELL_GRID = 2       ! SWB cell data

!> @brief Type that contains the data for a grid.
!>
!>   Type that contains the data for a grid. Depending on the need of the
!>   application code, the grid may contain integer, real, or cell-by-cell
!>   data. This 'generic' data type makes many data validation problems
!>   simpler, but at the cost of two unpopulated pointer variables per grid.
!>   This implements a coding mechanism comparable to templates in C++.!
  type T_GENERAL_GRID
!      integer (kind=T_INT) :: iGridType            ! One of the grid type options above
      integer (kind=T_INT) :: iNX                   ! Number of cells in the x-direction
      integer (kind=T_INT) :: iNY                   ! Number of cells in the y-direction
      integer (kind=T_INT) :: iNumGridCells         ! Total number of grid cells
      integer (kind=T_INT) :: iDataType             ! Type of the grid
      real (kind=T_SGL) :: rGridCellSize            ! size of one side of a grid cell
      integer (kind=T_INT) :: iLengthUnits= -99999  ! length units code
      real (kind=T_DBL) :: rX0, rX1              ! World-coordinate range in X
      real (kind=T_DBL) :: rY0, rY1              ! World-coordinate range in Y
      integer (kind=T_INT), dimension(:,:), pointer :: iData ! Integer data
      real (kind=T_SGL), dimension(:,:), pointer :: rData    ! Real data
      type (T_CELL), dimension(:,:), pointer :: Cells        ! T_CELL objects
  end type T_GENERAL_GRID


  !> define parameter values for working with type T_GENERAL_GRID
  integer (kind=T_INT), parameter :: iGRID_LENGTH_UNITS_METERS = 0
  integer (kind=T_INT), parameter :: iGRID_LENGTH_UNITS_FEET = 1

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
	integer (kind=T_INT) :: iLandUseType
    !> Land use description
    character (len=256) :: sLandUseDescription
    !> Assumed percent imperviousness (not used in any calculations)
	character (len=256) :: sAssumedPercentImperviousness
    !> Interception value (inches per day) during growing season
	real (kind=T_SGL) :: rIntercept_GrowingSeason
    !> Interception value (inches per day) outside of growing season
	real (kind=T_SGL) :: rIntercept_NonGrowingSeason
!	logical (kind=T_LOGICAL) :: lCONSTANT_ROOT_ZONE_DEPTH
!    real (kind=T_SGL), dimension(iNUM_ROOT_ZONE_PAIRS) :: rX_ROOT_ZONE
!    real (kind=T_SGL), dimension(iNUM_ROOT_ZONE_PAIRS) :: rY_ROOT_ZONE
  end type T_LANDUSE_LOOKUP

  !> @brief Type that contains information needed to calculate irrigation for
  !> each land use.
  !>
  !> Type that contains information needed to estimate crop coefficients and
  !> calculate irrigation for each land use.
  type T_IRRIGATION_LOOKUP

    !> Landuse code corresponding to the codes specified in landuse grid
 	integer (kind=T_INT) :: iLandUseType

    !> Land use description
    character (len=256) :: sLandUseDescription

    !> Plant or crop description
    character (len=256) :: sCropDescription

    !> Mean plant or crop height, feet
    real (kind=T_SGL) :: rMeanPlantHeight

    !> Crop coefficient, basal, for a given day
    real (kind=T_SGL) :: rKcb

    !> Crop coefficient, basal, initial growth phase (Kcb_ini)
    real (kind=T_SGL) :: rKcb_ini = 0.15

    !> Crop coefficient, basal, mid-growth phase (Kcb_mid)
    real (kind=T_SGL) :: rKcb_mid = 1.0

    !> Crop coefficient, basal, end-growth phase (Kcb_end)
    real (kind=T_SGL) :: rKcb_end = 0.7

    !> Crop coefficient, MINIMUM allowed value (Kc_min)
    real (kind=T_SGL) :: rKc_min = 0.02

    !> Crop coefficient, MAXIMUM allowed value (Kc_max)
    real (kind=T_SGL) :: rKc_max = 1.3

    !> Day of year (or GDD) for initial planting
    integer (kind=T_INT) :: iL_plant

    !> Day of year (or GDD) for end of initial growth phase
    integer (kind=T_INT) :: iL_ini

    !> Day of year (or GDD) for end of development phase
    integer (kind=T_INT) :: iL_dev

    !> Day of year (or GDD) for end of mid-season growth phase
    integer (kind=T_INT) :: iL_mid

    !> Day of year (or GDD) for end of late season growth phase
    integer (kind=T_INT) :: iL_late

    !> How should the growth phase identifiers be treated (DOY or GDD)
    logical (kind=T_LOGICAL) :: lUnitsAreDOY

    !> Growing degree-day base temperature (10 degrees C for corn)
    real (kind=T_SGL) :: rGDD_BaseTemp  = 50.

    !> Growing degree-day maximum temperature (cutoff; 30 degrees C for corn)
    real (kind=T_SGL) :: rGDD_MaxTemp   = 130.

    ! Maximum allowable depletion (MAD) = maximum allowed soil water depletion (in percent)
    real (kind=T_SGL) :: rMAD           = 100.

    !> Day of year before which no irrigation is assumed to take place
    integer (kind=T_INT) :: iBeginIrrigation = 120

    !> Day of year after which no irrigation is assumed to take place
    integer (kind=T_INT) :: iEndIrrigation = 240

    !> Fraction of irrigation water obtained from GW rather than surface water
    real (kind=T_SGL) :: rFractionOfIrrigationFromGW = rONE

    !> Fraction of exposed and wetted soil (f_ew)
    real (kind=T_SGL) :: r_f_ew

  end type T_IRRIGATION_LOOKUP

  !> container for basin mask table data
  type T_BASIN_MASK
    character(len=256) :: sUSGS_UpstreamOrderID
    character(len=256) :: sBasinDescription
    character(len=256) :: sBasinMaskFilename
    character(len=256) :: sFileType
    real (kind=T_SGL) :: rPestWeight = 1.0
    character (len=256) :: sPestGroup

    type(T_GENERAL_GRID), pointer :: pGrd

    real (kind=T_SGL) :: rQb
    real (kind=T_SGL) :: rDrainageArea
    integer (kind=T_INT) :: iLENGTH
    real (kind=T_SGL) :: rMIN
    real (kind=T_SGL) :: rMAX
    real (kind=T_SGL) :: rMEAN
  end type T_BASIN_MASK

  !> container for SSF file information
  type T_SSF_FILES
    integer (kind=T_INT) :: iLU      ! Fortran logical unit #
    character (len=128) :: sFileName
    integer (kind=T_INT) :: iRowNum
    integer (kind=T_INT) :: iColNum
    integer (kind=T_INT) :: iVarNum  ! T_STATS variable number
  end type T_SSF_FILES

!if_defined NETCDF_SUPPORT
  type T_NETCDF_FILE
    integer(kind=T_INT) :: iNCID
    character(len=64)  :: sFilename
    character(len=24)  :: sVarName = ""
    character(len=24) :: sUnits
    integer(kind=T_INT) :: iProjID
!    integer(kind=T_INT) :: iCRSID
    integer(kind=T_INT) :: iVarID
    integer(kind=T_INT) :: iVarType
!    integer(kind=T_INT) :: iLatVarID
!    integer(kind=T_INT) :: iLonVarID
    integer(kind=T_INT) :: iXVarID
    integer(kind=T_INT) :: iYVarID
    logical(kind=T_LOGICAL) :: lYVarBeforeXVar = lTRUE
    integer(kind=T_INT) :: iXDimID
    integer(kind=T_INT) :: iYDimID
    integer(kind=T_INT) :: iTimeDimID
    integer(kind=T_INT) :: iTimeVarID
    integer(kind=T_INT) :: iOriginMonth
    integer(kind=T_INT) :: iOriginDay
    integer(kind=T_INT) :: iOriginYear
    integer(kind=T_INT) :: iOriginJulianDay
    integer(kind=T_INT) :: iStartJulianDay
    integer(kind=T_INT) :: iEndJulianDay
    real(kind=T_DBL) :: rX_LowerLeft
    real(kind=T_DBL) :: rY_LowerLeft
    real(kind=T_DBL) :: rX_UpperRight
    real(kind=T_DBL) :: rY_UpperRight
    real(kind=T_SGL) :: rGridCellSize
    integer(kind=T_INT) :: iX_NumGridCells
    integer(kind=T_INT) :: iY_NumGridCells
    logical(kind=T_LOGICAL) :: lInterpolate = lFALSE
    real (kind=T_DBL) :: rScaleFactor = 1_T_DBL
    real (kind=T_DBL) :: rAddOffset = 0_T_DBL
    character(len=24) :: sProjectionName = "wtm"
    character(len=12) :: sProjectionUnits = "meters"
    real (kind=T_DBL) :: rFalseEasting = 520000_T_DBL      ! defaults for WTM 83/91
    real (kind=T_DBL) :: rFalseNorthing = -4480000_T_DBL   ! defaults for WTM 83/91
    real (kind=T_DBL) :: rLongOrigin = -90_T_DBL     ! defaults for WTM 83/91
    real (kind=T_DBL) :: rLatOrigin = 0_T_DBL        ! defaults for WTM 83/91
    integer (kind=T_INT) :: iZoneNumber = 0
    character(len=48) :: sEllipsoidName = "GRS 1980"
    integer (kind=T_INT) :: iEllipsoidID = 11     ! defaults for WTM 83/91
  end type T_NETCDF_FILE
!end_if

  !> Container for calendar lookup information
  type T_MONTH
      ! Container for calendar lookup information
    character (len=3) :: sName          ! Abbreviated name
	 character (len=9) :: sFullName      ! Full month name
    integer (kind=T_INT) :: iStart      ! Starting (Julian) date
    integer (kind=T_INT) :: iEnd        ! Ending (Julian) date
	 integer (kind=T_INT) :: iMonth      ! Month number (1-12)
    integer (kind=T_INT) :: iNumDays    ! Max number of days in month
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
    integer(kind=T_INT) :: iLU
    !> Integer number of characters to indent (on-screen display)
    integer (kind=T_INT) :: iIndent
    !> Multiplier for calculating mass balances (1=source, 0=NA, -1=sink)
    integer(kind=T_INT) :: iMassBalanceConst
    !> Does it make sense to show the sum of this accumulator?
    logical(kind=T_LOGICAL) :: lShowSum
    !> Does it make sense to show the daily sum of this accumulator?
    logical(kind=T_LOGICAL) :: lShowDailySum
    !> Is this variable active given the compiler options?
    logical (kind=T_LOGICAL) :: lActive
    !> Text describing the unit of measure (i.e. inches)
    character (len=20) :: sUnits
    !> Long form of the variable name
    character(len=64) :: sLongName
    !> Multiplication factor when writing to a NetCDF file
    real (kind=T_DBL) :: rNC_MultFactor
    !> Offset when writing to a NetCDF file
    real (kind=T_DBL) :: rNC_AddOffset
    !> Type of daily output desired (0=none; 1=grid; 2=plot; 3=both; 4=stats)
    integer(kind=T_INT) :: iDailyOutput
    !> Type of monthly output desired (0=none; 1=grid; 2=plot; 3=both; 4=stats)
    integer(kind=T_INT) :: iMonthlyOutput
    !> Type of annual output desired (0=none; 1=grid; 2=plot; 3=both; 4=stats)
    integer(kind=T_INT) :: iAnnualOutput
    !> Is output to be written to a NetCDF file?
    integer(kind=T_INT) :: iNetCDFOutput
    !> Description of utility of variable in calculating mass balance
    character (len=24) :: sMSB_Note
    !> Offset value; contains number of bytes written for a given day
    integer (kind=T_INT) :: iOffset
    !> Position value; location of current day's position marker
    integer (kind=T_INT) :: iPos
  end type T_STATS

  !> Global parameter defining the number of elements in the YEAR_INFO array.
  integer (kind=T_INT), parameter :: iNUM_MONTHS = 12
  integer(kind=T_INT), parameter :: iNUM_VARIABLES = 29

  ! constants defining T_STATS output types
  integer(kind=T_INT), parameter :: iNONE = 0
  integer(kind=T_INT), parameter :: iGRID = 1
  integer(kind=T_INT), parameter :: iGRAPH = 2
  integer(kind=T_INT), parameter :: iBOTH = 3
  integer(kind=T_INT), parameter :: iSTATS = 4

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
      lFALSE, 'inches','daily estimated irrigation amount', &
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

    T_STATS ('NET_PRECIP',0,2,0,lTRUE,lTRUE, lTRUE, &
      'inches','gross precipitation minus interception', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('NET_INFLOW',0,2,0,lTRUE,lTRUE, lTRUE, &
      'inches','sum of net precip and inflow', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('NET_INFIL',0,2,0,lTRUE,lTRUE, lTRUE, &
      'inches','precip and inflow minus outflow', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0), &

    T_STATS ('POT_ET',0,2,0,lTRUE,lTRUE, lTRUE, &
      'inches','potential evapotranspiration', &
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

    T_STATS ('GDD',0,2,0,lFALSE,lFALSE, &
#ifdef IRRIGATION_MODULE
      lTRUE, &
#else
      lFALSE, &
#endif
      'degree-day','growing degree day', &
      1.,0.0,iNONE,iNONE,iNONE,iNONE,'info',0,0) ]

  !> @anchor const_stat
  !> @name Constants: Statistics that SWB knows how to calculate and store
  !> Index values for members of the rMonthly, rDaily, and rAnnual arrays
  !> that are global variables within module swb_stats
  !> @{
  integer (kind=T_INT), parameter :: iNUM_STATS = 5
  integer (kind=T_INT), parameter :: iMIN = 1
  integer (kind=T_INT), parameter :: iMEAN = 2
  integer (kind=T_INT), parameter :: iMAX = 3
  integer (kind=T_INT), parameter :: iSUM = 4
  integer (kind=T_INT), parameter :: iLENGTH = 5
  character(len=7), dimension(iNUM_STATS), parameter :: STAT_NAMES = &
    [ "Minimum", "Mean   ", "Maximum", "Sum    ", "Length " ]
  !> @}

  !> @name Constants: Variables that SWB can output or summarize
  !> Index values for members of the rMonthly, rDaily, and rAnnual arrays
  !> that are global variables within module swb_stats
  !> @note These constants are arranged in the desired order of output
  !> for daily and annual reporting: sources first, then the sinks,
  !> and then all other informational items
  !> @{
  integer (kind=T_INT), parameter :: iGROSS_PRECIP = 1
  integer (kind=T_INT), parameter :: iSNOWMELT = 2
  integer (kind=T_INT), parameter :: iINFLOW = 3
  integer (kind=T_INT), parameter :: iIRRIGATION = 4
  integer (kind=T_INT), parameter :: iSNOWFALL_SWE = 5
  integer (kind=T_INT), parameter :: iINTERCEPTION = 6
  integer (kind=T_INT), parameter :: iOUTFLOW = 7
  integer (kind=T_INT), parameter :: iRUNOFF_OUTSIDE = 8
  integer (kind=T_INT), parameter :: iACT_ET = 9
  integer (kind=T_INT), parameter :: iCHG_IN_SOIL_MOIST = 10
  integer (kind=T_INT), parameter :: iRECHARGE = 11
  integer (kind=T_INT), parameter :: iREJECTED_RECHARGE = 12
  integer (kind=T_INT), parameter :: iSTREAM_CAPTURE = 13
  integer (kind=T_INT), parameter :: iSNOWCOVER = 14
  integer (kind=T_INT), parameter :: iCFGI = 15
  integer (kind=T_INT), parameter :: iMIN_TEMP = 16
  integer (kind=T_INT), parameter :: iMAX_TEMP = 17
  integer (kind=T_INT), parameter :: iAVG_TEMP = 18
  integer (kind=T_INT), parameter :: iCHG_IN_SNOW_COV = 19
  integer (kind=T_INT), parameter :: iNET_PRECIP = 20
  integer (kind=T_INT), parameter :: iNET_INFLOW = 21
  integer (kind=T_INT), parameter :: iNET_INFIL = 22
  integer (kind=T_INT), parameter :: iPOT_ET = 23
  integer (kind=T_INT), parameter :: iP_MINUS_PET = 24
  integer (kind=T_INT), parameter :: iSM_DEFICIT = 25
  integer (kind=T_INT), parameter :: iSM_SURPLUS = 26
  integer (kind=T_INT), parameter :: iSM_APWL = 27
  integer (kind=T_INT), parameter :: iSOIL_MOISTURE = 28
  integer (kind=T_INT), parameter :: iGDD = 29

#ifdef STREAM_INTERACTIONS
  ! The maximum number of fracture recharge entries
  integer (kind=T_INT), parameter :: STREAM_INTERACTIONS_MAX = 100
#endif

  !> generic configuration
  integer (kind=T_INT), parameter :: CONFIG_NONE = -999

  !> @}

  !> @brief Type that contains model configuration data.
  !>
  !> The configuration data structure is passed by pointer throughout
  !> the SWB model modules. Most of the functioning of SWB may be controlled
  !> by changing the configuration options prior to a run.
  type T_MODEL_CONFIGURATION

      !> Runoff calculation method @ref const_runoffCalc "(\em see defined constants)"
      integer (kind=T_INT) :: iConfigureRunoff = CONFIG_NONE
      !> Runoff routing solution mode @ref const_runoffSoln "(\em see defined constants)"
      integer (kind=T_INT) :: iConfigureRunoffMode = CONFIG_NONE
      !> Reference evapotranspiration calculation method
      integer (kind=T_INT) :: iConfigureET = CONFIG_NONE
      !> Precipitation input option
      integer (kind=T_INT) :: iConfigurePrecip = CONFIG_NONE
      !> Temperature data input option
      integer (kind=T_INT) :: iConfigureTemperature = CONFIG_NONE
      !> Landuse data input option
      integer (kind=T_INT) :: iConfigureLanduse = CONFIG_NONE
      !> Soil moisture calculation option
      integer (kind=T_INT) :: iConfigureSM = CONFIG_NONE
      !> Snowfall and snowmelt option
      integer (kind=T_INT) :: iConfigureSnow = CONFIG_NONE
      !> Maximum soil water capacity option
      integer (kind=T_INT) :: iConfigureSMCapacity = CONFIG_NONE
      !> Irrigation calculation option
      integer (kind=T_INT) :: iConfigureIrrigation = CONFIG_NONE
      !> Initial abstraction method: use 0.2S or 0.05S as estimate of initial abstraction
      integer (kind=T_INT) :: iConfigureInitialAbstraction = CONFIG_NONE
      !> Option to write extra files when using PEST
      logical (kind=T_LOGICAL) :: lWriteExtraPestFiles = lFALSE

      !> flag indicating whether or not screen output should include
      !> ANSI.sys-like colors
      logical (kind=T_LOGICAL) :: lANSI_Colors = lFALSE

      !> allow for alternate methods of delimiting subdirectories;
      !> needed for operation on Windows *and* Linux platforms
      character(len=1) :: sSlash = sBACKSLASH

      integer(kind=T_INT) :: iRLE_MULT = 10000
      real (kind=T_SGL) :: rRLE_OFFSET = 0.

      !> flag indicating whether this is the first year of a
      !> multiple-year simulation
      logical (kind=T_LOGICAL) :: lFirstYearOfSimulation  = lTRUE
      logical (kind=T_LOGICAL) :: lFirstDayOfSimulation  = lTRUE
      integer (kind=T_INT) :: iStartYear
      integer (kind=T_INT) :: iEndYear
      integer (kind=T_INT) :: iYear           ! keep track of current
      integer (kind=T_INT) :: iMonth          ! simulation date by
      integer (kind=T_INT) :: iDay            ! updating these values
      integer (kind=T_INT) :: iDayOfYear
      integer (kind=T_INT) :: iNumDaysInYear
      integer (kind=T_INT) :: iStartJulianDay
      integer (kind=T_INT) :: iCurrentJulianDay
      integer (kind=T_INT) :: iEndJulianDay
      integer (kind=T_INT) :: iStartYearforCalculation = -99999
      integer (kind=T_INT) :: iEndYearforCalculation = 99999
      integer (kind=T_INT) :: iNumberOfLanduses
      integer (kind=T_INT) :: iNumberOfSoilTypes
      logical (kind=T_LOGICAL) :: lGriddedData = lFALSE
      logical (kind=T_LOGICAL) :: lUseSWBRead = lFALSE
      logical (kind=T_LOGICAL) :: lHaltIfMissingClimateData = lTRUE

      ! flag indicating whether a previous downhill routing table exists
      logical (kind=T_LOGICAL) :: lDownhillRoutingTableExists = lFALSE

      ! Prefix of input ARC or SURFER gridded precip time-series files
      character (len=256) :: sPrecipFilePrefix

      ! Prefix of input ARC or SURFER gridded temperature time-series files
      character (len=256) :: sTMAXFilePrefix
      character (len=256) :: sTMINFilePrefix

      ! Prefix of the input ARC of SURFER gridded DYNAMIC LANDUSE files
      character (len=256) :: sDynamicLanduseFilePrefix

      ! ET parameters
      real (kind=T_SGL) :: rET_Slope = 0.0023    ! default is for Hargreaves (1985) method
      real (kind=T_SGL) :: rET_Exponent = 0.5
      real (kind=T_SGL) :: rET_Constant = 17.8

      ! precip correction factors
      real (kind=T_SGL) :: rRainfall_Corr_Factor = 1.0
      real (kind=T_SGL) :: rSnowFall_SWE_Corr_Factor = 1.0

      ! minimum value for valid precip  and temperature data
      real (kind=T_SGL) :: rMinValidPrecip = 0.
      real (kind=T_SGL) :: rMinValidTemp = -100.0

      ! define temperature values at which precip is all rain or all snow
      real (kind=T_SGL) :: rTMaxAllSnow = 30.5
      real (kind=T_SGL) :: rTMaxAllRain = 40.0

      ! SNOW DEPTH parameters for fresh snow
      real (kind=T_SGL) :: rSNWD_slp1 = 51.3
      real (kind=T_SGL) :: rSNWD_intcp1 = 67.9
      real (kind=T_SGL) :: rSNWD_denom = 2.6

      ! Filename for standard (single-station) time-series file
      character (len=256) :: sTimeSeriesFilename

      ! Filename for land use lookup table
      character (len=256) :: sLanduseLookupFilename

      ! Filename for irrigation lookup table
      character (len=256) :: sIrrigationLookupFilename

      ! Filename for basin mask table
      character (len=256) :: sBasinMaskFilename

      ! Target prefixes for output files
      character (len=256) :: sOutputFilePrefix = ""
      character (len=256) :: sFutureFilePrefix = ""

      ! Target suffixes for output files
      character (len=256) :: sOutputFileSuffix

      ! Precipitation amounts describing antecedent runoff conditions
      real (kind=T_SGL) :: rDRY_DORMANT = 0.50_T_SGL   ! shift to Type I
      real (kind=T_SGL) :: rWET_DORMANT = 1.10_T_SGL   ! shift to Type II
      real (kind=T_SGL) :: rDRY_GROWING = 1.40_T_SGL   ! shift to Type I
      real (kind=T_SGL) :: rWET_GROWING = 2.10_T_SGL   ! shift to Type II

      ! Configuration flag for type of grid output to write to disk
      ! output file is either ARC_GRID or SURFER
      integer (kind=T_INT) :: iOutputFormat

      ! Growing season configuration
      integer (kind=T_INT) :: iDayOfFirstFrost
      integer (kind=T_INT) :: iDayOfLastFrost
      logical (kind=T_LOGICAL) :: lNorthernHemisphere = lTRUE

      ! flags to control output
      logical (kind=T_LOGICAL) :: lReportDaily       !
      logical (kind=T_LOGICAL) :: lWriteToScreen     !

      integer (kind=T_INT) :: iHeaderPrintInterval ! frequency with which to reprint
                                                   ! detailed output header

      ! Define how low iteration-to-iteration variance must be
      ! before the iterative approach is determined to have converged
      real (kind=T_SGL) :: rIterationTolerance = 1.0E-6

      ! define a pointer to the landuse lookup table
      type (T_LANDUSE_LOOKUP), dimension(:), pointer :: LU  ! T_LANDUSE_LOOKUP objects

      ! define a pointer to the IRRIGATION lookup table
      type (T_IRRIGATION_LOOKUP), dimension(:), pointer :: IRRIGATION

      ! define a pointer to the BASIN MASK lookup table
      type (T_BASIN_MASK), dimension(:), pointer :: BMASK  ! T_BASIN_MASK objects

      ! define a pointer to the CURVE NUMBER lookup table
      real (kind=T_SGL), dimension(:,:), pointer :: CN

      ! define a pointer to the MAXIMUM RECHARGE RATE lookup table
      real (kind=T_SGL), dimension(:,:),pointer :: MAX_RECHARGE

      ! define a pointer to the ROOTING DEPTH lookup table
      real (kind=T_SGL), dimension(:,:),pointer :: ROOTING_DEPTH

      ! define a pointer to the READILY_EVAPORABLE_WATER lookup table
      real (kind=T_SGL), dimension(:,:),pointer :: READILY_EVAPORABLE_WATER

      ! define a pointer to the TOTAL_EVAPORABLE_WATER lookup table
      real (kind=T_SGL), dimension(:,:),pointer :: TOTAL_EVAPORABLE_WATER

      ! define threshold value for CFGI below which ground is considered unfrozen
      real (kind=T_SGL) :: rLL_CFGI = 9999.0
      ! define threshold value for CFGI above which ground is considered frozen
      real (kind=T_SGL) :: rUL_CFGI = 9999.0

      ! define the land use category associated with open water
      integer(kind=T_INT) :: iOPEN_WATER_LU = -99999

      ! define southern and northern latitude values bounding the grid
	  real (kind=T_SGL) :: rSouthernLatitude
 	  real (kind=T_SGL) :: rNorthernLatitude

#ifdef STREAM_INTERACTIONS
 	    ! Data for the elevation corrections on temperature
 	    logical (kind=T_LOGICAL) :: lElevAdjustment
 	    real (kind=T_SGL) :: rElevStationElevation
 	    real (kind=T_SGL) :: rElevDryFactor
 	    real (kind=T_SGL) :: rElevHumidFactor
 	    real (kind=T_SGL) :: rElevHumidityThreshold
#endif

       ! data structure to hold information about which cells we
       ! want to write our SSF files for
       type (T_SSF_FILES), dimension(:), pointer :: SSF_FILES

#ifdef NETCDF_SUPPORT
      ! define a pointer to structure holding NETCDF file information
      type (T_NETCDF_FILE), dimension(:,:), pointer :: NETCDF_FILE  ! NETCDF objects
      character(len=48) :: sProjectionName = "wtm"
      real (kind=T_DBL) :: rFalseEasting = 520000_T_DBL      ! defaults for WTM 83/91
      real (kind=T_DBL) :: rFalseNorthing = -4480000_T_DBL   ! defaults for WTM 83/91
      real (kind=T_DBL) :: rLongOrigin = -90_T_DBL     ! defaults for WTM 83/91
      real (kind=T_DBL) :: rLatOrigin = 0_T_DBL        ! defaults for WTM 83/91
      integer (kind=T_INT) :: iZoneNumber = 0
      character(len=48) :: sEllipsoidName = "GRS 1980"
      integer (kind=T_INT) :: iEllipsoidID = 11     ! defaults for WTM 83/91
#endif

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
	  real (kind=T_SGL), dimension(STREAM_INTERACTIONS_MAX) :: rStreamMaxInflow
 	  real (kind=T_SGL), dimension(STREAM_INTERACTIONS_MAX) :: rStreamMaxCapture
#endif

  end type T_MODEL_CONFIGURATION

  !> data structure to hold a line of input from the classic time-series file
  type T_TIME_SERIES_FILE
     integer (kind=T_INT) :: iMonth
     integer (kind=T_INT) :: iDay
     integer (kind=T_INT) :: iYear
     real (kind=T_SGL) :: rAvgT
     real (kind=T_SGL) :: rPrecip
     real (kind=T_SGL) :: rRH
     real (kind=T_SGL) :: rMaxT
     real (kind=T_SGL) :: rMinT
     real (kind=T_SGL) :: rWindSpd
     real (kind=T_SGL) :: rMinRH
     real (kind=T_SGL) :: rSunPct
     logical (kind=T_LOGICAL) :: lEOF = lFALSE
  end type T_TIME_SERIES_FILE

  !> Type that holds parameters used to create graphics with the DISLIN
  !> library.
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

    integer(kind=T_INT) :: iWinSizeX = 768
    integer(kind=T_INT) :: iWinSizeY = 1024

    integer(kind=T_INT) :: iTimeFrame = 1

    real(kind=T_SGL),dimension(3) :: rZA = 0.
    real(kind=T_SGL),dimension(3) :: rZE = 10.
    real(kind=T_SGL),dimension(3) :: rZOR = 0.
    real(kind=T_SGL),dimension(3) :: rZSTEP = 1.

    logical (kind=T_LOGICAL) :: lEnableDislinMessages = lTRUE

  end type T_GRAPH_CONFIGURATION

  integer(kind=T_INT),parameter :: iDAILY = 1
  integer(kind=T_INT),parameter :: iMONTHLY = 2
  integer(kind=T_INT),parameter :: iANNUAL = 3

  !> @anchor const_runoffCalc
  !> @name Constants: Runoff calculation
  !> Options controlling the selection of a runoff calculation algorithm
  !> @{
  integer (kind=T_INT),parameter :: CONFIG_RUNOFF_CURVE_NUMBER = 0
  integer (kind=T_INT),parameter :: CONFIG_RUNOFF_GREEN_AMPT = 1
  !> @}

  !> @anchor const_runoffSoln
  !> @name Constants: Runoff routing calculation
  !> Options for routing mechanism selection
  !> @{
  integer (kind=T_INT),parameter :: CONFIG_RUNOFF_ITERATIVE = 0
  integer (kind=T_INT),parameter :: CONFIG_RUNOFF_DOWNHILL = 1
  integer (kind=T_INT),parameter :: CONFIG_RUNOFF_NO_ROUTING = 2
  !> @}


  !> @name Constants: Evapotranspiration algorithm
  !> Options for specifying the choice of evapotranspiration algorithm
  !> @{
  integer (kind=T_INT),parameter :: CONFIG_ET_NONE = 0
  integer (kind=T_INT),parameter :: CONFIG_ET_THORNTHWAITE_MATHER = 1
  integer (kind=T_INT),parameter :: CONFIG_ET_TURC = 2
  integer (kind=T_INT),parameter :: CONFIG_ET_JENSEN_HAISE = 3
  integer (kind=T_INT),parameter :: CONFIG_ET_BLANEY_CRIDDLE = 4
  integer (kind=T_INT),parameter :: CONFIG_ET_HARGREAVES = 5
  !> @}

  !> @name Constants: Precipitation data format
  !> Options for specifying the method of input for precipitation data
  !> @{
  integer (kind=T_INT),parameter :: CONFIG_PRECIP_SINGLE_STATION = 0
  integer (kind=T_INT),parameter :: CONFIG_PRECIP_ARC_GRID = 1
  integer (kind=T_INT),parameter :: CONFIG_PRECIP_SURFER_GRID = 2
  integer (kind=T_INT),parameter :: CONFIG_PRECIP_NETCDF = 3
  !> @}

  !> @name Constants: Temperature data format
  !> Options for specifying the method of input for temperature data
  !> @{
  integer (kind=T_INT),parameter :: CONFIG_TEMPERATURE_SINGLE_STATION = 0
  integer (kind=T_INT),parameter :: CONFIG_TEMPERATURE_ARC_GRID = 1
  integer (kind=T_INT),parameter :: CONFIG_TEMPERATURE_SURFER_GRID = 2
  integer (kind=T_INT),parameter :: CONFIG_TEMPERATURE_NETCDF = 3
  !> @}

  !> @name Constants: Landuse input data format
  !> Options for specifying the method of input for temperature data
  !> @{
  integer (kind=T_INT),parameter :: CONFIG_LANDUSE_CONSTANT = 0
  integer (kind=T_INT),parameter :: CONFIG_LANDUSE_DYNAMIC_ARC_GRID = 1
  integer (kind=T_INT),parameter :: CONFIG_LANDUSE_DYNAMIC_SURFER = 2
  integer (kind=T_INT),parameter :: CONFIG_LANDUSE_DYNAMIC_NETCDF = 3
  integer (kind=T_INT),parameter :: CONFIG_LANDUSE_STATIC_GRID = 4
  !> @}

  !> @name Constants: Landuse input data format
  !> Configuration for selection of snowfall and snowmelt modules
  !> @{
  integer (kind=T_INT),parameter :: CONFIG_SNOW_ORIGINAL_SWB = 0
  integer (kind=T_INT),parameter :: CONFIG_SNOW_NEW_SWB = 1
  !> @}

  !> @name Constants: Landuse input data format
  !> Configuration information for soil-moisture capacity calculations
  !> @{
  integer (kind=T_INT),parameter :: CONFIG_SM_CAPACITY_CALCULATE = 0
  integer (kind=T_INT),parameter :: CONFIG_SM_CAPACITY_CONSTANT = 1
  integer (kind=T_INT),parameter :: CONFIG_SM_CAPACITY_FM_TABLE = 2
  !> @}

  !> @name Constants: Landuse input data format
  !> Configuration information for soil-moisture retention calculations
  !> @{
  integer (kind=T_INT),parameter :: CONFIG_SM_NONE = 0
  integer (kind=T_INT),parameter :: CONFIG_SM_THORNTHWAITE_MATHER = 1
  !> @}

  !> @name Constants: Landuse input data format
  !> Configuration information for irrigation calculations
  !> @{
  integer (kind=T_INT), parameter :: CONFIG_IRRIGATION_FAO56_DUAL = 0
  !> @}

  !> @name Constants: Landuse input data format
  !> Configuration information for initial abstraction assumptions
  !> @{
  integer (kind=T_INT), parameter :: CONFIG_SM_INIT_ABSTRACTION_TR55 = 0
  integer (kind=T_INT), parameter :: CONFIG_SM_INIT_ABSTRACTION_HAWKINS = 1
  !> @}

  ! Define behavior in the case of missing data [UNIMPLEMENTED]
  integer (kind=T_INT), parameter :: CONFIG_ESTIMATE_MISSING_DATA = 0
  integer (kind=T_INT), parameter :: CONFIG_END_IF_MISSING_DATA = 1

  !> @name Constants: Landuse input data format
  !> Options for output formats
  !> @{
  integer (kind=T_INT),parameter :: OUTPUT_SURFER = 0
  integer (kind=T_INT),parameter :: OUTPUT_ARC = 1
  !> @}

!**********************************************************************
!! GENERIC interfaces
!**********************************************************************

  interface approx_equal
    module procedure approx_equal_sgl
    module procedure approx_equal_dbl
  end interface approx_equal

  interface assert
    module procedure assert_simple_sub
    module procedure assert_module_details_sub
  end interface assert

  interface chomp
    module procedure chomp_delim_sub
    module procedure chomp_default_sub
  end interface

contains

function nextunit(iLU)  result(iUnit)

  integer (kind=T_INT), intent(out), optional :: iLU
  integer (kind=T_INT) :: iUnit

  ! [ LOCALS ]
  logical (kind=T_LOGICAL) :: lOpened
  integer (kind=T_INT) :: iStartLU = 201
  integer (kind=T_INT) :: iStopLU = 10000

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

function dquote(sString)                                   result(sQuotedString)

  character (len=*), intent(in) :: sString
  character (len=len_trim(adjustl(sString))+2) :: sQuotedString

  sQuotedString = '"'//trim(adjustl(sString))//'"'

end function dquote

!------------------------------------------------------------------------------

function str_compare(sString1, sString2)                   result(lBool)

  character(len=*), intent(in) :: sString1
  character(len=*), intent(in) :: sString2
  logical (kind=T_LOGICAL) :: lBool

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
  logical (kind=T_LOGICAL), intent(in) :: lCondition
  character (len=*), intent(in) :: sErrorMessage
  character (len=len(sErrorMessage)) :: sRecord
  character (len=256) :: sItem

  ! [ LOCALS ]
  integer (kind=T_INT) :: i, iEnd
  integer (kind=T_INT), dimension(2) :: iLU
  logical (kind=T_LOGICAL) :: lFileOpen

  iLU = [ LU_STD_OUT, LU_LOG ]

  if ( .not. lCondition ) then

  ! echo error condition to the log file ONLY if it is open!
  inquire (unit=LU_LOG, opened=lFileOpen)
  if(lFileOpen) then
      iEnd = 2
    else
      iEnd = 1
    endif

    do i=1,2
      sRecord = sErrorMessage
      write(UNIT=iLU(i),FMT="(/,a,/)") '** FATAL ERROR - HALTING SWB **'
      do
        call chomp(sRecord, sItem, "~")
        if(len_trim(sItem) == 0) exit
        write(UNIT=iLU(i),FMT="(a)") trim(sItem)
      enddo
    enddo

    stop

  endif

end subroutine assert_simple_sub

!--------------------------------------------------------------------------

subroutine assert_module_details_sub(lCondition,sErrorMessage,sFilename,iLineNum)

  ! ARGUMENTS
  logical (kind=T_LOGICAL), intent(in) :: lCondition
  character (len=*), intent(in) :: sErrorMessage
  character (len=*) :: sFilename
  integer (kind=T_INT) :: iLineNum
  character (len=len(sErrorMessage)) :: sRecord
  character (len=256) :: sItem

  ! [ LOCALS ]
  integer (kind=T_INT) :: i, iEnd
  integer (kind=T_INT), dimension(2) :: iLU
  logical (kind=T_LOGICAL) :: lFileOpen
  character (len=12) :: sLineNum

  iLU = [ LU_STD_OUT, LU_LOG ]

  if ( .not. lCondition ) then

    write(sLineNum,fmt="(i12)") iLineNum

    ! echo error condition to the log file ONLY if it is open!
    inquire (unit=LU_LOG, opened=lFileOpen)
    if(lFileOpen) then
      iEnd = 2
    else
      iEnd = 1
    endif

    do i=1,2
      sRecord = sErrorMessage
      write(UNIT=iLU(i),FMT="(/,a,/)") '** FATAL ERROR - HALTING SWB **'
      do
        call chomp(sRecord, sItem, "~")
        if(len_trim(sItem) == 0) exit
        write(UNIT=iLU(i),FMT="(a)") trim(sItem)
      enddo

      write(UNIT=iLU(i),FMT="(/,'   fortran module:',t27, a)") trim(sFilename)
      write(UNIT=iLU(i),FMT="('   module line number:',t27,a)") trim(adjustl(sLineNum))
    enddo

    stop

  endif

end subroutine assert_module_details_sub

!-------------------------------------------------------------------------------

!> @brief echo to screen AND write to logfile
  subroutine echolog(sMessage, sFormat)

    character(len=*), intent(in)             :: sMessage
    character(len=*), intent(in), optional   :: sFormat

    ! [ LOCALS ]
    character (len=256) :: sFormatString = ""
    logical (kind=T_LOGICAL) :: lOpened

    if(present(sFormat)) then
      sFormatString = '('//trim(sFormat)//')'
    else
      sFormatString = '(3x,a)'
    endif

    write(unit=LU_STD_OUT, fmt=trim(sFormatString)) sMessage

    inquire(unit=LU_LOG, opened = lOpened)
    if(lOpened)  write(unit=LU_LOG, fmt=trim(sFormatString)) sMessage

  end subroutine echolog

!-------------------------------------------------------------------------------

subroutine Chomp_delim_sub(sRecord, sItem, sDelimiters)

  ! ARGUMENTS
  character (len=*), intent(inout)                 :: sRecord
  character (len=256), intent(out)   :: sItem
  character (len=*), intent(in)                    :: sDelimiters
  ! LOCALS
  integer (kind=T_INT) :: iR                      ! Index in sRecord
  integer (kind=T_INT) :: iB                      !
  integer (kind=T_INT) :: iLen

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
  integer (kind=T_INT) :: iR                      ! Index in sRecord
  integer (kind=T_INT) :: iB                      !
  integer (kind=T_INT) :: iLen

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
!   integer (kind=T_INT) :: iR                      ! Index in sRecord
!   integer (kind=T_INT) :: iS                      ! Index in sItem
!   logical (kind=T_LOGICAL) :: lSkip               ! TRUE while skipping spaces
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
   integer (kind=T_INT) :: iR                      ! Index in sRecord
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
  integer (kind=T_INT) :: iR                      ! Index in sRecord
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
  integer (kind=T_INT) :: iJD

  ! [ LOCALS ]
  integer (kind=T_INT) :: iMonth
  integer (kind=T_INT) :: iDay
  integer (kind=T_INT) :: iYear
  character (len=256) :: sItem, sBuf
  integer (kind=T_INT) :: iStat

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

function mmddyyyy2doy(sMMDDYYYY)  result(iDOY)

  character (len=*) :: sMMDDYYYY
  integer (kind=T_INT) :: iDOY

  ! [ LOCALS ]
  integer (kind=T_INT) :: iMonth
  integer (kind=T_INT) :: iDay
  integer (kind=T_INT) :: iYear
  character (len=256) :: sItem, sBuf
  integer (kind=T_INT) :: iStat
  integer (kind=T_INT) :: iJD
  integer (kind=T_INT) :: iStartingJD

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
  integer (kind=T_INT) :: i    ! do loop index
  ! CONSTANTS
  integer (kind=T_INT), parameter :: LOWER_TO_UPPER = -32

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
  integer (kind=T_INT) :: i    ! do loop index
  ! CONSTANTS
  integer (kind=T_INT), parameter :: UPPER_TO_LOWER = 32

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
  integer (kind=T_INT) :: i    ! do loop index
  ! CONSTANTS
  integer (kind=T_INT) :: LOWER_TO_UPPER = ichar( "A" ) - ichar( "a" )

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
  integer (kind=T_INT) :: i,j
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
  integer (kind=T_INT) :: i,j
  ! LOCAL PARAMETERS
  character (len=1),dimension(5),parameter :: &
                REPLACE_CHARS = [ ',', sTAB, '"', &
                  sBACKSLASH, sFORWARDSLASH ]

  do i=1,size(REPLACE_CHARS)
    do j=1,len(s)
      if ( s(j:j) == REPLACE_CHARS(i) ) s(j:j) = ' '
    end do
  end do

  return
end subroutine CleanUpSlash


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
  real (kind=T_SGL),intent(in) :: x
  real (kind=T_SGL),dimension(:) :: a
  ! [ RETURN VALUE ]
  real (kind=T_SGL) :: rV
  ! [ LOCALS ]
  integer (kind=T_INT) :: i

  rV =a(1)
  do i=2,size(a)
    rV = rV*x + a(i)
  end do

  return
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
  integer (kind=T_INT),intent(in) :: iMonth
  integer (kind=T_INT),intent(in) :: iDay
  integer (kind=T_INT),intent(in) :: iYear
  integer (kind=T_INT),intent(out) :: iDayOfYear
  character (len=3),intent(out) :: sMonthName
  logical (kind=T_LOGICAL),intent(out) :: lMonthEnd
  ! [ LOCALS ]
  integer (kind=T_INT) :: i
  type ( T_MONTH ),pointer :: mo
  integer (kind=T_INT) :: iEpochMonth = 1
  integer (kind=T_INT) :: iEpochDay = 1
  integer (kind=T_INT) :: iEpochJulianDay
  integer (kind=T_INT) :: iDummy1, iDummy2
  integer (kind=T_INT) :: iJulianDay
  integer (kind=T_INT) :: iTomorrowsMonth

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

   real(kind=T_DBL) :: rA
   real(kind=T_DBL) :: rB
   real(kind=T_DBL), optional :: rTol
   logical(kind=T_LOGICAL) :: lTest

   ! [ LOCALS ]
   real (kind=T_DBL) :: rDiff
   integer (kind=T_INT) :: iDiff
   real(kind=T_DBL) :: rMultiplier

   if(present(rTol) ) then
     rMultiplier = rTol
   else
     rTol = 10000.
   endif

   rDiff = ABS(rA - rB)
   iDiff = int(rDiff * rMultiplier, kind=T_INT)

   if(iDiff == 0) then
     lTest = lTRUE
   else
     lTest = lFALSE
   endif

   return

end function approx_equal_dbl


function approx_equal_sgl(rA, rB, rTol)  result(lTest)

   real(kind=T_SGL) :: rA
   real(kind=T_SGL) :: rB
   real(kind=T_SGL), optional :: rTol
   logical(kind=T_LOGICAL) :: lTest

   ! [ LOCALS ]
   real (kind=T_DBL) :: rDiff
   integer (kind=T_INT) :: iDiff
   real(kind=T_SGL) :: rMultiplier

   if(present(rTol) ) then
     rMultiplier = rTol
   else
     rTol = 10000.
   endif

   rDiff = ABS(rA - rB)
   iDiff = int(rDiff * rMultiplier, kind=T_INT)

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
  integer (kind=T_INT),intent(in) :: iDayOfYear
  ! [ RETURN VALUE ]
  logical (kind=T_LOGICAL) :: lGrowing

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

 function FtoC(rF) result(rC)
  ! Converts degrees Fahrenheit to degrees Celsius
  ! [ ARGUMENTS ]
  real (kind=T_SGL),intent(in) :: rF
  ! [ RETURN VALUE ]
  real (kind=T_SGL) :: rC

  rC = (rF - rFREEZING) * dpC_PER_F

  return
end function FtoC

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

 function CtoF(rC) result(rF)
  ! Converts degrees Celsius to degrees Fahrenheit
  ! [ ARGUMENTS ]
  real (kind=T_SGL),intent(in) :: rC
  ! [ RETURN VALUE ]
  real (kind=T_SGL) :: rF

  rF = rC * dpF_PER_C + rFREEZING

  return
end function CtoF

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

 function FtoK(rF) result(rK)
  ! Converts degrees Fahrenheit to kelvins
  ! [ ARGUMENTS ]
  real (kind=T_SGL),intent(in) :: rF
  ! [ RETURN VALUE ]
  real (kind=T_SGL) :: rK

  rK = (rF - rFREEZING) * dpC_PER_F + 273.15_T_SGL

  return
end function FtoK

!***
!--------------------------------------------------------------------------
function count_fields(sRecord) result(iNumFields)

  character (len=*), intent(inout) :: sRecord

  character (len=256) :: sItem
  integer(kind=T_INT) :: iNumFields, i

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
  real (kind=T_SGL),intent(in) :: r_mm
  ! [ RETURN VALUE ]
  real (kind=T_SGL) :: r_in

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
  real (kind=T_SGL), dimension(:), intent(in) :: rXT, rYT
  real (kind=T_SGL), intent(in) :: rX
  ! [ RETURN VALUE ]
  real (kind=T_SGL) :: rY
  ! [ LOCALS ]
  integer (kind=T_INT) :: i,n

  call Assert(LOGICAL(size(rXT) == size(rYT),kind=T_LOGICAL), &
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
  integer (kind=T_INT), intent(in) :: iYear, iMonth, iDay
  integer (kind=T_INT), optional :: iOrigin

  ! [ LOCALS ]
  integer (kind=T_INT) i,j,k
  integer (kind=T_INT) :: iOffset

  ! [ RETURN VALUE ]
  integer (kind=T_INT) :: iJD

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

  iJD= ( k-32075_T_INT + 1461_T_INT * (i + 4800_T_INT + (j - 14_T_INT) / 12_T_INT) &
        /4_T_INT + 367_T_INT * (j - 2_T_INT - (j - 14_T_INT)/ 12_T_INT * 12_T_INT) &
        /12_T_INT - 3_T_INT *((i + 4900_T_INT + (j - 14_T_INT) &
        /12_T_INT)/100_T_INT)/4_T_INT ) - iOffset

  return

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
  integer (kind=T_INT), intent(in) :: iJD

  ! [ LOCALS ]
  integer (kind=T_INT) iMonth, iDay, iYear


  ! [ RETURN VALUE ]
  integer (kind=T_INT) :: iSol

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

!> Convert an integer value into a formatted character string
function int2char(iValue)  result(sBuf)

  integer (kind=T_INT) :: iValue
  character(len=256) :: sBuf

  write(UNIT=sBuf,FMT="(i14)") iValue
  sBuf = ADJUSTL(sBuf)

  return

end function int2char

!--------------------------------------------------------------------------

!> Convert a real value into a formatted character string
function real2char(rValue, iDec, iWidth)  result(sBuf)

  real (kind=T_SGL) :: rValue
  integer (kind=T_INT), optional :: iDec
  integer (kind=T_INT), optional :: iWidth

  ![ LOCALS ]
  character(len=256) :: sBuf, sFmt
  integer (kind=T_INT) :: iD, iW



  if(present(iDec)) then
    iD = iDec
  else
    iD = 4
  endif

  if(present(iWidth)) then
    iW = iWidth
  else
   iW = 16
  endif

  if(abs(rValue) < rNEAR_ZERO) then
    sBuf = "0."
  else
    sFmt = "(G"//TRIM(int2char(iW))//"."//TRIM(int2char(iD))//")"
    write(UNIT=sBuf,FMT=TRIM(sFmt)) rValue
    sBuf = ADJUSTL(sBuf)
  endif

  return

end function real2char

!> @brief Return the number of days in the given year.
!>
!> This function simply returns the number of days given the current year.
function num_days_in_year(iYear) result(iNumDaysInYear)

  integer (kind=T_INT), intent(in) :: iYear

  ! [ LOCALS ]
  integer (kind=T_INT) :: iFirstDay, iLastDay, iNumDaysInYear

  iFirstDay = julian_day ( iYear, 1, 1 )
  iLastDay = julian_day ( iYear, 12, 31 )
  iNumDaysInYear = iLastDay - iFirstDay + 1

  return

end function num_days_in_year

!--------------------------------------------------------------------------

function day_of_year(iJD) result(iDOY)

  integer (kind=T_INT), intent(in) :: iJD

  ! [ LOCALS ]
  integer (kind=T_INT) :: iFirstDay, iLastDay, iDOY
  integer (kind=T_INT) :: iMonth, iDay, iYear


  call gregorian_date(iJD, iYear, iMonth, iDay)
  iFirstDay = julian_day ( iYear, 1, 1 )

  iDOY = iJD - iFirstDay + 1

  return

end function day_of_year

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


!> @brief Convert from a Julian day number to a Gregorian date.
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
  integer (kind=T_INT) :: iJD
  integer (kind=T_INT), intent(inout) :: iYear, iMonth, iDay
  integer (kind=T_INT), optional :: iOrigin
  ! [ LOCALS ]
  integer (kind=T_INT) iI,iJ,iK,iL,iN
  integer (kind=T_INT) :: iOffset

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

  iL= iJD + 68569_T_INT
  iN= 4*iL / 146097_T_INT
  iL= iL - (146097_T_INT * iN + 3_T_INT)/4_T_INT
  iI= 4000_T_INT * (iL + 1_T_INT) / 1461001_T_INT
  iL= iL - 1461_T_INT * iI / 4_T_INT + 31_T_INT
  iJ= 80_T_INT * iL / 2447_T_INT
  iK= iL - 2447_T_INT * iJ / 80_T_INT
  iL= iJ / 11_T_INT
  iJ= iJ + 2_T_INT - 12_T_INT * iL
  iI= 100_T_INT * (iN - 49_T_INT) + iI + iL

  iYear = iI
  iMonth = iJ
  iDay = iK

  return

end subroutine gregorian_date

end module types
