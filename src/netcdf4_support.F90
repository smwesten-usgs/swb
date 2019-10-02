!> @file
!!  Contains a single module, @ref netcdf4_support, which
!!  provides support for use of NetCDF files as input or output.
!!
!! Supports use of NetCDF files as input for time-varying,
!! gridded meteorlogic data, or output for any SWB-generated variable.
!!
!! from the C API:
!! The @c nc_get_vars_ type family of functions read a subsampled (strided)
!! array section of values from a netCDF variable of an open netCDF dataset.
!! The subsampled array section is specified by giving a corner,
!! a vector of edge lengths, and a stride vector. The values are read
!! with the last dimension of the netCDF variable varying fastest.
!!          ^^^^                                  ^^^^^^^ ^^^^^^^
!!
!! from the Fortran 90 API:
!! The values to be read are associated with the netCDF variable by
!! assuming that the first dimension of the netCDF variable
!!                   ^^^^^
!! varies fastest in the Fortran 90 interface.
!! ^^^^^^ ^^^^^^^

!> Provide support for use of NetCDF files as input for time-varying,
!! gridded meteorlogic data, or output for any SWB-generated variable.
module netcdf4_support

  use iso_c_binding
  use types

  use swb_grid
!  use typesizes
  use netcdf_c_api_interfaces

  implicit none

  private

  integer(c_int), public :: NC_READONLY          = 0
  integer(c_int), public :: NC_READWRITE         = 1

  integer(c_int), parameter ::  NC_NAT    = 0
  integer(c_int), parameter ::  NC_BYTE   = 1
  integer(c_int), parameter ::  NC_CHAR   = 2
  integer(c_int), parameter ::  NC_SHORT  = 3
  integer(c_int), parameter ::  NC_INT    = 4
  integer(c_int), parameter ::  NC_LONG   = NC_INT
  integer(c_int), parameter ::  NC_FLOAT  = 5
  integer(c_int), parameter ::  NC_DOUBLE = 6
  integer(c_int), parameter ::  NC_UBYTE  = 7
  integer(c_int), parameter ::  NC_USHORT = 8
  integer(c_int), parameter ::  NC_UINT   = 9
  integer(c_int), parameter ::  NC_INT64  = 10
  integer(c_int), parameter ::  NC_UINT64 = 11
  integer(c_int), parameter ::  NC_STRING = 12

  integer(c_int), parameter :: NC_FILL_CHAR    = 0
  integer(c_int), parameter :: NC_FILL_BYTE    = -127
  integer(c_int), parameter :: NC_FILL_SHORT   = -32767
  integer(c_int), parameter :: NC_FILL_INT     = -2147483647
  real(c_float),  parameter :: NC_FILL_FLOAT   = 9.9692099683868690e+36
  real(c_double), parameter :: NC_FILL_DOUBLE  = 9.9692099683868690d+36

  ! mode flags for opening and creating datasets
  integer(c_int), parameter :: NC_NOWRITE          = 0
  integer(c_int), parameter :: NC_WRITE            = 1
  integer(c_int), parameter :: NC_CLOBBER          = 0
  integer(c_int), parameter :: NC_NOCLOBBER        = 4
  integer(c_int), parameter :: NC_FILL             = 0
  integer(c_int), parameter :: NC_NOFILL           = 256
  integer(c_int), parameter :: NC_LOCK             = 1024
  integer(c_int), parameter :: NC_SHARE            = 2048
  integer(c_int), parameter :: NC_STRICT_NC3       = 8
  integer(c_int), parameter :: NC_64BIT_OFFSET     = 512
  integer(c_int), parameter :: NC_SIZEHINT_DEFAULT = 0
  integer(c_int), parameter :: NC_ALIGN_CHUNK      = -1
  integer(c_int), parameter :: NC_FORMAT_CLASSIC   = 1
  integer(c_int), parameter :: NC_FORMAT_64BIT     = 2
  integer(c_int), parameter :: NC_FORMAT_NETCDF4   = 3
  integer(c_int), parameter :: NC_FORMAT_NETCDF4_CLASSIC = 4

  ! implementation limits (warning!  should be the same as c interface)
  integer(c_int), parameter :: NC_MAX_DIMS     = 1024
  integer(c_int), parameter :: NC_MAX_ATTRS    = 8192
  integer(c_int), parameter :: NC_MAX_VARS     = 8192
  integer(c_int), parameter :: NC_MAX_NAME     = 256

  integer (c_int), parameter :: NC_SHUFFLE_YES = 1
  integer (c_int), parameter :: NC_SHUFFLE_NO = 0
  integer (c_int), parameter :: NC_DEFLATE_YES = 1
  integer (c_int), parameter :: NC_DEFLATE_NO = 0

  integer(c_int), parameter :: NC_NETCDF4        = 4096
  integer(c_int), parameter :: NC_CLASSIC_MODEL  = 256


  integer(c_int),  parameter :: NC_UNLIMITED = 0
  integer(c_int),  parameter :: NC_GLOBAL    = -1

  integer (c_int), public, parameter :: NC_TIME    = 0
  integer (c_int), public, parameter :: NC_Y       = 1
  integer (c_int), public, parameter :: NC_X       = 2
  integer (c_int), public, parameter :: NC_Z       = 3

  integer (c_int), parameter :: NC_FIRST = 0
  integer (c_int), parameter :: NC_LAST  = 1
  integer (c_int), parameter :: NC_BY    = 2

  integer (c_int), public, parameter :: NC_LEFT  = 0
  integer (c_int), public, parameter :: NC_RIGHT = 1
  integer (c_int), public, parameter :: NC_TOP    = 0
  integer (c_int), public, parameter :: NC_BOTTOM = 1

  integer (c_int), parameter   :: NETCDF_IO_ERROR           = -68
  integer (c_int), parameter   :: NETCDF_HDF_LAYER_ERROR    = -101
  integer (c_int), parameter   :: NETCDF_CANNOT_READ_ERROR  = -102

  ! how many times should SWB attempt to read data via a THREDDs URL
  ! before declaring an error and halting?
  integer (c_int), parameter   :: NETCDF_IO_ERROR_RETRIES = 5

  character (len=25), dimension(4), parameter :: NETCDF_FORMAT_STRING = &
    ["NC_FORMAT_CLASSIC        ", &
     "NC_FORMAT_64BIT          ", &
     "NC_FORMAT_NETCDF4        ", &
     "NC_FORMAT_NETCDF4_CLASSIC" ]

  character (len=6), dimension(0:6), parameter :: NETCDF_DATA_TYPE = &
    ["nat   ", &
     "byte  ", &
     "char  ", &
     "short ", &
     "int   ", &
     "float ", &
     "double" ]

  type T_NETCDF_DIMENSION
    character (len=64) :: sDimensionName
    integer (c_int) :: iNC_DimID = -9999
    integer (c_size_t) :: iNC_DimSize
    logical (c_bool) :: lUnlimited = lFALSE
  end type T_NETCDF_DIMENSION

  type T_NETCDF_ATTRIBUTE
    character (len=64) :: sAttributeName
    character (len=64), dimension(:), allocatable :: sAttValue
    integer (c_short), dimension(:), allocatable :: i2AttValue
    integer (c_int), dimension(:), allocatable :: iAttValue
    real (c_float), dimension(:), allocatable :: rAttValue
    real (c_double), dimension(:), allocatable :: dpAttValue
    integer (c_int) :: iNC_AttType
    integer (c_size_t) :: iNC_AttSize
  end type T_NETCDF_ATTRIBUTE

  type T_NETCDF_VARIABLE
    character (len=64) :: sVariableName
    integer (c_int) :: iNC_VarID = -9999
    integer (c_int) :: iNC_VarType
    integer (c_int) :: iNumberOfDimensions
    integer (c_int), dimension(0:3) :: iNC_DimID = -9999
    real (c_double) :: rScaleFactor = 1_c_double
    real (c_double) :: rAddOffset = 0_c_double
    integer (c_int) :: iNumberOfAttributes
    type (T_NETCDF_ATTRIBUTE), dimension(:), pointer :: pNC_ATT => null()
  end type T_NETCDF_VARIABLE

  type T_NETCDF4_FILE
    integer (c_int) :: iNCID
    character (len=256)  :: sFilename
    integer (c_int) :: iFileFormat
    integer (c_int) :: iNumberOfDimensions
    integer (c_int) :: iNumberOfVariables
    integer (c_int) :: iNumberOfAttributes
    integer (c_int) :: iNC3_UnlimitedDimensionNumber
    integer (c_int) :: iOriginJD
    integer (c_int) :: iFirstDayJD
    integer (c_int) :: iLastDayJD
    integer (c_int) :: iOriginMonth
    integer (c_int) :: iOriginDay
    integer (c_int) :: iOriginYear
    integer (c_int) :: iOriginHH
    integer (c_int) :: iOriginMM
    integer (c_int) :: iOriginSS
    integer (c_int) :: lLeapYearTreatment
    integer (c_size_t), dimension(0:3) :: iStart
    integer (c_size_t), dimension(0:3) :: iCount
    integer (c_size_t), dimension(0:3) :: iStride = 1
    integer (c_size_t), dimension(0:1) :: iColBounds
    integer (c_size_t), dimension(0:1) :: iRowBounds
    integer (c_int) :: iNX
    integer (c_int) :: iNY
    character (len=3) :: sVariableOrder = "tyx"
    real (c_double), dimension(0:1) :: rX
    real (c_double), dimension(0:1) :: rY
    logical (c_bool) :: lX_IncreasesWithIndex = lTRUE
    logical (c_bool) :: lY_IncreasesWithIndex = lFALSE

    real (c_double), dimension(0:1) :: dpFirstAndLastTimeValues
    character (len=64), dimension(0:3) :: sVarName = ["time","y   ","x   ","z   "]
    integer (c_int), dimension(0:3) :: iVarID = -9999
    integer (c_int), dimension(0:3) :: iVarIndex = -9999
    integer (c_int), dimension(0:3) :: iVarType = -9999
    character (len=64), dimension(0:3) :: sVarUnits = "NA"
    integer (c_int), dimension(0:3, 0:3) :: iVar_DimID = -9999
    real (c_double), dimension(0:3) :: rScaleFactor = 1.0_c_double
    real (c_double), dimension(0:3) :: rAddOffset = 0.0_c_double

    integer (c_int), dimension(0:2) :: iRowIter
    integer (c_int), dimension(0:2) :: iColIter
    logical (c_bool) :: lFlipHorizontal = lFALSE
    logical (c_bool) :: lFlipVertical = lFALSE

    real (c_double), allocatable, dimension(:) :: rX_Coords
    real (c_double), allocatable, dimension(:) :: rY_Coords
    real (c_double)                            :: rX_Coord_AddOffset = 0.0_c_double
    real (c_double)                            :: rY_Coord_AddOffset = 0.0_c_double
    real (c_double), allocatable, dimension(:) :: rDateTimeValues
    real (c_double) :: rGridCellSizeX
    real (c_double) :: rGridCellSizeY

    type (T_NETCDF_DIMENSION), dimension(:), pointer :: pNC_DIM => null()
    type (T_NETCDF_VARIABLE), dimension(:), pointer :: pNC_VAR => null()
    type (T_NETCDF_ATTRIBUTE), dimension(:), pointer :: pNC_ATT => null()
  end type T_NETCDF4_FILE

!
!                  /\
!  coordinates     |   |
!  increase        |   |
!  in upward       |   |
!  direction       |   |
!                  |   |
!                  |   | column index increases in downward direction
!                  |   |
!                  |   |
!                      \/
!
!

  public :: T_NETCDF_DIMENSION, T_NETCDF_VARIABLE, T_NETCDF_ATTRIBUTE
  public :: T_NETCDF4_FILE

  public :: netcdf_open_and_prepare_as_input
  public :: netcdf_open_and_prepare_as_output
  public :: netcdf_date_within_range
  public :: netcdf_deallocate_data_struct
  public :: netcdf_nullify_data_struct
  public :: netcdf_dump_cdl
  public :: netcdf_open_file
  public :: netcdf_close_file
  public :: netcdf_get_variable_slice
  public :: netcdf_update_time_starting_index
  public :: netcdf_put_variable_array
  public :: netcdf_put_variable_vector

contains

!----------------------------------------------------------------------

function netcdf_date_within_range( NCFILE, iJulianDay)  result( lWithinRange )

  type (T_NETCDF4_FILE ) :: NCFILE
  integer (c_int) :: iJulianDay
  logical (c_bool) :: lWithinRange

  if ( iJulianDay >= NCFILE%iFirstDayJD &
      .and. iJulianDay <= NCFILE%iLastDayJD ) then

    lWithinRange = lTRUE

  else

    lWithinRange = lFALSE

  endif

end function netcdf_date_within_range

!----------------------------------------------------------------------

!> This was so clear the other day. Basically, I think we need
!> two functions to convert from index to timeval, and timeval to JD;
!> note that timeval refers to the number of days from the origin
!> of the NetCDF file

!> return the day value (number of days since origin

function nf_julian_day_to_index(NCFILE, rJulianDay)  result (iIndex)

  type (T_NETCDF4_FILE) :: NCFILE
  real (c_double) :: rJulianDay
  integer (c_int) :: iIndex

  iIndex = aint(rJulianDay) - NCFILE%iFirstDayJD

end function nf_julian_day_to_index


function nf_index_to_dayvalue(NCFILE, iIndex)   result(rDayValue)

  type (T_NETCDF4_FILE) :: NCFILE
  integer (c_int) :: iIndex
  real (c_double) :: rDayValue

  call assert(iIndex >= lbound(NCFILE%rDateTimeValues, 1) &
  .and. iIndex <= ubound(NCFILE%rDateTimeValues, 1),&
  "Dimension out of bounds", &
  trim(__FILE__), __LINE__)
  rDayValue = NCFILE%rDateTimeValues(iIndex)

end function nf_index_to_dayvalue

!--------------------------------------------------------------------------------------------------

function nf_dayvalue_to_julian_day(NCFILE, rDayValue)   result(rJulianDay)

  type (T_NETCDF4_FILE) :: NCFILE
  real (c_double) :: rDayValue
  real (c_double) :: rJulianDay

  rJulianDay = real(NCFILE%iOriginJD, c_double) &
    + real(NCFILE%iOriginHH, c_double) / 24_c_double &
    + real(NCFILE%iOriginMM, c_double) / 1440_c_double &
    + real(NCFILE%iOriginSS, c_double) / 86400_c_double &
    + rDayValue

end function nf_dayvalue_to_julian_day

!--------------------------------------------------------------------------------------------------

!> Given the current Julian Date of the simulation, return the corresponding
!! index value for the NetCDF file.
function nf_julian_day_to_index_adj( NCFILE, rJulianDay )  result(iStart)

  type (T_NETCDF4_FILE ) :: NCFILE
  real (c_double) :: rJulianDay
  integer (c_size_t) :: iStart

  ! [ LOCALS ]
  integer (c_int)              :: iJD_Difference
  real (c_double)              :: rJD_atIndex
  integer (c_int)              :: iIndex
  integer (c_int)              :: iMonth, iDay, iYear
  integer (c_int)              :: iIterations
  integer (c_int), parameter   :: MAX_ITERATIONS = 100
  logical (c_bool)             :: lFound

  ! return value of this function represents the *starting* index of the time dimension
  ! if no value is found in association with the current Julian Date, return -9999
  iStart = -9999
  iIterations = 0
  lFound = lFALSE

  !> First guess at what the appropriate index value should be.
  !> Current JD minus the Origin JD is a good guess.
  iIndex = max(0, nf_julian_day_to_index(NCFILE, rJulianDay) )

  do

    ! need to get out of here if the first guess is out-of-bounds for our DateTimeValues array
    if ( iIndex > ubound(NCFILE%rDateTimeValues,1) ) exit

    ! determine Julian Date associated with the current date/time value at this index position
    rJD_atIndex = nf_dayvalue_to_julian_day( NCFILE=NCFILE, rDayValue=NCFILE%rDateTimeValues(iIndex) )

    ! index value is relative to zero in C API;
    ! first day's data should be associated with index value of zero
    iJD_Difference = int( rJD_atIndex - rJulianDay )

    select case ( iJD_Difference )

      ! difference between estimated Julian Date and current is zero; we've found the desired index
      case ( 0 )

        iStart = iIndex
        lFound = lTRUE
        exit

      ! difference between estimated JD and current JD is < 0; JD at index is *less* than
      ! the current JD; increase index value and try again
      case ( :-1 )

        iIndex = min( ubound(NCFILE%rDateTimeValues, 1) , iIndex + 1 )

!        iIndex = max( 0, iIndex - 1 )
        iIterations = iIterations + 1

      ! difference between estimated JD and current JD is > 0; JD at index is *greater* than
      ! the current JD; reduce index value and try again
      case ( 1: )

        iIndex = max( 0, iIndex - 1 )
        iIterations = iIterations + 1

    end select

    ! we've examined the Julian Dates in a reasonable subset of the date/time variable;
    ! if we haven't found it yet, we're probably not going to.
    if ( iIterations > MAX_ITERATIONS ) exit

  enddo

  ! if ( .not. lFound ) then
  !
  !   write(LU_LOG, fmt="('JD (SWB start): ', i10, '   JD (NetCDF file): ', i10)" )  &
  !     int( rJulianDay ), int( rJD_atIndex )
  !
  !   call assert( lTRUE, "Problem finding the index number of the time " &
  !   //"variable in NetCDF file "//dquote(NCFILE%sFilename), trim(__FILE__), __LINE__)
  !
  ! endif

end function nf_julian_day_to_index_adj

!----------------------------------------------------------------------

function nf_return_VarID( NCFILE, iVarIndex)   result(iVarID)

  type (T_NETCDF4_FILE ) :: NCFILE
   integer (c_int) :: iVarIndex
   integer (c_int) :: iVarID

   type (T_NETCDF_VARIABLE), pointer :: pNC_VAR

   pNC_VAR => NCFILE%pNC_VAR(iVarIndex)

   iVarID = pNC_VAR%iNC_VarID

end function nf_return_VarID

!----------------------------------------------------------------------

function nf_return_DimID( NCFILE, iDimIndex)   result(iDimID)

  type (T_NETCDF4_FILE ) :: NCFILE
   integer (c_int) :: iDimIndex
   integer (c_int) :: iDimID

   type (T_NETCDF_DIMENSION), pointer :: pNC_DIM

   pNC_DIM => NCFILE%pNC_DIM(iDimIndex)

   iDimID = pNC_DIM%iNC_DimID

end function nf_return_DimID

!----------------------------------------------------------------------

function nf_return_VarIndex( NCFILE, iVarID)   result(iVarIndex)

  type (T_NETCDF4_FILE ) :: NCFILE
   integer (c_int) :: iVarID
   integer (c_int) :: iVarIndex

   type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
   integer (c_int) :: iIndex
   logical (c_bool) :: lFound

  lFound = lFALSE

  do iIndex=0, NCFILE%iNumberOfVariables - 1

    pNC_VAR => NCFILE%pNC_VAR(iIndex)

    if (pNC_VAR%iNC_VarID == iVarID) then
      lFound = lTRUE
      exit
    endif

  enddo

  call assert(lFound, "INTERNAL PROGRAMMING ERROR - No matching variable " &
    //"ID found: was looking for Variable ID: "//trim(asCharacter(iVarID)), &
    trim(__FILE__), __LINE__)

  iVarIndex = iIndex

end function nf_return_VarIndex

!----------------------------------------------------------------------

function nf_return_AttValue( NCFILE, iVarIndex, sAttName)   result(sAttValue)

  type (T_NETCDF4_FILE ) :: NCFILE
   integer (c_int) :: iVarIndex
   character (len=*) :: sAttName
   character (len=256) :: sAttValue

   type (T_NETCDF_ATTRIBUTE), dimension(:), pointer :: pNC_ATT
   integer (c_int) :: iIndex, iIndex2
   logical (c_bool) :: lFound

  if (iVarIndex < 0) then

    pNC_ATT => NCFILE%pNC_ATT

  else

    call assert(iVarIndex >= lbound(NCFILE%pNC_VAR,1) &
      .and. iVarIndex <= ubound(NCFILE%pNC_VAR,1), &
      "Index out of bounds referencing NCFILE%pNC_VAR" &
      //"~Offending index value: "//trim(asCharacter(iVarIndex)), &
      trim(__FILE__), __LINE__)

    pNC_ATT => NCFILE%pNC_VAR(iVarIndex)%pNC_ATT

  endif

  lFound = lFALSE

  do iIndex=lbound(pNC_ATT,1), ubound(pNC_ATT,1)

    if (str_compare(sAttName, pNC_ATT(iIndex)%sAttributeName) ) then
      lFound = lTRUE
      exit
    endif

  enddo

  call assert(lFound, "INTERNAL PROGRAMMING ERROR - No matching attribute " &
    //"name found: was looking for attribute with name: "//dquote(sAttName), &
    trim(__FILE__), __LINE__)

  sAttValue = ""
  do iIndex2=0, ubound(pNC_ATT(iIndex)%sAttValue,1)
    sAttValue = sAttValue//" "//trim(pNC_ATT(iIndex)%sAttValue(iIndex))
  enddo

  sAttValue = adjustl(sAttValue)

end function nf_return_AttValue

!----------------------------------------------------------------------

function nf_return_DimIndex( NCFILE, iDimID)   result(iDimIndex)

  type (T_NETCDF4_FILE ) :: NCFILE
   integer (c_int) :: iDimID
   integer (c_int) :: iDimIndex

   type (T_NETCDF_DIMENSION), pointer :: pNC_DIM
   integer (c_int) :: iIndex
   logical (c_bool) :: lFound

  lFound = lFALSE

  do iIndex=0, NCFILE%iNumberOfDimensions - 1

    pNC_DIM => NCFILE%pNC_DIM(iIndex)

    if (pNC_DIM%iNC_DimID == iDimID) then
      lFound = lTRUE
      exit
    endif

  enddo

  call assert(lFound, "INTERNAL PROGRAMMING ERROR - No matching dimension " &
    //"ID found: was looking for Dimension ID: "//trim(asCharacter(iDimID)), &
    trim(__FILE__), __LINE__)

  iDimIndex = iIndex

end function nf_return_DimIndex

!----------------------------------------------------------------------

function nf_return_DimSize( NCFILE, iDimID)   result(iDimSize)

  type (T_NETCDF4_FILE ) :: NCFILE
   integer (c_int) :: iDimID
   integer (c_size_t) :: iDimSize

   type (T_NETCDF_DIMENSION), pointer :: pNC_DIM
   integer (c_int) :: iIndex
   logical (c_bool) :: lFound

  lFound = lFALSE

  do iIndex=0, NCFILE%iNumberOfDimensions - 1

    pNC_DIM => NCFILE%pNC_DIM(iIndex)

    if (pNC_DIM%iNC_DimID == iDimID) then
      lFound = lTRUE
      exit
    endif

  enddo

  call assert(lFound, "INTERNAL PROGRAMMING ERROR - No matching dimension " &
    //"ID found: was looking for Dimension ID: "//trim(asCharacter(iDimID)), &
    trim(__FILE__), __LINE__)

  iDimSize = pNC_DIM%iNC_DimSize

end function nf_return_DimSize

!----------------------------------------------------------------------

subroutine netcdf_open_and_prepare_as_input(NCFILE, sFilename, &
    lFlipHorizontal, lFlipVertical, &
    rX_Coord_AddOffset, rY_Coord_AddOffset, &
    sVariableOrder, sVarName_x, &
    sVarName_y, sVarName_z, sVarName_time, &
    tGridBounds, iLU)

  type (T_NETCDF4_FILE ) :: NCFILE
  character (len=*) :: sFilename
  logical (c_bool), optional   :: lFlipHorizontal
  logical (c_bool), optional   :: lFlipVertical
  character (len=*), optional       :: sVariableOrder
  real (c_double), optional    :: rX_Coord_AddOffset
  real (c_double), optional    :: rY_Coord_AddOffset
  character (len=*), optional       :: sVarName_x
  character (len=*), optional       :: sVarName_y
  character (len=*), optional       :: sVarName_z
  character (len=*), optional       :: sVarName_time
  type (T_GRID_BOUNDS), optional    :: tGridBounds
  integer (c_int), optional    :: iLU

  ! [ LOCALS ]
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  type (T_NETCDF_DIMENSION), pointer :: pNC_DIM
  logical (c_bool) :: lFileOpen
  integer (c_int), dimension(2) :: iColRow_ll, iColRow_ur, iColRow_lr, iColRow_ul
  integer (c_int) :: iColmin, iColmax, iRowmin, iRowmax
  integer (c_int) :: iIndex

  call nf_open_file(NCFILE=NCFILE, sFilename=sFilename)

  call nf_populate_dimension_struct( NCFILE )
  call nf_populate_variable_struct( NCFILE )

  if (present(lFlipHorizontal) ) NCFILE%lFlipHorizontal = lFlipHorizontal
  if (present(lFlipVertical) ) NCFILE%lFlipVertical = lFlipVertical
  if (present(rX_Coord_AddOffset))  NCFILE%rX_Coord_AddOffset = rX_Coord_AddOffset
  if (present(rY_Coord_AddOffset))  NCFILE%rY_Coord_AddOffset = rY_Coord_AddOffset

  if (present(sVariableOrder) )  NCFILE%sVariableOrder = sVariableOrder

  if( present(iLU) ) then
    inquire (unit=iLU, opened=lFileOpen)
    if ( lFileOpen )  call netcdf_dump_cdl( NCFILE, iLU)
  endif

  if (present(sVarName_x) ) then
    NCFILE%sVarName(NC_X) = sVarName_x
  else
    NCFILE%sVarName(NC_X) = "x"
  endif

  if (present(sVarName_y) ) then
    NCFILE%sVarName(NC_Y) = sVarName_y
  else
    NCFILE%sVarName(NC_Y) = "y"
  endif

  if (present(sVarName_z) ) then
    NCFILE%sVarName(NC_Z) = sVarName_z
  else
    NCFILE%sVarName(NC_Z) = "prcp"
  endif

  if (present(sVarName_time) ) then
    NCFILE%sVarName(NC_TIME) = sVarName_time
  else
    NCFILE%sVarName(NC_TIME) = "time"
  endif

  call nf_get_variable_id_and_type( NCFILE )

  call nf_get_time_units(NCFILE=NCFILE)

  NCFILE%dpFirstAndLastTimeValues = nf_get_first_and_last(NCFILE=NCFILE, &
      iVarIndex=NCFILE%iVarIndex( NC_TIME ) )

  !> retrieve the time values as included in the NetCDF file
  call nf_get_time_vals(NCFILE)

  call nf_get_xyz_units(NCFILE=NCFILE)

  !> establish scale_factor and add_offset values, if present
  call nf_get_scale_and_offset(NCFILE=NCFILE)

  call nf_calculate_time_range(NCFILE)

  !> retrieve the X and Y coordinates from the NetCDF file...
  call nf_get_x_and_y(NCFILE)

  if (present(tGridBounds) ) then

    !> define a subset of the grid as the AOI
    !> need all four corner points since it is likely that
    !> the AOI rectangle is rotated relative to the base
    !> projection
    iColRow_ll = nf_coord_to_col_row(NCFILE=NCFILE, &
                                     rX=tGridBounds%rXll, &
                                     rY=tGridBounds%rYll)

    iColRow_lr = nf_coord_to_col_row(NCFILE=NCFILE, &
                                     rX=tGridBounds%rXlr, &
                                     rY=tGridBounds%rYlr)

    iColRow_ul = nf_coord_to_col_row(NCFILE=NCFILE, &
                                     rX=tGridBounds%rXul, &
                                     rY=tGridBounds%rYul)

    iColRow_ur = nf_coord_to_col_row(NCFILE=NCFILE, &
                                     rX=tGridBounds%rXur, &
                                     rY=tGridBounds%rYur)
#ifdef DEBUG_PRINT
    write(*, fmt="(a,a,i6)") "Find correspondence between project bounds (in native projection) and row, col of dataset |", &
      trim(__FILE__), __LINE__
    write(*, fmt="(a)") "      column     row              X              Y"
    write(*, fmt="(a,i6,i6,a,f14.3,f14.3)") "LL: ", iColRow_ll(COLUMN), iColRow_ll(ROW), " <==> ", tGridBounds%rXll, tGridBounds%rYll
    write(*, fmt="(a,i6,i6,a,f14.3,f14.3)") "LR: ", iColRow_lr(COLUMN), iColRow_lr(ROW), " <==> ", tGridBounds%rXlr, tGridBounds%rYlr
    write(*, fmt="(a,i6,i6,a,f14.3,f14.3)") "UL: ", iColRow_ul(COLUMN), iColRow_ul(ROW), " <==> ", tGridBounds%rXul, tGridBounds%rYul
    write(*, fmt="(a,i6,i6,a,f14.3,f14.3)") "UR: ", iColRow_ur(COLUMN), iColRow_ur(ROW), " <==> ", tGridBounds%rXur, tGridBounds%rYur
#endif

    NCFILE%iColBounds(NC_LEFT) = &
      max( min( iColRow_ul(COLUMN), iColRow_ur(COLUMN), iColRow_ll(COLUMN), iColRow_lr(COLUMN) ) - 4, &
                lbound(NCFILE%rX_Coords,1) )

    NCFILE%iColBounds(NC_RIGHT) = &
      min( max( iColRow_ul(COLUMN), iColRow_ur(COLUMN), iColRow_ll(COLUMN), iColRow_lr(COLUMN) ) + 4, &
                ubound(NCFILE%rX_Coords,1) )


      NCFILE%iRowBounds(NC_TOP) = &
        max( min( iColRow_ul(ROW), iColRow_ur(ROW), iColRow_ll(ROW), iColRow_lr(ROW) ) - 4, &
                  lbound(NCFILE%rY_Coords,1) )

      NCFILE%iRowBounds(NC_BOTTOM) = &
        min( max( iColRow_ul(ROW), iColRow_ur(ROW), iColRow_ll(ROW), iColRow_lr(ROW) ) + 4, &
                  ubound(NCFILE%rY_Coords,1) )

  else

    !> define the entire grid area as the AOI
    NCFILE%iColBounds(NC_LEFT) = lbound(NCFILE%rX_Coords,1)
    NCFILE%iColBounds(NC_RIGHT) = ubound(NCFILE%rX_Coords,1)

    NCFILE%iRowBounds(NC_TOP) = lbound(NCFILE%rY_Coords,1)
    NCFILE%iRowBounds(NC_BOTTOM) = ubound(NCFILE%rY_Coords,1)

  endif

  !> based on the subset of the NetCDF file as determined above, set the
  !> start, count, and stride parameters for use in all further data
  !> retrievals
  call nf_set_start_count_stride(NCFILE)

  !> establish the bounds to iterate over; this can enable horiz or vert flipping
  call nf_set_iteration_bounds(NCFILE)

  !> now that we have (possibly) created a subset, need to get the
  !> **NATIVE** coordinate bounds so that the intermediate grid file
  !> can be created
  call nf_return_native_coord_bounds(NCFILE)

end subroutine netcdf_open_and_prepare_as_input

!----------------------------------------------------------------------

!> Open a NetCDF file in order to save a local copy of the source NetCDF
!! file.
subroutine netcdf_open_and_prepare_as_output(NCFILE, NCFILE_ARCHIVE, &
   iOriginMonth, iOriginDay, iOriginYear, iStartYear, iEndYear, &
   rX, rY)

  type (T_NETCDF4_FILE ) :: NCFILE
  type (T_NETCDF4_FILE ) :: NCFILE_ARCHIVE
  integer (c_int) :: iOriginMonth
  integer (c_int) :: iOriginDay
  integer (c_int) :: iOriginYear
  integer (c_int) :: iStartYear
  integer (c_int) :: iEndYear
  real (c_double), optional :: rX(:,:)
  real (c_double), optional :: rY(:,:)

  ! [ LOCALS ]
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  type (T_NETCDF_DIMENSION), pointer :: pNC_DIM
  integer (c_int) :: iIndex
  integer (c_int) :: iNumCols, iNumRows
  integer (c_int) :: iMinCol, iMaxCol
  integer (c_int) :: iMinRow, iMaxRow
  real (c_double), dimension(:), allocatable :: rX_vec, rY_vec
  character (len=10) :: sOriginText
  character (len=256) :: sFilename

  write(sOriginText, fmt="(i4.4,'-',i2.2,'-',i2.2)") iOriginYear, &
    iOriginMonth, iOriginDay

  iMaxRow = maxval(NCFILE%iRowBounds)
  iMinRow = minval(NCFILE%iRowBounds)
  iMaxCol = maxval(NCFILE%iColBounds)
  iMinCol = minval(NCFILE%iColBounds)

  iNumRows = iMaxRow - iMinRow + 1
  iNumCols = iMaxCol - iMinCol + 1

  allocate(rX_vec(iNumCols))
  allocate(rY_vec(iNumRows))
  rX_vec = NCFILE%rX_Coords(iMinCol:iMaxCol)
  rY_vec = NCFILE%rY_Coords(iMinRow:iMaxRow)

  sFilename = trim(NCFILE%sVarName(NC_Z))//"_"//trim(asCharacter(iStartYear)) &
    //"_"//trim(asCharacter(iEndYear))//"__" &
    //trim(asCharacter(iNumRows)) &
     //"_by_"//trim(asCharacter(iNumCols))//".nc"

  call nf_create(NCFILE=NCFILE_ARCHIVE, sFilename=trim(sFilename) )

  !> set dimension values in the NCFILE struct
  call nf_set_standard_dimensions(NCFILE=NCFILE_ARCHIVE, &
                       iNX=iNumCols, &
                       iNY=iNumRows)

  NCFILE_ARCHIVE%sVarUnits(NC_X) =   NCFILE%sVarUnits(NC_X)
  NCFILE_ARCHIVE%sVarUnits(NC_Y) =   NCFILE%sVarUnits(NC_Y)
  NCFILE_ARCHIVE%sVarUnits(NC_Z) =   NCFILE%sVarUnits(NC_Z)

  !> transfer dimension values to NetCDF file
  call nf_define_dimensions( NCFILE=NCFILE_ARCHIVE )

  !> set variable values in the NCFILE struct
  call nf_set_standard_variables(NCFILE=NCFILE_ARCHIVE, &
       sVarName_z = trim(NCFILE%sVarName(NC_Z)) )

  !> transfer variable values to NetCDF file
  call nf_define_variables(NCFILE=NCFILE_ARCHIVE)

  call nf_get_variable_id_and_type( NCFILE=NCFILE_ARCHIVE )

  call nf_set_standard_attributes(NCFILE=NCFILE_ARCHIVE, &
    sOriginText=sOriginText)

  call nf_set_global_attributes(NCFILE=NCFILE_ARCHIVE, &
     sDataType=trim(NCFILE%sVarName(NC_Z)), &
     sSourceFile=trim(NCFILE%sFilename))

  call nf_put_attributes(NCFILE=NCFILE_ARCHIVE)

  !> enable a low level of data compression for the
  !> variable of interest
  call nf_define_deflate(NCFILE=NCFILE_ARCHIVE, &
     iVarID=NCFILE_ARCHIVE%iVarID(NC_Z), &
     iShuffle=NC_SHUFFLE_YES, &
     iDeflate=NC_DEFLATE_YES, &
     iDeflate_level=2 )

  call nf_enddef(NCFILE=NCFILE_ARCHIVE)

  call nf_put_x_and_y(NCFILE=NCFILE_ARCHIVE, &
       dpX=NCFILE%rX_Coords(iMinCol:iMaxCol), &
       dpY=NCFILE%rY_Coords(iMinRow:iMaxRow) )
!       dpX=rX, &
!       dpY=rY )

!  call netcdf_close_file(NCFILE_ARCHIVE)

end subroutine netcdf_open_and_prepare_as_output

!----------------------------------------------------------------------

subroutine nf_set_z_variable_name(NCFILE, sVarName_z)

  type (T_NETCDF4_FILE ) :: NCFILE
  character (len=*) :: sVarName_z

  NCFILE%sVarName(NC_Z) = sVarName_z

end subroutine nf_set_z_variable_name

!----------------------------------------------------------------------

subroutine nf_set_iteration_bounds(NCFILE)

  type (T_NETCDF4_FILE ) :: NCFILE

  if (NCFILE%lFlipVertical) then
    NCFILE%iRowIter(NC_FIRST) = NCFILE%iNY
    NCFILE%iRowIter(NC_LAST) = 1
    NCFILE%iRowIter(NC_BY) = -1
  else
    NCFILE%iRowIter(NC_FIRST) = 1
    NCFILE%iRowIter(NC_LAST) = NCFILE%iNY
    NCFILE%iRowIter(NC_BY) = 1
  endif

  if (NCFILE%lFlipHorizontal) then
    NCFILE%iColIter(NC_FIRST) = NCFILE%iNX
    NCFILE%iColIter(NC_LAST) = 1
    NCFILE%iColIter(NC_BY) = -1
  else
    NCFILE%iColIter(NC_FIRST) = 1
    NCFILE%iColIter(NC_LAST) = NCFILE%iNX
    NCFILE%iColIter(NC_BY) = 1
  endif

end subroutine nf_set_iteration_bounds

!----------------------------------------------------------------------

subroutine nf_set_start_count_stride(NCFILE)

  type (T_NETCDF4_FILE ) :: NCFILE

  ! [ LOCALS ]
  integer (c_int) :: iIndex

  ! loop over the three (assumed) dimensions of the "Z" variable;
  ! assign appropriate bounds to each
  do iIndex = 0,3

    select case (iIndex)

      case (NC_X)

        !> need to subtract 1 from the start index: we're using the
        !> NetCDF C API, in which index values are relative to zero
        NCFILE%iStart(iIndex) = minval(NCFILE%iColBounds) - 1
        NCFILE%iNX = maxval(NCFILE%iColBounds) - minval(NCFILE%iColBounds) + 1
        NCFILE%iCount(iIndex) = NCFILE%iNX
!        NCFILE%iCount(iIndex) = maxval(NCFILE%iColBounds) - minval(NCFILE%iColBounds)
        NCFILE%iStride(iIndex) = 1_c_size_t

      case (NC_Y)

        !> note: this assumes that the row numbers increase from top to bottom,
        !>       while the Y coordinates decrease top to bottom

        NCFILE%iStart(iIndex) = minval(NCFILE%iRowBounds) - 1
        NCFILE%iNY = maxval(NCFILE%iRowBounds) - minval(NCFILE%iRowBounds) + 1
        NCFILE%iCount(iIndex) = NCFILE%iNY
        !>
        !> count must be set to the number of values! maxval minus minval results
        !> in a diagonal pattern in the input as we read in the incorrect number
        !> of results
!        NCFILE%iCount(iIndex) = maxval(NCFILE%iRowBounds) - minval(NCFILE%iRowBounds)
        NCFILE%iStride(iIndex) = 1_c_size_t

      case (NC_TIME)

        NCFILE%iStart(iIndex) = 0_c_size_t
        NCFILE%iCount(iIndex) = 1_c_size_t
        NCFILE%iStride(iIndex) = 1_c_size_t

      case default

    end select

  enddo

end subroutine nf_set_start_count_stride

!----------------------------------------------------------------------

subroutine nf_return_native_coord_bounds(NCFILE)

  type (T_NETCDF4_FILE ) :: NCFILE

  ! [ LOCALS ]
  real (c_double) :: rXmin, rXmax
  real (c_double) :: rYmin, rYmax

  !> find the (x,y) associated with the column and row number bounds
  rXmin = minval(NCFILE%rX_Coords(NCFILE%iColBounds(NC_LEFT):NCFILE%iColBounds(NC_RIGHT)) )
  rXmax = maxval(NCFILE%rX_Coords(NCFILE%iColBounds(NC_LEFT):NCFILE%iColBounds(NC_RIGHT)) )
  rYmin = minval(NCFILE%rY_Coords(NCFILE%iRowBounds(NC_TOP):NCFILE%iRowBounds(NC_BOTTOM)) )
  rYmax = maxval(NCFILE%rY_Coords(NCFILE%iRowBounds(NC_TOP):NCFILE%iRowBounds(NC_BOTTOM)) )

  NCFILE%rX(NC_LEFT) = rXmin - NCFILE%rGridCellSizeX * dpHALF
  NCFILE%rX(NC_RIGHT) = rXmax + NCFILE%rGridCellSizeX * dpHALF
  NCFILE%rY(NC_TOP) = rYmax + NCFILE%rGridCellSizeY * dpHALF
  NCFILE%rY(NC_BOTTOM) = rYmin - NCFILE%rGridCellSizeY * dpHALF

#ifdef DEBUG_PRINT
  print *, "Grid cell size (X): ", NCFILE%rGridCellSizeX
  print *, "Grid cell size (Y): ", NCFILE%rGridCellSizeY

  print *, "Bounds of data subset area, in native coordinates"
  print *, "X (left): ", NCFILE%rX(NC_LEFT)
  print *, "X (right): ", NCFILE%rX(NC_RIGHT)
  print *, "Y (top): ", NCFILE%rY(NC_TOP)
  print *, "Y (bottom): ", NCFILE%rY(NC_BOTTOM)
#endif

end subroutine nf_return_native_coord_bounds

!----------------------------------------------------------------------

subroutine nf_get_x_and_y(NCFILE)

  type (T_NETCDF4_FILE ) :: NCFILE

  integer (c_int) :: iVarIndex_x, iVarIndex_y
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR_x, pNC_VAR_y
  type (T_NETCDF_DIMENSION), pointer :: pNC_DIM_x, pNC_DIM_y
  integer (c_int) :: iLowerBound, iUpperBound
  integer (c_int) :: iStat

  iVarIndex_x = NCFILE%iVarIndex(NC_X)
  iVarIndex_y = NCFILE%iVarIndex(NC_Y)

  call assert(iVarIndex_x >= lbound(NCFILE%pNC_VAR,1) &
    .and. iVarIndex_x <= ubound(NCFILE%pNC_VAR,1), &
    "INTERNAL PROGRAMMING ERROR - Index out of bounds", trim(__FILE__), __LINE__)

  call assert(iVarIndex_y >= lbound(NCFILE%pNC_VAR,1) &
    .and. iVarIndex_y <= ubound(NCFILE%pNC_VAR,1), &
    "INTERNAL PROGRAMMING ERROR - Index out of bounds", trim(__FILE__), __LINE__)

  pNC_VAR_x => NCFILE%pNC_VAR(iVarIndex_x)
  pNC_VAR_y => NCFILE%pNC_VAR(iVarIndex_y)

  call assert( pNC_VAR_x%iNumberOfDimensions == 1, &
    "Dimensions other than one for the x-coordinate variable are currently unsupported.", &
    trim(__FILE__), __LINE__)

  call assert( pNC_VAR_y%iNumberOfDimensions == 1, &
    "Dimensions other than one for the y-coordinate variable are currently unsupported.", &
    trim(__FILE__), __LINE__)

  pNC_DIM_x => NCFILE%pNC_DIM( pNC_VAR_x%iNC_DimID(0) )
  pNC_DIM_y => NCFILE%pNC_DIM( pNC_VAR_y%iNC_DimID(0) )

  allocate( NCFILE%rX_Coords( pNC_DIM_x%iNC_DimSize ), stat=iStat )
  call assert(iStat==0, "Failed to allocate memory for X-coordinate values", &
    trim(__FILE__), __LINE__)

  allocate (NCFILE%rY_Coords( pNC_DIM_y%iNC_DimSize  ), stat=iStat )
  call assert(iStat==0, "Failed to allocate memory for Y-coordinate values", &
    trim(__FILE__), __LINE__)

  call nf_get_variable_vector_double(NCFILE=NCFILE, &
       iNC_VarID=pNC_VAR_x%iNC_VarID, &
       iNC_Start=0_c_size_t, &
       iNC_Count=pNC_DIM_x%iNC_DimSize, &
       iNC_Stride=1_c_size_t, &
       dpNC_Vars=NCFILE%rX_Coords)

  call nf_get_variable_vector_double(NCFILE=NCFILE, &
       iNC_VarID=pNC_VAR_y%iNC_VarID, &
       iNC_Start=0_c_size_t, &
       iNC_Count=pNC_DIM_y%iNC_DimSize, &
       iNC_Stride=1_c_size_t, &
       dpNC_Vars=NCFILE%rY_Coords)

  NCFILE%rX_Coords = NCFILE%rX_Coords + NCFILE%rX_Coord_AddOffset
  NCFILE%rY_Coords = NCFILE%rY_Coords + NCFILE%rY_Coord_AddOffset

  iLowerBound = lbound(NCFILE%rX_Coords, 1)
  iUpperBound = ubound(NCFILE%rX_Coords, 1)

  if (NCFILE%rX_Coords(iUpperBound) > NCFILE%rX_Coords(iLowerBound) ) then
    NCFILE%lX_IncreasesWithIndex = lTRUE
  else
    NCFILE%lX_IncreasesWithIndex = lFALSE
  endif

  iLowerBound = lbound(NCFILE%rY_Coords, 1)
  iUpperBound = ubound(NCFILE%rY_Coords, 1)

  if (NCFILE%rY_Coords(iUpperBound) > NCFILE%rY_Coords(iLowerBound) ) then
    NCFILE%lY_IncreasesWithIndex = lTRUE
  else
    NCFILE%lY_IncreasesWithIndex = lFALSE
  endif

  call assert(pNC_DIM_x%iNC_DimSize > 2, "INTERNAL PROGRAMMING ERROR - " &
    //"NetCDF X dimension size must be greater than 2.", trim(__FILE__), __LINE__)

  call assert(pNC_DIM_y%iNC_DimSize > 2, "INTERNAL PROGRAMMING ERROR - " &
    //"NetCDF Y dimension size must be greater than 2.", trim(__FILE__), __LINE__)

  NCFILE%rGridCellSizeX = ( maxval(NCFILE%rX_Coords) &
                                - minval(NCFILE%rX_Coords) ) &
                                / real (pNC_DIM_x%iNC_DimSize - 1, c_double)

  NCFILE%rGridCellSizeY = ( maxval(NCFILE%rY_Coords) &
                                - minval(NCFILE%rY_Coords) ) &
                                / real (pNC_DIM_y%iNC_DimSize - 1, c_double)

end subroutine nf_get_x_and_y

!----------------------------------------------------------------------

subroutine nf_open_file(NCFILE, sFilename, iLU)

  type (T_NETCDF4_FILE ) :: NCFILE
  character (len=*) :: sFilename
  integer (c_int), optional :: iLU

  ! [ LOCALS ]
  logical (c_bool) :: lFileOpen

  call echolog( "Attempting to open READONLY NetCDF file: " &
    //dquote(sFilename))

  call nf_trap( nc_open(trim(sFilename)//c_null_char, &
                NC_READONLY, NCFILE%iNCID), __FILE__, __LINE__ )

  call nf_trap( nc_inq_format(ncid=NCFILE%iNCID, formatp=NCFILE%iFileFormat), &
               __FILE__, __LINE__)

  call echolog( "   Succeeded.  ncid: "//trim(asCharacter(NCFILE%iNCID)) &
         //"  format: "//trim(NETCDF_FORMAT_STRING(NCFILE%iFileFormat) ) )

  NCFILE%sFilename = sFilename

!  call netcdf_dump_cdl( NCFILE, LU_STD_OUT)

!  NCFILE%dpFirstAndLastTimeValues = nf_get_first_and_last(NCFILE=NCFILE, &
!    iVarIndex=NCFILE%iVarIndex(NC_TIME) )

!  call nf_calculate_time_range(NCFILE)

  if( present(iLU) ) then
    inquire (unit=iLU, opened=lFileOpen)
    if ( lFileOpen )  call netcdf_dump_cdl( NCFILE, iLU)
  endif

end subroutine nf_open_file

!----------------------------------------------------------------------

subroutine netcdf_open_file(NCFILE, sFilename, iLU)

  type (T_NETCDF4_FILE ) :: NCFILE
  character (len=*) :: sFilename
  integer (c_int), optional :: iLU

  if (present(iLU) ) then

    call nf_open_file(NCFILE=NCFILE,        &
                      sFilename=sFilename,  &
                      iLU=iLU)

  else

    call nf_open_file(NCFILE=NCFILE,        &
                      sFilename=sFilename)

  endif

  !> Similarly, the structure of the file may be slightly different from the
  !> previous file
  call nf_populate_dimension_struct( NCFILE )
  call nf_populate_variable_struct( NCFILE )

  !> CANNOT ASSUME THAT THIS WILL REMAIN CONSTANT ACROSS FILES FROM THE
  !> SAME PROVIDER!! MUST UPDATE TO ENSURE THAT THE INDICES ARE STILL RELEVANT
  call nf_get_variable_id_and_type( NCFILE )

  !> retrieve the units used to encode the time variable (e.g. hours, days)
  call nf_get_time_units( NCFILE )

  !> retrieve the time value specific to this file
  call nf_get_time_vals( NCFILE )

  NCFILE%dpFirstAndLastTimeValues = nf_get_first_and_last( NCFILE=NCFILE,                           &
                                                           iVarIndex=NCFILE%iVarIndex( NC_TIME ) )

  call nf_calculate_time_range(NCFILE)

  !> establish scale_factor and add_offset values, if present
  call nf_get_scale_and_offset(NCFILE=NCFILE)

end subroutine netcdf_open_file

!----------------------------------------------------------------------

subroutine nf_trap( iResultCode, sFilename, iLineNumber )

  integer (c_int) :: iResultCode
  character (len=*), optional :: sFilename
  integer (c_int), optional :: iLineNumber

  ! [ LOCALS ]
  type(c_ptr) :: cpResult
  character (len=256) :: sTextString
  character (len=256) :: sFile
  integer (c_int) :: iLine

  if (iResultCode /= 0) then

    if (present(sFilename)) then
      sFile = trim(sFilename)
    else
      sFile = trim(__FILE__)
    endif

    if (present(iLinenumber)) then
      iLine = iLinenumber
    else
      iLine = __LINE__
    endif

    cpResult = nc_strerror(iResultCode)
    sTextString = char_ptr_to_fortran_string( cpResult )

    call echolog( "NetCDF ERROR: "//dquote( sTextString  )//" | error code was: " &
      //trim(asCharacter(iResultCode)) )

    call assert(lFALSE, "SWB is stopping due to a problem reading or accessing" &
      //" a NetCDF file", trim(sFile), iLine)

  endif

end subroutine nf_trap

!----------------------------------------------------------------------

subroutine netcdf_close_file( NCFILE)

  type (T_NETCDF4_FILE ) :: NCFILE

  call echolog( "Closing NetCDF file with name: "//dquote(NCFILE%sFilename))
  call nf_trap( nc_close(NCFILE%iNCID), __FILE__, __LINE__ )

!  call nf_deallocate_data_struct( NCFILE=NCFILE )

end subroutine netcdf_close_file

!----------------------------------------------------------------------

subroutine netcdf_deallocate_data_struct( NCFILE )

  type (T_NETCDF4_FILE ) :: NCFILE

  ! [ LOCALS ]
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  integer (c_int) :: iIndex

  do iIndex=0, NCFILE%iNumberOfVariables - 1

    pNC_VAR => NCFILE%pNC_VAR(iIndex)

    if (pNC_VAR%iNumberOfAttributes == 0 ) cycle

    if (associated( pNC_VAR%pNC_ATT ))  deallocate( pNC_VAR%pNC_ATT )
    pNC_VAR%pNC_ATT => null()

  enddo

  if (associated( NCFILE%pNC_VAR ))  deallocate( NCFILE%pNC_VAR )
  if (associated( NCFILE%pNC_ATT ))  deallocate( NCFILE%pNC_ATT )
  if (associated( NCFILE%pNC_DIM ))  deallocate( NCFILE%pNC_DIM )

  NCFILE%pNC_VAR => null()
  NCFILE%pNC_ATT => null()
  NCFILE%pNC_DIM => null()

end subroutine netcdf_deallocate_data_struct

!----------------------------------------------------------------------

subroutine netcdf_nullify_data_struct( NCFILE )

  type (T_NETCDF4_FILE ) :: NCFILE

  ! [ LOCALS ]

  NCFILE%pNC_VAR => null()
  NCFILE%pNC_ATT => null()
  NCFILE%pNC_DIM => null()

end subroutine netcdf_nullify_data_struct

!----------------------------------------------------------------------

subroutine nf_populate_dimension_struct( NCFILE )

  type (T_NETCDF4_FILE) :: NCFILE
  integer (c_int) :: iStat
  integer (c_int) :: iIndex
  character (len=256) :: sDimName


  call nf_trap( nc_inq_ndims(ncid=NCFILE%iNCID, ndimsp=NCFILE%iNumberOfDimensions), &
                __FILE__, __LINE__ )

  iStat = 0
  if (associated(NCFILE%pNC_DIM) ) deallocate(NCFILE%pNC_DIM, stat=iStat)
  call assert(iStat == 0, "Could not deallocate memory for NC_DIM member in NC_FILE defined type", &
    trim(__FILE__), __LINE__)

  allocate(NCFILE%pNC_DIM( 0 : NCFILE%iNumberOfDimensions-1), stat=iStat )
  call assert(iStat == 0, "Could not allocate memory for NC_DIM member in NC_FILE defined type", &
    trim(__FILE__), __LINE__)

  ! NetCDF 3 function
  call nf_trap( nc_inq_unlimdim(ncid=NCFILE%iNCID, unlimdimidp=NCFILE%iNC3_UnlimitedDimensionNumber), &
               __FILE__, __LINE__ )

  do iIndex = 0, NCFILE%iNumberOfDimensions-1

    call nf_trap(nc_inq_dim(ncid=NCFILE%iNCID, dimid=iIndex, &
      name=sDimName, &
      lenp=NCFILE%pNC_DIM(iIndex)%iNC_DimSize), __FILE__, __LINE__ )

    NCFILE%pNC_DIM(iIndex)%iNC_DimID = iIndex
    NCFILE%pNC_DIM(iIndex)%sDimensionName = c_to_fortran_string(sDimName)

  enddo

end subroutine nf_populate_dimension_struct

!----------------------------------------------------------------------

subroutine nf_populate_variable_struct( NCFILE )

  type (T_NETCDF4_FILE) :: NCFILE

  type (T_NETCDF_ATTRIBUTE), pointer :: pNC_ATT
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  integer (c_int) :: iStat
  integer (c_int) :: iIndex, iIndex2 , iIndex3
  character (len=256) :: sVarName
  character (len=256) :: sAttName
  character (len=512) :: sAttValue
  integer (c_int), dimension(0:25) :: iAttValue
  integer (c_short), dimension(0:25) :: i2AttValue
  real (c_double), dimension(0:25) :: cdAttValue

  call nf_trap( nc_inq_nvars(ncid=NCFILE%iNCID, nvarsp=NCFILE%iNumberOfVariables), &
       __FILE__, __LINE__ )

  iStat = 0
  if (associated(NCFILE%pNC_VAR) ) deallocate(NCFILE%pNC_VAR, stat=iStat)
  call assert(iStat == 0, "Could not deallocate memory for NC_VAR member in NC_FILE defined type", &
    trim(__FILE__), __LINE__)

  allocate(NCFILE%pNC_VAR( 0 : NCFILE%iNumberOfVariables-1), stat=iStat )
  call assert(iStat == 0, "Could not allocate memory for NC_VAR member in NC_FILE defined type", &
    trim(__FILE__), __LINE__)

  do iIndex = 0, NCFILE%iNumberOfVariables-1

    pNC_VAR => NCFILE%pNC_VAR(iIndex)

    call nf_trap(nc_inq_var(ncid=NCFILE%iNCID, &
        varid=iIndex, &
        name=sVarName, &
        xtypep=pNC_VAR%iNC_VarType, &
        ndimsp=pNC_VAR%iNumberOfDimensions, &
        dimidsp=pNC_VAR%iNC_DimID, &
        nattsp=pNC_VAR%iNumberOfAttributes ), __FILE__, __LINE__ )

    pNC_VAR%iNC_VarID = iIndex
    pNC_VAR%sVariableName = c_to_fortran_string(sVarName)

    if( pNC_VAR%iNumberOfAttributes > 0 ) then

      if (associated(pNC_VAR%pNC_ATT) ) deallocate(pNC_VAR%pNC_ATT, stat=iStat)
      call assert(iStat == 0, "Could not deallocate memory for NC_ATT member within NC_VAR in NC_FILE defined type", &
        trim(__FILE__), __LINE__)

      allocate( pNC_VAR%pNC_ATT( 0:pNC_VAR%iNumberOfAttributes - 1 ), stat = iStat)
      call assert(iStat == 0, "Could not allocate memory for NC_ATT member within NC_VAR in NC_FILE defined type", &
        trim(__FILE__), __LINE__)

      do iIndex2=0, pNC_VAR%iNumberOfAttributes - 1

        pNC_ATT => pNC_VAR%pNC_ATT(iIndex2)

        call nf_populate_attribute_struct( NCFILE=NCFILE, pNC_ATT=pNC_ATT, &
          iNC_VarID=iIndex, iAttNum=iIndex2 )

      enddo

    endif

  enddo

  call nf_trap( nc_inq_natts(ncid=NCFILE%iNCID, ngattsp=NCFILE%iNumberOfAttributes), &
       __FILE__, __LINE__ )


  if (associated(NCFILE%pNC_ATT) )  deallocate(NCFILE%pNC_ATT, stat=iStat)
  call assert(iStat == 0, "Could not deallocate memory for NC_ATT member within NC_FILE defined type", &
    trim(__FILE__), __LINE__)

  allocate(NCFILE%pNC_ATT(0:NCFILE%iNumberOfAttributes - 1), stat=iStat )
  call assert(iStat == 0, "Could not allocate memory for NC_ATT member within NC_FILE defined type", &
    trim(__FILE__), __LINE__)

  do iIndex=0, NCFILE%iNumberOfAttributes - 1
    pNC_ATT => NCFILE%pNC_ATT(iIndex)

    call nf_populate_attribute_struct( NCFILE=NCFILE, pNC_ATT=pNC_ATT, &
      iNC_VarID=NC_GLOBAL, iAttNum=iIndex )

  enddo

end subroutine nf_populate_variable_struct

!----------------------------------------------------------------------

subroutine nf_populate_attribute_struct( NCFILE, pNC_ATT, iNC_VarID, iAttNum )

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE
  type (T_NETCDF_ATTRIBUTE), pointer :: pNC_ATT
  integer (c_int) :: iNC_VarID
  integer (c_int) :: iAttNum

  ![ LOCALS ]
  integer (c_int) :: iStat
  character (len=256) :: sVarName
  character (len=256) :: sAttName
  integer (c_int) :: iIndex
  integer (c_int) :: iLength

  call nf_trap( nc_inq_attname(ncid=NCFILE%iNCID, &
    varid=iNC_VarID, &
    attnum=iAttNum, &
    name=sAttName), __FILE__, __LINE__ )

  pNC_ATT%sAttributeName = c_to_fortran_string(sAttName)

  call nf_trap( nc_inq_att(ncid=NCFILE%iNCID, &
    varid=iNC_VarID, &
    name=sAttName, &
    xtypep=pNC_ATT%iNC_AttType, &
    lenp=pNC_ATT%iNC_AttSize), __FILE__, __LINE__ )

  iLength = pNC_ATT%iNC_AttSize

  iStat = 0
  allocate(pNC_ATT%sAttValue(0:iLength-1), stat=iStat )
  call assert(iStat==0, "INTERNAL PROGRAMMING ERROR - problem allocating memory", &
    trim(__FILE__), __LINE__)
  pNC_ATT%sAttValue = ""

  select case(pNC_ATT%iNC_AttType)

    case (NC_CHAR)

      call nf_trap( nc_get_att_text(ncid=NCFILE%iNCID, &
        varid=iNC_VarID, &
        name=sAttName, &
        ip=pNC_ATT%sAttValue), __FILE__, __LINE__ )

        pNC_ATT%sAttValue = c_to_fortran_string(pNC_ATT%sAttValue)

    case (NC_SHORT)

      allocate(pNC_ATT%i2AttValue(0:iLength-1), stat=iStat )
      call assert(iStat==0, "INTERNAL PROGRAMMING ERROR - problem allocating memory", &
        trim(__FILE__), __LINE__)


      call nf_trap( nc_get_att_short(ncid=NCFILE%iNCID, &
        varid=iNC_VarID, &
        name=sAttName, &
        ip=pNC_ATT%i2AttValue), __FILE__, __LINE__ )

      pNC_ATT%sAttValue = asCharacter(pNC_ATT%i2AttValue)

    case (NC_INT)

      allocate(pNC_ATT%iAttValue(0:iLength-1), stat=iStat )
      call assert(iStat==0, "INTERNAL PROGRAMMING ERROR - problem allocating memory", &
        trim(__FILE__), __LINE__)

      call nf_trap( nc_get_att_int(ncid=NCFILE%iNCID, &
        varid=iNC_VarID, &
        name=sAttName, &
        ip=pNC_ATT%iAttValue), __FILE__, __LINE__ )

      pNC_ATT%sAttValue = asCharacter(pNC_ATT%iAttValue)

    case (NC_FLOAT)

      allocate(pNC_ATT%rAttValue(0:iLength-1), stat=iStat )
      call assert(iStat==0, "INTERNAL PROGRAMMING ERROR - problem allocating memory", &
        trim(__FILE__), __LINE__)


      call nf_trap( nc_get_att_float(ncid=NCFILE%iNCID, &
        varid=iNC_VarID, &
        name=sAttName, &
        ip=pNC_ATT%rAttValue), __FILE__, __LINE__ )

      pNC_ATT%sAttValue = asCharacter(pNC_ATT%rAttValue)

    case (NC_DOUBLE)

      allocate(pNC_ATT%dpAttValue(0:iLength-1), stat=iStat )
      call assert(iStat==0, "INTERNAL PROGRAMMING ERROR - problem allocating memory", &
        trim(__FILE__), __LINE__)

      call nf_trap( nc_get_att_double(ncid=NCFILE%iNCID, &
        varid=iNC_VarID, &
        name=sAttName, &
        ip=pNC_ATT%dpAttValue), __FILE__, __LINE__ )

      pNC_ATT%sAttValue = asCharacter(pNC_ATT%dpAttValue)

    case default

  end select

end subroutine nf_populate_attribute_struct

!----------------------------------------------------------------------

! this routine is called in order to update the NetCDF file TIME index
! prior to the call to get a slice of data

function netcdf_update_time_starting_index(NCFILE, iJulianDay)  result(lDateTimeFound)

  type (T_NETCDF4_FILE) :: NCFILE
  integer (c_int) :: iJulianDay
  logical (c_bool) :: lDateTimeFound

  ! [ LOCALS ]
  real (c_double) :: rNC_DateTime
  integer (c_int) :: iMonth, iDay, iYear

  NCFILE%iStart(NC_TIME) = nf_julian_day_to_index_adj( NCFILE=NCFILE,                                &
                                                       rJulianDay=real(iJulianDay, c_double ) )

  if (NCFILE%iStart(NC_TIME) < 0) then
    NCFILE%iStart(NC_TIME) = 0
    lDateTimeFound = lFALSE
  else
    lDateTimeFound = lTRUE
  endif

end function netcdf_update_time_starting_index

!----------------------------------------------------------------------

subroutine netcdf_get_variable_slice(NCFILE, rValues, dpValues, iValues)

  type (T_NETCDF4_FILE), intent(inout)       :: NCFILE
  real (c_float), dimension(:,:), optional   :: rValues
  real (c_double), dimension(:,:), optional  :: dpValues
  integer (c_int), dimension(:,:), optional  :: iValues

  real (c_float), allocatable     :: rTempVals(:,:)
  integer (c_short), allocatable  :: i2TempVals(:,:)
  integer (c_int), allocatable    :: iTempVals(:,:)
  real (c_double), allocatable    :: dpTempVals(:,:)
  integer (c_int)                 :: nrow, ncol

  if (present( rValues) ) then
    nrow = size(rValues,2)
    ncol = size(rValues,1)
  elseif (present( dpValues) ) then
    nrow = size(dpValues,2)
    ncol = size(dpValues,1)
  elseif (present( iValues) ) then
    nrow = size(iValues,2)
    ncol = size(iValues,1)
  else
    call assert( lFALSE, "Internal programming error: unhandled data type",                &
      __FILE__, __LINE__)
  endif

  if (   NCFILE%iVarType(NC_Z) == NC_SHORT                                     &
    .or. NCFILE%iVarType(NC_Z) == NC_USHORT) then

    allocate( i2TempVals(ncol, nrow))

    call nf_get_variable_slice_short(NCFILE, i2TempVals)

    if (present(rValues))  rValues = asReal(i2TempVals)
    if (present(iValues))  iValues = i2TempVals
    if (present(dpValues))  dpValues = asDouble(i2TempVals)

  elseif (   NCFILE%iVarType(NC_Z) == NC_INT                                   &
    .or. NCFILE%iVarType(NC_Z) == NC_UINT) then

    allocate( iTempVals(ncol, nrow))
    call nf_get_variable_slice_int(NCFILE, iTempVals)

    if (present(rValues))  rValues = asReal(iTempVals)
    if (present(iValues))  iValues = iTempVals
    if (present(dpValues))  dpValues = asDouble(iTempVals)

  elseif (NCFILE%iVarType(NC_Z) == NC_FLOAT) then

    allocate( rTempVals(ncol, nrow))
    call nf_get_variable_slice_float(NCFILE, rTempVals)

    if (present(rValues))  rValues = rTempVals
    if (present(iValues))  iValues = asInt(rTempVals)
    if (present(dpValues))  dpValues = asDouble(rTempVals)

  elseif (NCFILE%iVarType(NC_Z) == NC_DOUBLE) then

    allocate( dpTempVals(ncol, nrow))
    call nf_get_variable_slice_double(NCFILE, dpTempVals)

    if (present(rValues))  rValues = asReal(dpTempVals)
    if (present(iValues))  iValues = asInt(dpTempVals)
    if (present(dpValues))  dpValues = dpTempVals

  else

    call warn("Failed to find a method to retrieve data of the given type.", __FILE__, __LINE__)

    print *, 'var type: ',NCFILE%iVarType(NC_Z)

  endif

end subroutine netcdf_get_variable_slice

!----------------------------------------------------------------------

subroutine nf_get_variable_slice_short(NCFILE, i2Values)

  type (T_NETCDF4_FILE) :: NCFILE
  integer (c_short), dimension(:,:) :: i2Values

  ! [ LOCALS ]
  !! dimension #1 = column (iNX)
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR

  integer (c_short), dimension(size(i2Values,2) * size(i2Values,1)) :: iTemp

  integer (c_int) :: iStat
  integer (c_int) :: iRow, iCol, iIndex
  integer (c_int) :: iFromRow, iToRow, iByRow
  integer (c_int) :: iFromCol, iToCol, iByCol

  iFromRow = NCFILE%iRowIter(NC_FIRST)
  iToRow = NCFILE%iRowIter(NC_LAST)
  iByRow = NCFILE%iRowIter(NC_BY)

  iFromCol = NCFILE%iColIter(NC_FIRST)
  iToCol = NCFILE%iColIter(NC_LAST)
  iByCol = NCFILE%iColIter(NC_BY)

  pNC_VAR => NCFILE%pNC_VAR(nf_return_VarIndex( NCFILE, NCFILE%iVarID(NC_Z)) )

  select case (NCFILE%sVariableOrder)

    case ("txy")    ! time, col, row

      call nf_get_variable_array_as_vector_short(NCFILE=NCFILE, &
        iNC_VarID=NCFILE%iVarID(NC_Z), &
        iNC_Start=[NCFILE%iStart(NC_TIME), NCFILE%iStart(NC_X), NCFILE%iStart(NC_Y)], &
        iNC_Count=[NCFILE%iCount(NC_TIME), NCFILE%iCount(NC_X), NCFILE%iCount(NC_Y)], &
        iNC_Stride=[NCFILE%iStride(NC_TIME), NCFILE%iStride(NC_X), NCFILE%iStride(NC_Y)], &
        iNC_Vars=iTemp)

        iIndex = 0
        do iCol=iFromCol, iToCol, iByCol
          do iRow=iFromRow, iToRow, iByRow
            iIndex = iIndex + 1
            i2Values(iCol,iRow) = iTemp(iIndex)
          enddo
        enddo

    case ("tyx")    ! time, row, col

      call nf_get_variable_array_as_vector_short(NCFILE=NCFILE, &
        iNC_VarID=NCFILE%iVarID(NC_Z), &
        iNC_Start=[NCFILE%iStart(NC_TIME), NCFILE%iStart(NC_Y), NCFILE%iStart(NC_X)], &
        iNC_Count=[NCFILE%iCount(NC_TIME), NCFILE%iCount(NC_Y), NCFILE%iCount(NC_X)], &
        iNC_Stride=[NCFILE%iStride(NC_TIME), NCFILE%iStride(NC_Y), NCFILE%iStride(NC_X)], &
        iNC_Vars=iTemp)

        iIndex = 0
        do iRow=iFromRow, iToRow, iByRow
          do iCol=iFromCol, iToCol, iByCol
            iIndex = iIndex + 1
            i2Values(iCol,iRow) = iTemp(iIndex)
          enddo
        enddo

  end select

end subroutine nf_get_variable_slice_short



subroutine nf_get_variable_slice_int(NCFILE, iValues)

  type (T_NETCDF4_FILE) :: NCFILE
  integer (c_int), dimension(:,:) :: iValues

  ! [ LOCALS ]
  !! dimension #1 = column (iNX)
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR

  integer (c_int), dimension(size(iValues,2) * size(iValues,1)) :: iTemp

  integer (c_int) :: iStat
  integer (c_int) :: iRow, iCol, iIndex
  integer (c_int) :: iFromRow, iToRow, iByRow
  integer (c_int) :: iFromCol, iToCol, iByCol

  iFromRow = NCFILE%iRowIter(NC_FIRST)
  iToRow = NCFILE%iRowIter(NC_LAST)
  iByRow = NCFILE%iRowIter(NC_BY)

  iFromCol = NCFILE%iColIter(NC_FIRST)
  iToCol = NCFILE%iColIter(NC_LAST)
  iByCol = NCFILE%iColIter(NC_BY)

  pNC_VAR => NCFILE%pNC_VAR(nf_return_VarIndex( NCFILE, NCFILE%iVarID(NC_Z)) )

  select case (NCFILE%sVariableOrder)

    case ("txy")    ! time, col, row

      call nf_get_variable_array_as_vector_int(NCFILE=NCFILE, &
        iNC_VarID=NCFILE%iVarID(NC_Z), &
        iNC_Start=[NCFILE%iStart(NC_TIME), NCFILE%iStart(NC_X), NCFILE%iStart(NC_Y)], &
        iNC_Count=[NCFILE%iCount(NC_TIME), NCFILE%iCount(NC_X), NCFILE%iCount(NC_Y)], &
        iNC_Stride=[NCFILE%iStride(NC_TIME), NCFILE%iStride(NC_X), NCFILE%iStride(NC_Y)], &
        iNC_Vars=iTemp)

        iIndex = 0
        do iCol=iFromCol, iToCol, iByCol
          do iRow=iFromRow, iToRow, iByRow
            iIndex = iIndex + 1
            iValues(iCol,iRow) = iTemp(iIndex)
          enddo
        enddo

    case ("tyx")    ! time, row, col

      call nf_get_variable_array_as_vector_int(NCFILE=NCFILE, &
        iNC_VarID=NCFILE%iVarID(NC_Z), &
        iNC_Start=[NCFILE%iStart(NC_TIME), NCFILE%iStart(NC_Y), NCFILE%iStart(NC_X)], &
        iNC_Count=[NCFILE%iCount(NC_TIME), NCFILE%iCount(NC_Y), NCFILE%iCount(NC_X)], &
        iNC_Stride=[NCFILE%iStride(NC_TIME), NCFILE%iStride(NC_Y), NCFILE%iStride(NC_X)], &
        iNC_Vars=iTemp)

        iIndex = 0
        do iRow=iFromRow, iToRow, iByRow
          do iCol=iFromCol, iToCol, iByCol
            iIndex = iIndex + 1
            iValues(iCol,iRow) = iTemp(iIndex)
          enddo
        enddo

    case default

      call warn("INTERNAL PROGRAMMING ERROR: Unhandled select case. Program will probably fail.", __FILE__, __LINE__)


  end select

end subroutine nf_get_variable_slice_int

!----------------------------------------------------------------------

subroutine nf_get_variable_slice_float(NCFILE, rValues)

  type (T_NETCDF4_FILE) :: NCFILE
  real (c_float), dimension(:,:) :: rValues

  ! [ LOCALS ]
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  real (c_float), dimension(size(rValues,2) * size(rValues,1)) :: rTemp
  integer (c_int) :: iStat
  integer (c_int) :: iRow, iCol, iIndex
  integer (c_int) :: iFromRow, iToRow, iByRow
  integer (c_int) :: iFromCol, iToCol, iByCol

  iFromRow = NCFILE%iRowIter(NC_FIRST)
  iToRow = NCFILE%iRowIter(NC_LAST)
  iByRow = NCFILE%iRowIter(NC_BY)

  iFromCol = NCFILE%iColIter(NC_FIRST)
  iToCol = NCFILE%iColIter(NC_LAST)
  iByCol = NCFILE%iColIter(NC_BY)

  pNC_VAR => NCFILE%pNC_VAR(nf_return_VarIndex( NCFILE, NCFILE%iVarID(NC_Z)) )

  select case (NCFILE%sVariableOrder)

    case ("txy")    ! time, col, row

      call nf_get_variable_array_as_vector_float(NCFILE=NCFILE, &
        iNC_VarID=NCFILE%iVarID(NC_Z), &
        iNC_Start=[NCFILE%iStart(NC_TIME), NCFILE%iStart(NC_X), NCFILE%iStart(NC_Y)], &
        iNC_Count=[NCFILE%iCount(NC_TIME), NCFILE%iCount(NC_X), NCFILE%iCount(NC_Y)], &
        iNC_Stride=[NCFILE%iStride(NC_TIME), NCFILE%iStride(NC_X), NCFILE%iStride(NC_Y)], &
        rNC_Vars=rTemp)

      iIndex = 0
      do iCol=iFromCol, iToCol, iByCol
        do iRow=iFromRow, iToRow, iByRow
          iIndex = iIndex + 1
          rValues(iCol,iRow) = rTemp(iIndex)
        enddo
      enddo

    case ("tyx")    ! time, row, col

      call nf_get_variable_array_as_vector_float(NCFILE=NCFILE, &
        iNC_VarID=NCFILE%iVarID(NC_Z), &
        iNC_Start=[NCFILE%iStart(NC_TIME), NCFILE%iStart(NC_Y), NCFILE%iStart(NC_X)], &
        iNC_Count=[NCFILE%iCount(NC_TIME), NCFILE%iCount(NC_Y), NCFILE%iCount(NC_X)], &
        iNC_Stride=[NCFILE%iStride(NC_TIME), NCFILE%iStride(NC_Y), NCFILE%iStride(NC_X)], &
        rNC_Vars=rTemp)

      iIndex = 0
      do iRow=iFromRow, iToRow, iByRow
        do iCol=iFromCol, iToCol, iByCol
          iIndex = iIndex + 1
          rValues(iCol,iRow) = rTemp(iIndex)
        enddo
      enddo

  end select

end subroutine nf_get_variable_slice_float

!----------------------------------------------------------------------

subroutine nf_get_variable_slice_double(NCFILE, dpValues)

  type (T_NETCDF4_FILE) :: NCFILE
  real (c_double), dimension(:,:) :: dpValues

  ! [ LOCALS ]
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  real (c_double), dimension(size(dpValues,2) * size(dpValues,1)) :: dpTemp
  integer (c_int) :: iStat
  integer (c_int) :: iRow, iCol, iIndex
  integer (c_int) :: iFromRow, iToRow, iByRow
  integer (c_int) :: iFromCol, iToCol, iByCol

  iFromRow = NCFILE%iRowIter(NC_FIRST)
  iToRow = NCFILE%iRowIter(NC_LAST)
  iByRow = NCFILE%iRowIter(NC_BY)

  iFromCol = NCFILE%iColIter(NC_FIRST)
  iToCol = NCFILE%iColIter(NC_LAST)
  iByCol = NCFILE%iColIter(NC_BY)

  pNC_VAR => NCFILE%pNC_VAR(nf_return_VarIndex( NCFILE, NCFILE%iVarID(NC_Z)) )

  select case (NCFILE%sVariableOrder)

    case ("txy")    ! time, col, row

      call nf_get_variable_array_as_vector_double(NCFILE=NCFILE, &
        iNC_VarID=NCFILE%iVarID(NC_Z), &
        iNC_Start=[NCFILE%iStart(NC_TIME), NCFILE%iStart(NC_X), NCFILE%iStart(NC_Y)], &
        iNC_Count=[NCFILE%iCount(NC_TIME), NCFILE%iCount(NC_X), NCFILE%iCount(NC_Y)], &
        iNC_Stride=[NCFILE%iStride(NC_TIME), NCFILE%iStride(NC_X), NCFILE%iStride(NC_Y)], &
        dpNC_Vars=dpTemp)

      iIndex = 0
      do iCol=iFromCol, iToCol, iByCol
        do iRow=iFromRow, iToRow, iByRow
          iIndex = iIndex + 1
          dpValues(iCol,iRow) = dpTemp(iIndex)
        enddo
      enddo

    case ("tyx")    ! time, row, col

      call nf_get_variable_array_as_vector_double(NCFILE=NCFILE, &
        iNC_VarID=NCFILE%iVarID(NC_Z), &
        iNC_Start=[NCFILE%iStart(NC_TIME), NCFILE%iStart(NC_Y), NCFILE%iStart(NC_X)], &
        iNC_Count=[NCFILE%iCount(NC_TIME), NCFILE%iCount(NC_Y), NCFILE%iCount(NC_X)], &
        iNC_Stride=[NCFILE%iStride(NC_TIME), NCFILE%iStride(NC_Y), NCFILE%iStride(NC_X)], &
        dpNC_Vars=dpTemp)

      iIndex = 0
      do iRow=iFromRow, iToRow, iByRow
        do iCol=iFromCol, iToCol, iByCol
          iIndex = iIndex + 1
          dpValues(iCol,iRow) = dpTemp(iIndex)
        enddo
      enddo

    case default

      call warn("INTERNAL PROGRAMMING ERROR: Unhandled select case. Program will probably fail.", __FILE__, __LINE__)

  end select

end subroutine nf_get_variable_slice_double

!----------------------------------------------------------------------

subroutine nf_get_variable_vector_short(NCFILE, iNC_VarID, iNC_Start, iNC_Count, &
   iNC_Stride, iNC_Vars)

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE
  integer (c_int) :: iNC_VarID
  integer (c_size_t) :: iNC_Start
  integer (c_size_t) :: iNC_Count
  integer (c_ptrdiff_t) :: iNC_Stride
  integer (c_short), dimension(:) :: iNC_Vars

  call nf_trap(nc_get_vars_short(ncid=NCFILE%iNCID, &
       varid=iNC_VarID, &
       startp=[iNC_Start], &
       countp=[iNC_Count], &
       stridep=[iNC_Stride], &
       vars=iNC_Vars), __FILE__, __LINE__ )

end subroutine nf_get_variable_vector_short

!----------------------------------------------------------------------

subroutine nf_get_variable_array_short(NCFILE, iNC_VarID, iNC_Start, iNC_Count, &
   iNC_Stride, iNC_Vars)

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE
  integer (c_int) :: iNC_VarID
  integer (c_size_t), dimension(:) :: iNC_Start
  integer (c_size_t), dimension(:) :: iNC_Count
  integer (c_size_t), dimension(:) :: iNC_Stride
  integer (c_short), dimension(:,:) :: iNC_Vars

  call nf_trap(nc_get_vars_short(ncid=NCFILE%iNCID, &
       varid=iNC_VarID, &
       startp=[iNC_Start], &
       countp=[iNC_Count], &
       stridep=[iNC_Stride], &

       vars=iNC_Vars), __FILE__, __LINE__ )

end subroutine nf_get_variable_array_short

!----------------------------------------------------------------------

subroutine nf_get_variable_array_as_vector_short(NCFILE, iNC_VarID, iNC_Start, iNC_Count, &
   iNC_Stride, iNC_Vars)

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE
  integer (c_int) :: iNC_VarID
  integer (c_size_t), dimension(:) :: iNC_Start
  integer (c_size_t), dimension(:) :: iNC_Count
  integer (c_ptrdiff_t), dimension(:) :: iNC_Stride
  integer (c_short), dimension(:) :: iNC_Vars

  ! [ LOCALS ]
  integer (c_int) :: iErrorCount
  integer (c_int) :: iResultCode

  iErrorCount = 0

  do

    iResultCode = nc_get_vars_short(ncid=NCFILE%iNCID, &
      varid=iNC_VarID, &
      startp=[iNC_Start], &
      countp=[iNC_Count], &
      stridep=[iNC_Stride], &
      vars=iNC_Vars)

    if ( iResultCode == NETCDF_IO_ERROR ) then

      if ( iErrorCount < NETCDF_IO_ERROR_RETRIES ) then
        iErrorCount = iErrorCount + 1
        call echolog( "** netCDF I/O error; possible network issues." )
        call echolog( "   ==> making another attempt to access netCDF data." )
        cycle
      endif

    endif

    call nf_trap(iResultCode, __FILE__, __LINE__ )

    exit

  enddo

end subroutine nf_get_variable_array_as_vector_short

!----------------------------------------------------------------------

subroutine nf_get_variable_array_as_vector_int(NCFILE, iNC_VarID, iNC_Start, iNC_Count, &
  iNC_Stride, iNC_Vars)

 type (T_NETCDF4_FILE), intent(inout) :: NCFILE
 integer (c_int) :: iNC_VarID
 integer (c_size_t), dimension(:) :: iNC_Start
 integer (c_size_t), dimension(:) :: iNC_Count
 integer (c_ptrdiff_t), dimension(:) :: iNC_Stride
 integer (c_int), dimension(:) :: iNC_Vars

 ! [ LOCALS ]
 integer (c_int) :: iErrorCount
 integer (c_int) :: iResultCode

 iErrorCount = 0

 do

   iResultCode = nc_get_vars_int(ncid=NCFILE%iNCID, &
     varid=iNC_VarID, &
     startp=[iNC_Start], &
     countp=[iNC_Count], &
     stridep=[iNC_Stride], &
     vars=iNC_Vars)

   if ( iResultCode == NETCDF_IO_ERROR ) then

     if ( iErrorCount < NETCDF_IO_ERROR_RETRIES ) then
       iErrorCount = iErrorCount + 1
       call echolog( "** netCDF I/O error; possible network issues." )
       call echolog( "   ==> making another attempt to access netCDF data." )
       cycle
     endif

   endif

   call nf_trap(iResultCode, __FILE__, __LINE__ )

   exit

 enddo

end subroutine nf_get_variable_array_as_vector_int

!----------------------------------------------------------------------

subroutine nf_get_variable_vector_int(NCFILE, iNC_VarID, iNC_Start, iNC_Count, &
   iNC_Stride, iNC_Vars)

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE
  integer (c_int) :: iNC_VarID
  integer (c_size_t) :: iNC_Start
  integer (c_size_t) :: iNC_Count
  integer (c_ptrdiff_t) :: iNC_Stride
  integer (c_int), dimension(:) :: iNC_Vars

  ! [ LOCALS ]
  integer (c_int) :: iErrorCount
  integer (c_int) :: iResultCode

  iErrorCount = 0

  do

    iResultCode = nc_get_vars_int( ncid=NCFILE%iNCID,   &
      varid=iNC_VarID,                                  &
      startp=[iNC_Start],                               &
      countp=[iNC_Count],                               &
      stridep=[iNC_Stride],                             &
      vars=iNC_Vars )

    if ( iResultCode == NETCDF_IO_ERROR ) then

      if ( iErrorCount < NETCDF_IO_ERROR_RETRIES ) then
        iErrorCount = iErrorCount + 1
        call echolog( "** netCDF I/O error; possible network issues." )
        call echolog( "   ==> making another attempt to access netCDF data." )
        cycle
      endif

    endif

    call nf_trap(iResultCode, __FILE__, __LINE__ )

    exit

  enddo

end subroutine nf_get_variable_vector_int

!----------------------------------------------------------------------

subroutine nf_get_variable_vector_double(NCFILE, iNC_VarID, iNC_Start, iNC_Count, &
   iNC_Stride, dpNC_Vars)

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE
  integer (c_int) :: iNC_VarID
  integer (c_size_t) :: iNC_Start
  integer (c_size_t) :: iNC_Count
  integer (c_size_t) :: iNC_Stride
  real (c_double), dimension(:) :: dpNC_Vars

  ! [ LOCALS ]
  integer (c_int) :: iErrorCount
  integer (c_int) :: iResultCode

  iErrorCount = 0

  do

    iResultCode = nc_get_vars_double( ncid=NCFILE%iNCID,   &
      varid=iNC_VarID,                                     &
      startp=[iNC_Start],                                  &
      countp=[iNC_Count],                                  &
      stridep=[iNC_Stride],                                &
      vars=dpNC_Vars )

    if ( iResultCode == NETCDF_IO_ERROR ) then

      if ( iErrorCount < NETCDF_IO_ERROR_RETRIES ) then
        iErrorCount = iErrorCount + 1
        call echolog( "** netCDF I/O error; possible network issues." )
        call echolog( "   ==> making another attempt to access netCDF data." )
        cycle
      endif

    endif

    call nf_trap(iResultCode, __FILE__, __LINE__ )

    exit

  enddo

end subroutine nf_get_variable_vector_double

!----------------------------------------------------------------------

subroutine nf_get_variable_array_double(NCFILE, iNC_VarID, iNC_Start, iNC_Count, &
   iNC_Stride, dpNC_Vars)

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE
  integer (c_int) :: iNC_VarID
  integer (c_size_t), dimension(:) :: iNC_Start
  integer (c_size_t), dimension(:) :: iNC_Count
  integer (c_size_t), dimension(:) :: iNC_Stride
  real (c_double), dimension(:,:) :: dpNC_Vars


  ! [ LOCALS ]
  integer (c_int) :: iErrorCount
  integer (c_int) :: iResultCode

  iErrorCount = 0

  do

    iResultCode = nc_get_vars_double( ncid=NCFILE%iNCID,    &
      varid=iNC_VarID,                                      &
      startp=[iNC_Start],                                   &
      countp=[iNC_Count],                                   &
      stridep=[iNC_Stride],                                 &
      vars=dpNC_Vars )

    if ( iResultCode == NETCDF_IO_ERROR ) then

      if ( iErrorCount < NETCDF_IO_ERROR_RETRIES ) then
        iErrorCount = iErrorCount + 1
        call echolog( "** netCDF I/O error; possible network issues." )
        call echolog( "   ==> making another attempt to access netCDF data." )
        cycle
      endif

    endif

    call nf_trap(iResultCode, __FILE__, __LINE__ )

    exit

  enddo

end subroutine nf_get_variable_array_double

!----------------------------------------------------------------------

subroutine nf_get_variable_array_as_vector_double(NCFILE, iNC_VarID, iNC_Start, iNC_Count, &
   iNC_Stride, dpNC_Vars)

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE
  integer (c_int) :: iNC_VarID
  integer (c_size_t), dimension(:) :: iNC_Start
  integer (c_size_t), dimension(:) :: iNC_Count
  integer (c_ptrdiff_t), dimension(:) :: iNC_Stride
  real (c_double), dimension(:) :: dpNC_Vars


  ! [ LOCALS ]
  integer (c_int) :: iErrorCount
  integer (c_int) :: iResultCode

  iErrorCount = 0

  do

    iResultCode = nc_get_vars_double( ncid=NCFILE%iNCID,    &
      varid=iNC_VarID,                                      &
      startp=[iNC_Start],                                   &
      countp=[iNC_Count],                                   &
      stridep=[iNC_Stride],                                 &
      vars=dpNC_Vars )

    if ( iResultCode == NETCDF_IO_ERROR ) then

      if ( iErrorCount < NETCDF_IO_ERROR_RETRIES ) then
        iErrorCount = iErrorCount + 1
        call echolog( "** netCDF I/O error; possible network issues." )
        call echolog( "   ==> making another attempt to access netCDF data." )
        cycle
      endif

    endif

    call nf_trap(iResultCode, __FILE__, __LINE__ )

    exit

  enddo

end subroutine nf_get_variable_array_as_vector_double

!----------------------------------------------------------------------

subroutine nf_get_variable_vector_float(NCFILE, iNC_VarID, iNC_Start, iNC_Count, &
   iNC_Stride, rNC_Vars )

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE
  integer (c_int) :: iNC_VarID
  integer (c_size_t) :: iNC_Start
  integer (c_size_t) :: iNC_Count
  integer (c_ptrdiff_t) :: iNC_Stride
  real (c_float), dimension(:) :: rNC_Vars

  ! [ LOCALS ]
  integer (c_int) :: iErrorCount
  integer (c_int) :: iResultCode

  iErrorCount = 0

  do

    iResultCode = nc_get_vars_float( ncid=NCFILE%iNCID,       &
      varid=iNC_VarID,                                        &
      startp=[iNC_Start],                                     &
      countp=[iNC_Count],                                     &
      stridep=[iNC_Stride],                                   &
      vars=rNC_Vars )

    if ( iResultCode == NETCDF_IO_ERROR ) then

      if ( iErrorCount < NETCDF_IO_ERROR_RETRIES ) then
        iErrorCount = iErrorCount + 1
        call echolog( "** netCDF I/O error; possible network issues." )
        call echolog( "   ==> making another attempt to access netCDF data." )
        cycle
      endif

    endif

    call nf_trap(iResultCode, __FILE__, __LINE__ )

    exit

  enddo

end subroutine nf_get_variable_vector_float

!----------------------------------------------------------------------

subroutine nf_get_variable_array_float(NCFILE, iNC_VarID, iNC_Start, iNC_Count, &
   iNC_Stride, rNC_Vars )

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE
  integer (c_int) :: iNC_VarID
  integer (c_size_t), dimension(:) :: iNC_Start
  integer (c_size_t), dimension(:) :: iNC_Count
  integer (c_ptrdiff_t), dimension(:) :: iNC_Stride
  real (c_float), dimension(:,:) :: rNC_Vars


  ! [ LOCALS ]
  integer (c_int) :: iErrorCount
  integer (c_int) :: iResultCode

  iErrorCount = 0

  do

    iResultCode = nc_get_vars_float( ncid=NCFILE%iNCID,     &
      varid=iNC_VarID,                                      &
      startp=[iNC_Start],                                   &
      countp=[iNC_Count],                                   &
      stridep=[iNC_Stride],                                 &
      vars=rNC_Vars )

    if ( iResultCode == NETCDF_IO_ERROR ) then

      if ( iErrorCount < NETCDF_IO_ERROR_RETRIES ) then
        iErrorCount = iErrorCount + 1
        call echolog( "** netCDF I/O error; possible network issues." )
        call echolog( "   ==> making another attempt to access netCDF data." )
        cycle
      endif

    endif

    call nf_trap(iResultCode, __FILE__, __LINE__ )

    exit

  enddo

end subroutine nf_get_variable_array_float

!----------------------------------------------------------------------

subroutine nf_get_variable_array_as_vector_float(NCFILE, iNC_VarID, iNC_Start, iNC_Count, &
   iNC_Stride, rNC_Vars )

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE
  integer (c_int) :: iNC_VarID
  integer (c_size_t), dimension(:) :: iNC_Start
  integer (c_size_t), dimension(:) :: iNC_Count
  integer (c_ptrdiff_t), dimension(:) :: iNC_Stride
  real (c_float), dimension(:) :: rNC_Vars


  ! [ LOCALS ]
  integer (c_int)              :: iErrorCount
  integer (c_int)              :: iResultCode
  integer (c_int), parameter   :: MAX_RETRIES = 12
  real (c_float)               :: pause_length
  real (c_float)               :: random_num

  iErrorCount = 0

  do

    iResultCode = nc_get_vars_float( ncid=NCFILE%iNCID,      &
      varid=iNC_VarID,                                       &
      startp=[iNC_Start],                                    &
      countp=[iNC_Count],                                    &
      stridep=[iNC_Stride],                                  &
      vars=rNC_Vars )

    if ( iResultCode == NETCDF_IO_ERROR                                 &
         .or. iResultCode == NETCDF_HDF_LAYER_ERROR                     &
         .or. iResultCode == NETCDF_CANNOT_READ_ERROR ) then

      if ( iErrorCount < NETCDF_IO_ERROR_RETRIES ) then
        iErrorCount = iErrorCount + 1

        call set_random_number_generator_seed()
        call random_number( random_num )

        pause_length = iErrorCount * 6.0 * random_num
        if ( iErrorCount > MAX_RETRIES ) exit
        call echolog( "** netCDF I/O error; possible network issues or conflicting applications." )
        call echolog("    filename: "//squote( NCFILE%sFilename ) )
        call echolog( "   ==> making another attempt to access netCDF data." )
        call echolog( "       PAUSING for "//trim( asCharacter(pause_length) )//" seconds before attempting to re-read the data." )
        call sleep_for_x_seconds( pause_length )
        cycle
      endif

    endif

    call nf_trap(iResultCode, __FILE__, __LINE__ )

    exit

  enddo

end subroutine nf_get_variable_array_as_vector_float

!----------------------------------------------------------------------

subroutine netcdf_dump_cdl(NCFILE, iLU)

  type (T_NETCDF4_FILE ) :: NCFILE
  type (T_NETCDF_ATTRIBUTE), pointer :: pNC_ATT
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  type (T_NETCDF_DIMENSION), pointer :: pNC_DIM
  integer :: iLU
  character (len=256) :: sBuf, sBuf2
  character (len=256) :: sDimName
  integer (c_int) :: iDimID
  integer (c_int) :: iUbound

  integer :: iResult, iIndex, iIndex2, iIndex3, iIndex4

  sBuf=""; sBuf2=""

  write(unit=iLU, fmt="(a)") "netcdf "//trim(NCFILE%sFilename)//" {"
  write(unit=iLU, fmt="(a)") "  dimensions:"

  do iIndex = 0, NCFILE%iNumberOfDimensions - 1
    write(unit=iLU, fmt="(4x,a, ' = ', i0, ';')") trim(NCFILE%pNC_DIM(iIndex)%sDimensionName), &
      NCFILE%pNC_DIM(iIndex)%iNC_DimSize
  enddo

  do iIndex = 0, NCFILE%iNumberOfVariables - 1

    pNC_VAR => NCFILE%pNC_VAR(iIndex)

    if(pNC_VAR%iNumberOfDimensions > 0) then

      sBuf = ' ('

      iUbound = pNC_VAR%iNumberOfDimensions - 1
      do iIndex3 = 0, iUbound

        iDimID = pNC_VAR%iNC_DimID(iIndex3)

        call assert(iDimID >=0 .and. &
          iDimID <= ubound( NCFILE%pNC_DIM, 1 ), &
          "INTERNAL PROGRAMMING ERROR -- iDimID out of bounds", &
          trim(__FILE__), __LINE__)

        pNC_DIM => NCFILE%pNC_DIM(iDimID)
        sDimName = pNC_DIM%sDimensionName

        write(sBuf2, fmt="(i12)") pNC_DIM%iNC_DimSize
        sBuf = trim(sBuf)//trim(pNC_DIM%sDimensionName)//"=" &
           //trim(adjustl(sBuf2))

        if (iIndex3 /= iUbound) sBuf = trim(sBuf)//", "

      enddo

      sBuf = trim(sBuf)//')'

    else

      sBuf = ""

    endif

    sBuf = trim(NETCDF_DATA_TYPE(pNC_VAR%iNC_VarType)) &
       //" "//trim(pNC_VAR%sVariableName)//sBuf//";"

    write(unit=iLU, fmt="(2x,a)") trim(sBuf)

    iUbound = pNC_VAR%iNumberOfAttributes - 1
    do iIndex3 = 0, iUbound

      pNC_ATT => NCFILE%pNC_VAR(iIndex)%pNC_ATT(iIndex3)

      sBuf = trim(pNC_VAR%sVariableName)//":"//trim(pNC_ATT%sAttributeName )//" ="

      do iIndex4=0, ubound(pNC_ATT%sAttValue, 1)

          sBuf = trim(sBuf)//" "//trim(pNC_ATT%sAttValue(iIndex4))

      enddo

      sBuf=trim(sBuf)//"; // "//trim(NETCDF_DATA_TYPE(pNC_ATT%iNC_AttType) )

      write(unit=iLU, fmt="(4x,a)") trim(sBuf)

    enddo

  enddo

  do iIndex = 0, NCFILE%iNumberOfAttributes - 1

    pNC_ATT => NCFILE%pNC_ATT(iIndex)

    sBuf = ":"//trim(pNC_ATT%sAttributeName )//" ="

    do iIndex4=0, ubound(pNC_ATT%sAttValue, 1)

        sBuf = trim(sBuf)//" "//trim(pNC_ATT%sAttValue(iIndex4))

    enddo

    sBuf=trim(sBuf)//"; // "//trim(NETCDF_DATA_TYPE(pNC_ATT%iNC_AttType) )

    write(unit=iLU, fmt="(a)") trim(sBuf)

  enddo

  write(unit=iLU, fmt="(a,/,/)") "}"


end subroutine netcdf_dump_cdl

!----------------------------------------------------------------------

function nf_get_first_and_last(NCFILE, iVarIndex)  result(dpValues)

  type (T_NETCDF4_FILE ) :: NCFILE
  integer (c_int) :: iVarIndex
  real (c_double), dimension(0:1) :: dpValues

  ! [ LOCALS ]
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  type (T_NETCDF_DIMENSION), pointer :: pNC_DIM
  integer (c_int) :: iDimSize
  integer (c_int) :: iDimIndex
  integer (c_size_t) :: iStride
  integer (c_size_t) :: iCount
  integer (c_short), dimension(0:1) :: spValues
  integer (c_int), dimension(0:1) :: ipValues
  real (c_float), dimension(0:1) :: rpValues

  call assert (iVarIndex >= lbound(NCFILE%pNC_VAR,1) &
    .and. iVarIndex <= ubound(NCFILE%pNC_VAR,1), &
      "INTERNAL PROGRAMMING ERROR - index out of bounds NC_FILE%pNC_VAR" &
      //"~Offending index value: "//trim(asCharacter(iVarIndex)), &
      trim(__FILE__), __LINE__)

  pNC_VAR => NCFILE%pNC_VAR( iVarIndex )
  iDimSize = nf_return_DimSize(NCFILE, pNC_VAR%iNC_DimID(0) )

  if (iDimSize > 1) then
    iCount = 2_c_size_t
    iStride = int(iDimSize, c_size_t) - 1_c_size_t
  else
    iCount = 1_c_size_t
    iStride = 1_c_size_t
  endif

  select case ( pNC_VAR%iNC_VarType )

    case (NC_SHORT)

      call nf_get_variable_vector_short(NCFILE=NCFILE, &
        iNC_VarID=pNC_VAR%iNC_VarID, &
        iNC_Start=0_c_size_t, &
        iNC_Count=iCount, &
        iNC_Stride=iStride, &
        iNC_Vars=spValues)

      dpValues = real(spValues, c_double)

    case (NC_INT)

      call nf_get_variable_vector_int(NCFILE=NCFILE, &
        iNC_VarID=pNC_VAR%iNC_VarID, &
        iNC_Start=0_c_size_t, &
        iNC_Count=iCount, &
        iNC_Stride=iStride, &
        iNC_Vars=ipValues)

      dpValues = real(ipValues, c_double)

    case (NC_FLOAT)

      call nf_get_variable_vector_float(NCFILE=NCFILE, &
        iNC_VarID=pNC_VAR%iNC_VarID, &
        iNC_Start=0_c_size_t, &
        iNC_Count=iCount, &
        iNC_Stride=iStride, &
        rNC_Vars=rpValues)

      dpValues = real(rpValues, c_double)

    case (NC_DOUBLE)

      call nf_get_variable_vector_double(NCFILE=NCFILE, &
        iNC_VarID=pNC_VAR%iNC_VarID, &
        iNC_Start=0_c_size_t, &
        iNC_Count=iCount, &
        iNC_Stride=iStride, &
        dpNC_Vars=dpValues)

    case default

  end select

!print *, __FILE__, ": ", __LINE__, dpValues
  dpValues = dpValues * pNC_VAR%rScaleFactor + pNC_VAR%rAddOffset
!  print *, pNC_VAR%rScaleFactor, pNC_VAR%rAddOffset, pNC_VAR%sVariableName
!print *, "MODIFIED: ", dpValues

  !> if there is only one day of data in this NetCDF file, the
  !> first day equals the last day
  if (iCount == 1) dpValues(NC_LAST) = dpValues(NC_FIRST)

end function nf_get_first_and_last

!----------------------------------------------------------------------

subroutine nf_calculate_time_range(NCFILE)

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE

  ! [ LOCALS ]
  integer (c_int) :: iMonth, iDay, iYear

  NCFILE%iOriginJD = julian_day(NCFILE%iOriginYear, &
    NCFILE%iOriginMonth, NCFILE%iOriginDay)

  NCFILE%iFirstDayJD = NCFILE%iOriginJD + NCFILE%dpFirstAndLastTimeValues(NC_FIRST)
  NCFILE%iLastDayJD = NCFILE%iOriginJD + NCFILE%dpFirstAndLastTimeValues(NC_LAST)

end subroutine nf_calculate_time_range

!----------------------------------------------------------------------

subroutine nf_get_time_vals(NCFILE)

  type (T_NETCDF4_FILE ) :: NCFILE

  integer (c_int) :: iVarIndex_time
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR_time
  type (T_NETCDF_DIMENSION), pointer :: pNC_DIM_time
  integer (c_int) :: iLowerBound, iUpperBound
  integer (c_int) :: iStat

  iStat = 0

  iVarIndex_time = NCFILE%iVarIndex( NC_TIME )

  call assert( iVarIndex_time >= lbound( NCFILE%pNC_VAR, 1 )                             &
    .and. iVarIndex_time <= ubound( NCFILE%pNC_VAR, 1 ),                                 &
    "INTERNAL PROGRAMMING ERROR - Index out of bounds", trim(__FILE__), __LINE__)

  pNC_VAR_time => NCFILE%pNC_VAR( iVarIndex_time )
  pNC_DIM_time => NCFILE%pNC_DIM( pNC_VAR_time%iNC_DimID( 0 ) )

  if ( allocated( NCFILE%rDateTimeValues ) )   deallocate( NCFILE%rDateTimeValues, stat=iStat )
  call assert( iStat==0, "Failed to deallocate memory for time values", trim(__FILE__), __LINE__)

  allocate( NCFILE%rDateTimeValues(0 : pNC_DIM_time%iNC_DimSize - 1 ), stat=iStat )
    call assert(iStat==0, "Failed to allocate memory for time values", &
    trim(__FILE__), __LINE__)

  !> @todo allow time to be read in as float, short, or int as well

  call nf_get_variable_vector_double(NCFILE=NCFILE, &
       iNC_VarID=pNC_VAR_time%iNC_VarID, &
       iNC_Start=0_c_size_t, &
       iNC_Count=pNC_DIM_time%iNC_DimSize, &
       iNC_Stride=1_c_size_t, &
       dpNC_Vars=NCFILE%rDateTimeValues)

  NCFILE%rDateTimeValues = NCFILE%rDateTimeValues * pNC_VAR_time%rScaleFactor + pNC_VAR_time%rAddOffset

end subroutine nf_get_time_vals

!----------------------------------------------------------------------

subroutine nf_get_time_units(NCFILE)

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE

  ! [ LOCALS ]
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  character (len=256)               :: sDateTime
  character (len=256)               :: sItem
  integer (c_int)              :: iIndex
  logical (c_bool)             :: lFound
  integer (c_int)              :: iStat

  call assert(NCFILE%iVarID(NC_TIME) >= 0, "INTERNAL PROGRAMMING ERROR -- " &
    //"nf_get_time_units must be called only after a call is made to ~" &
    //"netcdf_get_variable_ids", trim(__FILE__), __LINE__)

  pNC_VAR => NCFILE%pNC_VAR( NCFILE%iVarID(NC_TIME) )

  lFound = lFALSE

  do iIndex=0, pNC_VAR%iNumberOfAttributes - 1

    if ( str_compare(pNC_VAR%pNC_ATT(iIndex)%sAttributeName, "units") ) then
      lFound = lTRUE
      exit
    endif

  enddo

  call assert (lFound, "Failed to find the 'units' attribute associated " &
    //"with time variable "//dquote(pNC_VAR%sVariableName), &
    trim(__FILE__), __LINE__)

  sDateTime = pNC_VAR%pNC_ATT(iIndex)%sAttValue(0)

  call chomp(sDateTime, sItem)    !> should be "days"

  select case ( lowercase_fn(sItem) )

    case( "seconds")

      pNC_VAR%rScaleFactor = 1.0_c_double / 86400.0_c_double
      call echolog( "| *** NON-STANDARD TIME UNITS DETECTED: seconds ***")
      call echolog( "|   ==> time units will be divided by 86400 to yield values of days.")

    case( "minutes")

      pNC_VAR%rScaleFactor = 1.0_c_double / 1440.0_c_double
      call echolog( "| *** NON-STANDARD TIME UNITS DETECTED: minutes ***")
      call echolog( "|   ==> time units will be divided by 1440 to yield values of days.")

    case( "hours")

      pNC_VAR%rScaleFactor = 1.0_c_double / 24.0_c_double
      call echolog( "| *** NON-STANDARD TIME UNITS DETECTED: hours ***")
      call echolog( "|   ==> time units will be divided by 24 to yield values of days.")

    case default

      pNC_VAR%rScaleFactor = 1.0_c_double

  end select

  call chomp(sDateTime, sItem)    !> should be "since"

  call chomp(sDateTime, sItem, "/-")
  read(sItem, *, iostat=iStat) NCFILE%iOriginYear
  call assert(iStat == 0, "Problem parsing year value in NetCDF file", __FILE__, __LINE__)

  call chomp(sDateTime, sItem, "/-")
  read(sItem, *, iostat=iStat) NCFILE%iOriginMonth
  call assert(iStat == 0, "Problem parsing month value in NetCDF file", __FILE__, __LINE__)

  call chomp(sDateTime, sItem )
  read(sItem, *, iostat=iStat) NCFILE%iOriginDay
  call assert(iStat == 0, "Problem parsing day value in NetCDF file", __FILE__, __LINE__)

  ! if no time value has been given, assign values of zero to HH:MM:SS
  if (len_trim(sDateTime) == 0 ) then

    NCFILE%iOriginHH = 0
    NCFILE%iOriginMM = 0
    NCFILE%iOriginSS = 0

  else

    call chomp(sDateTime, sItem, ":")
    read(sItem, *, iostat=iStat) NCFILE%iOriginHH

    if (iStat /= 0) then
      NCFILE%iOriginHH = 0
      NCFILE%iOriginMM = 0
      NCFILE%iOriginSS = 0
    else

      call chomp(sDateTime, sItem, ":")
      read(sItem, *, iostat=iStat) NCFILE%iOriginMM
      if (iStat /= 0)  NCFILE%iOriginMM = 0

      read(sDateTime, *, iostat=iStat) NCFILE%iOriginSS
      if (iStat /= 0)  NCFILE%iOriginSS = 0

    endif

  endif

  !print *, __FILE__, ": ", __LINE__, "   |  ", pNC_VAR%rScaleFactor, pNC_VAR%rAddOffset, pNC_VAR%sVariableName

end subroutine nf_get_time_units

!----------------------------------------------------------------------

subroutine nf_get_xyz_units(NCFILE)

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE

  ! [ LOCALS ]
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  integer (c_int) :: iIndex, iIndex2
  logical (c_bool) :: lFound
  integer (c_int) :: iStat

  do iIndex = NC_Y, NC_Z

    call assert(NCFILE%iVarID(iIndex) >= 0, "INTERNAL PROGRAMMING ERROR -- " &
    //"nc_get_XYZ_units must be called only after a call is made to ~" &
    //"netcdf_get_variable_ids", trim(__FILE__), __LINE__)

    pNC_VAR => NCFILE%pNC_VAR(NCFILE%iVarID(iIndex) )

    lFound = lFALSE

    do iIndex2=0, pNC_VAR%iNumberOfAttributes - 1

      if ( str_compare(pNC_VAR%pNC_ATT(iIndex2)%sAttributeName, "units") ) then
        lFound = lTRUE
        exit
      endif

    enddo

    if (lFound) then
      NCFILE%sVarUnits(iIndex) = trim(pNC_VAR%pNC_ATT(iIndex2)%sAttValue(0))
    endif

  enddo

end subroutine nf_get_xyz_units

!----------------------------------------------------------------------

subroutine nf_get_scale_and_offset(NCFILE)

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE

  ! [ LOCALS ]
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  integer (c_int) :: iIndex
  logical (c_bool) :: lFound
  integer (c_int) :: iStat
  character (len=32) :: sBuf

  pNC_VAR => NCFILE%pNC_VAR( NCFILE%iVarID( NC_Z ) )

  lFound = lFALSE

  do iIndex=0, pNC_VAR%iNumberOfAttributes - 1

    if ( str_compare(pNC_VAR%pNC_ATT(iIndex)%sAttributeName, "scale_factor") ) then
      lFound = lTRUE
      exit
    endif

  enddo

  if (lFound) then
    if (allocated( pNC_VAR%pNC_ATT(iIndex)%i2AttValue )) then
      NCFILE%rScaleFactor(NC_Z) = asReal( pNC_VAR%pNC_ATT(iIndex)%i2AttValue(0) )
    elseif (allocated( pNC_VAR%pNC_ATT(iIndex)%iAttValue )) then
      NCFILE%rScaleFactor(NC_Z) = asReal( pNC_VAR%pNC_ATT(iIndex)%iAttValue(0) )
    elseif (allocated( pNC_VAR%pNC_ATT(iIndex)%rAttValue )) then
      NCFILE%rScaleFactor(NC_Z) = pNC_VAR%pNC_ATT(iIndex)%rAttValue(0)
    elseif (allocated( pNC_VAR%pNC_ATT(iIndex)%dpAttValue )) then
      NCFILE%rScaleFactor(NC_Z) = asReal( pNC_VAR%pNC_ATT(iIndex)%dpAttValue(0) )
    elseif (len_trim(pNC_VAR%pNC_ATT(iIndex)%sAttValue(0)) > 0) then
      sBuf = trim(pNC_VAR%pNC_ATT(iIndex)%sAttValue(0) )
      read(sBuf,*) NCFILE%rScaleFactor(NC_Z)
    else
      call assert(lFALSE,"Error reading the 'scale_factor' attribute from the netCDF file"   &
             //dQuote(NCFILE%sFilename))
    endif
  endif  
          
  !> Now repeat the process for "add_offset" attribute
  lFound = lFALSE
          
  do iIndex=0, pNC_VAR%iNumberOfAttributes - 1
            
    if ( str_compare(pNC_VAR%pNC_ATT(iIndex)%sAttributeName, "add_offset") ) then
      lFound = lTRUE
      exit
    endif
            
  enddo
          
  if (lFound) then
    if (allocated( pNC_VAR%pNC_ATT(iIndex)%i2AttValue )) then
      NCFILE%rAddOffset(NC_Z) = asReal( pNC_VAR%pNC_ATT(iIndex)%i2AttValue(0) )
    elseif (allocated( pNC_VAR%pNC_ATT(iIndex)%iAttValue )) then
      NCFILE%rAddOffset(NC_Z) = asReal( pNC_VAR%pNC_ATT(iIndex)%iAttValue(0) )
    elseif (allocated( pNC_VAR%pNC_ATT(iIndex)%rAttValue )) then
      NCFILE%rAddOffset(NC_Z) = pNC_VAR%pNC_ATT(iIndex)%rAttValue(0)
    elseif (allocated( pNC_VAR%pNC_ATT(iIndex)%dpAttValue )) then
      NCFILE%rAddOffset(NC_Z) = asReal( pNC_VAR%pNC_ATT(iIndex)%dpAttValue(0) )
    elseif (len_trim(pNC_VAR%pNC_ATT(iIndex)%sAttValue(0)) > 0) then
      sBuf = trim(pNC_VAR%pNC_ATT(iIndex)%sAttValue(0) )
      read(sBuf,*) NCFILE%rAddOffset(NC_Z)
    else
      call assert(lFALSE,"Error reading the 'add_offset' attribute from the netCDF file"    &
        //dQuote(NCFILE%sFilename))
    endif    
  endif
          
 end subroutine nf_get_scale_and_offset
        
 !----------------------------------------------------------------------


subroutine nf_get_variable_id_and_type( NCFILE )
  type (T_NETCDF4_FILE), intent(inout) :: NCFILE

   ! [ LOCALS ]
   integer (c_int) :: iIndex
   type (T_NETCDF_VARIABLE), pointer :: pNC_VAR

   NCFILE%iVarID = -9999

   do iIndex=0, NCFILE%iNumberOfVariables - 1

     pNC_VAR => NCFILE%pNC_VAR(iIndex)

     if (str_compare(pNC_VAR%sVariableName, NCFILE%sVarName(NC_X) ) ) then
       NCFILE%iVarIndex(NC_X) = iIndex
       NCFILE%iVarID(NC_X) = pNC_VAR%iNC_VarID
       NCFILE%iVarType(NC_X) = pNC_VAR%iNC_VarType
       NCFILE%iVar_DimID(NC_X,:) = pNC_VAR%iNC_DimID

     elseif (str_compare(pNC_VAR%sVariableName,  NCFILE%sVarName(NC_Y) ) ) then
       NCFILE%iVarIndex(NC_Y) = iIndex
       NCFILE%iVarID(NC_Y) = pNC_VAR%iNC_VarID
       NCFILE%iVarType(NC_Y) = pNC_VAR%iNC_VarType
       NCFILE%iVar_DimID(NC_Y,:) = pNC_VAR%iNC_DimID

     elseif (str_compare(pNC_VAR%sVariableName,  NCFILE%sVarName(NC_Z) ) ) then
       NCFILE%iVarIndex(NC_Z) = iIndex
       NCFILE%iVarID(NC_Z) = pNC_VAR%iNC_VarID
       NCFILE%iVarType(NC_Z) = pNC_VAR%iNC_VarType
       NCFILE%iVar_DimID(NC_Z,:) = pNC_VAR%iNC_DimID

     elseif (str_compare(pNC_VAR%sVariableName,  NCFILE%sVarName(NC_TIME) ) ) then
       NCFILE%iVarIndex(NC_TIME) = iIndex
       NCFILE%iVarID(NC_TIME) = pNC_VAR%iNC_VarID
       NCFILE%iVarType(NC_TIME) = pNC_VAR%iNC_VarType
       NCFILE%iVar_DimID(NC_TIME,:) = pNC_VAR%iNC_DimID
     endif

   enddo

   call assert(NCFILE%iVarID(NC_X) >= 0, &
     "Unable to find the variable named "//dquote(NCFILE%sVarName(NC_X) )//" in " &
     //"file "//dquote(NCFILE%sFilename), trim(__FILE__), __LINE__)

   call assert(NCFILE%iVarID(NC_Y) >= 0, &
     "Unable to find the variable named "//dquote(NCFILE%sVarName(NC_Y))//" in " &
     //"file "//dquote(NCFILE%sFilename), trim(__FILE__), __LINE__)

   call assert(NCFILE%iVarID(NC_Z) >= 0, &
     "Unable to find the variable named "//dquote(NCFILE%sVarName(NC_Z))//" in " &
     //"file "//dquote(NCFILE%sFilename), trim(__FILE__), __LINE__)

   call assert(NCFILE%iVarID(NC_TIME) >= 0, &
     "Unable to find the variable named "//dquote(NCFILE%sVarName(NC_TIME))//" in " &
     //"file "//dquote(NCFILE%sFilename), trim(__FILE__), __LINE__)

end subroutine nf_get_variable_id_and_type

!----------------------------------------------------------------------

function nf_return_index_double(rValues, rTargetValue)  result(iIndex)

  real (c_double), dimension(:) :: rValues
  real (c_double) :: rTargetValue
  integer (c_int) :: iIndex

  ! [ LOCALS ]
  integer (c_int) :: iCount
  real (c_double) :: rDiff, rDiffMin

  if ( .not. (rTargetValue >= minval(rValues) .and. rTargetValue <= maxval(rValues)) ) then
    call echolog( "~SWB grid coordinate value (" &
    //trim(asCharacter(rTargetValue))//") is not within the range of coordinate values covered ~" &
    //"by the NetCDF file. Range of NetCDF file coordinates: "  &
    //trim(asCharacter(minval(rValues)))//" to "//trim(asCharacter(maxval(rValues))) )

    call assert(lFALSE, "USER ERROR: NetCDF file coverage is not large enough to cover your area of interest.~" &
      //"Try creating a NetCDF file whose boundaries extend several cells beyond your area of ~interest in all directions.", &
      trim(__FILE__), __LINE__)
  endif

  rDiffMin = 1.e+20

  do iCount=lbound(rValues,1), ubound(rValues,1)

    rDiff = abs(rValues(iCount) - rTargetValue)

    if ( rDiff < rDiffMin ) then
      iIndex = iCount
      rDiffMin =rDiff
    endif

  enddo

end function nf_return_index_double

!----------------------------------------------------------------------

function nf_coord_to_col_row(NCFILE, rX, rY)  result(iColRow)

  type (T_NETCDF4_FILE ) :: NCFILE
  real (c_double) :: rX
  real (c_double) :: rY
  integer (c_size_t), dimension(2) :: iColRow


  ! [ LOCALS ]
  integer (c_int) :: iColNum, iRowNum

  iColNum = nf_return_index_double(NCFILE%rX_Coords, rX)
  iRowNum = nf_return_index_double(NCFILE%rY_Coords, rY)

  iColRow(COLUMN) = iColNum
  iColRow(ROW) = iRowNum

end function nf_coord_to_col_row

!----------------------------------------------------------------------

function nf_get_varid(NCFILE, sVariableName)  result(iNC_VarID)

  type (T_NETCDF4_FILE ) :: NCFILE
  character (len=*) :: sVariableName
  integer (c_int) :: iNC_VarID

  integer (c_int) :: iIndex
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR

  iNC_VarID = -9999

  do iIndex=0, NCFILE%iNumberOfVariables - 1

    pNC_VAR => NCFILE%pNC_VAR(iIndex)

    if(trim(sVariableName) .eq. trim(pNC_VAR%sVariableName) ) then

      iNC_VarID = iIndex
      exit

    endif

  enddo

end function nf_get_varid

!----------------------------------------------------------------------

subroutine nf_create(NCFILE, sFilename, iLU)

  type (T_NETCDF4_FILE ) :: NCFILE
  character (len=*) :: sFilename
  integer (c_int), optional :: iLU

  character (len=256) :: sBuf
  call getcwd(sBuf)

  call nf_trap(nc_create(path=trim(fortran_to_c_string(sFilename)), &
                 cmode=NC_NETCDF4, &
                 ncidp=NCFILE%iNCID), &
                 __FILE__, __LINE__)

  NCFILE%sFilename = trim(sFilename)
  NCFILE%iFileFormat = NC_FORMAT_NETCDF4

  if (present(iLU) ) then
    call echolog( "Created NetCDF file for output. Filename: " &
      //dquote(NCFILE%sFilename)//"; NCID="//trim(asCharacter(NCFILE%iNCID) ) )
  endif

end subroutine nf_create

!----------------------------------------------------------------------

subroutine nf_define_deflate(NCFILE, iVarID, iShuffle, iDeflate, iDeflate_level)

  type (T_NETCDF4_FILE ) :: NCFILE
  integer (c_int) :: iVarID
  integer (c_int) :: iShuffle
  integer (c_int) :: iDeflate
  integer (c_int) :: iDeflate_level

  call nf_trap(nc_def_var_deflate(ncid=NCFILE%iNCID, &
          varid=iVarID, &
          shuffle=iShuffle, &
          deflate=iDeflate, &
          deflate_level=iDeflate_level), &
          __FILE__, __LINE__)

end subroutine nf_define_deflate

!----------------------------------------------------------------------

subroutine nf_enddef(NCFILE)

  type (T_NETCDF4_FILE ) :: NCFILE

  call nf_trap(nc_enddef(ncid=NCFILE%iNCID), &
       __FILE__, __LINE__)

end subroutine nf_enddef

!----------------------------------------------------------------------

function nf_define_dimension(NCFILE, sDimensionName, iDimensionSize) &
      result(iDimID)

  type (T_NETCDF4_FILE ) :: NCFILE
  character (len=*) :: sDimensionName
  integer (c_int) :: iDimensionSize
  integer (c_int) :: iDimID

  integer (c_size_t) :: iDimSize

  iDimSize = int(iDimensionSize, c_size_t)

  call nf_trap(nc_def_dim(ncid=NCFILE%iNCID, &
                          name=trim(sDimensionName)//c_null_char, &
                          lenv=iDimSize, &
                          dimidp=iDimID), &
                          __FILE__, __LINE__)

end function nf_define_dimension

!----------------------------------------------------------------------

subroutine nf_define_dimensions( NCFILE )

  type (T_NETCDF4_FILE) :: NCFILE

  ! [ LOCALS ]
  integer (c_int) :: iStat
  integer (c_int) :: iIndex
  character (len=256) :: sDimName
  type (T_NETCDF_DIMENSION), pointer :: pNC_DIM

  do iIndex = 0, NCFILE%iNumberOfDimensions-1

    pNC_DIM => NCFILE%pNC_DIM(iIndex)

    call nf_trap(nc_def_dim(ncid=NCFILE%iNCID, &
      name=trim(pNC_DIM%sDimensionName)//c_null_char, &
      lenv=pNC_DIM%iNC_DimSize, &
      dimidp=pNC_DIM%iNC_DimID), &
      __FILE__, __LINE__ )

  enddo

end subroutine nf_define_dimensions

!----------------------------------------------------------------------

subroutine nf_set_standard_dimensions(NCFILE, iNX, iNY)

  type (T_NETCDF4_FILE ) :: NCFILE
  integer (c_int) :: iNX
  integer (c_int) :: iNY

  ! [ LOCALS ]
  integer (c_int) :: iStat

  iStat = 0

  NCFILE%iNumberOfDimensions = 3

  if (associated(NCFILE%pNC_DIM) ) deallocate(NCFILE%pNC_DIM, stat=iStat)
  call assert(iStat == 0, "Could not deallocate memory for NC_DIM member in NC_FILE defined type", &
    trim(__FILE__), __LINE__)

  allocate(NCFILE%pNC_DIM( 0 : NCFILE%iNumberOfDimensions-1), stat=iStat )
  call assert(iStat == 0, "Could not allocate memory for NC_DIM member in NC_FILE defined type", &
    trim(__FILE__), __LINE__)

  !> define the time dimension;
  NCFILE%pNC_DIM(NC_TIME)%sDimensionName = "time"
  NCFILE%pNC_DIM(NC_TIME)%iNC_DimSize = NC_UNLIMITED

  !> define the y dimension;
  NCFILE%pNC_DIM(NC_Y)%sDimensionName = "y"
  NCFILE%pNC_DIM(NC_Y)%iNC_DimSize = iNY

  !> define the x dimension;
  NCFILE%pNC_DIM(NC_X)%sDimensionName = "x"
  NCFILE%pNC_DIM(NC_X)%iNC_DimSize = iNX

end subroutine nf_set_standard_dimensions

!----------------------------------------------------------------------

subroutine nf_set_standard_variables(NCFILE, sVarName_z)

  type (T_NETCDF4_FILE ) :: NCFILE
  character (len=*) :: sVarName_z

  ! [ LOCALS ]
  integer (c_int) :: iStat

  iStat = 0

  NCFILE%iNumberOfVariables = 4

  if (associated(NCFILE%pNC_VAR) ) deallocate(NCFILE%pNC_VAR, stat=iStat)
  call assert(iStat == 0, "Could not deallocate memory for NC_VAR member in NC_FILE defined type", &
    trim(__FILE__), __LINE__)

  allocate(NCFILE%pNC_VAR( 0 : NCFILE%iNumberOfVariables-1), stat=iStat )
  call assert(iStat == 0, "Could not allocate memory for NC_VAR member in NC_FILE defined type", &
    trim(__FILE__), __LINE__)

  NCFILE%pNC_VAR(NC_TIME)%sVariableName = "time"
  NCFILE%pNC_VAR(NC_TIME)%iNC_VarType = NC_DOUBLE
  NCFILE%pNC_VAR(NC_TIME)%iNumberOfDimensions = 1
  NCFILE%pNC_VAR(NC_TIME)%iNC_DimID(0) = NCFILE%pNC_DIM(NC_TIME)%iNC_DimID

  NCFILE%pNC_VAR(NC_Y)%sVariableName = "y"
  NCFILE%pNC_VAR(NC_Y)%iNC_VarType = NC_DOUBLE
  NCFILE%pNC_VAR(NC_Y)% iNumberOfDimensions = 1
  NCFILE%pNC_VAR(NC_Y)%iNC_DimID = NCFILE%pNC_DIM(NC_Y)%iNC_DimID

  NCFILE%pNC_VAR(NC_X)%sVariableName = "x"
  NCFILE%pNC_VAR(NC_X)%iNC_VarType = NC_DOUBLE
  NCFILE%pNC_VAR(NC_X)% iNumberOfDimensions = 1
  NCFILE%pNC_VAR(NC_X)%iNC_DimID = NCFILE%pNC_DIM(NC_X)%iNC_DimID

  NCFILE%pNC_VAR(NC_Z)%sVariableName = trim(sVarName_z)
  NCFILE%pNC_VAR(NC_Z)%iNC_VarType = NC_FLOAT
  NCFILE%pNC_VAR(NC_Z)%iNumberOfDimensions = 3
  NCFILE%pNC_VAR(NC_Z)%iNC_DimID = [NCFILE%pNC_DIM(NC_TIME)%iNC_DimID, &
                                 NCFILE%pNC_DIM(NC_Y)%iNC_DimID, &
                                 NCFILE%pNC_DIM(NC_X)%iNC_DimID,0]

  NCFILE%sVarName(NC_Z) = trim(sVarName_z)

end subroutine nf_set_standard_variables

!----------------------------------------------------------------------

subroutine nf_set_global_attributes(NCFILE, sDataType, sSourceFile)

  type (T_NETCDF4_FILE ) :: NCFILE
  character (len=*) :: sDataType
  character (len=*) :: sSourceFile

  ! [ LOCALS ]
  integer (c_int) :: iStat

  allocate( NCFILE%pNC_ATT(0:0), stat=iStat)
  call assert(iStat == 0, "Could not allocate memory for NC_ATT member of NC_FILE", &
    trim(__FILE__), __LINE__)


  block

    NCFILE%pNC_ATT(0)%sAttributeName = "source"
    allocate(NCFILE%pNC_ATT(0)%sAttValue(0:0))
    NCFILE%pNC_ATT(0)%sAttValue(0) = trim(sDataType)//" data from file "//trim(sSourceFile)
    NCFILE%pNC_ATT(0)%iNC_AttType = NC_CHAR
    NCFILE%pNC_ATT(0)%iNC_AttSize = 1_c_size_t

  end block

end subroutine nf_set_global_attributes

!----------------------------------------------------------------------

subroutine nf_set_standard_attributes(NCFILE, sOriginText, rX, rY)

  type (T_NETCDF4_FILE ) :: NCFILE
  character (len=*) :: sOriginText
  real (c_double), optional  :: rX(:,:)
  real (c_double), optional  :: rY(:,:)

  ! [ LOCALS ]
  integer (c_int) :: iStat
  integer (c_int) :: iNumAttributes
  type (T_NETCDF_ATTRIBUTE), dimension(:), pointer :: pNC_ATT

  iNumAttributes = 3
  allocate( NCFILE%pNC_VAR(NC_TIME)%pNC_ATT(0:iNumAttributes-1), stat=iStat)
  call assert(iStat == 0, "Could not allocate memory for NC_ATT member in NC_VAR struct of NC_FILE", &
    trim(__FILE__), __LINE__)
  NCFILE%pNC_VAR(NC_TIME)%iNumberOfAttributes = iNumAttributes

  pNC_ATT => NCFILE%pNC_VAR(NC_TIME)%pNC_ATT

  block

    pNC_ATT(0)%sAttributeName = "units"
    allocate(pNC_ATT(0)%sAttValue(0:0))
    pNC_ATT(0)%sAttValue(0) = "days since "//trim(sOriginText)//" 00:00:00"
    pNC_ATT(0)%iNC_AttType = NC_CHAR
    pNC_ATT(0)%iNC_AttSize = 1_c_size_t

    pNC_ATT(1)%sAttributeName = "calendar"
    allocate(pNC_ATT(1)%sAttValue(0:0))
    pNC_ATT(1)%sAttValue(0) = "proleptic_gregorian"
    ! pNC_ATT(1)%sAttValue(0) = "standard"
    pNC_ATT(1)%iNC_AttType = NC_CHAR
    pNC_ATT(1)%iNC_AttSize = 1_c_size_t

    pNC_ATT(2)%sAttributeName = "long_name"
    allocate(pNC_ATT(2)%sAttValue(0:0))
    pNC_ATT(2)%sAttValue(0) = "time"
    pNC_ATT(2)%iNC_AttType = NC_CHAR
    pNC_ATT(2)%iNC_AttSize = 1_c_size_t


  end block

  allocate( NCFILE%pNC_VAR(NC_Z)%pNC_ATT(0:iNumAttributes-1), stat=iStat)
  call assert(iStat == 0, "Could not allocate memory for NC_ATT member in NC_VAR struct of NC_FILE", &
    trim(__FILE__), __LINE__)
  NCFILE%pNC_VAR(NC_Z)%iNumberOfAttributes = iNumAttributes

  pNC_ATT => NCFILE%pNC_VAR(NC_Z)%pNC_ATT

  block

    pNC_ATT(0)%sAttributeName = "units"
    allocate(pNC_ATT(0)%sAttValue(0:0))
    pNC_ATT(0)%sAttValue(0) = NCFILE%sVarUnits(NC_Z)
    pNC_ATT(0)%iNC_AttType = NC_CHAR
    pNC_ATT(0)%iNC_AttSize = 1_c_size_t

    pNC_ATT(1)%sAttributeName = "calendar"
    allocate(pNC_ATT(1)%sAttValue(0:0))
    pNC_ATT(1)%sAttValue(0) = "standard"
    pNC_ATT(1)%iNC_AttType = NC_CHAR
    pNC_ATT(1)%iNC_AttSize = 1_c_size_t

    pNC_ATT(2)%sAttributeName = "long_name"
    allocate(pNC_ATT(2)%sAttValue(0:0))
    pNC_ATT(2)%sAttValue(0) = "time"
    pNC_ATT(2)%iNC_AttType = NC_CHAR
    pNC_ATT(2)%iNC_AttSize = 1_c_size_t


  end block


  allocate( NCFILE%pNC_VAR(NC_Y)%pNC_ATT(0:iNumAttributes-1), stat=iStat)
  call assert(iStat == 0, "Could not allocate memory for NC_ATT member in NC_VAR struct of NC_FILE", &
    trim(__FILE__), __LINE__)
  NCFILE%pNC_VAR(NC_Y)%iNumberOfAttributes = iNumAttributes

  pNC_ATT => NCFILE%pNC_VAR(NC_Y)%pNC_ATT

  block

    pNC_ATT(0)%sAttributeName = "units"
    allocate(pNC_ATT(0)%sAttValue(0:0))
    pNC_ATT(0)%sAttValue(0) = NCFILE%sVarUnits(NC_Y)
    pNC_ATT(0)%iNC_AttType = NC_CHAR
    pNC_ATT(0)%iNC_AttSize = 1_c_size_t

    pNC_ATT(1)%sAttributeName = "long_name"
    allocate(pNC_ATT(1)%sAttValue(0:0))
    pNC_ATT(1)%sAttValue(0) = "y coordinate of projection"
    pNC_ATT(1)%iNC_AttType = NC_CHAR
    pNC_ATT(1)%iNC_AttSize = 1_c_size_t

    pNC_ATT(2)%sAttributeName = "standard_name"
    allocate(pNC_ATT(2)%sAttValue(0:0))
    pNC_ATT(2)%sAttValue(0) = "projection_y_coordinate"
    pNC_ATT(2)%iNC_AttType = NC_CHAR
    pNC_ATT(2)%iNC_AttSize = 1_c_size_t


  end block

  allocate( NCFILE%pNC_VAR(NC_X)%pNC_ATT(0:iNumAttributes-1), stat=iStat)
  call assert(iStat == 0, "Could not allocate memory for NC_ATT member in NC_VAR struct of NC_FILE", &
    trim(__FILE__), __LINE__)
  NCFILE%pNC_VAR(NC_X)%iNumberOfAttributes = iNumAttributes

  pNC_ATT => NCFILE%pNC_VAR(NC_X)%pNC_ATT

  block

    pNC_ATT(0)%sAttributeName = "units"
    allocate(pNC_ATT(0)%sAttValue(0:0))
    pNC_ATT(0)%sAttValue(0) = NCFILE%sVarUnits(NC_X)
    pNC_ATT(0)%iNC_AttType = NC_CHAR
    pNC_ATT(0)%iNC_AttSize = 1_c_size_t

    pNC_ATT(1)%sAttributeName = "long_name"
    allocate(pNC_ATT(1)%sAttValue(0:0))
    pNC_ATT(1)%sAttValue(0) = "x coordinate of projection"
    pNC_ATT(1)%iNC_AttType = NC_CHAR
    pNC_ATT(1)%iNC_AttSize = 1_c_size_t

    pNC_ATT(2)%sAttributeName = "standard_name"
    allocate(pNC_ATT(2)%sAttValue(0:0))
    pNC_ATT(2)%sAttValue(0) = "projection_x_coordinate"
    pNC_ATT(2)%iNC_AttType = NC_CHAR
    pNC_ATT(2)%iNC_AttSize = 1_c_size_t


  end block



end subroutine nf_set_standard_attributes

!----------------------------------------------------------------------

subroutine nf_put_x_and_y(NCFILE, dpX, dpY)

  type (T_NETCDF4_FILE) :: NCFILE
  real (c_double), dimension(:) :: dpX
  real (c_double), dimension(:) :: dpY

  ! [ LOCALS ]
  integer (c_size_t) :: iLength
  real (c_double), dimension(:), allocatable :: rX, rY

  iLength = int(size(dpX, 1), c_size_t)

  call netcdf_put_variable_vector(NCFILE=NCFILE, &
                   iVarID=NCFILE%pNC_VAR(NC_X)%iNC_VarID, &
                   iStart=[0_c_size_t], &
                   iCount=[iLength], &
                   iStride=[1_c_ptrdiff_t], &
                   dpValues=dpX)

  iLength = int(size(dpY, 1), c_size_t)

  call netcdf_put_variable_vector(NCFILE=NCFILE, &
                   iVarID=NCFILE%pNC_VAR(NC_Y)%iNC_VarID, &
                   iStart=[0_c_size_t], &
                   iCount=[iLength], &
                   iStride=[1_c_ptrdiff_t], &
                   dpValues=dpY)

end subroutine nf_put_x_and_y

!----------------------------------------------------------------------

function nf_define_variable(NCFILE, sVariableName, iVariableType, &
   iNumberOfDimensions, iDimIDs)    result(iVarID)

  type (T_NETCDF4_FILE ) :: NCFILE
  character (len=*) :: sVariableName
  integer (c_int) :: iVariableType
  integer (c_int) :: iNumberOfDimensions
  integer (c_int), dimension(:) :: iDimIDs
  integer (c_int) :: iVarID

  call nf_trap( nc_def_var(ncid=NCFILE%iNCID,&
                           name=trim(fortran_to_c_string(sVariableName)), &
                           xtype=iVariableType, &
                           ndims=iNumberOfDimensions, &
                           dimidsp=iDimIDs, &
                           varidp=iVarID), &
                           __FILE__, __LINE__)

end function nf_define_variable

!----------------------------------------------------------------------

subroutine nf_define_variables( NCFILE )

  type (T_NETCDF4_FILE) :: NCFILE

  ! [ LOCALS ]
  integer (c_int) :: iStat
  integer (c_int) :: iIndex
  character (len=256) :: sDimName
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR

  do iIndex = 0, NCFILE%iNumberOfVariables-1

    pNC_VAR => NCFILE%pNC_VAR(iIndex)

    call nf_trap( nc_def_var(ncid=NCFILE%iNCID,&
                             name=trim(fortran_to_c_string(pNC_VAR%sVariableName)), &
                             xtype=pNC_VAR%iNC_VarType, &
                             ndims=pNC_VAR%iNumberOfDimensions, &
                             dimidsp=pNC_VAR%iNC_DimID, &
                             varidp=pNC_VAR%iNC_VarID), &
                             __FILE__, __LINE__)

  enddo

end subroutine nf_define_variables

!----------------------------------------------------------------------

subroutine nf_put_attribute(NCFILE, iVarID, sAttributeName, &
  sAttributeValue, iAttributeValue, rAttributeValue, dpAttributeValue)

  type (T_NETCDF4_FILE ) :: NCFILE
  integer (c_int) :: iVarID
  character (len=*) :: sAttributeName
  character (len=*), dimension(:), optional :: sAttributeValue
  integer (c_int), dimension(:), optional :: iAttributeValue
  real (c_float), dimension(:), optional :: rAttributeValue
  real (c_double), dimension(:), optional :: dpAttributeValue

  ! [ LOCALS ]
  integer (c_size_t) :: iNumberOfAttributes

  if (present(sAttributeValue) ) then

    iNumberOfAttributes = size( sAttributeValue, 1)
    iNumberOfAttributes = int(len_trim(sAttributeValue(1)), c_size_t)

    call nf_trap( nc_put_att_text(ncid=NCFILE%iNCID, &
                    varid=iVarID, &
                    name=trim(sAttributeName), &
                    nlen=iNumberOfAttributes, &
                    tp=trim(sAttributeValue(1))), &
                    __FILE__, __LINE__)

  elseif (present(iAttributeValue) ) then

    iNumberOfAttributes = size( iAttributeValue, 1)

    call nf_trap( nc_put_att_int(ncid=NCFILE%iNCID, &
                    varid=iVarID, &
                    name=trim(sAttributeName), &
                    xtype=NC_INT, &
                    nlen=iNumberOfAttributes, &
                     ip=iAttributeValue), &
                     __FILE__, __LINE__)

  elseif (present(rAttributeValue) ) then

    iNumberOfAttributes = size( rAttributeValue, 1)

    call nf_trap( nc_put_att_float(ncid=NCFILE%iNCID, &
                    varid=iVarID, &
                    name=trim(sAttributeName), &
                    xtype=NC_FLOAT, &
                    nlen=iNumberOfAttributes, &
                    fp=rAttributeValue), &
                    __FILE__, __LINE__)

  elseif (present(dpAttributeValue) ) then

    iNumberOfAttributes = size( dpAttributeValue, 1)

    call nf_trap( nc_put_att_double(ncid=NCFILE%iNCID, &
                       varid=iVarID, &
                       name=trim(sAttributeName), &
                       xtype=NC_DOUBLE, &
                       nlen=iNumberOfAttributes, &
                       dp=dpAttributeValue), &
                       __FILE__, __LINE__)

  endif


end subroutine nf_put_attribute

!----------------------------------------------------------------------

subroutine nf_put_attributes(NCFILE)

  type (T_NETCDF4_FILE ) :: NCFILE

  ! [ LOCALS ]
  integer (c_size_t) :: iNumberOfAttributes
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  type (T_NETCDF_ATTRIBUTE), pointer :: pNC_ATT
  integer (c_int) :: iIndex
  integer (c_int) :: iIndex2
  integer (c_int) :: iStat

  do iIndex = 0, NCFILE%iNumberOfVariables-1

    pNC_VAR => NCFILE%pNC_VAR(iIndex)

    do iIndex2 = 0, pNC_VAR%iNumberOfAttributes-1

      pNC_ATT => pNC_VAR%pNC_ATT(iIndex2)

        select case (pNC_ATT%iNC_AttType)

          case (NC_DOUBLE)

            call assert(allocated(pNC_ATT%dpAttValue), &
              "INTERNAL PROGRAMMING ERROR - attempt to use unallocated variable", &
              trim(__FILE__), __LINE__)

            call nf_put_attribute(NCFILE=NCFILE, &
                iVarID=pNC_VAR%iNC_VarID, &
                sAttributeName=trim(pNC_ATT%sAttributeName)//c_null_char, &
                dpAttributeValue=pNC_ATT%dpAttValue)

          case (NC_INT)

            call assert(allocated(pNC_ATT%iAttValue), &
              "INTERNAL PROGRAMMING ERROR - attempt to use unallocated variable", &
              trim(__FILE__), __LINE__)

            call nf_put_attribute(NCFILE=NCFILE, &
                iVarID=pNC_VAR%iNC_VarID, &
                sAttributeName=trim(pNC_ATT%sAttributeName)//c_null_char, &
                iAttributeValue=pNC_ATT%iAttValue)

          case (NC_FLOAT)

            call assert(allocated(pNC_ATT%rAttValue), &
              "INTERNAL PROGRAMMING ERROR - attempt to use unallocated variable", &
              trim(__FILE__), __LINE__)

            call nf_put_attribute(NCFILE=NCFILE, &
                iVarID=pNC_VAR%iNC_VarID, &
                sAttributeName=trim(pNC_ATT%sAttributeName)//c_null_char, &
                rAttributeValue=pNC_ATT%rAttValue)

          case (NC_CHAR)

            call assert(allocated(pNC_ATT%sAttValue), &
              "INTERNAL PROGRAMMING ERROR - attempt to use unallocated variable", &
              trim(__FILE__), __LINE__)

            call nf_put_attribute(NCFILE=NCFILE, &
                iVarID=pNC_VAR%iNC_VarID, &
                sAttributeName=trim(pNC_ATT%sAttributeName)//c_null_char, &
                sAttributeValue=[trim(pNC_ATT%sAttValue(0))//c_null_char])

        end select

    enddo

  enddo


  do iIndex2 = 0, NCFILE%iNumberOfAttributes-1

    pNC_ATT => NCFILE%pNC_ATT(iIndex2)

    select case (pNC_ATT%iNC_AttType)

      case (NC_DOUBLE)

        call assert(allocated(pNC_ATT%dpAttValue), &
          "INTERNAL PROGRAMMING ERROR - attempt to use unallocated variable", &
          trim(__FILE__), __LINE__)

        call nf_put_attribute(NCFILE=NCFILE, &
            iVarID=NC_GLOBAL, &
            sAttributeName=trim(pNC_ATT%sAttributeName)//c_null_char, &
            dpAttributeValue=pNC_ATT%dpAttValue)

      case (NC_INT)

        call assert(allocated(pNC_ATT%iAttValue), &
          "INTERNAL PROGRAMMING ERROR - attempt to use unallocated variable", &
          trim(__FILE__), __LINE__)

        call nf_put_attribute(NCFILE=NCFILE, &
            iVarID=NC_GLOBAL, &
            sAttributeName=trim(pNC_ATT%sAttributeName)//c_null_char, &
            iAttributeValue=pNC_ATT%iAttValue)

      case (NC_FLOAT)

        call assert(allocated(pNC_ATT%rAttValue), &
          "INTERNAL PROGRAMMING ERROR - attempt to use unallocated variable", &
          trim(__FILE__), __LINE__)

        call nf_put_attribute(NCFILE=NCFILE, &
            iVarID=NC_GLOBAL, &
            sAttributeName=trim(pNC_ATT%sAttributeName)//c_null_char, &
            rAttributeValue=pNC_ATT%rAttValue)

      case (NC_CHAR)

        call assert(allocated(pNC_ATT%sAttValue), &
          "INTERNAL PROGRAMMING ERROR - attempt to use unallocated variable", &
          trim(__FILE__), __LINE__)

        call nf_put_attribute(NCFILE=NCFILE, &
            iVarID=NC_GLOBAL, &
            sAttributeName=trim(pNC_ATT%sAttributeName)//c_null_char, &
            sAttributeValue=[trim(pNC_ATT%sAttValue(0))//c_null_char])

    end select

  enddo

end subroutine nf_put_attributes

!----------------------------------------------------------------------

subroutine netcdf_put_variable_array(NCFILE, iVarID, iStart, iCount, iStride, &
   iValues, i2Values, rValues, dpValues)

  type (T_NETCDF4_FILE ) :: NCFILE
  integer (c_int) :: iVarID
  integer (c_size_t), dimension(:) :: iStart
  integer (c_size_t), dimension(:) :: iCount
  integer (c_ptrdiff_t), dimension(:) :: iStride
  integer (c_int), dimension(:,:), optional :: iValues
  integer (c_short), dimension(:,:), optional :: i2Values
  real (c_float), dimension(:,:), optional :: rValues
  real (c_double), dimension(:,:), optional :: dpValues

  if (present(iValues) ) then

    call nf_trap(nc_put_vars_int(ncid=NCFILE%iNCID, &
                       varid=iVarID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=iValues), &
                       __FILE__, __LINE__)

  elseif (present(i2Values) ) then

    call nf_trap(nc_put_vars_short(ncid=NCFILE%iNCID, &
                       varid=iVarID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=i2Values), &
                       __FILE__, __LINE__)

  elseif (present(rValues) ) then

   call nf_trap(nc_put_vars_float(ncid=NCFILE%iNCID, &
                       varid=iVarID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=rValues), &
                       __FILE__, __LINE__)

  elseif (present(dpValues) ) then



    call nf_trap(nc_put_vars_double(ncid=NCFILE%iNCID, &
                       varid=iVarID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=dpValues), &
                       __FILE__, __LINE__)

  endif

end subroutine netcdf_put_variable_array


subroutine netcdf_put_variable_vector(NCFILE, iVarID, iStart, iCount, iStride, &
   iValues, i2Values, rValues, dpValues)

  type (T_NETCDF4_FILE ) :: NCFILE
  integer (c_int) :: iVarID
  integer (c_size_t), dimension(:) :: iStart
  integer (c_size_t), dimension(:) :: iCount
  integer (c_ptrdiff_t), dimension(:) :: iStride
  integer (c_int), dimension(:), optional :: iValues
  integer (c_short), dimension(:), optional :: i2Values
  real (c_float), dimension(:), optional :: rValues
  real (c_double), dimension(:), optional :: dpValues

  if (present(iValues) ) then

    call nf_trap(nc_put_vars_int(ncid=NCFILE%iNCID, &
                       varid=iVarID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=iValues), &
                       __FILE__, __LINE__)

  elseif (present(i2Values) ) then

    call nf_trap(nc_put_vars_short(ncid=NCFILE%iNCID, &
                       varid=iVarID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=i2Values), &
                       __FILE__, __LINE__)

  elseif (present(rValues) ) then

   call nf_trap(nc_put_vars_float(ncid=NCFILE%iNCID, &
                       varid=iVarID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=rValues), &
                       __FILE__, __LINE__)

  elseif (present(dpValues) ) then



    call nf_trap(nc_put_vars_double(ncid=NCFILE%iNCID, &
                       varid=iVarID, &
                       startp=iStart, &
                       countp=iCount, &
                       stridep=iStride, &
                       vars=dpValues), &
                       __FILE__, __LINE__)

  endif

end subroutine netcdf_put_variable_vector

end module netcdf4_support
