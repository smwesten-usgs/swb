!> @file
!> @brief  Contains a single module, @ref netcdf_support, which
!>  provides support for use of NetCDF files as input or output.

!> @brief Provides support for use of NetCDF files as input for time-varying,
!>  gridded meteorlogic data, or output for any SWB-generated variable.
module netcdf4_support

#ifdef NETCDF_SUPPORT

  use types
  use swb_grid
  use typesizes
  use netcdf_c_api_interfaces
  use iso_c_binding

  implicit none

  integer(kind=c_int) :: NC_READONLY          = 0
  integer(kind=c_int) :: NC_READWRITE         = 1

  integer(kind=c_int), parameter :: NC_FORMAT_CLASSIC   = 1
  integer(kind=c_int), parameter :: NC_FORMAT_64BIT     = 2
  integer(kind=c_int), parameter :: NC_FORMAT_NETCDF4   = 3
  integer(kind=c_int), parameter :: NC_FORMAT_NETCDF4_CLASSIC = 4

  integer(kind=c_int), parameter ::  NC_NAT    = 0
  integer(kind=c_int), parameter ::  NC_BYTE   = 1
  integer(kind=c_int), parameter ::  NC_CHAR   = 2
  integer(kind=c_int), parameter ::  NC_SHORT  = 3
  integer(kind=c_int), parameter ::  NC_INT    = 4
  integer(kind=c_int), parameter ::  NC_FLOAT  = 5
  integer(kind=c_int), parameter ::  NC_DOUBLE = 6

  integer(kind=c_int),  parameter :: NC_UNLIMITED = 0
  integer(kind=c_int),  parameter :: NC_GLOBAL    = -1

  integer (c_int), parameter :: NC_X    = 0
  integer (c_int), parameter :: NC_Y    = 1
  integer (c_int), parameter :: NC_Z    = 2
  integer (c_int), parameter :: NC_TIME = 3

  integer (kind=c_int), parameter :: FIRST = 0
  integer (kind=c_int), parameter :: LAST = 1

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
    integer (kind=c_int) :: iNC_DimID = -9999
    integer (kind=c_size_t) :: iNC_DimSize
    logical (kind=T_LOGICAL) :: lUnlimited = lFALSE
  end type T_NETCDF_DIMENSION

  type T_NETCDF_ATTRIBUTE
    character (len=64) :: sAttributeName
    character (len=512) :: sAttributeValue
    integer (kind=c_int) :: iNC_AttType
    integer (kind=c_size_t) :: iNC_AttSize
  end type T_NETCDF_ATTRIBUTE

  type T_NETCDF_VARIABLE
    character (len=64) :: sVariableName
    integer (kind=c_int) :: iNC_VarID = -9999
    integer (kind=c_int) :: iNC_VarType
    integer (kind=c_int) :: iNumberOfDimensions
    integer (kind=c_int), dimension(0:3) :: iNC_DimID = -9999
    integer (kind=c_int) :: iNumberOfAttributes
    type (T_NETCDF_ATTRIBUTE), dimension(:), pointer :: NC_ATT
  end type T_NETCDF_VARIABLE

  type T_NETCDF4_FILE
    integer (kind=c_int) :: iNCID
    character (len=64) :: sFilename
    integer (kind=c_int) :: iFileFormat
    integer (kind=c_int) :: iNumberOfDimensions
    integer (kind=c_int) :: iNumberOfVariables
    integer (kind=c_int) :: iNumberOfAttributes
    integer (kind=c_int) :: iNC3_UnlimitedDimensionNumber
    integer (kind=c_int) :: iOriginJD
    integer (kind=c_int) :: iFirstDayJD
    integer (kind=c_int) :: iLastDayJD
    integer (kind=c_int) :: iOriginMonth
    integer (kind=c_int) :: iOriginDay
    integer (kind=c_int) :: iOriginYear
    integer (kind=c_int) :: iOriginHH
    integer (kind=c_int) :: iOriginMM
    integer (kind=c_int) :: iOriginSS
    integer (kind=c_size_t), dimension(0:2) :: iStart
    integer (kind=c_size_t), dimension(0:2) :: iCount
    integer (kind=c_size_t), dimension(0:2) :: iStride = 1
    integer (kind=c_size_t), dimension(0:1) :: iRowBounds
    integer (kind=c_size_t), dimension(0:1) :: iColBounds
    real (kind=c_double), dimension(2) :: dpFirstAndLastTimeValues
    integer (kind=c_int), dimension(0:3) :: iVarID = -9999
    integer (kind=c_int), dimension(0:3) :: iVarIndex = -9999
    integer (kind=c_int), dimension(0:3) :: iVarType = -9999
    integer (kind=c_int), dimension(0:3, 0:3) :: iVar_DimID = -9999

    real (kind=c_float), allocatable, dimension(:) :: rX_Coords
    real (kind=c_float), allocatable, dimension(:) :: rY_Coords

    type (T_NETCDF_DIMENSION), dimension(:), pointer :: NC_DIM
    type (T_NETCDF_VARIABLE), dimension(:), pointer :: NC_VAR
    type (T_NETCDF_ATTRIBUTE), dimension(:), pointer :: NC_ATT
  end type T_NETCDF4_FILE

contains

!----------------------------------------------------------------------

function netcdf_date_within_range( NCFILE, iJulianDay)  result( lWithinRange )

  type (T_NETCDF4_FILE ), pointer :: NCFILE
  integer (kind=T_INT) :: iJulianDay
  logical (kind=T_LOGICAL) :: lWithinRange

  if ( iJulianDay >= NCFILE%iFirstDayJD &
      .and. iJulianDay <= NCFILE%iLastDayJD ) then

    lWithinRange = lTRUE

  else

    lWithinRange = lFALSE

  endif

end function netcdf_date_within_range

!----------------------------------------------------------------------

function netcdf_date_to_index( NCFILE, iJulianDay )  result(iStart)

  type (T_NETCDF4_FILE ), pointer :: NCFILE
  integer (kind=T_INT) :: iJulianDay
  integer (kind=c_size_t) :: iStart

  iStart = int(iJulianDay - NCFILE%iFirstDayJD, kind=c_size_t)

end function netcdf_date_to_index

!----------------------------------------------------------------------

function netcdf_return_VarID( NCFILE, iVarIndex)   result(iVarID)

  type (T_NETCDF4_FILE ), pointer :: NCFILE
   integer (kind=T_INT) :: iVarIndex
   integer (kind=T_INT) :: iVarID

   type (T_NETCDF_VARIABLE), pointer :: pNC_VAR

   pNC_VAR => NCFILE%NC_VAR(iVarIndex)

   iVarID = pNC_VAR%iNC_VarID

end function netcdf_return_VarID

!----------------------------------------------------------------------

function netcdf_return_DimID( NCFILE, iDimIndex)   result(iDimID)

  type (T_NETCDF4_FILE ), pointer :: NCFILE
   integer (kind=T_INT) :: iDimIndex
   integer (kind=T_INT) :: iDimID

   type (T_NETCDF_DIMENSION), pointer :: pNC_DIM

   pNC_DIM => NCFILE%NC_DIM(iDimIndex)

   iDimID = pNC_DIM%iNC_DimID

end function netcdf_return_DimID

!----------------------------------------------------------------------

function netcdf_return_VarIndex( NCFILE, iVarID)   result(iVarIndex)

  type (T_NETCDF4_FILE ), pointer :: NCFILE
   integer (kind=T_INT) :: iVarID
   integer (kind=T_INT) :: iVarIndex

   type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
   integer (kind=T_INT) :: iIndex
   logical (kind=T_LOGICAL) :: lFound

  lFound = lFALSE

  do iIndex=0, NCFILE%iNumberOfVariables - 1

    pNC_VAR => NCFILE%NC_VAR(iIndex)

    if (pNC_VAR%iNC_VarID == iVarID) then
      lFound = lTRUE
      exit
    endif

  enddo

  call assert(lFound, "INTERNAL PROGRAMMING ERROR - No matching variable " &
    //"ID found: was looking for Variable ID: "//trim(asCharacter(iVarID)), &
    trim(__FILE__), __LINE__)

  iVarIndex = iIndex

end function netcdf_return_VarIndex

!----------------------------------------------------------------------

function netcdf_return_AttValue( NCFILE, iVarIndex, sAttName)   result(sAttValue)

  type (T_NETCDF4_FILE ), pointer :: NCFILE
   integer (kind=T_INT) :: iVarIndex
   character (len=*) :: sAttName
   character (len=256) :: sAttValue

   type (T_NETCDF_ATTRIBUTE), dimension(:), pointer :: pNC_ATT
   integer (kind=T_INT) :: iIndex
   logical (kind=T_LOGICAL) :: lFound

  if (iVarIndex < 0) then

    pNC_ATT => NCFILE%NC_ATT

  else

    call assert(iVarIndex >= lbound(NCFILE%NC_VAR,1) &
      .and. iVarIndex <= ubound(NCFILE%NC_VAR,1), &
      "Index out of bounds referencing NCFILE%NC_VAR" &
      //"~Offending index value: "//trim(asCharacter(iVarIndex)), &
      trim(__FILE__), __LINE__)

    pNC_ATT => NCFILE%NC_VAR(iVarIndex)%NC_ATT

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

  sAttValue = pNC_ATT(iIndex)%sAttributeValue

end function netcdf_return_AttValue

!----------------------------------------------------------------------

function netcdf_return_DimIndex( NCFILE, iDimID)   result(iDimIndex)

  type (T_NETCDF4_FILE ), pointer :: NCFILE
   integer (kind=T_INT) :: iDimID
   integer (kind=T_INT) :: iDimIndex

   type (T_NETCDF_DIMENSION), pointer :: pNC_DIM
   integer (kind=T_INT) :: iIndex
   logical (kind=T_LOGICAL) :: lFound

  lFound = lFALSE

  do iIndex=0, NCFILE%iNumberOfDimensions - 1

    pNC_DIM => NCFILE%NC_DIM(iIndex)

    if (pNC_DIM%iNC_DimID == iDimID) then
      lFound = lTRUE
      exit
    endif

  enddo

  call assert(lFound, "INTERNAL PROGRAMMING ERROR - No matching dimension " &
    //"ID found: was looking for Dimension ID: "//trim(asCharacter(iDimID)), &
    trim(__FILE__), __LINE__)

  iDimIndex = iIndex

end function netcdf_return_DimIndex

!----------------------------------------------------------------------

function netcdf_return_DimSize( NCFILE, iDimID)   result(iDimSize)

  type (T_NETCDF4_FILE ), pointer :: NCFILE
   integer (kind=T_INT) :: iDimID
   integer (kind=c_size_t) :: iDimSize

   type (T_NETCDF_DIMENSION), pointer :: pNC_DIM
   integer (kind=T_INT) :: iIndex
   logical (kind=T_LOGICAL) :: lFound

  lFound = lFALSE

  do iIndex=0, NCFILE%iNumberOfDimensions - 1

    pNC_DIM => NCFILE%NC_DIM(iIndex)

    if (pNC_DIM%iNC_DimID == iDimID) then
      lFound = lTRUE
      exit
    endif

  enddo

  call assert(lFound, "INTERNAL PROGRAMMING ERROR - No matching dimension " &
    //"ID found: was looking for Dimension ID: "//trim(asCharacter(iDimID)), &
    trim(__FILE__), __LINE__)

  iDimSize = pNC_DIM%iNC_DimSize

end function netcdf_return_DimSize

!----------------------------------------------------------------------

function return_start_indices(NCFILE, iJulianDay)  result( iIndices)

  type (T_NETCDF4_FILE ), pointer :: NCFILE
  integer (kind=c_int) :: iJulianDay

  integer (kind=c_int), dimension(:), allocatable :: iIndices





end function return_start_indices

!----------------------------------------------------------------------

subroutine netcdf_parse_origin_date(NCFILE)

  type (T_NETCDF4_FILE ), pointer :: NCFILE

  ! [ LOCALS ]
  integer (kind=c_int) :: iMonth, iDay, iYear
  integer (kind=c_int) :: iHH, iMM, iSS
  character (len=256) :: sDateText, sTimeText
  character (len=256) :: sRecord, sItem

  sRecord = netcdf_return_AttValue(NCFILE=NCFILE, &
      iVarIndex=NCFILE%iVarIndex(NC_TIME), &
      sAttName="units")

  ! after call sItem should contain "days"
  call chomp(sRecord, sItem)

  ! after call sItem should contain "since"
  call chomp(sRecord, sItem)

  ! now get the date and time text strings
  call chomp(sRecord, sDateText)
  call chomp(sRecord, sTimeText)

  call chomp(sDateText, sItem, "/-")
  read(sItem, *) NCFILE%iOriginYear

  call chomp(sDateText, sItem, "/-")
  read(sItem, *) NCFILE%iOriginMonth

  read(sDateText, *) NCFILE%iOriginDay

  call chomp(sTimeText, sItem, ":")
  read(sItem, *) NCFILE%iOriginHH

  call chomp(sTimeText, sItem, ":")
  read(sItem, *) NCFILE%iOriginMM

  read(sTimeText, *) NCFILE%iOriginSS

end subroutine netcdf_parse_origin_date

!----------------------------------------------------------------------

function netcdf_open_and_prepare(sFilename, sVarName_x, &
    sVarName_y, sVarName_z, sVarName_time, tGridBounds, iLU)     result(NCFILE)

  character (len=*) :: sFilename
  character (len=*), optional :: sVarName_x
  character (len=*), optional :: sVarName_y
  character (len=*), optional :: sVarName_z
  character (len=*), optional :: sVarName_time
  type (T_GRID_BOUNDS), optional :: tGridBounds
  integer (kind=T_INT), optional :: iLU

  ! [ LOCALS ]
  type (T_NETCDF4_FILE ), pointer :: NCFILE
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  type (T_NETCDF_DIMENSION), pointer :: pNC_DIM
  logical (kind=T_LOGICAL) :: lFileOpen
  character (len=256) :: sX_VarName
  character (len=256) :: sY_VarName
  character (len=256) :: sZ_VarName
  character (len=256) :: sTime_VarName
  integer (kind=c_int) :: iXll, iXur, iYll, iYur
  integer (kind=T_INT) :: iIndex


  allocate(NCFILE)

  call netcdf_open_file(NCFILE=NCFILE, sFilename=sFilename)

  call netcdf_populate_dimension_struct( NCFILE )
  call netcdf_populate_variable_struct( NCFILE )

  if( present(iLU) ) then
    inquire (unit=iLU, opened=lFileOpen)
    if ( lFileOpen )  call netcdf_dump_cdl( NCFILE, iLU)
  endif

  call netcdf_dump_cdl( NCFILE, LU_STD_OUT)

  if (present(sVarName_x) ) then
    sX_VarName = sVarName_x
  else
    sX_VarName = "x"
  endif

  if (present(sVarName_y) ) then
    sY_VarName = sVarName_y
  else
    sY_VarName = "y"
  endif

  if (present(sVarName_z) ) then
    sZ_VarName = sVarName_z
  else
    sZ_VarName = "prcp"
  endif

  if (present(sVarName_time) ) then
    sTime_VarName = sVarName_time
  else
    sTime_VarName = "time"
  endif

  call netcdf_get_variable_id_and_type( NCFILE, trim(sX_VarName), &
    trim(sY_VarName), trim(sZ_VarName), trim(sTime_VarName) )

  NCFILE%dpFirstAndLastTimeValues = netcdf_get_first_and_last(NCFILE=NCFILE, &
      iVarIndex=NCFILE%iVarIndex(NC_TIME) )

  call netcdf_parse_origin_date(NCFILE=NCFILE)

  call netcdf_calculate_time_range(NCFILE)
!  call netcdf_get_x_and_y(NCFILE)

  if (present(tGridBounds) ) then

    NCFILE%iRowBounds = netcdf_get_index_double(NCFILE, NCFILE%iVarID(NC_Y), &
      [tGridBounds%rYll,tGridBounds%rYur])

    NCFILE%iColBounds = netcdf_get_index_double(NCFILE, NCFILE%iVarID(NC_X), &
      [tGridBounds%rXll,tGridBounds%rXur])

  else

    NCFILE%iRowBounds = [1_c_size_t, &
                         netcdf_return_DimSize( NCFILE, NCFILE%iVar_DimID(NC_Y, 0) ) ]

    NCFILE%iColBounds = [1_c_size_t, &
                         netcdf_return_DimSize( NCFILE, NCFILE%iVar_DimID(NC_X, 0) ) ]

  endif

  pNC_VAR => NCFILE%NC_VAR( NCFILE%iVarIndex(NC_Z) )

  ! loop over the three (assumed) dimensions of the "Z" variable;
  ! assign appropriate bounds to each
  do iIndex = 0,2

    call assert (iIndex >= lbound(NCFILE%NC_DIM,1) &
      .and. iIndex <= pNC_VAR%iNumberOfDimensions-1, &
      "INTERNAL PROGRAMMING ERROR? - Dimension out of bounds", &
      trim(__FILE__), __LINE__)

    select case (iIndex)

      case (NC_X)

        NCFILE%iStart(iIndex) = NCFILE%iColBounds(FIRST)
        NCFILE%iCount(iIndex) = NCFILE%iColBounds(LAST) - NCFILE%iColBounds(FIRST) + 1
        NCFILE%iStride(iIndex) = 1_c_size_t

      case (NC_Y)

        NCFILE%iStart(iIndex) = NCFILE%iRowBounds(FIRST)
        NCFILE%iCount(iIndex) = NCFILE%iRowBounds(LAST) - NCFILE%iRowBounds(FIRST) + 1
        NCFILE%iStride(iIndex) = 1_c_size_t

      case (NC_TIME)

        NCFILE%iStart(iIndex) = 0_c_size_t
        NCFILE%iCount(iIndex) = 1_c_size_t
        NCFILE%iStride(iIndex) = 1_c_size_t

      case default

    end select

  enddo

end function netcdf_open_and_prepare

!----------------------------------------------------------------------

subroutine netcdf_get_x_and_y(NCFILE)

  type (T_NETCDF4_FILE ), pointer :: NCFILE

  integer (kind=T_INT) :: iVarIndex_x, iVarIndex_y
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR_x, pNC_VAR_y
  type (T_NETCDF_DIMENSION), pointer :: pNC_DIM_x, pNC_DIM_y

  iVarIndex_x = NCFILE%iVarIndex(NC_X)
  iVarIndex_y = NCFILE%iVarIndex(NC_Y)

  call assert(iVarIndex_x >= lbound(NCFILE%NC_VAR,1) &
    .and. iVarIndex_x <= ubound(NCFILE%NC_VAR,1), &
    "INTERNAL PROGRAMMING ERROR - Index out of bounds", trim(__FILE__), __LINE__)

  call assert(iVarIndex_y >= lbound(NCFILE%NC_VAR,1) &
    .and. iVarIndex_y <= ubound(NCFILE%NC_VAR,1), &
    "INTERNAL PROGRAMMING ERROR - Index out of bounds", trim(__FILE__), __LINE__)

  pNC_VAR_x => NCFILE%NC_VAR(iVarIndex_x)
  pNC_VAR_y => NCFILE%NC_VAR(iVarIndex_y)

  call assert( pNC_VAR_x%iNumberOfDimensions == 1, &
    "Dimensions other than one for the x-coordinate variable are currently unsupported.", &
    trim(__FILE__), __LINE__)

  call assert( pNC_VAR_y%iNumberOfDimensions == 1, &
    "Dimensions other than one for the y-coordinate variable are currently unsupported.", &
    trim(__FILE__), __LINE__)

  pNC_DIM_x => NCFILE%NC_DIM( pNC_VAR_x%iNC_DimID(0) )
  pNC_DIM_y => NCFILE%NC_DIM( pNC_VAR_y%iNC_DimID(0) )

  allocate( NCFILE%rX_Coords( pNC_DIM_x%iNC_DimSize ) )
  allocate (NCFILE%rY_Coords( pNC_DIM_y%iNC_DimSize  ) )

  NCFILE%rX_Coords = netcdf_get_variable_double(NCFILE=NCFILE, &
       iNC_VarID=pNC_VAR_x%iNC_VarID, &
       iNC_Start=[0], &
       iNC_Count=[pNC_DIM_x%iNC_DimSize], &
       iNC_Stride=[1])

  NCFILE%rY_Coords = netcdf_get_variable_double(NCFILE=NCFILE, &
       iNC_VarID=pNC_VAR_y%iNC_VarID, &
       iNC_Start=[0], &
       iNC_Count=[pNC_DIM_y%iNC_DimSize], &
       iNC_Stride=[1])

! get coords from NCFILE

! need another subroutine that takes the grid bounds and turns them
! into row, column bounds


end subroutine netcdf_get_x_and_y

!----------------------------------------------------------------------

subroutine netcdf_open_file(NCFILE, sFilename, iLU)

  type (T_NETCDF4_FILE ), pointer :: NCFILE
  character (len=*) :: sFilename
  integer (kind=T_INT), optional :: iLU

  ! [ LOCALS ]
  logical (kind=T_LOGICAL) :: lFileOpen

  write(UNIT=LU_LOG,FMT="(a)") "Attempting to open READONLY NetCDF file: " &
    //dquote(sFilename)

  call nc_trap( nc_open(sFilename//c_null_char, &
                NC_READONLY, NCFILE%iNCID) )

  call nc_trap( nc_inq_format(ncid=NCFILE%iNCID, formatp=NCFILE%iFileFormat) )

  write(UNIT=LU_LOG,FMT="(a,/,a,i0,/,a)") "   Succeeded.","  ncid: ",NCFILE%iNCID, &
         "  format: "//trim(NETCDF_FORMAT_STRING(NCFILE%iFileFormat) )

  NCFILE%sFilename = sFilename

  if( present(iLU) ) then
    inquire (unit=iLU, opened=lFileOpen)
    if ( lFileOpen )  call netcdf_dump_cdl( NCFILE, iLU)
  endif

end subroutine netcdf_open_file

!----------------------------------------------------------------------

subroutine nc_trap( iResultCode )

  integer (kind=c_int) :: iResultCode
  type(c_ptr) :: cpResult
  character (len=256) :: sTextString

  if (iResultCode /= 0) then

    cpResult = nc_strerror(iResultCode)
    sTextString = char_ptr_to_fortran_string( cpResult )

    call echolog("NetCDF ERROR: "//dquote( sTextString  )//" | error code was: " &
      //trim(asCharacter(iResultCode)) )

    call assert(lFALSE, "SWB is stopping due to a problem reading or accessing" &
      //" a NetCDF file", trim(__FILE__), __LINE__)

  endif

end subroutine nc_trap

!----------------------------------------------------------------------

subroutine netcdf_close_file( NCFILE)

  type (T_NETCDF4_FILE ), pointer :: NCFILE

  call nc_trap( nc_close(NCFILE%iNCID) )

end subroutine netcdf_close_file

!----------------------------------------------------------------------

subroutine netcdf_deallocate_data_struct( NCFILE )

  type (T_NETCDF4_FILE ), pointer :: NCFILE

  ! [ LOCALS ]
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  integer (kind=T_INT) :: iIndex

  do iIndex=0, NCFILE%iNumberOfVariables - 1

    pNC_VAR => NCFILE%NC_VAR(iIndex)

    if (pNC_VAR%iNumberOfAttributes == 0 ) cycle

    if (associated( pNC_VAR%NC_ATT ))  deallocate( pNC_VAR%NC_ATT )

  enddo

  if (associated( NCFILE%NC_VAR ))  deallocate( NCFILE%NC_VAR )
  if (associated( NCFILE%NC_ATT ))  deallocate( NCFILE%NC_ATT )
  if (associated( NCFILE%NC_DIM ))  deallocate( NCFILE%NC_DIM )
  if (associated( NCFILE ))  deallocate ( NCFILE )

end subroutine netcdf_deallocate_data_struct

!----------------------------------------------------------------------

subroutine netcdf_populate_dimension_struct( NCFILE )

  type (T_NETCDF4_FILE), pointer :: NCFILE
  integer (kind=T_INT) :: iStat
  integer (kind=T_INT) :: iIndex
  character (len=256) :: sDimName

  call nc_trap( nc_inq_ndims(ncid=NCFILE%iNCID, ndimsp=NCFILE%iNumberOfDimensions) )

  allocate(NCFILE%NC_DIM( 0 : NCFILE%iNumberOfDimensions-1), stat=iStat )
  call assert(iStat == 0, "Could not allocate memory for NC_DIM member in NC_FILE defined type", &
    trim(__FILE__), __LINE__)

  ! NetCDF 3 function
  call nc_trap( nc_inq_unlimdim(ncid=NCFILE%iNCID, unlimdimidp=NCFILE%iNC3_UnlimitedDimensionNumber) )

  do iIndex = 0, NCFILE%iNumberOfDimensions-1

    call nc_trap(nc_inq_dim(ncid=NCFILE%iNCID, dimid=iIndex, &
      name=sDimName, &
      lenp=NCFILE%NC_DIM(iIndex)%iNC_DimSize) )

    NCFILE%NC_DIM(iIndex)%iNC_DimID = iIndex
    NCFILE%NC_DIM(iIndex)%sDimensionName = c_to_fortran_string(sDimName)

  enddo

end subroutine netcdf_populate_dimension_struct

!----------------------------------------------------------------------

subroutine netcdf_populate_variable_struct( NCFILE )

  type (T_NETCDF4_FILE), pointer :: NCFILE

  type (T_NETCDF_ATTRIBUTE), pointer :: pNC_ATT
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  integer (kind=T_INT) :: iStat
  integer (kind=T_INT) :: iIndex, iIndex2 , iIndex3
  character (len=256) :: sVarName
  character (len=256) :: sAttName
  character (len=512) :: sAttValue
  integer (kind=c_int), dimension(0:25) :: iAttValue
  integer (kind=c_short), dimension(0:25) :: i2AttValue
  real (kind=c_double), dimension(0:25) :: cdAttValue

  call nc_trap( nc_inq_nvars(ncid=NCFILE%iNCID, nvarsp=NCFILE%iNumberOfVariables) )

  allocate(NCFILE%NC_VAR( 0 : NCFILE%iNumberOfVariables-1), stat=iStat )
  call assert(iStat == 0, "Could not allocate memory for NC_VAR member in NC_FILE defined type", &
    trim(__FILE__), __LINE__)

  do iIndex = 0, NCFILE%iNumberOfVariables-1

    pNC_VAR => NCFILE%NC_VAR(iIndex)

    call nc_trap(nc_inq_var(ncid=NCFILE%iNCID, &
        varid=iIndex, &
        name=sVarName, &
        xtypep=pNC_VAR%iNC_VarType, &
        ndimsp=pNC_VAR%iNumberOfDimensions, &
        dimidsp=pNC_VAR%iNC_DimID, &
        nattsp=pNC_VAR%iNumberOfAttributes ) )

    pNC_VAR%iNC_VarID = iIndex
    pNC_VAR%sVariableName = c_to_fortran_string(sVarName)

    if( pNC_VAR%iNumberOfAttributes > 0 ) then

      allocate( pNC_VAR%NC_ATT( 0:pNC_VAR%iNumberOfAttributes - 1 ), stat = iStat)
      call assert(iStat == 0, "Could not allocate memory for NC_ATT member within NC_VAR in NC_FILE defined type", &
        trim(__FILE__), __LINE__)

      do iIndex2=0, pNC_VAR%iNumberOfAttributes - 1

        pNC_ATT => pNC_VAR%NC_ATT(iIndex2)

        call netcdf_populate_attribute_struct( NCFILE=NCFILE, pNC_ATT=pNC_ATT, &
          iNC_VarID=iIndex, iAttNum=iIndex2 )

      enddo

    endif

  enddo

  call nc_trap( nc_inq_natts(ncid=NCFILE%iNCID, ngattsp=NCFILE%iNumberOfAttributes) )

  allocate(NCFILE%NC_ATT(0:NCFILE%iNumberOfAttributes - 1), stat=iStat )
  call assert(iStat == 0, "Could not allocate memory for NC_ATT member within NC_FILE defined type", &
    trim(__FILE__), __LINE__)

  do iIndex=0, NCFILE%iNumberOfAttributes - 1
    pNC_ATT => NCFILE%NC_ATT(iIndex)

    call netcdf_populate_attribute_struct( NCFILE=NCFILE, pNC_ATT=pNC_ATT, &
      iNC_VarID=NC_GLOBAL, iAttNum=iIndex )

  enddo

end subroutine netcdf_populate_variable_struct

!----------------------------------------------------------------------

subroutine netcdf_populate_attribute_struct( NCFILE, pNC_ATT, iNC_VarID, iAttNum )

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE
  type (T_NETCDF_ATTRIBUTE), pointer :: pNC_ATT
  integer (kind=c_int) :: iNC_VarID
  integer (kind=c_int) :: iAttNum

  ![ LOCALS ]
  integer (kind=T_INT) :: iStat
  character (len=256) :: sVarName
  character (len=256) :: sAttName
  character (len=512) :: sAttValue
  integer (kind=T_INT) :: iIndex
  integer (kind=c_int), dimension(0:25) :: iAttValue
  integer (kind=c_short), dimension(0:25) :: i2AttValue
  real (kind=c_float), dimension(0:25) :: cfAttValue
  real (kind=c_double), dimension(0:25) :: cdAttValue

  call nc_trap( nc_inq_attname(ncid=NCFILE%iNCID, &
    varid=iNC_VarID, &
    attnum=iAttNum, &
    name=sAttName) )

  pNC_ATT%sAttributeName = c_to_fortran_string(sAttName)

  call nc_trap( nc_inq_att(ncid=NCFILE%iNCID, &
    varid=iNC_VarID, &
    name=sAttName, &
    xtypep=pNC_ATT%iNC_AttType, &
    lenp=pNC_ATT%iNC_AttSize) )

  select case(pNC_ATT%iNC_AttType)

    case (NC_CHAR)

      sAttValue = repeat(" ", 512)

      call nc_trap( nc_get_att_text(ncid=NCFILE%iNCID, &
        varid=iNC_VarID, &
        name=sAttName, &
        ip=sAttValue) )

        pNC_ATT%sAttributeValue = c_to_fortran_string(sAttValue)

    case (NC_SHORT)

      call nc_trap( nc_get_att_short(ncid=NCFILE%iNCID, &
        varid=iNC_VarID, &
        name=sAttName, &
        ip=i2AttValue) )

      write(pNC_ATT%sAttributeValue, fmt="(20(i0, 1x))" ) &
        (i2AttValue(iIndex), iIndex=0, pNC_ATT%iNC_AttSize-1)

    case (NC_INT)

      call nc_trap( nc_get_att_int(ncid=NCFILE%iNCID, &
        varid=iNC_VarID, &
        name=sAttName, &
        ip=iAttValue) )

      write(pNC_ATT%sAttributeValue, fmt="(20(i0,1x))" ) &
        (iAttValue(iIndex), iIndex=0, pNC_ATT%iNC_AttSize-1)

    case (NC_FLOAT)

      call nc_trap( nc_get_att_float(ncid=NCFILE%iNCID, &
        varid=iNC_VarID, &
        name=sAttName, &
        ip=cfAttValue) )

      write(pNC_ATT%sAttributeValue, fmt="(20(f16.6, 1x))" ) &
        (cfAttValue(iIndex), iIndex=0, pNC_ATT%iNC_AttSize-1)

    case (NC_DOUBLE)

      call nc_trap( nc_get_att_double(ncid=NCFILE%iNCID, &
        varid=iNC_VarID, &
        name=sAttName, &
        ip=cdAttValue) )

      write(pNC_ATT%sAttributeValue, fmt="(20(f16.6, 1x))" ) &
        (cdAttValue(iIndex), iIndex=0, pNC_ATT%iNC_AttSize-1)

    case default

  end select

end subroutine netcdf_populate_attribute_struct

!----------------------------------------------------------------------

function netcdf_get_variable_short(NCFILE, iNC_VarID, iNC_Start, iNC_Count, &
   iNC_Stride )    result(iNC_Vars)

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE
  integer (kind=c_int) :: iNC_VarID
  integer (kind=T_INT), dimension(:) :: iNC_Start
  integer (kind=T_INT), dimension(:) :: iNC_Count
  integer (kind=T_INT), dimension(:) :: iNC_Stride
  integer (kind=c_short), dimension(:), allocatable :: iNC_Vars

  type (c_ptr) :: pCount, pStart, pStride
  integer (kind=c_size_t), dimension(size(iNC_Start)), target :: tNC_Start
  integer (kind=c_size_t), dimension(size(iNC_Count)), target :: tNC_Count
  integer (kind=c_ptrdiff_t), dimension(size(iNC_Stride)), target :: tNC_Stride

  allocate(iNC_Vars(product(iNC_Count)))

  tNC_Start = iNC_Start
  tNC_Count = iNC_Count
  tNC_Stride = iNC_Stride

  pStart = c_loc(tNC_Start(1))
  pCount = c_loc(tNC_Count(1))
  pStride = c_loc(tNC_Stride(1))

  call nc_trap(nc_get_vars_short(ncid=NCFILE%iNCID, &
       varid=iNC_VarID, &
       startp=pStart, &
       countp=pCount, &
       stridep=pStride, &
       vars=iNC_Vars) )

end function netcdf_get_variable_short

!----------------------------------------------------------------------

function netcdf_get_variable_double(NCFILE, iNC_VarID, iNC_Start, iNC_Count, &
   iNC_Stride )    result(dpNC_Vars)

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE
  integer (kind=c_int) :: iNC_VarID
  integer (kind=c_int), dimension(:) :: iNC_Start
  integer (kind=c_size_t), dimension(:) :: iNC_Count
  integer (kind=c_int), dimension(:) :: iNC_Stride
  real (kind=c_double), dimension(:), allocatable :: dpNC_Vars

  type (c_ptr) :: pCount, pStart, pStride
  integer (kind=c_size_t), dimension(size(iNC_Start)), target :: tNC_Start
  integer (kind=c_size_t), dimension(size(iNC_Count)), target :: tNC_Count
  integer (kind=c_ptrdiff_t), dimension(size(iNC_Stride)), target :: tNC_Stride

  allocate(dpNC_Vars(product(iNC_Count)))

  tNC_Start = iNC_Start
  tNC_Count = iNC_Count
  tNC_Stride = iNC_Stride

  pStart = c_loc(tNC_Start(1))
  pCount = c_loc(tNC_Count(1))
  pStride = c_loc(tNC_Stride(1))

  call nc_trap(nc_get_vars_double(ncid=NCFILE%iNCID, &
       varid=iNC_VarID, &
       startp=pStart, &
       countp=pCount, &
       stridep=pStride, &
       vars=dpNC_Vars) )

end function netcdf_get_variable_double

!----------------------------------------------------------------------

subroutine netcdf_dump_cdl(NCFILE, iLU)

  type (T_NETCDF4_FILE ) :: NCFILE
  type (T_NETCDF_ATTRIBUTE), pointer :: pNC_ATT
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  type (T_NETCDF_DIMENSION), pointer :: pNC_DIM
  integer :: iLU
  character (len=256) :: sBuf
  character (len=256) :: sDimName
  integer (kind=T_INT) :: iDimID
  integer (kind=T_INT) :: iUbound

  integer :: iResult, iIndex, iIndex2, iIndex3

  sBuf=""

  write(unit=iLU, fmt="(a)") "netcdf "//trim(NCFILE%sFilename)//" {"
  write(unit=iLU, fmt="(a)") "  dimensions:"

  do iIndex = 0, NCFILE%iNumberOfDimensions - 1
    write(unit=iLU, fmt="(4x,a, ' = ', i0, ';')") trim(NCFILE%NC_DIM(iIndex)%sDimensionName), &
      NCFILE%NC_DIM(iIndex)%iNC_DimSize
  enddo

  do iIndex = 0, NCFILE%iNumberOfVariables - 1

    pNC_VAR => NCFILE%NC_VAR(iIndex)

    if(pNC_VAR%iNumberOfDimensions > 0) then

      sBuf = ' ('

      iUbound = pNC_VAR%iNumberOfDimensions - 1
      do iIndex3 = 0, iUbound

        iDimID = pNC_VAR%iNC_DimID(iIndex3)

        call assert(iDimID >=0 .and. &
          iDimID <= ubound( NCFILE%NC_DIM, 1 ), &
          "INTERNAL PROGRAMMING ERROR -- iDimID out of bounds", &
          trim(__FILE__), __LINE__)

        pNC_DIM => NCFILE%NC_DIM(iDimID)
        sDimName = pNC_DIM%sDimensionName

        sBuf = trim(sBuf)//trim(pNC_DIM%sDimensionName)//"=" &
               //trim(asCharacter(pNC_DIM%iNC_DimSize))

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

      pNC_ATT => NCFILE%NC_VAR(iIndex)%NC_ATT(iIndex3)

      if(pNC_ATT%iNC_AttType == NC_CHAR) then

        sBuf = trim(pNC_VAR%sVariableName)//":"//trim(pNC_ATT%sAttributeName ) &
             //" = "//trim(pNC_ATT%sAttributeValue ) &
             //"; // "//trim(NETCDF_DATA_TYPE(pNC_ATT%iNC_AttType) )

      else

        sBuf = trim(pNC_VAR%sVariableName)//":"//trim(pNC_ATT%sAttributeName ) &
             //" = "//trim(asCSV(pNC_ATT%sAttributeValue ) ) &
             //"; // "//trim(NETCDF_DATA_TYPE(pNC_ATT%iNC_AttType) )

      endif

      write(unit=iLU, fmt="(4x,a)") trim(sBuf)

    enddo

  enddo

  do iIndex = 0, NCFILE%iNumberOfAttributes - 1

    pNC_ATT => NCFILE%NC_ATT(iIndex)

    if(pNC_ATT%iNC_AttType == NC_CHAR) then

      sBuf = ":"//trim(pNC_ATT%sAttributeName)//" = " &
           //trim(pNC_ATT%sAttributeValue )

    else

      sBuf = ":"//trim(pNC_ATT%sAttributeName)//" = " &
           //trim(asCSV(pNC_ATT%sAttributeValue) )

    endif

    write(unit=iLU, fmt="(a)") trim(sBuf)

  enddo

  write(unit=iLU, fmt="(a,/,/)") "}"


end subroutine netcdf_dump_cdl

!----------------------------------------------------------------------

function netcdf_get_first_and_last(NCFILE, iVarIndex)  result(dpValues)

  type (T_NETCDF4_FILE ), pointer :: NCFILE
  integer (kind=c_int) :: iVarIndex
  real (kind=c_double), dimension(2) :: dpValues

  ! [ LOCALS ]
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  type (T_NETCDF_DIMENSION), pointer :: pNC_DIM
  integer (kind=T_INT) :: iDimSize
  integer (kind=T_INT) :: iDimIndex
  integer (kind=c_short), dimension(2) :: spValues
  integer (kind=c_int), dimension(2) :: ipValues
  real (kind=c_float), dimension(2) :: rpValues

  call assert (iVarIndex >= lbound(NCFILE%NC_VAR,1) &
    .and. iVarIndex <= ubound(NCFILE%NC_VAR,1), &
      "INTERNAL PROGRAMMING ERROR - index out of bounds NC_FILE%NC_VAR" &
      //"~Offending index value: "//trim(asCharacter(iVarIndex)), &
      trim(__FILE__), __LINE__)

  pNC_VAR => NCFILE%NC_VAR(iVarIndex)

  iDimIndex = netcdf_return_DimIndex( NCFILE, pNC_VAR%iNC_DimID(0) )

  pNC_DIM => NCFILE%NC_DIM( iDimIndex )
  iDimSize = pNC_DIM%iNC_DimSize

  select case (pNC_VAR%iNC_VarType )

    case (NC_SHORT)

      spValues = netcdf_get_variable_short(NCFILE=NCFILE, &
        iNC_VarID=pNC_VAR%iNC_VarID, &
        iNC_Start=[0], &
        iNC_Count=[2], &
        iNC_Stride=[iDimSize-1] )

      dpValues = real(spValues, kind=c_double)

    case (NC_INT)

    case (NC_FLOAT)

    case (NC_DOUBLE)

      dpValues = netcdf_get_variable_double(NCFILE=NCFILE, &
        iNC_VarID=pNC_VAR%iNC_VarID, &
        iNC_Start=[0], &
        iNC_Count=[2_c_size_t], &
        iNC_Stride=[iDimSize-1] )

    case default

  end select

end function netcdf_get_first_and_last

!----------------------------------------------------------------------

subroutine netcdf_calculate_time_range(NCFILE)

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE

  NCFILE%iOriginJD = julian_day(NCFILE%iOriginYear, &
    NCFILE%iOriginMonth, NCFILE%iOriginDay)

  NCFILE%iFirstDayJD = NCFILE%iOriginJD + NCFILE%dpFirstAndLastTimeValues(1)
  NCFILE%iLastDayJD = NCFILE%iOriginJD + NCFILE%dpFirstAndLastTimeValues(2)

end subroutine netcdf_calculate_time_range

!----------------------------------------------------------------------

subroutine netcdf_get_time_units(NCFILE)

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE

  ! [ LOCALS ]
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  character (len=256) :: sDateTime
  character (len=256) :: sItem
  integer (kind=T_INT) :: iIndex
  logical (kind=T_LOGICAL) :: lFound
  integer (kind=T_INT) :: iStat

  call assert(NCFILE%iVarID(NC_TIME) >= 0, "INTERNAL PROGRAMMING ERROR -- " &
    //"netcdf_get_time_units must be called only after a call is made to ~" &
    //"netcdf_get_variable_ids", trim(__FILE__), __LINE__)

  pNC_VAR => NCFILE%NC_VAR(NCFILE%iVarID(NC_TIME) )

  lFound = lFALSE

  do iIndex=0, pNC_VAR%iNumberOfAttributes - 1

    if ( str_compare(pNC_VAR%NC_ATT(iIndex)%sAttributeName, "units") ) then
      lFound = lTRUE
      exit
    endif

  enddo

  call assert (lFound, "Failed to find the 'units' attribute associated " &
    //"with time variable "//dquote(pNC_VAR%sVariableName), &
    trim(__FILE__), __LINE__)

  sDateTime = pNC_VAR%NC_ATT(iIndex)%sAttributeValue

  call chomp(sDateTime, sItem)  ! sItem should be "days"
  call chomp(sDateTime, sItem)  ! sItem should be "since"
  call chomp(sDateTime, sItem)  ! sItem should be date as YYYY-MM-DD
  read(sItem,FMT="(i4,1x,i2,1x,i2)", iostat=iStat) &
    NCFILE%iOriginYear, NCFILE%iOriginMonth, NCFILE%iOriginDay
  call chomp(sDateTime, sItem)  ! sItem should be time as HH:MM:SS
  read(sItem,FMT="(i4,1x,i2,1x,i2)", iostat=iStat) &
    NCFILE%iOriginHH, NCFILE%iOriginMM, NCFILE%iOriginSS

end subroutine netcdf_get_time_units

!----------------------------------------------------------------------

subroutine netcdf_get_variable_id_and_type( NCFILE, sX_VarName, sY_VarName, &
   sZ_VarName, sTime_VarName)

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE

  character (len=*) :: sX_VarName
  character (len=*) :: sY_VarName
  character (len=*) :: sZ_VarName
  character (len=*) :: sTime_VarName

   ! [ LOCALS ]
   integer (kind=T_INT) :: iIndex
   type (T_NETCDF_VARIABLE), pointer :: pNC_VAR

   NCFILE%iVarID = -9999

   print *, trim(__FILE__), __LINE__

   do iIndex=0, NCFILE%iNumberOfVariables - 1

     pNC_VAR => NCFILE%NC_VAR(iIndex)

     if (str_compare(pNC_VAR%sVariableName, sX_VarName) ) then
       NCFILE%iVarIndex(NC_X) = iIndex
       NCFILE%iVarID(NC_X) = pNC_VAR%iNC_VarID
       NCFILE%iVarType(NC_X) = pNC_VAR%iNC_VarType
       NCFILE%iVar_DimID(NC_X,:) = pNC_VAR%iNC_DimID

     elseif (str_compare(pNC_VAR%sVariableName, sY_VarName) ) then
       NCFILE%iVarIndex(NC_Y) = iIndex
       NCFILE%iVarID(NC_Y) = pNC_VAR%iNC_VarID
       NCFILE%iVarType(NC_Y) = pNC_VAR%iNC_VarType
       NCFILE%iVar_DimID(NC_Y,:) = pNC_VAR%iNC_DimID

     elseif (str_compare(pNC_VAR%sVariableName, sZ_VarName) ) then
       NCFILE%iVarIndex(NC_Z) = iIndex
       NCFILE%iVarID(NC_Z) = pNC_VAR%iNC_VarID
       NCFILE%iVarType(NC_Z) = pNC_VAR%iNC_VarType
       NCFILE%iVar_DimID(NC_Z,:) = pNC_VAR%iNC_DimID

     elseif (str_compare(pNC_VAR%sVariableName, sTime_VarName) ) then
       NCFILE%iVarIndex(NC_TIME) = iIndex
       NCFILE%iVarID(NC_TIME) = pNC_VAR%iNC_VarID
       NCFILE%iVarType(NC_TIME) = pNC_VAR%iNC_VarType
       NCFILE%iVar_DimID(NC_TIME,:) = pNC_VAR%iNC_DimID
     endif

     write(*, fmt="(i2.2,a12,1x,10(i2,1x))"), iIndex, dquote(pNC_VAR%sVariableName), pNC_VAR%iNC_VarID, pNC_VAR%iNC_VarType, pNC_VAR%iNC_DimID


   enddo

   print *, trim(__FILE__), __LINE__

   call assert(NCFILE%iVarID(NC_X) >= 0, &
     "Unable to find the variable named "//dquote(sX_VarName)//" in " &
     //"file "//dquote(NCFILE%sFilename), trim(__FILE__), __LINE__)

   call assert(NCFILE%iVarID(NC_Y) >= 0, &
     "Unable to find the variable named "//dquote(sY_VarName)//" in " &
     //"file "//dquote(NCFILE%sFilename), trim(__FILE__), __LINE__)

   call assert(NCFILE%iVarID(NC_Z) >= 0, &
     "Unable to find the variable named "//dquote(sZ_VarName)//" in " &
     //"file "//dquote(NCFILE%sFilename), trim(__FILE__), __LINE__)

   call assert(NCFILE%iVarID(NC_TIME) >= 0, &
     "Unable to find the variable named "//dquote(sTime_VarName)//" in " &
     //"file "//dquote(NCFILE%sFilename), trim(__FILE__), __LINE__)

end subroutine netcdf_get_variable_id_and_type

!----------------------------------------------------------------------

function netcdf_get_index_double(NCFILE, iNC_VarID, dpValue)  result(iIndex)

  type (T_NETCDF4_FILE ), pointer :: NCFILE
  integer (kind=c_int) :: iNC_VarID
  real (kind=c_double), dimension(0:1) :: dpValue
  integer (kind=c_size_t), dimension(0:1) :: iIndex
  integer (kind=c_size_t) :: iCount
  integer (kind=T_INT) :: i, j

  ! [ LOCALS ]
  real (kind=c_double), dimension(:), allocatable :: dpValues
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  real (kind=c_double) :: dpMin, dpDiff

  call assert(iNC_VarID >=0 .and. iNC_VarID <= ubound(NCFILE%NC_VAR, 1), &
    "INTERNAL PROGRAMMING ERROR? - iNC_VarID out of bounds", &
    trim(__FILE__), __LINE__)

  pNC_VAR => NCFILE%NC_VAR( &
           netcdf_return_VarIndex( NCFILE=NCFILE, iVarID=iNC_VarID) )

  iCount = NCFILE%NC_DIM(pNC_VAR%iNC_DimID(0))%iNC_DimSize

  allocate(dpValues(iCount))

  dpValues = netcdf_get_variable_double(NCFILE=NCFILE, &
         iNC_VarID=iNC_VarID, &
         iNC_Start=[0], &
         iNC_Count=[iCount], &
         iNC_Stride=[1])

  iIndex = -999

!  call assert(dpValue(0) >= minval(dpValues) .and. dpValue(0) <= maxval(dpValues), &
!    "Value is out of range. It is not possible to calculate a meaningful index value.", &
!      trim(__FILE__), __LINE__)

!  call assert(dpValue(1) >= minval(dpValues) .and. dpValue(1) <= maxval(dpValues), &
!    "Value is out of range. It is not possible to calculate a meaningful index value.", &
!      trim(__FILE__), __LINE__)

  print *, "dpValue(0:1): ", dpValue(0), dpValue(1)
  print *, "dpValues (min/max): ", minval(dpValues), maxval(dpValues)


  dpMin = 1.e20

  do j=0,1

    do i=lbound(dpValues, 1), ubound(dpValues, 1)

      dpDiff = (dpValues(i) - dpValue(j))**2
      if(dpDiff < dpMin) then
        dpMin = dpDiff
        iIndex(j) = i
      endif

    enddo

  enddo

 deallocate(dpValues)

end function netcdf_get_index_double

!----------------------------------------------------------------------

function netcdf_get_varid(NCFILE, sVariableName)  result(iNC_VarID)

  type (T_NETCDF4_FILE ) :: NCFILE
  character (len=*) :: sVariableName
  integer (kind=c_int) :: iNC_VarID

  integer (kind=T_INT) :: iIndex
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR

  iNC_VarID = -9999

  do iIndex=0, NCFILE%iNumberOfVariables - 1

    pNC_VAR => NCFILE%NC_VAR(iIndex)

    if(trim(sVariableName) .eq. trim(pNC_VAR%sVariableName) ) then

      iNC_VarID = iIndex
      exit

    endif

  enddo

end function netcdf_get_varid

#endif

end module netcdf4_support
