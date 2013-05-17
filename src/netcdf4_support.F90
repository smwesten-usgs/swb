!> @file
!> @brief  Contains a single module, @ref netcdf4_support, which
!>  provides support for use of NetCDF files as input or output.

!> @brief Provides support for use of NetCDF files as input for time-varying,
!>  gridded meteorlogic data, or output for any SWB-generated variable.
module netcdf4_support

#ifdef NETCDF_SUPPORT

  use iso_c_binding
  use types
  use stats, only : stats_WriteMinMeanMax
  use swb_grid
  use typesizes
  use netcdf_c_api_interfaces

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

  integer (kind=c_int), parameter :: LEFT = 0
  integer (kind=c_int), parameter :: RIGHT = 1
  integer (kind=c_int), parameter :: BOTTOM = 0
  integer (kind=c_int), parameter :: TOP = 1

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
    logical (kind=c_bool) :: lUnlimited = lFALSE
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
    character (len=256) :: sFilename
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
    integer (kind=c_int) :: lLeapYearTreatment
    integer (kind=c_size_t), dimension(0:3) :: iStart
    integer (kind=c_size_t), dimension(0:3) :: iCount
    integer (kind=c_size_t), dimension(0:3) :: iStride = 1
    integer (kind=c_size_t), dimension(0:1) :: iColBounds
    integer (kind=c_size_t), dimension(0:1) :: iRowBounds
    integer (kind=c_int) :: iNX
    integer (kind=c_int) :: iNY
    real (kind=c_double), dimension(0:1) :: rX
    real (kind=c_double), dimension(0:1) :: rY
    logical (kind=c_bool) :: lX_IncreasesWithIndex = lTRUE
    logical (kind=c_bool) :: lY_IncreasesWithIndex = lFALSE

    real (kind=c_double), dimension(0:1) :: dpFirstAndLastTimeValues
    integer (kind=c_int), dimension(0:3) :: iVarID = -9999
    integer (kind=c_int), dimension(0:3) :: iVarIndex = -9999
    integer (kind=c_int), dimension(0:3) :: iVarType = -9999
    character (len=256), dimension(0:3) :: sVarUnits = "NA"
    integer (kind=c_int), dimension(0:3, 0:3) :: iVar_DimID = -9999

    real (kind=c_double), allocatable, dimension(:) :: rX_Coords
    real (kind=c_double), allocatable, dimension(:) :: rY_Coords
    real (kind=c_double) :: rGridCellSizeX
    real (kind=c_double) :: rGridCellSizeY

    type (T_NETCDF_DIMENSION), dimension(:), pointer :: NC_DIM
    type (T_NETCDF_VARIABLE), dimension(:), pointer :: NC_VAR
    type (T_NETCDF_ATTRIBUTE), dimension(:), pointer :: NC_ATT
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


contains

!----------------------------------------------------------------------

function netcdf_date_within_range( NCFILE, iJulianDay)  result( lWithinRange )

  type (T_NETCDF4_FILE ), pointer :: NCFILE
  integer (kind=c_int) :: iJulianDay
  logical (kind=c_bool) :: lWithinRange

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
  integer (kind=c_int) :: iJulianDay
  integer (kind=c_size_t) :: iStart

  iStart = int(iJulianDay - NCFILE%iFirstDayJD, kind=c_size_t)

  call assert(iStart >=0, "Problem finding the index number of the time " &
    //"variable in NetCDF file "//dquote(NCFILE%sFilename), trim(__FILE__), __LINE__)


end function netcdf_date_to_index

!----------------------------------------------------------------------

function netcdf_return_VarID( NCFILE, iVarIndex)   result(iVarID)

  type (T_NETCDF4_FILE ), pointer :: NCFILE
   integer (kind=c_int) :: iVarIndex
   integer (kind=c_int) :: iVarID

   type (T_NETCDF_VARIABLE), pointer :: pNC_VAR

   pNC_VAR => NCFILE%NC_VAR(iVarIndex)

   iVarID = pNC_VAR%iNC_VarID

end function netcdf_return_VarID

!----------------------------------------------------------------------

function netcdf_return_DimID( NCFILE, iDimIndex)   result(iDimID)

  type (T_NETCDF4_FILE ), pointer :: NCFILE
   integer (kind=c_int) :: iDimIndex
   integer (kind=c_int) :: iDimID

   type (T_NETCDF_DIMENSION), pointer :: pNC_DIM

   pNC_DIM => NCFILE%NC_DIM(iDimIndex)

   iDimID = pNC_DIM%iNC_DimID

end function netcdf_return_DimID

!----------------------------------------------------------------------

function netcdf_return_VarIndex( NCFILE, iVarID)   result(iVarIndex)

  type (T_NETCDF4_FILE ), pointer :: NCFILE
   integer (kind=c_int) :: iVarID
   integer (kind=c_int) :: iVarIndex

   type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
   integer (kind=c_int) :: iIndex
   logical (kind=c_bool) :: lFound

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
   integer (kind=c_int) :: iVarIndex
   character (len=*) :: sAttName
   character (len=256) :: sAttValue

   type (T_NETCDF_ATTRIBUTE), dimension(:), pointer :: pNC_ATT
   integer (kind=c_int) :: iIndex
   logical (kind=c_bool) :: lFound

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
   integer (kind=c_int) :: iDimID
   integer (kind=c_int) :: iDimIndex

   type (T_NETCDF_DIMENSION), pointer :: pNC_DIM
   integer (kind=c_int) :: iIndex
   logical (kind=c_bool) :: lFound

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

function nc_return_DimSize( NCFILE, iDimID)   result(iDimSize)

  type (T_NETCDF4_FILE ), pointer :: NCFILE
   integer (kind=c_int) :: iDimID
   integer (kind=c_size_t) :: iDimSize

   type (T_NETCDF_DIMENSION), pointer :: pNC_DIM
   integer (kind=c_int) :: iIndex
   logical (kind=c_bool) :: lFound

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

end function nc_return_DimSize

!----------------------------------------------------------------------

function return_start_indices(NCFILE, iJulianDay)  result( iIndices)

  type (T_NETCDF4_FILE ), pointer :: NCFILE
  integer (kind=c_int) :: iJulianDay

  integer (kind=c_int), dimension(:), allocatable :: iIndices





end function return_start_indices

!----------------------------------------------------------------------

subroutine netcdf_open_and_prepare(NCFILE, sFilename, sVarName_x, &
    sVarName_y, sVarName_z, sVarName_time, tGridBounds, iLU)

  type (T_NETCDF4_FILE ), pointer :: NCFILE
  character (len=*) :: sFilename
  character (len=*), optional :: sVarName_x
  character (len=*), optional :: sVarName_y
  character (len=*), optional :: sVarName_z
  character (len=*), optional :: sVarName_time
  type (T_GRID_BOUNDS), optional :: tGridBounds
  integer (kind=c_int), optional :: iLU

  ! [ LOCALS ]
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  type (T_NETCDF_DIMENSION), pointer :: pNC_DIM
  logical (kind=c_bool) :: lFileOpen
  character (len=256) :: sX_VarName
  character (len=256) :: sY_VarName
  character (len=256) :: sZ_VarName
  character (len=256) :: sTime_VarName
  integer (kind=c_int), dimension(2) :: iColRow_ll, iColRow_ur, iColRow_lr, iColRow_ul
  integer (kind=c_int) :: iColmin, iColmax, iRowmin, iRowmax
  integer (kind=c_int) :: iIndex

  call netcdf_open_file(NCFILE=NCFILE, sFilename=sFilename)

  call nc_populate_dimension_struct( NCFILE )
  call nc_populate_variable_struct( NCFILE )

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

  call nc_get_time_units(NCFILE=NCFILE)

  call netcdf_calculate_time_range(NCFILE)

  !> retrieve the X and Y coordinates from the NetCDF file...
  call nc_get_x_and_y(NCFILE)

  print *, "X-coords from NetCDF file: ", minval(NCFILE%rX_Coords), maxval(NCFILE%rX_Coords)
  print *, "Y-coords from NetCDF file: ", minval(NCFILE%rY_Coords), maxval(NCFILE%rY_Coords)

  if (present(tGridBounds) ) then

    !> define a subset of the grid as the AOI
    !> need all four corner points since it is likely that
    !> the AOI rectangle is rotated relative to the base
    !> projection
    iColRow_ll = nc_coord_to_col_row(NCFILE=NCFILE, &
                                     rX=tGridBounds%rXll, &
                                     rY=tGridBounds%rYll)

    iColRow_lr = nc_coord_to_col_row(NCFILE=NCFILE, &
                                     rX=tGridBounds%rXlr, &
                                     rY=tGridBounds%rYlr)

    iColRow_ul = nc_coord_to_col_row(NCFILE=NCFILE, &
                                     rX=tGridBounds%rXul, &
                                     rY=tGridBounds%rYul)

    iColRow_ur = nc_coord_to_col_row(NCFILE=NCFILE, &
                                     rX=tGridBounds%rXur, &
                                     rY=tGridBounds%rYur)

    write(*, fmt="(a,a,i6)") "Find correspondence between project bounds (in native projection) and row, col of dataset |", &
      trim(__FILE__), __LINE__
    write(*, fmt="(a)") "      column     row              X              Y"
    write(*, fmt="(a,i6,i6,a,f14.3,f14.3)") "LL: ", iColRow_ll(COLUMN), iColRow_ll(ROW), " <==> ", tGridBounds%rXll, tGridBounds%rYll
    write(*, fmt="(a,i6,i6,a,f14.3,f14.3)") "LR: ", iColRow_lr(COLUMN), iColRow_lr(ROW), " <==> ", tGridBounds%rXlr, tGridBounds%rYlr
    write(*, fmt="(a,i6,i6,a,f14.3,f14.3)") "UL: ", iColRow_ul(COLUMN), iColRow_ul(ROW), " <==> ", tGridBounds%rXul, tGridBounds%rYul
    write(*, fmt="(a,i6,i6,a,f14.3,f14.3)") "UR: ", iColRow_ur(COLUMN), iColRow_ur(ROW), " <==> ", tGridBounds%rXur, tGridBounds%rYur

    NCFILE%iColBounds(FIRST) = &
      max( min( iColRow_ul(COLUMN), iColRow_ur(COLUMN), iColRow_ll(COLUMN), iColRow_lr(COLUMN) ) - 4, &
                lbound(NCFILE%rX_Coords,1) )

    NCFILE%iColBounds(LAST) = &
      min( max( iColRow_ul(COLUMN), iColRow_ur(COLUMN), iColRow_ll(COLUMN), iColRow_lr(COLUMN) ) + 4, &
                ubound(NCFILE%rX_Coords,1) )


      NCFILE%iRowBounds(FIRST) = &
        max( min( iColRow_ul(ROW), iColRow_ur(ROW), iColRow_ll(ROW), iColRow_lr(ROW) ) - 4, &
                  lbound(NCFILE%rY_Coords,1) )

      NCFILE%iRowBounds(LAST) = &
        min( max( iColRow_ul(ROW), iColRow_ur(ROW), iColRow_ll(ROW), iColRow_lr(ROW) ) + 4, &
                  ubound(NCFILE%rY_Coords,1) )

  else

    !> define the entire grid area as the AOI
    NCFILE%iColBounds(FIRST) = lbound(NCFILE%rX_Coords,1)
    NCFILE%iColBounds(LAST) = ubound(NCFILE%rX_Coords,1)

    NCFILE%iRowBounds(FIRST) = lbound(NCFILE%rY_Coords,1)
    NCFILE%iRowBounds(LAST) = ubound(NCFILE%rY_Coords,1)

    print *, trim(__FILE__), __LINE__
    print *, NCFILE%iColBounds(FIRST), NCFILE%iRowBounds(FIRST)
    print *, NCFILE%iColBounds(LAST), NCFILE%iRowBounds(LAST)

  endif

  call nc_set_start_count_stride(NCFILE)

  !> now that we have (possibly) created a subset, need to get the
  !> **NATIVE** coordinate bounds so that the intermediate grid file
  !> can be created
  call nc_return_native_coord_bounds(NCFILE)

!  write(*,fmt="('iStart: ',10(i6,1x))") NCFILE%iStart
!  write(*,fmt="('iCount: ',10(i6,1x))") NCFILE%iCount
!  write(*,fmt="('iStride: ',10(i6,1x))") NCFILE%iStride

end subroutine netcdf_open_and_prepare

subroutine netcdf_update_time_range(NCFILE)

  type (T_NETCDF4_FILE ), pointer :: NCFILE

  NCFILE%dpFirstAndLastTimeValues = netcdf_get_first_and_last(NCFILE=NCFILE, &
      iVarIndex=NCFILE%iVarIndex(NC_TIME) )

  call nc_get_time_units(NCFILE)

  call netcdf_calculate_time_range(NCFILE)

end subroutine netcdf_update_time_range

!----------------------------------------------------------------------

subroutine nc_set_start_count_stride(NCFILE)

  type (T_NETCDF4_FILE ), pointer :: NCFILE

  ! [ LOCALS ]
  integer (kind=c_int) :: iIndex

  ! loop over the three (assumed) dimensions of the "Z" variable;
  ! assign appropriate bounds to each
  do iIndex = 0,3

    select case (iIndex)

      case (NC_X)

        !> need to subtract 1 from the start index: we're using the
        !> NetCDF C API, in which index values are relative to zero
        NCFILE%iStart(iIndex) = minval(NCFILE%iColBounds) - 1
        NCFILE%iNX = maxval(NCFILE%iColBounds) - minval(NCFILE%iColBounds) + 1
        NCFILE%iCount(iIndex) = maxval(NCFILE%iColBounds) - minval(NCFILE%iColBounds)
        NCFILE%iStride(iIndex) = 1_c_size_t

      case (NC_Y)

        !> note: this assumes that the row numbers increase from top to bottom,
        !>       while the Y coordinates decrease top to bottom

        NCFILE%iStart(iIndex) = minval(NCFILE%iRowBounds) - 1
        NCFILE%iNY = maxval(NCFILE%iRowBounds) - minval(NCFILE%iRowBounds) + 1
        NCFILE%iCount(iIndex) = maxval(NCFILE%iRowBounds) - minval(NCFILE%iRowBounds)
        NCFILE%iStride(iIndex) = 1_c_size_t

      case (NC_TIME)

        NCFILE%iStart(iIndex) = 0_c_size_t
        NCFILE%iCount(iIndex) = 1_c_size_t
        NCFILE%iStride(iIndex) = 1_c_size_t

      case default

    end select

  enddo

end subroutine nc_set_start_count_stride

!----------------------------------------------------------------------

subroutine nc_return_native_coord_bounds(NCFILE)

  type (T_NETCDF4_FILE ), pointer :: NCFILE

  ! [ LOCALS ]
  real (kind=c_double) :: rXmin, rXmax
  real (kind=c_double) :: rYmin, rYmax

  rXmin = minval(NCFILE%rX_Coords(NCFILE%iColBounds(FIRST):NCFILE%iColBounds(LAST)) )
  rXmax = maxval(NCFILE%rX_Coords(NCFILE%iColBounds(FIRST):NCFILE%iColBounds(LAST)) )
  rYmin = minval(NCFILE%rY_Coords(NCFILE%iRowBounds(FIRST):NCFILE%iRowBounds(LAST)) )
  rYmax = maxval(NCFILE%rY_Coords(NCFILE%iRowBounds(FIRST):NCFILE%iRowBounds(LAST)) )

  NCFILE%rX(LEFT) = rXmin - NCFILE%rGridCellSizeX * dpHALF
  NCFILE%rX(RIGHT) = rXmax + NCFILE%rGridCellSizeX * dpHALF
  NCFILE%rY(TOP) = rYmax + NCFILE%rGridCellSizeY * dpHALF
  NCFILE%rY(BOTTOM) = rYmin - NCFILE%rGridCellSizeY * dpHALF

  print *, "Grid cell size (X): ", NCFILE%rGridCellSizeX
  print *, "Grid cell size (Y): ", NCFILE%rGridCellSizeY

  print *, "Bounds of data subset area, in native coordinates"
  print *, "X (left): ", NCFILE%rX(LEFT)
  print *, "X (right): ", NCFILE%rX(RIGHT)
  print *, "Y (top): ", NCFILE%rY(TOP)
  print *, "Y (bottom): ", NCFILE%rY(BOTTOM)

end subroutine nc_return_native_coord_bounds

!----------------------------------------------------------------------

subroutine nc_get_x_and_y(NCFILE)

  type (T_NETCDF4_FILE ), pointer :: NCFILE

  integer (kind=c_int) :: iVarIndex_x, iVarIndex_y
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR_x, pNC_VAR_y
  type (T_NETCDF_DIMENSION), pointer :: pNC_DIM_x, pNC_DIM_y
  integer (kind=c_int) :: iLowerBound, iUpperBound
  integer (kind=c_int) :: iStat

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

  allocate( NCFILE%rX_Coords( pNC_DIM_x%iNC_DimSize ), stat=iStat )
  call assert(iStat==0, "Failed to allocate memory for X-coordinate values", &
    trim(__FILE__), __LINE__)

  allocate (NCFILE%rY_Coords( pNC_DIM_y%iNC_DimSize  ), stat=iStat )
  call assert(iStat==0, "Failed to allocate memory for Y-coordinate values", &
    trim(__FILE__), __LINE__)

  call nc_get_variable_double(NCFILE=NCFILE, &
       iNC_VarID=pNC_VAR_x%iNC_VarID, &
       iNC_Start=[0_c_size_t], &
       iNC_Count=[pNC_DIM_x%iNC_DimSize], &
       iNC_Stride=[1_c_size_t], &
       dpNC_Vars=NCFILE%rX_Coords)

  call nc_get_variable_double(NCFILE=NCFILE, &
       iNC_VarID=pNC_VAR_y%iNC_VarID, &
       iNC_Start=[0_c_size_t], &
       iNC_Count=[pNC_DIM_y%iNC_DimSize], &
       iNC_Stride=[1_c_size_t], &
       dpNC_Vars=NCFILE%rY_Coords)

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

  NCFILE%rGridCellSizeX = ( maxval(NCFILE%rX_Coords) &
                                - minval(NCFILE%rX_Coords) ) &
                                / real (pNC_DIM_x%iNC_DimSize, kind=c_double)

  NCFILE%rGridCellSizeY = ( maxval(NCFILE%rY_Coords) &
                                - minval(NCFILE%rY_Coords) ) &
                                / real (pNC_DIM_y%iNC_DimSize, kind=c_double)

end subroutine nc_get_x_and_y

!----------------------------------------------------------------------

subroutine netcdf_open_file(NCFILE, sFilename, iLU)

  type (T_NETCDF4_FILE ), pointer :: NCFILE
  character (len=*) :: sFilename
  integer (kind=c_int), optional :: iLU

  ! [ LOCALS ]
  logical (kind=c_bool) :: lFileOpen

  call echolog("Attempting to open READONLY NetCDF file: " &
    //dquote(sFilename))

  call nc_trap( nc_open(trim(sFilename)//c_null_char, &
                NC_READONLY, NCFILE%iNCID), __FILE__, __LINE__ )

  call nc_trap( nc_inq_format(ncid=NCFILE%iNCID, formatp=NCFILE%iFileFormat), &
               __FILE__, __LINE__)

  call echolog("   Succeeded.  ncid: "//trim(asCharacter(NCFILE%iNCID)) &
         //"  format: "//trim(NETCDF_FORMAT_STRING(NCFILE%iFileFormat) ) )

  NCFILE%sFilename = sFilename

!  call netcdf_dump_cdl( NCFILE, LU_STD_OUT)

!  NCFILE%dpFirstAndLastTimeValues = netcdf_get_first_and_last(NCFILE=NCFILE, &
!    iVarIndex=NCFILE%iVarIndex(NC_TIME) )

!  call netcdf_calculate_time_range(NCFILE)

  if( present(iLU) ) then
    inquire (unit=iLU, opened=lFileOpen)
    if ( lFileOpen )  call netcdf_dump_cdl( NCFILE, iLU)
  endif

end subroutine netcdf_open_file

!----------------------------------------------------------------------

subroutine nc_trap( iResultCode, sFilename, iLineNumber )

  integer (kind=c_int) :: iResultCode
  character (len=*), optional :: sFilename
  integer (kind=c_int), optional :: iLineNumber

  ! [ LOCALS ]
  type(c_ptr) :: cpResult
  character (len=256) :: sTextString
  character (len=256) :: sFile
  integer (kind=c_int) :: iLine

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

    call echolog("NetCDF ERROR: "//dquote( sTextString  )//" | error code was: " &
      //trim(asCharacter(iResultCode)) )

    call assert(lFALSE, "SWB is stopping due to a problem reading or accessing" &
      //" a NetCDF file", trim(sFile), iLine)

  endif

end subroutine nc_trap

!----------------------------------------------------------------------

subroutine netcdf_close_file( NCFILE)

  type (T_NETCDF4_FILE ), pointer :: NCFILE

  call echolog("Closing NetCDF file with name: "//dquote(NCFILE%sFilename))
  call nc_trap( nc_close(NCFILE%iNCID), __FILE__, __LINE__ )

end subroutine netcdf_close_file

!----------------------------------------------------------------------

subroutine nc_deallocate_data_struct( NCFILE )

  type (T_NETCDF4_FILE ), pointer :: NCFILE

  ! [ LOCALS ]
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  integer (kind=c_int) :: iIndex

  do iIndex=0, NCFILE%iNumberOfVariables - 1

    pNC_VAR => NCFILE%NC_VAR(iIndex)

    if (pNC_VAR%iNumberOfAttributes == 0 ) cycle

    if (associated( pNC_VAR%NC_ATT ))  deallocate( pNC_VAR%NC_ATT )

  enddo

  if (associated( NCFILE%NC_VAR ))  deallocate( NCFILE%NC_VAR )
  if (associated( NCFILE%NC_ATT ))  deallocate( NCFILE%NC_ATT )
  if (associated( NCFILE%NC_DIM ))  deallocate( NCFILE%NC_DIM )
  if (associated( NCFILE ))  deallocate ( NCFILE )

end subroutine nc_deallocate_data_struct

!----------------------------------------------------------------------

subroutine nc_populate_dimension_struct( NCFILE )

  type (T_NETCDF4_FILE), pointer :: NCFILE
  integer (kind=c_int) :: iStat
  integer (kind=c_int) :: iIndex
  character (len=256) :: sDimName

  call nc_trap( nc_inq_ndims(ncid=NCFILE%iNCID, ndimsp=NCFILE%iNumberOfDimensions), &
                __FILE__, __LINE__ )

  allocate(NCFILE%NC_DIM( 0 : NCFILE%iNumberOfDimensions-1), stat=iStat )
  call assert(iStat == 0, "Could not allocate memory for NC_DIM member in NC_FILE defined type", &
    trim(__FILE__), __LINE__)

  ! NetCDF 3 function
  call nc_trap( nc_inq_unlimdim(ncid=NCFILE%iNCID, unlimdimidp=NCFILE%iNC3_UnlimitedDimensionNumber), &
               __FILE__, __LINE__ )

  do iIndex = 0, NCFILE%iNumberOfDimensions-1

    call nc_trap(nc_inq_dim(ncid=NCFILE%iNCID, dimid=iIndex, &
      name=sDimName, &
      lenp=NCFILE%NC_DIM(iIndex)%iNC_DimSize), __FILE__, __LINE__ )

    NCFILE%NC_DIM(iIndex)%iNC_DimID = iIndex
    NCFILE%NC_DIM(iIndex)%sDimensionName = c_to_fortran_string(sDimName)

  enddo

end subroutine nc_populate_dimension_struct

!----------------------------------------------------------------------

subroutine nc_populate_variable_struct( NCFILE )

  type (T_NETCDF4_FILE), pointer :: NCFILE

  type (T_NETCDF_ATTRIBUTE), pointer :: pNC_ATT
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  integer (kind=c_int) :: iStat
  integer (kind=c_int) :: iIndex, iIndex2 , iIndex3
  character (len=256) :: sVarName
  character (len=256) :: sAttName
  character (len=512) :: sAttValue
  integer (kind=c_int), dimension(0:25) :: iAttValue
  integer (kind=c_short), dimension(0:25) :: i2AttValue
  real (kind=c_double), dimension(0:25) :: cdAttValue

  call nc_trap( nc_inq_nvars(ncid=NCFILE%iNCID, nvarsp=NCFILE%iNumberOfVariables), &
       __FILE__, __LINE__ )

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
        nattsp=pNC_VAR%iNumberOfAttributes ), __FILE__, __LINE__ )

    pNC_VAR%iNC_VarID = iIndex
    pNC_VAR%sVariableName = c_to_fortran_string(sVarName)

    if( pNC_VAR%iNumberOfAttributes > 0 ) then

      allocate( pNC_VAR%NC_ATT( 0:pNC_VAR%iNumberOfAttributes - 1 ), stat = iStat)
      call assert(iStat == 0, "Could not allocate memory for NC_ATT member within NC_VAR in NC_FILE defined type", &
        trim(__FILE__), __LINE__)

      do iIndex2=0, pNC_VAR%iNumberOfAttributes - 1

        pNC_ATT => pNC_VAR%NC_ATT(iIndex2)

        call nc_populate_attribute_struct( NCFILE=NCFILE, pNC_ATT=pNC_ATT, &
          iNC_VarID=iIndex, iAttNum=iIndex2 )

      enddo

    endif

  enddo

  call nc_trap( nc_inq_natts(ncid=NCFILE%iNCID, ngattsp=NCFILE%iNumberOfAttributes), &
       __FILE__, __LINE__ )

  allocate(NCFILE%NC_ATT(0:NCFILE%iNumberOfAttributes - 1), stat=iStat )
  call assert(iStat == 0, "Could not allocate memory for NC_ATT member within NC_FILE defined type", &
    trim(__FILE__), __LINE__)

  do iIndex=0, NCFILE%iNumberOfAttributes - 1
    pNC_ATT => NCFILE%NC_ATT(iIndex)

    call nc_populate_attribute_struct( NCFILE=NCFILE, pNC_ATT=pNC_ATT, &
      iNC_VarID=NC_GLOBAL, iAttNum=iIndex )

  enddo

end subroutine nc_populate_variable_struct

!----------------------------------------------------------------------

subroutine nc_populate_attribute_struct( NCFILE, pNC_ATT, iNC_VarID, iAttNum )

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE
  type (T_NETCDF_ATTRIBUTE), pointer :: pNC_ATT
  integer (kind=c_int) :: iNC_VarID
  integer (kind=c_int) :: iAttNum

  ![ LOCALS ]
  integer (kind=c_int) :: iStat
  character (len=256) :: sVarName
  character (len=256) :: sAttName
  character (len=512) :: sAttValue
  integer (kind=c_int) :: iIndex
  integer (kind=c_int), dimension(0:25) :: iAttValue
  integer (kind=c_short), dimension(0:25) :: i2AttValue
  real (kind=c_float), dimension(0:25) :: cfAttValue
  real (kind=c_double), dimension(0:25) :: cdAttValue

  call nc_trap( nc_inq_attname(ncid=NCFILE%iNCID, &
    varid=iNC_VarID, &
    attnum=iAttNum, &
    name=sAttName), __FILE__, __LINE__ )

  pNC_ATT%sAttributeName = c_to_fortran_string(sAttName)

  call nc_trap( nc_inq_att(ncid=NCFILE%iNCID, &
    varid=iNC_VarID, &
    name=sAttName, &
    xtypep=pNC_ATT%iNC_AttType, &
    lenp=pNC_ATT%iNC_AttSize), __FILE__, __LINE__ )

  select case(pNC_ATT%iNC_AttType)

    case (NC_CHAR)

      sAttValue = repeat(" ", 512)

      call nc_trap( nc_get_att_text(ncid=NCFILE%iNCID, &
        varid=iNC_VarID, &
        name=sAttName, &
        ip=sAttValue), __FILE__, __LINE__ )

        pNC_ATT%sAttributeValue = c_to_fortran_string(sAttValue)

    case (NC_SHORT)

      call nc_trap( nc_get_att_short(ncid=NCFILE%iNCID, &
        varid=iNC_VarID, &
        name=sAttName, &
        ip=i2AttValue), __FILE__, __LINE__ )

      write(pNC_ATT%sAttributeValue, fmt="(20(i0, 1x))" ) &
        (i2AttValue(iIndex), iIndex=0, pNC_ATT%iNC_AttSize-1)

    case (NC_INT)

      call nc_trap( nc_get_att_int(ncid=NCFILE%iNCID, &
        varid=iNC_VarID, &
        name=sAttName, &
        ip=iAttValue), __FILE__, __LINE__ )

      write(pNC_ATT%sAttributeValue, fmt="(20(i0,1x))" ) &
        (iAttValue(iIndex), iIndex=0, pNC_ATT%iNC_AttSize-1)

    case (NC_FLOAT)

      call nc_trap( nc_get_att_float(ncid=NCFILE%iNCID, &
        varid=iNC_VarID, &
        name=sAttName, &
        ip=cfAttValue), __FILE__, __LINE__ )

      write(pNC_ATT%sAttributeValue, fmt="(20(f16.6, 1x))" ) &
        (cfAttValue(iIndex), iIndex=0, pNC_ATT%iNC_AttSize-1)

    case (NC_DOUBLE)

      call nc_trap( nc_get_att_double(ncid=NCFILE%iNCID, &
        varid=iNC_VarID, &
        name=sAttName, &
        ip=cdAttValue), __FILE__, __LINE__ )

      write(pNC_ATT%sAttributeValue, fmt="(20(f16.6, 1x))" ) &
        (cdAttValue(iIndex), iIndex=0, pNC_ATT%iNC_AttSize-1)

    case default

  end select

end subroutine nc_populate_attribute_struct

!----------------------------------------------------------------------

subroutine netcdf_update_time_starting_index(NCFILE, iJulianDay)

  type (T_NETCDF4_FILE), pointer :: NCFILE
  integer (kind=c_int) :: iJulianDay

  NCFILE%iStart(NC_TIME) = netcdf_date_to_index( NCFILE=NCFILE, &
                                     iJulianDay=iJulianDay )

end subroutine netcdf_update_time_starting_index

!----------------------------------------------------------------------

subroutine netcdf_get_variable_time_y_x(NCFILE, rValues, iValues)

  type (T_NETCDF4_FILE), pointer :: NCFILE
  real (kind=c_float), dimension(:,:), optional :: rValues
  integer (kind=c_int), dimension(:,:), optional :: iValues

  if (NCFILE%iVarType(NC_Z) == NC_SHORT) then

    if (present(rValues) ) call nc_get_variable_time_y_x_short(NCFILE, rValues)

  elseif (NCFILE%iVarType(NC_Z) == NC_FLOAT) then

    if (present(rValues) ) call nc_get_variable_time_y_x_float(NCFILE, rValues)

  endif

end subroutine netcdf_get_variable_time_y_x

!----------------------------------------------------------------------

subroutine nc_get_variable_time_y_x_short(NCFILE, rValues)

  type (T_NETCDF4_FILE), pointer :: NCFILE
  real (kind=c_float), dimension(:,:) :: rValues

  ! [ LOCALS ]
  !! dimension #1 = column
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  integer (kind=c_short), dimension(size(rValues,1)*size(rValues,2)) :: i2TempData
  integer (kind=c_short), dimension(size(rValues,2), size(rValues,1)) :: i21
  integer (kind=c_short), dimension(size(rValues,1), size(rValues,2)) :: i12
  integer (kind=c_int) :: iStat

    pNC_VAR => NCFILE%NC_VAR(netcdf_return_VarIndex( NCFILE, NCFILE%iVarID(NC_Z)) )

    call nc_get_variable_short(NCFILE=NCFILE, &
       iNC_VarID=NCFILE%iVarID(NC_Z), &
       iNC_Start=[NCFILE%iStart(NC_TIME),NCFILE%iStart(NC_Y), NCFILE%iStart(NC_X)], &
       iNC_Count=[NCFILE%iCount(NC_TIME),NCFILE%iCount(NC_Y), NCFILE%iCount(NC_X)], &
       iNC_Stride=[NCFILE%iStride(NC_TIME),NCFILE%iStride(NC_Y), NCFILE%iStride(NC_X)], &
       iNC_Vars=i2TempData)

!    rValues = transpose(reshape(source=real(i2TempData, kind=c_float), &
!                shape=[NCFILE%iCount(NC_Y), NCFILE%iCount(NC_X)], &
!                       order=[2, 1]))


    i12 = reshape(source=i2TempData, &
                shape=[NCFILE%iNX, NCFILE%iNY], &
                order=[1, 2] )

    rValues = real(i12, kind=c_float)

    call stats_WriteMinMeanMax( iLU=LU_LOG, &
       sText=trim(NCFILE%sFilename), &
       rData=rValues)


end subroutine nc_get_variable_time_y_x_short

!----------------------------------------------------------------------

subroutine nc_get_variable_time_y_x_float(NCFILE, rValues)

  type (T_NETCDF4_FILE), pointer :: NCFILE
  real (kind=c_float), dimension(:,:) :: rValues

  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  real (kind=c_float), dimension(size(rValues,1)*size(rValues,2)) :: rTempData
  real (kind=c_float), dimension(size(rValues,1), size(rValues,2)) :: r12
  real (kind=c_float), dimension(size(rValues,2), size(rValues,1)) :: r21
  integer (kind=c_int) :: iStat

    pNC_VAR => NCFILE%NC_VAR(netcdf_return_VarIndex( NCFILE, NCFILE%iVarID(NC_Z)) )

    call nc_get_variable_float(NCFILE=NCFILE, &
       iNC_VarID=NCFILE%iVarID(NC_Z), &
       iNC_Start=[NCFILE%iStart(NC_TIME),NCFILE%iStart(NC_Y), NCFILE%iStart(NC_X)], &
       iNC_Count=[NCFILE%iCount(NC_TIME),NCFILE%iCount(NC_Y), NCFILE%iCount(NC_X)], &
       iNC_Stride=[NCFILE%iStride(NC_TIME),NCFILE%iStride(NC_Y), NCFILE%iStride(NC_X)], &
       rNC_Vars=rTempData)

!    rValues = transpose(reshape(source=real(i2TempData, kind=c_float), &
!                shape=[NCFILE%iCount(NC_Y), NCFILE%iCount(NC_X)], &
!                       order=[2, 1]))

!    r12 = reshape(source=rTempData, &
!                shape=[NCFILE%iCount(NC_Y), NCFILE%iCount(NC_X)], &
!                       order=[2, 1] )
!
!    r21 = transpose(r12)
!
!    rValues = r21

    r12 = reshape(source=rTempData, &
                shape=[NCFILE%iNX, NCFILE%iNY], &
                order=[1, 2] )

    rValues = r12

    call stats_WriteMinMeanMax( iLU=LU_LOG, &
       sText=trim(NCFILE%sFilename), &
       rData=rValues)

end subroutine nc_get_variable_time_y_x_float

!----------------------------------------------------------------------

subroutine nc_get_variable_short(NCFILE, iNC_VarID, iNC_Start, iNC_Count, &
   iNC_Stride, iNC_Vars)

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE
  integer (kind=c_int) :: iNC_VarID
  integer (kind=c_size_t), dimension(:) :: iNC_Start
  integer (kind=c_size_t), dimension(:) :: iNC_Count
  integer (kind=c_size_t), dimension(:) :: iNC_Stride
  integer (kind=c_short), dimension(:) :: iNC_Vars

  type (c_ptr) :: pCount, pStart, pStride
  integer (kind=c_size_t), dimension(size(iNC_Start)), target :: tNC_Start
  integer (kind=c_size_t), dimension(size(iNC_Count)), target :: tNC_Count
  integer (kind=c_ptrdiff_t), dimension(size(iNC_Stride)), target :: tNC_Stride

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
       vars=iNC_Vars), __FILE__, __LINE__ )

end subroutine nc_get_variable_short

!----------------------------------------------------------------------

subroutine nc_get_variable_double(NCFILE, iNC_VarID, iNC_Start, iNC_Count, &
   iNC_Stride, dpNC_Vars)

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE
  integer (kind=c_int) :: iNC_VarID
  integer (kind=c_size_t), dimension(:) :: iNC_Start
  integer (kind=c_size_t), dimension(:) :: iNC_Count
  integer (kind=c_size_t), dimension(:) :: iNC_Stride
  real (kind=c_double), dimension(:) :: dpNC_Vars

  type (c_ptr) :: pCount, pStart, pStride
  integer (kind=c_size_t), dimension(size(iNC_Start)), target :: tNC_Start
  integer (kind=c_size_t), dimension(size(iNC_Count)), target :: tNC_Count
  integer (kind=c_ptrdiff_t), dimension(size(iNC_Stride)), target :: tNC_Stride

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
       vars=dpNC_Vars), __FILE__, __LINE__ )

end subroutine nc_get_variable_double

!----------------------------------------------------------------------

subroutine nc_get_variable_float(NCFILE, iNC_VarID, iNC_Start, iNC_Count, &
   iNC_Stride, rNC_Vars )

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE
  integer (kind=c_int) :: iNC_VarID
  integer (kind=c_size_t), dimension(:) :: iNC_Start
  integer (kind=c_size_t), dimension(:) :: iNC_Count
  integer (kind=c_size_t), dimension(:) :: iNC_Stride
  real (kind=c_float), dimension(:) :: rNC_Vars

  type (c_ptr) :: pCount, pStart, pStride
  integer (kind=c_size_t), dimension(size(iNC_Start)), target :: tNC_Start
  integer (kind=c_size_t), dimension(size(iNC_Count)), target :: tNC_Count
  integer (kind=c_ptrdiff_t), dimension(size(iNC_Stride)), target :: tNC_Stride

  tNC_Start = iNC_Start
  tNC_Count = iNC_Count
  tNC_Stride = iNC_Stride

  pStart = c_loc(tNC_Start(1))
  pCount = c_loc(tNC_Count(1))
  pStride = c_loc(tNC_Stride(1))

  call nc_trap(nc_get_vars_float(ncid=NCFILE%iNCID, &
       varid=iNC_VarID, &
       startp=pStart, &
       countp=pCount, &
       stridep=pStride, &
       vars=rNC_Vars), __FILE__, __LINE__ )

end subroutine nc_get_variable_float

!----------------------------------------------------------------------

subroutine netcdf_dump_cdl(NCFILE, iLU)

  type (T_NETCDF4_FILE ) :: NCFILE
  type (T_NETCDF_ATTRIBUTE), pointer :: pNC_ATT
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  type (T_NETCDF_DIMENSION), pointer :: pNC_DIM
  integer :: iLU
  character (len=256) :: sBuf
  character (len=256) :: sDimName
  integer (kind=c_int) :: iDimID
  integer (kind=c_int) :: iUbound

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
  real (kind=c_double), dimension(0:1) :: dpValues

  ! [ LOCALS ]
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  type (T_NETCDF_DIMENSION), pointer :: pNC_DIM
  integer (kind=c_int) :: iDimSize
  integer (kind=c_int) :: iDimIndex
  integer (kind=c_size_t) :: iStride
  integer (kind=c_size_t) :: iCount
  integer (kind=c_short), dimension(0:1) :: spValues
  integer (kind=c_int), dimension(0:1) :: ipValues
  real (kind=c_float), dimension(0:1) :: rpValues

  call assert (iVarIndex >= lbound(NCFILE%NC_VAR,1) &
    .and. iVarIndex <= ubound(NCFILE%NC_VAR,1), &
      "INTERNAL PROGRAMMING ERROR - index out of bounds NC_FILE%NC_VAR" &
      //"~Offending index value: "//trim(asCharacter(iVarIndex)), &
      trim(__FILE__), __LINE__)

  pNC_VAR => NCFILE%NC_VAR(iVarIndex)
  iDimSize = nc_return_DimSize(NCFILE, pNC_VAR%iNC_DimID(0) )

  if (iDimSize > 1) then
    iCount = 2_c_size_t
    iStride = int(iDimSize, kind=c_size_t) - 1_c_size_t
  else
    iCount = 1_c_size_t
    iStride = 1_c_size_t
  endif

  select case (pNC_VAR%iNC_VarType )

    case (NC_SHORT)

      call nc_get_variable_short(NCFILE=NCFILE, &
        iNC_VarID=pNC_VAR%iNC_VarID, &
        iNC_Start=[0_c_size_t], &
        iNC_Count=[iCount], &
        iNC_Stride=[iStride], &
        iNC_Vars=spValues)

      dpValues = real(spValues, kind=c_double)

    case (NC_INT)

    case (NC_FLOAT)

      call nc_get_variable_float(NCFILE=NCFILE, &
        iNC_VarID=pNC_VAR%iNC_VarID, &
        iNC_Start=[0_c_size_t], &
        iNC_Count=[iCount], &
        iNC_Stride=[iStride], &
        rNC_Vars=rpValues)

      dpValues = real(rpValues, kind=c_double)

    case (NC_DOUBLE)

      call nc_get_variable_double(NCFILE=NCFILE, &
        iNC_VarID=pNC_VAR%iNC_VarID, &
        iNC_Start=[0_c_size_t], &
        iNC_Count=[iCount], &
        iNC_Stride=[iStride], &
        dpNC_Vars=dpValues)

    case default

  end select

  !> if there is only one day of data in this NetCDF file, the
  !> first day equals the last day
  if (iCount == 1) dpValues(LAST) = dpValues(FIRST)

end function netcdf_get_first_and_last

!----------------------------------------------------------------------

subroutine netcdf_calculate_time_range(NCFILE)

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE

  NCFILE%iOriginJD = julian_day(NCFILE%iOriginYear, &
    NCFILE%iOriginMonth, NCFILE%iOriginDay)

  NCFILE%iFirstDayJD = NCFILE%iOriginJD + NCFILE%dpFirstAndLastTimeValues(FIRST)
  NCFILE%iLastDayJD = NCFILE%iOriginJD + NCFILE%dpFirstAndLastTimeValues(LAST)

end subroutine netcdf_calculate_time_range

!----------------------------------------------------------------------

subroutine nc_get_time_units(NCFILE)

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE

  ! [ LOCALS ]
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  character (len=256) :: sDateTime
  character (len=256) :: sItem
  integer (kind=c_int) :: iIndex
  logical (kind=c_bool) :: lFound
  integer (kind=c_int) :: iStat

  call assert(NCFILE%iVarID(NC_TIME) >= 0, "INTERNAL PROGRAMMING ERROR -- " &
    //"nc_get_time_units must be called only after a call is made to ~" &
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

  call chomp(sDateTime, sItem)    !> should be "days"
  call chomp(sDateTime, sItem)    !> should be "since"

  call chomp(sDateTime, sItem, "/-")
  read(sItem, *) NCFILE%iOriginYear

  call chomp(sDateTime, sItem, "/-")
  read(sItem, *) NCFILE%iOriginMonth

  read(sDateTime, *) NCFILE%iOriginDay

  call chomp(sDateTime, sItem, ":")
  read(sItem, *) NCFILE%iOriginHH

  call chomp(sDateTime, sItem, ":")
  read(sItem, *) NCFILE%iOriginMM

  read(sDateTime, *) NCFILE%iOriginSS

end subroutine nc_get_time_units

!----------------------------------------------------------------------

subroutine nc_get_XYZ_units(NCFILE)

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE

  ! [ LOCALS ]
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  integer (kind=c_int) :: iIndex, iIndex2
  logical (kind=c_bool) :: lFound
  integer (kind=c_int) :: iStat

  do iIndex = NC_X, NC_Z

    call assert(NCFILE%iVarID(iIndex) >= 0, "INTERNAL PROGRAMMING ERROR -- " &
    //"nc_get_XYZ_units must be called only after a call is made to ~" &
    //"netcdf_get_variable_ids", trim(__FILE__), __LINE__)

    pNC_VAR => NCFILE%NC_VAR(NCFILE%iVarID(iIndex) )

    lFound = lFALSE

    do iIndex2=0, pNC_VAR%iNumberOfAttributes - 1

      if ( str_compare(pNC_VAR%NC_ATT(iIndex2)%sAttributeName, "units") ) then
        lFound = lTRUE
        exit
      endif

    enddo

    if (lFound) then
      NCFILE%sVarUnits(iIndex) = trim(pNC_VAR%NC_ATT(iIndex2)%sAttributeValue)
    endif

  enddo

end subroutine nc_get_XYZ_units

!----------------------------------------------------------------------

subroutine netcdf_get_variable_id_and_type( NCFILE, sX_VarName, sY_VarName, &
   sZ_VarName, sTime_VarName)

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE

  character (len=*) :: sX_VarName
  character (len=*) :: sY_VarName
  character (len=*) :: sZ_VarName
  character (len=*) :: sTime_VarName

   ! [ LOCALS ]
   integer (kind=c_int) :: iIndex
   type (T_NETCDF_VARIABLE), pointer :: pNC_VAR

   NCFILE%iVarID = -9999

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

!     write(*, fmt="(i2.2,a12,1x,10(i2,1x))"), iIndex, dquote(pNC_VAR%sVariableName), pNC_VAR%iNC_VarID, pNC_VAR%iNC_VarType, pNC_VAR%iNC_DimID


   enddo

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

function nc_return_index_double(rValues, rTargetValue)  result(iIndex)

  real (kind=c_double), dimension(:) :: rValues
  real (kind=c_double) :: rTargetValue
  integer (kind=c_int) :: iIndex

  ! [ LOCALS ]
  integer (kind=c_int) :: iCount
  real (kind=c_double) :: rDiff, rDiffMin

  if ( .not. (rTargetValue >= minval(rValues) .and. rTargetValue <= maxval(rValues)) ) then
    call echolog("rTargetValue (" &
    //trim(asCharacter(rTargetValue))//") is not within the range " &
    //trim(asCharacter(minval(rValues)))//" to "//trim(asCharacter(maxval(rValues))) )

    call assert(lFALSE, "INTERNAL PROGRAMMING ERROR", trim(__FILE__), __LINE__)
  endif

  rDiffMin = 1.e+20

  do iCount=lbound(rValues,1), ubound(rValues,1)

    rDiff = abs(rValues(iCount) - rTargetValue)

    if ( rDiff < rDiffMin ) then
      iIndex = iCount
      rDiffMin =rDiff
    endif

  enddo

end function nc_return_index_double

!----------------------------------------------------------------------

function nc_coord_to_col_row(NCFILE, rX, rY)  result(iColRow)

  type (T_NETCDF4_FILE ), pointer :: NCFILE
  real (kind=c_double) :: rX
  real (kind=c_double) :: rY
  integer (kind=c_size_t), dimension(2) :: iColRow


  ! [ LOCALS ]
  integer (kind=c_int) :: iColNum, iRowNum

  iColNum = nc_return_index_double(NCFILE%rX_Coords, rX)
  iRowNum = nc_return_index_double(NCFILE%rY_Coords, rY)

  iColRow(COLUMN) = iColNum
  iColRow(ROW) = iRowNum

end function nc_coord_to_col_row

!----------------------------------------------------------------------

function netcdf_get_varid(NCFILE, sVariableName)  result(iNC_VarID)

  type (T_NETCDF4_FILE ) :: NCFILE
  character (len=*) :: sVariableName
  integer (kind=c_int) :: iNC_VarID

  integer (kind=c_int) :: iIndex
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
