!> @file
!>  Contains a single module, swb_grid, which
!>  provides support for gridded ASCII data file and data structure operations
!> @ingroup grid

!>  Provides support for input and output of gridded ASCII data,
!> as well as for creation and destruction of grid data structures (defined types).
module swb_grid

  use iso_c_binding
  use types

  implicit none

  !> interface to C code that provides a simplified entry point to PROJ4
  !> capabilities: it has been modified so that all C pointers are kept within the
  !> C code; no pointers are returned to fortran
  interface
    function pj_init_and_transform(from_projection, to_projection, point_count, x, y) &
     bind(c,name='pj_init_and_transform')
    import
    character(kind=c_char) :: from_projection(*)
    character(kind=c_char) :: to_projection(*)
    integer(kind=c_long),value :: point_count
    real(kind=c_double) :: x(*)
    real(kind=c_double) :: y(*)
    integer(kind=c_int) :: pj_init_and_transform
    end function pj_init_and_transform
  end interface

  type T_ERROR_MESSAGE
    character(len=40) :: cErrorMessageText
    integer           :: iErrorNumber
  end type T_ERROR_MESSAGE

  ! error messages adapted from file "pj_strerrno.c" in PROJ.4.8.0
  type(T_ERROR_MESSAGE), dimension(49) :: tErrorMessage = (/ &
    T_ERROR_MESSAGE("no arguments in initialization list          ",  -1), &
    T_ERROR_MESSAGE("no options found in 'init' file              ",  -2), &
    T_ERROR_MESSAGE("no colon in init= string                     ",  -3), &
    T_ERROR_MESSAGE("projection not named                         ",  -4), &
    T_ERROR_MESSAGE("unknown projection id                        ",  -5), &
    T_ERROR_MESSAGE("effective eccentricity = 1.                  ",  -6), &
    T_ERROR_MESSAGE("unknown unit conversion id                   ",  -7), &
    T_ERROR_MESSAGE("invalid boolean param argument               ",  -8), &
    T_ERROR_MESSAGE("unknown elliptical parameter name            ",  -9), &
    T_ERROR_MESSAGE("reciprocal flattening (1/f) = 0              ", -10), &
    T_ERROR_MESSAGE("|radius reference latitude| > 90             ", -11), &
    T_ERROR_MESSAGE("squared eccentricity < 0                     ", -12), &
    T_ERROR_MESSAGE("major axis or radius = 0 or not given        ", -13), &
    T_ERROR_MESSAGE("latitude or longitude exceeded limits        ", -14), &
    T_ERROR_MESSAGE("invalid x or y                               ", -15), &
    T_ERROR_MESSAGE("improperly formed DMS value                  ", -16), &
    T_ERROR_MESSAGE("non-convergent inverse meridional dist       ", -17), &
    T_ERROR_MESSAGE("non-convergent inverse phi2                  ", -18), &
    T_ERROR_MESSAGE("acos/asin: |arg| >1.+1e-14                   ", -19), &
    T_ERROR_MESSAGE("tolerance condition error                    ", -20), &
    T_ERROR_MESSAGE("conic lat_1 = -lat_2                         ", -21), &
    T_ERROR_MESSAGE("lat_1 >= 90                                  ", -22), &
    T_ERROR_MESSAGE("lat_1 = 0                                    ", -23), &
    T_ERROR_MESSAGE("lat_ts >= 90                                 ", -24), &
    T_ERROR_MESSAGE("no distance between control points           ", -25), &
    T_ERROR_MESSAGE("projection not selected to be rotated        ", -26), &
    T_ERROR_MESSAGE("W <= 0 or M <= 0                             ", -27), &
    T_ERROR_MESSAGE("lsat not in 1-5 range                        ", -28), &
    T_ERROR_MESSAGE("path not in range                            ", -29), &
    T_ERROR_MESSAGE("h <= 0                                       ", -30), &
    T_ERROR_MESSAGE("k <= 0                                       ", -31), &
    T_ERROR_MESSAGE("lat_0 = 0 or 90 or alpha = 90                ", -32), &
    T_ERROR_MESSAGE("lat_1=lat_2 or lat_1=0 or lat_2=90           ", -33), &
    T_ERROR_MESSAGE("elliptical usage required                    ", -34), &
    T_ERROR_MESSAGE("invalid UTM zone number                      ", -35), &
    T_ERROR_MESSAGE("arg(s) out of range for Tcheby eval          ", -36), &
    T_ERROR_MESSAGE("failed to find projection to be rotated      ", -37), &
    T_ERROR_MESSAGE("failed to load datum shift file              ", -38), &
    T_ERROR_MESSAGE("both n ), & m must be spec'd and > 0         ", -39), &
    T_ERROR_MESSAGE("n <= 0, n > 1 or not specified               ", -40), &
    T_ERROR_MESSAGE("lat_1 or lat_2 not specified                 ", -41), &
    T_ERROR_MESSAGE("|lat_1| == |lat_2|                           ", -42), &
    T_ERROR_MESSAGE("lat_0 is pi/2 from mean lat                  ", -43), &
    T_ERROR_MESSAGE("unparseable coordinate system definition     ", -44), &
    T_ERROR_MESSAGE("geocentric transformation missing z or ellps ", -45), &
    T_ERROR_MESSAGE("unknown prime meridian conversion id         ", -46), &
    T_ERROR_MESSAGE("illegal axis orientation combination         ", -47), &
    T_ERROR_MESSAGE("point not within available datum shift grids ", -48), &
    T_ERROR_MESSAGE("invalid sweep axis, choose x or y            ", -49) /)

  interface grid_gridToGrid
    module procedure grid_gridToGrid_int
    module procedure grid_gridToGrid_short
    module procedure grid_gridToGrid_sgl
  end interface grid_gridToGrid

  interface grid_Create
    module procedure grid_CreateSimple
    module procedure grid_CreateComplete
  end interface grid_Create

  ! global parameters for use with the majority filter
  integer (kind=c_int), parameter :: FOUR_CELLS = 1
  integer (kind=c_int), parameter :: EIGHT_CELLS = 2

  integer (kind=c_int), parameter :: COLUMN = 1
  integer (kind=c_int), parameter :: ROW = 2

contains

!--------------------------------------------------------------------------

!> Creates a grid of a specified type.
!>
!>  Creates a grid pointer object and allocates memory for the data
!>  associated with the grid (REAL, INTEGER, or T_CELL).
!
!> @param iNX Number of grid cells in the x direction
!> @param iNY Number of grid cells in the y direction
!> @param rX0 X coordinate for the lower left corner of the grid
!> @param rY0 Y coordinate for the lower left corner of the grid
!> @param rX1 X coordinate for the upper right corner of the grid
!> @param rY1 Y coordinate for the upper right corner of the grid
!> @param iDataType Integer value corresponding to the type of data contained in the grid
!>
!> @return pGrd Pointer to a grid object
function grid_CreateComplete ( iNX, iNY, rX0, rY0, rX1, rY1, iDataType ) result ( pGrd )

  ! ARGUMENTS
  integer (kind=c_int), intent(in) :: iNX, iNY        ! Grid dimensions
  real (kind=c_double), intent(in) :: rX0, rY0          ! Lower-left corner (world coords)
  real (kind=c_double), intent(in) :: rX1, rY1          ! Upper-right corner (world coords)
  integer (kind=c_int), intent(in) :: iDataType       ! Data type (DATATYPE_INT, etc.)
  ! RETURN VALUE
  type (T_GENERAL_GRID), pointer :: pGrd
  ! LOCALS
  integer (kind=c_int) :: iStat

  allocate ( pGrd, stat=iStat )
  call Assert ( iStat == 0, &
     "Could not allocate pointer to T_GRID object", trim(__FILE__),__LINE__ )

  if (iNX <= 0 .or. iNY <= 0) then
    call echolog("Illegal grid dimensions: ")
    call echolog("iNX: "//trim(asCharacter(iNX) ) )
    call echolog("iNY: "//trim(asCharacter(iNY) ) )
    call echolog("rX0: "//trim(asCharacter(rX0) ) )
    call echolog("rY0: "//trim(asCharacter(rY0) ) )
    call echolog("rX1: "//trim(asCharacter(rX1) ) )
    call echolog("rY1: "//trim(asCharacter(rY1) ) )
    call Assert ( lFALSE, &
       "INTERNAL PROGRAMMING ERROR? - Illegal grid dimensions specified", trim(__FILE__),__LINE__)
  endif

  select case (iDataType)
      case ( GRID_DATATYPE_INT )

          allocate ( pGrd%iData( iNX, iNY ), stat=iStat )
          call Assert (iStat == 0, &
             "Could not allocate integer data", &
              TRIM(__FILE__),__LINE__)
          pGrd%iData = 0

      case ( GRID_DATATYPE_REAL )
          allocate ( pGrd%rData( iNX, iNY ), stat=iStat )
          call Assert (iStat == 0, &
             "Could not allocate real data", &
              TRIM(__FILE__),__LINE__)
          pGrd%rData = rZERO

      case ( GRID_DATATYPE_CELL_GRID )
          allocate ( pGrd%Cells( iNX, iNY ), stat=iStat )
          call Assert (iStat == 0, &
             "Could not allocate cell-by-cell data", &
             TRIM(__FILE__),__LINE__)

      case ( GRID_DATATYPE_ALL )
          allocate ( pGrd%iData( iNX, iNY ), stat=iStat )
          call Assert (iStat == 0, &
             "Could not allocate integer data", &
              TRIM(__FILE__),__LINE__)
              pGrd%iData = 0

          allocate ( pGrd%rData( iNX, iNY ), stat=iStat )
          call Assert (iStat == 0, &
             "Could not allocate real data", &
              TRIM(__FILE__),__LINE__)
              pGrd%rData = 0.0

          allocate ( pGrd%Cells( iNX, iNY ), stat=iStat )
          call Assert (iStat == 0, &
             "Could not allocate cell-by-cell data", &
             TRIM(__FILE__),__LINE__)

      case default
          call Assert ( lFALSE, 'Internal error -- illegal grid data type' )
  end select

  pGrd%iDataType = iDataType
  pGrd%iNX = iNX
  pGrd%iNY = iNY
  pGrd%rX0 = rX0
  pGrd%rX1 = rX1
  pGrd%rY0 = rY0
  pGrd%rY1 = rY1
  pGrd%rGridCellSize = (pGrd%rX1 - pGrd%rX0) / real(pGrd%iNX, kind=c_float)
  pGrd%iNumGridCells = iNX * iNY

  allocate(pGrd%iMask(iNX, iNY))
  pGrd%iMask = 1

end function grid_CreateComplete

function grid_CreateSimple ( iNX, iNY, rX0, rY0, rGridCellSize, iDataType ) result ( pGrd )
  !! Creates a new iNX-by-iNY T_GRID of data type iDataType, over the extent
  !! (rX0,rY0)-(rX1,rY1), and returns a pointer.
  ! ARGUMENTS
  integer (kind=c_int), intent(in) :: iNX, iNY        ! Grid dimensions
  real (kind=c_double), intent(in) :: rX0, rY0          ! Lower-left corner (world coords)
  real (kind=c_double), intent(in) :: rGridCellSize
  integer (kind=c_int), intent(in) :: iDataType       ! Data type (DATATYPE_INT, etc.)

  ! RETURN VALUE
  type (T_GENERAL_GRID), pointer :: pGrd
  ! LOCALS
  integer (kind=c_int) :: iStat

  allocate ( pGrd, stat=iStat )
  call Assert ( iStat == 0, &
     "Could not allocate pointer to T_GRID object", trim(__FILE__),__LINE__ )
  call Assert ( iNX > 0 .and. iNY > 0, &
     "Illegal grid dimensions specified", trim(__FILE__),__LINE__)

  select case (iDataType)
      case ( GRID_DATATYPE_INT )

          allocate ( pGrd%iData( iNX, iNY ), stat=iStat )
          call Assert (iStat == 0, &
             "Could not allocate integer data", &
              TRIM(__FILE__),__LINE__)
          pGrd%iData = 0

      case ( GRID_DATATYPE_REAL )
          allocate ( pGrd%rData( iNX, iNY ), stat=iStat )
          call Assert (iStat == 0, &
             "Could not allocate real data", &
              TRIM(__FILE__),__LINE__)
          pGrd%rData = rZERO

      case ( GRID_DATATYPE_CELL_GRID )
          allocate ( pGrd%Cells( iNX, iNY ), stat=iStat )
          call Assert (iStat == 0, &
             "Could not allocate cell-by-cell data", &
             TRIM(__FILE__),__LINE__)

      case ( GRID_DATATYPE_ALL )
          allocate ( pGrd%iData( iNX, iNY ), stat=iStat )
          call Assert (iStat == 0, &
             "Could not allocate integer data", &
              TRIM(__FILE__),__LINE__)
              pGrd%iData = 0

          allocate ( pGrd%rData( iNX, iNY ), stat=iStat )
          call Assert (iStat == 0, &
             "Could not allocate real data", &
              TRIM(__FILE__),__LINE__)
              pGrd%rData = 0.0

          allocate ( pGrd%Cells( iNX, iNY ), stat=iStat )
          call Assert (iStat == 0, &
             "Could not allocate cell-by-cell data", &
             TRIM(__FILE__),__LINE__)

      case default
          call Assert ( lFALSE, 'Internal error -- illegal grid data type' )
  end select

  pGrd%iDataType = iDataType
  pGrd%iNX = iNX
  pGrd%iNY = iNY
  pGrd%rX0 = rX0
  pGrd%rX1 = rX0 + real(iNX, kind=c_double) * rGridCellSize
  pGrd%rY0 = rY0
  pGrd%rY1 = rY0 + real(iNY, kind=c_double) * rGridCellSize
  pGrd%rGridCellSize = rGridCellSize
  pGrd%iNumGridCells = iNX * iNY

  allocate(pGrd%iMask(iNX, iNY))
  pGrd%iMask = 1

end function grid_CreateSimple

!!***

!--------------------------------------------------------------------------
!!****f* grid/grid_Destroy
! NAME
!   grid_Destroy - Destroys a grid of a specified type.
!
! SYNOPSIS
!   Destroys a grid pointer object and deallocates memory for the data
!   associated with the grid (REAL, INTEGER, or T_CELL).
!
! INPUTS
!   pGrd - Pointer to a grid object
!
! OUTPUTS
!   None
!
! NOTES
!   Code refers to parameters that are set within types.f95.
!
! SOURCE

subroutine grid_Destroy ( pGrd )
  !! Destroys the data in the T_GENERAL_GRID pGrd
  ! ARGUMENTS
  type ( T_GENERAL_GRID ), pointer :: pGrd
  ! LOCALS
  integer (kind=c_int) :: iStat

  if(associated(pGrd) )then

    if ( pGrd%iDataType == GRID_DATATYPE_INT ) then
      deallocate ( pGrd%iData, stat=iStat )
      call Assert ( iStat == 0, "Failed to deallocate integer grid" )
    else if ( pGrd%iDataType == GRID_DATATYPE_REAL ) then
      deallocate ( pGrd%rData, stat=iStat )
      call Assert ( iStat == 0, "Failed to deallocate real grid" )
    else if ( pGrd%iDataType == GRID_DATATYPE_CELL_GRID ) then
      deallocate ( pGrd%Cells, stat=iStat )
      call Assert ( iStat == 0, "Failed to deallocate cell grid" )
    else if ( pGrd%iDataType == GRID_DATATYPE_ALL ) then
      deallocate ( pGrd%iData, stat=iStat )
      call Assert ( iStat == 0, "Failed to deallocate integer grid" )
      deallocate ( pGrd%rData, stat=iStat )
      call Assert ( iStat == 0, "Failed to deallocate real grid" )
      deallocate ( pGrd%Cells, stat=iStat )
      call Assert ( iStat == 0, "Failed to deallocate cell grid" )
    else
      call Assert ( lFALSE, "Internal error -- unknown grid type", &
        trim(__FILE__), __LINE__)
    end if

    if( allocated(pGrd%rX) ) then
      deallocate( pGrd%rX, stat=iStat)
      call Assert ( iStat == 0, "Failed to deallocate X-coordinate data structure associated with grid", &
        trim(__FILE__), __LINE__ )
    endif

    if( allocated(pGrd%rY) ) then
      deallocate( pGrd%rY, stat=iStat)
      call Assert ( iStat == 0, "Failed to deallocate Y-coordinate data structure associated with grid", &
        trim(__FILE__), __LINE__ )
    endif

  endif

  pGrd => null()

end subroutine grid_Destroy
!!***

!--------------------------------------------------------------------------
!!****f* grid/grid_Read
! NAME
!   grid_Read - Reads a grid of a specified type.
!
! SYNOPSIS
!   Reads a grid of the
! INPUTS
!   sFileName - Character string containing the name of the file to be read
!   sFileType - Character string indicating the type of file to be read
!   iDataType - Integer value corresponding to the type of data contained
!     in the input data file
!
! OUTPUTS
!   pGrd - Pointer to a grid object
!
! NOTES
!
! Current legal file types are
!    "ARC_GRID"            ESRI ARC Grid (ASCII)
!    "SURFER"              Golden Software SURFER grid (ASCII)
!
! Valid data types are (see module 'types'):
!    DATATYPE_INT
!    DATATYPE_REAL
!
! SOURCE

function grid_Read ( sFilename, sFileType, iDataType ) result ( pGrd )

  ! ARGUMENTS
  character (len=*), intent(in) :: sFilename          ! Name of the grid input file
  character (len=*), intent(in) :: sFileType          ! File type (see above)
  integer (kind=c_int), intent(in) :: iDataType       ! T_GRID_INT or T_GRID_REAL
  ! RETURN VALUE
  type (T_GENERAL_GRID), pointer :: pGrd

  if ( trim(sFileType) == "ARC_GRID" ) then
      pGrd => grid_ReadArcGrid_fn( sFileName, iDataType )
  else if ( trim(sFileType) == "SURFER" ) then
      pGrd => grid_ReadSurferGrid_fn( sFileName, iDataType )
  else
      call Assert( lFALSE, "Illegal grid file type requested" )
  end if

  pGrd%sFilename = trim(sFilename)

end function grid_Read

!--------------------------------------------------------------------------

subroutine grid_ReadExisting ( sFileName, sFileType, pGrd )

  ! ARGUMENTS
  character (len=*), intent(in) :: sFilename          ! Name of the grid input file
  character (len=*), intent(in) :: sFileType          ! File type (see above)
  type (T_GENERAL_GRID), pointer :: pGrd

  if ( trim(sFileType) == "ARC_GRID" ) then
      call grid_ReadArcGrid_sub( sFilename, pGrd )
  else if ( trim(sFileType) == "SURFER" ) then
      call grid_ReadSurferGrid_sub( sFilename, pGrd )
  else
      call Assert( lFALSE, "Illegal grid file type requested" )
  end if

  pGrd%sFilename = trim(sFilename)

end subroutine grid_ReadExisting

!!***
!--------------------------------------------------------------------------
!!****f* grid/grid_ReadArcGrid_fn
! NAME
!   grid_ReadArcGrid_fn - Reads an ARC ASCII grid of a specified type.
!
! SYNOPSIS
!   Reads an ARC ASCII grid of specified data type
!
! INPUTS
!   sFileName - Character string containing the name of the file to be read
!   iDataType - Integer value corresponding to the type of data contained
!     in the input data file
!
! OUTPUTS
!   pGrd - Pointer to a grid object
!
! NOTES
!
! Valid data types are (see module 'types'):
!    DATATYPE_INT
!    DATATYPE_REAL
!
! SOURCE

function grid_ReadArcGrid_fn ( sFileName, iDataType ) result ( pGrd )

  ! ARGUMENTS
  character (len=*), intent(in) :: sFileName          ! Name of the grid input file
  integer (kind=c_int), intent(in) :: iDataType       ! T_GRID_INT or T_GRID_REAL
  ! RETURN VALUE
  type (T_GENERAL_GRID), pointer :: pGrd
  ! LOCALS
  character (len=256) :: sInputRecord                 ! Text record for input
  character (len=256) :: sDirective                   ! Directive for input
  character (len=256) :: sArgument                    ! Argument for keyword directives
  character (len=256) :: sNoDataValue                 ! String to hold nodata value
  character (len=8192) :: sBuf
  integer (kind=c_int) :: iStat                       ! For "iostat="
  integer (kind=c_int) :: iNX,iNY                     ! Grid dimensions
  integer (kind=c_int) :: iHdrRecs                    ! Number of records in header
  integer (kind=c_int) :: iCol,iRow,k                 ! Loop indices for grid reading
  real (kind=c_double) :: rX0,rX1                        ! Limits in X
  real (kind=c_double) :: rY0,rY1                        ! Limits in Y
  real (kind=c_double) :: rCellSize                      ! Cell size
  integer (kind=c_int) :: iCount,iCumlCount
  logical (kind=c_bool) :: lXLLCenter, lYLLCenter  ! Flags XLLCENTER / XLLCORNER
  logical (kind=c_bool) :: lFileExists


  ! Pre-scan for the number of header records and read the header
  inquire(file=trim(sFileName), EXIST=lFileExists)
  call assert( lFileExists, "The Arc ASCII grid file "//dquote(sFilename)// &
    " could not be found.",trim(__FILE__),__LINE__)

  open ( LU_GRID, iostat=iStat, file=trim(sFileName) )
  call Assert( iStat == 0, &
    "Could not open input file " // trim(sFileName) )

  iHdrRecs = 0
  lXLLCenter = lFALSE
  lYLLCenter = lFALSE
  do
      read ( unit=LU_GRID, fmt="(a256)", iostat=iStat ) sInputRecord
      call Assert ( iStat == 0, &
      "Could not read input record - file:"//trim(sFileName) )
      call Chomp ( sInputRecord, sDirective )
      call Uppercase ( sDirective )
      call Chomp ( sInputRecord, sArgument )
      if ( sDirective == "NCOLS" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) iNX
          call Assert ( iStat == 0, "Could not read NCOLS" )
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "NROWS" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) iNY
          call Assert ( iStat == 0, "Could not read NROWS" )
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "XLLCENTER" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) rX0
          call Assert ( iStat == 0, "Could not read XLLCENTER" )
          lXLLCenter = lTRUE
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "YLLCENTER" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) rY0
          call Assert ( iStat == 0, "Could not read YLLCENTER" )
          lXLLCenter = lTRUE
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "XLLCORNER" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) rX0
          call Assert ( iStat == 0, "Could not read XLLCORNER" )
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "YLLCORNER" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) rY0
          call Assert ( iStat == 0, "Could not read YLLCORNER" )
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "CELLSIZE" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) rCellSize
          call Assert ( iStat == 0, "Could not read CELLSIZE" )
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "NODATA_VALUE" ) then
          sNoDataValue = trim(sArgument)
          iHdrRecs = iHdrRecs + 1
      else
          ! Found the data -- construct the grid
          if ( lXLLCenter ) rX0 = rX0 - rHALF*rCellSize
          if ( lYLLCenter ) rY0 = rY0 - rHALF*rCellSize
          rX1 = rX0 + real(iNX, kind=c_double) * rCellSize
          rY1 = rY0 + real(iNY, kind=c_double) * rCellSize

          pGrd => grid_Create ( iNX, iNY, rX0, rY0, rX1, rY1, iDataType )

          pGrd%rGridCellSize = rCellSize
          ! Go back to the top, skip the header...
          rewind ( unit=LU_GRID, iostat=iStat )
          call Assert ( iStat == 0, "Failed to rewind grid file" )
          do iCol=1,iHdrRecs
              read ( unit=LU_GRID, fmt="(a256)", iostat=iStat ) sInputRecord
              call Assert ( iStat == 0, &
                "Could not read input record - file: "//trim(sFileName))
          end do
          ! ... and read the data.
          select case ( iDataType )
              case ( DATATYPE_INT )
                do iRow=1,pGrd%iNY
                  read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%iData(:,iRow)
                  call Assert ( iStat == 0, &
                    "Failed to read integer grid data - file: " &
                    //trim(sFileName)//"  row num: "//TRIM(int2char(iRow)), &
                   TRIM(__FILE__),__LINE__ )
                end do
                if(len_trim(sNoDataValue) > 0) then
                  read(unit=sNoDataValue, fmt=*, iostat=iStat) pGrd%iNoDataValue
                  call Assert ( iStat == 0, &
                    "Failed to read NODATA value in grid data - file: " &
                    //dquote(sFileName), TRIM(__FILE__),__LINE__ )
                endif
              case ( DATATYPE_REAL )
                do iRow=1,pGrd%iNY
                  read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%rData(:,iRow)
                  call Assert ( iStat == 0, &
                    "Failed to read real grid data - file: " &
                    //trim(sFileName)//"  row num: "//TRIM(int2char(iRow)), &
                   TRIM(__FILE__),__LINE__ )
                end do
                if(len_trim(sNoDataValue) > 0) then
                  read(unit=sNoDataValue, fmt=*, iostat=iStat) pGrd%rNoDataValue
                  call Assert ( iStat == 0, &
                    "Failed to read NODATA value in grid data - file: " &
                    //trim(sFileName), TRIM(__FILE__),__LINE__ )
                endif
              case default
                  call Assert ( lFALSE, &
                    "Internal error -- illegal ARC GRID data type", &
                    TRIM(__FILE__),__LINE__)
          end select
          exit
      end if
  end do

  if(iDataType==DATATYPE_INT) then    ! check for strange or illegal values
    iCumlCount = 0
    write(LU_LOG,"(1x,'Summary of integer grid data values')")

    do k=0,maxval(pGrd%iData)
      iCount=COUNT(pGrd%iData==k)
      if(iCount>0) then
        iCumlCount = iCumlCount + iCount
        write(LU_LOG,FMT="(3x,i8,' grid cells have value: ',i8)") &
          iCount, k
      end if
    end do

    write(LU_LOG,FMT="(1x,a,t48,i12)") "Total number of grid cells with value NODATA: ", &
      COUNT(pGrd%iData == pGrd%iNoDataValue )

    write(LU_LOG,FMT="(1x,a,t48,i12)") "Total number of grid cells: ", &
      size(pGrd%iData)
    write(LU_LOG,FMT="(1x,a,t48,i12)") &
      "Total number of grid cells with value >= 0: ",iCumlCount
    flush(LU_LOG)
!    call Assert(size(pGrd%iData)==iCumlCount, &
!      "Illegal or missing values in integer grid file: "//trim(sFileName))

  end if

  close ( unit=LU_GRID, iostat=iStat )
  call Assert ( iStat == 0, "Failed to close grid file" )

  return
end function grid_ReadArcGrid_fn

!--------------------------------------------------------------------------

subroutine grid_ReadArcGrid_sub ( sFileName, pGrd )

  ! ARGUMENTS
  character (len=*), intent(in) :: sFileName          ! Name of the grid input file
  type (T_GENERAL_GRID), pointer :: pGrd

  ! LOCALS
  character (len=256) :: sInputRecord                 ! Text record for input
  character (len=256) :: sDirective                   ! Directive for input
  character (len=256) :: sArgument                    ! Argument for keyword directives
  character (len=256) :: sNoDataValue                 ! String to hold nodata value
  character (len=8192) :: sBuf
  integer (kind=c_int) :: iStat                       ! For "iostat="
  integer (kind=c_int) :: iNX,iNY                     ! Grid dimensions
  integer (kind=c_int) :: iHdrRecs                    ! Number of records in header
  integer (kind=c_int) :: iCol,iRow,k                 ! Loop indices for grid reading
  real (kind=c_double) :: rX0,rX1                        ! Limits in X
  real (kind=c_double) :: rY0,rY1                        ! Limits in Y
  real (kind=c_double) :: rCellSize                      ! Cell size
  integer (kind=c_int) :: iCount,iCumlCount
  logical (kind=c_bool) :: lXLLCenter, lYLLCenter  ! Flags XLLCENTER / XLLCORNER
  logical (kind=c_bool) :: lFileExists

  ! Pre-scan for the number of header records and read the header

  inquire(file=trim(sFileName), EXIST=lFileExists)
  call assert( lFileExists, "The Arc ASCII grid file "//dquote(sFilename)// &
    " could not be found.",trim(__FILE__),__LINE__)

  open ( LU_GRID, iostat=iStat, file=trim(sFileName) )
  call Assert( iStat == 0, &
    "Could not open input file " // trim(sFileName) )

  iHdrRecs = 0
  lXLLCenter = lFALSE
  lYLLCenter = lFALSE
  do
      read ( unit=LU_GRID, fmt="(a256)", iostat=iStat ) sInputRecord
      call Assert ( iStat == 0, &
      "Could not read input record - file:"//trim(sFileName) )
      call Chomp ( sInputRecord, sDirective )
      call Uppercase ( sDirective )
      call Chomp ( sInputRecord, sArgument )
      if ( sDirective == "NCOLS" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) iNX
          call Assert ( iStat == 0, "Could not read NCOLS" )
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "NROWS" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) iNY
          call Assert ( iStat == 0, "Could not read NROWS" )
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "XLLCENTER" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) rX0
          call Assert ( iStat == 0, "Could not read XLLCENTER" )
          lXLLCenter = lTRUE
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "YLLCENTER" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) rY0
          call Assert ( iStat == 0, "Could not read YLLCENTER" )
          lXLLCenter = lTRUE
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "XLLCORNER" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) rX0
          call Assert ( iStat == 0, "Could not read XLLCORNER" )
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "YLLCORNER" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) rY0
          call Assert ( iStat == 0, "Could not read YLLCORNER" )
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "CELLSIZE" ) then
          read ( unit=sArgument, fmt=*, iostat=iStat ) rCellSize
          call Assert ( iStat == 0, "Could not read CELLSIZE" )
          iHdrRecs = iHdrRecs + 1
      else if ( sDirective == "NODATA_VALUE" ) then
          sNoDataValue = trim(sArgument)
          iHdrRecs = iHdrRecs + 1
      else
          ! Found the data -- construct the grid

          ! Go back to the top, skip the header...
          rewind ( unit=LU_GRID, iostat=iStat )
          call Assert ( iStat == 0, "Failed to rewind grid file" )
          do iCol=1,iHdrRecs
              read ( unit=LU_GRID, fmt="(a256)", iostat=iStat ) sInputRecord
              call Assert ( iStat == 0, &
                "Could not read input record - file: "//trim(sFileName))
          end do
          ! ... and read the data.
          select case ( pGrd%iDataType )
              case ( DATATYPE_INT )
                do iRow=1,pGrd%iNY
                  read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%iData(:,iRow)
                  call Assert ( iStat == 0, &
                    "Failed to read integer grid data - file: " &
                    //trim(sFileName)//"  row num: "//TRIM(int2char(iRow)), &
                   TRIM(__FILE__),__LINE__ )
                end do
                if(len_trim(sNoDataValue) > 0) then
                  read(unit=sNoDataValue, fmt=*, iostat=iStat) pGrd%iNoDataValue
                  call Assert ( iStat == 0, &
                    "Failed to read NODATA value in grid data - file: " &
                    //trim(sFileName), TRIM(__FILE__),__LINE__ )
                endif
              case ( DATATYPE_REAL )
                do iRow=1,pGrd%iNY
                  read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%rData(:,iRow)
                  call Assert ( iStat == 0, &
                    "Failed to read real grid data - file: " &
                    //trim(sFileName)//"  row num: "//TRIM(int2char(iRow)), &
                   TRIM(__FILE__),__LINE__ )
                end do
                if(len_trim(sNoDataValue) > 0) then
                  read(unit=sNoDataValue, fmt=*, iostat=iStat) pGrd%rNoDataValue
                  call Assert ( iStat == 0, &
                    "Failed to read NODATA value in grid data - file: " &
                    //trim(sFileName), TRIM(__FILE__),__LINE__ )
                endif
              case default
                  call Assert ( lFALSE, &
                    "Internal error -- illegal ARC GRID data type", &
                    TRIM(__FILE__),__LINE__)
          end select
          exit
      end if
  end do

  if(pGrd%iDataType==DATATYPE_INT) then    ! check for strange or illegal values
    iCumlCount = 0
    write(LU_LOG,"(1x,'Summary of integer grid data values')")

    do k=0,maxval(pGrd%iData)
      iCount=COUNT(pGrd%iData==k)
      if(iCount>0) then
        iCumlCount = iCumlCount + iCount
        write(LU_LOG,FMT="(3x,i8,' grid cells have value: ',i8)") &
          iCount, k
      end if
    end do

    write(LU_LOG,FMT="(1x,a,t48,i12)") "Total number of grid cells with value NODATA: ", &
      COUNT(pGrd%iData == pGrd%iNoDataValue )

    write(LU_LOG,FMT="(1x,a,t48,i12)") "Total number of grid cells: ", &
      size(pGrd%iData)
    write(LU_LOG,FMT="(1x,a,t48,i12)") &
      "Total number of grid cells with value >= 0: ",iCumlCount
    flush(LU_LOG)
    call Assert(LOGICAL(size(pGrd%iData)==iCumlCount,kind=c_bool), &
      "Illegal or missing values in integer grid file: "//trim(sFileName))

  end if

  close ( unit=LU_GRID, iostat=iStat )
  call Assert ( iStat == 0, "Failed to close grid file" )

end subroutine grid_ReadArcGrid_sub

!!***
!--------------------------------------------------------------------------
!!****f* grid/grid_ReadSurferGrid_fn
! NAME
!   grid_ReadSurferGrid_fn - Reads an Surfer ASCII grid of a specified type.
!
! SYNOPSIS
!   Reads an Surfer ASCII grid of specified data type
!
! INPUTS
!   sFileName - Character string containing the name of the file to be read
!   iDataType - Integer value corresponding to the type of data contained
!     in the input data file
!
! OUTPUTS
!   pGrd - Pointer to a grid object
!
! NOTES
!
! Valid data types are (see module 'types'):
!    DATATYPE_INT
!    DATATYPE_REAL
!
! SOURCE

function grid_ReadSurferGrid_fn ( sFileName, iDataType ) result ( pGrd )

  ! ARGUMENTS
  character (len=*), intent(in) :: sFileName          ! Name of the grid input file
  integer (kind=c_int), intent(in) :: iDataType       ! T_GRID_INT or T_GRID_REAL
  ! RETURN VALUE
  type (T_GENERAL_GRID), pointer :: pGrd
  ! LOCALS
  character (len=4) :: sSentinel                      ! Better be "DSAA" for SURFER!
  integer (kind=c_int) :: iStat                       ! For "iostat="
  integer (kind=c_int) :: iNX,iNY                     ! Grid dimensions
  integer (kind=c_int) :: iCol,iRow                         ! Loop indices for grid reading
  real (kind=c_double) :: rX0,rX1                       ! Limits in X
  real (kind=c_double) :: rY0,rY1                       ! Limits in Y
  real (kind=c_float) :: rZ0,rZ1                       ! Limits in Z (not used)
  logical (kind=c_bool) :: lFileExists

  inquire(file=trim(sFileName), EXIST=lFileExists)
  call assert( lFileExists, "The Surfer ASCII grid file "//dquote(sFilename)// &
    " could not be found.",trim(__FILE__),__LINE__)

  open ( LU_GRID, iostat=iStat, file=trim(sFileName) )
  call Assert( LOGICAL(iStat == 0,kind=c_bool), &
     "Could not open input file " // trim(sFileName) )

  read ( unit=LU_GRID, fmt=*, iostat=iStat ) sSentinel
  call Assert ( LOGICAL(iStat == 0,kind=c_bool), &
     "Could not read first record of SURFER grid" )
  call Assert ( LOGICAL(trim(sSentinel) == "DSAA",kind=c_bool), &
     "This is not a SURFER grid" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) iNX, iNY
  call Assert ( LOGICAL(iStat == 0,kind=c_bool), &
     "Error reading SURFER grid dimensions" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) rX0, rX1
  call Assert ( LOGICAL(iStat == 0,kind=c_bool), &
     "Error reading SURFER X limits" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) rY0, rY1
  call Assert ( LOGICAL(iStat == 0,kind=c_bool), &
     "Error reading SURFER y limits" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) rZ0, rZ1
  call Assert ( LOGICAL(iStat == 0,kind=c_bool), &
     "Error reading SURFER Z limits" )

  pGrd => grid_Create ( iNX, iNY, rX0, rY0, rX1, rY1, iDataType )
  select case ( iDataType )
      case ( DATATYPE_INT )
          do iRow=1, iNY
              read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%iData(:,iRow)
              call Assert ( iStat == 0, "Failed to read integer grid data" )
          end do
      case ( DATATYPE_REAL )
          do iRow=1, iNY
              read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%rData(:,iRow)
              call Assert ( iStat == 0, "Failed to read real grid data" )
          end do
      case default
          call Assert ( lFALSE, "Internal error -- illegal SURFER grid data type" )
  end select

  call Assert(LOGICAL(iNX>0,kind=c_bool),"Must have a non-zero number of columns surfer grid file...")
  call Assert(LOGICAL(iNY>0,kind=c_bool),"Must have a non-zero number of rows in a surfer grid file...")

  pGrd%rGridCellSize = (rX1-rX0)/iNX

  close ( unit=LU_GRID, iostat=iStat )
  call Assert ( iStat == 0, "Failed to close grid file" )

end function grid_ReadSurferGrid_fn


subroutine grid_ReadSurferGrid_sub ( sFileName, pGrd )

  ! ARGUMENTS
  character (len=*), intent(in) :: sFileName          ! Name of the grid input file
  type (T_GENERAL_GRID), pointer :: pGrd

  ! LOCALS
  character (len=4) :: sSentinel                      ! Better be "DSAA" for SURFER!
  integer (kind=c_int) :: iStat                       ! For "iostat="
  integer (kind=c_int) :: iNX,iNY                     ! Grid dimensions
  integer (kind=c_int) :: iCol,iRow                         ! Loop indices for grid reading
  real (kind=c_double) :: rX0,rX1                       ! Limits in X
  real (kind=c_double) :: rY0,rY1                       ! Limits in Y
  real (kind=c_float) :: rZ0,rZ1                       ! Limits in Z (not used)
  logical (kind=c_bool) :: lFileExists

  inquire(file=trim(sFileName), EXIST=lFileExists)
  call assert( lFileExists, "The Surfer ASCII grid file "//dquote(sFilename)// &
    " could not be found.",trim(__FILE__),__LINE__)

  open ( LU_GRID, iostat=iStat, file=trim(sFileName) )
  call Assert( LOGICAL(iStat == 0,kind=c_bool), &
     "Could not open input file " // trim(sFileName) )

  read ( unit=LU_GRID, fmt=*, iostat=iStat ) sSentinel
  call Assert ( LOGICAL(iStat == 0,kind=c_bool), &
     "Could not read first record of SURFER grid" )
  call Assert ( LOGICAL(trim(sSentinel) == "DSAA",kind=c_bool), &
     "This is not a SURFER grid" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) iNX, iNY
  call Assert ( LOGICAL(iStat == 0,kind=c_bool), &
     "Error reading SURFER grid dimensions" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) rX0, rX1
  call Assert ( LOGICAL(iStat == 0,kind=c_bool), &
     "Error reading SURFER X limits" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) rY0, rY1
  call Assert ( LOGICAL(iStat == 0,kind=c_bool), &
     "Error reading SURFER y limits" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) rZ0, rZ1
  call Assert ( LOGICAL(iStat == 0,kind=c_bool), &
     "Error reading SURFER Z limits" )

  select case ( pGrd%iDataType )
      case ( DATATYPE_INT )
          do iRow=1, iNY
              read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%iData(:,iRow)
              call Assert ( iStat == 0, "Failed to read integer grid data" )
          end do
      case ( DATATYPE_REAL )
          do iRow=1, iNY
              read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%rData(:,iRow)
              call Assert ( iStat == 0, "Failed to read real grid data" )
          end do
      case default
          call Assert ( lFALSE, "Internal error -- illegal SURFER grid data type" )
  end select

  call Assert(LOGICAL(iNX>0,kind=c_bool),"Must have a non-zero number of grid cells in a surfer grid file...")
  call Assert(LOGICAL(iNY>0,kind=c_bool),"Must have a non-zero number of grid cells in a surfer grid file...")

  close ( unit=LU_GRID, iostat=iStat )
  call Assert ( iStat == 0, "Failed to close grid file" )

end subroutine grid_ReadSurferGrid_sub


subroutine grid_WriteGrid(sFilename, pGrd, iOutputFormat)

  ! [ ARGUMENTS ]
  character (len=*),intent(in) :: sFilename
  type (T_GENERAL_GRID), pointer :: pGrd
  integer (kind=c_int) :: iOutputFormat

  if ( iOutputFormat == OUTPUT_ARC ) then

    call grid_WriteArcGrid(sFilename, pGrd)

  elseif ( iOutputFormat == OUTPUT_SURFER ) then

    call grid_WriteSurferGrid(sFilename, pGrd)

  endif

end subroutine grid_WriteGrid


subroutine grid_WriteArcGrid(sFilename, pGrd)

  ! [ ARGUMENTS ]
  character (len=*),intent(in) :: sFilename
  type (T_GENERAL_GRID), pointer :: pGrd


  ! [ LOCALS ]
  integer (kind=c_int) :: iCol,iRow
  integer (kind=c_int) :: iNumCols, iNumRows
  integer (kind=c_int) ::  iStat
  character(len=256) :: sBuf

  if ( pGrd%iDataType == DATATYPE_INT ) then
    iNumCols = size(pGrd%iData,1)
    iNumRows = size(pGrd%iData,2)
  elseif ( pGrd%iDataType == DATATYPE_REAL ) then
    iNumCols = size(pGrd%rData,1)
    iNumRows = size(pGrd%rData,2)
  else
    call assert(lFALSE, "Internal programming error - Unsupported grid type", &
      trim(__FILE__), __LINE__)
  endif

  ! dynamically create the Fortran output format
  write(sBuf,FMT="(a,a,a)") '(',TRIM(int2char(iNumCols)),'(a,1x))'

  open ( LU_TEMP, file=sFilename, iostat=istat, status="REPLACE" )
  call Assert( istat==0, "Could not open output file "//dQuote(sFilename), &
      TRIM(__FILE__),__LINE__)

  write ( unit=LU_TEMP, fmt="('NCOLS ',i10)", iostat=istat ) iNumCols
  call Assert( istat==0, "Error writing grid file header", trim(__FILE__), __LINE__)

  write ( unit=LU_TEMP, fmt="('NROWS ',i10)", iostat=istat ) iNumRows
  call Assert( istat==0, "Error writing grid file header", trim(__FILE__), __LINE__)

  write ( unit=LU_TEMP, fmt="('XLLCORNER ',f14.3)", iostat=istat ) pGrd%rX0
  call Assert( istat==0, "Error writing X limits", trim(__FILE__), __LINE__)

  write ( unit=LU_TEMP, fmt="('YLLCORNER ',f14.3)", iostat=istat ) pGrd%rY0
  call Assert( istat==0, "Error writing Y limits", trim(__FILE__), __LINE__)

  write ( unit=LU_TEMP, fmt="('CELLSIZE ',f14.3)", iostat=istat ) pGrd%rGridCellSize
  call Assert( istat==0, "Error writing cell size", trim(__FILE__), __LINE__)

  if ( pGrd%iDataType == DATATYPE_INT ) then

    write ( unit=LU_TEMP, fmt="('NODATA_VALUE ',i14)", iostat=istat ) pGrd%iNoDataValue
    call Assert( istat==0, "Error writing NODATA value", trim(__FILE__), __LINE__)
    do iRow=1,iNumRows
      write( unit=LU_TEMP, fmt=TRIM(sBuf), iostat=istat ) &
        (TRIM( asCharacter(pGrd%iData(iCol,iRow) ) ),iCol=1,iNumCols)
      call Assert( istat==0, "Error writing Arc ASCII INTEGER grid data", &
        trim(__FILE__), __LINE__)
    end do

  elseif ( pGrd%iDataType == DATATYPE_REAL ) then

    write ( unit=LU_TEMP, fmt="('NODATA_VALUE ',f14.3)", iostat=istat ) pGrd%rNoDataValue
    call Assert( istat==0, "Error writing NODATA value", trim(__FILE__), __LINE__)
    do iRow=1,iNumRows
      write( unit=LU_TEMP, fmt=TRIM(sBuf), iostat=istat ) &
        (TRIM(asCharacter( pGrd%rData(iCol,iRow) )),iCol=1,iNumCols)
      call Assert( istat==0, "Error writing Arc ASCII REAL grid data", &
        trim(__FILE__), __LINE__)
    end do

  endif

  close (unit=LU_TEMP)

end subroutine grid_WriteArcGrid


subroutine grid_WriteSurferGrid(sFilename, pGrd)

  ! [ ARGUMENTS ]
  character (len=*),intent(in) :: sFilename
  type (T_GENERAL_GRID), pointer :: pGrd

  ! [ LOCALS ]
  integer (kind=c_int) :: iCol,iRow
  integer (kind=c_int) :: iNumCols, iNumRows
  integer (kind=c_int) ::  iStat
  character(len=256) :: sBuf
  real (kind=c_float) :: rHalfCell

  if ( pGrd%iDataType == DATATYPE_INT ) then
    iNumCols = size(pGrd%iData,1)
    iNumRows = size(pGrd%iData,2)
  elseif ( pGrd%iDataType == DATATYPE_REAL ) then
    iNumCols = size(pGrd%rData,1)
    iNumRows = size(pGrd%rData,2)
  else
    call assert(lFALSE, "Internal programming error - Unsupported grid type", &
      trim(__FILE__), __LINE__)
  endif

  rHalfCell = pGrd%rGridCellSize * rHALF

  ! dynamically create the Fortran output format
  write(sBuf,FMT="(a,a,a)") '(',TRIM(int2char(iNumCols)),'(a,1x))'

  open ( LU_TEMP, file=sFilename, iostat=istat, status="REPLACE" )
  call Assert( istat==0, "Could not open output file "//dQuote(sFilename), &
      TRIM(__FILE__),__LINE__)

  write ( unit=LU_TEMP, fmt="('DSAA')", iostat=istat )
  call Assert( istat==0, "Error writing SURFER header", &
    trim(__FILE__), __LINE__)

  write ( unit=LU_TEMP, fmt="(2i8)", iostat=istat ) iNumCols, iNumRows
  call Assert( istat==0, "Error writing SURFER dimensions", &
    trim(__FILE__), __LINE__)

  write ( unit=LU_TEMP, fmt="(2f14.3)", iostat=istat ) &
           pGrd%rX0 + rHalfCell , pGrd%rX1 - rHalfCell
  call Assert( istat==0, "Error writing SURFER X limits", &
    trim(__FILE__), __LINE__)

  write ( unit=LU_TEMP, fmt="(2f14.3)", iostat=istat ) &
           pGrd%rY0 + rHalfCell, pGrd%rY1 - rHalfCell
  call Assert( istat==0, "Error writing SURFER Y limits", &
    trim(__FILE__), __LINE__)

  if ( pGrd%iDataType == DATATYPE_INT ) then

    write ( unit=LU_TEMP, fmt="(2i14)", iostat=istat ) minval(pGrd%iData),maxval(pGrd%iData)
    call Assert( istat==0, "Error writing SURFER Z limits", &
      trim(__FILE__), __LINE__)


    do iRow=iNumRows,1,-1
      write( unit=LU_TEMP, fmt=TRIM(sBuf), iostat=istat ) &
        (TRIM(asCharacter(pGrd%iData(iCol,iRow) ) ),iCol=1,iNumCols)
      call Assert( istat==0, "Error writing SURFER grid data" , &
        trim(__FILE__), __LINE__)
    end do

  elseif ( pGrd%iDataType == DATATYPE_REAL ) then

    write ( unit=LU_TEMP, fmt="(2f14.3)", iostat=istat ) minval(pGrd%rData),maxval(pGrd%rData)
    call Assert( istat==0, "Error writing SURFER Z limits", &
      trim(__FILE__), __LINE__)

    do iRow=iNumRows,1,-1
      write( unit=LU_TEMP, fmt=TRIM(sBuf), iostat=istat ) &
        (TRIM( asCharacter(pGrd%rData(iCol,iRow) ) ),iCol=1,iNumCols)
      call Assert( istat==0, "Error writing SURFER grid data" , &
        trim(__FILE__), __LINE__)
    end do

  endif

  close (unit=LU_TEMP)

end subroutine grid_WriteSurferGrid
!!***
!--------------------------------------------------------------------------
!!****f* grid/grid_Conform
! NAME
!   grid_WriteArcGrid - Reads an Surfer ASCII grid of a specified type.
!
! SYNOPSIS
!   Write an ARC ASCII grid of specified data type
!
! INPUTS
!
!  pGrd1 - Pointer to first grid
!  pGrd2 - Pointer to second grid
!  rTolerance - OPTIONAL - Allowable tolerance between the two grids
!
! OUTPUTS
!  lConform - Logical, TRUE if the grids are compatible within the
!    specified tolerance, FALSE if otherwise
!
! SOURCE

function grid_Conform ( pGrd1, pGrd2, rTolerance ) result ( lConform )
  !! Returns .true. if the T_GRID objects conform (in terms of cell sizes and extents)
  !! The optional argument rTolerance is the precision for checking the (floating-point)
  !! extent coordinates (this defaults to rDEFAULT_TOLERANCE, below). The tolerance
  !! is set to abs(rTolerance * ( rX1-rX1 ) )
  ! ARGUMENTS
  type (T_GENERAL_GRID), pointer :: pGrd1,pGrd2
  real (kind=c_float), intent(in), optional :: rTolerance
  ! RETURN VALUE
  logical (kind=c_bool) :: lConform
  ! LOCALS
  real (kind=c_float) :: rTol
  real (kind=c_float), parameter :: rDEFAULT_TOLERANCE = 0.5_c_float

  if ( present ( rTolerance ) ) then
      rTol = rTolerance * ( pGrd1%rX1 - pGrd1%rX0 )
  else
      rTol = rDEFAULT_TOLERANCE * ( pGrd1%rX1 - pGrd1%rX0 )
  end if

  lConform = lTRUE

  if ( pGrd1%iNX /= pGrd2%iNX ) then
    lConform = lFALSE
    write(LU_LOG,fmt="(a)") "Unequal number of columns between grids"
  endif

  if ( pGrd1%iNY /= pGrd2%iNY ) then
    lConform = lFALSE
    write(LU_LOG,fmt="(a)") "Unequal number of rows between grids"
  endif

  if( abs ( pGrd1%rX0 - pGrd2%rX0 ) > rTol ) then
     write(LU_LOG,fmt="(a)") "Lower left-hand side X coordinates don't match:"
     write(LU_LOG,fmt="('Grid 1 value: ',f12.3,'; grid 2 value: ',f12.3)") &
       pGrd1%rX0, pGrd2%rX0
    lConform = lFALSE
  endif

  if( abs ( pGrd1%rY0 - pGrd2%rY0 ) > rTol ) then
    write(LU_LOG,fmt="(a)") "Lower left-hand side Y coordinates don't match:"
    write(LU_LOG,fmt="('Grid 1 value: ',f12.3,'; grid 2 value: ',f12.3)") &
      pGrd1%rY0, pGrd2%rY0
    lConform = lFALSE
  endif

  if( abs ( pGrd1%rX1 - pGrd2%rX1 ) > rTol ) then
     write(LU_LOG,fmt="(a)") "Upper right-hand side X coordinates don't match:"
     write(LU_LOG,fmt="('Grid 1 value: ',f12.3,'; grid 2 value: ',f12.3)") &
       pGrd1%rX1, pGrd2%rX1
    lConform = lFALSE
  endif

  if( abs ( pGrd1%rY1 - pGrd2%rY1 ) > rTol ) then
    write(LU_LOG,fmt="(a)") "Upper right-hand side X coordinates don't match:"
    write(LU_LOG,fmt="('Grid 1 value: ',f12.3,'; grid 2 value: ',f12.3)") &
      pGrd1%rY1, pGrd2%rY1
    lConform = lFALSE
  endif

end function grid_Conform

function grid_CompletelyCover( pBaseGrd, pOtherGrd, rTolerance ) result ( lCompletelyCover )
  !! Returns .true. if the T_GRID objects conform (in terms of cell sizes and extents)
  !! The optional argument rTolerance is the precision for checking the (floating-point)
  !! extent coordinates (this defaults to rDEFAULT_TOLERANCE, below). The tolerance
  !! is set to abs(rTolerance * ( rX1-rX1 ) )
  ! ARGUMENTS
  type (T_GENERAL_GRID), pointer :: pBaseGrd,pOtherGrd
  real (kind=c_float), intent(in), optional :: rTolerance
  ! RETURN VALUE
  logical (kind=c_bool) :: lCompletelyCover
  ! LOCALS
  real (kind=c_float) :: rTol
  real (kind=c_float) :: rDEFAULT_TOLERANCE

!  rDEFAULT_TOLERANCE = pBaseGrd%rGridCellSize / 2.
  rDEFAULT_TOLERANCE = rZERO

  if ( present ( rTolerance ) ) then
      rTol = rTolerance
  else
      rTol = rDEFAULT_TOLERANCE
  end if

  if ( (pBaseGrd%rX0 - pOtherGrd%rX0 >= rTol) .and. &
       (pBaseGrd%rY0 - pOtherGrd%rY0 >= rTol) .and. &
       (pOtherGrd%rX1 - pBaseGrd%rX1 >= rTol) .and. &
       (pOtherGrd%rY1 - pBaseGrd%rY1 >= rTol) ) then
      lCompletelyCover = lTRUE
  else
      lCompletelyCover = lFALSE

      call echolog("Extents of the grid file "//dquote(pOtherGrd%sFilename) &
          //" do not cover the base grid extents.")

      call echolog(" ")
      call echolog("    BASE GRID EXTENTS    ANCILLARY GRID EXTENTS")
      call echolog("X0:  "//trim(asCharacter(pBaseGrd%rX0))//"      "//trim(asCharacter(pOtherGrd%rX0)) )
      call echolog("Y0:  "//trim(asCharacter(pBaseGrd%rY0))//"      "//trim(asCharacter(pOtherGrd%rY0)) )
      call echolog("X1:  "//trim(asCharacter(pBaseGrd%rX1))//"      "//trim(asCharacter(pOtherGrd%rX1)) )
      call echolog("Y1:  "//trim(asCharacter(pBaseGrd%rY1))//"      "//trim(asCharacter(pOtherGrd%rY1)) )
      call echolog(" ")

  end if

end function grid_CompletelyCover

!!***
!--------------------------------------------------------------------------
!!****s* grid/grid_LookupColumn
! NAME
!  grid_LookupColumn - Finds the column position of the value rXval
!     in the grid
!
! SYNOPSIS
!  Finds the column position of the value rXval in the grid and returns:
!   iBefore =  column before the value rYval
!   iAfter  =  column after the value rYval
!   rFrac    =  fraction of the distance between iBefore and iAfter
!  If iBefore or iAfter are outside the grid, they're set to -1
!
! INPUTS
!  pGrd - Pointer to a data grid
!  rXval - Real value to be interpolated from the gridded data.
!  rTolerance - OPTIONAL - Allowable tolerance between the two grids
!
! OUTPUTS
!  iBefore - column before the value rXval
!  iAfter - column after the value rXval
!  rFrac - fraction of the distance between iBefore and iAfter
!
!  If iBefore or iAfter are outside the grid, they're set to -1
!
! NOTES
!  The grid lookup functions were designed specifically to work with the
!  Thornthwaite-Mather soil moisture retention table. The functions will
!  work with any data that are similarly arranged.
!
!  If X=0.65, for example, and the following table were queried, the subroutine
! will return: iBefore=1, iAfter=2, rFrac=0.3
!
!                 X=Max SM Capacity
!
!                0.50  1.00  1.50
!               _________________
!          0.0 | 0.50  1.00  1.50
!  Y=APWL  0.1 | 0.45  0.90  1.40
!          0.2 | 0.40  0.80  1.30
!
! SOURCE

subroutine grid_LookupColumn(pGrd,rXval,iBefore,iAfter,rFrac)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd
  real (kind=c_float),intent(in) :: rXval
  integer (kind=c_int),intent(out) :: iBefore,iAfter
  real (kind=c_float),intent(out) :: rFrac
  ! [ LOCALS ]
  real (kind=c_float) :: rColPosition

  rColPosition = (pGrd%iNX - 1) * (rXval - pGrd%rX0) / (pGrd%rX1 - pGrd%rX0)
  rFrac = rZERO
  iBefore = floor(rColPosition) + 1
  iAfter = iBefore + 1
  if ( iBefore > pGrd%iNX .or. iBefore < 1) then
    iBefore = -1
    iAfter = -1
  else if ( iAfter > pGrd%iNX ) then
    iAfter = -1
  else
    rFrac = mod(rColPosition,rONE)
  end if

end subroutine grid_LookupColumn
!!***
!--------------------------------------------------------------------------
!!****s* grid/grid_LookupRow
! NAME
!  grid_LookupRow - Finds the row position of the value rYval
!     in the grid
!
! SYNOPSIS
!  Finds the column position of the value rYval in the grid and returns:
!    iBefore =  column before the value rYval
!    iAfter  =  column after the value rYval
!    rFrac    =  fraction of the distance between iBefore and iAfter
!  If iBefore or iAfter are outside the grid, they're set to -1
!
! INPUTS
!  pGrd - Pointer to a data grid
!  rYval - Real value to be interpolated from the gridded data.
!  rTolerance - OPTIONAL - Allowable tolerance between the two grids
!
! OUTPUTS
!  iBefore - column before the value rXval
!  iAfter - column after the value rXval
!  rFrac - fraction of the distance between iBefore and iAfter
!
!  If iBefore or iAfter are outside the grid, they're set to -1
!
! NOTES
!  The grid lookup functions were designed specifically to work with the
!  Thornthwaite-Mather soil moisture retention table. The functions will
!  work with any data that are similarly arranged.
!
!  If Y=0.15, for example, and the following table were queried, the subroutine
! will return: iBefore=2, iAfter=3, rFrac=0.5
!
!                 X=Max SM Capacity
!
!                0.50  1.00  1.50
!               _________________
!          0.0 | 0.50  1.00  1.50
!  Y=APWL  0.1 | 0.45  0.90  1.40
!          0.2 | 0.40  0.80  1.30
!
! SOURCE

subroutine grid_LookupRow(pGrd,rYval,iBefore,iAfter,rFrac)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd
  real (kind=c_float),intent(in) :: rYval
  integer (kind=c_int),intent(out) :: iBefore,iAfter
  real (kind=c_float),intent(out) :: rFrac
  ! [ LOCALS ]
  real (kind=c_float) :: rColPosition

  rColPosition = (pGrd%iNY - 1) * (pGrd%rY1 - rYval) / (pGrd%rY1 - pGrd%rY0)
  rFrac = rZERO
  iBefore = floor(rColPosition) + 1
  iAfter = iBefore + 1
  if ( iBefore > pGrd%iNY ) then
    iBefore = -1
    iAfter = -1
  else if ( iBefore < 1) then
    iBefore = 1
    iAfter = 1
  else if ( iAfter > pGrd%iNY ) then
    iAfter = iBefore
  else
    rFrac = mod(rColPosition,rONE)
  end if

  return
end subroutine grid_LookupRow


!> Call PROJ4 to transform coordinates.
!> @details This subroutine calls a Fortran wrapper to the C library
!> PROJ4. A set of input coordinates is transformed to a different
!> coordinate system.
!> @param[inout] pGrd
!> @param[in] sFromPROJ4
!> @param[in] sToPROJ4
subroutine grid_Transform(pGrd, sFromPROJ4, sToPROJ4 )

  use, intrinsic :: iso_c_binding

  type ( T_GENERAL_GRID ),pointer :: pGrd
  character (len=*) :: sFromPROJ4, sToPROJ4
  character (kind=C_CHAR, len=len_trim(sFromPROJ4)) :: csFromPROJ4
  character (kind=C_CHAR, len=len_trim(sToPROJ4)) :: csToPROJ4

  ! [ LOCALS ]
  integer (kind=c_int) :: iRetVal
  integer (kind=c_int) :: i
  logical (kind=c_bool), dimension(pGrd%iNY, pGrd%iNX) :: lMask

  call grid_PopulateXY(pGrd)

  csFromPROJ4 = trim(sFromPROJ4)
  csToPROJ4 = trim(sToPROJ4)

  !lMask = lTRUE

  !x = pack(pGrd%rX, lMask)
  !y = pack(pGrd%rY, lMask)

  !> PROJ4 expects unprojected coordinates (i.e. lat lon) to be provided
  !> in RADIANS. Therefore, we convert to radians prior to the call...
  if( index(string=csFromPROJ4, substring="latlon") > 0 &
      .or. index(string=csFromPROJ4, substring="lonlat") > 0 ) then

    pGrd%rX = pGrd%rX * dpPI_OVER_180
    pGrd%rY = pGrd%rY * dpPI_OVER_180

  endif

  iRetVal = pj_init_and_transform(csFromPROJ4//C_NULL_CHAR, &
                                  csToPROJ4//C_NULL_CHAR, &
                                  int(pGrd%iNumGridCells, kind=c_long), pGrd%rX, pGrd%rY)

  call grid_CheckForPROJ4Error(iRetVal, sFromPROJ4, sToPROJ4)

  ! transfer coordinate values back into an array structure
!  pGrd%rY = unpack(y, lMask, pGrd%rY)

  ! transfer coordinate values back into an array structure
!  pGrd%rX = unpack(x, lMask, pGrd%rX)

  ! now update the grid boundaries based on the transformed coordinate values
  pGrd%rGridCellSize = ( maxval(pGrd%rX) - minval(pGrd%rX) ) &
             / real(pGrd%iNX - 1, kind=c_double)
  pGrd%rX0 = minval(pGrd%rX) - pGrd%rGridCellSize / 2_c_float
  pGrd%rX1 = maxval(pGrd%rX) + pGrd%rGridCellSize / 2_c_float
  pGrd%rY0 = minval(pGrd%rY) - pGrd%rGridCellSize / 2_c_float
  pGrd%rY1 = maxval(pGrd%rY) + pGrd%rGridCellSize / 2_c_float

  ! finally, change the projection string to reflect the new coordinate system
  pGrd%sPROJ4_string = trim(sToPROJ4)

end subroutine grid_Transform

!--------------------------------------------------------------------------

subroutine grid_CheckForPROJ4Error(iRetVal, sFromPROJ4, sToPROJ4)

  ! [ ARGUMENTS ]
  integer (kind=c_int) :: iRetVal
  character (len=*) :: sFromPROJ4
  character (len=*) :: sToPROJ4

  ! [ LOCALS ]
  integer (kind=c_int) :: i
  logical (kind=c_bool) :: lFound
  character (len=256) :: sErrorMessage

  sErrorMessage = ""

  if (iRetVal /= 0) then

    write(sErrorMessage,fmt="(a,a,a,a,a)") &
      "There was an error transforming a grid from this projection:~", &
      dquote(sFromPROJ4), "    to:~", &
      dquote(sToPROJ4)

    do i=1,49
      if(iRetVal == tErrorMessage(i)%iErrorNumber) then
          sErrorMessage = sErrorMessage &
          //"~ PROJ4 Error number "//int2char(iRetVal)//" reported.~" &
          //"   ==> error description: "//trim(tErrorMessage(i)%cErrorMessageText)
          lFound = lTRUE
        exit
      endif
    enddo

    call assert(lFALSE, trim(sErrorMessage), trim(__FILE__), __LINE__)

  endif

end subroutine grid_CheckForPROJ4Error

!!***
!--------------------------------------------------------------------------
!!****f* grid/grid_Interpolate
! NAME
!  grid_Interpolate - Returns an interpolated table value given a row
!     and column position
!

! SYNOPSIS
!  Returns an interpolated table value given a row and column position
!
! INPUTS
!  pGrd - Pointer to a data grid
!  rXval - X value for which we want to interpolate table data.
!  rYval - Y value for which we want to interpolate table data.
!
! OUTPUTS
!   rValue - Value interpolated from input grid.
!
! NOTES
!  The grid lookup functions were designed specifically to work with the
!  Thornthwaite-Mather soil moisture retention table. The functions will
!  work with any data that are similarly arranged.
!
! SOURCE

function grid_Interpolate(pGrd,rXval,rYval) result ( rValue )
  !! Interpolates values from the grid 'grd' for the row position 'yval' and
  !! the column position 'xval'. Assumes that the row and column spacing
  !! are constant. Applicable only to DATATYPE_REAL grids.
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd
  real (kind=c_float),intent(in) :: rXval,rYval
  ! [ RETURN VALUE ]
  real (kind=c_float) :: rValue
  ! [ LOCALS ]
  integer (kind=c_int) :: ib,jb,ia,ja
  real (kind=c_float) :: ylocal,u,v

  call grid_LookupColumn(pGrd,rXval,ib,ia,u)

  call Assert (ib>0 .and. ia>0 .and. ib <= ubound(pGrd%rData,1) &
     .and. ia <= ubound(pGrd%rData,1), &
    "Internal programming error: illegal bounds caught~requested column value " &
    //trim(real2char(rValue=rXval, sFmt="F12.3")) &
    //" out of range", trim(__FILE__), __LINE__)

  ! In some cases, when things really dry out, the y value
  ! goes out of range - enforce bounds.
  if ( rYval < pGrd%rY0 ) then
    ylocal = pGrd%rY0
  else if ( rYval > pGrd%rY1 ) then
    ylocal = pGrd%rY1
  else
    ylocal = rYval
  end if

  call grid_LookupRow(pGrd=pGrd, &
                      rYval=ylocal, &
                      iBefore=jb, &
                      iAfter=ja, &
                      rFrac=v)

  call Assert (jb>0 .and. ja>0 .and. jb <= ubound(pGrd%rData,2) &
     .and. ja <= ubound(pGrd%rData,2), &
    "Internal programming error: illegal bounds caught~requested row value " &
    //trim(real2char(rValue=rXval, sFmt="F12.3")) &
    //" out of range", trim(__FILE__), __LINE__)

  rValue = (rONE-u) * (rONE-v) * pGrd%rData(ib,jb)   + &
              u  * (rONE-v) * pGrd%rData(ib,ja)   + &
           (rONE-u) *       v  * pGrd%rData(ia,jb)   + &
              u  *       v  * pGrd%rData(ia,ja)

end function grid_Interpolate
!!***
!--------------------------------------------------------------------------
!!****f* grid/grid_SearchColumn
! NAME
!  grid_SearchColumn - Returns the value of table Y value given the X value
!     and the table Z value
!
! SYNOPSIS
!  Searches in the y-direction, given the x value and the table value
!  z, and returns the value of y. Assumes that the row and
!  column spacing are constant. The search begins at the _top_ of the
!  table (y=pGrd%rY1). Applicable only to DATATYPE_REAL grids.
!  Parameter 'rNoData' is the missing value code for the grid.

! INPUTS
!  pGrd - Pointer to a data grid
!  rXval - X value corresponding to one or more rows of the table.
!  rZval - Table value to scan for.
!  rNoData - Real value assigned to "No Data" cells.
!
! OUTPUTS
!  rYval - Returned value associated with the input x and z values.
!
! NOTES
!  The grid lookup functions were designed specifically to work with the
!  Thornthwaite-Mather soil moisture retention table. The functions will
!  work with any data that are similarly arranged.
!
!  If X=1.00 and Z=0.90, for example, and the following table were queried,
!  the subroutine will return: rValue=0.1
!
!                 X=Max SM Capacity
!
!                 0.50  1.00  1.50
!                _________________
!         0.0 | 0.50  1.00  1.50
!  Y=APWL 0.1 | 0.45  0.90  1.40
!         0.2 | 0.40  0.80  1.30
!
! SOURCE

function grid_SearchColumn(pGrd,rXval,rZval,rNoData) result ( rValue )
  !! Searches in the y-direction, given the x value 'xval', for the value
  !! 'zval', and returns the value of y in 'ry'. Assumes that the row and
  !! column spacing are constant. The search begins at the _top_ of the
  !! table (y=pGrd%rY1). Applicable only to DATATYPE_REAL grids.
  !! Parameter 'rmv' is the missing value code for the grid.
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd
  real (kind=c_float),intent(in) :: rXval,rZval,rNoData
  ! [ RETURN VALUE ]
  real (kind=c_float) :: rValue
  ! [ LOCALS ]
  integer (kind=c_int) :: ib,ia,istat,iRow
  real (kind=c_float) :: u,v,frac,rprev
  real (kind=c_float),dimension(:),allocatable :: rCol
  character (len=128) :: buf

  ! allocate space for all ROWS of data that come from a single COLUMN
  allocate ( rCol(pGrd%iNY), stat=istat )
  call Assert( LOGICAL(istat==0,kind=c_bool), &
     "Couldn't allocate temporary array" )

  call grid_LookupColumn(pGrd=pGrd, &
                         rXval=rXval, &
                         iBefore=ib, &
                         iAfter=ia, &
                         rFrac=u)

#ifdef DEBUG_PRINT
  write(UNIT=LU_LOG,FMT=*)'lookup ',rXval,ib,ia
#endif

  call Assert (ib>0 .and. ia>0 .and. ib <= ubound(pGrd%rData,1) &
     .and. ia <= ubound(pGrd%rData,1), &
    "Internal programming error: requested X value " &
    //trim(real2char(rValue=rXval, sFmt="F12.3")) &
    //" out of range", trim(__FILE__), __LINE__)

  call assert(ubound(rCol,1) == ubound(pGrd%rData,2), &
    "Internal programming error: upper bound of rCol /= upper " &
    //"bound of first array element of pGrd%rData~" &
    //"ubound(rCol)="//trim(int2char(ubound(rCol,1))) &
    //"~ubound(pGrd%rData)="//trim(int2char(ubound(pGrd%rData,1))), &
    trim(__FILE__), __LINE__)

  ! interpolate the column of values based on the columns of values
  ! that bracket the real value rXval
!  rCol = u*pGrd%rData(:,ia) + (rONE-u)*pGrd%rData(:,ib)
  rCol = u * pGrd%rData(ia,:) + (rONE-u)*pGrd%rData(ib,:)
  ! Fix missing values

  do iRow=1,pGrd%iNY
    if ( pGrd%rData(ia, iRow) == rNoData .and. pGrd%rData(ib, iRow) == rNoData ) then
      rCol(iRow) = rNoData
    else if ( pGrd%rData(ib,iRow) == rNoData .and. u>0.9_c_float ) then
      rCol(iRow) = pGrd%rData(ia,iRow)
    else if ( pGrd%rData(ia,iRow) == rNoData .and. u<0.1_c_float ) then
      rCol(iRow) = pGrd%rData(ib,iRow)
    else if ( pGrd%rData(ia,iRow) == rNoData .or. pGrd%rData(ib,iRow) == rNoData ) then
      rCol(iRow) = rNoData
    end if
  end do

  ! Search for the specified value, assuming that the data vary monotonically
  ! Skip MVs
  rprev = rNoData
  rValue = rNoData
  do iRow=1,pGrd%iNY
    if ( rCol(iRow) == rNoData ) then
      if ( rprev == rNoData ) then
        ! keep skipping missing values...
        continue
      else
        ! end of the line -- we didn't find the value, so return the limit
        v = real(iRow-1) / real(pGrd%iNY-1)
        rValue = (rONE-v)*pGrd%rY0 + v*pGrd%rY1
        exit
      endif
    else
      if ( rprev == rNoData ) then
        rprev = rCol(iRow)
      else
        if ( sign(rONE,rCol(iRow)-rZval) /= sign(rONE,rprev-rZval) ) then
          ! Found it!
          frac = (rZval-rCol(iRow-1)) / (rCol(iRow)-rCol(iRow-1))
          ! Note: it's 'iRow-2' in the next expr to account for one-based indexing
          v = ( real(iRow-2)+frac ) / real(pGrd%iNY-1)
          rValue = v*pGrd%rY0 + (rONE-v)*pGrd%rY1
          exit
        else if ( rZval < rprev .and. rCol(iRow) > rprev ) then
          ! Doesn't exist, choose the limit
          v = real(iRow-2) / real(pGrd%iNY-1)
          rValue = v*pGrd%rY0 + (rONE-v)*pGrd%rY1
          exit
        else if ( rZval > rprev .and. rCol(iRow) < rprev ) then
          v = real(iRow-2) / real(pGrd%iNY-1)
          rValue = v*pGrd%rY0 + (rONE-v)*pGrd%rY1
          exit
        else if ( iRow == pGrd%iNY ) then
          v = rONE
          rValue = v*pGrd%rY0 + (rONE-v)*pGrd%rY1
          exit
        end if
      end if
    end if
  end do

  deallocate ( rCol )

  return
end function grid_SearchColumn
!!***
!--------------------------------------------------------------------------
!!****f* grid/grid_LookupReal
! NAME
!  grid_LookupReal - Returns the grid cell value nearest to that for a
!     given a row and column position
!
! SYNOPSIS
!   Returns the grid cell value nearest to that for a given a row and
!   column position.  No interpolation is performed.
!
! INPUTS
!  pGrd - Pointer to a data grid
!  rXval - X value for which we want to interpolate table data.
!  rYval - Y value for which we want to interpolate table data.
!
! OUTPUTS
!   rValue - Value of the grid cell nearest the given row, column combination.
!
! NOTES
!  The grid lookup functions were designed specifically to work with the
!  Thornthwaite-Mather soil moisture retention table. The functions will
!  work with any data that are similarly arranged.
!
! SOURCE

function grid_LookupReal(pGrd,rXval,rYval) result(rValue)
  !! Returns the grid value for the cell containing (rXval,rYval)
  type ( T_GENERAL_GRID ),pointer :: pGrd
  real (kind=c_float),intent(in) :: rXval,rYval
  real (kind=c_float) :: rValue
  integer (kind=c_int) :: iCol,iRow

  iRow = int( pGrd%iNY * (pGrd%rY0 - rYval) / (pGrd%rY0 - pGrd%rY1) ) + 1
  if ( iRow > pGrd%iNY ) iRow = pGrd%iNY
  iCol = int( pGrd%iNX * (rXval - pGrd%rX0) / (pGrd%rX1 - pGrd%rX0) ) + 1
  if ( iCol > pGrd%iNX ) iCol = pGrd%iNX
  rValue = pGrd%rData(iCol,iRow)

  return
end function grid_LookupReal
!!***

!----------------------------------------------------------------------

function grid_GetGridColNum(pGrd,rX)  result(iColumnNumber)

  type ( T_GENERAL_GRID ),pointer :: pGrd
  real (kind=c_double) :: rX
  integer (kind=c_int) :: iColumnNumber

!  print *, "rX: ",rX
!  print *, pGrd%rX0, pGrd%rX1, pGrd%iNX

  iColumnNumber = NINT(real(pGrd%iNX, kind=c_double) &
               * ( rX - pGrd%rX0 ) / (pGrd%rX1 - pGrd%rX0) + 0.5_c_double, kind=c_int)

!               * ( rX - pGrd%rX0 ) / (pGrd%rX1 - pGrd%rX0) + 0.5_c_double, kind=c_int)

!  print *, "iColumnNumber = ", iColumnNumber
!  print *, "calc: ",  real(pGrd%iNX, kind=c_double) &
!               * ( rX - pGrd%rX0 ) / (pGrd%rX1 - pGrd%rX0) + 0.5_c_double
!  print *, "numerator: ", real(pGrd%iNX, kind=c_double) * ( rX - pGrd%rX0 )
!  print *, "numerator (LHS): ", real(pGrd%iNX, kind=c_double)
!  print *, "numerator (RHS): ", rX, pGrd%rX0, ( rX - pGrd%rX0 )
!  print *, "denominator: ", (pGrd%rX1 - pGrd%rX0)

  if ( iColumnNumber < 1 .or. iColumnNumber > pGrd%iNX ) then
    call grid_DumpGridExtent(pGrd)
    write(*, fmt="(a)") "was attempting to find column associated with X: "//trim(asCharacter(rX))
    call assert(lFALSE,  "INTERNAL PROGRAMMING ERROR: Column number out of bounds (value: " &
     //trim(int2char(iColumnNumber))//")", &
     trim(__FILE__), __LINE__)
  endif

end function grid_GetGridColNum

!----------------------------------------------------------------------

function grid_GetGridRowNum(pGrd,rY)  result(iRowNumber)

  type ( T_GENERAL_GRID ),pointer :: pGrd
  real (kind=c_double) :: rY
  integer (kind=c_int) :: iRowNumber

  iRowNumber = pGrd%iNY - NINT(real(pGrd%iNY, kind=c_double) &
               * ( rY - pGrd%rY0 ) / (pGrd%rY1 - pGrd%rY0) - 0.5_c_double, kind=c_int)

  if ( iRowNumber < 1 .or. iRowNumber > pGrd%iNY ) then
    call grid_DumpGridExtent(pGrd)
    write(*, fmt="(a)") "was attempting to find row associated with Y: "//trim(asCharacter(rY))
    call assert(lFALSE,  "INTERNAL PROGRAMMING ERROR: Row number out of bounds (value: " &
     //trim(int2char(iRowNumber))//")", &
     trim(__FILE__), __LINE__)
  endif

end function grid_GetGridRowNum

!----------------------------------------------------------------------

function grid_GetGridColRowNum(pGrd, rX, rY)    result(iColRow)

  type ( T_GENERAL_GRID ),pointer :: pGrd
  real (kind=c_double) :: rX, rY
  integer (kind=c_int), dimension(2) :: iColRow

  ! [ LOCALS ]
  real (kind=c_float) :: rDist, rMinDistance, rDist2

  integer (kind=c_int), save :: iLastColNum, iLastRowNum
  integer (kind=c_int) :: iStartingColNum, iStartingRowNum
  integer (kind=c_int) :: iRowBoundLower, iRowBoundUpper
  integer (kind=c_int) :: iColBoundLower, iColBoundUpper
  integer (kind=c_int) :: iCol, iRow
  integer (kind=c_int) :: iCandidateRow, iCandidateCol
  integer (kind=c_int) :: iStat
  logical (kind=c_bool) :: lChanged

  ! this will get us close to where we need to be; however, since the
  ! transformed grid may contain significant nonlinearity between
  ! adjacent grid coordinates, we need to do a more thorough search
  ! in the neighborhood of these points

  iStartingColNum = grid_GetGridColNum(pGrd,rX)
  iStartingRowNum = grid_GetGridRowNum(pGrd,rY)

  rDist = hypot(pGrd%rX(iStartingColNum,iStartingRowNum) - rX, &
              pGrd%rY(iStartingColNum,iStartingRowNum) - rY)

  ! need to ensure that the leftover column and row numbers from
  ! perhaps an entirely different grid are not used as indices
  if (iLastColNum > 0 .and. iLastColNum <= pGrd%iNX &
     .and. iLastRowNum > 0 .and. iLastRowNum <= pGrd%iNY) then
    rDist2 = hypot(pGrd%rX(iLastColNum,iLastRowNum) - rX, &
              pGrd%rY(iLastColNum,iLastRowNum) - rY)
  else
    rDist2 = rBIGVAL
  endif

  if (rDist > rDist2) then
    iCandidateRow = iLastRowNum
    iCandidateCol = iLastColNum
  else
    iCandidateRow = iStartingRowNum
    iCandidateCol = iStartingColNum
  endif

  rMinDistance = rBIGVAL

  do

    !> need to ensure that whatever bound is calculated
    !> is within the declared array bounds or we get a segfault
    iRowBoundLower = min(max( 1, iCandidateRow - 1), pGrd%iNY)
    iRowBoundUpper = max(min( pGrd%iNY, iCandidateRow + 1), 1)

    iColBoundLower = min(max( 1, iCandidateCol - 1), pGrd%iNX)
    iColBoundUpper = max(min( pGrd%iNX, iCandidateCol + 1), 1)

    lChanged = lFALSE

    do iRow=iRowBoundLower,iRowBoundUpper
      do iCol=iColBoundLower,iColBoundUpper

        rDist = hypot(pGrd%rX(iCol,iRow) - rX, pGrd%rY(iCol,iRow) - rY)

        if (rDist < rMinDistance ) then

          rMinDistance = rDist
          iCandidateCol = iCol
          iCandidateRow = iRow
          lChanged = lTRUE

        endif

      enddo
    enddo

    if (.not. lChanged ) exit

  enddo

!  iLastColNum = iCol
!  iLastRowNum = iRow

  iLastColNum = iCandidateCol
  iLastRowNum = iCandidateRow

!  iColRow(COLUMN) = iCol
!  iColRow(ROW) = iRow
  iColRow(COLUMN) = iCandidateCol
  iColRow(ROW) = iCandidateRow


end function grid_GetGridColRowNum


!----------------------------------------------------------------------

function grid_GetGridX(pGrd,iColumnNumber)  result(rX)

  type ( T_GENERAL_GRID ),pointer :: pGrd
  real (kind=c_double) :: rX
  integer (kind=c_int) :: iColumnNumber

  rX = pGrd%rX0 + pGrd%rGridCellSize * (REAL(iColumnNumber, kind=c_double) - dpHALF)

end function grid_GetGridX

!----------------------------------------------------------------------

function grid_GetGridY(pGrd,iRowNumber)  result(rY)

  type ( T_GENERAL_GRID ),pointer :: pGrd
  real (kind=c_double) :: rY
  integer (kind=c_int) :: iRowNumber

  rY = pGrd%rY1 &
          - pGrd%rGridCellSize * (REAL(iRowNumber, kind=c_double) - dpHALF)

end function grid_GetGridY

!----------------------------------------------------------------------

subroutine grid_PopulateXY(pGrd)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd         ! pointer to model grid
    ! model options, flags, and other settings

  ! [ LOCALS ]
  integer (kind=c_int) :: iCol,iRow
  integer (kind=c_int) :: iStat

  if ( .not. allocated(pGrd%rX) ) then

    ALLOCATE (pGrd%rX(pGrd%iNX, pGrd%iNY), STAT=iStat)
    call Assert( iStat == 0, &
       "Could not allocate memory for x-coordinates within grid data structure", &
       trim(__FILE__), __LINE__)
  endif

  if ( .not. allocated(pGrd%rY) ) then
    ALLOCATE (pGrd%rY(pGrd%iNX, pGrd%iNY), STAT=iStat)
    call Assert( iStat == 0, &
       "Could not allocate memory for y-coordinates within grid data structure", &
       trim(__FILE__), __LINE__)
  endif

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX
      pGrd%rX(iCol, iRow) = grid_GetGridX(pGrd, iCol)
      pGrd%rY(iCol, iRow) = grid_GetGridY(pGrd, iRow)
    enddo
  enddo

end subroutine grid_PopulateXY

!----------------------------------------------------------------------

subroutine grid_DumpGridExtent(pGrd)

  type ( T_GENERAL_GRID ),pointer :: pGrd

  call echolog("---------------------------------------------------")
  call echolog("GRID DETAILS:")
  call echolog("---------------------------------------------------")
  call echolog("file: "//dquote(pGrd%sFilename) )
  call echolog("nx: "//trim(int2char(pGrd%iNX) ) )
  call echolog("ny: "//trim(int2char(pGrd%iNY) ) )
  call echolog("cellsize: "//trim(asCharacter(pGrd%rGridCellSize) ) )
  call echolog("X0: "//trim(asCharacter(pGrd%rX0) ) )
  call echolog("Y0: "//trim(asCharacter(pGrd%rY0) ) )
  call echolog("X1: "//trim(asCharacter(pGrd%rX1) ) )
  call echolog("Y1: "//trim(asCharacter(pGrd%rY1) ) )
  call echolog("Type: "//trim(asCharacter(pGrd%iDataType) ) )
  call echolog("PROJ4 string: "//dquote(pGrd%sPROJ4_string) )
  call echolog("---------------------------------------------------")

end subroutine grid_DumpGridExtent

!----------------------------------------------------------------------

subroutine grid_GridToGrid_int(pGrdFrom, iArrayFrom, pGrdTo, iArrayTo)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrdFrom   ! pointer to source grid
  type ( T_GENERAL_GRID ),pointer :: pGrdTo     ! pointer to destination grid
  integer (kind=c_int), dimension(:,:) :: iArrayFrom
  integer (kind=c_int), dimension(:,:), intent(inout) :: iArrayTo


  ! [ LOCALS ]
  integer (kind=c_int) :: iCol, iRow
  integer (kind=c_int), dimension(2) :: iColRow
  integer (kind=c_int) :: iSrcCol, iSrcRow
  integer (kind=c_int) :: iSpread

  ! must ensure that there are coordinates associated with the "to" grid...
  ! by default, these are left unpopulated during a "normal" swb run
  if(.not. allocated(pGrdTo%rX) )  call grid_PopulateXY(pGrdTo)
  if(.not. allocated(pGrdFrom%rX) )  call grid_PopulateXY(pGrdFrom)

  call echolog("Target grid resolution: "//trim(asCharacter( pGrdTo%rGridCellSize )))
  call echolog("Source grid resolution: "//trim(asCharacter( pGrdFrom%rGridCellSize )))

  iSpread = max(1, nint(pGrdTo%rGridCellSize / pGrdFrom%rGridCellSize / 2.))

!  if (.not. str_compare(pGrdFrom%sPROJ4_string,pGrdTo%sPROJ4_string)) then
  if ( lTRUE ) then

!$OMP PARALLEL DO

  !$OMP ORDERED

    do iRow=1,pGrdTo%iNY
      do iCol=1,pGrdTo%iNX

        iColRow = grid_GetGridColRowNum(pGrd=pGrdFrom, &
                   rX=real(pGrdTo%rX(iCol, iRow), kind=c_double), &
                   rY=real(pGrdTo%rY(iCol, iRow), kind=c_double))

        call assert(iColRow(COLUMN) > 0 .and. iColRow(COLUMN) <= pGrdFrom%iNX, &
          "Illegal column number supplied: "//trim(asCharacter(iColRow(COLUMN))), &
          trim(__FILE__), __LINE__)

        call assert(iColRow(ROW) > 0 .and. iColRow(ROW) <= pGrdFrom%iNY, &
          "Illegal row number supplied: "//trim(asCharacter(iColRow(ROW))), &
          trim(__FILE__), __LINE__)

        iArrayTo(iCol,iRow) = grid_majorityFilter_int(iValues=iArrayFrom, &
          iNX=pGrdFrom%iNX, &
          iNY=pGrdFrom%iNY, &
          iTargetCol=iColRow(COLUMN), &
          iTargetRow=iColRow(ROW), &
          iNoDataValue=pGrdFrom%iNoDataValue, &
          iSpread=iSpread)

      enddo
    enddo
!$OMP END ORDERED

!$OMP END PARALLEL DO

  else

!$OMP PARALLEL DO ORDERED
    do iRow=1,pGrdTo%iNY
      do iCol=1,pGrdTo%iNX
        iSrcCol = grid_GetGridColNum(pGrdFrom,real(pGrdTo%rX(iCol, iRow), kind=c_double) )
        iSrcRow = grid_GetGridRowNum(pGrdFrom,real(pGrdTo%rY(iCol, iRow), kind=c_double) )

        call assert(iSrcCol > 0 .and. iSrcCol <= pGrdFrom%iNX, &
          "Illegal column number supplied: "//trim(asCharacter(iSrcCol)), &
          trim(__FILE__), __LINE__)

        call assert(iSrcRow > 0 .and. iSrcRow <= pGrdFrom%iNY, &
          "Illegal row number supplied: "//trim(asCharacter(iSrcRow)), &
          trim(__FILE__), __LINE__)

        iArrayTo(iCol,iRow) = grid_majorityFilter_int(iValues=iArrayFrom, &
          iNX=pGrdFrom%iNX, &
          iNY=pGrdFrom%iNY, &
          iTargetCol=iSrcCol, &
          iTargetRow=iSrcRow, &
          iNoDataValue=pGrdFrom%iNoDataValue, &
          iSpread=iSpread)

      enddo
    enddo
!$OMP END PARALLEL DO

  endif

end subroutine grid_GridToGrid_int

subroutine grid_GridToGrid_short(pGrdFrom, iArrayFrom, pGrdTo, iArrayTo)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrdFrom   ! pointer to source grid
  type ( T_GENERAL_GRID ),pointer :: pGrdTo     ! pointer to destination grid
  integer (kind=c_int), dimension(:,:) :: iArrayFrom
  integer (kind=c_short), dimension(:,:), intent(inout) :: iArrayTo


  ! [ LOCALS ]
  integer (kind=c_int) :: iCol, iRow
  integer (kind=c_int), dimension(2) :: iColRow
  integer (kind=c_int) :: iSpread

  print *, trim(__FILE__), __LINE__
  print *, pGrdTo%rGridCellSize
  print *, pGrdFrom%rGridCellSize

  iSpread = max(1, nint(pGrdTo%rGridCellSize / pGrdFrom%rGridCellSize / 2.))

  ! must ensure that there are coordinates associated with the "to" grid...
  ! by default, these are left unpopulated during a "normal" swb run
  if(.not. allocated(pGrdTo%rX) )  call grid_PopulateXY(pGrdTo)
  if(.not. allocated(pGrdFrom%rX) )  call grid_PopulateXY(pGrdFrom)

  do iRow=1,pGrdTo%iNY
    do iCol=1,pGrdTo%iNX

       iColRow = grid_GetGridColRowNum(pGrd=pGrdFrom, &
                   rX=real(pGrdTo%rX(iCol, iRow), kind=c_double), &
                   rY=real(pGrdTo%rY(iCol, iRow), kind=c_double))

       call assert(iColRow(COLUMN) > 0 .and. iColRow(COLUMN) <= pGrdFrom%iNX, &
         "Illegal column number supplied: "//trim(asCharacter(iColRow(COLUMN))), &
         trim(__FILE__), __LINE__)

      call assert(iColRow(ROW) > 0 .and. iColRow(ROW) <= pGrdFrom%iNY, &
        "Illegal row number supplied: "//trim(asCharacter(iColRow(ROW))), &
        trim(__FILE__), __LINE__)


      iArrayTo(iCol,iRow) = grid_majorityFilter_int(iValues=iArrayFrom, &
         iNX=pGrdFrom%iNX, &
         iNY=pGrdFrom%iNY, &
         iTargetCol=iColRow(COLUMN), &
         iTargetRow=iColRow(ROW), &
         iNoDataValue=pGrdFrom%iNoDataValue, &
         iSpread=iSpread)

    enddo
  enddo

end subroutine grid_GridToGrid_short

!----------------------------------------------------------------------

subroutine grid_GridToGrid_sgl(pGrdFrom, rArrayFrom, pGrdTo, rArrayTo)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrdFrom   ! pointer to source grid
  type ( T_GENERAL_GRID ),pointer :: pGrdTo     ! pointer to destination grid
  real (kind=c_float), dimension(:,:) :: rArrayFrom
  real (kind=c_float), dimension(:,:), intent(inout) :: rArrayTo

  ! [ LOCALS ]
  integer (kind=c_int), dimension(2) :: iColRow
  integer (kind=c_int) :: iCol, iRow
  integer (kind=c_int) :: iSrcCol, iSrcRow
  real (kind=c_float), dimension(3,3) :: rKernel

  rKernel = 1.
  rKernel(2,2) = 8.

  ! must ensure that there are coordinates associated with the "to" grid...
  ! by default, these are left unpopulated during a "normal" swb run
  if(.not. allocated(pGrdTo%rX) )  call grid_PopulateXY(pGrdTo)
  if(.not. allocated(pGrdFrom%rX) )  call grid_PopulateXY(pGrdFrom)

!  if (.not. str_compare(pGrdFrom%sPROJ4_string,pGrdTo%sPROJ4_string)) then
  if ( lTRUE ) then

    do iRow=1,pGrdTo%iNY
      do iCol=1,pGrdTo%iNX

        iColRow = grid_GetGridColRowNum(pGrd=pGrdFrom, &
                   rX=real(pGrdTo%rX(iCol, iRow), kind=c_double), &
                   rY=real(pGrdTo%rY(iCol, iRow), kind=c_double))

        call assert(iColRow(COLUMN) > 0 .and. iColRow(COLUMN) <= pGrdFrom%iNX, &
          "Illegal column number supplied: "//trim(asCharacter(iColRow(COLUMN))), &
          trim(__FILE__), __LINE__)

        call assert(iColRow(ROW) > 0 .and. iColRow(ROW) <= pGrdFrom%iNY, &
          "Illegal row number supplied: "//trim(asCharacter(iColRow(ROW))), &
          trim(__FILE__), __LINE__)

        rArrayTo(iCol,iRow) = rArrayFrom( iColRow(COLUMN), iColRow(ROW) )

!         rArrayTo(iCol,iRow) = grid_Convolve_sgl(rValues=rArrayFrom, &
!           iTargetCol=iColRow(COLUMN), &
!           iTargetRow=iColRow(ROW), &
!           rKernel=rKernel)

      enddo
    enddo

  else  ! if we have the same projections as the base, our standard methods work

    do iRow=1,pGrdTo%iNY
      do iCol=1,pGrdTo%iNX
        iSrcCol = grid_GetGridColNum(pGrdFrom,real(pGrdTo%rX(iCol, iRow), kind=c_double) )
        iSrcRow = grid_GetGridRowNum(pGrdFrom,real(pGrdTo%rY(iCol, iRow), kind=c_double) )

        call assert(iSrcCol > 0 .and. iSrcCol <= pGrdFrom%iNX, &
          "Illegal column number supplied: "//trim(asCharacter(iSrcCol)), &
          trim(__FILE__), __LINE__)

        call assert(iSrcRow > 0 .and. iSrcRow <= pGrdFrom%iNY, &
          "Illegal row number supplied: "//trim(asCharacter(iSrcRow)), &
          trim(__FILE__), __LINE__)

        rArrayTo(iCol,iRow) = rArrayFrom( iSrcCol, iSrcRow )
      enddo
    enddo

  endif

end subroutine grid_GridToGrid_sgl

!----------------------------------------------------------------------

function grid_MajorityFilter_int(iValues, iNX, iNY, iTargetCol, &
   iTargetRow, iNoDataValue, iSpread)  result(iRetVal)

  ! [ ARGUMENTS ]
  integer (kind=c_int), dimension(iNX, iNY) :: iValues
  integer (kind=c_int) :: iNX
  integer (kind=c_int) :: iNY
  integer (kind=c_int) :: iTargetCol          ! column number of target cell
  integer (kind=c_int) :: iTargetRow          ! row number of target cell
  integer (kind=c_int) :: iNoDataValue
  integer (kind=c_int) :: iRetVal
  integer (kind=c_int) :: iSpread             ! integer representing how many
                                              ! cells should be searched

  ! [ LOCALS ]
  integer (kind=c_int), dimension(625) :: iValue
  integer (kind=c_int), dimension(625) :: iCount
  logical (kind=c_bool) :: lMatch
  integer (kind=c_int) :: iRow
  integer (kind=c_int) :: iCol
  integer (kind=c_int) :: i, iSize, iLast, iIndex, iCellNum

  iValue = 0
  iCount = 0
  iLast = 0
  iCellNum = 0

  ! need to set a reasonable upper bound on the spread to consider
  iSpread = min(iSpread, 12)

   do iRow=max(1,iTargetRow - iSpread), min(iNY, iTargetRow + iSpread)
     do iCol=max(1,iTargetCol - iSpread), min(iNX, iTargetCol + iSpread)
!
       iCellNum = iCellNum + 1
!
       lMatch = lFALSE
!
       do i=1, iLast
         if (iValue(i) == iValues(iCol, iRow) ) then
           iCount(i) = iCount(i) + 1
           lMatch = lTRUE
           exit
         endif
       enddo
!
       if (.not. lMatch) then
         iLast = iLast + 1
         iValue(iLast) = iValues(iCol, iRow)
         iCount(iLast) = iCount(iLast) + 1
         lMatch = lTRUE
       endif
!
!       if (iRow == iTargetRow .and. iCol == iTargetCol) then
!         print *, "iCol, iRow, iValue, iMask: ", iCol, iRow, "[", iValues(iCol, iRow), "]", iMask(iCellNum)," {central cell}, value = ", iValues(iCol, iRow)
!       else
!         print *, "iCol, iRow, iValue, iMask: ", iCol, iRow, "[", iValues(iCol, iRow), "]", iMask(iCellNum)
!       endif

     enddo
   enddo
!
!   print *, "values: ", iValue
!   print *, "counts: ", iCount
!
!   iIndex = MAXLOC(iCount, mask=(iValue /= iNoDataValue), dim=1)
   iIndex = MAXLOC(iCount, dim=1)
!
!   print *, "index: ", iIndex
!   print *, "return value: ", iValue(iIndex)
!   print *, "------------------------------------------------------"
!   print *, "  "
!
   iRetVal = iValue(iIndex)

end function grid_majorityFilter_int

function grid_Convolve_sgl(rValues, iTargetCol, &
   iTargetRow, rKernel, rNoDataValue)  result(rRetVal)

  ! [ ARGUMENTS ]
  real (kind=c_float), dimension(:,:) :: rValues
  integer (kind=c_int) :: iNX
  integer (kind=c_int) :: iNY
  integer (kind=c_int) :: iTargetCol          ! column number of target cell
  integer (kind=c_int) :: iTargetRow          ! row number of target cell
  real (kind=c_float), dimension(:,:) :: rKernel
  real (kind=c_float), optional :: rNoDataValue
  real (kind=c_float) :: rRetVal

  ! [ LOCALS ]
  integer (kind=c_int) :: iRowMin, iRowMax
  integer (kind=c_int) :: iColMin, iColMax
  integer (kind=c_int) :: iKernelSize         ! i.e. 3, 5, 7, 9, 11, etc.
  integer (kind=c_int) :: iIncValue
  integer (kind=c_int) :: iCol, iRow, iRownum, iColnum
  real (kind=c_float) :: rKernelSum

  rKernelSum = rZERO
  rRetVal = rZERO

  iKernelSize = size(rKernel, dim=1)
  iIncValue = (iKernelSize - 1) / 2

  iNX = ubound(rValues,1)
  iNY = ubound(rValues,2)

  iRowMin = max(1,iTargetRow - iIncValue)
  iRowMax = min(iNY, iTargetRow + iIncValue)

  iColMin = max(1,iTargetCol - iIncValue)
  iColMax = min(iNY, iTargetCol + iIncValue)

  if( (iRowMax - iRowMin + 1)  /= iKernelsize &
    .or. (iColMax - iColMin + 1)  /= iKernelsize ) then

    ! This is a simple, but less than desirable way to treat cells that
    ! fall near the edge of the source grid. Ideally, the source grid will
    ! be greater than the target grid by a wide enough margin that this
    ! code will rarely be used.
    rRetVal = rValues(iTargetCol, iTargetRow)

  else

!    rRetVal = sum(rValues(iColMin:iColMax, iRowMin:iRowMax) * rKernel ) / sum( rKernel )

    do iCol=0,iKernelSize-1
      do iRow=0,iKernelSize-1

        iRownum = iRow + iRowMin
        iColnum = iCol + iColmin

        if (iRownum > iNY .or. iColnum > iNX) then
          cycle   ! our calculated row or column number is outside of the
                  ! bounds of the rValues array
        else
          rRetVal = rRetVal + rValues(iColnum,iRownum) * rKernel(iCol+1, iRow+1)
          rKernelSum = rKernelSum + rKernel(iCol+1, iRow+1)
        endif
      enddo
    enddo

    if (rKernelSum > 0.) rRetVal = rRetVal / rKernelSum

  endif

end function grid_Convolve_sgl

end module swb_grid
