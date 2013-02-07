!> @file
!> @brief  Contains a single module, swb_grid, which
!>  provides support for gridded ASCII data file and data structure operations


!> @brief  Provides support for input and output of gridded ASCII data,
!> as well as for creation and destruction of grid data structures (defined types).
module swb_grid
!!****h* SWB/grid
! NAME
!
!   grid.f95 - Grid I/O and support routines for SWB model
!
! SYNOPSIS
!   These routines provide generic grid I/O and grid functions for
!   the SWB model.
!
! NOTES
!
!!***

  use types

  implicit none

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
    module procedure grid_gridToGrid_sgl
  end interface grid_gridToGrid

contains

!--------------------------------------------------------------------------
!!****f* grid/grid_Create
! NAME
!   grid_Create - Creates a grid of a specified type.
!
! SYNOPSIS
!   Creates a grid pointer object and allocates memory for the data
!   associated with the grid (REAL, INTEGER, or T_CELL).
!
! INPUTS
!   iNX - Number of grid cells in the x direction
!   iNY - Number of grid cells in the y direction
!   rX0 - X coordinate for the lower left corner of the grid
!   rY0 - Y coordinate for the lower left corner of the grid
!   rX1 - X coordinate for the upper right corner of the grid
!   rY1 - Y coordinate for the upper right corner of the grid
!   iDataType - Integer value corresponding to the type of data contained
!     in the grid
!
! OUTPUTS
!   pGrd - Pointer to a grid object
!
! NOTES
!   Code refers to parameters that are set within types.f95.
!
! SOURCE

function grid_Create ( iNX, iNY, rX0, rY0, rX1, rY1, iDataType ) result ( pGrd )
  !! Creates a new iNX-by-iNY T_GRID of data type iDataType, over the extent
  !! (rX0,rY0)-(rX1,rY1), and returns a pointer.
  ! ARGUMENTS
  integer (kind=T_INT), intent(in) :: iNX, iNY        ! Grid dimensions
  real (kind=T_DBL), intent(in) :: rX0, rY0          ! Lower-left corner (world coords)
  real (kind=T_DBL), intent(in) :: rX1, rY1          ! Upper-right corner (world coords)
  integer (kind=T_INT), intent(in) :: iDataType       ! Data type (T_INT_GRID, etc.)
  ! RETURN VALUE
  type (T_GENERAL_GRID), pointer :: pGrd
  ! LOCALS
  integer (kind=T_INT) :: iStat
#ifdef DEBUG_PRINT
  integer (kind=T_INT) :: iNumGridCells
  integer(kind=T_INT) :: iSize
  integer(kind=T_INT) :: iSizeCells
  integer(kind=T_INT) :: iNumCells
  integer(kind=T_INT) :: iSizeSingleCell

  iNumGridCells = iNX * iNY
#endif

  allocate ( pGrd, stat=iStat )
  call Assert ( iStat == 0, &
     "Could not allocate pointer to T_GRID object", trim(__FILE__),__LINE__ )
  call Assert ( LOGICAL(iNX>0 .and. iNY>0,kind=T_LOGICAL), &
     "Illegal grid dimensions specified", trim(__FILE__),__LINE__)

  select case (iDataType)
      case ( T_INT_GRID )
#ifdef DEBUG_PRINT
          write(unit=LU_LOG,FMT="( &
          '  attempting to allocate memory for INT_GRID with ',i14,' cells')") iNumGridCells
#endif
          allocate ( pGrd%iData( iNX,iNY ), stat=iStat )
          call Assert (iStat == 0, &
             "Could not allocate integer data", &
              TRIM(__FILE__),__LINE__)
          pGrd%iData = 0
      case ( T_SGL_GRID )
#ifdef DEBUG_PRINT
          write(unit=LU_LOG,FMT="( &
          '  attempting to allocate memory for SGL_GRID with ',i14,' cells')") iNumGridCells
#endif
          allocate ( pGrd%rData( iNX,iNY ), stat=iStat )
          call Assert (iStat == 0, &
             "Could not allocate real data", &
              TRIM(__FILE__),__LINE__)
          pGrd%rData = rZERO
      case ( T_CELL_GRID )
#ifdef DEBUG_PRINT
          write(unit=LU_LOG,FMT="( &
          '  attempting to allocate memory for CELL_GRID with ',i14,' cells')") iNumGridCells
          flush(unit=LU_LOG)
#endif
          allocate ( pGrd%Cells( iNX,iNY ), stat=iStat )
          call Assert (iStat == 0, &
             "Could not allocate cell-by-cell data", &
             TRIM(__FILE__),__LINE__)

#ifdef DEBUG_PRINT
          iSize = SIZEOF(pGrd)
          iSizeCells = SIZEOF(pGrd%Cells)
          iNumCells = SIZE(pGrd%Cells)
          iSizeSingleCell = iSizeCells / iNumCells

          write(unit=LU_LOG,FMT=*) '    Allocated size (Grid):',iSize
          write(unit=LU_LOG,FMT=*) '    Allocated size (Cells):',iSizeCells,'(',iSizeCells/1024./1024.,' MB)'
          write(unit=LU_LOG,FMT=*) '    Memory required for each grid cell: ',iSizeSingleCell,' (bytes)'
#endif

      case default
          call Assert ( .false._T_LOGICAL, 'Internal error -- illegal grid data type' )
  end select
  pGrd%iDataType = iDataType
  pGrd%iNX = iNX
  pGrd%iNY = iNY
  pGrd%rX0 = rX0
  pGrd%rX1 = rX1
  pGrd%rY0 = rY0
  pGrd%rY1 = rY1
  pGrd%rGridCellSize = (pGrd%rX1 - pGrd%rX0) / real(pGrd%iNX, kind=T_SGL)
  pGrd%iNumGridCells = iNX * iNY

end function grid_Create

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
  integer (kind=T_INT) :: iStat

  if(associated(pGrd) )then

    if ( pGrd%iDataType == T_INT_GRID ) then
      deallocate ( pGrd%iData, stat=iStat )
      call Assert ( iStat == 0, "Failed to deallocate integer grid" )
    else if ( pGrd%iDataType == T_SGL_GRID ) then
      deallocate ( pGrd%rData, stat=iStat )
      call Assert ( iStat == 0, "Failed to deallocate real grid" )
    else if ( pGrd%iDataType == T_CELL_GRID ) then
      deallocate ( pGrd%Cells, stat=iStat )
      call Assert ( iStat == 0, "Failed to deallocate cell grid" )
    else
      call Assert ( lFALSE, "Internal error -- unknown grid type" )
    end if

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
!    T_INT_GRID
!    T_SGL_GRID
!
! SOURCE

function grid_Read ( sFileName, sFileType, iDataType ) result ( pGrd )

  ! ARGUMENTS
  character (len=*), intent(in) :: sFileName          ! Name of the grid input file
  character (len=*), intent(in) :: sFileType          ! File type (see above)
  integer (kind=T_INT), intent(in) :: iDataType       ! T_GRID_INT or T_GRID_REAL
  ! RETURN VALUE
  type (T_GENERAL_GRID), pointer :: pGrd

  if ( trim(sFileType) == "ARC_GRID" ) then
      pGrd => grid_ReadArcGrid_fn( sFileName, iDataType )
  else if ( trim(sFileType) == "SURFER" ) then
      pGrd => grid_ReadSurferGrid_fn( sFileName, iDataType )
  else
      call Assert( lFALSE, "Illegal grid file type requested" )
  end if

end function grid_Read

subroutine grid_Read_sub ( sFileName, sFileType, pGrd )

  ! ARGUMENTS
  character (len=*), intent(in) :: sFileName          ! Name of the grid input file
  character (len=*), intent(in) :: sFileType          ! File type (see above)
  type (T_GENERAL_GRID), pointer :: pGrd

  if ( trim(sFileType) == "ARC_GRID" ) then
      call grid_ReadArcGrid_sub( sFileName, pGrd )
  else if ( trim(sFileType) == "SURFER" ) then
      call grid_ReadSurferGrid_sub( sFileName, pGrd )
  else
      call Assert( lFALSE, "Illegal grid file type requested" )
  end if

end subroutine grid_Read_sub

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
!    T_INT_GRID
!    T_SGL_GRID
!
! SOURCE

function grid_ReadArcGrid_fn ( sFileName, iDataType ) result ( pGrd )

  ! ARGUMENTS
  character (len=*), intent(in) :: sFileName          ! Name of the grid input file
  integer (kind=T_INT), intent(in) :: iDataType       ! T_GRID_INT or T_GRID_REAL
  ! RETURN VALUE
  type (T_GENERAL_GRID), pointer :: pGrd
  ! LOCALS
  character (len=256) :: sInputRecord                 ! Text record for input
  character (len=256) :: sDirective                   ! Directive for input
  character (len=256) :: sArgument                    ! Argument for keyword directives
  character (len=8192) :: sBuf
  integer (kind=T_INT) :: iStat                       ! For "iostat="
  integer (kind=T_INT) :: iNX,iNY                     ! Grid dimensions
  integer (kind=T_INT) :: iHdrRecs                    ! Number of records in header
  integer (kind=T_INT) :: iCol,iRow,k                 ! Loop indices for grid reading
  real (kind=T_DBL) :: rX0,rX1                        ! Limits in X
  real (kind=T_DBL) :: rY0,rY1                        ! Limits in Y
  real (kind=T_SGL) :: rCellSize                      ! Cell size
  integer (kind=T_INT) :: iCount,iCumlCount
  logical (kind=T_LOGICAL) :: lXLLCenter, lYLLCenter  ! Flags XLLCENTER / XLLCORNER
  logical (kind=T_LOGICAL) :: lFileExists

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
          iHdrRecs = iHdrRecs + 1
      else
          ! Found the data -- construct the grid
          if ( lXLLCenter ) rX0 = rX0 - rHALF*rCellSize
          if ( lYLLCenter ) rY0 = rY0 - rHALF*rCellSize
          rX1 = rX0 + iNX*rCellSize
          rY1 = rY0 + iNY*rCellSize

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
              case ( T_INT_GRID )
                do iRow=1,pGrd%iNY
                  read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%iData(:,iRow)
                  call Assert ( iStat == 0, &
                    "Failed to read integer grid data - file: " &
                    //trim(sFileName)//"  row num: "//TRIM(int2char(iRow)), &
                   TRIM(__FILE__),__LINE__ )
                end do
              case ( T_SGL_GRID )
                do iRow=1,pGrd%iNY
                  read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%rData(:,iRow)
                  call Assert ( iStat == 0, &
                    "Failed to read real grid data - file: " &
                    //trim(sFileName)//"  row num: "//TRIM(int2char(iRow)), &
                   TRIM(__FILE__),__LINE__ )
                end do
              case default
                  call Assert ( lFALSE, &
                    "Internal error -- illegal ARC GRID data type", &
                    TRIM(__FILE__),__LINE__)
          end select
          exit
      end if
  end do

  if(iDataType==T_INT_GRID) then    ! check for strange or illegal values
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

    write(LU_LOG,FMT="(1x,a,t45,i12)") "Total number of grid cells: ", &
      size(pGrd%iData)
    write(LU_LOG,FMT="(1x,a,t45,i12)") &
      "Total number of grid cells with value >= 0: ",iCumlCount
    flush(LU_LOG)
    call Assert(LOGICAL(size(pGrd%iData)==iCumlCount,kind=T_LOGICAL), &
      "Illegal or missing values in integer grid file: "//trim(sFileName))

  end if

  close ( unit=LU_GRID, iostat=iStat )
  call Assert ( iStat == 0, "Failed to close grid file" )

  return
end function grid_ReadArcGrid_fn

subroutine grid_ReadArcGrid_sub ( sFileName, pGrd )

  ! ARGUMENTS
  character (len=*), intent(in) :: sFileName          ! Name of the grid input file
  type (T_GENERAL_GRID), pointer :: pGrd

  ! LOCALS
  character (len=256) :: sInputRecord                 ! Text record for input
  character (len=256) :: sDirective                   ! Directive for input
  character (len=256) :: sArgument                    ! Argument for keyword directives
  character (len=8192) :: sBuf
  integer (kind=T_INT) :: iStat                       ! For "iostat="
  integer (kind=T_INT) :: iNX,iNY                     ! Grid dimensions
  integer (kind=T_INT) :: iHdrRecs                    ! Number of records in header
  integer (kind=T_INT) :: iCol,iRow,k                 ! Loop indices for grid reading
  real (kind=T_DBL) :: rX0,rX1                        ! Limits in X
  real (kind=T_DBL) :: rY0,rY1                        ! Limits in Y
  real (kind=T_SGL) :: rCellSize                      ! Cell size
  integer (kind=T_INT) :: iCount,iCumlCount
  logical (kind=T_LOGICAL) :: lXLLCenter, lYLLCenter  ! Flags XLLCENTER / XLLCORNER
  logical (kind=T_LOGICAL) :: lFileExists

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
          iHdrRecs = iHdrRecs + 1
      else
          ! Found the data -- construct the grid
          if ( lXLLCenter ) rX0 = rX0 - rHALF*rCellSize
          if ( lYLLCenter ) rY0 = rY0 - rHALF*rCellSize
          rX1 = rX0 + iNX*rCellSize
          rY1 = rY0 + iNY*rCellSize

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
          select case ( pGrd%iDataType )
              case ( T_INT_GRID )
                do iRow=1,pGrd%iNY
                  read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%iData(:,iRow)
                  call Assert ( iStat == 0, &
                    "Failed to read integer grid data - file: " &
                    //trim(sFileName)//"  row num: "//TRIM(int2char(iRow)), &
                   TRIM(__FILE__),__LINE__ )
                end do
              case ( T_SGL_GRID )
                do iRow=1,pGrd%iNY
                  read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%rData(:,iRow)
                  call Assert ( iStat == 0, &
                    "Failed to read real grid data - file: " &
                    //trim(sFileName)//"  row num: "//TRIM(int2char(iRow)), &
                   TRIM(__FILE__),__LINE__ )
                end do
              case default
                  call Assert ( lFALSE, &
                    "Internal error -- illegal ARC GRID data type", &
                    TRIM(__FILE__),__LINE__)
          end select
          exit
      end if
  end do

  if(pGrd%iDataType==T_INT_GRID) then    ! check for strange or illegal values
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

    write(LU_LOG,FMT="(1x,a,t45,i12)") "Total number of grid cells: ", &
      size(pGrd%iData)
    write(LU_LOG,FMT="(1x,a,t45,i12)") &
      "Total number of grid cells with value >= 0: ",iCumlCount
    flush(LU_LOG)
    call Assert(LOGICAL(size(pGrd%iData)==iCumlCount,kind=T_LOGICAL), &
      "Illegal or missing values in integer grid file: "//trim(sFileName))

  end if

  close ( unit=LU_GRID, iostat=iStat )
  call Assert ( iStat == 0, "Failed to close grid file" )

  return
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
!    T_INT_GRID
!    T_SGL_GRID
!
! SOURCE

function grid_ReadSurferGrid_fn ( sFileName, iDataType ) result ( pGrd )

  ! ARGUMENTS
  character (len=*), intent(in) :: sFileName          ! Name of the grid input file
  integer (kind=T_INT), intent(in) :: iDataType       ! T_GRID_INT or T_GRID_REAL
  ! RETURN VALUE
  type (T_GENERAL_GRID), pointer :: pGrd
  ! LOCALS
  character (len=4) :: sSentinel                      ! Better be "DSAA" for SURFER!
  integer (kind=T_INT) :: iStat                       ! For "iostat="
  integer (kind=T_INT) :: iNX,iNY                     ! Grid dimensions
  integer (kind=T_INT) :: iCol,iRow                         ! Loop indices for grid reading
  real (kind=T_DBL) :: rX0,rX1                       ! Limits in X
  real (kind=T_DBL) :: rY0,rY1                       ! Limits in Y
  real (kind=T_SGL) :: rZ0,rZ1                       ! Limits in Z (not used)
  logical (kind=T_LOGICAL) :: lFileExists

  inquire(file=trim(sFileName), EXIST=lFileExists)
  call assert( lFileExists, "The Surfer ASCII grid file "//dquote(sFilename)// &
    " could not be found.",trim(__FILE__),__LINE__)

  open ( LU_GRID, iostat=iStat, file=trim(sFileName) )
  call Assert( LOGICAL(iStat == 0,kind=T_LOGICAL), &
     "Could not open input file " // trim(sFileName) )

  read ( unit=LU_GRID, fmt=*, iostat=iStat ) sSentinel
  call Assert ( LOGICAL(iStat == 0,kind=T_LOGICAL), &
     "Could not read first record of SURFER grid" )
  call Assert ( LOGICAL(trim(sSentinel) == "DSAA",kind=T_LOGICAL), &
     "This is not a SURFER grid" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) iNX, iNY
  call Assert ( LOGICAL(iStat == 0,kind=T_LOGICAL), &
     "Error reading SURFER grid dimensions" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) rX0, rX1
  call Assert ( LOGICAL(iStat == 0,kind=T_LOGICAL), &
     "Error reading SURFER X limits" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) rY0, rY1
  call Assert ( LOGICAL(iStat == 0,kind=T_LOGICAL), &
     "Error reading SURFER y limits" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) rZ0, rZ1
  call Assert ( LOGICAL(iStat == 0,kind=T_LOGICAL), &
     "Error reading SURFER Z limits" )

  pGrd => grid_Create ( iNX, iNY, rX0, rY0, rX1, rY1, iDataType )
  select case ( iDataType )
      case ( T_INT_GRID )
          do iRow=1, iNY
              read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%iData(:,iRow)
              call Assert ( iStat == 0, "Failed to read integer grid data" )
          end do
      case ( T_SGL_GRID )
          do iRow=1, iNY
              read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%rData(:,iRow)
              call Assert ( iStat == 0, "Failed to read real grid data" )
          end do
      case default
          call Assert ( lFALSE, "Internal error -- illegal SURFER grid data type" )
  end select

  call Assert(LOGICAL(iNX>0,kind=T_LOGICAL),"Must have a non-zero number of grid cells in a surfer grid file...")
  call Assert(LOGICAL(iNY>0,kind=T_LOGICAL),"Must have a non-zero number of grid cells in a surfer grid file...")

  pGrd%rGridCellSize = (rX1-rX0)/iNX

  close ( unit=LU_GRID, iostat=iStat )
  call Assert ( iStat == 0, "Failed to close grid file" )

  return
end function grid_ReadSurferGrid_fn


subroutine grid_ReadSurferGrid_sub ( sFileName, pGrd )

  ! ARGUMENTS
  character (len=*), intent(in) :: sFileName          ! Name of the grid input file
  type (T_GENERAL_GRID), pointer :: pGrd

  ! LOCALS
  character (len=4) :: sSentinel                      ! Better be "DSAA" for SURFER!
  integer (kind=T_INT) :: iStat                       ! For "iostat="
  integer (kind=T_INT) :: iNX,iNY                     ! Grid dimensions
  integer (kind=T_INT) :: iCol,iRow                         ! Loop indices for grid reading
  real (kind=T_DBL) :: rX0,rX1                       ! Limits in X
  real (kind=T_DBL) :: rY0,rY1                       ! Limits in Y
  real (kind=T_SGL) :: rZ0,rZ1                       ! Limits in Z (not used)
  logical (kind=T_LOGICAL) :: lFileExists

  inquire(file=trim(sFileName), EXIST=lFileExists)
  call assert( lFileExists, "The Surfer ASCII grid file "//dquote(sFilename)// &
    " could not be found.",trim(__FILE__),__LINE__)

  open ( LU_GRID, iostat=iStat, file=trim(sFileName) )
  call Assert( LOGICAL(iStat == 0,kind=T_LOGICAL), &
     "Could not open input file " // trim(sFileName) )

  read ( unit=LU_GRID, fmt=*, iostat=iStat ) sSentinel
  call Assert ( LOGICAL(iStat == 0,kind=T_LOGICAL), &
     "Could not read first record of SURFER grid" )
  call Assert ( LOGICAL(trim(sSentinel) == "DSAA",kind=T_LOGICAL), &
     "This is not a SURFER grid" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) iNX, iNY
  call Assert ( LOGICAL(iStat == 0,kind=T_LOGICAL), &
     "Error reading SURFER grid dimensions" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) rX0, rX1
  call Assert ( LOGICAL(iStat == 0,kind=T_LOGICAL), &
     "Error reading SURFER X limits" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) rY0, rY1
  call Assert ( LOGICAL(iStat == 0,kind=T_LOGICAL), &
     "Error reading SURFER y limits" )
  read ( unit=LU_GRID, fmt=*, iostat=iStat ) rZ0, rZ1
  call Assert ( LOGICAL(iStat == 0,kind=T_LOGICAL), &
     "Error reading SURFER Z limits" )

  select case ( pGrd%iDataType )
      case ( T_INT_GRID )
          do iRow=1, iNY
              read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%iData(:,iRow)
              call Assert ( iStat == 0, "Failed to read integer grid data" )
          end do
      case ( T_SGL_GRID )
          do iRow=1, iNY
              read ( unit=LU_GRID, fmt=*, iostat=iStat ) pGrd%rData(:,iRow)
              call Assert ( iStat == 0, "Failed to read real grid data" )
          end do
      case default
          call Assert ( lFALSE, "Internal error -- illegal SURFER grid data type" )
  end select

  call Assert(LOGICAL(iNX>0,kind=T_LOGICAL),"Must have a non-zero number of grid cells in a surfer grid file...")
  call Assert(LOGICAL(iNY>0,kind=T_LOGICAL),"Must have a non-zero number of grid cells in a surfer grid file...")

  pGrd%rGridCellSize = (rX1-rX0)/iNX

  close ( unit=LU_GRID, iostat=iStat )
  call Assert ( iStat == 0, "Failed to close grid file" )

end subroutine grid_ReadSurferGrid_sub

!!***
!--------------------------------------------------------------------------
!!****s* grid/grid_WriteArcGrid
! NAME
!   grid_WriteArcGrid - Reads an ARC ASCII grid of a specified type.
!
! SYNOPSIS
!   Write an ARC ASCII grid of specified data type
!
! INPUTS
!   cFilename - Character string containing the filename of the ARC ASCII
!               grid to be written
!   rXmin - Real value of the x coordinate of the lower left corner of
!             the grid
!   rXmax - Real value of the x coordinate of the upper right corner of
!             the grid
!   rYmin - Real value of the y coordinate of the lower left corner of
!             the grid
!   rYmax - Real value of the y coordinate of the upper right corner of
!             the grid
!   rValues - Array of real values to be written to the grid file
!   rNoData - Optional real value to be used in place of "No Data" values
!
! OUTPUTS
!   pGrd - Pointer to a grid object
!
! SOURCE

subroutine grid_WriteArcGrid(cFilename,rXmin,rXmax,rYmin,rYmax,rValues,rNoData)
  !! Writes an ARC-format grid file on 'fname', for the gridded data in
  !! 'values', with the data limits rXmin,rXmax,rYmin,rYmax, and NODATA_VALUE.
  ! [ ARGUMENTS ]
  character (len=*),intent(in) :: cFilename
  real (kind=T_DBL),intent(in) :: rXmin,rXmax,rYmin,rYmax
  real (kind=T_SGL),intent(in), optional :: rNoData
  real (kind=T_SGL),dimension(:,:),intent(in) :: rValues
  ! [ LOCALS ]
  integer (kind=T_INT) :: iCol,iRow
  integer (kind=T_INT) :: iNumCols, iNumRows
  integer (kind=T_INT) ::  istat
  real (kind=T_SGL) :: nodata_value
  character(len=256) :: sBuf

  iNumCols = size(rValues,1)
  iNumRows = size(rValues,2)

  ! dynamically create the Fortran output format
  write(sBuf,FMT="(a,a,a)") '(',TRIM(int2char(iNumCols)),'(a,1x))'

  if ( present(rNoData)) then
    nodata_value = rNoData
  else
    nodata_value = -9999.
  end if

  open ( LU_TEMP, file=cFilename, iostat=istat, status="REPLACE" )
  call Assert( istat==0, "Could not open output file " // cFilename, &
      TRIM(__FILE__),__LINE__)

  write ( unit=LU_TEMP, fmt="('NCOLS ',i10)", iostat=istat ) iNumCols
  call Assert( LOGICAL(istat==0,kind=T_LOGICAL), "Error writing grid file header" )
  write ( unit=LU_TEMP, fmt="('NROWS ',i10)", iostat=istat ) iNumRows
  call Assert( LOGICAL(istat==0,kind=T_LOGICAL), "Error writing grid file header" )
  write ( unit=LU_TEMP, fmt="('XLLCORNER ',f14.3)", iostat=istat ) rXmin
  call Assert( LOGICAL(istat==0,kind=T_LOGICAL), "Error writing X limits" )
  write ( unit=LU_TEMP, fmt="('YLLCORNER ',f14.3)", iostat=istat ) rYmin
  call Assert( LOGICAL(istat==0,kind=T_LOGICAL), "Error writing Y limits" )
  write ( unit=LU_TEMP, fmt="('CELLSIZE ',f14.3)", iostat=istat ) &
    (rXmax-rXmin) / real(iNumCols,kind=T_SGL)
  call Assert( LOGICAL(istat==0,kind=T_LOGICAL), "Error writing cell size" )
  write ( unit=LU_TEMP, fmt="('NODATA_VALUE ',f12.3)", iostat=istat ) nodata_value
  call Assert( LOGICAL(istat==0,kind=T_LOGICAL), "Error writing NODATA_VALUE" )
  do iRow=1,iNumRows
    write( unit=LU_TEMP, fmt=TRIM(sBuf), iostat=istat ) &
      (TRIM(real2char(rValues(iCol,iRow),iNUM_DIGITS,iFIELD_WIDTH)),iCol=1,iNumCols)
    call Assert( LOGICAL(istat==0,kind=T_LOGICAL), "Error writing Arc grid data" )
  end do

  close (unit=LU_TEMP)

end subroutine grid_WriteArcGrid
!!***
!--------------------------------------------------------------------------
!!****s* grid/grid_WriteSurferGrid
! NAME
!   grid_WriteSurferGrid - Reads an Surfer ASCII grid of a specified type.
!
! SYNOPSIS
!   Write a Surfer ASCII grid of specified data type
!
! INPUTS
!   cFilename - Character string containing the filename of the ARC ASCII
!               grid to be written
!   rXmin - Real value of the x coordinate of the lower left corner of
!             the grid
!   rXmax - Real value of the x coordinate of the upper right corner of
!             the grid
!   rYmin - Real value of the y coordinate of the lower left corner of
!             the grid
!   rYmax - Real value of the y coordinate of the upper right corner of
!             the grid
!   rValues - Array of real values to be written to the grid file
!   rNoData - Optional real value to be used in place of "No Data" values
!
! OUTPUTS
!   pGrd - Pointer to a grid object
!
! SOURCE

subroutine grid_WriteSurferGrid(cFilename,rXmin,rXmax,rYmin,rYmax,rValues)
  !! Writes a SURFER-format grid file on 'fname', for the gridded data in
  !! 'values', with the data limits rXmin,rXmax,rYmin,rYmax.
  ! [ ARGUMENTS ]
  character (len=*),intent(in) :: cFilename
  real (kind=T_DBL),intent(in) :: rXmin,rXmax,rYmin,rYmax
  real (kind=T_SGL),dimension(:,:),intent(in) :: rValues
  ! [ LOCALS ]
  integer (kind=T_INT) :: iCol,iRow
  integer (kind=T_INT) :: istat
  character(len=256) :: sBuf
  integer (kind=T_INT) :: iNumCols, iNumRows

  iNumCols = size(rValues,1)
  iNumRows = size(rValues,2)

  ! dynamically create the Fortran output format
  write(sBuf,FMT="(a,a,a)") '(',TRIM(int2char(iNumCols)),'(a,1x))'

  open (LU_TEMP, file=cFilename, iostat=istat, status="REPLACE" )
  call Assert( istat==0, "Could not open output file " // cFilename, &
      TRIM(__FILE__),__LINE__)

  write ( unit=LU_TEMP, fmt="('DSAA')", iostat=istat )
  call Assert( LOGICAL(istat==0,kind=T_LOGICAL), &
     "Error writing SURFER header" )
  write ( unit=LU_TEMP, fmt="(2i8)", iostat=istat ) iNumCols, iNumRows
  call Assert( istat==0, "Error writing SURFER dimensions", &
    trim(__FILE__), __LINE__)
  write ( unit=LU_TEMP, fmt="(2f14.3)", iostat=istat ) rXmin,rXmax
  call Assert( istat==0, "Error writing SURFER X limits", &
    trim(__FILE__), __LINE__)
  write ( unit=LU_TEMP, fmt="(2f14.3)", iostat=istat ) rYmin,rYmax
  call Assert( istat==0, "Error writing SURFER Y limits", &
    trim(__FILE__), __LINE__)
  write ( unit=LU_TEMP, fmt="(2f14.3)", iostat=istat ) minval(rValues),maxval(rValues)
  call Assert( istat==0, "Error writing SURFER Z limits", &
    trim(__FILE__), __LINE__)

  do iRow=iNumRows,1,-1
    write( unit=LU_TEMP, fmt=TRIM(sBuf), iostat=istat ) &
      (TRIM(real2char(rValues(iCol,iRow),iNUM_DIGITS,iFIELD_WIDTH)),iCol=1,iNumCols)
    call Assert( istat==0, "Error writing SURFER grid data" , &
      trim(__FILE__), __LINE__)
  end do

  close (unit=LU_TEMP)
  return
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
  real (kind=T_SGL), intent(in), optional :: rTolerance
  ! RETURN VALUE
  logical (kind=T_LOGICAL) :: lConform
  ! LOCALS
  real (kind=T_SGL) :: rTol
  real (kind=T_SGL), parameter :: rDEFAULT_TOLERANCE = 1.0e-2_T_SGL

  if ( present ( rTolerance ) ) then
      rTol = rTolerance * ( pGrd1%rX1 - pGrd1%rX0 )
  else
      rTol = rDEFAULT_TOLERANCE * ( pGrd1%rX1 - pGrd1%rX0 )
  end if

  if ( pGrd1%iNX /= pGrd2%iNX .or. &
       pGrd1%iNY /= pGrd2%iNY .or. &
       abs ( pGrd1%rX0 - pGrd2%rX0 ) > rTol .or. &
       abs ( pGrd1%rY0 - pGrd2%rY0 ) > rTol .or. &
       abs ( pGrd1%rX1 - pGrd2%rX1 ) > rTol .or. &
       abs ( pGrd1%rY1 - pGrd2%rY1 ) > rTol ) then
      lConform = lFALSE
  else
      lConform = lTRUE
  end if

  return
end function grid_Conform

function grid_CompletelyCover( pBaseGrd, pOtherGrd, rTolerance ) result ( lCompletelyCover )
  !! Returns .true. if the T_GRID objects conform (in terms of cell sizes and extents)
  !! The optional argument rTolerance is the precision for checking the (floating-point)
  !! extent coordinates (this defaults to rDEFAULT_TOLERANCE, below). The tolerance
  !! is set to abs(rTolerance * ( rX1-rX1 ) )
  ! ARGUMENTS
  type (T_GENERAL_GRID), pointer :: pBaseGrd,pOtherGrd
  real (kind=T_SGL), intent(in), optional :: rTolerance
  ! RETURN VALUE
  logical (kind=T_LOGICAL) :: lCompletelyCover
  ! LOCALS
  real (kind=T_SGL) :: rTol
  real (kind=T_SGL) :: rDEFAULT_TOLERANCE

  rDEFAULT_TOLERANCE = pBaseGrd%rGridCellSize * 2.

  if ( present ( rTolerance ) ) then
      rTol = rTolerance
  else
      rTol = rDEFAULT_TOLERANCE
  end if

  if ( ((pBaseGrd%rX0 - pOtherGrd%rX0) > rTol) .and. &
       ((pBaseGrd%rY0 - pOtherGrd%rY0) > rTol) .and. &
       ((pOtherGrd%rX1 - pBaseGrd%rX1) > rTol) .and. &
       ((pOtherGrd%rY1 - pBaseGrd%rY1) > rTol) ) then
      lCompletelyCover = lTRUE
  else
      lCompletelyCover = lFALSE
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
  real (kind=T_SGL),intent(in) :: rXval
  integer (kind=T_INT),intent(out) :: iBefore,iAfter
  real (kind=T_SGL),intent(out) :: rFrac
  ! [ LOCALS ]
  real (kind=T_SGL) :: rRowPosition

  rRowPosition = (pGrd%iNX - 1) * (rXval - pGrd%rX0) / (pGrd%rX1 - pGrd%rX0)
  rFrac = rZERO
  iBefore = floor(rRowPosition) + 1
  iAfter = iBefore + 1
  if ( iBefore > pGrd%iNX .or. iBefore < 1) then
    iBefore = -1
    iAfter = -1
  else if ( iAfter > pGrd%iNX ) then
    iAfter = -1
  else
    rFrac = mod(rRowPosition,rONE)
  end if

  return
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
  real (kind=T_SGL),intent(in) :: rYval
  integer (kind=T_INT),intent(out) :: iBefore,iAfter
  real (kind=T_SGL),intent(out) :: rFrac
  ! [ LOCALS ]
  real (kind=T_SGL) :: rColPosition

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


!> @brief Call PROJ4 to transform coordinates.
!> @details This subroutine calls a Fortran wrapper to the C library
!> PROJ4. A set of input coordinates is transformed to a different
!> coordinate system.
!> @param[inout] pGrd
!> @param[in] sFromPROJ4
!> @param[in] sToPROJ4
subroutine grid_Transform(pGrd, sFromPROJ4, sToPROJ4 )

  use proj
  use, intrinsic :: iso_c_binding

  type ( T_GENERAL_GRID ),pointer :: pGrd
  character (len=*) :: sFromPROJ4, sToPROJ4

  ! [ LOCALS ]
  type(C_PTR) :: pFromPROJ4
  type(C_PTR) :: pToPROJ4
  integer (kind=T_SGL) :: iRetVal
  character (len=256) :: sErrorMessage
  logical (kind=T_LOGICAL) :: lFound
  integer (kind=T_INT) :: i
  REAL(kind=c_double), dimension( pGrd%iNumGridCells ) :: x
  REAL(kind=c_double), dimension( pGrd%iNumGridCells ) :: y
  REAL(kind=c_double), dimension( pGrd%iNumGridCells ) :: z
  logical (kind=T_LOGICAL), dimension(pGrd%iNY, pGrd%iNX) :: lMask

  call grid_PopulateXY(pGrd)

  lMask = lTRUE

  x = pack(pGrd%rX, lMask)
  y = pack(pGrd%rY, lTRUE)
  z = rZERO

  sErrorMessage = repeat(" ",256)

  ! obtain pointer to PROJ.4 projection object for the base projection
  pFromPROJ4 = pj_init_plus(trim(sFromPROJ4))

  print *, pFromPROJ4

  ! obtain pointer to PROJ.4 projection object for the new projection
  pToPROJ4 = pj_init_plus(trim(sToPROJ4))

  print*, pToPROJ4

print *, 5, "  ", dquote(sToPROJ4)
  iRetVal = pj_transform(pFromPROJ4, pToPROJ4, size(x), 1, x, y, z)

print *, 6
  lFound = lFALSE

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


          print *, 7
  ! transfer coordinate values back into an array structure
  pGrd%rX = unpack(x, lMask, pGrd%rX)

  print *, 8
  pGrd%rY = unpack(y, lMask, pGrd%rY)

  ! now update the grid boundaries based on the transformed coordinate values
  pGrd%rX0 = minval(x)
  pGrd%rX1 = maxval(x)
  pGrd%rY0 = minval(y)
  pGrd%rY1 = maxval(y)

print *, 9

end subroutine grid_Transform

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
  !! are constant. Applicable only to T_SGL_GRID grids.
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd
  real (kind=T_SGL),intent(in) :: rXval,rYval
  ! [ RETURN VALUE ]
  real (kind=T_SGL) :: rValue
  ! [ LOCALS ]
  integer (kind=T_INT) :: ib,jb,ia,ja
  real (kind=T_SGL) :: ylocal,u,v

  call grid_LookupColumn(pGrd,rXval,ib,ia,u)
  call Assert ( LOGICAL(ib>0 .and. ia>0,kind=T_LOGICAL), &
               "Requested column value out of range~" &
               //"rXval: "//trim(real2char(rXval)), &
               TRIM(__FILE__),__LINE__)

  ! In some cases, when things really dry out, the y value gets out of range. - truncate.
  if ( rYval < pGrd%rY0 ) then
    ylocal = pGrd%rY0
  else if ( rYval > pGrd%rY1 ) then
    ylocal = pGrd%rY1
  else
    ylocal = rYval
  end if
  call grid_LookupRow(pGrd,ylocal,jb,ja,v)

  call Assert ( LOGICAL(jb>0 .and. ja>0,kind=T_LOGICAL) , &
               "Requested row value out of range~" &
               //"rYval: "//trim(real2char(rXval)), &
               TRIM(__FILE__),__LINE__)

  rValue = (rONE-u) * (rONE-v) * pGrd%rData(jb,ib)   + &
              u  * (rONE-v) * pGrd%rData(jb,ia)   + &
           (rONE-u) *       v  * pGrd%rData(ja,ib)   + &
              u  *       v  * pGrd%rData(ja,ia)

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
!  table (y=pGrd%rY1). Applicable only to T_SGL_GRID grids.
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
!                0.50  1.00  1.50
!               _________________
!          0.0 | 0.50  1.00  1.50
!  Y=APWL  0.1 | 0.45  0.90  1.40
!          0.2 | 0.40  0.80  1.30
!
! SOURCE

function grid_SearchColumn(pGrd,rXval,rZval,rNoData) result ( rValue )
  !! Searches in the y-direction, given the x value 'xval', for the value
  !! 'zval', and returns the value of y in 'ry'. Assumes that the row and
  !! column spacing are constant. The search begins at the _top_ of the
  !! table (y=pGrd%rY1). Applicable only to T_SGL_GRID grids.
  !! Parameter 'rmv' is the missing value code for the grid.
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd
  real (kind=T_SGL),intent(in) :: rXval,rZval,rNoData
  ! [ RETURN VALUE ]
  real (kind=T_SGL) :: rValue
  ! [ LOCALS ]
  integer (kind=T_INT) :: ib,ia,istat,iRow
  real (kind=T_SGL) :: u,v,frac,rprev
  real (kind=T_SGL),dimension(:),allocatable :: rCol
  character (len=128) :: buf

  allocate ( rCol(pGrd%iNY), stat=istat )
  call Assert( LOGICAL(istat==0,kind=T_LOGICAL), &
     "Couldn't allocate temporary array" )

  call grid_LookupColumn(pGrd,rXval,ib,ia,u)
!  write(UNIT=LU_LOG,FMT=*)'lookup ',rXval,ib,ia


  write (unit=buf,fmt=*) "Requested X value ", rXval, " out of range"
  call Assert (LOGICAL(ib>0 .and. ia>0,kind=T_LOGICAL), buf )

  ! interpolate the column of values based on the columns of values
  ! that bracket the real value rXval
  rCol = u*pGrd%rData(:,ia) + (rONE-u)*pGrd%rData(:,ib)
  ! Fix missing values
  do iRow=1,pGrd%iNY
    if ( pGrd%rData(iRow,ia) == rNoData .and. pGrd%rData(iRow,ib) == rNoData ) then
      rCol(iRow) = rNoData
    else if ( pGrd%rData(iRow,ib) == rNoData .and. u>0.9_T_SGL ) then
      rCol(iRow) = pGrd%rData(iRow,ia)
    else if ( pGrd%rData(iRow,ia) == rNoData .and. u<0.1_T_SGL ) then
      rCol(iRow) = pGrd%rData(iRow,ib)
    else if ( pGrd%rData(iRow,ia) == rNoData .or. pGrd%rData(iRow,ib) == rNoData ) then
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
  real (kind=T_SGL),intent(in) :: rXval,rYval
  real (kind=T_SGL) :: rValue
  integer (kind=T_INT) :: iCol,iRow

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
  real (kind=T_DBL) :: rX
  integer (kind=T_INT) :: iColumnNumber

  iColumnNumber = NINT(real(pGrd%iNX, kind=T_SGL) &
               * ( rX - pGrd%rX0 ) / (pGrd%rX1 - pGrd%rX0) + 0.5, kind=T_INT)

  call assert(iColumnNumber > 0 .and. iColumnNumber <= pGrd%iNX, &
     "INTERNAL PROGRAMMING ERROR: Column number out of bounds", &
     trim(__FILE__), __LINE__)

end function grid_GetGridColNum

!----------------------------------------------------------------------

function grid_GetGridRowNum(pGrd,rY)  result(iRowNumber)

  type ( T_GENERAL_GRID ),pointer :: pGrd
  real (kind=T_DBL) :: rY
  integer (kind=T_INT) :: iRowNumber

  iRowNumber = NINT(real(pGrd%iNY, kind=T_SGL) &
               * ( rY - pGrd%rY0 ) / (pGrd%rY1 - pGrd%rY0) + 0.5, kind=T_INT)

  call assert(iRowNumber > 0 .and. iRowNumber <= pGrd%iNY, &
     "INTERNAL PROGRAMMING ERROR: Column number out of bounds", &
     trim(__FILE__), __LINE__)

end function grid_GetGridRowNum

!----------------------------------------------------------------------

function grid_GetGridX(pGrd,iColumnNumber)  result(rX)

  type ( T_GENERAL_GRID ),pointer :: pGrd
  real (kind=T_SGL) :: rX
  integer (kind=T_INT) :: iColumnNumber

  rX = pGrd%rX0 + pGrd%rGridCellSize * (REAL(iColumnNumber, kind=T_SGL) - rHALF)

end function grid_GetGridX

!----------------------------------------------------------------------

function grid_GetGridY(pGrd,iRowNumber)  result(rY)

  type ( T_GENERAL_GRID ),pointer :: pGrd
  real (kind=T_SGL) :: rY
  integer (kind=T_INT) :: iRowNumber

  rY = pGrd%rY1 &
          - pGrd%rGridCellSize * (REAL(iRowNumber, kind=T_SGL) - rHALF)

end function grid_GetGridY

!----------------------------------------------------------------------

subroutine grid_PopulateXY(pGrd)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd         ! pointer to model grid
    ! model options, flags, and other settings

  ! [ LOCALS ]
  integer (kind=T_INT) :: iCol,iRow
  integer (kind=T_INT) :: iStat

  if ( .not. associated(pGrd%rX) ) then

    ALLOCATE (pGrd%rX(pGrd%iNX, pGrd%iNY), STAT=iStat)
    call Assert( iStat == 0, &
       "Could not allocate memory for x-coordinates within grid data structure", &
       trim(__FILE__), __LINE__)
  endif

  if ( .not. associated(pGrd%rY) ) then
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

subroutine grid_gridToGrid_int(pGrdFrom, iArrayFrom, pGrdTo, iArrayTo)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrdFrom   ! pointer to source grid
  type ( T_GENERAL_GRID ),pointer :: pGrdTo     ! pointer to destination grid
  integer (kind=T_INT), dimension(:,:) :: iArrayFrom
  integer (kind=T_INT), dimension(:,:) :: iArrayTo


  ! [ LOCALS ]
  integer (kind=T_INT) :: iCol, iRow
  integer (kind=T_INT) :: iSrc_Col, iSrc_Row

  do iRow=1,pGrdTo%iNY
    do iCol=1,pGrdTo%iNX
      iSrc_Col = grid_GetGridColNum(pGrdFrom,real(pGrdTo%rX(iCol, iRow), kind=T_DBL) )
      iSrc_Row = grid_GetGridColNum(pGrdFrom,real(pGrdTo%rY(iCol, iRow), kind=T_DBL) )
      iArrayTo(iCol,iRow) = iArrayFrom(iSrc_Col, iSrc_Row)
    enddo
  enddo

end subroutine grid_gridToGrid_int

!----------------------------------------------------------------------

subroutine grid_gridToGrid_sgl(pGrdFrom, rArrayFrom, pGrdTo, rArrayTo)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrdFrom   ! pointer to source grid
  type ( T_GENERAL_GRID ),pointer :: pGrdTo     ! pointer to destination grid
  real (kind=T_SGL), dimension(:,:) :: rArrayFrom
  real (kind=T_SGL), dimension(:,:) :: rArrayTo

  ! [ LOCALS ]
  integer (kind=T_INT) :: iCol, iRow
  integer (kind=T_INT) :: iSrc_Col, iSrc_Row

  do iRow=1,pGrdTo%iNY
    do iCol=1,pGrdTo%iNX
      iSrc_Col = grid_GetGridColNum(pGrdFrom,real(pGrdTo%rX(iCol, iRow), kind=T_DBL) )
      iSrc_Row = grid_GetGridColNum(pGrdFrom,real(pGrdTo%rY(iCol, iRow), kind=T_DBL) )
      rArrayTo(iCol,iRow) = rArrayFrom(iSrc_Col, iSrc_Row)
    enddo
  enddo

end subroutine grid_gridToGrid_sgl

end module swb_grid
