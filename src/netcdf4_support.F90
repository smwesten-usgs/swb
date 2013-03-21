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
    integer (kind=c_int) :: iNC_DimID
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
    integer (kind=c_int) :: iNC_VarID
    integer (kind=c_int) :: iNC_VarType
    integer (kind=c_int) :: iNumberOfDimensions
    integer (kind=c_int), dimension(0:4) :: iNC_DimID
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
    real (kind=c_double), dimension(2) :: dpFirstAndLastTimeValues
    integer (kind=c_int) :: iVarID_x = -9999
    integer (kind=c_int) :: iVarID_y = -9999
    integer (kind=c_int) :: iVarID_z = -9999
    integer (kind=c_int) :: iVarID_time = -9999
    integer (kind=c_int) :: iVarIndex_x = -9999
    integer (kind=c_int) :: iVarIndex_y = -9999
    integer (kind=c_int) :: iVarIndex_z = -9999
    integer (kind=c_int) :: iVarIndex_time = -9999

    type (T_NETCDF_DIMENSION), dimension(:), pointer :: NC_DIM
    type (T_NETCDF_VARIABLE), dimension(:), pointer :: NC_VAR
    type (T_NETCDF_ATTRIBUTE), dimension(:), pointer :: NC_ATT
  end type T_NETCDF4_FILE

contains

!----------------------------------------------------------------------

function netcdf_date_within_range( NCFILE, iJulianDay)  result( lWithinRange )

  type (T_NETCDF4_FILE) :: NCFILE
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

function netcdf_date_to_index( NCFILE, iJulianDay )  result(iVarIndex)

   type (T_NETCDF4_FILE) :: NCFILE
   integer (kind=T_INT) :: iJulianDay
   integer (kind=c_int) :: iVarIndex


end function netcdf_date_to_index

!----------------------------------------------------------------------

function netcdf_return_VarID( NCFILE, iVarIndex)   result(iVarID)

   type (T_NETCDF4_FILE) :: NCFILE
   integer (kind=T_INT) :: iVarIndex
   integer (kind=T_INT) :: iVarID

   type (T_NETCDF_VARIABLE), pointer :: pNC_VAR

   pNC_VAR => NCFILE%NC_VAR(iVarIndex)

   iVarID = pNC_VAR%iNC_VarID

end function netcdf_return_VarID

!----------------------------------------------------------------------

function netcdf_return_DimID( NCFILE, iDimIndex)   result(iDimID)

   type (T_NETCDF4_FILE) :: NCFILE
   integer (kind=T_INT) :: iDimIndex
   integer (kind=T_INT) :: iDimID

   type (T_NETCDF_DIMENSION), pointer :: pNC_DIM

   pNC_DIM => NCFILE%NC_DIM(iDimIndex)

   iDimID = pNC_DIM%iNC_DimID

end function netcdf_return_DimID

!----------------------------------------------------------------------

function netcdf_return_VarIndex( NCFILE, iVarID)   result(iVarIndex)

   type (T_NETCDF4_FILE) :: NCFILE
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

function netcdf_return_DimIndex( NCFILE, iDimID)   result(iDimIndex)

   type (T_NETCDF4_FILE) :: NCFILE
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

function return_start_indices(NCFILE, iJulianDay)  result( iIndices)

  type (T_NETCDF4_FILE ) :: NCFILE
  integer (kind=c_int) :: iJulianDay

  integer (kind=c_int), dimension(:), allocatable :: iIndices





end function return_start_indices

!----------------------------------------------------------------------

function netcdf_open_and_prepare(sFilename, sVarName_x, &
    sVarName_y, sVarName_z, sVarName_time, iLU)     result(NCFILE)

  character (len=*) :: sFilename
  character (len=*), optional :: sVarName_x
  character (len=*), optional :: sVarName_y
  character (len=*), optional :: sVarName_z
  character (len=*), optional :: sVarName_time
  integer (kind=T_INT), optional :: iLU

  ! [ LOCALS ]
  type (T_NETCDF4_FILE ) :: NCFILE
  logical (kind=T_LOGICAL) :: lFileOpen
  character (len=256) :: sX_VarName
  character (len=256) :: sY_VarName
  character (len=256) :: sZ_VarName
  character (len=256) :: sTime_VarName

  NCFILE = netcdf_open_file(sFilename)
  call netcdf_read_dimension_info( NCFILE )
  call netcdf_read_variable_info( NCFILE )

  if( present(iLU) ) then
    inquire (unit=iLU, opened=lFileOpen)
    if ( lFileOpen )  call netcdf_dump_cdl( NCFILE, iLU)
  endif

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

  call netcdf_get_variable_ids( NCFILE, trim(sX_VarName), &
    trim(sY_VarName), trim(sZ_VarName), trim(sTime_VarName) )

  NCFILE%dpFirstAndLastTimeValues = netcdf_get_first_and_last(NCFILE=NCFILE, &
      iVarIndex=NCFILE%iVarIndex_time)

  call netcdf_calculate_time_range(NCFILE)

end function netcdf_open_and_prepare

!----------------------------------------------------------------------

function netcdf_open_file( sFilename)   result( NCFILE )

  character (len=*) :: sFilename

  type (T_NETCDF4_FILE ) :: NCFILE

  write(UNIT=LU_LOG,FMT="(a)") "Attempting to open READONLY NetCDF file: " &
    //dquote(sFilename)

  call nc_trap( nc_open(sFilename//c_null_char, &
                NC_READONLY, NCFILE%iNCID) )

  call nc_trap( nc_inq_format(ncid=NCFILE%iNCID, formatp=NCFILE%iFileFormat) )

  write(UNIT=LU_LOG,FMT="(a,/,a,i0,/,a)") "   Succeeded.","  ncid: ",NCFILE%iNCID, &
         "  format: "//trim(NETCDF_FORMAT_STRING(NCFILE%iFileFormat) )

  NCFILE%sFilename = sFilename
  nullify(NCFILE%NC_DIM)
  nullify(NCFILE%NC_VAR)
  nullify(NCFILE%NC_ATT)

end function netcdf_open_file

!----------------------------------------------------------------------

subroutine nc_trap( iResultCode )

  integer (kind=c_int) :: iResultCode
  type(c_ptr) :: cpResult
  character (len=256) :: sTextString

  if (iResultCode /= 0) then

    cpResult = nc_strerror(iResultCode)
    sTextString = char_ptr_to_fortran_string( cpResult )

    write(*, fmt="(/,a,i5,/)") trim( sTextString  )//" | error code was: ", iResultCode

    stop( ""  )
  endif

end subroutine nc_trap

!----------------------------------------------------------------------

subroutine netcdf_close_file( NCFILE)

  type (T_NETCDF4_FILE ) :: NCFILE
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR

  integer (kind=T_INT) :: iIndex

  call nc_trap( nc_close(NCFILE%iNCID) )

  do iIndex=0, NCFILE%iNumberOfVariables - 1

    pNC_VAR => NCFILE%NC_VAR(iIndex)

    if (pNC_VAR%iNumberOfAttributes == 0 ) cycle

    if (associated( pNC_VAR%NC_ATT ))  deallocate( pNC_VAR%NC_ATT )

  enddo

  if (associated( NCFILE%NC_VAR ))  deallocate( NCFILE%NC_VAR )
!  if (associated( NCFILE%NC_ATT ))  deallocate( NCFILE%NC_ATT )
  if (associated( NCFILE%NC_DIM ))  deallocate( NCFILE%NC_DIM )

end subroutine netcdf_close_file

!----------------------------------------------------------------------

subroutine netcdf_read_dimension_info( NCFILE )

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE
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

end subroutine netcdf_read_dimension_info

!----------------------------------------------------------------------

subroutine netcdf_read_variable_info( NCFILE )

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE

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

        call netcdf_get_attribute( NCFILE=NCFILE, pNC_ATT=pNC_ATT, &
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

    call netcdf_get_attribute( NCFILE=NCFILE, pNC_ATT=pNC_ATT, &
      iNC_VarID=NC_GLOBAL, iAttNum=iIndex )

  enddo

end subroutine netcdf_read_variable_info

!----------------------------------------------------------------------

subroutine netcdf_get_attribute( NCFILE, pNC_ATT, iNC_VarID, iAttNum )

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

    case (NC_DOUBLE)

      call nc_trap( nc_get_att_double(ncid=NCFILE%iNCID, &
        varid=iNC_VarID, &
        name=sAttName, &
        ip=cdAttValue) )

      write(pNC_ATT%sAttributeValue, fmt="(20(f16.6, 1x))" ) &
        (cdAttValue(iIndex), iIndex=0, pNC_ATT%iNC_AttSize-1)

    case default

  end select

end subroutine netcdf_get_attribute

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
  integer (kind=T_INT), dimension(:) :: iNC_Start
  integer (kind=T_INT), dimension(:) :: iNC_Count
  integer (kind=T_INT), dimension(:) :: iNC_Stride
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

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE
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
      "INTERNAL PROGRAMMING ERROR - bounds exceeded in NC_FILE%NCVAR" &
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
        iNC_Count=[2], &
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

  call assert(NCFILE%iVarID_time >= 0, "INTERNAL PROGRAMMING ERROR -- " &
    //"netcdf_get_time_units must be called only after a call is made to ~" &
    //"netcdf_get_variable_ids", trim(__FILE__), __LINE__)

  pNC_VAR => NCFILE%NC_VAR(NCFILE%iVarID_time)

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

subroutine netcdf_get_variable_ids( NCFILE, sX_VarName, sY_VarName, &
   sZ_VarName, sTime_VarName)

  type (T_NETCDF4_FILE), intent(inout) :: NCFILE

  character (len=*) :: sX_VarName
  character (len=*) :: sY_VarName
  character (len=*) :: sZ_VarName
  character (len=*) :: sTime_VarName

   ! [ LOCALS ]
   integer (kind=T_INT) :: iIndex
   type (T_NETCDF_VARIABLE), pointer :: pNC_VAR

   NCFILE%iVarID_x = -9999; NCFILE%iVarID_y = -9999
   NCFILE%iVarID_z = -9999; NCFILE%iVarID_time = -9999

   do iIndex=0, NCFILE%iNumberOfVariables - 1

     pNC_VAR => NCFILE%NC_VAR(iIndex)

     if (str_compare(pNC_VAR%sVariableName, sX_VarName) ) &
       NCFILE%iVarIndex_x = iIndex
       NCFILE%iVarID_x = pNC_VAR%iNC_VarID

     if (str_compare(pNC_VAR%sVariableName, sY_VarName) ) &
       NCFILE%iVarIndex_y = iIndex
       NCFILE%iVarID_y = pNC_VAR%iNC_VarID

     if (str_compare(pNC_VAR%sVariableName, sZ_VarName) ) &
       NCFILE%iVarIndex_z = iIndex
       NCFILE%iVarID_z = pNC_VAR%iNC_VarID

     if (str_compare(pNC_VAR%sVariableName, sTime_VarName) ) &
       NCFILE%iVarIndex_time = iIndex
       NCFILE%iVarID_time = pNC_VAR%iNC_VarID

   enddo

   call assert(NCFILE%iVarID_x >= 0, &
     "Unable to find the variable named "//dquote(sX_VarName)//" in " &
     //"file "//dquote(NCFILE%sFilename), trim(__FILE__), __LINE__)

   call assert(NCFILE%iVarID_y >= 0, &
     "Unable to find the variable named "//dquote(sY_VarName)//" in " &
     //"file "//dquote(NCFILE%sFilename), trim(__FILE__), __LINE__)

   call assert(NCFILE%iVarID_z >= 0, &
     "Unable to find the variable named "//dquote(sZ_VarName)//" in " &
     //"file "//dquote(NCFILE%sFilename), trim(__FILE__), __LINE__)

   call assert(NCFILE%iVarID_time >= 0, &
     "Unable to find the variable named "//dquote(sTime_VarName)//" in " &
     //"file "//dquote(NCFILE%sFilename), trim(__FILE__), __LINE__)

end subroutine netcdf_get_variable_ids

!----------------------------------------------------------------------

function netcdf_get_index_double(NCFILE, sVariableName, dpValue)  result(iIndex)

  type (T_NETCDF4_FILE ) :: NCFILE
  character (len=*) :: sVariableName
  real (kind=c_double) :: dpValue
  integer (kind=c_int) :: iIndex
  integer (kind=T_INT) :: iCount
  integer (kind=T_INT) :: i

  ! [ LOCALS ]
  integer (kind=c_int) :: iNC_VarID
  real (kind=c_double), dimension(:), allocatable :: dpValues
  type (T_NETCDF_VARIABLE), pointer :: pNC_VAR
  real (kind=c_double) :: dpMin, dpDiff
  logical :: lMatch

  iNC_VarID = netcdf_get_varid(NCFILE, trim(sVariableName) )

  call assert(iNC_VarID >=0 .and. iNC_VarID <= ubound(NCFILE%NC_VAR, 1), &
    "INTERNAL PROGRAMMING ERROR? - iNC_VarID out of bounds", &
    trim(__FILE__), __LINE__)

  pNC_VAR => NCFILE%NC_VAR(iNC_VarID)
  iCount = int(NCFILE%NC_DIM(pNC_VAR%iNC_DimID(0))%iNC_DimSize, kind=T_INT)

  dpValues = netcdf_get_variable_double(NCFILE=NCFILE, &
         iNC_VarID=iNC_VarID, &
         iNC_Start=[0], &
         iNC_Count=[iCount-1], &
         iNC_Stride=[1])

  iIndex = -999

  dpMin = 1.e20

  lMatch = lFALSE
  do i=lbound(dpValues, 1), ubound(dpValues, 1)

    dpDiff = (dpValues(i) - dpValue)**2
    if(dpDiff < dpMin) then
      dpMin = dpDiff
      iIndex = i
    endif

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
