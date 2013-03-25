module data_factory

  use types
  use swb_grid
  use netcdf4_support
  use iso_c_binding
  implicit none
  private


  type T_GRID_BOUNDS
    real (kind=c_double) :: rXll, rYll
    real (kind=c_double) :: rXul, rYul
    real (kind=c_double) :: rXlr, rYlr
    real (kind=c_double) :: rXur, rYur
    character (len=256) :: sPROJ4_string
  end type T_GRID_BOUNDS

  type, public :: T_DATA_GRID
    integer (kind=T_INT) :: iSourceDataForm  ! constant, static grid, dynamic grid
    integer (kind=T_INT) :: iSourceDataType        ! real, short, integer, etc.
    integer (kind=T_INT) :: iSourceFileType
    character (len=256) :: sDescription
    character (len=256) :: sSourcePROJ4_string
    character (len=256) :: sSourceFileType
    character (len=256) :: sFilenameTemplate   ! e.g. %Y_%#_prcp.nc
    character (len=256) :: sSourceFilename     ! e.g. 1980_00_prcp.nc
    integer (kind=T_INT) :: iFileCount = 0
    integer (kind=T_INT) :: iFileCountYear = -9999
    logical (kind=T_LOGICAL) :: lProjectionDiffersFromBase = lFALSE

    ! the following are only used if data are being read from a NetCDF file
    character (len=256) :: sVariableName_x = "x"
    character (len=256) :: sVariableName_y = "y"
    character (len=256) :: sVariableName_z = ""
    character (len=256) :: sVariableName_time = "time"

    type (T_GRID_BOUNDS) :: GRID_BOUNDS
    ! units?
    ! conversion factor?

    integer (kind=T_INT) :: iNC_FILE_STATUS
    type (T_NETCDF4_FILE) :: NCFILE
    integer (kind=T_INT) :: iConstantValue
    real (kind=T_SGL) :: rConstantValue

    ! pGrdNative is a grid created to serve as an intermediary between
    ! the native coordinate of the data source file and the project coordinates
    ! in use by swb
    type (T_GENERAL_GRID), pointer :: pGrdNative

  contains

    procedure :: init_const_int => initialize_constant_int_data_object_sub
    procedure :: init_const_real => initialize_constant_real_data_object_sub
    procedure :: init_gridded => initialize_gridded_data_object_sub
    procedure :: initialize_netcdf => initialize_netcdf_data_object_sub

    generic :: initialize => init_const_int, &
                             init_const_real, &
                             init_gridded

    procedure :: getvalues_constant => getvalues_constant_sub
    procedure :: getvalues_gridded => getvalues_gridded_sub
    procedure :: getvalues_netcdf => getvalues_dynamic_netcdf_sub
    procedure :: getvalues => getvalues_sub

 !   procedure :: update => update_data_object_sub
 !   procedure :: destroy => create_data_object_sub
    procedure :: get_filetype => get_source_filetype_fn

    procedure :: set_filecount => set_filecount
    procedure :: reset_filecount => reset_filecount
    procedure :: reset_at_yearend_filecount => reset_at_yearend_filecount
    procedure :: increment_filecount => increment_filecount

    procedure :: set_const_int => set_constant_value_int
    procedure :: set_const_real => set_constant_value_real
    generic :: set_constant => set_const_int, set_const_real

    procedure :: make_filename => make_filename_from_template
    procedure :: definePROJ4 => set_PROJ4_string

  end type T_DATA_GRID

  type (T_DATA_GRID), dimension(7), public, target :: DAT

  integer (kind=T_INT), parameter, public :: LANDUSE_DATA = 1
  integer (kind=T_INT), parameter, public :: AWC_DATA = 2
  integer (kind=T_INT), parameter, public :: SOILS_GROUP_DATA = 3
  integer (kind=T_INT), parameter, public :: FLOWDIR_DATA = 4
  integer (kind=T_INT), parameter, public :: PRECIP_DATA = 5
  integer (kind=T_INT), parameter, public :: TMIN_DATA = 6
  integer (kind=T_INT), parameter, public :: TMAX_DATA = 7

  integer (kind=T_INT), parameter :: NETCDF_FILE_OPEN = 0
  integer (kind=T_INT), parameter :: NETCDF_FILE_CLOSED = 1

contains

  subroutine initialize_constant_real_data_object_sub( &
     this, &
     sDescription, &
     rConstant )

     class (T_DATA_GRID) :: this
     character (len=*) :: sDescription
     real (kind=T_SGL), intent(in) :: rConstant

     this%rConstantValue = rConstant
     this%iSourceDataForm = CONSTANT_GRID
     this%iSourceDataType = DATATYPE_REAL
     this%iSourceFileType = FILETYPE_NONE

  end subroutine initialize_constant_real_data_object_sub

!----------------------------------------------------------------------

  subroutine initialize_constant_int_data_object_sub( &
     this, &
     sDescription, &
     iConstant )

     class (T_DATA_GRID) :: this
     character (len=*) :: sDescription
     integer (kind=T_INT), intent(in) :: iConstant

     this%iConstantValue = iConstant
     this%iSourceDataForm = CONSTANT_GRID
     this%iSourceDataType = DATATYPE_INT
     this%iSourceFileType = FILETYPE_NONE

  end subroutine initialize_constant_int_data_object_sub

!----------------------------------------------------------------------

subroutine initialize_gridded_data_object_sub( &
   this, &
   sDescription, &
   sFileType, &
   iDataType, &
   sFilename, &
   sFilenameTemplate, &
   sPROJ4)

   class (T_DATA_GRID) :: this
   type ( T_GENERAL_GRID ),pointer :: pGrd
   character (len=*) :: sDescription
   character (len=*) :: sFileType
   character (len=*), optional :: sFilename
   integer (kind=T_INT) :: iDataType
   character (len=*), optional :: sFilenameTemplate
   character (len=*), optional :: sPROJ4

   if (present(sPROJ4) ) then
     this%sSourcePROJ4_string = trim(sPROJ4)
     if(.not. str_compare(sPROJ4, pGrd%sPROJ4_string) ) &
         this%lProjectionDiffersFromBase = lTRUE
   else
     this%sSourcePROJ4_string =  ""
   endif

   if (present(sFilename)) then
     this%sSourceFilename = sFilename
     this%iSourceDataForm = STATIC_GRID
   else
     this%sSourceFilename = ""
   endif

   if (present(sFilenameTemplate)) then
     this%sFilenameTemplate = sFilenameTemplate
     this%sSourceFilename = ""
     this%iSourceDataForm = DYNAMIC_GRID
   else
     this%sFilenameTemplate = ""
   endif

   call assert(.not. (len_trim(this%sSourceFilename) > 0 &
      .and. len_trim(this%sFilenameTemplate) > 0), &
      "INTERNAL PROGRAMMING ERROR - values may be assigned to either " &
      //dquote("Filename")//" or the "//dquote("sFilenameTemplate") &
      //" -- NOT BOTH!", trim(__FILE__), __LINE__)


   this%sSourceFileType = sFileType
   this%iSourceFileType = this%get_filetype()

   this%iSourceDataType = iDataType

  call assert(this%iSourceFileType == FILETYPE_ARC_ASCII .or. &
    this%iSourceFileType == FILETYPE_SURFER, "Only Arc ASCII or " &
    //"Surfer grids are supported as static grid inputs (for now).", &
    trim(__FILE__), __LINE__)

  call assert(this%iSourceDataType == DATATYPE_INT .or. &
    this%iSourceDataType == DATATYPE_REAL, "Only integer or " &
    //"real data types are supported as static grid inputs.", &
    trim(__FILE__), __LINE__)

end subroutine initialize_gridded_data_object_sub

!----------------------------------------------------------------------

subroutine initialize_netcdf_data_object_sub( &
   this, &
   sDescription, &
   sFilenameTemplate, &
   iDataType, &
   sPROJ4)

   class (T_DATA_GRID) :: this
   character (len=*) :: sDescription
   character (len=*) :: sFilenameTemplate
   integer (kind=T_INT) :: iDataType
   character (len=*), optional :: sPROJ4

   if (present(sPROJ4) ) then
     this%sSourcePROJ4_string = trim(sPROJ4)
   else
     this%sSourcePROJ4_string =  ""
   endif

   this%sSourceFileType = "NETCDF"
   this%iSourceFileType = this%get_filetype()

   this%iSourceDataForm = DYNAMIC_NETCDF_GRID
   this%iSourceDataType = iDataType

   this%iNC_FILE_STATUS = NETCDF_FILE_CLOSED

end subroutine initialize_netcdf_data_object_sub

!----------------------------------------------------------------------

  subroutine getvalues_sub( this, pGrdBase, iMonth, iDay, iYear, iJulianDay)

    class (T_DATA_GRID) :: this
    type ( T_GENERAL_GRID ), pointer, optional :: pGrdBase
    integer (kind=T_INT), optional :: iMonth, iDay, iYear, iJulianDay

    if ( this%iSourceDataForm == DYNAMIC_NETCDF_GRID ) then

      call getvalues_dynamic_netcdf_sub( this, &
                           pGrdBase,  iMonth, iDay, iYear, iJulianDay)

    elseif(this%iSourceDataForm == DYNAMIC_GRID ) then

      call getvalues_gridded_sub( this, pGrdBase, iMonth, iDay, iYear)

    elseif(this%iSourceDataForm == STATIC_GRID ) then

      call getvalues_gridded_sub( this, pGrdBase)

    elseif(this%iSourceDataForm == CONSTANT_GRID ) then

      call getvalues_constant_sub( this, pGrdBase )

    else

    endif

  end subroutine getvalues_sub

!----------------------------------------------------------------------

subroutine getvalues_constant_sub( this, pGrdBase )

  class (T_DATA_GRID) :: this
  type ( T_GENERAL_GRID ), pointer :: pGrdBase

  select case (this%iSourceDataType)

    case ( DATATYPE_REAL )

      pGrdBase%rData = this%rConstantValue

    case ( DATATYPE_INT)

      pGrdBase%iData = this%iConstantValue

    case default

      call assert(lFALSE, "INTERNAL PROGRAMMING ERROR - Unhandled data type: value=" &
        //trim(asCharacter(this%iSourceDataType)), &
        trim(__FILE__), __LINE__)

    end select

  end subroutine getvalues_constant_sub

!----------------------------------------------------------------------

  subroutine getvalues_gridded_sub( this, pGrdBase, iMonth, iDay, iYear)

    class (T_DATA_GRID) :: this
    type ( T_GENERAL_GRID ), pointer :: pGrdBase
    integer (kind=T_INT), optional :: iMonth
    integer (kind=T_INT), optional :: iDay
    integer (kind=T_INT), optional :: iYear

    call assert(this%iSourceFileType == FILETYPE_ARC_ASCII .or. &
      this%iSourceFileType == FILETYPE_SURFER, "INTERNAL PROGRAMMING ERROR -" &
      //" improper file type in use for a call to this subroutine", &
      trim(__FILE__), __LINE__)

    if ( len_trim(this%sFilenameTemplate) > 0 ) then
      if(.not. (present(iMonth) .and. present(iDay) .and. present(iYear) ) ) &
        call assert(lFALSE, "INTERNAL PROGRAMMING ERROR - month, day, and year" &
          //" arguments must be supplied when calling this subroutine in a " &
          //"dynamic mode.", trim(__FILE__), __LINE__)
      call this%make_filename(iMonth, iDay, iYear)
    endif

    ! create a grid in native coordinates of the source dataset.
    this%pGrdNative => grid_Read( sFileName=this%sSourceFilename, &
       sFileType=this%sSourceFileType, &
       iDataType=this%iSourceDataType )

    ! ensure that PROJ4 string is associated with the native grid
    this%pGrdNative%sPROJ4_string = this%sSourcePROJ4_string

    if( len_trim( this%sSourcePROJ4_string ) > 0 ) then

      call echolog(" ")
      call echolog("Transforming gridded data in file: "//dquote(this%sSourceFilename) )
      call echolog("  FROM: "//squote(this%sSourcePROJ4_string) )
      call echolog("  TO:   "//squote(pGrdBase%sPROJ4_string) )

      ! only invoke the transform procedure if the PROJ4 strings are different
      if (.not. str_compare(this%sSourcePROJ4_string,pGrdBase%sPROJ4_string)) then

        print *, dQuote(this%sSourcePROJ4_string)
        print *, dQuote(pGrdBase%sPROJ4_string)

        call grid_Transform(pGrd=this%pGrdNative, &
                          sFromPROJ4=this%sSourcePROJ4_string, &
                          sToPROJ4=pGrdBase%sPROJ4_string )

      endif

      call Assert( grid_CompletelyCover( pGrdBase, this%pGrdNative ), &
        "Transformed grid read from file "//dquote(this%sSourceFilename) &
        //" doesn't completely cover your model domain.")

      select case (this%iSourceDataType)

        case ( DATATYPE_REAL )

          call grid_gridToGrid(pGrdFrom=this%pGrdNative,&
                              rArrayFrom=this%pGrdNative%rData, &
                              pGrdTo=pGrdBase, &
                              rArrayTo=pGrdBase%rData )

        case ( DATATYPE_INT)

          call grid_gridToGrid_int(pGrdFrom=this%pGrdNative,&
                            iArrayFrom=this%pGrdNative%iData, &
                            pGrdTo=pGrdBase, &
                            iArrayTo=pGrdBase%iData )
        case default

          call assert(lFALSE, "INTERNAL PROGRAMMING ERROR - Unhandled data type: value=" &
            //trim(asCharacter(this%iSourceDataType)), &
            trim(__FILE__), __LINE__)

      end select

    else

      call Assert( grid_Conform( pGrdBase, this%pGrdNative ), &
                        "Non-conforming grid. Filename: " &
                        //dquote(this%pGrdNative%sFilename) )

      select case (this%iSourceDataType)

        case ( DATATYPE_REAL )

          pGrdBase%rData = this%pGrdNative%rData

        case ( DATATYPE_INT)

          pGrdBase%rData = this%pGrdNative%rData

        case default

          call assert(lFALSE, "INTERNAL PROGRAMMING ERROR - Unhandled data type: value=" &
            //trim(asCharacter(this%iSourceDataType)), &
            trim(__FILE__), __LINE__)

      end select

    endif

    call grid_Destroy(this%pGrdNative)

  end subroutine getvalues_gridded_sub

!----------------------------------------------------------------------

subroutine set_constant_value_int( this, iValue )

    class (T_DATA_GRID) :: this
    integer (kind=T_INT) :: iValue

    this%iConstantValue = iValue

end subroutine set_constant_value_int

!----------------------------------------------------------------------

subroutine set_constant_value_real( this, rValue )

    class (T_DATA_GRID) :: this
    real (kind=T_SGL) :: rValue

    this%rConstantValue = rValue

end subroutine set_constant_value_real

!----------------------------------------------------------------------

  subroutine set_filecount( this, iValue, iYear)

    class (T_DATA_GRID) :: this
    integer (kind=T_INT) :: iValue
    integer (kind=T_INT), optional :: iYear

    this%iFileCount = iValue

    if (present(iYear) ) this%iFileCountYear = iYear

  end subroutine set_filecount

!----------------------------------------------------------------------

  subroutine increment_filecount( this )

    class (T_DATA_GRID) :: this

    this%iFileCount = this%iFileCount + 1

  end subroutine increment_filecount

!----------------------------------------------------------------------

  subroutine reset_filecount( this )

    class (T_DATA_GRID) :: this

    this%iFileCount = 0

  end subroutine reset_filecount

!----------------------------------------------------------------------

  subroutine reset_at_yearend_filecount( this, iYear )

    class (T_DATA_GRID) :: this
    integer (kind=T_INT) :: iYear

    if (iYear /= this%iFileCountYear )  then
      this%iFileCount = 0
      this%iFileCountYear = iYear
    endif

  end subroutine reset_at_yearend_filecount

!----------------------------------------------------------------------

  subroutine make_filename_from_template( this, iMonth, iDay, iYear )

    class (T_DATA_GRID) :: this
    integer (kind=T_INT), optional :: iMonth, iDay, iYear

    ! [ LOCALS ]
    character (len=256) :: sNewFilename
    integer (kind=T_INT) :: iPos_Y, iPos_D, iPos_M, iPos, iLen
    logical (kind=T_LOGICAL) :: lMatch
    logical (kind=T_LOGICAL) :: lExist

    iPos_Y = 0; iPos_M = 0; iPos_D = 0; iPos = 0

    ! EXAMPLES of the kinds of templates that we need to be able to understand:
    ! tars1980\prcp.nc   template => "tars%Y\prcp.nc"
    ! prcp_1980_00.nc    template => "prcp_%Y_%m.nc"

    sNewFilename = this%sFilenameTemplate

    do

      lMatch = lFALSE

      if (present(iYear) )   iPos_Y = index(sNewFilename, "%Y")

      if (iPos_Y > 0) then
        lMatch = lTRUE
        iLen=len_trim(sNewFilename)
        sNewFilename = sNewFilename(1:iPos_Y - 1)//trim(asCharacter(iYear)) &
                       //sNewFilename(iPos_Y + 2:iLen)
      endif

      iPos = index(sNewFilename, "%#")

      if (iPos > 0) then
        lMatch = lTRUE
        iLen=len_trim(sNewFilename)
        sNewFilename = sNewFilename(1:iPos-1)//trim(asCharacter(this%iFileCount)) &
                       //sNewFilename(iPos+2:iLen)
      endif

      if (present(iMonth) ) iPos_M = index(sNewFilename, "%M")

      if (iPos_M > 0) then
        lMatch = lTRUE
        iLen=len_trim(sNewFilename)
        sNewFilename = sNewFilename(1:iPos_M - 1)//trim(asCharacter(iMonth)) &
                       //sNewFilename(iPos_M + 2:iLen)
      endif

      if (present(iMonth) ) iPos_M = index(sNewFilename, "%M")

      if (iPos_D > 0) then
        lMatch = lTRUE
        iLen=len_trim(sNewFilename)
        sNewFilename = sNewFilename(1:iPos_D - 1)//trim(asCharacter(iDay)) &
                       //sNewFilename(iPos_D + 2:iLen)
      endif

      if (.not. lMatch) exit

    enddo

    this%sSourceFilename = trim(sNewFilename)

    ! does this file actually exist?
    inquire( file=this%sSourceFilename, exist=lExist )

    call assert(lExist, "The filename created from your template refers to " &
      //"a nonexistent file. ~ Attempted to open filename "&
      //dquote(this%sSourceFilename), trim(__FILE__), __LINE__)

  end subroutine make_filename_from_template

!----------------------------------------------------------------------

  subroutine getvalues_dynamic_netcdf_sub( this, pGrdBase, &
     iMonth, iDay, iYear, iJulianDay)

    class (T_DATA_GRID) :: this
    type ( T_GENERAL_GRID ), pointer :: pGrdBase
    integer (kind=T_INT) :: iMonth, iDay, iYear, iJulianDay
    integer (kind=c_int) :: iTimeIndex

    ! call once at start of run...
    if ( this%iFileCountYear < 0 ) call this%set_filecount(-1, iYear)

    if (this%iNC_FILE_STATUS == NETCDF_FILE_OPEN) then

      ! check to see whether currently opened file is within date range
      ! if past date range, close file

      if ( netcdf_date_within_range(NCFILE=this%NCFILE, iJulianDay=iJulianDay ) ) &
        call netcdf_close_file( NCFILE=this%NCFILE )

    endif

    if ( this%iNC_FILE_STATUS == NETCDF_FILE_CLOSED ) then

      ! increment or reset file counter based on current year value
      call this%increment_filecount()
      call this%reset_at_yearend_filecount(iYear)

      call make_filename_from_template( this, &
        iMonth=iMonth, &
        iYear=iYear )



      this%NCFILE = netcdf_open_and_prepare(sFilename=this%sSourceFilename, &
             sVarName_x=this%sVariableName_x, &
             sVarName_y=this%sVariableName_y, &
             sVarName_z=this%sVariableName_z, &
             sVarName_time=this%sVariableName_time, &
             iLU=LU_LOG)

    endif

    iTimeIndex = netcdf_date_to_index( NCFILE=this%NCFILE, &
                                       iJulianDay=iJulianDay )

    if ( this%iSourceDataType == DATATYPE_INT) then

!      pGrdBase%iData=netcdf_get_variable_short(NCFILE=this%NCFILE, &
!         iNC_VarID=this%NCFILE%iVarID_z, &
!         iNC_Start= &,
!         iNC_Count= &,
!         iNC_Stride )

      pGrdBase%iData = 1

    elseif ( this%iSourceDataType == DATATYPE_REAL) then

!      pGrdBase%rData=netcdf_get_variable_double(NCFILE=this%NCFILE, &
!         iNC_VarID=, &
!         iNC_Start= &,
!         iNC_Count= &,
!         iNC_Stride )

      pGrdBase%rData = 0.1

    endif






  end subroutine getvalues_dynamic_netcdf_sub

!----------------------------------------------------------------------

  function get_source_filetype_fn(this)  result(iFileType)

     class (T_DATA_GRID) :: this
     integer (kind=T_INT) :: iFileType

     if (str_compare(this%sSourceFileType, "ARC_GRID") ) then

       iFileType = FILETYPE_ARC_ASCII

     elseif (str_compare(this%sSourceFileType, "SURFER") ) then

       iFileType = FILETYPE_SURFER

     elseif (str_compare(this%sSourceFileType, "NETCDF") ) then

       iFileType = FILETYPE_NETCDF

     else

       call assert(lFALSE, "Unknown input file type specified. ~"&
         //"  filename: "//dquote(this%sSourceFilename) &
         //"~  file type specified as: "//dquote(this%sSourceFileType), &
         trim(__FILE__), __LINE__)

     endif

  end function get_source_filetype_fn

!----------------------------------------------------------------------

  subroutine set_PROJ4_string(this, sPROJ4_string)

     class (T_DATA_GRID) :: this
     character (len=*), optional :: sPROJ4_string

     this%sSourcePROJ4_string = sPROJ4_string

  end subroutine set_PROJ4_string

!----------------------------------------------------------------------

  subroutine calc_project_boundaries(this, pGrdBase)

    class (T_DATA_GRID) :: this
    type ( T_GENERAL_GRID ), pointer :: pGrdBase

    ! [ LOCALS ]
    integer (kind=T_INT), dimension(4) :: iRetVal
    real (kind=T_SGL) :: rMultiplier = 10.

    ! ensure that there is sufficient coverage on all sides of grid
    this%GRID_BOUNDS%rXll = pGrdBase%rX0 - pGrdBase%rGridCellSize * rMultiplier
    this%GRID_BOUNDS%rYll = pGrdBase%rY0 - pGrdBase%rGridCellSize * rMultiplier
    this%GRID_BOUNDS%rXur = pGrdBase%rX1 + pGrdBase%rGridCellSize * rMultiplier
    this%GRID_BOUNDS%rYur = pGrdBase%rY1 + pGrdBase%rGridCellSize * rMultiplier
    this%GRID_BOUNDS%rXlr = pGrdBase%rX1 + pGrdBase%rGridCellSize * rMultiplier
    this%GRID_BOUNDS%rYlr = pGrdBase%rY0 - pGrdBase%rGridCellSize * rMultiplier
    this%GRID_BOUNDS%rXul = pGrdBase%rX0 - pGrdBase%rGridCellSize * rMultiplier
    this%GRID_BOUNDS%rYul = pGrdBase%rY1 + pGrdBase%rGridCellSize * rMultiplier

    ! now transform the project coordinates to native coordinates so we can
    ! use the native coordinate boundaries to "cookie-cut" only the data
    ! pertinent to our project area.
    iRetVal(1) = pj_init_and_transform(pGrdBase%sPROJ4_string//C_NULL_CHAR, &
                this%sSourcePROJ4_string//C_NULL_CHAR, 1, &
                [this%GRID_BOUNDS%rXll], [this%GRID_BOUNDS%rYll] )

    iRetVal(2) = pj_init_and_transform(pGrdBase%sPROJ4_string//C_NULL_CHAR, &
                this%sSourcePROJ4_string//C_NULL_CHAR, 1, &
                [this%GRID_BOUNDS%rXur], [this%GRID_BOUNDS%rYur] )

    iRetVal(3) = pj_init_and_transform(pGrdBase%sPROJ4_string//C_NULL_CHAR, &
                this%sSourcePROJ4_string//C_NULL_CHAR, 1, &
                [this%GRID_BOUNDS%rXul], [this%GRID_BOUNDS%rYul] )

    iRetVal(4) = pj_init_and_transform(pGrdBase%sPROJ4_string//C_NULL_CHAR, &
                this%sSourcePROJ4_string//C_NULL_CHAR, 1, &
                [this%GRID_BOUNDS%rXlr], [this%GRID_BOUNDS%rYlr] )

  end subroutine calc_project_boundaries

end module data_factory
