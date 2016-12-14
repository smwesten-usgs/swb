module data_factory

  use types
  use swb_grid
  use netcdf4_support
  use iso_c_binding, only : c_int, c_bool, c_float, c_double
  implicit none
  private

  integer (kind=c_int), public, parameter :: NETCDF_FILE_OPEN = 27
  integer (kind=c_int), public, parameter :: NETCDF_FILE_CLOSED = 42

  integer (kind=c_int), parameter, public :: FILE_TEMPLATE_CAPITALIZED_MONTHNAME = 0
  integer (kind=c_int), parameter, public :: FILE_TEMPLATE_LOWERCASE_MONTHNAME   = 1
  integer (kind=c_int), parameter, public :: FILE_TEMPLATE_UPPERCASE_MONTHNAME   = 2

  type, public :: T_DATA_GRID
    integer (kind=c_int)  :: iSourceDataForm    ! constant, static grid, dynamic grid
    integer (kind=c_int)  :: iSourceDataType = DATATYPE_NA  ! real, short, integer, etc.
    integer (kind=c_int)  :: iSourceFileType  ! Arc ASCII, Surfer, NetCDF
    integer (kind=c_int)  :: iTargetDataType = DATATYPE_NA  ! Fortran real, integer, etc.
    character (len=256)   :: sDescription = ""
    character (len=256)   :: sSourcePROJ4_string
    character (len=256)   :: sSourceFileType
    character (len=256)   :: sFilenameTemplate
    integer (kind=c_int)  :: iFilename_Monthname_Capitalization_Rule = FILE_TEMPLATE_CAPITALIZED_MONTHNAME
    character (len=256)   :: sSourceFilename      ! e.g. 1980_00_prcp.nc
    character (len=256)   :: sOldFilename = "NA"  ! e.g. 1980_00_prcp.nc
    integer (kind=c_int)  :: iFileCount = -1
    integer (kind=c_int)  :: iFileCountYear = -9999
    logical (kind=c_bool) :: lProjectionDiffersFromBase = lFALSE
    real (kind=c_float)   :: rMinAllowedValue = -rBIGVAL     ! default condition is to impose
    real (kind=c_float)   :: rMaxAllowedValue = rBIGVAL      ! no bounds on data
    integer (kind=c_int)  :: iMinAllowedValue = -iBIGVAL     ! default condition is to impose
    integer (kind=c_int)  :: iMaxAllowedValue = iBIGVAL      ! no bounds on data
    real (kind=c_float)   :: rMissingValuesCode = -rBIGVAL
    integer (kind=c_int)  :: iMissingValuesCode = -iBIGVAL
    character (len=2)     :: sMissingValuesOperator = "<="
    integer (kind=c_int)  :: iMissingValuesAction = 0
    real (kind=c_double)  :: rScaleFactor = 1.0_c_double
    real (kind=c_double)  :: rAddOffset = 0.0_c_double
    real (kind=c_double)  :: rX_Coord_AddOffset = 0.0_c_double
    real (kind=c_double)  :: rY_Coord_AddOffset = 0.0_c_double
    real (kind=c_double)  :: rUserOffset = 0.0_c_double
    real (kind=c_double)  :: rUserScaleFactor = 1.0_c_double

    logical (kind=c_bool) :: lMissingFilesAreAllowed = lFALSE
    logical (kind=c_bool) :: lFlipHorizontal = lFALSE
    logical (kind=c_bool) :: lFlipVertical = lFALSE
    integer (kind=c_int)  :: iMajorityFilterType = NO_MAJORITY_FILTER

    integer (kind=c_int)  :: iDaysToPadAtYearsEnd = 0
    integer (kind=c_int)  :: iDaysToPadIfLeapYear = 1
    integer (kind=c_int)  :: iStartYear = -9999
    integer (kind=c_int)  :: iEndYear = -9999
    logical (kind=c_bool) :: lPadReplaceWithZero = lFALSE
    logical (kind=c_bool) :: lPadValues = lFALSE

    ! the following are only used if data are being read from a NetCDF file
    character (len=256) :: sVariableName_x = "x"
    character (len=256) :: sVariableName_y = "y"
    character (len=256) :: sVariableName_z = ""
    character (len=256) :: sVariableName_time = "time"
    character (len=3)   :: sVariableOrder = "tyx"

    type (T_GRID_BOUNDS) :: GRID_BOUNDS_NATIVE
    type (T_GRID_BOUNDS) :: GRID_BOUNDS_BASE

    integer (kind=c_int)  :: iNC_FILE_STATUS = NETCDF_FILE_CLOSED
    type (T_NETCDF4_FILE) :: NCFILE

    integer (kind=c_int)    :: iNC_ARCHIVE_STATUS = NETCDF_FILE_CLOSED
    type (T_NETCDF4_FILE)   :: NCFILE_ARCHIVE
    integer (kind=c_size_t) :: iNCFILE_RECNUM = 0

    integer (kind=c_int) :: iConstantValue
    real (kind=c_float)  :: rConstantValue

    ! pGrdNative is a grid created to serve as an intermediary between
    ! the native coordinate of the data source file and the project coordinates
    ! in use by swb
    type (T_GENERAL_GRID), pointer :: pGrdNative => null()
    logical (kind=c_bool) :: lGridIsPersistent = lFALSE
    logical (kind=c_bool) :: lGridHasChanged = lFALSE
    logical (kind=c_bool) :: lPerformFullInitialization = lTRUE
    logical (kind=c_bool) :: lCreateLocalNetCDFArchive = lFALSE

    type (T_GENERAL_GRID), pointer :: pGrdBase => null()

  contains

    procedure, private :: init_const_int => initialize_constant_int_data_object_sub
    procedure, private :: init_const_real => initialize_constant_real_data_object_sub
    procedure, private :: init_gridded => initialize_gridded_data_object_sub

    procedure, public  :: initialize_netcdf => initialize_netcdf_data_object_sub

    generic, public :: initialize => init_const_int, &
                             init_const_real, &
                             init_gridded

    procedure, public :: set_user_offset => set_user_offset_sub
    procedure, public :: set_user_scale => set_user_scale_sub
    procedure, public :: set_X_offset => set_X_coord_offset_sub
    procedure, public :: set_Y_offset => set_Y_coord_offset_sub

    procedure, private :: set_minimum_allowable_value_int_sub
    procedure, private :: set_minimum_allowable_value_real_sub
    procedure, private :: set_maximum_allowable_value_int_sub
    procedure, private :: set_maximum_allowable_value_real_sub
    generic :: set_valid_minimum => set_minimum_allowable_value_int_sub, &
                                    set_minimum_allowable_value_real_sub
    generic :: set_valid_maximum => set_maximum_allowable_value_int_sub, &
                                    set_maximum_allowable_value_real_sub

    procedure, public :: set_grid_flip_horizontal => set_grid_flip_horizontal_sub
    procedure, public :: set_grid_flip_vertical => set_grid_flip_vertical_sub

    procedure, public :: getvalues_constant => getvalues_constant_sub
    procedure, public :: getvalues_gridded => getvalues_gridded_sub

    procedure, public :: getvalues_netcdf => getvalues_dynamic_netcdf_sub

    procedure, public :: getvalues => getvalues_sub

 !   procedure :: update => update_data_object_sub
 !   procedure :: destroy => create_data_object_sub
    procedure :: get_filetype => get_source_filetype_fn

    procedure, public :: set_filecount => set_filecount
    procedure, public :: reset_filecount => reset_filecount
    procedure, public :: reset_at_yearend_filecount => reset_at_yearend_filecount
    procedure, public :: increment_filecount => increment_filecount

    procedure, private :: set_const_int => set_constant_value_int
    procedure, private :: set_const_real => set_constant_value_real
    generic, public :: set_constant => set_const_int, set_const_real

    procedure, public :: make_filename => make_filename_from_template
    procedure, public :: set_PROJ4 => set_PROJ4_string_sub
    procedure, public :: set_variable_order => set_variable_order_sub
    procedure, public :: dump_data_structure => dump_data_structure_sub
    procedure, public :: set_make_local_archive => set_archive_local_sub
    procedure, public :: put_values_to_archive => put_values_to_local_NetCDF_sub
    procedure, public :: transfer_from_native => transform_grid_to_grid

    procedure, private :: enforce_limits_real => data_GridEnforceLimits_real
    procedure, private :: enforce_limits_int => data_GridEnforceLimits_int
    generic, public    :: enforce_limits => enforce_limits_real, enforce_limits_int

    procedure, private :: create_missing_values_mask_real => data_CreateMissingValuesMask_real
    procedure, private :: create_missing_values_mask_int  => data_CreateMissingValuesMask_int
    generic, public    :: create_missing_values_mask => create_missing_values_mask_real, &
                                                        create_missing_values_mask_int

    procedure, private :: apply_scale_and_offset_real => data_GridApplyScaleAndOffset_real
    procedure, private :: apply_scale_and_offset_int  => data_GridApplyScaleAndOffset_int
    generic, public    :: apply_scale_and_offset => apply_scale_and_offset_int, &
                                                    apply_scale_and_offset_real

    procedure, public :: calc_project_boundaries => calc_project_boundaries
    procedure, public :: test_for_need_to_pad_values => test_for_need_to_pad_values

  end type T_DATA_GRID

  type (T_DATA_GRID), dimension(13), public, target :: DAT
  type (T_DATA_GRID), dimension(:), public, target, allocatable :: DAT_EXTRA

  integer (kind=c_int), parameter, public :: LANDUSE_DATA = 1
  integer (kind=c_int), parameter, public :: AWC_DATA = 2
  integer (kind=c_int), parameter, public :: SOILS_GROUP_DATA = 3
  integer (kind=c_int), parameter, public :: FLOWDIR_DATA = 4
  integer (kind=c_int), parameter, public :: ROUTING_FRAC_DATA = 5
  integer (kind=c_int), parameter, public :: PRECIP_DATA = 6
  integer (kind=c_int), parameter, public :: TMIN_DATA = 7
  integer (kind=c_int), parameter, public :: TMAX_DATA = 8
  integer (kind=c_int), parameter, public :: REL_HUM_DATA = 9
  integer (kind=c_int), parameter, public :: SOL_RAD_DATA = 10
  integer (kind=c_int), parameter, public :: WIND_VEL_DATA = 11
  integer (kind=c_int), parameter, public :: MASK_DATA = 12
  integer (kind=c_int), parameter, public :: IRRIGATED_LAND_MASK_DATA = 13

  integer (kind=c_int), parameter, public :: MISSING_VALUES_ZERO_OUT = 0
  integer (kind=c_int), parameter, public :: MISSING_VALUES_REPLACE_WITH_MEAN = 1

contains

  subroutine initialize_constant_real_data_object_sub( this, &
     sDescription, &
     rConstant )

     class (T_DATA_GRID) :: this
     character (len=*) :: sDescription
     real (kind=c_float), intent(in) :: rConstant

     this%rConstantValue = rConstant
     this%sDescription = trim(sDescription)
     this%iSourceDataForm = CONSTANT_GRID
     this%iSourceDataType = DATATYPE_REAL
     this%iTargetDataType = DATATYPE_REAL
     this%iSourceFileType = FILETYPE_NONE

    call netcdf_nullify_data_struct( NCFILE=this%NCFILE )
    call netcdf_nullify_data_struct( NCFILE=this%NCFILE_ARCHIVE )

  end subroutine initialize_constant_real_data_object_sub

!----------------------------------------------------------------------

  subroutine initialize_constant_int_data_object_sub( this, &
    sDescription, &
    iConstant )

    class (T_DATA_GRID) :: this
    character (len=*) :: sDescription
    integer (kind=c_int), intent(in) :: iConstant

    this%iConstantValue = iConstant
    this%sDescription = trim(sDescription)
    this%iSourceDataForm = CONSTANT_GRID
    this%iSourceDataType = DATATYPE_INT
    this%iTargetDataType = DATATYPE_INT
    this%iSourceFileType = FILETYPE_NONE

    call netcdf_nullify_data_struct( NCFILE=this%NCFILE )
    call netcdf_nullify_data_struct( NCFILE=this%NCFILE_ARCHIVE )

  end subroutine initialize_constant_int_data_object_sub

!----------------------------------------------------------------------

subroutine initialize_gridded_data_object_sub( this, &
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
   integer (kind=c_int) :: iDataType
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
     this%lGridIsPersistent = lTRUE
   else
     this%sFilenameTemplate = ""
   endif

   call assert(.not. (len_trim(this%sSourceFilename) > 0 &
      .and. len_trim(this%sFilenameTemplate) > 0), &
      "INTERNAL PROGRAMMING ERROR - values may be assigned to either " &
      //dquote("Filename")//" or the "//dquote("sFilenameTemplate") &
      //" -- NOT BOTH", trim(__FILE__), __LINE__)

   this%sSourceFileType = sFileType
   this%iSourceFileType = this%get_filetype()

   this%iSourceDataType = iDataType
   this%iTargetDataType = iDataType

   this%sDescription = trim(sDescription)

  call assert(this%iSourceFileType == FILETYPE_ARC_ASCII .or. &
    this%iSourceFileType == FILETYPE_SURFER, "Only Arc ASCII or " &
    //"Surfer grids are supported as static grid inputs (for now).", &
    trim(__FILE__), __LINE__)

  call assert(this%iSourceDataType == DATATYPE_INT .or. &
    this%iSourceDataType == DATATYPE_REAL, "Only integer or " &
    //"real data types are supported as static grid inputs.", &
    trim(__FILE__), __LINE__)

  nullify(this%pGrdNative)
  call netcdf_nullify_data_struct( NCFILE=this%NCFILE )
  call netcdf_nullify_data_struct( NCFILE=this%NCFILE_ARCHIVE )

end subroutine initialize_gridded_data_object_sub

!----------------------------------------------------------------------

subroutine initialize_netcdf_data_object_sub( this, &
   sDescription, &
   iDataType, &
   pGrdBase, &
   sFilename, &
   sFilenameTemplate, &
   sPROJ4)

   class (T_DATA_GRID) :: this
   character (len=*) :: sDescription
   integer (kind=c_int) :: iDataType
   type ( T_GENERAL_GRID ),pointer :: pGrdBase
   character (len=*), optional :: sFilename
   character (len=*), optional :: sFilenameTemplate
   character (len=*), optional :: sPROJ4

   if (present(sPROJ4) ) then
     this%sSourcePROJ4_string = trim(sPROJ4)
   else
     this%sSourcePROJ4_string =  ""
   endif

   if (present(sFilename)) then
     this%sSourceFilename = sFilename
     this%iSourceDataForm = STATIC_NETCDF_GRID
     this%lGridIsPersistent = lFALSE
   else
     this%sSourceFilename = ""
   endif

   if (present(sFilenameTemplate)) then
     this%sFilenameTemplate = sFilenameTemplate
     this%sSourceFilename = ""
     this%lGridIsPersistent = lTRUE
     this%iSourceDataForm = DYNAMIC_NETCDF_GRID
   else
     this%sFilenameTemplate = ""
   endif

   call assert(.not. (len_trim(this%sSourceFilename) > 0 &
      .and. len_trim(this%sFilenameTemplate) > 0), &
      "INTERNAL PROGRAMMING ERROR - values may be assigned to either " &
      //dquote("Filename")//" or the "//dquote("sFilenameTemplate") &
      //" -- NOT BOTH", trim(__FILE__), __LINE__)

   this%sSourceFileType = "NETCDF"
   this%iSourceFileType = this%get_filetype()

   this%iTargetDataType = iDataType
   this%iNC_FILE_STATUS = NETCDF_FILE_CLOSED

   call netcdf_nullify_data_struct( NCFILE=this%NCFILE )
   call netcdf_nullify_data_struct( NCFILE=this%NCFILE_ARCHIVE )

end subroutine initialize_netcdf_data_object_sub

!----------------------------------------------------------------------

  subroutine getvalues_sub( this, pGrdBase, iMonth, iDay, iYear, iJulianDay, &
    iValues, rValues)

    class (T_DATA_GRID) :: this
    type ( T_GENERAL_GRID ), pointer :: pGrdBase
    integer (kind=c_int), intent(in), optional :: iMonth, iDay, iYear, iJulianDay
    integer (kind=c_int), dimension(:,:), optional :: iValues
    real (kind=c_float), dimension(:,:), optional :: rValues

    ! [ LOCALS ]
    integer (kind=c_int) :: iNumDaysToPad
    integer (kind=c_int) :: iPadDays
    integer (kind=c_int) :: iLocalJulianDay

    if(this%iSourceDataForm == DYNAMIC_GRID ) then

      call getvalues_gridded_sub( this, pGrdBase, iMonth, iDay, iYear)


    elseif ( this%iSourceDataForm == DYNAMIC_NETCDF_GRID ) then

      iLocalJulianDay = iJulianDay
      call getvalues_dynamic_netcdf_sub( this, &
                           pGrdBase,  iMonth, iDay, iYear, iLocalJulianDay)


    elseif(this%iSourceDataForm == STATIC_GRID ) then

      call getvalues_gridded_sub( this, pGrdBase)

    elseif(this%iSourceDataForm == CONSTANT_GRID ) then

      call getvalues_constant_sub( this, pGrdBase )

    else

      call assert(lFALSE, "Unsupported data source specified", &
        trim(__FILE__), __LINE__)

    endif

    !> Here is where the data are actually assigned to the data structure that the
    !! rest of SWB sees...
    if (present(rValues)) then

       rValues = ( pGrdBase%rData * this%rUserScaleFactor ) + this%rUserOffset

    endif

    if (present(iValues)) then

        iValues = ( pGrdBase%iData * int(this%rUserScaleFactor, kind=c_int) ) + int( this%rUserOffset, kind=c_int)
    endif

  end subroutine getvalues_sub

!----------------------------------------------------------------------

subroutine getvalues_constant_sub( this, pGrdBase )

  class (T_DATA_GRID) :: this
  type ( T_GENERAL_GRID ), pointer :: pGrdBase

  ! [ LOCALS ]
  integer (kind=c_int), save   :: call_number = 0

  this%lGridHasChanged = lFALSE

  do

    if ( call_number > 0 ) exit

    select case (this%iSourceDataType)

      case ( DATATYPE_REAL )

        if (.not. all( pGrdBase%rData == this%rConstantValue ) ) then

          this%lGridHasChanged = lTRUE
          pGrdBase%rData = this%rConstantValue

        endif

      case ( DATATYPE_INT)

        if (.not. all( pGrdBase%iData == this%iConstantValue ) ) then

          this%lGridHasChanged = lTRUE
          pGrdBase%iData = this%iConstantValue

        endif

      case default

        call dump_data_structure_sub(this)

        call assert(lFALSE, "INTERNAL PROGRAMMING ERROR - Unhandled data type: " &
          //"name="//dquote(this%sDescription) &
          //"; value="//trim(asCharacter(this%iSourceDataType)), &
          trim(__FILE__), __LINE__)

      end select

      call_number = call_number + 1
      exit

    enddo

  end subroutine getvalues_constant_sub

!----------------------------------------------------------------------

  subroutine dump_data_structure_sub(this)

  class (T_DATA_GRID) :: this

  call echolog("---------------------------------------------------")
  call echolog("DATA STRUCTURE DETAILS:")
  call echolog("---------------------------------------------------")

  call echolog("  source data form: "//trim(asCharacter(this%iSourceDataForm)) )
  call echolog("  source data type: "//trim(asCharacter(this%iSourceDataType)) )
  call echolog("  source file type: "//trim(asCharacter(this%iSourceFileType)) )
  call echolog("  description: "//trim(this%sDescription) )
  call echolog("  source PROJ4 string: "//trim(this%sSourcePROJ4_string) )
  call echolog("  source file type: "//trim(this%sSourceFileType) )
  call echolog("  filename template: "//trim(this%sFilenameTemplate) )
  call echolog("  source filename: "//trim(this%sSourceFilename) )

  if (associated(this%pGrdNative))  call grid_DumpGridExtent(this%pGrdNative)

  end subroutine dump_data_structure_sub

!----------------------------------------------------------------------

  subroutine getvalues_gridded_sub( this, pGrdBase, iMonth, iDay, iYear)

    class (T_DATA_GRID) :: this
    type ( T_GENERAL_GRID ), pointer :: pGrdBase
    integer (kind=c_int), optional :: iMonth
    integer (kind=c_int), optional :: iDay
    integer (kind=c_int), optional :: iYear
    logical (kind=c_bool) :: lExist
    logical (kind=c_bool) :: lOpened

    this%lGridHasChanged = lFALSE

    do

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

      ! if the source filename hasn't changed, we don't need to be here
      if (str_compare(this%sOldFilename, this%sSourceFilename) ) exit

      this%sOldFilename = this%sSourceFilename

      inquire(file=this%sSourceFilename, exist=lExist, opened=lOpened)
      ! if the file doesn't exist, EXIT
      if (.not. lExist ) then
        if ( this%lMissingFilesAreAllowed ) then
         exit
        else
          call assert( lFALSE, &
            "Could not find input data file~filename:"//dquote(this%sSourceFilename) &
            //"~data description: "//trim(this%sDescription))
        endif
      endif

      write( unit=LU_LOG, fmt="(/,1x,a)") "Opening file "//dQuote(this%sSourceFilename) &
        //" for "//trim(this%sDescription)//" data."

      if ( this%lGridIsPersistent .and. associated(this%pGrdNative) ) then

        call grid_ReadExisting ( sFileName=this%sSourceFilename, &
          sFileType=this%sSourceFileType, &
          pGrd=this%pGrdNative )

      else

        ! create a grid in native coordinates of the source dataset.
        this%pGrdNative => grid_Read( sFileName=this%sSourceFilename, &
          sFileType=this%sSourceFileType, &
          iDataType=this%iSourceDataType )

        ! ensure that PROJ4 string is associated with the native grid
        this%pGrdNative%sPROJ4_string = this%sSourcePROJ4_string

      endif

      this%lGridHasChanged = lTRUE

      select case (this%iTargetDataType)

        case ( GRID_DATATYPE_REAL )

          call this%create_missing_values_mask( rValues = this%pGrdNative%rData )
          call this%apply_scale_and_offset( rValues = this%pGrdNative%rData )
          call this%enforce_limits( rValues = this%pGrdNative%rData )

        case ( GRID_DATATYPE_INT )

!          call this%handle_missing_values(this%pGrdNative%iData)
!          call this%enforce_limits(this%pGrdNative%iData)

        case default

          call assert(lFALSE, "INTERNAL PROGRAMMING ERROR - Unhandled data type: value=" &
            //trim(asCharacter(this%iSourceDataType)), &
            trim(__FILE__), __LINE__)

      end select

      ! Now transfer data from the NATIVE gridspace to the PROJECT (BASE) gridspace
      call this%transfer_from_native( pGrdBase )

      if (.not. this%lGridIsPersistent )  call grid_Destroy(this%pGrdNative)

      exit

    enddo

  end subroutine getvalues_gridded_sub

!----------------------------------------------------------------------

subroutine transform_grid_to_grid(this, pGrdBase)

    class (T_DATA_GRID) :: this
    type ( T_GENERAL_GRID ), pointer :: pGrdBase

    if( len_trim( this%sSourcePROJ4_string ) > 0 ) then

      ! only invoke the transform procedure if the PROJ4 strings are different
      if (.not. str_compare(this%pGrdNative%sPROJ4_string, pGrdBase%sPROJ4_string)) then

        call echolog(" ")
        call echolog("Transforming gridded data in file: "//dquote(this%sSourceFilename) )
        call echolog("  FROM: "//squote(this%sSourcePROJ4_string) )
        call echolog("  TO:   "//squote(pGrdBase%sPROJ4_string) )

        call grid_Transform(pGrd=this%pGrdNative, &
                          sFromPROJ4=this%sSourcePROJ4_string, &
                          sToPROJ4=pGrdBase%sPROJ4_string )

      endif

      call Assert( grid_CompletelyCover( pGrdBase, this%pGrdNative ), &
        "Transformed grid read from file "//dquote(this%sSourceFilename) &
        //" doesn't completely cover your model domain.")

      select case (this%iTargetDataType)

        case ( GRID_DATATYPE_REAL )

          call grid_gridToGrid(pGrdFrom=this%pGrdNative,&
                              rArrayFrom=this%pGrdNative%rData, &
                              pGrdTo=pGrdBase, &
                              rArrayTo=pGrdBase%rData )

        case ( GRID_DATATYPE_INT )

          call grid_gridToGrid(pGrdFrom=this%pGrdNative,                 &
                            iArrayFrom=this%pGrdNative%iData,            &
                            pGrdTo=pGrdBase,                             &
                            iArrayTo=pGrdBase%iData,                     &
                            iFilterType=this%iMajorityFilterType )
        case default

          call assert(lFALSE, "INTERNAL PROGRAMMING ERROR - Unhandled data type: value=" &
            //trim(asCharacter(this%iSourceDataType)), &
            trim(__FILE__), __LINE__)

      end select

    else

      call Assert( grid_Conform( pGrdBase, this%pGrdNative ), &
                        "Non-conforming grid. Filename: " &
                        //dquote(this%pGrdNative%sFilename) )

      select case (this%iTargetDataType)

        case ( GRID_DATATYPE_REAL )

          pGrdBase%rData = this%pGrdNative%rData

        case ( GRID_DATATYPE_INT)

          pGrdBase%iData = this%pGrdNative%iData

        case default

          call assert(lFALSE, "INTERNAL PROGRAMMING ERROR - Unhandled data type: value=" &
              //trim(asCharacter(this%iSourceDataType)), &
            trim(__FILE__), __LINE__)

      end select

    endif

end subroutine transform_grid_to_grid

!----------------------------------------------------------------------

subroutine set_constant_value_int( this, iValue )

    class (T_DATA_GRID) :: this
    integer (kind=c_int) :: iValue

    this%iConstantValue = iValue

end subroutine set_constant_value_int

!----------------------------------------------------------------------

subroutine set_constant_value_real( this, rValue )

    class (T_DATA_GRID) :: this
    real (kind=c_float) :: rValue

    this%rConstantValue = rValue

end subroutine set_constant_value_real

!----------------------------------------------------------------------

  subroutine set_filecount( this, iValue, iYear)

    class (T_DATA_GRID) :: this
    integer (kind=c_int) :: iValue
    integer (kind=c_int), optional :: iYear

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
    integer (kind=c_int) :: iYear

    if (iYear /= this%iFileCountYear )  then
      this%iFileCount = 0
      this%iFileCountYear = iYear
    endif

  end subroutine reset_at_yearend_filecount

!----------------------------------------------------------------------

  subroutine make_filename_from_template( this, iMonth, iDay, iYear )

    class (T_DATA_GRID) :: this
    integer (kind=c_int), optional :: iMonth
    integer (kind=c_int), optional :: iDay
    integer (kind=c_int), optional :: iYear

    ! [ LOCALS ]
    character (len=256) :: sNewFilename
    character (len=256) :: sUppercaseFilename
    character (len=256) :: sCWD
    character (len=256) :: sBuf2
    integer (kind=c_int) :: iPos_Y, iPos_D, iPos_M, iPos_0D, iPos_0M, iPos_B, iPos_BF, iPos, iPos2, iLen, iCount
    integer (kind=c_int) :: iNumZeros, iNumZerosToPrint
    logical (kind=c_bool) :: lMatch
    logical (kind=c_bool) :: lExist
    character (len=16) :: sBuf
    character (len=12) :: sNumber
    character (len=1) :: sDelimiter
    integer (kind=c_int) :: iStatus

    iPos_Y = 0; iPos_M = 0; iPos_D = 0; iPos = 0; iPos_B = 0; iPos_BF = 0; sNumber = ""

    ! EXAMPLES of the kinds of templates that we need to be able to understand:
    ! tars1980\prcp.nc   template => "tars%Y\prcp.nc"
    ! prcp_1980_00.nc    template => "prcp_%Y_%m.nc"

    iStatus = getcwd(sCWD )

    call assert(iStatus==0, "Problem detemining what the current working" &
      //" directory is", trim(__FILE__), __LINE__)

    sNewFilename = this%sFilenameTemplate

    iCount = 0

    do

      lMatch = lFALSE

      if (present(iYear) ) then

        iPos_Y = max(index(sNewFilename, "%Y"), index(sNewFilename, "%y") )

        if (iPos_Y > 0) then
          lMatch = lTRUE
          iLen=len_trim(sNewFilename)
          sNewFilename = sNewFilename(1:iPos_Y - 1)//trim(asCharacter(iYear)) &
                         //sNewFilename(iPos_Y + 2:iLen)

        endif

      endif

      ! evaluate template string for "#" characters
      iPos = index(sNewFilename, "#")

      if (iPos > 0) then

        ! example:  %000#
        ! trying to determine how many zero values have been inserted between % and # characters
        iPos2 = index(sNewFilename(1:iPos),"%", BACK=lTRUE)
        sBuf2 = trim(asCharacter(this%iFileCount))
        iNumZeros = max(0, iPos - iPos2 - 1)

        if (iNumZeros > 0) then
          iNumZerosToPrint = max(0,iNumZeros - len_trim(sBuf2) + 1)
          sNumber = repeat("0", iNumZerosToPrint )//trim(sBuf2)
        else
          sNumber = trim(sBuf2)
        endif

        lMatch = lTRUE
        iLen=len_trim(sNewFilename)
        sNewFilename = sNewFilename(1:iPos-2-iNumZeros)//trim(sNumber) &
                       //sNewFilename(iPos+1:iLen)
      endif


      ! evaluate template string for "%m": month number
      if (present(iMonth) ) then

        iPos_M = index(sNewFilename, "%m")
        iPos_0M = index(sNewFilename, "%0m")
        iPos_B = index(sNewFilename, "%b")
        iPos_BF = index(sNewFilename, "%B")

        if ( iPos_0M > 0 ) then

          lMatch = lTRUE
          write (unit=sBuf, fmt="(i2.2)") iMonth

          iLen=len_trim(sNewFilename)
          sNewFilename = sNewFilename(1:iPos_0M - 1)//trim(sBuf) &
                         //sNewFilename(iPos_0M + 3:iLen)

        elseif ( iPos_M > 0 ) then

          lMatch = lTRUE
          sBuf = asCharacter( iMonth )

          iLen=len_trim(sNewFilename)
          sNewFilename = sNewFilename(1:iPos_M - 1)//trim(sBuf) &
                         //sNewFilename(iPos_M + 2:iLen)

        elseif ( iPos_B > 0 ) then

          lMatch = lTRUE

          select case ( this% iFilename_Monthname_Capitalization_Rule )

            case ( FILE_TEMPLATE_UPPERCASE_MONTHNAME )

              sBuf = YEAR_INFO( iMonth )%sName
              call uppercase( sBuf )

            case ( FILE_TEMPLATE_LOWERCASE_MONTHNAME )

              sBuf = YEAR_INFO( iMonth )%sName
              call lowercase ( sBuf )

            case default

              sBuf = YEAR_INFO( iMonth )%sName

          end select

          iLen=len_trim(sNewFilename)
          sNewFilename = sNewFilename(1:iPos_B - 1)//trim(sBuf) &
                         //sNewFilename(iPos_B + 2:iLen)

        elseif ( iPos_BF > 0 ) then

          lMatch = lTRUE

          select case ( this%iFilename_Monthname_Capitalization_Rule )

            case ( FILE_TEMPLATE_UPPERCASE_MONTHNAME )

              sBuf = YEAR_INFO( iMonth )%sFullName
              call uppercase( sBuf )

            case ( FILE_TEMPLATE_LOWERCASE_MONTHNAME )

              sBuf = YEAR_INFO( iMonth )%sFullName
              call lowercase( sBuf )

            case default

              sBuf = YEAR_INFO( iMonth )%sFullName

          end select

          iLen=len_trim(sNewFilename)
          sNewFilename = sNewFilename(1:iPos_BF - 1)//trim(sBuf) &
                         //sNewFilename( ( iPos_BF + len_trim(sBuf) - 1):iLen)

        endif

      endif

      ! evaluate template string for "%d": day number
      if (present(iDay) )  then

        iPos_D = max(index(sNewFilename, "%D"),index(sNewFilename, "%d") )
        iPos_0D = max(index(sNewFilename, "%0D"), index(sNewFilename, "%0d") )

        if (iPos_0D > 0) then
          lMatch = lTRUE
          write (unit=sBuf, fmt="(i2.2)") iDay
          iLen=len_trim(sNewFilename)
          sNewFilename = sNewFilename(1:iPos_0D - 1)//trim(sBuf) &
                         //sNewFilename(iPos_0D + 3:iLen)

        elseif ( iPos_D > 0 ) then

          lMatch = lTRUE
          sBuf = asCharacter( iDay )

          iLen=len_trim(sNewFilename)
          sNewFilename = sNewFilename(1:iPos_D - 1)//trim(sBuf) &
                         //sNewFilename(iPos_D + 2:iLen)

        endif

      endif

      if (.not. lMatch) exit

      iCount = iCount + 1

      ! failsafe
      if (iCount > 4) exit

    enddo

    if( index(string=sCWD, substring=sFORWARDSLASH) > 0 ) then
      sDelimiter = sFORWARDSLASH
    else
      sDelimiter = sBACKSLASH
    endif

!    this%sSourceFilename = trim(sCWD)//trim(sDelimiter)//trim(sNewFilename)
    this%sSourceFilename = trim(sNewFilename)

  end subroutine make_filename_from_template

!----------------------------------------------------------------------

  function test_for_need_to_pad_values(this, iMonth, iDay, iYear ) &
                                            result(lNeedToPadData)

    class (T_DATA_GRID) :: this
    integer (kind=c_int) :: iMonth, iDay, iYear

    ! [ LOCALS ]
    logical (kind=c_bool) :: lExist
    integer (kind=c_int) :: iDaysLeftInMonth
    integer (kind=c_int) :: iPos
    logical (kind=c_bool) :: lNeedToPadData

    do

      lNeedToPadData = lFALSE

      iPos = scan(string=trim(this%sSourceFilename), set="http://")

      ! if this is a URL, we don't want to test for file existence using
      ! the Fortran "inquire" function
      if (this%sSourceFilename(iPos:iPos+6) == "http://") then

        exit

      else

        ! does this file actually exist?
        inquire( file=this%sSourceFilename, exist=lExist )

        ! if the file exists, don't bother with padding any values
        if (lExist) exit

        ! if file doesn't exist, and we're close to the end of the year,
        ! assume that we should pad values at the end of the year
        if (iMonth == 12 ) then

          iDaysLeftInMonth = 31 - iDay

          if (isLeap(iYear)) then

            if ( iDaysLeftInMonth <= this%iDaysToPadIfLeapYear ) then

              lNeedToPadData = lTRUE
              exit

            endif

          else    ! it's not leap year

            if ( iDaysLeftInMonth <= this%iDaysToPadAtYearsEnd ) then

              lNeedToPadData = lTRUE
              exit

            endif

          endif

        endif

        ! if we've reached this point, we cannot locate the proper file and
        ! we are not within the proper range of dates to allow for padding.
        call assert(lExist, "The filename created from your template refers to " &
          //"a nonexistent file. ~ Attempted to open filename "&
          //dquote(this%sSourceFilename), trim(__FILE__), __LINE__)

        exit

      endif

    enddo


  end function test_for_need_to_pad_values

!----------------------------------------------------------------------

  subroutine getvalues_dynamic_netcdf_sub( this, pGrdBase, &
     iMonth, iDay, iYear, iJulianDay)

    class (T_DATA_GRID) :: this
    type ( T_GENERAL_GRID ), pointer :: pGrdBase
    integer (kind=c_int) :: iMonth, iDay, iYear, iJulianDay

    ! [ LOCALS ]
    integer (kind=c_int) :: iTimeIndex
    integer (kind=c_int) :: iStat
    logical (kind=c_bool) :: lDateTimeFound

    this%lPadValues = lFALSE

    ! call once at start of run...
    if ( this%iFileCountYear < 0 ) call this%set_filecount(-1, iYear)

    if (this%iNC_FILE_STATUS == NETCDF_FILE_OPEN) then

      ! check to see whether currently opened file is within date range
      ! if past date range, close file

      if ( .not. netcdf_date_within_range(NCFILE=this%NCFILE, iJulianDay=iJulianDay ) ) then
        call netcdf_close_file( NCFILE=this%NCFILE )
        this%iNC_FILE_STATUS = NETCDF_FILE_CLOSED
      endif

    endif

    if ( this%iNC_FILE_STATUS == NETCDF_FILE_CLOSED ) then

      ! increment or reset file counter based on current year value
      call this%increment_filecount()

      ! the numerical counter used in creating filenames is reset at the end of each year
      call this%reset_at_yearend_filecount(iYear)

      ! based on the template information, create the filename that SWB
      ! is to look for
      call this%make_filename( iMonth=iMonth, iYear=iYear, iDay=iDay)

      this%lPadValues = this%test_for_need_to_pad_values(iMonth=iMonth, iYear=iYear, iDay=iDay)

      ! call to test_for_need_to_pad_values return value of "TRUE" if
      ! if attempts to open a nonexistent file within the last few days of a year.
      ! The assumption is that values missing at the end of a calendar year
      ! translates into a missing file at the year's end

      if (.not. this%lPadValues) then

        if (this%lPerformFullInitialization ) then

          if( len_trim( this%sSourcePROJ4_string ) > 0 ) then

            ! calculate the project boundaries in the coordinate system of
            ! the native data file
            call this%calc_project_boundaries(pGrdBase=pGrdBase)

            call netcdf_open_and_prepare_as_input(NCFILE=this%NCFILE, &
              sFilename=this%sSourceFilename, &
              lFlipHorizontal=this%lFlipHorizontal, &
              lFlipVertical=this%lFlipVertical, &
              rX_Coord_AddOffset = this%rX_Coord_AddOffset, &
              rY_Coord_AddOffset = this%rY_Coord_AddOffset, &
              sVariableOrder=this%sVariableOrder, &
              sVarName_x=this%sVariableName_x, &
              sVarName_y=this%sVariableName_y, &
              sVarName_z=this%sVariableName_z, &
              sVarName_time=this%sVariableName_time, &
              tGridBounds=this%GRID_BOUNDS_NATIVE, &
              iLU=LU_LOG)

          else  ! PROJ4 string is blank

            ! assume source NetCDF file is in same projection and
            ! of same dimensions as base grid
            call netcdf_open_and_prepare_as_input(NCFILE=this%NCFILE, &
              sFilename=this%sSourceFilename, &
              lFlipHorizontal=this%lFlipHorizontal, &
              lFlipVertical=this%lFlipVertical, &
              rX_Coord_AddOffset = this%rX_Coord_AddOffset, &
              rY_Coord_AddOffset = this%rY_Coord_AddOffset, &
              sVariableOrder=this%sVariableOrder, &
              sVarName_x=this%sVariableName_x, &
              sVarName_y=this%sVariableName_y, &
              sVarName_z=this%sVariableName_z, &
              sVarName_time=this%sVariableName_time, &
              iLU=LU_LOG)

            this%NCFILE%iNX = pGrdBase%iNX
            this%NCFILE%iNY = pGrdBase%iNY
            this%NCFILE%rX(NC_LEFT) = pGrdBase%rX0
            this%NCFILE%rY(NC_BOTTOM) = pGrdBase%rY0
            this%NCFILE%rX(NC_RIGHT) = pGrdBase%rX1
            this%NCFILE%rY(NC_TOP) = pGrdBase%rY1

          endif

          this%iNC_FILE_STATUS = NETCDF_FILE_OPEN

          this%iSourceDataType = this%NCFILE%iVarType(NC_Z)

          ! populate these values with the scale and offset
          ! factor included in the NetCDF attribute data, if any.
          this%rAddOffset = this%NCFILE%rAddOffset(NC_Z)
          this%rScaleFactor = this%NCFILE%rScaleFactor(NC_Z)

          ! Amongst other things, the call to netcdf_open_and_prepare
          ! finds the nearest column and row that correspond to the
          ! project bounds, then back-calculates the coordinate values
          ! of the column and row numbers in the *NATIVE* coordinate system
          this%pGrdNative => grid_CreateComplete ( iNX=this%NCFILE%iNX, &
                    iNY=this%NCFILE%iNY, &
                    rX0=this%NCFILE%rX(NC_LEFT), &
                    rY0=this%NCFILE%rY(NC_BOTTOM), &
                    rX1=this%NCFILE%rX(NC_RIGHT), &
                    rY1=this%NCFILE%rY(NC_TOP), &
                    iDataType=this%iTargetDataType )

          if( len_trim( this%sSourcePROJ4_string ) > 0 ) then
            ! ensure that PROJ4 string is associated with the native grid
            this%pGrdNative%sPROJ4_string = this%sSourcePROJ4_string
          endif

          this%pGrdNative%sFilename = this%sSourceFilename

          ! we don't need to perform all these steps for the next file; we are
          ! assuming, of course, that all of the subsequent files cover the same
          ! extents and are in the same projection as this first file
          this%lPerformFullInitialization = lFALSE

        else
          ! Projection settings can be left alone; read values from new
          ! NetCDF file with same grid boundaries, projection, etc.

!          call netcdf_open_file(NCFILE=this%NCFILE, sFilename=this%sSourceFilename, iLU=LU_LOG)
          call netcdf_open_file(NCFILE=this%NCFILE, sFilename=this%sSourceFilename)

          this%iNC_FILE_STATUS = NETCDF_FILE_OPEN

        endif

      endif   ! if(lPadValues)

    endif  ! If (NC_FILE_STATUS == NETCDF_CLOSED)

    if ( .not. this%lPadValues ) then

      do
        lDateTimeFound = netcdf_update_time_starting_index(NCFILE=this%NCFILE, &
                                         iJulianDay=iJulianDay)

        if (.not. lDateTimeFound) then
          this%lPadValues = lTRUE
          exit
        endif

        call netcdf_get_variable_slice(NCFILE=this%NCFILE, rValues=this%pGrdNative%rData)

        ! the missing_values code stored in the NetCDF file needs to be processed *before* applying
        ! the scale and offset
        call this%create_missing_values_mask( rValues = this%pGrdNative%rData )
        call this%apply_scale_and_offset( rValues = this%pGrdNative%rData )
        call this%enforce_limits( rValues = this%pGrdNative%rData )
        exit
      enddo

    endif

    if ( this%lPadValues ) then

      if (this%lPadReplaceWithZero) then

        this%pGrdNative%rData = 0_c_float
        this%pGrdNative%iData = 0_c_int

      endif

      call echolog( repeat("=", 60) )
      call echolog( "Missing day found in NetCDF file - padding values" )
      call echolog( repeat("=", 60) )

    endif

    if (this%lCreateLocalNetCDFArchive) &
             call this%put_values_to_archive(iMonth, iDay, iYear)

    call this%transfer_from_native( pGrdBase )

  end subroutine getvalues_dynamic_netcdf_sub

!----------------------------------------------------------------------

  subroutine put_values_to_local_NetCDF_sub(this, iMonth, iDay, iYear)

    class (T_DATA_GRID) :: this
    integer (kind=c_int) :: iMonth
    integer (kind=c_int) :: iDay
    integer (kind=c_int) :: iYear

    ! [ LOCALS ]
    integer (kind=c_size_t) :: iNumRows, iNumCols, iNumRecs
    type (T_GENERAL_GRID), pointer  :: pTempGrd

    if (this%iNC_ARCHIVE_STATUS == NETCDF_FILE_CLOSED) then

  !> @todo finish the logic in this section: really want to output X(:,:) and Y(:,:)
  !!       as grid of latitude and longitude so that external NetCDF viewers can
  !!       plot the archived data correctly

!       if ( len_trim(this%sPROJ4_string ) > 0 ) then

!           pTempGrd => grid_CreateComplete ( iNX=this%NCFILE%iNX, &
!                     iNY=this%NCFILE%iNY, &
!                     rX0=this%NCFILE%rX(NC_LEFT), &
!                     rY0=this%NCFILE%rY(NC_BOTTOM), &
!                     rX1=this%NCFILE%rX(NC_RIGHT), &
!                     rY1=this%NCFILE%rY(NC_TOP), &
!                     iDataType=this%iTargetDataType )

!           call grid_Transform(pGrd=pTempGrd, sFromPROJ4=thi%sPROJ4_string, sToPROJ4="+proj=lonlat +ellps=WGS84 +datum=WGS84 +no_defs" )

!           call netcdf_open_and_prepare_as_output(NCFILE=this%NCFILE, &
!                    NCFILE_ARCHIVE=this%NCFILE_ARCHIVE, &
!                    iOriginMonth=iMonth, iOriginDay=iDay, iOriginYear=iYear, &
!                    iStartYear=this%iStartYear, iEndYear=this%iEndYear, &
!                    rX=pTempGrd%rX, rY=pTempGrd%rY )

!       else

          call netcdf_open_and_prepare_as_output(NCFILE=this%NCFILE, &
                   NCFILE_ARCHIVE=this%NCFILE_ARCHIVE, &
                   iOriginMonth=iMonth, iOriginDay=iDay, iOriginYear=iYear, &
                   iStartYear=this%iStartYear, iEndYear=this%iEndYear)

!       endif

      this%iNC_ARCHIVE_STATUS = NETCDF_FILE_OPEN

    endif

    iNumRows = int(size(this%pGrdNative%rData, 2), kind=c_size_t)
    iNumCols = int(size(this%pGrdNative%rData, 1), kind=c_size_t)
    iNumRecs = this%iNCFILE_RECNUM

    ! write out value array to NetCDF
    call netcdf_put_variable_array(NCFILE=this%NCFILE_ARCHIVE, &
       iVarID=this%NCFILE_ARCHIVE%iVarID(NC_Z), &
       iStart=[iNumRecs, 0_c_size_t, 0_c_size_t], &
       iCount=[1_c_size_t, iNumRows, iNumCols], &
       iStride=[1_c_ptrdiff_t,1_c_ptrdiff_t,1_c_ptrdiff_t], &
       rValues=this%pGrdNative%rData)

    ! write out time value
    call netcdf_put_variable_vector(NCFILE=this%NCFILE_ARCHIVE, &
       iVarID=this%NCFILE_ARCHIVE%iVarID(NC_TIME), &
       iStart=[this%iNCFILE_RECNUM], &
       iCount=[1_c_size_t], &
       iStride=[1_c_size_t], &
       dpValues=[real(this%iNCFILE_RECNUM , kind=c_double)])

    this%iNCFILE_RECNUM = this%iNCFILE_RECNUM + 1

  end subroutine put_values_to_local_NetCDF_sub

!----------------------------------------------------------------------

  function get_source_filetype_fn(this)  result(iFileType)

     class (T_DATA_GRID) :: this
     integer (kind=c_int) :: iFileType

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

  subroutine set_PROJ4_string_sub(this, sPROJ4_string)

     class (T_DATA_GRID) :: this
     character (len=*), optional :: sPROJ4_string

     this%sSourcePROJ4_string = sPROJ4_string

  end subroutine set_PROJ4_string_sub

!----------------------------------------------------------------------

  subroutine set_grid_flip_horizontal_sub(this)

    class (T_DATA_GRID) :: this

    this%lFlipHorizontal = lTRUE

  end subroutine set_grid_flip_horizontal_sub

!----------------------------------------------------------------------

  subroutine set_grid_flip_vertical_sub(this)

    class (T_DATA_GRID) :: this

    this%lFlipVertical = lTRUE

  end subroutine set_grid_flip_vertical_sub

!----------------------------------------------------------------------

  subroutine set_variable_order_sub(this, sVariableOrder)

    class (T_DATA_GRID) :: this
    character (len=*) :: sVariableOrder

    this%sVariableOrder = sVariableOrder

  end subroutine set_variable_order_sub

!----------------------------------------------------------------------

subroutine set_archive_local_sub(this, lValue)

   class (T_DATA_GRID) :: this
   logical (kind=c_bool) :: lValue

   this%lCreateLocalNetCDFArchive = lValue

end subroutine set_archive_local_sub

!----------------------------------------------------------------------

subroutine set_X_coord_offset_sub(this, rXOffset)

   class (T_DATA_GRID) :: this
   real (kind=c_double) :: rXOffset

   this%rX_Coord_AddOffset = rXOffset

end subroutine set_X_coord_offset_sub

!----------------------------------------------------------------------

subroutine set_Y_coord_offset_sub(this, rYOffset)

   class (T_DATA_GRID) :: this
   real (kind=c_double) :: rYOffset

   this%rY_Coord_AddOffset = rYOffset

end subroutine set_Y_coord_offset_sub

subroutine set_user_scale_sub(this, rUserScaleFactor)

   class (T_DATA_GRID) :: this
   real (kind=c_float) :: rUserScaleFactor

   this%rUserScaleFactor = rUserScaleFactor

end subroutine set_user_scale_sub


subroutine set_user_offset_sub(this, rUserOffset)

   class (T_DATA_GRID) :: this
   real (kind=c_float) :: rUserOffset

   this%rUserOffset = rUserOffset

end subroutine set_user_offset_sub

!----------------------------------------------------------------------

subroutine set_missing_value_int_sub(this, iMissingVal)

  class (T_DATA_GRID) :: this
  integer (kind=c_int) :: iMissingVal

  this%iMissingValuesCode = iMissingVal

end subroutine set_missing_value_int_sub

!----------------------------------------------------------------------

subroutine set_missing_value_real_sub(this, rMissingVal)

  class (T_DATA_GRID) :: this
  integer (kind=c_int) :: rMissingVal

  this%rMissingValuesCode = rMissingVal

end subroutine set_missing_value_real_sub

!----------------------------------------------------------------------

subroutine set_minimum_allowable_value_int_sub(this, iMinVal)

  class (T_DATA_GRID) :: this
  integer (kind=c_int) :: iMinVal

  this%iMinAllowedValue = iMinVal

end subroutine set_minimum_allowable_value_int_sub

!----------------------------------------------------------------------

subroutine set_maximum_allowable_value_int_sub(this, iMaxVal)

  class (T_DATA_GRID) :: this
  integer (kind=c_int) :: iMaxVal

  this%iMaxAllowedValue = iMaxVal

end subroutine set_maximum_allowable_value_int_sub

!----------------------------------------------------------------------

subroutine set_minimum_allowable_value_real_sub(this, rMinVal)

  class (T_DATA_GRID) :: this
  real (kind=c_float) :: rMinVal

  this%rMinAllowedValue = rMinVal

end subroutine set_minimum_allowable_value_real_sub

!----------------------------------------------------------------------

subroutine set_maximum_allowable_value_real_sub(this, rMaxVal)

  class (T_DATA_GRID) :: this
  real (kind=c_float) :: rMaxVal

  this%rMaxAllowedValue = rMaxVal

end subroutine set_maximum_allowable_value_real_sub

!----------------------------------------------------------------------

  subroutine calc_project_boundaries(this, pGrdBase)

    class (T_DATA_GRID) :: this
    type ( T_GENERAL_GRID ), pointer :: pGrdBase

    ! [ LOCALS ]
    integer (kind=c_int) :: iRetVal
    real (kind=c_float) :: rMultiplier = 1.
    real (kind=c_double), dimension(4) :: rX, rY

    ! ensure that there is sufficient coverage on all sides of grid
    rX(1) = pGrdBase%rX0 - pGrdBase%rGridCellSize * rMultiplier ! Xll
    rY(1) = pGrdBase%rY0 - pGrdBase%rGridCellSize * rMultiplier ! Yll
    rX(2) = pGrdBase%rX1 + pGrdBase%rGridCellSize * rMultiplier ! Xlr
    rY(2) = pGrdBase%rY0 - pGrdBase%rGridCellSize * rMultiplier ! Ylr
    rX(3) = pGrdBase%rX0 - pGrdBase%rGridCellSize * rMultiplier ! Xul
    rY(3) = pGrdBase%rY1 + pGrdBase%rGridCellSize * rMultiplier ! Yul
    rX(4) = pGrdBase%rX1 + pGrdBase%rGridCellSize * rMultiplier ! Xur
    rY(4) = pGrdBase%rY1 + pGrdBase%rGridCellSize * rMultiplier ! Yur

    ! now transform the project coordinates to native coordinates so we can
    ! use the native coordinate boundaries to "cookie-cut" only the data
    ! pertinent to our project area.
    iRetVal = pj_init_and_transform(trim(pGrdBase%sPROJ4_string)//C_NULL_CHAR, &
                trim(this%sSourcePROJ4_string)//C_NULL_CHAR, 4_c_long, &
                rX, rY )

  call grid_CheckForPROJ4Error(iRetVal=iRetVal, &
     sFromPROJ4=trim(pGrdBase%sPROJ4_string), &
     sToPROJ4=trim(this%sSourcePROJ4_string))

  ! because PROJ4 works in RADIANS if data are unprojected (i.e. GEOGRAPHIC),
  ! we need to convert back to degrees on the assumption that the coordinates
  ! referenced in the file will also be i degrees
  if(    ( index(string=trim(this%sSourcePROJ4_string), substring="latlon") > 0 )    &
    .or. ( index(string=trim(this%sSourcePROJ4_string), substring="lonlat") > 0 )    &
    .or. ( index(string=trim(this%sSourcePROJ4_string), substring="longlat") > 0 ) )    then

    rX = rad2deg(rX)
    rY = rad2deg(rY)

  endif

   this%GRID_BOUNDS_NATIVE%rXll = rX(1); this%GRID_BOUNDS_NATIVE%rXlr = rX(2)
   this%GRID_BOUNDS_NATIVE%rYll = rY(1); this%GRID_BOUNDS_NATIVE%rYlr = rY(2)
   this%GRID_BOUNDS_NATIVE%rXul = rX(3); this%GRID_BOUNDS_NATIVE%rXur = rX(4)
   this%GRID_BOUNDS_NATIVE%rYul = rY(3); this%GRID_BOUNDS_NATIVE%rYur = rY(4)

#ifdef DEBUG_PRINT
   print *, " "
   print *, __FILE__, ": ", __LINE__
   print *, "--  BASE GRID BOUNDS projected to DATA NATIVE COORDS"
   print *, "FROM: ", dquote(pGrdBase%sPROJ4_string)
   print *, "TO:   ", dquote(this%sSourcePROJ4_string)
   PRINT *, "file: ", dquote(this%sSourceFileName)
   print *, "            X                            Y"
   print *, "LL: ", this%GRID_BOUNDS_NATIVE%rXll, this%GRID_BOUNDS_NATIVE%rYll
   print *, "LR: ", this%GRID_BOUNDS_NATIVE%rXlr, this%GRID_BOUNDS_NATIVE%rYlr
   print *, "UL: ", this%GRID_BOUNDS_NATIVE%rXul, this%GRID_BOUNDS_NATIVE%rYul
   print *, "UR: ", this%GRID_BOUNDS_NATIVE%rXur, this%GRID_BOUNDS_NATIVE%rYur
#endif

  end subroutine calc_project_boundaries

!----------------------------------------------------------------------

  subroutine data_GridEnforceLimits_int(this, iValues)

    class (T_DATA_GRID) :: this
    integer (kind=c_int), dimension(:,:) :: iValues

    ! [ LOCALS ]
    integer (kind=c_int) :: iMin, iMax

    iMin = this%iMinAllowedValue
    iMax = this%iMaxAllowedValue

    where ( iValues < iMin )  iValues = iMin
    where ( iValues > iMax )  iValues = iMax

  end subroutine data_GridEnforceLimits_int

!----------------------------------------------------------------------

  subroutine data_GridEnforceLimits_real(this, rValues)

    class (T_DATA_GRID)                 :: this
    real (kind=c_float), intent(inout)  :: rValues(:,:)

    ! [ LOCALS ]
    real (kind=c_float) :: rMin, rMax

    rMin = real(this%rMinAllowedValue, kind=c_float)
    rMax = real(this%rMaxAllowedValue, kind=c_float)

    associate ( values => rValues )

      where ( values < rMin )   values = rMin
      where ( values > rMax )   values = rMax

    end associate

  end subroutine data_GridEnforceLimits_real

!----------------------------------------------------------------------

  subroutine data_GridApplyScaleAndOffset_real(this, rValues)

    class (T_DATA_GRID)                   :: this
    real (kind=c_float), intent(inout)    :: rValues(:,:)

    ! [ LOCALS ]
    real (kind=c_float)  :: rMean, rSum
    integer (kind=c_int) :: iCount

    associate ( values => rValues, valid_data => this%pGrdNative%lMask,   &
                scale_factor => this%rScaleFactor, add_offset => this%rAddOffset )

      select case (this%iMissingValuesAction)

        case (MISSING_VALUES_ZERO_OUT)

          where ( valid_data )

            values = values * scale_factor + add_offset

          elsewhere

            values = rZERO

         endwhere

        case (MISSING_VALUES_REPLACE_WITH_MEAN)

          rMean = rZERO
          rSum = sum( values, valid_data )
          iCount = count( valid_data )

          if ( iCount > 0 )  rMean = rSum / real( iCount, kind=c_float) * scale_factor + add_offset

          where ( valid_data )

            values = values * scale_factor + add_offset

          elsewhere

            values = rMean

         endwhere

        case default

          call assert( lFALSE, "Unhandled case select", __FILE__, __LINE__ )

      end select

    end associate

  end subroutine data_GridApplyScaleAndOffset_real

!--------------------------------------------------------------------------------------------------

  subroutine data_GridApplyScaleAndOffset_int(this, iValues)

    class (T_DATA_GRID)                    :: this
    integer (kind=c_int), intent(inout)    :: iValues(:,:)

    ! [ LOCALS ]
    integer (kind=c_int) :: iMode

    associate ( values => iValues, valid_data => this%pGrdNative%lMask,   &
                scale_factor => this%rScaleFactor, add_offset => this%rAddOffset )

      select case (this%iMissingValuesAction)

        case (MISSING_VALUES_ZERO_OUT)

          where ( valid_data )

            values = real( values, kind=c_float ) * scale_factor + add_offset

          elsewhere

            values = iZERO

         endwhere

        case (MISSING_VALUES_REPLACE_WITH_MEAN)

          iMode =  grid__most_common_int( iValues=values, iFloor=0, iCeiling=256, lMask=valid_data )

          where ( valid_data )

            values = real( values, kind=c_float ) * scale_factor + add_offset

          elsewhere

            values = iMode

         endwhere

        case default

          call assert( lFALSE, "Unhandled case select", __FILE__, __LINE__ )

      end select

    end associate

  end subroutine data_GridApplyScaleAndOffset_int

!--------------------------------------------------------------------------------------------------

  subroutine data_CreateMissingValuesMask_real(this, rValues)

    class (T_DATA_GRID)               :: this
    real (kind=c_float), intent(inout)   :: rValues(:,:)

    ! [ LOCALS ]
    real (kind=c_float)  :: rMissing

    integer :: i, j

    rMissing = real(this%rMissingValuesCode, kind=c_float)

    associate ( values => rValues, mask => this%pGrdNative%lMask )

      ! by default, assume that ALL cells are included
      mask = lTRUE

      select case (trim(this%sMissingValuesOperator))

        case ("<=")

          where (values <= rMissing) mask = lFALSE

        case ("<")

          where (values < rMissing) mask = lFALSE

        case (">=")

          where (values >= rMissing) mask = lFALSE

        case (">")

          where (values > rMissing) mask = lFALSE

        case default

          call assert(lFALSE, "Unknown missing values code was supplied " &
            //"for processing data "//squote(this%sDescription)//": " &
            //dquote(this%sMissingValuesOperator) )

      end select

      ! scan for NaNs
      !> @TODO Replace isnan function with ieee_is_nan once gfortran supports it (gcc 5.1)
      where ( isnan( values) )  mask = lFALSE

    end associate

  end subroutine data_CreateMissingValuesMask_real

!-----------------------------------------------------------------------------------------------------

  subroutine data_CreateMissingValuesMask_int(this, iValues)

    class (T_DATA_GRID)               :: this
    integer (kind=c_int), intent(in)  :: iValues(:,:)

    ! [ LOCALS ]
    integer (kind=c_int)  :: iMissing

    iMissing = int(this%rMissingValuesCode, kind=c_int)

    associate ( values => iValues, mask => this%pGrdNative%lMask )

      ! by default, assume that ALL cells are included
      mask = lTRUE

      select case (trim(this%sMissingValuesOperator))

        case ("<=")

          where (values <= iMissing) mask = lFALSE

        case ("<")

          where (values < iMissing) mask = lFALSE

        case (">=")

          where (values >= iMissing) mask = lFALSE

        case (">")

          where (values > iMissing) mask = lFALSE

        case default

          call assert(lFALSE, "Unknown missing values code was supplied " &
            //"for processing data "//squote(this%sDescription)//": " &
            //dquote(this%sMissingValuesOperator) )

      end select

    end associate

  end subroutine data_CreateMissingValuesMask_int

!----------------------------------------------------------------------

end module data_factory
