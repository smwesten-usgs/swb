!> @file
!> This module provides a layer of abstraction between the
!> SWB data structures and the source of the data being accessed. The idea is that
!> when SWB requests updated grid values, this is the module that requests data
!> from the appropriate source, whether that is an ARC ASCII grid or NetCDF, etc.
!> @ingroup data

!> Provide a uniform way from within the main SWB code to access data,
!> for example, call mydata%getvalues(), which will retrieve data from the
!> appropriate underlying source, whether that is a series of ARC ASCII grids,
!> a single NetCDF file, or a series of NetCDF files.
module data_factory

  use types

  use swb_grid

  use netcdf4_support
  use iso_c_binding
  implicit none
  private

  integer (kind=c_int), parameter :: NETCDF_FILE_OPEN = 27
  integer (kind=c_int), parameter :: NETCDF_FILE_CLOSED = 42

  public :: NETCDF_FILE_OPEN, NETCDF_FILE_CLOSED
  public :: T_DATA_GRID

  !> @class T_DATA_GRID
  type T_DATA_GRID
    integer (kind=c_int) :: iSourceDataForm    ! constant, static grid, dynamic grid
    integer (kind=c_int) :: iSourceDataType = DATATYPE_NA  ! real, short, integer, etc.
    integer (kind=c_int) :: iSourceFileType  ! Arc ASCII, Surfer, NetCDF
    integer (kind=c_int) :: iTargetDataType = DATATYPE_NA  ! Fortran real, integer, etc.
    character (len=256) :: sDescription = ""
    character (len=256) :: sSourcePROJ4_string
    character (len=256) :: sSourceFileType
    character (len=256) :: sFilenameTemplate    ! e.g. %Y_%#_prcp.nc
    character (len=256) :: sSourceFilename      ! e.g. 1980_00_prcp.nc
    character (len=256) :: sOldFilename = "NA"  ! e.g. 1980_00_prcp.nc
    integer (kind=c_int) :: iFileCount = -1
    integer (kind=c_int) :: iFileCountYear = -9999
    logical (kind=c_bool) :: lProjectionDiffersFromBase = lFALSE
    real (kind=c_float) :: rMinAllowedValue = -rBIGVAL     ! default condition is to impose
    real (kind=c_float) :: rMaxAllowedValue = rBIGVAL      ! no bounds on data
    integer (kind=c_int) :: iMinAllowedValue = -iBIGVAL     ! default condition is to impose
    integer (kind=c_int) :: iMaxAllowedValue = iBIGVAL      ! no bounds on data
    real (kind=c_float) :: rMissingValuesCode = -rBIGVAL
    integer (kind=c_int) :: iMissingValuesCode = -iBIGVAL
    character (len=2) :: sMissingValuesOperator = "<="
    integer (kind=c_int) :: iMissingValuesAction = 0
    real (kind=c_double) :: rScaleFactor = 1_c_double
    real (kind=c_double) :: rAddOffset = 0_c_double
    real (kind=c_double) :: rConversionFactor = 1_c_double

    logical (kind=c_bool) :: lUserSuppliedScaleAndOffset = lFALSE
    logical (kind=c_bool) :: lApplyConversionFactor = lFALSE
    logical (kind=c_bool) :: lMissingFilesAreAllowed = lFALSE
    logical (kind=c_bool) :: lFlipHorizontal = lFALSE
    logical (kind=c_bool) :: lFlipVertical = lFALSE

    integer (kind=c_int) :: iDaysToPadAtYearsEnd = 0
    integer (kind=c_int) :: iDaysToPadIfLeapYear = 1
    integer (kind=c_int) :: iStartYear = -9999
    integer (kind=c_int) :: iEndYear = -9999
    logical (kind=c_bool) :: lPadReplaceWithZero = lFALSE
    logical (kind=c_bool) :: lPadValues = lFALSE

    ! the following are only used if data are being read from a NetCDF file
    character (len=256) :: sVariableName_x = "x"
    character (len=256) :: sVariableName_y = "y"
    character (len=256) :: sVariableName_z = ""
    character (len=256) :: sVariableName_time = "time"
    character (len=3) :: sVariableOrder = "tyx"

    type (T_GRID_BOUNDS) :: GRID_BOUNDS
    ! units?
    ! conversion factor?

    integer (kind=c_int) :: iNC_FILE_STATUS = NETCDF_FILE_CLOSED
    type (T_NETCDF4_FILE) :: NCFILE

    integer (kind=c_int) :: iNC_ARCHIVE_STATUS = NETCDF_FILE_CLOSED
    type (T_NETCDF4_FILE) :: NCFILE_ARCHIVE
    integer (kind=c_size_t) :: iNCFILE_RECNUM = 0

    integer (kind=c_int) :: iConstantValue
    real (kind=c_float) :: rConstantValue

    ! pGrdNative is a grid created to serve as an intermediary between
    ! the native coordinate of the data source file and the project coordinates
    ! in use by swb
    type (T_GENERAL_GRID), pointer :: pGrdNative => NULL()
    logical (kind=c_bool) :: lGridIsPersistent = lFALSE
    logical (kind=c_bool) :: lGridHasChanged = lFALSE
    logical (kind=c_bool) :: lPerformFullInitialization = lTRUE
    logical (kind=c_bool) :: lCreateLocalNetCDFArchive = lFALSE

  contains

    procedure :: init_const_int => initialize_constant_int_data_object_sub
    procedure :: init_const_real => initialize_constant_real_data_object_sub
    procedure :: init_gridded => initialize_gridded_data_object_sub

    procedure :: initialize_netcdf => initialize_netcdf_data_object_sub

    generic :: initialize => init_const_int, &
                             init_const_real, &
                             init_gridded

    procedure :: set_scale => set_scale_sub
    procedure :: set_offset => set_offset_sub

    procedure :: set_minimum_allowable_value_int_sub
    procedure :: set_minimum_allowable_value_real_sub
    procedure :: set_maximum_allowable_value_int_sub
    procedure :: set_maximum_allowable_value_real_sub
    generic :: set_valid_minimum => set_minimum_allowable_value_int_sub, &
                                    set_minimum_allowable_value_real_sub
    generic :: set_valid_maximum => set_maximum_allowable_value_int_sub, &
                                    set_maximum_allowable_value_real_sub

    procedure :: set_grid_flip_horizontal => set_grid_flip_horizontal_sub
    procedure :: set_grid_flip_vertical => set_grid_flip_vertical_sub
    procedure :: set_conversion_factor => set_conversion_factor_sub

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
    procedure :: set_PROJ4 => set_PROJ4_string_sub
    procedure :: set_variable_order => set_variable_order_sub
    procedure :: dump_data_structure => dump_data_structure_sub
    procedure :: set_make_local_archive => set_archive_local_sub
    procedure :: put_values_to_archive => put_values_to_local_NetCDF_sub
    procedure :: transfer_from_native => transform_grid_to_grid

    procedure :: enforce_limits_real => data_GridEnforceLimits_real
    procedure :: enforce_limits_int => data_GridEnforceLimits_int
    generic :: enforce_limits => enforce_limits_real, enforce_limits_int

    procedure :: handle_missing_values_real => data_GridHandleMissingData_real
    procedure :: handle_missing_values_int => data_GridHandleMissingData_int
    generic :: handle_missing_values => handle_missing_values_real, &
                                        handle_missing_values_int

    procedure :: calc_project_boundaries => calc_project_boundaries
    procedure :: test_for_need_to_pad_values => test_for_need_to_pad_values

  end type T_DATA_GRID

  type (T_DATA_GRID), dimension(12), public, target :: DAT

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

  integer (kind=c_int), parameter, public :: MISSING_VALUES_ZERO_OUT = 0
  integer (kind=c_int), parameter, public :: MISSING_VALUES_REPLACE_WITH_MEAN = 1

contains

  subroutine initialize_constant_real_data_object_sub( &
     this, &
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

  subroutine initialize_constant_int_data_object_sub( &
    this, &
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
      //" -- NOT BOTH!", trim(__FILE__), __LINE__)

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

subroutine initialize_netcdf_data_object_sub( &
   this, &
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
      //" -- NOT BOTH!", trim(__FILE__), __LINE__)

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

    if (present(rValues)) then

       rValues = ( pGrdBase%rData * this%rScaleFactor + this%rAddOffset ) * this%rConversionFactor

    endif

    if (present(iValues)) then

        iValues = ( pGrdBase%iData * int(this%rScaleFactor, kind=c_int)  &
                                  + int(this%rAddOffset,kind=c_int) ) * this%rConversionFactor
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

      call dump_data_structure_sub(this)

      call assert(lFALSE, "INTERNAL PROGRAMMING ERROR - Unhandled data type: " &
        //"name="//dquote(this%sDescription) &
        //"; value="//trim(asCharacter(this%iSourceDataType)), &
        trim(__FILE__), __LINE__)

    end select

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
      !!! if the file doesn't exist, EXIT!
      if (.not. lExist ) then
        if ( this%lMissingFilesAreAllowed ) then
         exit
        else
          call assert( lFALSE, &
            "Could not find input data file~filename:"//dquote(this%sSourceFilename) &
            //"~data description: "//trim(this%sDescription))
        endif
      endif

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

      write( unit=LU_LOG, fmt="(a)") "Opened file "//dQuote(this%sSourceFilename) &
        //" for "//trim(this%sDescription)//" data."

      this%lGridHasChanged = lTRUE

      select case (this%iTargetDataType)

        case ( GRID_DATATYPE_REAL )

          call this%handle_missing_values(this%pGrdNative%rData)
          call this%enforce_limits(this%pGrdNative%rData)

        case ( GRID_DATATYPE_INT )

!          call this%handle_missing_values(this%pGrdNative%iData)
!          call this%enforce_limits(this%pGrdNative%iData)

        case default

          call assert(lFALSE, "INTERNAL PROGRAMMING ERROR - Unhandled data type: value=" &
            //trim(asCharacter(this%iSourceDataType)), &
            trim(__FILE__), __LINE__)

      end select

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

          call grid_gridToGrid(pGrdFrom=this%pGrdNative,&
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
    integer (kind=c_int), optional :: iMonth, iDay, iYear

    ! [ LOCALS ]
    character (len=256) :: sNewFilename
    character (len=256) :: sUppercaseFilename
    character (len=256) :: sCWD
    character (len=256) :: sBuf2
    integer (kind=c_int) :: iPos_Y, iPos_D, iPos_M, iPos, iPos2, iLen, iCount
    integer (kind=c_int) :: iNumZeros, iNumZerosToPrint
    logical (kind=c_bool) :: lMatch
    logical (kind=c_bool) :: lExist
    character (len=2) :: sBuf
    character (len=12) :: sNumber
    character (len=1) :: sDelimiter
    integer (kind=c_int) :: iStatus

    iPos_Y = 0; iPos_M = 0; iPos_D = 0; iPos = 0; sNumber = ""

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

      if (present(iYear) ) iPos_Y = &
           max(index(sNewFilename, "%Y"), index(sNewFilename, "%y") )

      if (iPos_Y > 0) then
        lMatch = lTRUE
        iLen=len_trim(sNewFilename)
        sNewFilename = sNewFilename(1:iPos_Y - 1)//trim(asCharacter(iYear)) &
                       //sNewFilename(iPos_Y + 2:iLen)

      endif

      iPos = index(sNewFilename, "#")

      if (iPos > 0) then

        iPos2 = index(sNewFilename(1:iPos),"%", BACK=lTRUE)
        sBuf2 = trim(asCharacter(this%iFileCount))
        iNumZeros = max(0, iPos - iPos2 - 1)

        if (iNumZeros > 0) then
          iNumZerosToPrint = max(0,iNumZeros - len_trim(sBuf2) + 1)
          sNumber = repeat("0", iNumZerosToPrint )//trim(sBuf2)
        else
          sNumber = repeat("0", iNumZeros - len_trim(sBuf2) )//trim(sBuf2)
        endif

        lMatch = lTRUE
        iLen=len_trim(sNewFilename)
        sNewFilename = sNewFilename(1:iPos-2-iNumZeros)//trim(sNumber) &
                       //sNewFilename(iPos+1:iLen)
      endif

      if (present(iMonth) ) iPos_M = &
          max(index(sNewFilename, "%M"), index(sNewFilename, "%m") )

      if (iPos_M > 0) then
        lMatch = lTRUE
        write (unit=sBuf, fmt="(i2.2)") iMonth

        iLen=len_trim(sNewFilename)
        sNewFilename = sNewFilename(1:iPos_M - 1)//sBuf &
                       //sNewFilename(iPos_M + 2:iLen)
      endif

      if (present(iDay) ) iPos_D = &
           max(index(sNewFilename, "%D"),index(sNewFilename, "%d") )

      if (iPos_D > 0) then
        lMatch = lTRUE
        write (unit=sBuf, fmt="(i2.2)") iDay
        iLen=len_trim(sNewFilename)
        sNewFilename = sNewFilename(1:iPos_D - 1)//sBuf &
                       //sNewFilename(iPos_D + 2:iLen)
      endif

      if (.not. lMatch) exit

      iCount = iCount + 1

      ! failsafe
      if (iCount > 4) exit

    enddo

    if( index(string=sCWD, substring="/") > 0 ) then
      sDelimiter = "/"
    else
      sDelimiter = "\"
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

      !> if this is a URL, we don't want to test for file existence using
      !> the Fortran "inquire" function
      if (this%sSourceFilename(iPos:iPos+6) == "http://") then

        exit

      else

        ! does this file actually exist?
        inquire( file=this%sSourceFilename, exist=lExist )

        !> if the file exists, don't bother with padding any values
        if (lExist) exit

        !> if file doesn't exist, and we're close to the end of the year,
        !> assume that we should pad values at the end of the year
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

        !> if we've reached this point, we cannot locate the proper file and
        !> we are not within the proper range of dates to allow for padding.
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

      call this%reset_at_yearend_filecount(iYear)

      call this%make_filename( iMonth=iMonth, iYear=iYear, iDay=iDay)

      this%lPadValues = this%test_for_need_to_pad_values(iMonth=iMonth, iYear=iYear, iDay=iDay)

       !> call to test_for_need_to_pad_values return value of "TRUE" if
      !> if attempts to open a nonexistent file within the last few days of a year.
      !> The assumption is that values missing at the end of a calendar year
      !> translates into a missing file at the year's end

      if (.not. this%lPadValues) then

        if (this%lPerformFullInitialization ) then

          if( len_trim( this%sSourcePROJ4_string ) > 0 ) then

            !> calculate the project boundaries in the coordinate system of
            !> the native data file
            call this%calc_project_boundaries(pGrdBase=pGrdBase)

            call netcdf_open_and_prepare_as_input(NCFILE=this%NCFILE, &
              sFilename=this%sSourceFilename, &
              lFlipHorizontal=this%lFlipHorizontal, &
              lFlipVertical=this%lFlipVertical, &
              sVariableOrder=this%sVariableOrder, &
              sVarName_x=this%sVariableName_x, &
              sVarName_y=this%sVariableName_y, &
              sVarName_z=this%sVariableName_z, &
              sVarName_time=this%sVariableName_time, &
              tGridBounds=this%GRID_BOUNDS, &
              iLU=LU_LOG)

          else  ! PROJ4 string is blank

            !> assume source NetCDF file is in same projection and
            !> of same dimensions as base grid
            call netcdf_open_and_prepare_as_input(NCFILE=this%NCFILE, &
              sFilename=this%sSourceFilename, &
              lFlipHorizontal=this%lFlipHorizontal, &
              lFlipVertical=this%lFlipVertical, &
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

          !> if the user has not supplied a scale and offset,
          !> then populate these values with the scale and offset
          !> factor included in the NetCDF attribute data, if any.
          if (.not. this%lUserSuppliedScaleAndOffset) then
            this%rAddOffset = this%NCFILE%rAddOffset(NC_Z)
            this%rScaleFactor = this%NCFILE%rScaleFactor(NC_Z)
          endif

          !> Amongst other things, the call to netcdf_open_and_prepare
          !> finds the nearest column and row that correspond to the
          !> project bounds, then back-calculates the coordinate values
          !> of the column and row numbers in the *NATIVE* coordinate system
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

          this%lPerformFullInitialization = lFALSE

        else
          !> Projection settings can be left alone; read values from new
          !> NetCDF file with same grid boundaries, projection, etc.

!          call netcdf_open_file(NCFILE=this%NCFILE, sFilename=this%sSourceFilename, iLU=LU_LOG)
          call netcdf_open_file(NCFILE=this%NCFILE, sFilename=this%sSourceFilename)

          this%iNC_FILE_STATUS = NETCDF_FILE_OPEN

        endif

        if (.not. netcdf_date_within_range(NCFILE=this%NCFILE, iJulianDay=iJulianDay) ) then

          call echolog("Valid date range (NetCDF): "//trim(asCharacter(this%NCFILE%iFirstDayJD)) &
            //" to "//trim(asCharacter(this%NCFILE%iLastDayJD)) )

          call echolog("Current Julian Day value: "//trim(asCharacter(iJulianDay)) )

          call assert (lFALSE, "Date range for currently open NetCDF file" &
            //" does not include the present simulation date.", &
            trim(__FILE__), __LINE__)

        endif

      endif   ! if(lPadValues)

    endif  ! If (NC_FILE_STATUS == NETCDF_CLOSED)

    if (.not. this%lPadValues) then

      do
        lDateTimeFound = netcdf_update_time_starting_index(NCFILE=this%NCFILE, &
                                         iJulianDay=iJulianDay)

        if (.not. lDateTimeFound) then
          this%lPadValues = lTRUE
          exit
        endif

        call netcdf_get_variable_slice(NCFILE=this%NCFILE, rValues=this%pGrdNative%rData)
        call this%handle_missing_values(this%pGrdNative%rData)
        call this%enforce_limits(this%pGrdNative%rData)
        exit
      enddo

    endif

    if (this%lPadValues) then

      if (this%lPadReplaceWithZero) then

        this%pGrdNative%rData = 0_c_float
        this%pGrdNative%iData = 0_c_int

      endif

      call echolog( repeat("=", 60) )
      call echolog( "Missing day found in NetCDF file - padding values" )
!      call stats_WriteMinMeanMax( iLU=LU_STD_OUT, &
!        sText=trim(this%NCFILE%sFilename), &
!        rData=this%pGrdNative%rData)
!      call stats_WriteMinMeanMax( iLU=LU_LOG, &
!        sText=trim(this%NCFILE%sFilename), &
!        rData=this%pGrdNative%rData)
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

    if (this%iNC_ARCHIVE_STATUS == NETCDF_FILE_CLOSED) then

      call netcdf_open_and_prepare_as_output(NCFILE=this%NCFILE, &
               NCFILE_ARCHIVE=this%NCFILE_ARCHIVE, &
               iOriginMonth=iMonth, iOriginDay=iDay, iOriginYear=iYear, &
               iStartYear=this%iStartYear, iEndYear=this%iEndYear)

      this%iNC_ARCHIVE_STATUS = NETCDF_FILE_OPEN

    endif

    iNumRows = int(size(this%pGrdNative%rData, 2), kind=c_size_t)
    iNumCols = int(size(this%pGrdNative%rData, 1), kind=c_size_t)
    iNumRecs = this%iNCFILE_RECNUM

    call netcdf_put_variable_array(NCFILE=this%NCFILE_ARCHIVE, &
       iVarID=this%NCFILE_ARCHIVE%iVarID(NC_Z), &
       iStart=[iNumRecs, 0_c_size_t, 0_c_size_t], &
       iCount=[1_c_size_t, iNumRows, iNumCols], &
       iStride=[1_c_ptrdiff_t,1_c_ptrdiff_t,1_c_ptrdiff_t], &
       rValues=this%pGrdNative%rData)

    call netcdf_put_variable_vector(NCFILE=this%NCFILE_ARCHIVE, &
       iVarID=this%NCFILE_ARCHIVE%iVarID(NC_TIME), &
       iStart=[this%iNCFILE_RECNUM], &
       iCount=[1_c_size_t], &
       iStride=[1_c_size_t], &
       dpValues=[real(this%iNCFILE_RECNUM, kind=c_double)])

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

subroutine set_scale_sub(this, rScaleFactor)

   class (T_DATA_GRID) :: this
   real (kind=c_float) :: rScaleFactor

   this%rScaleFactor = rScaleFactor
   this%lUserSuppliedScaleAndOffset = lTRUE

end subroutine set_scale_sub

!----------------------------------------------------------------------

subroutine set_conversion_factor_sub(this, rConversionFactor)

   class (T_DATA_GRID) :: this
   real (kind=c_float) :: rConversionFactor

   this%rConversionFactor = rConversionFactor

end subroutine set_conversion_factor_sub

!----------------------------------------------------------------------

subroutine set_archive_local_sub(this, lValue)

   class (T_DATA_GRID) :: this
   logical (kind=c_bool) :: lValue

   this%lCreateLocalNetCDFArchive = lValue

end subroutine set_archive_local_sub

!----------------------------------------------------------------------

subroutine set_offset_sub(this, rAddOffset)

   class (T_DATA_GRID) :: this
   real (kind=c_float) :: rAddOffset

   this%rAddOffset = rAddOffset
   this%lUserSuppliedScaleAndOffset = lTRUE

end subroutine set_offset_sub

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
    real (kind=c_float) :: rMultiplier = 1.5
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

  !> because PROJ4 works in RADIANS if data are unprojected (i.e. GEOGRAPHIC),
  !> we need to convert back to degrees on the assumption that the coordinates
  !> referenced in the file will also be i degrees
  if( index(string=trim(this%sSourcePROJ4_string), substring="latlon") > 0 &
      .or. index(string=trim(this%sSourcePROJ4_string), substring="lonlat") > 0 ) then

    rX = rad2deg(rX)
    rY = rad2deg(rY)

  endif

   this%GRID_BOUNDS%rXll = rX(1); this%GRID_BOUNDS%rXlr = rX(2)
   this%GRID_BOUNDS%rYll = rY(1); this%GRID_BOUNDS%rYlr = rY(2)
   this%GRID_BOUNDS%rXul = rX(3); this%GRID_BOUNDS%rXur = rX(4)
   this%GRID_BOUNDS%rYul = rY(3); this%GRID_BOUNDS%rYur = rY(4)

#ifdef DEBUG_PRINT
   print *, " "
   print *, "--  BASE GRID BOUNDS projected to DATA NATIVE COORDS"
   print *, "FROM: ", dquote(pGrdBase%sPROJ4_string)
   print *, "TO:   ", dquote(this%sSourcePROJ4_string)
   PRINT *, "file: ", dquote(this%sSourceFileName)
   print *, "            X                            Y"
   print *, "LL: ", this%GRID_BOUNDS%rXll, this%GRID_BOUNDS%rYll
   print *, "LR: ", this%GRID_BOUNDS%rXlr, this%GRID_BOUNDS%rYlr
   print *, "UL: ", this%GRID_BOUNDS%rXul, this%GRID_BOUNDS%rYul
   print *, "UR: ", this%GRID_BOUNDS%rXur, this%GRID_BOUNDS%rYur
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

    class (T_DATA_GRID) :: this
    real (kind=c_float), dimension(:,:) :: rValues

    ! [ LOCALS ]
    real (kind=c_float) :: rMin, rMax

    rMin = real(this%rMinAllowedValue, kind=c_float)
    rMax = real(this%rMaxAllowedValue, kind=c_float)

    where ( rValues < rMin )  rValues = rMin
    where ( rValues > rMax )  rValues = rMax

  end subroutine data_GridEnforceLimits_real

!----------------------------------------------------------------------

  subroutine data_GridHandleMissingData_real(this, rValues)

    class (T_DATA_GRID) :: this
    real (kind=c_float), dimension(:,:), intent(inout) :: rValues

    ! [ LOCALS ]
    real (kind=c_float) :: rMissing, rMean

    rMissing = real(this%rMissingValuesCode, kind=c_float)

    select case (this%iMissingValuesAction)

      case (MISSING_VALUES_ZERO_OUT)

        select case (trim(this%sMissingValuesOperator))

          case ("<=")

            where (rValues <= rMissing) rValues = rZERO

          case ("<")

            where (rValues < rMissing) rValues = rZERO

          case (">=")

            where (rValues >= rMissing) rValues = rZERO

          case (">")

            where (rValues > rMissing) rValues = rZERO

          case default

            call assert(lFALSE, "Unknown missing values code was supplied " &
              //"for processing data "//squote(this%sDescription)//": " &
              //dquote(this%sMissingValuesOperator) )

          end select

      case (MISSING_VALUES_REPLACE_WITH_MEAN)

        select case (this%sMissingValuesOperator)

          case ("<=")

            rMean = sum(rValues, rValues > rMissing ) &
               / count(rValues > rMissing )

            where (rValues <= rMissing) rValues = rMean

          case ("<")

            rMean = sum(rValues, rValues >= rMissing ) &
               / count(rValues >= rMissing )

            where (rValues < rMissing) rValues = rMean

          case (">=")

            rMean = sum(rValues, rValues < rMissing ) &
               / count(rValues < rMissing )

            where (rValues >= rMissing) rValues = rMean

          case (">")

            rMean = sum(rValues, rValues <= rMissing ) &
               / count(rValues <= rMissing )

            where (rValues > rMissing) rValues = rMean

          case default

            call assert(lFALSE, "Unknown missing values code was supplied " &
              //"for processing data "//squote(this%sDescription)//": " &
              //dquote(this%sMissingValuesOperator) )

          end select

      case default

        call assert(lFALSE, "INTERNAL PROGRAMMING ERROR - unhandled iMissingValuesAction", &
        trim(__FILE__), __LINE__)

    end select

  end subroutine data_GridHandleMissingData_real

!----------------------------------------------------------------------

  subroutine data_GridHandleMissingData_int(this, iValues)

    class (T_DATA_GRID) :: this
    integer (kind=c_int), dimension(:,:), intent(inout) :: iValues

    ! [ LOCALS ]
    integer (kind=c_int) :: iMissing, iMean

    iMissing = this%iMissingValuesCode

    select case (this%iMissingValuesAction)

      case (MISSING_VALUES_ZERO_OUT)

        select case (trim(this%sMissingValuesOperator))

          case ("<=")

            where (iValues <= iMissing) iValues = iZERO

          case ("<")

            where (iValues < iMissing) iValues = iZERO

          case (">=")

            where (iValues >= iMissing) iValues = iZERO

          case (">")

            where (iValues > iMissing) iValues = iZERO

          case default

            call assert(lFALSE, "Unknown missing values code was supplied " &
              //"for processing data "//squote(this%sDescription)//": " &
              //dquote(this%sMissingValuesOperator) )

          end select

      case (MISSING_VALUES_REPLACE_WITH_MEAN)

        select case (this%sMissingValuesOperator)

          case ("<=")

            iMean = sum(iValues, iValues > iMissing ) &
               / count(iValues > iMissing )

            where (iValues <= iMissing) iValues = iMean

          case ("<")

            iMean = sum(iValues, iValues >= iMissing ) &
               / count(iValues >= iMissing )

            where (iValues < iMissing) iValues = iMean

          case (">=")

            iMean = sum(iValues, iValues < iMissing ) &
               / count(iValues < iMissing )

            where (iValues >= iMissing) iValues = iMean

          case (">")

            iMean = sum(iValues, iValues <= iMissing ) &
               / count(iValues <= iMissing )

            where (iValues > iMissing) iValues = iMean

          case default

            call assert(lFALSE, "Unknown missing values code was supplied " &
              //"for processing data "//squote(this%sDescription)//": " &
              //dquote(this%sMissingValuesOperator) )

          end select

      case default

        call assert(lFALSE, "INTERNAL PROGRAMMING ERROR - unhandled iMissingValuesAction", &
        trim(__FILE__), __LINE__)

    end select

  end subroutine data_GridHandleMissingData_int

end module data_factory
