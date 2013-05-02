module data_factory

  use types

  use swb_grid
  use stats
  use netcdf4_support
  use iso_c_binding
  implicit none
  private

  type, public :: T_DATA_GRID
    integer (kind=T_INT) :: iSourceDataForm  ! constant, static grid, dynamic grid
    integer (kind=T_INT) :: iSourceDataType  ! real, short, integer, etc.
    integer (kind=T_INT) :: iSourceFileType  ! Arc ASCII, Surfer, NetCDF
    integer (kind=T_INT) :: iTargetDataType  ! Fortran real, integer, etc.
    character (len=256) :: sDescription
    character (len=256) :: sSourcePROJ4_string
    character (len=256) :: sSourceFileType
    character (len=256) :: sFilenameTemplate    ! e.g. %Y_%#_prcp.nc
    character (len=256) :: sSourceFilename      ! e.g. 1980_00_prcp.nc
    character (len=256) :: sOldFilename = "NA"  ! e.g. 1980_00_prcp.nc
    integer (kind=T_INT) :: iFileCount = 0
    integer (kind=T_INT) :: iFileCountYear = -9999
    logical (kind=T_LOGICAL) :: lProjectionDiffersFromBase = lFALSE
    real (kind=T_SGL) :: rMinAllowedValue = 1.0
    real (kind=T_SGL) :: rMaxAllowedValue = 1.0e6
    real (kind=T_SGL) :: rScale = rONE
    real (kind=T_SGL) :: rOffset = rZERO
    real (kind=T_SGL) :: rConversionFactor = rONE

    logical (kind=T_LOGICAL) :: lApplyScaleAndOffset = lFALSE
    logical (kind=T_LOGICAL) :: lApplyConversionFactor = lFALSE
    logical (kind=T_LOGICAL) :: lMissingFilesAreAllowed = lFALSE

    integer (kind=T_INT) :: iDaysToPadAtYearsEnd = 0
    integer (kind=T_INT) :: iDaysToPadIfLeapYear = 1

    ! the following are only used if data are being read from a NetCDF file
    character (len=256) :: sVariableName_x = "x"
    character (len=256) :: sVariableName_y = "y"
    character (len=256) :: sVariableName_z = ""
    character (len=256) :: sVariableName_time = "time"

    type (T_GRID_BOUNDS) :: GRID_BOUNDS
    ! units?
    ! conversion factor?

    integer (kind=T_INT) :: iNC_FILE_STATUS
    type (T_NETCDF4_FILE), pointer :: NCFILE
    integer (kind=T_INT) :: iConstantValue
    real (kind=T_SGL) :: rConstantValue

    ! pGrdNative is a grid created to serve as an intermediary between
    ! the native coordinate of the data source file and the project coordinates
    ! in use by swb
    type (T_GENERAL_GRID), pointer :: pGrdNative
    logical (kind=T_LOGICAL) :: lGridIsPersistent = lFALSE
    logical (kind=T_LOGICAL) :: lGridHasChanged = lFALSE
    logical (kind=T_LOGICAL) :: lPerformFullInitialization = lTRUE

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
    procedure :: dump_data_structure => dump_data_structure_sub
    procedure :: transfer_from_native => transform_grid_to_grid

    procedure :: enforce_limits => data_GridEnforceLimits_int
    procedure :: calc_project_boundaries => calc_project_boundaries

  end type T_DATA_GRID

  type (T_DATA_GRID), dimension(9), public, target :: DAT

  integer (kind=T_INT), parameter, public :: LANDUSE_DATA = 1
  integer (kind=T_INT), parameter, public :: AWC_DATA = 2
  integer (kind=T_INT), parameter, public :: SOILS_GROUP_DATA = 3
  integer (kind=T_INT), parameter, public :: FLOWDIR_DATA = 4
  integer (kind=T_INT), parameter, public :: PRECIP_DATA = 5
  integer (kind=T_INT), parameter, public :: TMIN_DATA = 6
  integer (kind=T_INT), parameter, public :: TMAX_DATA = 7
  integer (kind=T_INT), parameter, public :: REL_HUM_DATA = 8
  integer (kind=T_INT), parameter, public :: SOL_RAD_DATA = 9


  integer (kind=T_INT), parameter :: NETCDF_FILE_OPEN = 27
  integer (kind=T_INT), parameter :: NETCDF_FILE_CLOSED = 42

contains

  subroutine initialize_constant_real_data_object_sub( &
     this, &
     sDescription, &
     rConstant )

     class (T_DATA_GRID) :: this
     character (len=*) :: sDescription
     real (kind=T_SGL), intent(in) :: rConstant

     this%rConstantValue = rConstant
     this%sDescription = trim(sDescription)
     this%iSourceDataForm = CONSTANT_GRID
     this%iSourceDataType = DATATYPE_REAL
     this%iTargetDataType = DATATYPE_REAL
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
     this%sDescription = trim(sDescription)
     this%iSourceDataForm = CONSTANT_GRID
     this%iSourceDataType = DATATYPE_INT
     this%iTargetDataType = DATATYPE_INT
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
   integer (kind=T_INT) :: iDataType
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

end subroutine initialize_netcdf_data_object_sub

!----------------------------------------------------------------------

  subroutine getvalues_sub( this, pGrdBase, iMonth, iDay, iYear, iJulianDay, &
      rValues, iValues)

    class (T_DATA_GRID) :: this
    type ( T_GENERAL_GRID ), pointer :: pGrdBase
    integer (kind=T_INT), intent(in), optional :: iMonth, iDay, iYear, iJulianDay
    real (kind=T_SGL), dimension(:,:), optional :: rValues
    integer (kind=T_INT), dimension(:,:), optional :: iValues

    ! [ LOCALS ]
    integer (kind=c_int) :: iNumDaysToPad
    integer (kind=c_int) :: iPadDays
    integer (kind=T_INT) :: iLocalJulianDay

    if ( this%iSourceDataForm == DYNAMIC_NETCDF_GRID ) then

      iLocalJulianDay = iJulianDay

      if (this%iNC_FILE_STATUS == NETCDF_FILE_OPEN) then

        iNumDaysToPad = this%iDaysToPadAtYearsEnd

        if(isLeap(iYear)) iNumDaysToPad = iNumDaysToPad + this%iDaysToPadIfLeapYear

        iPadDays = max(iJulianDay - this%NCFILE%iLastDayJD, 0)

        ! if the dataset is missing whole days of data at the end of the
        ! year, return the values found on the last day; repeat until
        ! we are legitimately into the next year
        if (iPadDays <= iNumDaysToPad &
            .and. iJulianDay > this%NCFILE%iLastDayJD) then

            iLocalJulianDay = this%NCFILE%iLastDayJD
            call echolog("NetCDF file ends before the calendar year is out:" &
              //" padding data with values from the last day of valid data")

        endif

      endif

      call getvalues_dynamic_netcdf_sub( this, &
                           pGrdBase,  iMonth, iDay, iYear, iLocalJulianDay)

    elseif(this%iSourceDataForm == DYNAMIC_GRID ) then

      call getvalues_gridded_sub( this, pGrdBase, iMonth, iDay, iYear)

    elseif(this%iSourceDataForm == STATIC_GRID ) then

      call getvalues_gridded_sub( this, pGrdBase)

    elseif(this%iSourceDataForm == CONSTANT_GRID ) then

      call getvalues_constant_sub( this, pGrdBase )

    else

    endif

    if (present(rValues)) &
       rValues = ( pGrdBase%rData * this%rScale + this%rOffset ) * this%rConversionFactor

    if (present(iValues)) &
       iValues = ( pGrdBase%iData * int(this%rScale, kind=T_INT)  &
                                  + int(this%rOffset,kind=T_INT) ) * this%rConversionFactor

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

  call grid_DumpGridExtent(this%pGrdNative)

  end subroutine dump_data_structure_sub

!----------------------------------------------------------------------

  subroutine getvalues_gridded_sub( this, pGrdBase, iMonth, iDay, iYear)

    class (T_DATA_GRID) :: this
    type ( T_GENERAL_GRID ), pointer :: pGrdBase
    integer (kind=T_INT), optional :: iMonth
    integer (kind=T_INT), optional :: iDay
    integer (kind=T_INT), optional :: iYear
    logical (kind=T_LOGICAL) :: lExist
    logical (kind=T_LOGICAL) :: lOpened

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

      ! TODO *** expand to include limits for real grids as well
      if (this%iSourceDataType == DATATYPE_INT) call this%enforce_limits()

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

      select case (this%iSourceDataType)

        case ( DATATYPE_REAL )

          pGrdBase%rData = this%pGrdNative%rData

        case ( DATATYPE_INT)

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
    character (len=256) :: sUppercaseFilename
    character (len=256) :: sCWD
    integer (kind=T_INT) :: iPos_Y, iPos_D, iPos_M, iPos, iLen, iCount
    logical (kind=T_LOGICAL) :: lMatch
    logical (kind=T_LOGICAL) :: lExist
    character (len=2) :: sBuf
    character (len=1) :: sDelimiter
    integer (kind=T_INT) :: iStatus

    iPos_Y = 0; iPos_M = 0; iPos_D = 0; iPos = 0

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

      iPos = index(sNewFilename, "%#")

      if (iPos > 0) then
        lMatch = lTRUE
        iLen=len_trim(sNewFilename)
        sNewFilename = sNewFilename(1:iPos-1)//trim(asCharacter(this%iFileCount)) &
                       //sNewFilename(iPos+2:iLen)
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

    this%sSourceFilename = trim(sCWD)//trim(sDelimiter)//trim(sNewFilename)

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

    ! [ LOCALS ]
    integer (kind=c_int) :: iTimeIndex

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

      if (this%lPerformFullInitialization) then

        allocate(this%NCFILE)

        call this%calc_project_boundaries(pGrdBase=pGrdBase)

        call netcdf_open_and_prepare(NCFILE=this%NCFILE, &
             sFilename=this%sSourceFilename, &
             sVarName_x=this%sVariableName_x, &
             sVarName_y=this%sVariableName_y, &
             sVarName_z=this%sVariableName_z, &
             sVarName_time=this%sVariableName_time, &
             tGridBounds=this%GRID_BOUNDS, &
             iLU=LU_LOG)

        this%iNC_FILE_STATUS = NETCDF_FILE_OPEN

        this%iSourceDataType = this%NCFILE%iVarType(NC_Z)

        this%pGrdNative => grid_CreateComplete ( iNX=this%NCFILE%iNX, &
                    iNY=this%NCFILE%iNY, &
                    rX0=this%NCFILE%rX(LEFT), &
                    rY0=this%NCFILE%rY(BOTTOM), &
                    rX1=this%NCFILE%rX(RIGHT), &
                    rY1=this%NCFILE%rY(TOP), &
!                    rX0=this%GRID_BOUNDS%rXll, &
!                    rY0=this%GRID_BOUNDS%rYll, &
!                    rX1=this%GRID_BOUNDS%rXur, &
!                    rY1=this%GRID_BOUNDS%rYur, &
                    iDataType=this%iTargetDataType )

        ! ensure that PROJ4 string is associated with the native grid
        this%pGrdNative%sPROJ4_string = this%sSourcePROJ4_string
        this%pGrdNative%sFilename = this%sSourceFilename

        this%lPerformFullInitialization = lFALSE

      else

        call netcdf_open_file(NCFILE=this%NCFILE, sFilename=this%sSourceFilename, iLU=LU_LOG)

        this%iNC_FILE_STATUS = NETCDF_FILE_OPEN

        call netcdf_update_time_range(NCFILE=this%NCFILE)

        if (.not. netcdf_date_within_range(NCFILE=this%NCFILE, iJulianDay=iJulianDay) ) then

          call echolog("Valid date range (NetCDF): "//trim(asCharacter(this%NCFILE%iFirstDayJD)) &
            //" to "//trim(asCharacter(this%NCFILE%iLastDayJD)) )
          call echolog("Current Julian Day value: "//trim(asCharacter(iJulianDay)) )

          call assert (lFALSE, "Date range for currently open NetCDF file" &
             //" does not include the present simulation date.", &
             trim(__FILE__), __LINE__)

        endif

      endif

    endif

    call netcdf_update_time_starting_index(NCFILE=this%NCFILE, &
                                           iJulianDay=iJulianDay)

    call netcdf_get_variable_time_y_x(NCFILE=this%NCFILE, rValues=this%pGrdNative%rData)

    call this%transfer_from_native( pGrdBase )

!    call stats_WriteMinMeanMax( iLU=LU_STD_OUT, &
!          sText="Precip: ", &
!          rData=pGrdBase%rData )


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
    integer (kind=T_INT) :: iRetVal
    real (kind=T_SGL) :: rMultiplier = 1.
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
                trim(this%sSourcePROJ4_string)//C_NULL_CHAR, 4, &
                rX, rY )

   this%GRID_BOUNDS%rXll = rX(1); this%GRID_BOUNDS%rXlr = rX(2)
   this%GRID_BOUNDS%rYll = rY(1); this%GRID_BOUNDS%rYlr = rY(2)
   this%GRID_BOUNDS%rXul = rX(3); this%GRID_BOUNDS%rXur = rX(4)
   this%GRID_BOUNDS%rYul = rY(3); this%GRID_BOUNDS%rYur = rY(4)

!   print *, "--"
!   print *, "            X                            Y"
!   print *, "LL: ", this%GRID_BOUNDS%rXll, this%GRID_BOUNDS%rYll
!   print *, "LR: ", this%GRID_BOUNDS%rXlr, this%GRID_BOUNDS%rYlr
!   print *, "UL: ", this%GRID_BOUNDS%rXul, this%GRID_BOUNDS%rYul
!   print *, "UR: ", this%GRID_BOUNDS%rXur, this%GRID_BOUNDS%rYur

  end subroutine calc_project_boundaries

!----------------------------------------------------------------------

  subroutine data_GridEnforceLimits_int(this)

    class (T_DATA_GRID) :: this

    ! [ LOCALS ]
    integer (kind=T_INT) :: iMin, iMax

    iMin = int(this%rMinAllowedValue, kind=T_INT)
    iMax = int(this%rMaxAllowedValue, kind=T_INT)

    where ( this%pGrdNative%iData < iMin )  this%pGrdNative%iData = iMin
    where ( this%pGrdNative%iData > iMax )  this%pGrdNative%iData = iMax

  end subroutine data_GridEnforceLimits_int

end module data_factory
