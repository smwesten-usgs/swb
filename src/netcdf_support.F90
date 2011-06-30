!> @file
!> @brief  Contains a single module, @ref netcdf_support, which
!>  provides support for use of NetCDF files as input or output.

!> @brief Provides support for use of NetCDF files as input for time-varying,
!>  gridded meteorlogic data, or output for any SWB-generated variable.
module netcdf_support

#ifdef NETCDF_SUPPORT

  use types
  use swb_grid
  use typesizes
  use netcdf

  implicit none

  type T_ELLIPSOID
    integer (kind=T_INT) :: iID
    character (len=48) :: sEllipsoidName
    real (kind=T_DBL) :: rEquatorialRadius
    real (kind=T_DBL) :: rEccentricitySquared
  end type T_ELLIPSOID

  type (T_ELLIPSOID), target, dimension(24) :: Ellipsoid = (/ &

  	T_ELLIPSOID( 1, "Airy", 6377563_T_DBL, 0.00667054), &
  	T_ELLIPSOID( 2, "Australian National", 6378160_T_DBL, 0.006694542), &
  	T_ELLIPSOID( 3, "Bessel 1841", 6377397_T_DBL, 0.006674372), &
	T_ELLIPSOID( 4, "Bessel 1841 (Nambia) ", 6377484, 0.006674372), &
  	T_ELLIPSOID( 5, "Clarke 1866", 6378206, 0.006768658), &
  	T_ELLIPSOID( 6, "Clarke 1880", 6378249, 0.006803511), &
  	T_ELLIPSOID( 7, "Everest", 6377276, 0.006637847), &
  	T_ELLIPSOID( 8, "Fischer 1960 (Mercury) ", 6378166, 0.006693422), &
  	T_ELLIPSOID( 9, "Fischer 1968", 6378150, 0.006693422), &
  	T_ELLIPSOID( 10, "GRS 1967", 6378160, 0.006694605), &
  	T_ELLIPSOID( 11, "GRS 1980", 6378137_T_DBL, 0.00669438_T_DBL), &
  	T_ELLIPSOID( 12, "Helmert 1906", 6378200, 0.006693422), &
  	T_ELLIPSOID( 13, "Hough", 6378270, 0.00672267), &
  	T_ELLIPSOID( 14, "International", 6378388, 0.00672267), &
  	T_ELLIPSOID( 15, "Krassovsky", 6378245, 0.006693422), &
  	T_ELLIPSOID( 16, "Modified Airy", 6377340, 0.00667054), &
  	T_ELLIPSOID( 17, "Modified Everest", 6377304, 0.006637847), &
  	T_ELLIPSOID( 18, "Modified Fischer 1960", 6378155, 0.006693422), &
  	T_ELLIPSOID( 19, "South American 1969", 6378160, 0.006694542), &
  	T_ELLIPSOID( 20, "WGS 60", 6378165, 0.006693422), &
  	T_ELLIPSOID( 21, "WGS 66", 6378145, 0.006694542), &
  	T_ELLIPSOID( 22, "WGS-72", 6378135, 0.006694318), &
  	T_ELLIPSOID( 23, "WGS-84", 6378137, 0.00669438), &
  	T_ELLIPSOID( 24, "Placeholder", 0, 0) /)

contains


function netcdf_create(sNetCDF_Filename)  result(iNCID)

  character(len=*) :: sNetCDF_Filename
  integer(kind=T_INT) :: iNCID

  write(UNIT=LU_LOG,FMT="(a,a)") "Attempting to open WRITE-ENABLED NetCDF file: ", &
    TRIM(sNetCDF_Filename)
  call netcdf_check( nf90_create(TRIM(sNetCDF_Filename), NF90_CLOBBER, iNCID) )
  write(UNIT=LU_LOG,FMT="(a)") "   Succeeded."

end function netcdf_create

!----------------------------------------------------------------------

function netcdf_open(sNetCDF_Filename)  result(iNCID)

  character(len=*) :: sNetCDF_Filename
  integer(kind=T_INT) :: iNCID

  write(UNIT=LU_LOG,FMT="(a)") "Attempting to open READONLY NetCDF file: " &
    //TRIM(sNetCDF_Filename)
  call netcdf_check( nf90_open(TRIM(sNetCDF_Filename), nf90_nowrite, iNCID), &
         TRIM(__FILE__),__LINE__)
  write(UNIT=LU_LOG,FMT="(a,i4)") "   Succeeded.   ncid: ",iNCID

end function netcdf_open

!----------------------------------------------------------------------

  subroutine netcdf_info(pConfig, iVarNum, iMode, sVariableName)

  type (T_MODEL_CONFIGURATION), pointer :: pConfig  ! pointer to data structure that contains
                                                    ! model options, flags, and other settings
  integer (kind=T_INT) :: iVarNum    ! corresponds to T_STATS parameters
  integer (kind=T_INT) :: iMode      ! iINPUT, iOUTPUT, or iOBSERVATION
  character(len=*) :: sVariableName  ! NC variable name

  ! [LOCALS]
  type (T_NETCDF_FILE), pointer :: pNC

  character(len=256), allocatable, dimension (:) :: sDimName, sVarName
  real, allocatable, dimension(:) :: rXCoord, rYCoord
  character(len=256), allocatable, dimension (:,:) :: sAttribName, &
      sAttribValue
  character(len=256) :: sUnitsString, sTextFragment
  real(kind=T_SGL), allocatable, dimension (:,:) :: rAttribValue

  integer(kind=T_INT), allocatable, dimension (:,:) :: iAttribType
  integer(kind=T_INT), allocatable, dimension (:) :: iVarType
  integer(kind=T_INT), allocatable, dimension (:,:) :: iValue, iVarDimID
  integer(kind=T_INT) :: nDimensions, nVariables, nAttributes, &
    unlimitedDimID, i, j, k
  integer(kind=T_INT), allocatable, dimension(:) :: iDimLen, iVarDim, iVarNumAttribs
  integer(kind=T_INT), parameter :: MAX_ATTRIBUTES = 20
  integer(kind=T_INT), parameter :: MAX_DIMENSIONS = 3
  integer(kind=T_INT) :: iXDim,iYDim, iTDim,iZVar,iTVar = -999
  real(kind=T_SGL) :: rCellSize
  integer(kind=T_INT) :: iStat, iMonth, iDay, iYear
  real(kind=EightByteReal) :: rStartDay, rEndDay

  iXDim = -999; iYDim = -999; iTDim = -999;iZVar = -999;iTVar = -999

  pNC => pConfig%NETCDF_FILE(iVarNum,iMode)

! the following call works w netCDF 3.6.3, but not earlier versions
!  call netcdf_check(nf90_inquire(iNCID,nDimensions, nVariables, nAttributes, &
!    unlimitedDimID, formatNum),__FILE__,__LINE__)

  call netcdf_check(nf90_inquire(pNC%iNCID,nDimensions, nVariables, nAttributes, &
    unlimitedDimID),__FILE__,__LINE__)

  allocate(sDimName(nDimensions))
  allocate(iDimLen(nDimensions))

  write(unit=LU_LOG,FMT="(/,a,/,t5,a,t34,a)") &
    "Summary of NetCDF dimensions: ","NAME","LENGTH"
  write(unit=LU_LOG,FMT=*) repeat("-",50)
  flush(unit=LU_LOG)

  do i=1,nDimensions

    call netcdf_check(nf90_inquire_dimension(pNC%iNCID, i, sDimName(i), &
      iDimLen(i)),__FILE__,__LINE__)
    write(unit=LU_LOG,FMT="(i3,') ',a25,i8)") i, sDimName(i), iDimLen(i)
    if(TRIM(sDimName(i)) == "x" .or. TRIM(sDimName(i)) == "easting") then
      iXDim = i
      pNC%iX_NumGridCells = iDimLen(i)
    elseif (TRIM(sDimName(i)) == "y" .or. TRIM(sDimName(i)) == "northing") then
      iYDim = i
      pNC%iY_NumGridCells = iDimLen(i)
    elseif(TRIM(sDimName(i)) == "time") then
      iTDim = i
    endif

  enddo

  call Assert(LOGICAL(iXDim > 0,kind=T_LOGICAL), &
     "'x' dimension not found in NetCDF file", &
     TRIM(__FILE__),__LINE__)

  call Assert(LOGICAL(iYDim > 0,kind=T_LOGICAL), &
     "'y' dimension not found in NetCDF file",&
     TRIM(__FILE__),__LINE__)

  call Assert(LOGICAL(iTDim > 0,kind=T_LOGICAL), &
     "'time' dimension not found in NetCDF file", &
     TRIM(__FILE__),__LINE__)

  allocate(sVarName(nVariables),stat=iStat)
   call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
     "Problem allocating memory for variable", &
     TRIM(__FILE__),__LINE__)

  allocate(iVarDim(nVariables),stat=iStat)
   call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
     "Problem allocating memory for variable", &
     TRIM(__FILE__),__LINE__)

  allocate(rXCoord(iDimLen(iXDim)),stat=iStat)
   call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
     "Problem allocating memory for variable", &
     TRIM(__FILE__),__LINE__)

  allocate(rYCoord(iDimLen(iYDim)),stat=iStat)
   call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
     "Problem allocating memory for variable", &
     TRIM(__FILE__),__LINE__)

  allocate(iValue(iDimLen(iXDim),iDimLen(iYDim)),stat=iStat)
   call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
     "Problem allocating memory for variable", &
     TRIM(__FILE__),__LINE__)

  allocate(iVarNumAttribs(nVariables),stat=iStat)
   call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
     "Problem allocating memory for variable", &
     TRIM(__FILE__),__LINE__)

  allocate(sAttribName(0:nVariables,MAX_ATTRIBUTES),stat=iStat)
   call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
     "Problem allocating memory for variable", &
     TRIM(__FILE__),__LINE__)

  sAttribName = ""

  allocate(iAttribType(0:nVariables,MAX_ATTRIBUTES),stat=iStat)
   call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
     "Problem allocating memory for variable", &
     TRIM(__FILE__),__LINE__)

  allocate(iVarType(nVariables),stat=iStat)
   call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
     "Problem allocating memory for variable", &
     TRIM(__FILE__),__LINE__)

  allocate(sAttribValue(0:nVariables,MAX_ATTRIBUTES),stat=iStat)
   call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
     "Problem allocating memory for variable", &
     TRIM(__FILE__),__LINE__)

  sAttribValue = ""

  allocate(rAttribValue(0:nVariables,MAX_ATTRIBUTES),stat=iStat)
   call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
     "Problem allocating memory for variable", &
     TRIM(__FILE__),__LINE__)

  allocate(iVarDimID(nVariables,MAX_DIMENSIONS),stat=iStat)
   call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
     "Problem allocating memory for variable", &
     TRIM(__FILE__),__LINE__)

  write(unit=LU_LOG,FMT="(/,a,/,t9,a,t32,a,t45,a)") "Summary of variables contained in NetCDF file",&
    "NAME","DIMENSIONS","TYPE"
  write(unit=LU_LOG,FMT=*) repeat("-",58)

  do i=1,nVariables

    call netcdf_check(nf90_inquire_variable(pNC%iNCID, i, name=sVarName(i), &
       ndims=iVarDim(i), dimids=iVarDimID(i,:),natts=iVarNumAttribs(i), &
       xtype = iVarType(i)), &
       __FILE__,__LINE__)

    select case(iVarDim(i))

      ! depending on how many dimensions this variable is related to,
      ! print out the names of those dimensions
      case(0)
        write(unit=LU_LOG,FMT="(i3,') ',a,t15,'type: ',i3)") &
           i, TRIM(sVarName(i)),iVarType(i)
      case (1)
        write(unit=LU_LOG,FMT="(i3,') ',a,'(',a,')',t30,'type: ',i3)") &
           i, TRIM(sVarName(i)),&
           (TRIM(sDimName( iVarDimID(i,j) ) ),j=1,iVarDim(i) ),iVarType(i)
      case (2)
        write(unit=LU_LOG,FMT="(i3,') ',a,'(',a,',',a,')',t30'type: ',i3)") &
           i, TRIM(sVarName(i)),&
           (TRIM(sDimName( iVarDimID(i,j) ) ),j=1,iVarDim(i) ),iVarType(i)
      case (3)
        write(unit=LU_LOG,&
          FMT="(i3,') ',a,'(',a,',',a,',',a,')',t30,'type: ',i3)") &
           i, TRIM(sVarName(i)), &
           (TRIM(sDimName( iVarDimID(i,j) ) ),j=1,iVarDim(i) ),iVarType(i)
      case default

        ! Die if we have greater than 3 dimensions assigned to this variable
        call Assert(lFALSE,"Dimensions > 3 are unsupported; iVarDim = "// &
           TRIM(int2char(iVarDim(i))), &
           TRIM(__FILE__), __LINE__)

    end select

    ! sVariableName is supplied by the calling routine
    if(TRIM(sVarName(i))==TRIM(sVariableName)) iZVar = i
      pNC%iVarID = iZVar

     do j=1,iVarNumAttribs(i)
       call netcdf_check(nf90_inq_attname(pNC%iNCID, i, j,sAttribName(i,j)), &
         TRIM(__FILE__),__LINE__)
       call netcdf_check(nf90_inquire_attribute(pNC%iNCID, i, &
         TRIM(sAttribName(i,j)), xtype=iAttribType(i,j)), &
         TRIM(__FILE__),__LINE__)
       if(iAttribType(i,j)==2) then
         call netcdf_check(nf90_get_att(pNC%iNCID,i,TRIM(sAttribName(i,j)),&
           sAttribValue(i,j)),TRIM(__FILE__),__LINE__)
         write(unit=LU_LOG,fmt="(t8,'==> 'a,t35,a)") &
           TRIM(sAttribName(i,j)),TRIM(sAttribValue(i,j))
       else
         call netcdf_check(nf90_get_att(pNC%iNCID,i,TRIM(sAttribName(i,j)),&
         rAttribValue(i,j)),TRIM(__FILE__),__LINE__)
         write(unit=LU_LOG,fmt="(t8,'==> 'a,t35,f14.3)") &
           TRIM(sAttribName(i,j)),rAttribValue(i,j)
       endif
     enddo  ! loop over attributes for this particular variable
     write(unit=LU_LOG,fmt="(/)")

  enddo  ! loop over all variables

  ! terminate if this variable is not present in the NetCDF file
  call Assert(LOGICAL(iZVar > 0,kind=T_LOGICAL), &
    "variable name "//TRIM(sVariableName)//" not found in NetCDF file", &
    TRIM(__FILE__),__LINE__)


  call netcdf_check(nf90_get_att(pNC%iNCID,iZVar,"scale_factor",&
     pNC%rScaleFactor), &
     TRIM(__FILE__),__LINE__)

  call netcdf_check(nf90_get_att(pNC%iNCID,iZVar,"add_offset",&
     pNC%rAddOffset), &
     TRIM(__FILE__),__LINE__)

  call netcdf_check(nf90_inq_varid(pNC%iNCID, "time", iTVar),&
     TRIM(__FILE__),__LINE__)

  call netcdf_check(nf90_get_att(pNC%iNCID,iTVar,"start_day",&
     rStartDay), &
     TRIM(__FILE__),__LINE__)

  call netcdf_check(nf90_get_att(pNC%iNCID,iTVar,"end_day",&
     rEndDay), &
     TRIM(__FILE__),__LINE__)

  call netcdf_check(nf90_get_att(pNC%iNCID,iTVar,"units",sUnitsString), &
     TRIM(__FILE__),__LINE__)


!  call netcdf_check(nf90_get_var(iNCID, iTVar, rTimeValues(iDimLen(iTDim):iDimLen(iTDim)), &
!    start=(/iDimLen(iTDim) - 1/),count=(/1/)), &
!    TRIM(__FILE__),__LINE__)
!
!  write(LU_LOG,FMT="('First value in variable time: ',F11.2)") rTimeValues(1)
!  write(LU_LOG,FMT="('Last value in variable time: ',F11.2)") &
!    rTimeValues(iDimLen(iTDim))

!  call netcdf_check(nf90_get_var(ncid=iNCID, varid=iTVar, &
!    values=iTimeValues), &
!      TRIM(__FILE__),__LINE__)

!  call netcdf_check(nf90_get_var(iNCID, iTVar,rTimeValues), &
!      TRIM(__FILE__),__LINE__)

!  expecting to find units something like:
!     "days since 1960-01-01 00:00:00"

  call Chomp(sUnitsString, sTextFragment)
  call Chomp(sUnitsString, sTextFragment)
  call Chomp(sUnitsString, sTextFragment)
  read(sTextFragment,FMT="(i4,1x,i2,1x,i2)", iostat=iStat) &
    pNC%iOriginYear, pNC%iOriginMonth, pNC%iOriginDay

  call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
    "Problem reading starting date from NC file",&
    TRIM(__FILE__),__LINE__)

  pNC%iOriginJulianDay = &
    julian_day ( pNC%iOriginYear, pNC%iOriginMonth, pNC%iOriginDay)

  pNC%iStartJulianDay = pNC%iOriginJulianDay + rStartDay

  pNC%iEndJulianDay = pNC%iOriginJulianDay + rEndDay

  ! determine origin, start, and end date of NC time-series data
  write(unit=LU_LOG,FMT="('  origin month = ',t25,i5)") &
    pNC%iOriginMonth
  write(unit=LU_LOG,FMT="('  origin day = ',t25,i5)") &
    pNC%iOriginDay
  write(unit=LU_LOG,FMT="('  origin year = ',t25,i5,/)") &
    pNC%iOriginYear
  write(unit=LU_LOG,FMT="('  origin Julian day = ',t25,i14,/,/)") &
    pNC%iOriginJulianDay

  call gregorian_date(pNC%iStartJulianDay, iYear, iMonth, iDay)

    write(unit=LU_LOG,FMT="('  start month = ',t25,i5)") &
    iMonth
  write(unit=LU_LOG,FMT="('  start day = ',t25,i5)") &
    iDay
  write(unit=LU_LOG,FMT="('  start year = ',t25,i5,/)") &
    iYear
  write(unit=LU_LOG,FMT="('  start Julian day = ',t25,i14,/,/)") &
    pNC%iStartJulianDay

  call gregorian_date(pNC%iEndJulianDay, &
       iYear, iMonth, iDay)


  write(unit=LU_LOG,FMT="('  end month = ',t25,i5)") &
    iMonth
  write(unit=LU_LOG,FMT="('  end day = ',t25,i5)") &
    iDay
  write(unit=LU_LOG,FMT="('  end year = ',t25,i5,/)") &
    iYear
  write(unit=LU_LOG,FMT="('  end Julian day = ',t25,i14,/,/)") &
    pNC%iEndJulianDay

  flush(unit=LU_LOG)

  ! list GLOBAL attributes
  write(unit=LU_LOG,fmt="(/,'GLOBAL ATTRIBUTES',/)")
  flush(unit=LU_LOG)

   do j=1,nAttributes
     call netcdf_check(nf90_inq_attname(pNC%iNCID, NF90_GLOBAL, j,sAttribName(0,j)), &
       TRIM(__FILE__),__LINE__)
     call netcdf_check(nf90_inquire_attribute(pNC%iNCID,NF90_GLOBAL,&
        TRIM(sAttribName(0,j)), &
       xtype=iAttribType(0,j)),__FILE__,__LINE__)
     if(iAttribType(0,j)==2) then
       call netcdf_check(nf90_get_att(pNC%iNCID,NF90_GLOBAL,TRIM(sAttribName(0,j)),&
         sAttribValue(0,j)),TRIM(__FILE__),__LINE__)
     else
       call netcdf_check(nf90_get_att(pNC%iNCID,NF90_GLOBAL,TRIM(sAttribName(0,j)),&
         rAttribValue(0,j)),TRIM(__FILE__),__LINE__)
       write(unit=LU_LOG,fmt="(t8,'==> 'a,t35,f14.3)") &
         TRIM(sAttribName(0,j)),rAttribValue(0,j)
       flush(unit=LU_LOG)
     endif
   enddo

  call netcdf_check(nf90_get_var(pNC%iNCID, iZVar, iValue, &
    start= (/1,1,4/), &
    count= (/ iDimLen(iVarDimID(iZVar,1)), iDimLen(iVarDimID(iZVar,2)),1/) ), &
    TRIM(__FILE__),__LINE__)

  call netcdf_check(nf90_get_var(pNC%iNCID, iXDim, rXCoord ), &
    TRIM(__FILE__),__LINE__)

  call netcdf_check(nf90_get_var(pNC%iNCID, iYDim, rYCoord ), &
    TRIM(__FILE__),__LINE__)

  do i=1,size(pNC%rXCoord)
    print *, "X: ", i, pNC%rXCoord
  enddo

  do i=1,size(pNC%rYCoord)
    print *, "Y: ",i, pNC%rYCoord
  enddo

  write(unit=LU_LOG,FMT="(/,a)") 'Grid point coordinates'
  write(unit=LU_LOG,FMT="('min X: ', f14.3,'  max X: ',f14.3)") &
      MINVAL(rXCoord), MAXVAL(rXCoord)
  write(unit=LU_LOG,FMT="('min Y: ', f14.3,'  max Y: ',f14.3)") &
      MINVAL(rYCoord), MAXVAL(rYCoord)

  rCellSize = ABS(rXCoord(1) - rXCoord(iDimLen(iVarDimID(iZVar,1)))) &
       /(iDimLen(iVarDimID(iZVar,1))-1)

  pNC%rGridCellSize = rCellSize

  write(unit=LU_LOG,FMT="('cell size: ', f11.3)") rCellSize

  write(unit=LU_LOG,FMT="(/,a)") 'Extent of grid domain'
  write(unit=LU_LOG,FMT="('min X: ', f14.3,'  max X: ',f14.3)") &
      MINVAL(rXCoord)-rCellSize/2., MAXVAL(rXCoord)+rCellSize/2.
  write(unit=LU_LOG,FMT="('min Y: ', f14.3,'  max Y: ',f14.3)") &
      MINVAL(rYCoord)-rCellSize/2., MAXVAL(rYCoord)+rCellSize/2.

  ! we're assuming that the coordinates are ordered from lower to higher
  ! values increase as row num and col num are increased

  pNC%rX_LowerLeft = rXCoord(1) - rCellSize/2.
  pNC%rY_LowerLeft = rYCoord(1) - rCellSize/2.
  pNC%rX_UpperRight = &
     rXCoord(iDimLen(iVarDimID(iZVar,1))) + rCellSize/2.
  pNC%rY_UpperRight = &
     rYCoord(iDimLen(iVarDimID(iZVar,2))) + rCellSize/2.

!  write(unit=LU_LOG,FMT="('[',i3,',',i3,']: ',i6)") 1,1,iValue(1,1)
!  write(unit=LU_LOG,FMT="('[',i3,',',i3,']: ',i6)") iDimLen(iVarDimID(iZVar,1)),&
!    1,iValue(iDimLen(iVarDimID(iZVar,1)),1)

!  print *, iDimLen(iVarDimID(iZVar,1))  ! 316
!  print *, iDimLen(iVarDimID(iZVar,2))  ! 410
!  write(unit=LU_LOG,FMT="('[',i3,',',i3,']: ',i6)") &
!    1, &
!    iDimLen(iVarDimID(iZVar,2)),&
!    iValue(1, iDimLen(iVarDimID(iZVar,2) ) )

!  write(unit=LU_LOG,FMT="('[',i3,',',i3,']: ',i6)") &
!    iDimLen(iVarDimID(iZVar,1)), &
!    iDimLen(iVarDimID(iZVar,2)),&
!    iValue(iDimLen(iVarDimID(iZVar,1)), iDimLen(iVarDimID(iZVar,2) ) )

  deallocate(sDimName, STAT=iStat)
  call Assert( iStat == 0, &
     "Could not deallocate memory for sDimName", &
     TRIM(__FILE__),__LINE__)
  deallocate(iDimLen, STAT=iStat)
  call Assert( iStat == 0, &
     "Could not deallocate memory for iDimLen", &
     TRIM(__FILE__),__LINE__)
  deallocate(sVarName, STAT=iStat)
  call Assert( iStat == 0, &
     "Could not deallocate memory for sVarName", &
     TRIM(__FILE__),__LINE__)
  deallocate(iVarDim, STAT=iStat)
  call Assert( iStat == 0, &
     "Could not deallocate memory for iVarName", &
     TRIM(__FILE__),__LINE__)
  deallocate(rXCoord, STAT=iStat)
  call Assert( iStat == 0, &
     "Could not deallocate memory for rXCoord", &
     TRIM(__FILE__),__LINE__)
  deallocate(rYCoord, STAT=iStat)
  call Assert( iStat == 0, &
     "Could not deallocate memory for rYCoord", &
     TRIM(__FILE__),__LINE__)
  deallocate(iValue, STAT=iStat)
  call Assert( iStat == 0, &
     "Could not deallocate memory for iValue", &
     TRIM(__FILE__),__LINE__)
  deallocate(iVarNumAttribs, STAT=iStat)
  call Assert( iStat == 0, &
     "Could not deallocate memory for iVarNumAttribs", &
     TRIM(__FILE__),__LINE__)
  deallocate(sAttribName, STAt=iStat)
  call Assert( iStat == 0, &
     "Could not deallocate memory for sAttribName", &
     TRIM(__FILE__),__LINE__)
  deallocate(iAttribType, STAT=iStat)
  call Assert( iStat == 0, &
     "Could not deallocate memory for iAttribType", &
     TRIM(__FILE__),__LINE__)
  deallocate(sAttribValue, STAT=iStat)
  call Assert( iStat == 0, &
     "Could not deallocate memory for sAttribValue", &
     TRIM(__FILE__),__LINE__)
  deallocate(rAttribValue, STAT=iStat)
  call Assert( iStat == 0, &
     "Could not deallocate memory for rAttribValue", &
     TRIM(__FILE__),__LINE__)
  deallocate(iVarDimID, STAT=iStat)
  call Assert( iStat == 0, &
     "Could not deallocate memory for iVarDimID", &
     TRIM(__FILE__),__LINE__)

  end subroutine netcdf_info

!----------------------------------------------------------------------

subroutine netcdf_chk_extent(pConfig, iVarNum, iMode, pGrd)

!subroutine netcdf_chk_extent( iNCID, iVarNum, pConfig, pGrd)

  type (T_MODEL_CONFIGURATION), pointer :: pConfig  ! pointer to data structure that contains
                                                    ! model options, flags, and other settings
  integer (kind=T_INT) :: iVarNum    ! corresponds to T_STATS parameters
  integer (kind=T_INT) :: iMode      ! iINPUT, iOUTPUT, or iOBSERVATION
  type ( T_GENERAL_GRID ),pointer :: pGrd    ! pointer to model grid

  ! [LOCALS]
  type (T_NETCDF_FILE), pointer :: pNC
  logical(kind=T_LOGICAL) :: congruent, larger_extent

  pNC => pConfig%NETCDF_FILE(iVarNum, iMode)

  write(unit=LU_LOG,FMT="(/,'COMPARISON of Grid Extents and cell size for variable: ',a)") &
    TRIM(pNC%sVarName)
  write(unit=LU_LOG,FMT="(/,t13,'NetCDF',t30,'Control File')")
  write(unit=LU_LOG,FMT="(t10,'--------------',t28,'---------------')")

  ! test to see whether all descriptors of the grid extent are approximately equal
  congruent = approx_equal(pNC%rX_LowerLeft, pGrd%rX0) &
     .and. approx_equal(pNC%rY_LowerLeft, pGrd%rY0) &
     .and. approx_equal(pNC%rX_UpperRight, pGrd%rX1) &
     .and. approx_equal(pNC%rY_UpperRight, pGrd%rY1) &
     .and. approx_equal(pNC%rGridCellSize, pGrd%rGridCellSize)

  ! test to see if the NetCDF boundaries cover the complete extent of the model grid
  larger_extent = LOGICAL(pNC%rX_LowerLeft <= pGrd%rX0,kind=T_LOGICAL) &
       .and. LOGICAL(pNC%rY_LowerLeft <= pGrd%rY0,kind=T_LOGICAL) &
       .and. LOGICAL(pNC%rX_UpperRight >= pGrd%rX1,kind=T_LOGICAL) &
       .and. LOGICAL(pNC%rY_UpperRight >= pGrd%rY1,kind=T_LOGICAL)

  write(unit=LU_LOG,FMT="(a6,f14.3,'    ',f14.3)") "x0:", &
      pNC%rX_LowerLeft, pGrd%rX0
  write(unit=LU_LOG,FMT="(a6,f14.3,'    ',f14.3)") "y0:", &
    pNC%rY_LowerLeft, pGrd%rY0
  write(unit=LU_LOG,FMT="(a6,f14.3,'    ',f14.3)") "x1:", &
      pNC%rX_UpperRight, pGrd%rX1
  write(unit=LU_LOG,FMT="(a6,f14.3,'    ',f14.3)") "y1:", &
    pNC%rY_UpperRight, pGrd%rY1
  write(unit=LU_LOG,FMT="(/,a6,f14.3,'    ',f14.3,/)") "cell:", &
    pNC%rGridCellSize, pGrd%rGridCellSize

  if(congruent) then
    pNC%lInterpolate=lFALSE
    write(unit=LU_LOG,FMT="(//,1x,a,/)") &
      "Grids are approximately equal. No interpolation will be required"
  else if(larger_extent) then
    pNC%lInterpolate=lTRUE
    write(unit=LU_LOG,FMT="(//,1x,a,/)") &
      "NetCDF area is greater than model domain. Interpolation will be used to assign values to model grids."
  else
    call Assert(lFALSE,"NetCDF file does not completely cover the model grid extent.", &
      TRIM(__FILE__),__LINE__)
  end if

  return

end subroutine netcdf_chk_extent

!----------------------------------------------------------------------

  subroutine netcdf_read(iVarNum, iMode, pConfig, pGrd, pDataGrd, iJulianDay)

    integer(kind=T_INT) :: iVarNum             ! Variable number in internal data struct
    integer (kind=T_INT) :: iMode              ! iINPUT, iOUTPUT, or iOBSERVED
    type ( T_GENERAL_GRID ),pointer :: pGrd    ! pointer to model grid
    type ( T_GENERAL_GRID ),pointer :: pDataGrd ! pointer to OUTPUT grid (MUST ALREADY EXIST!!)
    type (T_MODEL_CONFIGURATION), pointer :: pConfig  ! pointer to data structure that contains
                                                      ! model options, flags, and other settings
    integer(kind=T_INT) :: iJulianDay
    integer(kind=T_INT) :: iScaleFactor        ! factor by which NetCDF grid
                                               ! cell size varies from model
                                               ! grid cell size

    ! [ LOCALS ]
    type ( T_GENERAL_GRID ),pointer :: pGrd_nc  ! pointer to NetCDF grid
    integer (kind=T_INT) :: iTime, iCol, iRow, i0, i1, j0, j1, i2, j2, i3, j3
    type (T_NETCDF_FILE), pointer :: pNC
    real(kind=T_SGL), allocatable, dimension(:,:) :: rValues
    real (kind=T_SGL) :: rXval, rYval
    integer(kind=FourByteInt), allocatable, dimension(:,:) :: iValues
    integer (kind=T_INT) :: iStat

    pNC => pConfig%NETCDF_FILE(iVarNum, iMode)

!    call netcdf_check( nf90_open(TRIM(nc%sFilename), nf90_nowrite, iNCID) )

    call Assert(LOGICAL(pConfig%iStartJulianDay >= pNC%iStartJulianDay, kind=T_LOGICAL), &
      "Simulation begins before first date in data file: "//TRIM(pNC%sFileName), &
      TRIM(__FILE__),__LINE__)

    call Assert(LOGICAL(pConfig%iEndJulianDay <= pNC%iEndJulianDay, kind=T_LOGICAL), &
      "Data file ends before end of simulation: "//TRIM(pNC%sFileName), &
      TRIM(__FILE__),__LINE__)


    pNC%rGridCellSize = (pNC%rX_UpperRight - pNC%rX_LowerLeft) / &
                                                 pNC%iX_NumGridCells

    iScaleFactor = INT(pNC%rGridCellSize / pGrd%rGridCellSize)

    allocate(rValues(pNC%iX_NumGridCells, pNC%iY_NumGridCells),STAT=iStat)
    call Assert( iStat == 0, &
     "Could not allocate memory for rValues", &
     TRIM(__FILE__),__LINE__)

    allocate(iValues(pNC%iX_NumGridCells, pNC%iY_NumGridCells),STAT=iStat)
    call Assert( iStat == 0, &
     "Could not allocate memory for iValues", &
     TRIM(__FILE__),__LINE__)

    rValues = -9999.
    iValues = -9999

    iTime = iJulianDay - pNC%iStartJulianDay + 1

    call netcdf_check(nf90_get_var(pNC%iNCID, pNC%iVarID, iValues, &
      start= (/1,1,iTime/), &
      count= (/ pNC%iX_NumGridCells, &
                pNC%iY_NumGridCells,1/) ), &
                TRIM(__FILE__),__LINE__, pNC, iTime)

#ifdef DEBUG_PRINT
    write(*,FMT="(a)") repeat('-',30)
    write(*,FMT="(a)") 'netcdf_support - before scaling'
    write(*,FMT="(' ncID:',i3,' varID:',i3,' ncTime:',i6,' iX:',i5," &
      //"' iY:',i5,'  min value:',i10,'  max value:',i10)") &
    pNC%iNCID, pNC%iVarID, iTime, pNC%iX_NumGridCells, &
      pNC%iY_NumGridCells, MINVAL(iValues),MAXVAL(iValues)
#endif

    rValues = REAL(iValues, kind=T_SGL) * pNC%rScaleFactor + pNC%rAddOffset

#ifdef DEBUG_PRINT
    write(*,FMT="(a)") 'netcdf_support - after scaling'
    write(*,FMT="(' ncID:',i3,' varID:',i3,' ncTime:',i6,' iX:',i5," &
      //"' iY:',i5,'  min value:',f14.4,'  max value:',f14.4)") &
     pNC%iNCID, pNC%iVarID, iTime, pNC%iX_NumGridCells, &
      pNC%iY_NumGridCells, MINVAL(rValues),MAXVAL(rValues)
#endif

!    pOutGrd => grid_Create ( pGrd%iNX,  pGrd%iNY, &
!          pGrd%rX0, pGrd%rY0, pGrd%rX1,pGrd%rY1,T_SGL_GRID )

    call assert(grid_conform(pGrd, pDataGrd), &
      "Internal error - data grid does not conform to project grid", &
      trim(__FILE__), __LINE__)

    if(pNC%lInterpolate) then

      if(iScaleFactor==2) then
        do iRow=1,pNC%iY_NumGridCells
          do iCol=1,pNC%iX_NumGridCells
            j0 = pDataGrd%iNY - (iRow-1)*2
            i0 = 1 + (iCol-1)*2
            j1 = j0 - 1
            i1 = i0 + 1
            pDataGrd%rData(j0,i0) = rValues(iCol,iRow)
            pDataGrd%rData(j0,i1) = rValues(iCol,iRow)
            pDataGrd%rData(j1,i0) = rValues(iCol,iRow)
            pDataGrd%rData(j1,i1) = rValues(iCol,iRow)
          end do
        end do
      else if(iScaleFactor==4) then
        do iRow=1,pNC%iY_NumGridCells
          do iCol=1,pNC%iX_NumGridCells
            j0 = pDataGrd%iNY - (iRow-1)*4
            i0 = 1 + (iCol-1)*4
            j1 = j0 - 1
            i1 = i0 + 1
            j2 = j1 - 1
            i2 = i1 + 1
            j3 = j2 - 1
            i3 = i2 + 1
            pDataGrd%rData(j0,i0) = rValues(iCol,iRow)
            pDataGrd%rData(j0,i1) = rValues(iCol,iRow)
            pDataGrd%rData(j0,i2) = rValues(iCol,iRow)
            pDataGrd%rData(j0,i3) = rValues(iCol,iRow)
            pDataGrd%rData(j1,i0) = rValues(iCol,iRow)
            pDataGrd%rData(j1,i1) = rValues(iCol,iRow)
            pDataGrd%rData(j1,i2) = rValues(iCol,iRow)
            pDataGrd%rData(j1,i3) = rValues(iCol,iRow)
            pDataGrd%rData(j2,i0) = rValues(iCol,iRow)
            pDataGrd%rData(j2,i1) = rValues(iCol,iRow)
            pDataGrd%rData(j2,i2) = rValues(iCol,iRow)
            pDataGrd%rData(j2,i3) = rValues(iCol,iRow)
            pDataGrd%rData(j3,i0) = rValues(iCol,iRow)
            pDataGrd%rData(j3,i1) = rValues(iCol,iRow)
            pDataGrd%rData(j3,i2) = rValues(iCol,iRow)
            pDataGrd%rData(j3,i3) = rValues(iCol,iRow)
          end do
        end do
      else
        print *
        print *, "   NetCDF grid cell size: "//real2char(pGrd_nc%rGridCellSize)
        print *, "   Model grid cell size: "//real2char(pGrd%rGridCellSize)
        call Assert(lFALSE,"NetCDF grid cell size must be a factor of" &
          //" the model domain grid cell size",TRIM(__FILE__),__LINE__)
      end if

!      do iCol=1,pDataGrd%iNX
!        rXval = grid_GetGridX(pGrd,iCol)
!        do iRow=1,pDataGrd%iNY
!          rYval = grid_GetGridY(pGrd,iRow)
!          pDataGrd%rData(iRow,iCol) = grid_Interpolate(pGrd_nc,rXval,rYval)
!        end do
!      end do


    else  ! no interpolation needed
      do iRow=1,pDataGrd%iNY
        do iCol=1,pDataGrd%iNX
          pDataGrd%rData(iCol, (pDataGrd%iNY - iRow + 1)) = rValues(iCol,iRow)
        end do
      end do
    end if

    deallocate(rValues)

    return

  end subroutine netcdf_read

!----------------------------------------------------------------------

  subroutine netcdf_write_attributes(iVarNum, iMode, pConfig, pGrd)

    integer(kind=T_INT) :: iVarNum             ! Variable number in internal data struct
    integer (kind=T_INT) :: iMode              ! iINPUT, iOUTPUT, or iOBSERVED
    type (T_GENERAL_GRID),pointer :: pGrd      ! Grid of model cells
    type (T_MODEL_CONFIGURATION), pointer :: pConfig  ! pointer to data structure that contains
                                                      ! model options, flags, and other settings
    ! [LOCAL VARIABLES ]
    type (T_NETCDF_FILE), pointer :: pNC
    character(len=256) :: sDateStr
    character(len=256) :: sDateStrPretty
    real (kind=T_DBL) :: rLat, rLon
    real (kind=T_DBL) :: rX, rY
    integer (kind=T_INT) :: iCol,iRow,k
    integer (kind=T_INT) :: iDay, iMonth, iYear
    character(len=24) :: sStartDate
    real (kind=T_SGL) :: rMultFactor

    pNC => pConfig%NETCDF_FILE(iVarNum, iMode)

    call GetSysTimeDate(sDateStr,sDateStrPretty)
    call gregorian_date(pConfig%iStartJulianDay, iYear, iMonth, iDay)
    write(sStartDate,FMT="(i4,'-',i2.2,'-',i2.2,' 00:00:00')") &
        iYear, iMonth, iDay

    call netcdf_check(nf90_put_att(pNC%iNCID, NF90_GLOBAL, "title", &
      "SWB output for "//pNC%sVarName),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_put_att(pNC%iNCID, NF90_GLOBAL, "institution", &
      "institution"),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_put_att(pNC%iNCID, NF90_GLOBAL, "conventions", &
      "CF-1.4"),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_put_att(pNC%iNCID, NF90_GLOBAL, "history", &
      "Created on "//TRIM(sDateStrPretty)),TRIM(__FILE__),__LINE__)

    ! establish NetCDF DIMENSIONS
    call netcdf_check(nf90_def_dim(pNC%iNCID, "y", pGrd%iNY, &
        pNC%iYDimID),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_def_dim(pNC%iNCID, "x", pGrd%iNX, &
        pNC%iXDimID),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_def_dim(pNC%iNCID, "time", NF90_UNLIMITED, &
       pNC%iTimeDimID),TRIM(__FILE__),__LINE__)

    ! DEFINE PROJECTED COORDINATES
    ! define y VARIABLE
!    call netcdf_check(nf90_def_var(pNC%iNCID, "y", NF90_DOUBLE, &
!      (/pNC%iXDimID, pNC%iYDimID/),pNC%iYVarID),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_def_var(pNC%iNCID, "y", NF90_DOUBLE, &
      (/pNC%iYDimID/),pNC%iYVarID),TRIM(__FILE__),__LINE__)
   ! add attributes to variable "y"
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iYVarID, &
       "units", "meters"),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iYVarID, &
       "axis", "y"),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iYVarID, &
       "standard_name", "projection_y_coordinate"),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iYVarID, &
       "long_name", "projected y coordinate values"),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iYVarID, &
       "grid_spacing", pGrd%rGridCellSize),TRIM(__FILE__),__LINE__)


    ! define x VARIABLE
!    call netcdf_check(nf90_def_var(pNC%iNCID, "x", NF90_DOUBLE, &
!      (/pNC%iXDimID, pNC%iYDimID/),pNC%iXVarID),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_def_var(pNC%iNCID, "x", NF90_DOUBLE, &
      (/pNC%iXDimID/),pNC%iXVarID),TRIM(__FILE__),__LINE__)
   ! add attributes to variable "x"
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iXVarID, &
       "units", "meters"),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iXVarID, &
       "axis", "x"),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iXVarID, &
       "standard_name", "projection_x_coordinate"),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iXVarID, &
       "long_name", "projected x coordinate values"),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iXVarID, &
       "grid_spacing", pGrd%rGridCellSize),TRIM(__FILE__),__LINE__)


    ! define LATITUDE and LONGITUDE variables
    call netcdf_check(nf90_def_var(pNC%iNCID, "lat", NF90_DOUBLE, &
      (/pNC%iXDimID, pNC%iYDimID/),pNC%iLatVarID),TRIM(__FILE__),__LINE__)
    ! add attributes to variable "lat"
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iLatVarID, &
       "units", "degrees_north"))
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iLatVarID, &
       "standard_name", "latitude"))
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iLatVarID, &
       "long_name", "latitude"))


    call netcdf_check(nf90_def_var(pNC%iNCID, "lon", NF90_DOUBLE, &
      (/pNC%iXDimID, pNC%iYDimID/),pNC%iLonVarID),TRIM(__FILE__),__LINE__)
    ! add attributes to variable "lon"
   call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iLonVarID, &
       "units", "degrees_east"))
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iLonVarID, &
       "standard_name", "longitude"))
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iLonVarID, &
       "long_name", "longitude"))


    ! define variable TIME
    call netcdf_check(nf90_def_var(pNC%iNCID, "time", NF90_INT, &
      pNC%iTimeDimID,pNC%iTimeVarID),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iTimeVarID, &
       "units", "days since "//TRIM(sStartDate)),&
     TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iTimeVarID, &
       "calendar", "gregorian"),&
     TRIM(__FILE__),__LINE__)


    ! define PROJECTION variable
    call netcdf_check(nf90_def_var(pNC%iNCID, TRIM(pNC%sProjectionName), &
        NF90_INT, pNC%iProjID),TRIM(__FILE__),__LINE__)
   ! add attributes to variable "wtm" (or UTM16 etc.)
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iProjID, &
       "units", "meters"),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iProjID, &
       "grid_mapping_name", "transverse_mercator"),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iProjID, &
       "scale_factor_at_central_meridian", 0.9996_T_DBL),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iProjID, &
       "longitude_of_central_meridian", pNC%rLongOrigin),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iProjID, &
       "latitude_of_projection_origin", pNC%rLatOrigin),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iProjID, &
       "false_easting", pNC%rFalseEasting),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iProjID, &
       "false_northing", pNC%rFalseNorthing),TRIM(__FILE__),__LINE__)

    ! define "CRS" variable
    call netcdf_check(nf90_def_var(pNC%iNCID, "crs", &
        NF90_INT, pNC%iCRSID),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iCRSID, &
       "units", "decimal_degrees"))
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iCRSID, &
       "grid_mapping_name", "latitude_longitude"),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iCRSID, &
       "longitude_of_prime_meridian", 0.0_T_DBL),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iCRSID, &
       "semi_major_axis", 6378137.0_T_DBL ),TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iCRSID, &
       "inverse_flattening", 298.257223563_T_DBL ),TRIM(__FILE__),__LINE__)

    ! define OUTPUT VARIABLE as function of X, Y and TIME
!    call netcdf_check(nf90_def_var(pNC%iNCID, pNC%sVarName, NF90_SHORT, &
!      (/pNC%iXDimID, pNC%iYDimID, pNC%iTimeDimID/), pNC%iVarID), &
!         TRIM(__FILE__),__LINE__)
    call netcdf_check(nf90_def_var(pNC%iNCID, pNC%sVarName, NF90_FLOAT, &
      (/pNC%iXDimID, pNC%iYDimID, pNC%iTimeDimID/), pNC%iVarID), &
         TRIM(__FILE__),__LINE__)


    ! add attributes to variable
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iVarID, &
       "standard_name", pNC%sVarName))
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iVarID, &
       "long_name", pNC%sVarName))
!    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iVarID, &
!       "coordinates", "y x"))
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iVarID, &
       "coordinates", "lat lon"))
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iVarID, &
       "units", pNC%sUnits))
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iVarID, &
       "add_offset", pNC%rAddOffset))
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iVarID, &
       "scale_factor", pNC%rScaleFactor))
    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iVarID, &
       "grid_mapping", TRIM(pNC%sProjectionName)))
!    call netcdf_check(nf90_put_att(pNC%iNCID, pNC%iVarID, &
!       "grid_mapping", "crs"))


    ! take NetCDF file out of DEFINE mode and enable writing of variables
    call netcdf_check(nf90_enddef(pNC%iNCID),TRIM(__FILE__),__LINE__)

!    ! compute and output x values
!    do iCol=1,pGrd%iNX
!      rX = grid_GetGridX(pGrd,iCol)
!      call netcdf_check(nf90_put_var(pNC%iNCID,pNC%iXVarID, &
!           values = rX, start = (/iCol/)))
!    end do
!
!    ! compute and output y values
!    do iRow=1,pGrd%iNY
!      k =  pGrd%iNY - iRow + 1
!      rY = grid_GetGridY(pGrd,iRow)
!      call netcdf_check(nf90_put_var(pNC%iNCID,pNC%iYVarID, &
!           values = rY, start = (/iRow/)))
!    end do

    ! write out the latitude and longitude at each UTM X and Y

    if(pGrd%iLengthUnits == iGRID_LENGTH_UNITS_FEET) then
      rMultFactor = 0.3048_T_SGL
    else
      rMultFactor = rONE
    end if

    do iCol=1,pGrd%iNX
      do iRow=1,pGrd%iNY
        k =  pGrd%iNY - iRow + 1
        rX = grid_GetGridX(pGrd,iCol) * rMultFactor
        rY = grid_GetGridY(pGrd,iRow) * rMultFactor
        call UTMtoLL(pConfig, rX, rY, rLat, rLon)
        if(iCol==1 .and. iRow==1 .or. iCol==pGrd%iNX .and. iRow==pGrd%iNY) then
           write(unit=LU_LOG,FMT="('grid coordinate (',i4,',',i4,'): ',2f14.3)") &
             iCol,iRow,rX, rY
           write(unit=LU_LOG,FMT="('lat / long      (',i4,',',i4,'): ',2f14.3)") &
             iCol,iRow,rLon, rLat
        end if
        call netcdf_check(nf90_put_var(pNC%iNCID,pNC%iLatVarID   , &
           values = rLat, start = (/iCol,iRow/)))
        call netcdf_check(nf90_put_var(pNC%iNCID,pNC%iLonVarID   , &
           values = rLon, start = (/iCol,iRow/)))
!        call netcdf_check(nf90_put_var(pNC%iNCID,pNC%iXVarID   , &
!           values = rX, start = (/iCol,iRow/)))
!        call netcdf_check(nf90_put_var(pNC%iNCID,pNC%iYVarID   , &
!           values = rY, start = (/iCol,iRow/)))
      end do
    end do

    do iCol=1,pGrd%iNX
      rX = grid_GetGridX(pGrd,iCol) * rMultFactor
      call netcdf_check(nf90_put_var(pNC%iNCID,pNC%iXVarID   , &
         values = rX, start = (/iCol/)))
    end do

    do iRow=1,pGrd%iNY
      rY = grid_GetGridY(pGrd,iRow) * rMultFactor
      call netcdf_check(nf90_put_var(pNC%iNCID,pNC%iYVarID   , &
         values = rY, start = (/iRow/)))
    end do


    return

  end subroutine netcdf_write_attributes

!----------------------------------------------------------------------

  subroutine netcdf_write_variable(iVarNum, iMode, pConfig, pGrd, iTime)

    integer(kind=T_INT) :: iVarNum             ! Variable number in internal data struct
    integer (kind=T_INT) :: iMode              ! iINPUT, iOUTPUT, or iOBSERVED
    type (T_GENERAL_GRID),pointer :: pGrd      ! Grid of model cells
    type (T_MODEL_CONFIGURATION), pointer :: pConfig  ! pointer to data structure that contains
                                                      ! model options, flags, and other settings
    integer (kind=T_INT) :: iTime              ! current timestep value

    ! [LOCAL VARIABLES ]
    type (T_NETCDF_FILE), pointer :: pNC
    real (kind=T_DBL) :: rX, rY
    integer (kind=T_INT) :: iCol,iRow, k

    pNC => pConfig%NETCDF_FILE(iVarNum, iMode)

    do iRow=1,pGrd%iNY
      do iCol=1,pGrd%iNX
        k =  pGrd%iNY - iRow + 1
        call netcdf_check(nf90_put_var(pNC%iNCID,pNC%iVarID   , &
           values = pGrd%rData(iCol,k), start = (/iCol,iRow, iTime/)), &
                TRIM(__FILE__),__LINE__, pNC, iTime)
      end do
    end do

    ! must also write the current value for the time variable
    call netcdf_check(nf90_put_var(pNC%iNCID,pNC%iTimeVarID   , &
        values = (/iTime - 1/), start = (/iTime/)), &
                TRIM(__FILE__),__LINE__, pNC, iTime)

    return

  end subroutine netcdf_write_variable

!----------------------------------------------------------------------

  subroutine netcdf_write_variable_byte(iVarNum, iMode, pConfig, &
   rValue, pGrd, iRow, iCol, iTime)

    integer(kind=T_INT) :: iVarNum             ! Variable number in internal data struct
    integer (kind=T_INT) :: iMode              ! iINPUT, iOUTPUT, or iOBSERVED
    real(kind=T_SGL) :: rValue
    type (T_MODEL_CONFIGURATION), pointer :: pConfig  ! pointer to data structure that contains
                                                      ! model options, flags, and other settings
    type (T_GENERAL_GRID), pointer :: pGrd
    integer(kind=T_INT) :: iRow,iCol
    integer (kind=T_INT) :: iTime              ! current timestep value

    ! [LOCAL VARIABLES ]
    type (T_NETCDF_FILE), pointer :: pNC
    real (kind=T_DBL) :: rX, rY
    integer (kind=T_INT) :: iRowInverted
!    integer (kind=T_INT) :: iValue


    pNC => pConfig%NETCDF_FILE(iVarNum, iMode)

    iRowInverted =  pGrd%iNY - iRow + 1

!    iValue = INT(rValue / pNC%rScaleFactor - pNC%rAddOffset,kind=T_INT)

    call netcdf_check(nf90_put_var(pNC%iNCID,pNC%iVarID   , &
       values = rValue, start = (/iCol, iRowInverted, iTime + 1 /)), &
                TRIM(__FILE__),__LINE__, pNC, iTime, rValue)

    if(iRow==1 .and. iCol==1) then     ! only write out time once
      ! must also write the current value for the time variable

      call netcdf_check(nf90_put_var(pNC%iNCID,pNC%iTimeVarID   , &
          values = (/int(iTime)/), start = (/int(iTime + 1)/)), &
                TRIM(__FILE__),__LINE__, pNC, iTime)
    end if


    return

  end subroutine netcdf_write_variable_byte

!----------------------------------------------------------------------

  subroutine netcdf_check(iStatus, sFilename, iLineNo, pNC, iTime, rValue)
    integer(kind=T_INT), intent ( in) :: iStatus
    character(len=*), optional :: sFilename
    character(len=256) :: sErrMessage, sTime, sValue
    integer (kind=T_INT), optional :: iLineNo
    type (T_NETCDF_FILE), optional, pointer :: pNC
    integer (kind=T_INT), optional :: iTime
    real(kind=T_SGL), optional :: rValue


    if(iStatus /= nf90_noerr) then

      if(present(iTime)) then
        sTime = "; iTime:"//TRIM(int2char(iTime))
      else
        sTime = ""
      end if

      if(present(rValue)) then
        sValue = ";rValue:"//TRIM(real2char(rValue))
      else
        sValue = ""
      end if

      if(present(pNC)) then
        write(sErrMessage, FMT=*) TRIM(nf90_strerror(iStatus)) &
          //"; variable: "//TRIM(pNC%sVarName)//"; nx:"// &
          TRIM(int2char(pNC%iX_NumGridCells))//"; ny:"// &
          TRIM(int2char(pNC%iY_NumGridCells))//TRIM(sTime)//TRIM(sValue)
      else
        write(sErrMessage, FMT=*) TRIM(nf90_strerror(iStatus))//TRIM(sTime)//TRIM(sValue)
      end if

      if (present(sFilename) .and. present(iLineNo)) then
        call Assert(lFALSE,TRIM(sErrMessage), &
          TRIM(sFilename),iLineNo)
      else
          call Assert(lFALSE,TRIM(sErrMessage), &
            TRIM(__FILE__),__LINE__)
      end if
    end if

  end subroutine netcdf_check

!----------------------------------------------------------------------

subroutine LLtoUTM(pConfig, rLat, rLong, rUTMNorthing, rUTMEasting, iZoneNumber)

!//converts lat/long to UTM coords.  Equations from USGS Bulletin 1532
!//East Longitudes are positive, West longitudes are negative.
!//North latitudes are positive, South latitudes are negative
!//Lat and Long are in decimal degrees
!//Written by Chuck Gantz- chuck.gantz@globalstar.com

   type (T_MODEL_CONFIGURATION), pointer :: pConfig  ! pointer to data structure that contains
                                                     ! model options, flags, and other settings

   real (kind=T_DBL), intent(in) :: rLat
   real (kind=T_DBL), intent(in) :: rLong
   real (kind=T_DBL), intent(out) :: rUTMNorthing
   real (kind=T_DBL), intent(out) :: rUTMEasting
   integer (kind=T_INT), intent(out) ::  iZoneNumber

   ! [ LOCAL VARIABLES ]
	real (kind=T_DBL) :: r_a
	real (kind=T_DBL) :: r_eccSquared
	real (kind=T_DBL) :: rk0 = 0.9996
	real (kind=T_DBL) :: rLongOrigin
	real (kind=T_DBL) :: r_eccPrimeSquared
	real (kind=T_DBL) :: rN, rT, rC, rA, rM

	real (kind=T_DBL) :: rLongTemp
	real (kind=T_DBL) :: rLatRad
	real (kind=T_DBL) :: rLongRad
	real (kind=T_DBL) :: rLongOriginRad


   rLongTemp = ( rLong + 180_T_DBL)-INT((rLong+180_T_DBL)/360_T_DBL) &
                *360_T_DBL - 180_T_DBL

   rLatRad = rLat * dpPI / 180_T_DBL
   rLongRad = rLongTemp * dpPI / 180_T_DBL

   r_a = Ellipsoid(pConfig%iEllipsoidID)%rEquatorialRadius
   r_eccSquared = Ellipsoid(pConfig%iEllipsoidID)%rEccentricitySquared

   rLongTemp = (rLong + 180_T_DBL) - int((rLong + 180_T_DBL) &
                   / 360_T_DBL) * 360_T_DBL - 180_T_DBL
 	iZoneNumber = int((rLongTemp + 180_T_DBL)/6_T_DBL) + 1_T_INT

	if( rLat >= 56.0 .and. rLat < 64.0 .and. rLongTemp >= 3.0 &
	    .and. rLongTemp < 12.0 ) iZoneNumber = 32

	! Special zones for Svalbard
	if( rLat >= 72.0 .and. rLat < 84.0 ) then
	  if(rLongTemp >= 0.0  .and. rLongTemp <  9.0 ) iZoneNumber = 31
	  if(rLongTemp >= 9.0  .and. rLongTemp < 21.0 ) iZoneNumber = 33
	  if(rLongTemp >= 21.0 .and. rLongTemp < 33.0 ) iZoneNumber = 35
	  if(rLongTemp >= 33.0 .and. rLongTemp < 42.0 ) iZoneNumber = 37
	end if

	rLongOrigin = (iZoneNumber - 1_T_DBL) * 6_T_DBL - 180_T_DBL + 3_T_DBL
	rLongOriginRad = rLongOrigin * dpPI / 180_T_DBL

	!compute the UTM Zone from the latitude and longitude
	r_eccPrimeSquared = (r_eccSquared) / (1_T_DBL - r_eccSquared)
	rN = r_a/sqrt(1_T_DBL-r_eccSquared*sin(rLatRad)*sin(rLatRad))
	rT = tan(rLatRad)*tan(rLatRad)
	rC = r_eccPrimeSquared*cos(rLatRad)*cos(rLatRad)
	rA = cos(rLatRad)*(rLongRad-rLongOriginRad)
	rM = r_a*((1_T_DBL	- r_eccSquared / 4_T_DBL	&
	          - 3_T_DBL * r_eccSquared * r_eccSquared / 64_T_DBL &
	          - 5_T_DBL * r_eccSquared * r_eccSquared * r_eccSquared / 256_T_DBL)*rLatRad &
				- (3_T_DBL * r_eccSquared / 8_T_DBL	+ 3_T_DBL * r_eccSquared*r_eccSquared / 32_T_DBL &
				+ 45_T_DBL*r_eccSquared*r_eccSquared*r_eccSquared/1024_T_DBL)*sin(2_T_DBL*rLatRad) &
				+ (15_T_DBL * r_eccSquared*r_eccSquared/256_T_DBL &
				+ 45_T_DBL*r_eccSquared*r_eccSquared*r_eccSquared/1024_T_DBL)*sin(4_T_DBL*rLatRad) &
				- (35_T_DBL*r_eccSquared*r_eccSquared*r_eccSquared/3072_T_DBL)*sin(6_T_DBL*rLatRad))

	rUTMEasting = (rk0*rN*(rA+(1_T_DBL-rT+rC)*rA*rA*rA/6_T_DBL &
					+ (5_T_DBL-18_T_DBL*rT+rT*rT+72_T_DBL*rC &
					   -58_T_DBL*r_eccPrimeSquared)*rA*rA*rA*rA*rA/120_T_DBL) &
					+ pConfig%rFalseEasting)

	rUTMNorthing = (rk0*(rM+rN*tan(rLatRad) &
	               *(rA*rA/2_T_DBL+(5_T_DBL-rT+9_T_DBL*rC+4*rC*rC)*rA*rA*rA*rA/24_T_DBL &
                  + (61_T_DBL-58_T_DBL*rT+rT*rT+600_T_DBL &
                     *rC-330_T_DBL*r_eccPrimeSquared)*rA*rA*rA*rA*rA*rA/720_T_DBL))) &
                  + pConfig%rFalseNorthing

	if(rLat < 0) then
     rUTMNorthing = rUTMNorthing + 10000000.0 ! 10000000 meter offset for southern hemisphere
   end if

   return

end subroutine LLtoUTM

!----------------------------------------------------------------------

subroutine UTMtoLL(pConfig, rUTMEasting, rUTMNorthing, rLat, rLong)

!converts UTM coords to lat/long.  Equations from USGS Bulletin 1532
!East Longitudes are positive, West longitudes are negative.
!North latitudes are positive, South latitudes are negative
!Lat and Long are in decimal degrees.
!Written by Chuck Gantz- chuck.gantz@globalstar.com

   type (T_MODEL_CONFIGURATION), pointer :: pConfig  ! pointer to data structure that contains
                                                     ! model options, flags, and other settings

	real (kind=T_DBL), intent(in) :: rUTMEasting
	real (kind=T_DBL), intent(in) :: rUTMNorthing
   real (kind=T_DBL), intent(out) :: rLat
   real (kind=T_DBL), intent(out) :: rLong

   ! [LOCAL VARIABLES ]

	real (kind=T_DBL) :: rk0 = 0.9996
	real (kind=T_DBL) :: r_a
	real (kind=T_DBL) :: r_eccSquared
	real (kind=T_DBL) :: r_eccPrimeSquared
	real (kind=T_DBL) :: re1
	real (kind=T_DBL) :: rN1, rT1, rC1, rR1, rD, rM
	real (kind=T_DBL) :: rLongOrigin
	real (kind=T_DBL) :: r_mu, r_phi1, r_phi1Rad
	integer (kind=T_INT) :: iZoneNumber
   real (kind=T_DBL) :: rX
   real (kind=T_DBL) :: rY

   r_a = Ellipsoid(pConfig%iEllipsoidID)%rEquatorialRadius
   r_eccSquared = Ellipsoid(pConfig%iEllipsoidID)%rEccentricitySquared

	rX = rUTMEasting - pConfig%rFalseEasting
	rY = rUTMNorthing - pConfig%rFalseNorthing

	if(.not. pConfig%lNorthernHemisphere) then   !point is in southern hemisphere
     rY = rY - 10000000.0                           !remove 10,000,000 meter
   end if                                           !offset used for southern hemisphere

!	rLongOrigin = (rZoneNumber - 1)*6 - 180 + 3 ! +3 puts origin in middle of zone

   re1 = (1_T_DBL-sqrt(1_T_DBL - r_eccSquared)) &
                     / (1_T_DBL+sqrt(1_T_DBL - r_eccSquared))
	r_eccPrimeSquared = (r_eccSquared) / (1_T_DBL - r_eccSquared)
	rM = rY / rk0
	r_mu = rM/(r_a*(1_T_DBL -r_eccSquared /4_T_DBL &
	       -3_T_DBL*r_eccSquared*r_eccSquared / 64_T_DBL &
	       -5_T_DBL*r_eccSquared*r_eccSquared*r_eccSquared/256_T_DBL))

	r_phi1Rad = r_mu	+ (3_T_DBL*re1/2_T_DBL-27_T_DBL*re1*re1*re1/32_T_DBL) &
	                 *sin(2_T_DBL*r_mu) &
				+ (21_T_DBL*re1*re1/16_T_DBL-55_T_DBL*re1*re1*re1*re1/32_T_DBL)* &
				    sin(4_T_DBL*r_mu) &
				+(151_T_DBL*re1*re1*re1/96_T_DBL)*sin(6_T_DBL*r_mu)

	r_phi1 = r_phi1Rad * 180_T_DBL / dpPI
	rN1 = r_a/sqrt(1_T_DBL-r_eccSquared*sin(r_phi1Rad)*sin(r_phi1Rad))
	rT1 = tan(r_phi1Rad)*tan(r_phi1Rad)
	rC1 = r_eccPrimeSquared*cos(r_phi1Rad)*cos(r_phi1Rad)
	rR1 = r_a*(1_T_DBL-r_eccSquared) / (1_T_DBL - &
            r_eccSquared*sin(r_phi1Rad)*sin(r_phi1Rad))**1.5
	rD = rX/(rN1*rk0)
	rLat = r_phi1Rad - (rN1*tan(r_phi1Rad)/rR1)*(rD*rD/2_T_DBL &
	        -(5_T_DBL+3_T_DBL*rT1+10_T_DBL* &
           rC1-4_T_DBL*rC1*rC1-9_T_DBL*r_eccPrimeSquared)*rD*rD*rD*rD/24_T_DBL &
					+(61_T_DBL+90_T_DBL*rT1+298_T_DBL*rC1+45_T_DBL*rT1 &
					*rT1-252_T_DBL*r_eccPrimeSquared &
					-3_T_DBL*rC1*rC1)*rD*rD*rD*rD*rD*rD/720_T_DBL)

	rLat = pConfig%rLatOrigin + rLat * 180_T_DBL / dpPI

	rLong = (rD-(1_T_DBL+2_T_DBL*rT1+rC1)*rD*rD*rD/6_T_DBL &
	        +(5_T_DBL-2_T_DBL*rC1+28_T_DBL*rT1-3_T_DBL*rC1*rC1+8_T_DBL* &
	        r_eccPrimeSquared+24_T_DBL*rT1*rT1) &
					*rD*rD*rD*rD*rD/120_T_DBL)/cos(r_phi1Rad)

	rLong = pConfig%rLongOrigin + rLong * 180_T_DBL / dpPI

   return

end subroutine UTMtoLL

!----------------------------------------------------------------------

#endif

end module netcdf_support
