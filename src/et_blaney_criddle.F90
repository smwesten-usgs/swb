!> @file
!>  Contains a single module, et_blaney_criddle, which
!>  calculates potential evapotranspiration by means of the Blaney-Criddle (1950)
!>  method, as modified by Jensen and others (1990).

!>  Calculates potential evapotranspiration by means of the
!> Blaney-Criddle (1950) method, as modified by Jensen and others (1990).
module et_blaney_criddle
!!****h* SWB/et_blaney_criddle
! NAME
!   et_blaney_criddle.f95 - Evapotranspiration calculation using the
!   Blaney-Criddle method.
!
! SYNOPSIS
!   This module calculates evapotranspiration using the Blaney-Criddle
!   method, as modified by Jensen and others (1990).
!
! NOTES
!   Original method is documented in:
!
!   Blaney, H.F. and W.D. Criddle. 1950. Determining water requirements
!   in irrigated areas from climatological and irrigation data.
!   Soil Conservation Service Technical Paper 96. Soil Conservation Service,
!   U.S. Dept. of Agriculture: Washington, D.C.
!
!   Method included here has been modified for daily ET calculation and is
!   documented in:
!
!   Jensen, M.E, R.D Burman, and R.G. Allen. 1990. Evapotranspiration and
!   Irrigation Water Requirements. ASCE Manuals and Reports of Engineering
!   Practice No. 70. New York: ASCE.
!
!!***

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use types
  use meteorological_functions

  implicit none

  !! Module data

  !! Configuration -- input data
  real (kind=c_float) :: rLatitude       ! degrees on input; stored in radians
  real (kind=c_float) :: rSunAnnual      ! Annual sun hours

contains

subroutine et_bc_configure( sRecord )
  !! Configures the module, using the command line in 'sRecord'
  ! [ ARGUMENTS ]
  character (len=*),intent(inout) :: sRecord
  ! [ LOCALS ]
  character (len=256) :: sOption
  integer (kind=c_int) :: iStat

  write(UNIT=LU_LOG,FMT=*) "Configuring Blaney-Criddle PET model"

  call Chomp( sRecord,sOption )
  read ( unit=sOption, fmt=*, iostat=iStat ) rLatitude
  call Assert( LOGICAL(iStat==0,kind=c_bool), "Could not read the latitude" )
  rLatitude = dpTWOPI * rLatitude / 360.0_c_float

  return
end subroutine et_bc_configure

subroutine et_bc_initialize( grd, sFileName )
  !! Preconfigures necessary information from the time-series file 'sFileName'
  !! and based on the model grid 'grd'.
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: grd
  character (len=*),intent(in) :: sFileName
  ! [ LOCALS ]
  real (kind=c_float) :: rDelta
  integer (kind=c_int) :: iDay

  write(UNIT=LU_LOG,FMT=*)"Initializing Blaney-Criddle PET model"
  do iDay=1,365
    rDelta = 0.4093_c_float * sin( (dpTWOPI * iDay / 365.0_c_float) - 1.405_c_float )
    rSunAnnual = rSunAnnual + 24.0_c_float * acos(-tan(rLatitude) * tan(rDelta)) / dpPI
  end do

end subroutine et_bc_initialize

!------------------------------------------------------------------------------

subroutine et_bc_ComputeET( pGrd, iDayOfYear, rRH, &
                          rMinRH, rWindSpd, rSunPct )
  !! Computes the potential ET for each cell, based on the meteorological
  !! data given. Stores cell-by-cell PET values in the model grid.
  !! Note: for the T-M model, it's constant scross the grid
  !!
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd
  integer (kind=c_int),intent(in) :: iDayOfYear
  real (kind=c_float),intent(in) :: rRH,rMinRH,rWindSpd,rSunPct
  ! [ LOCALS ]
  real (kind=c_float) :: rDelta,rT,rSunFrac,rPRatio,rAbc,rBbc,rF
  integer (kind=c_int) :: iCol,iRow
  ! [ CONSTANTS ]
  real (kind=c_float),parameter :: UNIT_CONV = 0.313_c_float / 25.4_c_float

  call Assert( LOGICAL(rSunPct>=rZERO,kind=c_bool), "Missing data for percent sunshine" )
  call Assert( LOGICAL(rMinRH>=rZERO,kind=c_bool),"Missing data for relative humidity" )

!  rDelta = 0.4093_c_float * sin( (dpTWOPI * iDayOfYear / 365.0_c_float) - 1.405_c_float )
  rDelta = solar_declination(iDayOfYear, 365)

  rPRatio = ( 24.0_c_float * acos(-tan(rLatitude) * tan(rDelta)) / dpPI ) / rSunAnnual

  rSunFrac = rSunPct / rHUNDRED
  rAbc = ( 0.0043_c_float * rMinRH) - rSunFrac - 1.41_c_float

!
! The following is what was originally coded by WES and Vic.  Origin unclear.
! Possibly (probably) the source is Doorenbos and Pruitt (1977)?
! More like likely source is Allen and Pruitt (1986) "Rational Use of the FAO BC Formula"
!
  rBbc = 0.81917_c_float - 0.0040922_c_float*rMinRH +1.0705_c_float*rSunFrac + &
         0.065649*rWindSpd - 0.0059684*rMinRH*rSunFrac - &
         0.0005967*rMinRH*rWindSpd

! Formulation for rBbc as given below is from Jensen et al (1990):

!  rBbc = 0.908 - (0.00483*rMinRH) + (0.7949*rSunFrac) &
!         + (0.768*(LOG(rWindSpd+1.)**2)) &
!         - (0.0038*rMinRH*rSunFrac) &
!         - (0.000443*rMinRH*rWindSpd) &
!         + (0.281*LOG(rSunFrac+1.)) &
!         - (0.00975*LOG(rWindSpd+1.)*(LOG(rMinRH+1.)**2)*LOG(rSunFrac+1.))


  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX

      rT = FtoC(pGrd%Cells(iCol,iRow)%rTAvg)

      rF = rPRatio * ( 8.13_c_float + 0.46_c_float * rT)

      if ( pGrd%Cells(iCol,iRow)%rTAvg <= rFREEZING ) then
        pGrd%Cells(iCol,iRow)%rReferenceET0 = rZERO
      else
        pGrd%Cells(iCol,iRow)%rReferenceET0 = (rAbc + rF * rBbc) / rMM_PER_INCH
        write(UNIT=LU_LOG,FMT=*)'TEST',iDayOfYear, "rMinRH: ", rMinRH, &
          "Sunfrac: ", rSunFrac, "rPratio: ",rPratio, &
          "rAbc: ", rAbc, "rF: ", rF, "rBbc: ", rBbc, "ET0: ", pGrd%Cells(iCol,iRow)%rReferenceET0
      end if

    end do

  end do

  return
end subroutine et_bc_ComputeET

end module et_blaney_criddle
