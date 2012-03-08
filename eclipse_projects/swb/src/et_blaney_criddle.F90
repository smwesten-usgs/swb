!> @file
!> @brief  Contains a single module, et_blaney_criddle, which
!>  calculates potential evapotranspiration by means of the Blaney-Criddle (1950)
!>  method, as modified by Jensen and others (1990).

!> @brief  Calculates potential evapotranspiration by means of the
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

  use types

  implicit none

  !! Module data

  !! Configuration -- input data
  real (kind=T_SGL) :: rLatitude       ! degrees on input; stored in radians
  real (kind=T_SGL) :: rSunAnnual      ! Annual sun hours

contains

subroutine et_bc_configure( sRecord )
  !! Configures the module, using the command line in 'sRecord'
  ! [ ARGUMENTS ]
  character (len=*),intent(inout) :: sRecord
  ! [ LOCALS ]
  character (len=256) :: sOption
  integer (kind=T_INT) :: iStat

  write(UNIT=LU_LOG,FMT=*) "Configuring Blaney-Criddle PET model"

  call Chomp( sRecord,sOption )
  read ( unit=sOption, fmt=*, iostat=iStat ) rLatitude
  call Assert( LOGICAL(iStat==0,kind=T_LOGICAL), "Could not read the latitude" )
  rLatitude = dpTWOPI * rLatitude / 360.0_T_SGL

  return
end subroutine et_bc_configure

subroutine et_bc_initialize( grd, sFileName )
  !! Preconfigures necessary information from the time-series file 'sFileName'
  !! and based on the model grid 'grd'.
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: grd
  character (len=*),intent(in) :: sFileName
  ! [ LOCALS ]
  real (kind=T_SGL) :: rDelta
  integer (kind=T_INT) :: iDay

  write(UNIT=LU_LOG,FMT=*)"Initializing Blaney-Criddle PET model"
  do iDay=1,365
    rDelta = 0.4093_T_SGL * sin( (dpTWOPI * iDay / 365.0_T_SGL) - 1.405_T_SGL )
    rSunAnnual = rSunAnnual + 24.0_T_SGL * acos(-tan(rLatitude) * tan(rDelta)) / dpPI
  end do

  return
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
  integer (kind=T_INT),intent(in) :: iDayOfYear
  real (kind=T_SGL),intent(in) :: rRH,rMinRH,rWindSpd,rSunPct
  ! [ LOCALS ]
  real (kind=T_SGL) :: rDelta,rT,rSunFrac,rPRatio,rAbc,rBbc,rF
  integer (kind=T_INT) :: iCol,iRow
  ! [ CONSTANTS ]
  real (kind=T_SGL),parameter :: UNIT_CONV = 0.313_T_SGL / 25.4_T_SGL

  call Assert( LOGICAL(rSunPct>=rZERO,kind=T_LOGICAL), "Missing data for percent sunshine" )
  call Assert( LOGICAL(rMinRH>=rZERO,kind=T_LOGICAL),"Missing data for relative humidity" )

  rDelta = 0.4093_T_SGL * sin( (dpTWOPI * iDayOfYear / 365.0_T_SGL) - 1.405_T_SGL )
  rPRatio = ( 24.0_T_SGL * acos(-tan(rLatitude) * tan(rDelta)) / dpPI ) / rSunAnnual

  rSunFrac = rSunPct / rHUNDRED
  rAbc = ( 0.0043_T_SGL * rMinRH) - rSunFrac - 1.41_T_SGL

!
! The following is what was originally coded by WES and Vic.  Origin unclear.
! Possibly (probably) the source is Doorenbos and Pruitt (1977)?
!
!  rBbc = 0.81917_T_SGL - 0.0040922_T_SGL*rMinRH +1.0705_T_SGL*rSunFrac + &
!         0.065649*rWindSpd - 0.0059684*rMinRH*rSunFrac - &
!         0.0005967*rMinRH*rWindSpd

! Formulation for rBbc as given below is from Jensen et al (1990):

  rBbc = 0.908 - (0.00483*rMinRH) + (0.7949*rSunFrac) &
         + (0.768*(LOG(rWindSpd+1.)**2)) &
         - (0.0038*rMinRH*rSunFrac) &
         - (0.000443*rMinRH*rWindSpd) &
         + (0.281*LOG(rSunFrac+1.)) &
         - (0.00975*LOG(rWindSpd+1.)*(LOG(rMinRH+1.)**2)*LOG(rSunFrac+1.))


  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX

      rT = FtoC(pGrd%Cells(iCol,iRow)%rTAvg)

      rF = rPRatio * ( 8.13_T_SGL + 0.46_T_SGL * rT)

      if ( pGrd%Cells(iCol,iRow)%rTAvg <= rFREEZING ) then
        pGrd%Cells(iCol,iRow)%rSM_PotentialET = rZERO
      else
      !write(UNIT=LU_LOG,FMT=*)'TEST',iDayOfYear,rPratio,rAbc,rBbc
        pGrd%Cells(iCol,iRow)%rSM_PotentialET = (rAbc + rF * rBbc) / rMM_PER_INCH
      end if

    end do

  end do

  return
end subroutine et_bc_ComputeET

end module et_blaney_criddle
