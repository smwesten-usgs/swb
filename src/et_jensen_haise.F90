!> @file
!> @brief  Contains a single module, et_jensen_haise, which
!>  calculates potential evapotranspiration by means of the Jensen-Haise (1963) method.


!> @brief  Calculates potential evapotranspiration by means of the
!>  Jensen-Haise (1963) method.
module et_jensen_haise
!!****h* SWB/et_jensen_haise
! NAME
!   et_jensen_haise.f95 - Evapotranspiration calculation using the
!   Jensen-Haise method.
!
! SYNOPSIS
!   This module calculates evapotranspiration using the Jensen-Haise
!   method.
!
! NOTES
!   Original method is documented in:
!
!   Jensen, M.E., H.R. Haise. 1963. Estimating evapotranspiration from solar
!   radiation. Journal of Irrigation and Drainage Engineering 89(IR4):15-41.
!
!!***

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use types

  implicit none

  !! Module data

  !! Configuration -- input data
  real (kind=c_float) :: rLatitude       ! degrees on input; stored in radians
  real (kind=c_float) :: rAlbedo         ! defaults to 0.23
  real (kind=c_float) :: rAs             ! defaults to 0.25
  real (kind=c_float) :: rBs             ! defaults to 0.50

contains

subroutine et_jh_configure( sRecord )
  !! Configures the module, using the command line in 'sRecord'
  ! [ ARGUMENTS ]
  character (len=*),intent(inout) :: sRecord
  ! [ LOCALS ]
  character (len=256) :: sOption
  integer (kind=c_int) :: iStat

  write(UNIT=LU_LOG,FMT=*) "Configuring Jensen-Haise PET model"

  call Chomp( sRecord,sOption )
  read ( unit=sOption, fmt=*, iostat=iStat ) rLatitude
  call Assert( iStat == 0, "Could not read the latitude" )
  rLatitude = dpTWOPI * rLatitude / 360.0_c_float

  call Chomp( sRecord,sOption )
  read ( unit=sOption, fmt=*, iostat=iStat ) rAlbedo
  call Assert( iStat == 0, "Could not read the albedo" )

  call Chomp( sRecord,sOption )
  read ( unit=sOption, fmt=*, iostat=iStat ) rAs
  call Assert( iStat == 0, "Could not read a_s" )

  call Chomp( sRecord,sOption )
  read ( unit=sOption, fmt=*, iostat=iStat ) rBs
  call Assert( iStat == 0, "Could not read b_s" )

  return
end subroutine et_jh_configure

subroutine et_jh_initialize( grd, sFileName )
  !! Preconfigures necessary information from the time-series file 'sFileName'
  !! and based on the model grid 'grd'.
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: grd
  character (len=*),intent(in) :: sFileName
  ! [ LOCALS ]

  write(UNIT=LU_LOG,FMT=*)"Initializing Jensen-Haise PET model"

  return
end subroutine et_jh_initialize

subroutine et_jh_ComputeET( pGrd, iDayOfYear, rRH, &
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
  real (kind=c_float) :: rSo,rDelta,rOmega_s,rD_r,rS0,rSn,rT
  integer (kind=c_int) :: iCol, iRow
  ! [ CONSTANTS ]
  real (kind=c_float),parameter :: UNIT_CONV = 0.41_c_float / 25.4_c_float

  call Assert( LOGICAL(rSunPct>=rZERO, kind=c_bool),"Missing data for percent sunshine" )
  call Assert( LOGICAL(rRH>=rZERO, kind=c_bool),"Missing data for relative humidity" )

  rD_r = rONE + 0.033_c_float * cos( dpTWOPI * iDayOfYear / 365.0_c_float )
  rDelta = 0.4093_c_float * sin( (dpTWOPI * iDayOfYear / 365.0_c_float) - 1.405_c_float )
  rOmega_s = acos( -tan(rLatitude) * tan(rDelta) )
  rSo = 2.44722_c_float * 15.392_c_float * rD_r * (     rOmega_s  * sin(rLatitude) * sin(rDelta) + &
                                                  sin(rOmega_s) * cos(rLatitude) * cos(rDelta) )
  rSn = rSo * ( rONE-rAlbedo ) * ( rAs + rBS * rSunPct / rHUNDRED )

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX  ! last subscript in a Fortran array should be the slowest changing

      if ( pGrd%Cells(iCol,iRow)%rTAvg <= rFREEZING ) then
        pGrd%Cells(iCol,iRow)%rReferenceET0 = rZERO
      else
        rT = FtoC(pGrd%Cells(iCol,iRow)%rTAvg)
        pGrd%Cells(iCol,iRow)%rReferenceET0 = UNIT_CONV * ( 0.025_c_float * rT + 0.078_c_float ) * rSn
      end if

    end do

  end do

  return

end subroutine et_jh_ComputeET

end module et_jensen_haise
