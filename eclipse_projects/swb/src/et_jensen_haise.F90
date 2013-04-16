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

  use types

  implicit none

  !! Module data

  !! Configuration -- input data
  real (kind=T_SGL) :: rLatitude       ! degrees on input; stored in radians
  real (kind=T_SGL) :: rAlbedo         ! defaults to 0.23
  real (kind=T_SGL) :: rAs             ! defaults to 0.25
  real (kind=T_SGL) :: rBs             ! defaults to 0.50

contains

subroutine et_jh_configure( sRecord )
  !! Configures the module, using the command line in 'sRecord'
  ! [ ARGUMENTS ]
  character (len=*),intent(inout) :: sRecord
  ! [ LOCALS ]
  character (len=256) :: sOption
  integer (kind=T_INT) :: iStat

  write(UNIT=LU_LOG,FMT=*) "Configuring Jensen-Haise PET model"

  call Chomp( sRecord,sOption )
  read ( unit=sOption, fmt=*, iostat=iStat ) rLatitude
  call Assert( iStat == 0, "Could not read the latitude" )
  rLatitude = dpTWOPI * rLatitude / 360.0_T_SGL

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
  integer (kind=T_INT),intent(in) :: iDayOfYear
  real (kind=T_SGL),intent(in) :: rRH,rMinRH,rWindSpd,rSunPct
  ! [ LOCALS ]
  real (kind=T_SGL) :: rSo,rDelta,rOmega_s,rD_r,rS0,rSn,rT
  integer (kind=T_INT) :: iCol, iRow
  ! [ CONSTANTS ]
  real (kind=T_SGL),parameter :: UNIT_CONV = 0.41_T_SGL / 25.4_T_SGL

  call Assert( LOGICAL(rSunPct>=rZERO, kind=T_LOGICAL),"Missing data for percent sunshine" )
  call Assert( LOGICAL(rRH>=rZERO, kind=T_LOGICAL),"Missing data for relative humidity" )

  rD_r = rONE + 0.033_T_SGL * cos( dpTWOPI * iDayOfYear / 365.0_T_SGL )
  rDelta = 0.4093_T_SGL * sin( (dpTWOPI * iDayOfYear / 365.0_T_SGL) - 1.405_T_SGL )
  rOmega_s = acos( -tan(rLatitude) * tan(rDelta) )
  rSo = 2.44722_T_SGL * 15.392_T_SGL * rD_r * (     rOmega_s  * sin(rLatitude) * sin(rDelta) + &
                                                  sin(rOmega_s) * cos(rLatitude) * cos(rDelta) )
  rSn = rSo * ( rONE-rAlbedo ) * ( rAs + rBS * rSunPct / rHUNDRED )

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX  ! last subscript in a Fortran array should be the slowest changing

      if ( pGrd%Cells(iCol,iRow)%rTAvg <= rFREEZING ) then
        pGrd%Cells(iCol,iRow)%rSM_PotentialET = rZERO
      else
        rT = FtoC(pGrd%Cells(iCol,iRow)%rTAvg)
        pGrd%Cells(iCol,iRow)%rSM_PotentialET = UNIT_CONV * ( 0.025_T_SGL * rT + 0.078_T_SGL ) * rSn
      end if

    end do

  end do

  return

end subroutine et_jh_ComputeET

end module et_jensen_haise
