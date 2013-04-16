!> @file
!> @brief  Contains a single module, et_hargreaves, which
!>  calculates potential evapotranspiration by means of the Hargreaves-Samani (1985) method.


!> @brief  Calculates potential evapotranspiration by means of the
!>  Hargreaves-Samani (1985) method.
module et_hargreaves
!!****h* SWB/et_hargreaves
! NAME
!   et_hargreaves.f95 - Evapotranspiration calculation using the
!   Hargreaves method.
!
! SYNOPSIS
!   This module calculates evapotranspiration using the Hargreaves
!   method.
!
! NOTES
!
!  Reference:
!
!   Allen, R.G., and others, 2006, FAO Irrigation and Drainage Paper No. 56,
!    "Crop Evapotranspiration (Guidelines for computing crop water
!    requirements)", Food and Agriculture Organization, Rome, Italy.
!
!!***

  use types
  use swb_stats
  use meteorological_functions

  implicit none

contains

subroutine et_hargreaves_configure( pConfig, sRecord )
  !! Configures the module, using the command line in 'sRecord'
  ! [ ARGUMENTS ]
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  character (len=*),intent(inout) :: sRecord

  ! [ LOCALS ]
  character (len=256) :: sOption
  integer (kind=T_INT) :: iStat
  real (kind=T_SGL) :: rValue

  write(UNIT=LU_LOG,FMT=*) "Configuring Hargreaves PET model"

  call Chomp( sRecord,sOption )
  read ( unit=sOption, fmt=*, iostat=iStat ) rValue
  call Assert( iStat == 0, "Could not read the southerly latitude" )
  pConfig%rSouthernLatitude = dpTWOPI * rValue / 360.0_T_SGL

  call Chomp( sRecord,sOption )
  read ( unit=sOption, fmt=*, iostat=iStat ) rValue
  call Assert( iStat == 0, "Could not read the northerly latitude" )
  pConfig%rNorthernLatitude = dpTWOPI * rValue / 360.0_T_SGL

  return
end subroutine et_hargreaves_configure

subroutine et_hargreaves_initialize( pGrd, sFileName )
  !! Preconfigures necessary information from the time-series file 'sFileName'
  !! and based on the model grid 'grd'.
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd
  character (len=*),intent(in) :: sFileName
  ! [ LOCALS ]

  write(UNIT=LU_LOG,FMT=*)"Initializing Hargreaves PET model"

  return
end subroutine et_hargreaves_initialize

!------------------------------------------------------------------------------

subroutine et_hargreaves_ComputeET( pGrd, pConfig, iDayOfYear, iNumDaysInYear)
  !! Computes the potential ET for each cell, based on TMIN and TMAX.
  !! Stores cell-by-cell PET values in the model grid.

  !!
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings

  integer (kind=T_INT),intent(in) :: iDayOfYear
  integer (kind=T_INT),intent(in) :: iNumDaysInYear

  ! [ LOCALS ]
  real (kind=T_SGL) :: rDelta,rOmega_s,rD_r, rRa
  real (kind=T_SGL) :: rLatitude
  integer (kind=T_INT) :: iCol, iRow

!  write(UNIT=LU_LOG,FMT=*) iDayOfYear, iNumDaysInYear

  rD_r =rel_Earth_Sun_dist(iDayOfYear,iNumDaysInYear)
  rDelta = solar_declination(iDayOfYear, iNumDaysInYear)

  do iRow=1,pGrd%iNY

    rLatitude = row_latitude(pConfig%rNorthernLatitude, &
            pConfig%rSouthernLatitude, pGrd%iNY, iRow)
    rOmega_s = sunset_angle(rLatitude, rDelta)

	! NOTE that the following equation returns extraterrestrial radiation in
	! MJ / m**2 / day.  The Hargreaves equation requires extraterrestrial
	! radiation to be expressed in units of mm / day.
	rRa = extraterrestrial_radiation_Ra(rLatitude,rDelta,rOmega_s,rD_r)

!	write(UNIT=LU_LOG,FMT=*) "Row: ",iRow,"   Latitude: ",rLatitude, "  N_Lat: ",pConfig%rNorthernLatitude, &
!	   "  S_Lat: ",pConfig%rSouthernLatitude, "  Ra: ", rRa

    do iCol=1,pGrd%iNX

      pGrd%Cells(iCol,iRow)%rSM_PotentialET = ET0_hargreaves( &
                                           pConfig, &
                                           equivalent_evaporation(rRa), &
                                           pGrd%Cells(iCol,iRow)%rTMin, &
                                           pGrd%Cells(iCol,iRow)%rTMax)
    end do

  end do

!  write(UNIT=LU_LOG,FMT=*) "=========HARGREAVES POTET CALCULATION==========="
!  write(UNIT=LU_STD_OUT,FMT="(A)") &
!       "                                 min          mean           max"
!  call stats_WriteMinMeanMax(LU_STD_OUT,"POTET" , pGrd%Cells(:,:)%rSM_PotentialET )
!
!  write(UNIT=LU_LOG,FMT=*) "=========HARGREAVES POTET CALCULATION==========="

  return

end subroutine et_hargreaves_ComputeET

!--------------------------------------------------------------------------
!!****f* et_hargreaves/ET0_hargreaves
! NAME
!   ET0_hargreaves - Calculates reference ET from air temperature data.
!
! SYNOPSIS
!   Calculates reference ET given minimum and maximum air temperature.
!
! INPUTS
!   r_mm - Value in millimeters.
!
! OUTPUTS
!   r_in - Value in inches.
!
! SOURCE

function ET0_hargreaves(pConfig, rRa, rTMinF, rTMaxF) result(rET_0)

  ! [ ARGUMENTS ]

  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  real (kind=T_SGL),intent(in) :: rRa
  real (kind=T_SGL),intent(in) :: rTMinF
  real (kind=T_SGL),intent(in) :: rTMaxF

  ! [ RETURN VALUE ]
  real (kind=T_SGL) :: rET_0

  ! [ LOCALS ]
  real (kind=T_SGL) :: rTDelta
  real (kind=T_SGL) :: rTAvg

  rTAvg = (rTMinF + rTMaxF) / 2_T_SGL

  rTDelta = FtoK(rTMaxF) - FtoK(rTMinF)

!  rET_0 = MAX(rZERO, &
!           ( 0.0023_T_SGL &
!           * rRa &
!           * (FtoC(rTavg) + 17.8_T_SGL) &
!           * sqrt(rTDelta)) &
!           / rMM_PER_INCH)

  rET_0 = MAX(rZERO, &
           ( pConfig%rET_Slope &
           * rRa &
           * (FtoC(rTavg) + pConfig%rET_Constant) &
           * (rTDelta**pConfig%rET_Exponent)) &
           / rMM_PER_INCH)

  return

end function ET0_hargreaves

!!***

end module et_hargreaves














