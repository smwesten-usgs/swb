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

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use types
  use stats
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
  integer (kind=c_int) :: iStat
  real (kind=c_float) :: rValue

  write(UNIT=LU_LOG,FMT=*) "Configuring Hargreaves PET model"

  if (pConfig%rSouthernLatitude <= rNO_DATA_NCDC &
    .or. pConfig%rNorthernLatitude <= rNO_DATA_NCDC) then

    call Chomp( sRecord,sOption )
    read ( unit=sOption, fmt=*, iostat=iStat ) rValue
    call Assert( iStat == 0, "Could not read the southerly latitude" )
    pConfig%rSouthernLatitude = dpTWOPI * rValue / 360.0_c_float

    call Chomp( sRecord,sOption )
    read ( unit=sOption, fmt=*, iostat=iStat ) rValue
    call Assert( iStat == 0, "Could not read the northerly latitude" )
    pConfig%rNorthernLatitude = dpTWOPI * rValue / 360.0_c_float

  else

    call echolog("Southern and northern latitude values have been determined" &
      //"~from project grid bounds and projection parameters. The values supplied" &
      //"~with the Hargreaves PET option will be ignored...")

  endif

end subroutine et_hargreaves_configure

!------------------------------------------------------------------------------

subroutine et_hargreaves_ComputeET( pGrd, pConfig, iDayOfYear, iNumDaysInYear)
  !! Computes the potential ET for each cell, based on TMIN and TMAX.
  !! Stores cell-by-cell PET values in the model grid.

  !!
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings

  integer (kind=c_int),intent(in) :: iDayOfYear
  integer (kind=c_int),intent(in) :: iNumDaysInYear

  ! [ LOCALS ]
  real (kind=c_double) :: rLatitude, rDelta, rOmega_s, rD_r, rRa
  integer (kind=c_int) :: iCol, iRow
  type (T_CELL), pointer :: cel

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

      cel => pGrd%Cells(iCol, iRow)

      if (pGrd%iMask(iCol, iRow) == iINACTIVE_CELL) cycle

      pGrd%Cells(iCol,iRow)%rReferenceET0 = ET0_hargreaves( &
                                           pConfig, &
                                           equivalent_evaporation(rRa), &
                                           pGrd%Cells(iCol,iRow)%rTMin, &
                                           pGrd%Cells(iCol,iRow)%rTMax)
    end do

  end do

!  write(UNIT=LU_LOG,FMT=*) "=========HARGREAVES POTET CALCULATION==========="
!  write(UNIT=LU_STD_OUT,FMT="(A)") &
!       "                                 min          mean           max"
!  call stats_WriteMinMeanMax(LU_STD_OUT,"POTET" , pGrd%Cells(:,:)%rReferenceET0 )
!
!  write(UNIT=LU_LOG,FMT=*) "=========HARGREAVES POTET CALCULATION==========="


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
  real (kind=c_double),intent(in) :: rRa
  real (kind=c_float),intent(in) :: rTMinF
  real (kind=c_float),intent(in) :: rTMaxF

  ! [ RETURN VALUE ]
  real (kind=c_float) :: rET_0

  ! [ LOCALS ]
  real (kind=c_double) :: rTDelta
  real (kind=c_double) :: rTAvg

  rTAvg = (rTMinF + rTMaxF) / 2_c_double

  rTDelta = FtoK(rTMaxF) - FtoK(rTMinF)

!  rET_0 = MAX(rZERO, &
!           ( 0.0023_c_float &
!           * rRa &
!           * (FtoC(rTavg) + 17.8_c_float) &
!           * sqrt(rTDelta)) &
!           / rMM_PER_INCH)

  rET_0 = MAX(rZERO, &
           ( pConfig%rET_Slope &
           * rRa &
           * (FtoC(rTavg) + pConfig%rET_Constant) &
           * (rTDelta**pConfig%rET_Exponent)) &
           / rMM_PER_INCH)

end function ET0_hargreaves

!!***

end module et_hargreaves
