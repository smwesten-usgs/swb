!> @file
!> Contains a single module, @ref snow, which implements an
!> experimental energy-balance snowmelt routine.


!> Implements snow-related calculations (i.e. albedo, snow depth),
!> as well as a simple snow energy balance routine.
!> @note These routines are only invoked when control variable
!> \c pConfig%iConfigureSnow is set to \c CONFIG_SNOW_NEW_SWB
module snow

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use types
  use meteorological_functions
  implicit none

  contains

function snow_depth_in(rTAvg)  result(rSnowDepth)

  ! returns snow depth for fresh snow on the basis of the National
  ! Weather Service table "Approximate snowfall amounts at
  ! specified temperature ranges"

  real(kind=c_float), intent(in) :: rTAvg

  ! [ LOCALS ]
  real(kind=c_float) :: rSnowDepth

  rSnowDepth = 30.0_c_double &
     * exp(-0.04_c_double * REAL(rTAvg,kind=c_double))

  return

end function snow_depth_in

!--------------------------------------------------------------------------

function snow_depth_Hedstrom(rTAvg, pConfig)   result(rSnowDepth)

  ! returns fresh snow depth in inches, per inch of liquid precipitation
  real(kind=c_float), intent(in) :: rTAvg
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings

  ! [ LOCALS ]
  real(kind=c_float) :: rSnowDepth
  real (kind=c_float) :: rRhoS                       ! density of fresh snow in kg/m^3
  real (kind=c_float), parameter :: rRhoW = 1000.0   ! density of water in kg/m^3

  ! ensure that Hedstrom and Pomeroy's equation (1998) stays within reasonable values
!  rRhoS = MIN(MAX(50.,67.9 + 51.3 * exp(FtoC(rTAvg)/2.6)),130.)
  rRhoS = MIN(MAX(50.,pConfig%rSNWD_intcp1 + pConfig%rSNWD_slp1 &
             * exp(FtoC(rTAvg)/ pConfig%rSNWD_denom)),130.)

  rSnowDepth = rRhoW / rRhoS

end function snow_depth_Hedstrom

!!***
!--------------------------------------------------------------------------
!!****f* meteorological_functions/snow_energy_balance
! NAME
!   snow_energy_balance - estimates snow energy_balance
!
! SYNOPSIS
!   Estimates snow energy balance in kJ per square meter per day
!
! INPUTS
!
! OUTPUTS
!
! NOTES

subroutine snow_energy_balance(rTMin, rTMax, rTAvg, rRs, rRso, rAlbedo, &
   rSnowcover, rNetRainfall, rSnowTemperature, rMeltAmount, i, j)

  real (kind=c_float)::  rTMin
  real (kind=c_float) :: rTMax
  real (kind=c_float) :: rTAvg
  real (kind=c_double) :: rRs
  real (kind=c_double) :: rRso
  real (kind=c_float) :: rAlbedo
  real (kind=c_float) :: rSnowcover
  real (kind=c_float) :: rNetRainfall
  real (kind=c_float) :: rSnowTemperature
  real (kind=c_float) :: rMeltAmount
  real (kind=c_float) :: rMeltPotential
  integer (kind=c_int) :: i,j

  ! [ LOCALS ]
  real (kind=c_float) :: rSn                   ! net shortwave radiation
  real (kind=c_float) :: rLn                   ! net longwave radiation
  real (kind=c_float) :: rH                    ! sensible heat exchange
  real (kind=c_float) :: rE                    ! convective heat exchange
  real (kind=c_float), parameter :: rG = 173.  ! ground heat, kj per square meter
  real (kind=c_float) :: rP                    ! precipitation heat exchange
  real(kind=c_float) :: rSWE
  real (kind=c_double) :: rDeltaT
  real (kind=c_double) :: rDeltaSWE
  real (kind=c_float) :: rSnowTemp_degC
  real (kind=c_float) :: rTMin_C
  real (kind=c_double) :: rNetEnergy
  real (kind=c_double) :: rT_RiseEnergy
  real (kind=c_double) :: rMeltEnergy
  real (kind=c_double), parameter :: rCsnow = 2.1  ! kJ per kg per deg C
                                                ! snow heat capacity

  real (kind=c_double), parameter :: rLambda = 3.35E+05  ! kJ per cubic meter per deg C
                                                      ! latent heat of fusion

  ! Snowcover is in INCHES SWE; we need METERS SWE

  rSWE = rSnowCover / 12.0 *.3048
  rTMin_C = FtoC(rTMin)
  rSnowTemp_degC = FtoC(rSnowTemperature)

  ! all component energy values must be in kJ per square meter per day
  ! radiation components must be multiplied by 1000 since they are returned
  ! in MJ per square meters per day
  rSn = net_shortwave_radiation_Rns(rRs, real(rAlbedo, kind=c_double)) * 1000_c_double
  rLn = net_longwave_radiation_Rnl(rTMin, rTMax, rRs, rRso) * 1000_c_double
  rH = sensible_heat_exchange_h(rSnowTemperature, rTAvg)
  rE = convective_heat_exchange_e(rSnowTemperature, rTAvg)
  rP = precipitation_heat_p(rNetRainfall, rTAvg)

  if(i==3 .and. j==3) then
    write(*,fmt="(' rSnowTemperature (input): ',F14.4)") rSnowTemperature
    write(*,fmt="(' rSWE (input) : ',F14.4)") rSWE
  end if

  if(rSWE > 0) then  ! we have snow and should do the calculations

    rNetEnergy = ( rSn - rLn + rH + rE + rG + rP )
    rDeltaT = 0.01 * rNetEnergy / (rSWE * rCsnow * 1000.)

  if(i==3 .and. j==3) then
    write(*,fmt="(' rNetEnergy : ',F14.4)") rNetEnergy
    write(*,fmt="(' rDeltaT : ',F14.4)") rDeltaT
  end if


    if(rNetEnergy < 0.) then  ! insufficient energy is available for snowmelt
                              ! snowpack temperature decreases

      if(i==3 .and. j==3) print *, '  snowpack temp DECREASES'
      rSnowTemp_degC = rSnowTemp_degC + rDeltaT
      if( rSnowTemp_degC > 0.) rSnowTemp_degC = 0.
      if(rSnowTemp_degC < rTMin_C) rSnowTemp_degC = rTMin_C
      rMeltAmount = 0.
      ! now recalculate rDeltaT after constraints applied to rSnowTemp_degC
      rDeltaT = FtoC(rSnowTemperature) - rSnowTemp_degC
    else ! excess energy added to snowpack; possible snowmelt

      if(rSnowTemp_degC + rDeltaT > 0.) then ! more energy added to snowpack than
                                             ! it can absorb; melting occurs
        if(i==3 .and. j==3) print *, '  snowpack temp DECREASES; MELTING OCCURS'

        rT_RiseEnergy = (rSWE * rCsnow * 1000. ) * ABS( rSnowTemp_degC )
        rSnowTemp_degC = 0.
        rMeltEnergy = rNetEnergy - rT_RiseEnergy
        rMeltPotential = rMeltEnergy / (333.3 * 1000.)
        rDeltaSWE = MIN(rSWE,rMeltPotential)
        rMeltAmount = ABS(rDeltaSWE) / 0.3048 * 12.
        rSnowcover = MAX(rSnowcover - rMeltAmount,rZERO)

      else  ! sufficient energy to raise snowpack temperature, but no snowmelt occurs
        if(i==3 .and. j==3) print *, '  snowpack temp INCREASES'
        rMeltAmount = 0.
        rSnowTemp_degC = rSnowTemp_degC + rDeltaT

      end if
    end if
  else
    rMeltAmount = 0.
    rSnowcover = 0.
  end if

  rSnowTemperature = CtoF(rSnowTemp_degC)

end subroutine snow_energy_balance

!--------------------------------------------------------------------------
!!****f* meteorological_functions/snow_albedo
! NAME
!   snow_albedo - estimates snow albedo
!
! SYNOPSIS
!   Estimates snow albedo given number of days since last snowfall
!
! INPUTS
!   rAlbedoInit - initial albedo when snow first falls
!   iNumDaysLastSnow - number of days since the last snowfall took place
!   rZenithAngle - solar zenith angle at noon
!
! OUTPUTS
!   rAlbedo - current albedo of snow surface
!
! NOTES
!
!  Implemented as equation 13-15, Kustas et al, 1994
!
!   Reference:
!      Kustas, W.P., Rango, A. and Uijlenhoet, R., A simple energy
!          budget algorithm for the snowmelt runoff model:
!          Water Resources Research, v. 30, no. 5.
!
! SOURCE

function snow_albedo(rAlbedoInit, iNumDaysLastSnow, rZenithAngle)  result(rAlbedo)

  real (kind=c_float), intent(in) :: rAlbedoInit
  integer (kind=c_int), intent(in) :: iNumDaysLastSnow
  real (kind=c_float), intent(in) :: rZenithAngle

  ! [ LOCALS ]
  real (kind=c_float) :: rAlbedo
  real (kind=c_float) :: rSnowGrainSize
  real (kind=c_float), parameter :: rSnowGrainSizeBeforeMelting = 0.25 ! millimeters

  rSnowGrainSize = (rSnowGrainSizeBeforeMelting * rSnowGrainSizeBeforeMelting &
                     * rSnowGrainSizeBeforeMelting &
                       + 0.252_c_float * REAL(iNumDaysLastSnow))**0.3333333_c_double

  rAlbedo = rAlbedoInit - (0.083_c_float + 0.23_c_float * sqrt(rSnowGrainSize)) &
                            * sqrt(cos(rZenithAngle))

end function snow_albedo

end module snow
