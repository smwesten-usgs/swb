!> @file
!>  Contains a single module, @ref meteorological_functions, which
!>  provides support functions used by evapotranspiration and snow routines.

!>  Provides support functions used by evapotranspiration and snow routines.
!> @note  Many of the routines included here are based on equations given in
!>   the following reference:
!> @par
!>   Allen, R.G., and others, 2006, FAO Irrigation and Drainage Paper No. 56,
!>    "Crop Evapotranspiration (Guidelines for computing crop water
!>    requirements)", Food and Agriculture Organization, Rome, Italy.

module meteorological_functions
!
!   Many of the routines included here are based on equations given in
!   the following reference:
!
!   Allen, R.G., and others, 2006, FAO Irrigation and Drainage Paper No. 56,
!    "Crop Evapotranspiration (Guidelines for computing crop water
!    requirements)", Food and Agriculture Organization, Rome, Italy.
!
!!***

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use types

  implicit none

contains

! function capillary_flux(rZgw, rHae, rN, rB, rKs)  result(r_v)

!  real (kind=c_float) :: rZgw, rhae, rn, rB, rKs
!  real (kind=c_float) :: r_v

!  r_v = 1000. * rINCH_PER_MM * rB * rKs * ( rHae / rZgw ) ^ rN

!end function capillary_flux


  !> Calculate the number of daylight hours at a location.
  !>
  !> @param  [in] rOmega_s The sunset hour angle in Radians.
  !> @return rN The number of daylight hours.
  !>
  !> @note Implemented as equation 34, Allen and others (2006).
  !>
  !> @note Allen, R.G., and others, 2006, FAO Irrigation and Drainage Paper No. 56,
  !>       "Crop Evapotranspiration (Guidelines for computing crop water
  !>       requirements)", Food and Agriculture Organization, Rome, Italy.
  function daylight_hours(rOmega_s) result(rN)

    ! [ ARGUMENTS ]
    real (kind=c_double), intent(in) :: rOmega_s

    ! [ LOCALS ]
    real (kind=c_double) :: rN

    rN = 24_c_double / dpPI * rOmega_s

  end function daylight_hours

!>   Calculate extraterrestrial radiation given latitude and time of year.
!>
!> @param [in]   rLatitude  Latitude of grid cell in RADIANS.
!> @param [in]   rDelta     Solar declination in RADIANS.
!> @param [in]   rOmega_s   Sunset hour angle in RADIANS.
!> @param [in]   rDsubR     Inverse relative distance Earth-Sun.
!>
!> @retval rRa   Extraterrestrial radiation in MJ / m**2 / day.
!>
!> @note  1 MJ = 1e6 Joules; 1 Joule = 1 Watt / sec.
!> @note   Therefore, multiply by 1e6 and divide by 86400 to get W/m*2-day.
!>
!> @par Source
!>      Equation 21, Allen and others (2006).
!>
!> @par Reference
!>      Allen, R.G., and others, 2006, FAO Irrigation and Drainage Paper No. 56,
!>      "Crop Evapotranspiration (Guidelines for computing crop water
!>      requirements)", Food and Agriculture Organization, Rome, Italy.
!>
!> @sa http://www.fao.org/docrep/x0490e/x0490e07.htm#solar%20radiation
function extraterrestrial_radiation_Ra(rLatitude,rDelta,rOmega_s,rDsubR) result(rRa)

  ! [ ARGUMENTS ]
  real (kind=c_double), intent(in) :: rLatitude
  real (kind=c_double), intent(in) :: rDelta
  real (kind=c_double), intent(in) :: rOmega_s
  real (kind=c_double), intent(in) :: rDsubR

  ! [ LOCALS ]
  real (kind=c_double) :: rRa
  real (kind=c_double) :: rPartA, rPartB
  real (kind=c_double), parameter :: rGsc = 0.0820_c_double  ! MJ / m**2 / min

  rPartA = rOmega_s * sin(rLatitude) * sin(rDelta)
  rPartB = cos(rLatitude) * cos(rDelta) * sin(rOmega_s)


  rRa = 24_c_double * 60_c_double * rGsc * rDsubR * (rPartA + rPartB) / dpPI

end function extraterrestrial_radiation_Ra

!!***

!--------------------------------------------------------------------------
!!****f* meteorological_functions/equivalent_evaporation
! NAME
!   equivalent_evaporation - returns a radiation value in terms of equivalent
!                            evaporation
! SYNOPSIS
!   Returns a radiation value in terms of equivalent evaporation (mm/day),
!   given an input of radiation in MJ / m**2 / day
!
! INPUTS
!   rR - Input radiation, in MJ / m**2 / day
!
! OUTPUTS
!   rR_ET - Radiation expressed as equivalent evaporation, in mm / day
!
! SOURCE

function equivalent_evaporation(rR) result(rR_ET)

  ! [ ARGUMENTS ]
  real (kind=c_double), intent(in) :: rR

  ! [ LOCALS ]
  real (kind=c_double) :: rR_ET

    rR_ET = rR * 0.408_c_double

end function equivalent_evaporation

!!***
!--------------------------------------------------------------------------
!!****f* meteorological_functions/row_latitude
! NAME
!   row_latitude - scales northern and southern boundary latitudes to the
!                  current grid row.
! SYNOPSIS
!   Returns a latitude value for the current row given the northern and
!   southern boundary latitudes.
!
! INPUTS
!   rNorthLat - Latitude of northern grid boundary in RADIANS
!   rSouthat - Latitude of southern grid boundary in RADIANS
!   iNumRows - Number of rows in current grid
!   iCurrRow - Number of CURRENT grid row
!
! OUTPUTS
!   rRowLat - Return latitude of current row in radians
!
! SOURCE

 function row_latitude(rNorthLat, rSouthLat, iNumRows, iCurrRow) result(rRowLat)

  ! [ ARGUMENTS ]
  real (kind=c_float), intent(in) :: rNorthLat
  real (kind=c_float), intent(in) :: rSouthLat
  integer (kind=c_int), intent(in) :: iNumRows
  integer (kind=c_int), intent(in) :: iCurrRow

  ! [ LOCALS ]
  real (kind=c_double) :: rRowLat

!  print *, rNorthLat,rSouthLat,iCurrRow,iNumRows

  rRowLat = real(rNorthLat, kind=c_double) &
      - ((real(rNorthLat, kind=c_double) - real(rSouthLat, kind=c_double)) &
      * (REAL(iCurrRow,kind=c_double) / REAL(iNumRows,kind=c_double)))

end function row_latitude

!> Return sensible heat exchange between surface and air
!!
!! @param[in] rTSnow Snow temperature, in &deg;C
!! @param[in] rTAvg Mean daily air temperature, in &deg;C
!! @param[in] rWindSpd Wind speed in meters per second
!! @retval rH Sensible heat exchange between surface and air, in
!! kilojoules per square meter
!!
!! @note Implemented as equation 11, Walter and others (2005)
!! @note Reference:
!!   Walter, M.T., Brooks, E.S., McCool, D.K., King, L.G., Molnau, M.
!!   and Boll, J., 2005, Process-based snowmelt modeling:
!!   does it require more input data than temperature-index modeling?:
!!   Journal of Hydrology, v. 300, no. 1-4, p. 65–75.
function sensible_heat_exchange_h(rTSnow, rTAvg, rWindSpd) result(rH)

  real (kind=c_float) :: rTSnow
  real (kind=c_float) :: rTAvg
  real (kind=c_float), optional :: rWindSpd   ! wind speed in meters per second
  real(kind=c_double) :: rH

  ! [ LOCALS ]
  !> heat capacity of air; kJ per cubic meter per degree C
  real (kind=c_float), parameter :: rCa = 0.93
  !> density of air; kg per cubic meter
  real (kind=c_float), parameter :: rRho_air = 1.29
  real (kind=c_double) :: rWindSpeed
  !> @todo Check on the origins of the value specified for rTurbConst
  real (kind=c_double) :: rTurbConst = 385.16_c_double
  real (kind=c_float) :: rRh

  if(present(rWindspd)) then
    rWindSpeed = rWindSpd
  else
    rWindSpeed = 2.0
  end if

  rRh = rTurbConst / rWindSpeed

  rH = 86400 * rCa * rRho_air *(FtoC(rTAvg) - FtoC(rTSnow)) / rRh

end function sensible_heat_exchange_h

!--------------------------------------------------------------------------
!!****f* meteorological_functions/convective_heat_exchange_e
! NAME
!   convective_heat_exchange_e - Returns convective heat exchange between
!                              surface and air
! SYNOPSIS
!   Returns sensible heat exchange between surface and air
!
! INPUTS
!
!   rTSnow - Snow temperature in degrees Farenheit
!   rTMin - Minimum air temperature in degrees Farenheit
!   rWindSpd - Average windspeed in meters per second (OPTIONAL)
!
! OUTPUTS
!   rE - Returns convective heat exchange between surface and air in
!        kJ per square meter
!
! SOURCE
!
! (equation 13)
!
! Walter, M.T.., Brooks, E.S., McCool, D.K., King, L.G., Molnau, M.
!   and Boll, J., 2005, Process-based snowmelt modeling:
!   does it require more input data than temperature-index modeling?:
!   Journal of Hydrology, v. 300, no. 1-4, p. 65–75.
function convective_heat_exchange_e(rTSnow, rTAvg, rWindSpd) result(rE)

  real (kind=c_float) :: rTSnow
  real (kind=c_float) :: rTAvg
  real (kind=c_float), optional :: rWindSpd   ! wind speed in meters per second
  real(kind=c_double) :: rE

  ! [ LOCALS ]
  real (kind=c_float), parameter :: rLambda_v = 2500.0
                                              ! kJ per kg
                                              !(latent heat of vaporization)

  real (kind=c_float) :: rRho_surface   ! vapor density at snow surface
  real (kind=c_float) :: rRho_air       ! vapor density of air

  real (kind=c_double) :: rWindSpeed

  ! rTurbConst derived from equation 12 in the reference by assigning
  ! reasonable default values to the parameters and combining into a constant
  !
  ! R code follows:
  ! zu <- 2
  ! d <- 0
  ! zm <- 0.001
  ! a <- log((zu - d + zm) / zm)
  ! a
  ! [1] 7.601402
  ! zT <- 1
  ! zh <- 0.0002
  ! b <- log((zT - d + zh) / zh)
  ! b
  ! [1] 8.517393
  ! a * b / 86400 / 0.41^2
  ! [1] 0.0044577833643
!  real (kind=c_double) :: rTurbConst = 0.0044577833643_c_double
  real (kind=c_double) :: rTurbConst = 385.16_c_double

  real (kind=c_float) :: rRv                     ! resistance to heat x-fer in
                                               ! days per meter
  if(present(rWindspd)) then
    rWindSpeed = rWindSpd
  else
    rWindSpeed = 2.0
  end if

  rRv = rTurbConst / rWindSpeed

  ! function saturated_vapor_pressure returns vp in kilopascals- must convert
  ! to kg per cubic meter
  rRho_surface = sat_vapor_density(rTSnow)
  rRho_air = sat_vapor_density(rTAvg)

  rE = 86400 * rLambda_v * ( rRho_air - rRho_surface) / rRv

  return

end function convective_heat_exchange_e

!--------------------------------------------------------------------------

function net_shortwave_radiation_Rns(rRs, rAlbedo)  result(rRns)
!!***
!--------------------------------------------------------------------------
!!****f* meteorological_functions/net_shortwave_radiation_Rns
! NAME
!   net_shortwave_radiation_Rns - Calculates net shortwave radiation
! SYNOPSIS
!   Calculates net shortwave radiation
!
! INPUTS
!   rRs     - incoming shortwave solar radiation, in MJ / m**2 / day
!   rAlbedo - Albedo or canopy reflection coefficient; 0.23 for grass reference crop
!
! OUTPUTS
!   rRns - net shortwave radiation in MJ / m**2 / day
!
! NOTES
!
!  Implemented as equation 38, Allen and others (2006).
!
!   Reference:
!   Allen, R.G., and others, 2006, FAO Irrigation and Drainage Paper No. 56,
!    "Crop Evapotranspiration (Guidelines for computing crop water
!    requirements)", Food and Agriculture Organization, Rome, Italy.
!
! SOURCE

  real(kind=c_double), intent(in) :: rRs
  real(kind=c_double), intent(in) :: rAlbedo

  ! [ LOCALS ]
  real(kind=c_double) :: rRns

  rRns = (dpONE - rAlbedo) * rRs

end function net_shortwave_radiation_Rns

!!***
!--------------------------------------------------------------------------
!!****f* meteorological_functions/precipitation_heat_p
! NAME
!   precipitation_heat_p - Calculates heat content of precipitation
! SYNOPSIS
!   Calculates the heat content of precipitation
!
! INPUTS
!
! OUTPUTS
!
! NOTES
!
! (equation 15)
!
! Walter, M.T.., Brooks, E.S., McCool, D.K., King, L.G., Molnau, M.
!   and Boll, J., 2005, Process-based snowmelt modeling:
!   does it require more input data than temperature-index modeling?:
!   Journal of Hydrology, v. 300, no. 1-4, p. 65–75.

function precipitation_heat_p(rPrecipAmount, rTAvg) result(rP)

  real (kind=c_float) :: rPrecipAmount
  real (kind=c_float) :: rTAvg
  real (kind=c_float) :: rP

  ! [ LOCALS ]
  real (kind=c_double) :: rPrecipAmount_meters
  real (kind=c_double), parameter :: rCw = 4.2E+03  ! kJ per cubic meter per deg C

  rPrecipAmount_meters = real(rPrecipAmount, kind=c_double) / 12_c_double &
          * 0.3048_c_double

  ! this is supposed to represent the amount of heat added to snowpack
  ! when rain falls on the snowpack; restrict rainfall temperature to
  ! values > freezing
  rP = MAX(FtoC(rTAvg),rZERO) * rCw * rPrecipAmount_meters

  return

end function precipitation_heat_p

!!***
!--------------------------------------------------------------------------
!!****f* meteorological_functions/solar_declination
! NAME
!   solar_declination - Calculates the solar declination for a given day of the year.
! SYNOPSIS
!   Calculates the solar declination for a given day of the year.
!
! INPUTS
!   iDayOfYear - Integer day of the year (January 1 = 1)
!   iNumDaysInYear - Number of days in the current year
!
! OUTPUTS
!   rDelta - Solar declination in RADIANS
!
! SOURCE

function solar_declination(iDayOfYear, iNumDaysInYear) result(rDelta)

  ! [ ARGUMENTS ]
  integer (kind=c_int), intent(in) :: iDayOfYear
  integer (kind=c_int), intent(in) :: iNumDaysInYear

  ! [ LOCALS ]
  real (kind=c_double) :: rDelta

  rDelta = 0.409_c_double &
           * sin ( (2_c_double * dpPI &
           * real(iDayOfYear, kind=c_double) / real(iNumDaysInYear, kind=c_double)) &
		          - 1.39_c_double)

end function solar_declination

!!***
!--------------------------------------------------------------------------
!!****f* meteorological_functions/rel_Earth_Sun_dist
! NAME
!   rel_Earth_Sun_dist - Calculates the relative Earth-Sun distance
!                        for a given day of the year.
! SYNOPSIS
!   Calculates the relative Earth-Sun distance for a given day of the year.
!
! INPUTS
!   iDayOfYear - Integer day of the year (January 1 = 1)
!   iNumDaysInYear - Number of days in the current year
!
! OUTPUTS
!   rDsubR - Relative Earth-Sun distance
!
! NOTES
!
!  Implemented as equation 23, Allen and others (2006).
!
!   Reference:
!   Allen, R.G., and others, 2006, FAO Irrigation and Drainage Paper No. 56,
!    "Crop Evapotranspiration (Guidelines for computing crop water
!    requirements)", Food and Agriculture Organization, Rome, Italy.

! SOURCE

function rel_Earth_Sun_dist(iDayOfYear,iNumDaysInYear) result(rDsubR)

  ! [ ARGUMENTS ]
  integer (kind=c_int), intent(in) :: iDayOfYear
  integer (kind=c_int), intent(in) :: iNumDaysInYear

  ! [ LOCALS ]
  real (kind=c_float) :: rDsubR

  rDsubR = 1_c_float + 0.033_c_float &
           * cos ( 2_c_float * dpPI * iDayOfYear / iNumDaysInYear )
  return

end function rel_Earth_Sun_dist

!!***

!--------------------------------------------------------------------------
!!****f* meteorological_functions/sunset_angle
! NAME
!   sunset_angle - calculates sunset angle in RADIANS.
! SYNOPSIS
!   Calculates sunset angle in RADIANS for a given latitude.
!
! INPUTS
!   rLatitude - Latitude in RADIANS
!   rDelta - Solar declination in RADIANS
!
! OUTPUTS
!   rOmega_s - Sunset angle in RADIANS
!
! NOTES
!
!  Implemented as equation 25, Allen and others (2006).
!
!   Reference:
!   Allen, R.G., and others, 2006, FAO Irrigation and Drainage Paper No. 56,
!    "Crop Evapotranspiration (Guidelines for computing crop water
!    requirements)", Food and Agriculture Organization, Rome, Italy.

! SOURCE

 function sunset_angle(rLatitude, rDelta) result(rOmega_s)

  ! [ ARGUMENTS ]
  real (kind=c_double), intent(in) :: rLatitude
  real (kind=c_double), intent(in) :: rDelta

  ! [ LOCALS ]
  real (kind=c_double) :: rOmega_s

  call Assert(rLatitude <1.58 .and. rLatitude > -1.58, &
    "Internal programming error: Latitude must be expressed in RADIANS", &
    TRIM(__FILE__),__LINE__)

  rOmega_s = acos( - tan(rLatitude) * tan(rDelta) )

  return

end function sunset_angle
!!***

!--------------------------------------------------------------------------
!!****f* meteorological_functions/solar_radiation_Hargreaves_Rs
! NAME
!   solar_radiation_Rs - Calculates shortwave solar radiation.
!
! SYNOPSIS
!   Calculates the solar radiation using Hargreave's radiation formula.
!   For use when percent possible daily sunshine value is not available.
!
! INPUTS
!   rRa - Extraterrestrial radiation in MJ / m**2 / day
!   rTMIN - Minimum daily air temperature in Degrees FAHRENHEIT
!   rTMAX - Maximum daily air temperature in Degrees FAHRENHEIT
!
! OUTPUTS
!   rRa - Solar radiation in MJ / m**2 / day
!
! NOTES
!
!  Implemented as equation 50, Allen and others (2006).
!
!   Reference:
!   Allen, R.G., and others, 2006, FAO Irrigation and Drainage Paper No. 56,
!    "Crop Evapotranspiration (Guidelines for computing crop water
!    requirements)", Food and Agriculture Organization, Rome, Italy.
!
! SOURCE

function solar_radiation_Hargreaves_Rs(rRa, rTMIN, rTMAX) result(rRs)

  ! [ ARGUMENTS ]
  real (kind=c_double), intent(in) :: rRa
  real (kind=c_float), intent(in) :: rTMIN
  real (kind=c_float), intent(in) :: rTMAX

  ! [ LOCALS ]
  real (kind=c_double) :: rRs
  real (kind=c_double), parameter :: rKRs = 0.17

  rRs = rKRs * sqrt(FtoK(rTMAX) - FtoK(rTMIN)) * rRa

  return

end function solar_radiation_Hargreaves_Rs

!!***
!--------------------------------------------------------------------------
function estimate_percent_of_possible_sunshine(rTMAX, rTMIN)  result(rPsun)

  ! this function follows from equation 5 in "The Rational Use of the FAO Blaney-
  ! Criddle
  ! substituting the rearranged Hargreaves solar radiation formula into
  ! equation 5 results in the formulation below

  ! [ ARGUMENTS ]
  real (kind=c_float), intent(in) :: rTMIN
  real (kind=c_float), intent(in) :: rTMAX

  ! [ RETURNS ]

  real (kind=c_float) :: rPsun

  ! [ LOCALS ]
  real (kind=c_float), parameter :: rKRs = 0.175

  rPsun = ( 2_c_float * rKRs * sqrt(FtoK(rTMAX) - FtoK(rTMIN)) ) - 0.5_c_float

  if (rPsun < 0_c_float) then
    rPsun = 0_c_float
  elseif (rPsun > 1.0_c_float) then
    rPsun = 100_c_float
  else
    rPsun = rPsun * 100_c_float
  endif

end function estimate_percent_of_possible_sunshine

!!***
!--------------------------------------------------------------------------
!!****f* meteorological_functions/clear_sky_solar_radiation_Rso
! NAME
!   clear_sky_solar_radiation_Rso - Calculates the clear sky solar radiation.
!
! SYNOPSIS
!   Calculates the clear sky solar radiation (i.e. when rPctSun = 100,
!   n/N=1.  Required for computing net longwave radiation.
!
! INPUTS
!   rRa - Extraterrestrial radiation in MJ / m**2 / day
!   rAs - Solar radiation regression constant, expressing the fraction
!         of extraterrestrial radiation that reaches earth on OVERCAST days.
!   rBs - Solar radiation regression constant. As + Bs express the fraction
!         of extraterrestrial radiation that reaches earth on CLEAR days.
!
! OUTPUTS
!   rRso - Clear sky solar radiation in MJ / m**2 / day
!
! NOTES
!
!  Implemented as equation 36, Allen and others (2006).
!
!   Reference:
!   Allen, R.G., and others, 2006, FAO Irrigation and Drainage Paper No. 56,
!    "Crop Evapotranspiration (Guidelines for computing crop water
!    requirements)", Food and Agriculture Organization, Rome, Italy.
!
! SOURCE

function clear_sky_solar_radiation_Rso(rRa, rAs_in, rBs_in) result(rRso)

  ! [ ARGUMENTS ]
  real (kind=c_double), intent(in) :: rRa
  real (kind=c_double), intent(in), optional :: rAs_in
  real (kind=c_double), intent(in),optional :: rBs_in

  ! [ LOCALS ]
  real (kind=c_double) :: rRso
  real (kind=c_double) :: rAs
  real (kind=c_double) :: rBs

  ! assign default value to As if none is provided
  if(present(rAs_in)) then
    rAs = rAs_in
  else
    rAs = 0.25_c_double

  end if

  ! assign default value to Bs if none is provided
  if(present(rBs_in)) then
    rBs = rBs_in
  else
    rBs = 0.5_c_double
  end if

  rRso = (rAs + rBs) * rRa

end function clear_sky_solar_radiation_Rso

!!***
!--------------------------------------------------------------------------
!!****f* meteorological_functions/clear_sky_solar_radiation_noAB_Rso
! NAME
!   clear_sky_solar_radiation_noAB_Rso - Calculates the clear sky solar radiation.
!
! SYNOPSIS
!   Calculates the clear sky solar radiation (i.e. when rPctSun = 100,
!   n/N=1.  Required for computing net longwave radiation.
!   For use when no regression coefficients (A, B) are known.
!
! INPUTS
!   rRa - Extraterrestrial radiation in MJ / m**2 / day
!   eElevation - elevation in METERS above sea level
!
! OUTPUTS
!   rRso - Clear sky solar radiation in MJ / m**2 / day
!
! NOTES
!
!  Implemented as equation 37, Allen and others (2006).
!
!   Reference:
!   Allen, R.G., and others, 2006, FAO Irrigation and Drainage Paper No. 56,
!    "Crop Evapotranspiration (Guidelines for computing crop water
!    requirements)", Food and Agriculture Organization, Rome, Italy.
!
! SOURCE

function clear_sky_solar_radiation_noAB_Rso(rRa, rElevation) result(rRso)

  ! [ ARGUMENTS ]
  real (kind=c_double), intent(in) :: rRa
  real (kind=c_double), intent(in) :: rElevation

  ! [ LOCALS ]
  real (kind=c_float) :: rRso

  rRso = (0.75_c_double + 1.0E-5_c_double * rElevation) * rRa

end function clear_sky_solar_radiation_noAB_Rso

!!***
!--------------------------------------------------------------------------
!!****f* meteorological_functions/solar_radiation_Rs
! NAME
!   solar_radiation_Rs - Calculates the solar radiation.
!
! SYNOPSIS
!   Calculates the solar radiation using the Angstrom formula.
!
! INPUTS
!   rRa - Extraterrestrial radiation in MJ / m**2 / day
!   rAs - Solar radiation regression constant, expressing the fraction
!         of extraterrestrial radiation that reaches earth on OVERCAST days.
!   rBs - Solar radiation regression constant. As + Bs express the fraction
!         of extraterrestrial radiation that reaches earth on CLEAR days.
!   rPctSun - Percent of TOTAL number of sunshine hours during which the
!             sun actually shown.
!
! OUTPUTS
!   rRs - Solar radiation in MJ / m**2 / day
!
! NOTES
!
!  Implemented as equation 35, Allen and others (2006).
!
!   Reference:
!   Allen, R.G., and others, 2006, FAO Irrigation and Drainage Paper No. 56,
!    "Crop Evapotranspiration (Guidelines for computing crop water
!    requirements)", Food and Agriculture Organization, Rome, Italy.
!
! SOURCE

function solar_radiation_Rs(rRa, rAs, rBs, rPctSun) result(rRs)

  ! [ ARGUMENTS ]
  real (kind=c_double), intent(in) :: rRa
  real (kind=c_double), intent(in) :: rAs
  real (kind=c_double), intent(in) :: rBs
  real (kind=c_double), intent(in) :: rPctSun

  ! [ LOCALS ]
  real (kind=c_double) :: rRs

  rRs = ( rAs + (rBs * rPctSun / 100_c_float)) * rRa

  return

end function solar_radiation_Rs

!!***

!--------------------------------------------------------------------------
!!****f* meteorological_functions/sat_vapor_pressure_es
! NAME
!   sat_vapor_pressure_es - Calculates the mean saturation
!                                vapor pressure
!
! SYNOPSIS
!   Calculates the mean saturation vapor pressure for a given temperature.
!
! INPUTS
!   rT - Air temperature, in degrees FAHRENHEIT
!
! OUTPUTS
!   re_0 - Saturation vapor pressure at temperature, in kiloPascals
!
! NOTES
!
!  Implemented as equation 11 and 12, Allen and others (2006).
!
!   Reference:
!   Allen, R.G., and others, 2006, FAO Irrigation and Drainage Paper No. 56,
!    "Crop Evapotranspiration (Guidelines for computing crop water
!    requirements)", Food and Agriculture Organization, Rome, Italy.
!
! SOURCE

function sat_vapor_pressure_es(rT) result (re_0)

  ! [ ARGUMENTS ]
  real (kind=c_float), intent(in) :: rT

  ! [ LOCALS ]
  real (kind=c_float) :: re_0

  re_0 = 0.6108_c_double * exp (17.27_c_double * FtoC(rT) &
             / (FtoC(rT) + 237.3_c_double))

end function sat_vapor_pressure_es

!--------------------------------------------------------------------------

function sat_vapor_density(rT) result(rRho)

  real(kind=c_float), intent(in):: rT

  ! [ LOCALS ]
  real(kind=c_double) :: rRho
  real (kind=c_double), parameter :: rR = 0.4615_c_double     ! kJ per kg per deg K


  rRho = exp((16.78_c_double * FtoC(rT) - 116.8_c_double) / (FtoK(rT))) &
         * (1. / ((FtoK(rT)) * rR))

end function sat_vapor_density

!--------------------------------------------------------------------------
!!****f* meteorological_functions/dewpoint_vapor_pressure_ea
! NAME
!   dewpoint_vapor_pressure_ea - Calculates the mean saturation
!                                vapor pressure
!
! SYNOPSIS
!   Estimates the dewpoint vapor pressure for a given minimum air temperature.
!
! INPUTS
!   rTMIN - Minimum daily air temperature, in degrees FAHRENHEIT
!
! OUTPUTS
!   re_d - Dewpoint vapor pressure, estimated from at minimum air
!          temperature, in kiloPascals
!
! NOTES
!
!  Implemented as equation 14, 48, Allen and others (2006).
!
!   Reference:
!   Allen, R.G., and others, 2006, FAO Irrigation and Drainage Paper No. 56,
!    "Crop Evapotranspiration (Guidelines for computing crop water
!    requirements)", Food and Agriculture Organization, Rome, Italy.
!
! SOURCE

function dewpoint_vapor_pressure_ea(rTMIN) result (re_a)

  ! [ ARGUMENTS ]
  real (kind=c_float), intent(in) :: rTMIN

  ! [ LOCALS ]
  real (kind=c_double) :: re_a

  re_a = 0.6108_c_double * exp (17.27_c_double * FtoC(rTMIN) &
             / (FtoC(rTMIN) + 237.3_c_double))

end function dewpoint_vapor_pressure_ea
!!***
!--------------------------------------------------------------------------
!!****f* meteorological_functions/minimum_rel_hum
! NAME
!   minimum_rel_hum - estimates minimum daily relative humidity
!
! SYNOPSIS
!   Returns an estimate of the minimum relative humidity given the
!   daily minimum and maximum air temperatures.
!
! INPUTS
!   rTMin - Minimum daily air temperature, in degrees FAHRENHEIT
!   rTMax - Maximum daily air temperature, in degrees FAHRENHEIT
!
! OUTPUTS
!   rMinRH - Minimum estimated relative humidity, in percent
!
! NOTES
!
!  Reference
!
!  Tetens (1930), original unseen.  See the following URL for more detail:
!
!  http://biomet.ucdavis.edu/evapotranspiration/NWSETo/NWSETo.htm
!
! SOURCE

function minimum_rel_hum(rTMin, rTMax) result (rMinRH)

  ! [ ARGUMENTS ]
  real (kind=c_float), intent(in) :: rTMin, rTMax

  ! [ LOCALS ]
  real (kind=c_double) :: rMinRH, re_a, re_x

  re_a = dewpoint_vapor_pressure_ea(rTMin)
  re_x = sat_vapor_pressure_es(rTMax)

  rMinRH = 100_c_double * re_a / re_x

end function minimum_rel_hum

!!***
!--------------------------------------------------------------------------
!!****f* meteorological_functions/maximum_rel_hum
! NAME
!   maximum_rel_hum - estimates maximum daily relative humidity
!
! SYNOPSIS
!   Returns an estimate of the maximum relative humidity given the
!   daily minimum and maximum air temperatures.
!
! INPUTS
!   rTMin - Minimum daily air temperature, in degrees FAHRENHEIT
!   rTMax - Maximum daily air temperature, in degrees FAHRENHEIT
!
! OUTPUTS
!   rMaxRH - Maximum estimated relative humidity, in percent
!
! NOTES
!
!  Reference
!
!  Tetens (1930), original unseen.  See the following URL for more detail:
!
!  http://biomet.ucdavis.edu/evapotranspiration/NWSETo/NWSETo.htm
!
! SOURCE

function maximum_rel_hum(rTMin) result (rMaxRH)

  ! [ ARGUMENTS ]
  real (kind=c_float), intent(in) :: rTMin

  ! [ LOCALS ]
  real (kind=c_double) :: rMaxRH, re_a, re_n

  re_a = dewpoint_vapor_pressure_ea(rTMin)
  re_n = sat_vapor_pressure_es(rTMin)

  rMaxRH = 100_c_float * re_a / re_n

end function maximum_rel_hum

!!***
!--------------------------------------------------------------------------
!!****f* meteorological_functions/net_longwave_radiation_Rnl
! NAME
!   net_longwave_radiation_Rnl - estimates net longwave radiation term
!
! SYNOPSIS
!   Calculates net outgoing longwave radiation flux
!
! INPUTS
!   rTMin - Minimum daily air temperature, in degrees FAHRENHEIT
!   rTMax - Maximum daily air temperature, in degrees FAHRENHEIT
!   rRs - measured or calculated shortwave solar radiation, in MJ / m**2 / day
!   rRso - calculated clear-sky radiation, in MJ / m**2 / day
!
! OUTPUTS
!   rRnl - net longwave solar radiation flux (incoming minus outgoing)
!
! NOTES
!
!  Implemented as equation 39, Allen and others (2006).
!
!   Reference:
!   Allen, R.G., and others, 2006, FAO Irrigation and Drainage Paper No. 56,
!    "Crop Evapotranspiration (Guidelines for computing crop water
!    requirements)", Food and Agriculture Organization, Rome, Italy.
!
! SOURCE

function net_longwave_radiation_Rnl(rTMin, rTMax, rRs, rRso)  result(rRnl)

  real(kind=c_float), intent(in) :: rTMin
  real(kind=c_float), intent(in) :: rTMax
  real(kind=c_double), intent(in) :: rRs
  real(kind=c_double), intent(in) :: rRso

  ! [ LOCALS ]
  real(kind=c_double) :: rRnl
  real(kind=c_double) :: rTAvg_K
  real(kind=c_double) :: rTAvg_4

  real (kind=c_double) :: r_ea
  real (kind=c_double) :: rCloudFrac
  real (kind=c_double),parameter :: rSIGMA = 4.903E-9_c_double

  rTAvg_K = FtoK((rTMin + rTMax )/ 2.)

  rTAvg_4 = rTAvg_K * rTAvg_K * rTAvg_K * rTAvg_K * rSIGMA
  r_ea = dewpoint_vapor_pressure_ea(rTMin)

  rCloudFrac = min(rRs / rRso, 1.0)

  rRnl = rTAvg_4 * (0.34_c_double - 0.14_c_double * sqrt(r_ea)) &
          * (1.35_c_double * rCloudFrac - 0.35_c_double)

  return

end function net_longwave_radiation_Rnl


!!***
!--------------------------------------------------------------------------
!!****f* meteorological_functions/zenith_angle
! NAME
!   zenith_angle - calculates zenith angle at NOON
!
! SYNOPSIS
!   Estimates solar zenith angle at NOON given latitude and declination
!
! INPUTS
!   rLatitude - latitude of location for which estimate is being made (RADIANS)
!   rDelta - solar declination angle in RADIANS
!
! OUTPUTS
!   rZenithAngle - solar zenith angle for given location and time, in RADIANS
!
! NOTES
!
!  Implemented as equation 10.53, Jacobson, 1999
!
!   Reference:
!     Jacobson, M.Z., 1999, Fundamentals of atmospheric modeling:
!     Cambridge University Press.
!
! SOURCE

function zenith_angle(rLatitude, rDelta) result(rZenithAngle)

  real (kind=c_double), intent(in) :: rLatitude
  real (kind=c_double), intent(in) :: rDelta

  ! [ LOCALS ]
  real (kind=c_double) :: rZenithAngle

  call Assert(rLatitude <1.58 .and. rLatitude > -1.58, &
    "Internal programming error: Latitude must be expressed in RADIANS", &
    TRIM(__FILE__),__LINE__)


  rZenithAngle = sin(rLatitude) * sin(rDelta) + &
                   cos(rLatitude) * cos(rDelta)

  return


end function zenith_angle

!!***
!--------------------------------------------------------------------------
!!****f* meteorological_functions/slope_sat_vapor_pressure_curve
! NAME
!   slope_sat_vapor_pressure_curve - Calculates slope of the vapor pressure
!                                    curve.
!
! SYNOPSIS
!   Calculates the slope of the vapor pressure curve at a given temperature.
!
! INPUTS
!   rT - Air temperature, in degrees FAHRENHEIT
!
! OUTPUTS
!   re_0 - Saturation vapor pressure at temperature, in kiloPascals
!
! NOTES
!
!  Implemented as equation 13, Allen and others (2006).
!
!   Reference:
!   Allen, R.G., and others, 2006, FAO Irrigation and Drainage Paper No. 56,
!    "Crop Evapotranspiration (Guidelines for computing crop water
!    requirements)", Food and Agriculture Organization, Rome, Italy.
!
! SOURCE

function slope_sat_vapor_pressure_curve(rT) result (rSlope)

  ! [ ARGUMENTS ]
  real (kind=c_double), intent(in) :: rT

  ! [ LOCALS ]
  real (kind=c_double) :: rSlope

  rSlope = 4098_c_double * 0.6108_c_double * exp (17.27_c_double * FtoC(rT) &
                                       / (FtoC(rT) + 237.3_c_double)) &
			 / ((FtoC(rT) + 237.3_c_double)**2)

end function slope_sat_vapor_pressure_curve

end module meteorological_functions
