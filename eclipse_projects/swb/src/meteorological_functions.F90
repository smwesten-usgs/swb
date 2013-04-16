!> @file
!> @brief  Contains a single module, @ref meteorological_functions, which
!>  provides support functions used by evapotranspiration and snow routines.

!> @brief  Provides support functions used by evapotranspiration and snow routines.
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

use types
implicit none

contains

!--------------------------------------------------------------------------
!!****f* meteorological_functions/daylight_hours
! NAME
!   daylight_hours - Calculates daylight hours at a location.
! SYNOPSIS
!   Calculates daylight hours given the sunset hour angle (Omega_s).
!
! INPUTS
!   rOmega_s -
!
! OUTPUTS
!   rN - Daylight hours, in hours.
!
! NOTES
!
!  Implemented as equation 34, Allen and others (2006).
!
!   Reference:
!   Allen, R.G., and others, 2006, FAO Irrigation and Drainage Paper No. 56,
!    "Crop Evapotranspiration (Guidelines for computing crop water
!    requirements)", Food and Agriculture Organization, Rome, Italy.
!
! SOURCE


!!  The distance between \f$(x_1,y_1)\f$ and \f$(x_2,y_2)\f$ is
!!  \f$\sqrt{(x_2-x_1)^2+(y_2-y_1)^2}\f$.


!> @brief Calculates the number of daylight hours at a location.
!>
!> @param  rOmega_s The sunset hour angle in Radians.
!> @return rN The number of daylight hours.
!>
!> @note Implemented as equation 34, Allen and others (2006).
!> @par
!>   Allen, R.G., and others, 2006, FAO Irrigation and Drainage Paper No. 56,
!>   "Crop Evapotranspiration (Guidelines for computing crop water
!>   requirements)", Food and Agriculture Organization, Rome, Italy.


 function daylight_hours(rOmega_s) result(rN)

  ! [ ARGUMENTS ]
  real (kind=T_SGL), intent(in) :: rOmega_s

  ! [ LOCALS ]
  real (kind=T_SGL) :: rN

  rN = 24_T_SGL / dpPI * rOmega_s

  return

end function daylight_hours

!!***
!--------------------------------------------------------------------------
!!****f* meteorological_functions/extraterrestrial_radiation_Ra
! NAME
!   extraterrestrial_radiation_Ra - Calculates extraterrestrial radiation
!                                   at a given location and time of year.
! SYNOPSIS
!   Calculates extraterrestrial radiation given latitude and time of year.
!
! INPUTS
!   rLatitude - Latitude of grid cell in RADIANS
!   rDelta - Solar declination in RADIANS
!   rOmega_s - Sunset hour angle in RADIANS
!   rDsubR - Inverse relative distance Earth-Sun
!
! OUTPUTS
!   rRa - Extraterrestrial radiation in MJ / m**2 / day
!
!  1 MJ = 1e6 Joules
!  1 Joule = 1 Watt / sec
!  Therefore, multiply by 1e6 and divide by 86400 to get W/m*2-day
!
! NOTES
!
!  Implemented as equation 21, Allen and others (2006).
!
!   Reference:
!   Allen, R.G., and others, 2006, FAO Irrigation and Drainage Paper No. 56,
!    "Crop Evapotranspiration (Guidelines for computing crop water
!    requirements)", Food and Agriculture Organization, Rome, Italy.
!
! SOURCE

function extraterrestrial_radiation_Ra(rLatitude,rDelta,rOmega_s,rDsubR) result(rRa)

  ! [ ARGUMENTS ]
  real (kind=T_SGL), intent(in) :: rLatitude
  real (kind=T_SGL), intent(in) :: rDelta
  real (kind=T_SGL), intent(in) :: rOmega_s
  real (kind=T_SGL), intent(in) :: rDsubR

  ! [ LOCALS ]
  real (kind=T_SGL) :: rRa
  real (kind=T_SGL) :: rPartA, rPartB
  real (kind=T_SGL), parameter :: rGsc = 0.0820  ! MJ / m**2 / min

  rPartA = rOmega_s * sin(rLatitude) * sin(rDelta)
  rPartB = cos(rLatitude) * cos(rDelta) * sin(rOmega_s)


  rRa = 24_T_SGL * 60_T_SGL * rGsc * rDsubR * (rPartA + rPartB) / dpPI

  return

end function extraterrestrial_radiation_Ra

!!***
!--------------------------------------------------------------------------
!!****f* meteorological_functions/deg2rad
! NAME
!   deg2rad - converts decimal degrees to a value in radians.
! SYNOPSIS
!   Returns a value in radians given a value in decimal degrees.
!
! INPUTS
!   rDeg - Value in decimal degrees.
!
! OUTPUTS
!   rRad - Return value in radians
!
! SOURCE

function deg2rad(rDeg) result(rRad)

  ! [ ARGUMENTS ]
  real (kind=T_SGL), intent(in) :: rDeg

  ! [ LOCALS ]
  real (kind=T_SGL) :: rRad

  rRad = dpPI / 180_T_SGL * rDeg

  return

end function deg2rad

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
  real (kind=T_SGL), intent(in) :: rR

  ! [ LOCALS ]
  real (kind=T_SGL) :: rR_ET

    rR_ET = rR * 0.408_T_SGL

  return

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
  real (kind=T_SGL), intent(in) :: rNorthLat
  real (kind=T_SGL), intent(in) :: rSouthLat
  integer (kind=T_INT), intent(in) :: iNumRows
  integer (kind=T_INT), intent(in) :: iCurrRow

  ! [ LOCALS ]
  real (kind=T_SGL) :: rRowLat

!  print *, rNorthLat,rSouthLat,iCurrRow,iNumRows

  rRowLat = rNorthLat - ((rNorthLat - rSouthLat) &
      * (REAL(iCurrRow,kind=T_SGL) / REAL(iNumRows,kind=T_SGL)))

  return

end function row_latitude

!!***
!--------------------------------------------------------------------------
!!****f* meteorological_functions/sensible_heat_exchange_h
! NAME
!   sensible_heat_exchange_h - Returns sensible heat exchange between
!                              surface and air
! SYNOPSIS
!   Returns sensible heat exchange between surface and air
!
! INPUTS
!
! OUTPUTS
!   rH - Returns sensible heat exchange between surface and air in
!        kJ per square meter
!
! SOURCE
!
! (equation 11)
!
! Walter, M.T.., Brooks, E.S., McCool, D.K., King, L.G., Molnau, M.
!   and Boll, J., 2005, Process-based snowmelt modeling:
!   does it require more input data than temperature-index modeling?:
!   Journal of Hydrology, v. 300, no. 1-4, p. 65–75.

function sensible_heat_exchange_h(rTSnow, rTAvg, rWindSpd) result(rH)

  real (kind=T_SGL) :: rTSnow
  real (kind=T_SGL) :: rTAvg
  real (kind=T_SGL), optional :: rWindSpd   ! wind speed in meters per second
  real(kind=T_DBL) :: rH

  ! [ LOCALS ]
  real (kind=T_SGL), parameter :: rCa = 0.93   ! kJ per cubic meter per degree C
                                               ! (heat capacity of air)

  real (kind=T_SGL), parameter :: rRho_air = 1.29  ! kg per cubic meter

  real (kind=T_DBL) :: rWindSpeed

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
!  real (kind=T_DBL) :: rTurbConst = 0.0044577833643_T_DBL
  real (kind=T_DBL) :: rTurbConst = 385.16_T_DBL

  real (kind=T_SGL) :: rRh                     ! resistance to heat x-fer in
                                               ! days per meter
  if(present(rWindspd)) then
    rWindSpeed = rWindSpd
  else
    rWindSpeed = 2.0
  end if

  rRh = rTurbConst / rWindSpeed

  rH = 86400 * rCa * rRho_air *(FtoC(rTAvg) - FtoC(rTSnow)) / rRh

  return

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

  real (kind=T_SGL) :: rTSnow
  real (kind=T_SGL) :: rTAvg
  real (kind=T_SGL), optional :: rWindSpd   ! wind speed in meters per second
  real(kind=T_DBL) :: rE

  ! [ LOCALS ]
  real (kind=T_SGL), parameter :: rLambda_v = 2500.0
                                              ! kJ per kg
                                              !(latent heat of vaporization)

  real (kind=T_SGL) :: rRho_surface   ! vapor density at snow surface
  real (kind=T_SGL) :: rRho_air       ! vapor density of air

  real (kind=T_DBL) :: rWindSpeed

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
!  real (kind=T_DBL) :: rTurbConst = 0.0044577833643_T_DBL
  real (kind=T_DBL) :: rTurbConst = 385.16_T_DBL

  real (kind=T_SGL) :: rRv                     ! resistance to heat x-fer in
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

  real(kind=T_SGL), intent(in) :: rRs
  real(kind=T_SGL), intent(in) :: rAlbedo

  ! [ LOCALS ]
  real(kind=T_SGL) :: rRns

  rRns = (rONE - rAlbedo) * rRs

  return

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

  real (kind=T_SGL) :: rPrecipAmount
  real (kind=T_SGL) :: rTAvg
  real (kind=T_SGL) :: rP

  ! [ LOCALS ]
  real (kind=T_DBL) :: rPrecipAmount_meters
  real (kind=T_DBL), parameter :: rCw = 4.2E+03  ! kJ per cubic meter per deg C

  rPrecipAmount_meters = real(rPrecipAmount, kind=T_DBL) / 12_T_DBL &
          * 0.3048_T_DBL

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
  integer (kind=T_INT), intent(in) :: iDayOfYear
  integer (kind=T_INT), intent(in) :: iNumDaysInYear

  ! [ LOCALS ]
  real (kind=T_SGL) :: rDelta

  rDelta = 0.409_T_SGL &
           * sin ( (2_T_SGL * dpPI * iDayOfYear / iNumDaysInYear) &
		          - 1.39_T_SGL)

  return

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
  integer (kind=T_INT), intent(in) :: iDayOfYear
  integer (kind=T_INT), intent(in) :: iNumDaysInYear

  ! [ LOCALS ]
  real (kind=T_SGL) :: rDsubR

  rDsubR = 1_T_SGL + 0.033_T_SGL &
           * cos ( 2_T_SGL * dpPI * iDayOfYear / iNumDaysInYear )
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
  real (kind=T_SGL), intent(in) :: rLatitude
  real (kind=T_SGL), intent(in) :: rDelta

  ! [ LOCALS ]
  real (kind=T_SGL) :: rOmega_s

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
  real (kind=T_SGL), intent(in) :: rRa
  real (kind=T_SGL), intent(in) :: rTMIN
  real (kind=T_SGL), intent(in) :: rTMAX

  ! [ LOCALS ]
  real (kind=T_SGL) :: rRs
  real (kind=T_SGL), parameter :: rKRs = 0.175

  rRs = rKRs * sqrt(FtoC(rTMAX) - FtoC(rTMIN)) * rRa

  return

end function solar_radiation_Hargreaves_Rs
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
  real (kind=T_SGL), intent(in) :: rRa
  real (kind=T_SGL), intent(in), optional :: rAs_in
  real (kind=T_SGL), intent(in),optional :: rBs_in

  ! [ LOCALS ]
  real (kind=T_SGL) :: rRso
  real (kind=T_SGL) :: rAs
  real (kind=T_SGL) :: rBs

  ! assign default value to As if none is provided
  if(present(rAs_in)) then
    rAs = rAs_in
  else
    rAs = 0.25

  end if

  ! assign default value to Bs if none is provided
  if(present(rBs_in)) then
    rBs = rBs_in
  else
    rBs = 0.5
  end if

  rRso = (rAs + rBs) * rRa

  return

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
  real (kind=T_SGL), intent(in) :: rRa
  real (kind=T_SGL), intent(in) :: rElevation

  ! [ LOCALS ]
  real (kind=T_SGL) :: rRso

  rRso = (0.75_T_SGL + 1.0E-5*rElevation) * rRa

  return

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
  real (kind=T_SGL), intent(in) :: rRa
  real (kind=T_SGL), intent(in) :: rAs
  real (kind=T_SGL), intent(in) :: rBs
  real (kind=T_SGL), intent(in) :: rPctSun

  ! [ LOCALS ]
  real (kind=T_SGL) :: rRs

  rRs = ( rAs + (rBs * rPctSun / 100_T_SGL)) * rRa

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
  real (kind=T_SGL), intent(in) :: rT

  ! [ LOCALS ]
  real (kind=T_SGL) :: re_0

  re_0 = 0.6108_T_SGL * exp (17.27_T_SGL * FtoC(rT) &
             / (FtoC(rT) + 237.3_T_SGL))

  return

end function sat_vapor_pressure_es

!--------------------------------------------------------------------------

function sat_vapor_density(rT) result(rRho)

  real(kind=T_SGL), intent(in):: rT

  ! [ LOCALS ]
  real(kind=T_SGL) :: rRho
  real (kind=T_DBL), parameter :: rR = 0.4615     ! kJ per kg per deg K


  rRho = exp((16.78 * FtoC(rT) - 116.8) / (FtoK(rT))) &
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
  real (kind=T_SGL), intent(in) :: rTMIN

  ! [ LOCALS ]
  real (kind=T_SGL) :: re_a

  re_a = 0.6108_T_SGL * exp (17.27_T_SGL * FtoC(rTMIN) &
             / (FtoC(rTMIN) + 237.3_T_SGL))

  return

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
  real (kind=T_SGL), intent(in) :: rTMin, rTMax

  ! [ LOCALS ]
  real (kind=T_SGL) :: rMinRH, re_a, re_x

  re_a = dewpoint_vapor_pressure_ea(rTMin)
  re_x = sat_vapor_pressure_es(rTMax)

  rMinRH = 100_T_SGL * re_a / re_x

  return

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
  real (kind=T_SGL), intent(in) :: rTMin

  ! [ LOCALS ]
  real (kind=T_SGL) :: rMaxRH, re_a, re_n

  re_a = dewpoint_vapor_pressure_ea(rTMin)
  re_n = sat_vapor_pressure_es(rTMin)

  rMaxRH = 100_T_SGL * re_a / re_n

  return

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

  real(kind=T_SGL), intent(in) :: rTMin
  real(kind=T_SGL), intent(in) :: rTMax
  real(kind=T_SGL), intent(in) :: rRs
  real(kind=T_SGL), intent(in) :: rRso

  ! [ LOCALS ]
  real(kind=T_SGL) :: rRnl
  real(kind=T_DBL) :: rTAvg_K
  real(kind=T_DBL) :: rTAvg_4

  real (kind=T_DBL) :: r_ea
  real (kind=T_DBL) :: rCloudFrac
  real (kind=T_DBL),parameter :: rSIGMA = 4.903E-9_T_DBL

  rTAvg_K = FtoK((rTMin + rTMax )/ 2.)

  rTAvg_4 = rTAvg_K * rTAvg_K * rTAvg_K * rTAvg_K * rSigma
  r_ea = dewpoint_vapor_pressure_ea(rTMin)

  rCloudFrac = min(rRs / rRso, 1.0)

  rRnl = rTAvg_4 * (0.34_T_DBL - 0.14_T_DBL * sqrt(r_ea)) &
          * (1.35_T_DBL * rCloudFrac - 0.35_T_DBL)

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

  real (kind=T_SGL), intent(in) :: rLatitude
  real (kind=T_SGL), intent(in) :: rDelta

  ! [ LOCALS ]
  real (kind=T_SGL) :: rZenithAngle

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
  real (kind=T_SGL), intent(in) :: rT

  ! [ LOCALS ]
  real (kind=T_SGL) :: rSlope

  rSlope = 4098_T_SGL * 0.6108_T_SGL * exp (17.27_T_SGL * FtoC(rT) &
                                       / (FtoC(rT) + 237.3_T_SGL)) &
			 / ((FtoC(rT) + 237.3_T_SGL)**2)

  return

end function slope_sat_vapor_pressure_curve

end module meteorological_functions
