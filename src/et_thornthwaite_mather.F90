!> @file
!> @brief  Contains a single module, et_thornthwaite_mather, which
!>  calculates potential evapotranspiration by means of the Thornthwaite-Mather (1955, 1957) method.


!> @brief  Calculates potential evapotranspiration by means of the
!> Thornthwaite-Mather (1955, 1957) method.
module et_thornthwaite_mather
!!****h* SWB/et_thornthwaite_mather
! NAME
!   et_thornthwaite_mather.f95 - Evapotranspiration calculation using the
!   Thornthwaite-Mather method.
!
! SYNOPSIS
!   This module calculates evapotranspiration using the Thornthwaite-Mather
!   method.
!
! NOTES
!   Original method is documented in:
!
!   Thornthwaite, C.W., and Mather, J.R., 1955, The water balance,
!   in Publications in climatology, v. 8, no. 1: Centerton, N.J.,
!   Laboratory of Climatology, p. 1-104.
!
!   Thornthwaite, C.W., and Mather, J.R., 1957, Instructions and tables
!   for computing potential evapotranspiration and the water balance,
!   in Publications in climatology ; v. 10, no. 3: Centerton, N.J.,
!   Laboratory of Climatology, p. 185-311.
!
!!***

  use types

  use stats
  use meteorological_functions
  use datetime

  implicit none

  !! Module data

  !! Configuration -- input data
  real (kind=T_DBL) :: rLatitude
  real (kind=T_DBL) :: rTanLatitude

  !! Parameters from pre-scan of annual data
  real (kind=T_DBL) :: rAnnualIndex        ! Annual thermal index
  real (kind=T_DBL) :: rExponentA        ! Exponent 'a'

contains

subroutine et_tm_configure( sRecord )
  !! Configures the module, using the command line in 'sRecord'
  ! [ ARGUMENTS ]
  character (len=*),intent(inout) :: sRecord
  ! [ LOCALS ]
  character (len=256) :: sOption
  integer (kind=T_INT) :: iStat

  write(UNIT=LU_LOG,FMT=*) "Configuring Thornthwaite-Mather ET model"
  call Chomp( sRecord,sOption )
  read ( unit=sOption, fmt=*, iostat=iStat ) rLatitude
  call Assert( iStat == 0, &
    "Could not read the latitude" )
  rTanLatitude = tan(dpTWOPI * rLatitude / 360.0_T_SGL )
!   write(UNIT=LU_LOG,FMT=*)" ---> ",rLatitude, rTanLatitude

  return
end subroutine et_tm_configure

subroutine et_tm_initialize( pGrd, pConfig, sFileName )
  !! Preconfigures necessary information from the time-series file 'sFileName'
  !! and based on the model grid 'pGrd'.
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  character (len=*),intent(in) :: sFileName
  ! [ LOCALS ]
  integer (kind=T_INT) :: iJulianDay, iStat, iMonth, iDay, iYear,i
  integer (kind=T_INT) :: iDayOfYear
  real(kind=T_SGL) :: rPrecip, rAvgT, rMinT, rMaxT, rRH, rMinRH, rWindSpd, rSunPct
  real(kind=T_DBL) :: rMonthAvg,rMonthIndex
  character (len=256) :: sBuf
  character (len=3) :: sMonthName
  logical (kind=T_LOGICAL) :: lMonthEnd
  logical (kind=T_LOGICAL) :: lOpened
  integer (kind=T_INT) :: iLU
  integer (kind=T_INT), dimension(12) :: iMonthlyCount
  real (kind=T_DBL), dimension(12) :: rMonthlySum
  ! [ LOCAL CONSTANTS ]

  !> exponent value is from Thornthwaite (1948), p 89.
  real (kind=T_DBL),parameter :: rExp=1.514_T_DBL
  write(UNIT=LU_LOG,FMT=*)"Initializing Thornthwaite-Mather ET model with annual data ", trim(sFileName)

  open ( LU_TEMP, file=trim(sFileName), iostat=iStat )
  call Assert ( iStat == 0, "Could not open time series file " // trim(sFileName), &
    trim(__FILE__),__LINE__)

  ! Now, compute the monthly-average temperatures for all months
  iMonthlyCount = 0
  rMonthlySum = rZERO

  do
    read ( unit=LU_TEMP, fmt="(a256)", iostat=iStat ) sBuf
    if ( iStat<0 ) exit            ! Here on EOF
    call Assert ( iStat == 0, "Cannot read record from time-series file" )
    if ( sBuf(1:1) == '#' ) cycle      ! Check comments
    call CleanUpCsv ( sBuf )

    read ( unit=sBuf, fmt=*, iostat=iStat ) iMonth, iDay, iYear, rAvgT, &
                                            rPrecip, rRH, rMaxT, rMinT, &
                                            rWindSpd, rMinRH, rSunPct

    if (iStat/=0) then
      write(UNIT=LU_LOG,FMT=*)"   Skipping: ",trim(sBuf)
      cycle
    end if

    call LookupMonth(iMonth, iDay, iYear,iDayOfYear, &
                       sMonthName,lMonthEnd)

    rMonthlySum(iMonth) = rMonthlySum(iMonth) + real(rAvgT, kind=T_DBL)
    iMonthlyCount(iMonth) = iMonthlyCount(iMonth) + 1
  end do

  ! Now, compute monthly temperature indices (leave out the no-data months)
  ! And sum to get the annual index

  rAnnualIndex = rZERO
  do i=1,12
    if ( iMonthlyCount(i) > 0 ) then
      rMonthAvg = rMonthlySum(i) / real(iMonthlyCount(i), kind=T_DBL)
      if ( rMonthAvg > dpFREEZING ) then
        !> calculate monthly index as per Thornthwaite (1948), p89.
        rMonthIndex = (FtoC(rMonthAvg) / 5_T_DBL) ** 1.514_T_DBL
        rAnnualIndex = rAnnualIndex + rMonthIndex
      end if
    end if
  end do

  !> equation 9, Thornthwaite (1948), p. 89
  rExponentA = 6.75E-7 * rAnnualIndex**3 &
               - 7.71E-5 * rAnnualIndex**2 &
               + 1.7921E-2 * rAnnualIndex &
               + 0.49239

  close (unit=LU_TEMP)

  write(UNIT=LU_LOG,FMT=*) "  Thornthwaite-Mather annual heat index = ",rAnnualIndex
  write(UNIT=LU_LOG,FMT=*) "  Thornthwaite-Mather exponent 'a' = ",rExponentA

end subroutine et_tm_initialize

subroutine et_tm_ComputeET( pGrd, pConfig, iDayOfYear)
  !! Computes the potential ET for each cell, based on the meteorological
  !! data given. Stores cell-by-cell PET values in the model grid.
  !! Note: for the T-M model, it's constant scross the grid
  !!
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  integer (kind=T_INT),intent(in) :: iDayOfYear
  real (kind=T_DBL) :: rPotET, rDecl, rOmega_s, rN, rPotET_adj

  ! [ locals ]
  integer (kind=T_INT) :: iCol, iRow
!  real (kind=T_SGL),dimension(3),parameter :: rHighTPoly = &
!    (/ -5.25625072726565D-03, 1.04170341298537D+00, - 44.3259754866234D+00 /)
  type (T_CELL), pointer :: cel
  real (kind=T_DBL) :: rTempC


  rDecl = 0.4093_T_DBL * sin((dpTWOPI * real(iDayOfYear, kind=T_DBL)/365.0_T_DBL) - 1.405_T_DBL )
!  rDecl = solar_declination(iDayOfYear, MODEL_SIM%daysperyear())
  rOmega_s = sunset_angle(real(rLatitude * dpPI_OVER_180, kind=T_DBL), rDecl)
  rN = daylight_hours(rOmega_s)

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX  ! last subscript in a Fortran array should be the slowest changing
      cel => pGrd%Cells(iCol,iRow)

      rTempC = FtoC(real(cel%rTAvg, kind=T_DBL))

      if (pGrd%Cells(iCol,iRow)%rTAvg <= rFREEZING ) then
        rPotET = dpZERO
      else if ( cel%rTAvg <= 79.7_T_SGL ) then

        !> this is based on equation 10, Thornthwaite (1948), divided by the
        !> number of days in the month and converted to inches from mm
        rPotET = (16_T_DBL * (10_T_DBL*rTempC / rAnnualIndex)**rExponentA) &
                 / 25.4_T_DBL / 30_T_DBL

      else

        !> high temperature equation is equation 4 in Willmott, C.J., Rowe, C.M.
        !> and Mintz, Y., 1985, Climatology of the terrestrial seasonal
        !> water cycle: Journal of Climatology, v. 5, no. 6, p. 589–606.
        rPotET = (-415.85_T_DBL + 32.24_T_DBL*rTempC -0.43_T_DBL*rTempC**2) &
           / 25.4_T_DBL &
           / 30_T_DBL

      end if

      !> "adjusted" potential ET is arrived at by scaling according to the number of
      !> daylight hours on a particular day at given latitude
      rPotET_adj = rPotET * rN / 12.0_T_DBL

      pGrd%Cells(iCol,iRow)%rReferenceET0 = rPotET_adj

    end do

  end do

end subroutine et_tm_ComputeET

end module et_thornthwaite_mather
