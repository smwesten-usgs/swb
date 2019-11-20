!> @file
!>  Contains a single module, et_thornthwaite_mather, which
!>  calculates potential evapotranspiration by means of the Thornthwaite-Mather (1955, 1957) method.


!>  Calculates potential evapotranspiration by means of the
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

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use types
  use stats
  use meteorological_functions
  use datetime

  implicit none

  !! Module data

  !! Configuration -- input data
  real (c_double) :: rLatitude
  real (c_double) :: rTanLatitude

  !! Parameters from pre-scan of annual data
  real (c_double) :: rAnnualIndex        ! Annual thermal index
  real (c_double) :: rExponentA        ! Exponent 'a'

contains

subroutine et_tm_configure( sRecord )
  !! Configures the module, using the command line in 'sRecord'
  ! [ ARGUMENTS ]
  character (len=*),intent(inout) :: sRecord
  ! [ LOCALS ]
  character (len=256) :: sOption
  integer (c_int) :: iStat

  write(UNIT=LU_LOG,FMT=*) "Configuring Thornthwaite-Mather ET model"
  call Chomp( sRecord,sOption )
  read ( unit=sOption, fmt=*, iostat=iStat ) rLatitude
  call Assert( iStat == 0, &
    "Could not read the latitude" )
  rTanLatitude = tan(dpTWOPI * rLatitude / 360.0_c_float )
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
  integer (c_int) :: iJulianDay, iStat, iMonth, iDay, iYear,i
  integer (c_int) :: iDayOfYear
  real(c_float) :: rPrecip, rAvgT, rMinT, rMaxT, rRH, rMinRH, rWindSpd, rSunPct
  real(c_double) :: rMonthAvg,rMonthIndex
  character (len=256) :: sBuf
  character (len=3) :: sMonthName
  logical (c_bool) :: lMonthEnd
  logical (c_bool) :: lOpened
  integer (c_int) :: iLU
  integer (c_int), dimension(12) :: iMonthlyCount
  real (c_double), dimension(12) :: rMonthlySum
  logical (c_bool) :: lIsOpen

  !> exponent value is from Thornthwaite (1948), p 89.
  real (c_double),parameter :: rExp=1.514_c_double
  write(UNIT=LU_LOG,FMT=*)"Initializing Thornthwaite-Mather ET model with annual data ", trim(sFileName)


  !> @todo Fix T-M routine: broken now if using tabular data since
  !> by the time this routine is called, the tabular data file is already
  !> open

  
   inquire( unit=LU_TS, opened=lIsOpen )
!  open ( LU_TEMP, file=trim(sFileName), action='READ', iostat=iStat )
  call Assert ( lIsOpen, "INTERNAL ERROR--T-M method requires that time series file is open before calling this subroutine.", &
    trim(__FILE__),__LINE__)

  ! Now, compute the monthly-average temperatures for all months
  iMonthlyCount = 0
  rMonthlySum = rZERO

  do
    read ( unit=LU_TS, fmt="(a256)", iostat=iStat ) sBuf
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

    rMonthlySum(iMonth) = rMonthlySum(iMonth) + real(rAvgT, c_double)
    iMonthlyCount(iMonth) = iMonthlyCount(iMonth) + 1
  end do

  ! Now, compute monthly temperature indices (leave out the no-data months)
  ! And sum to get the annual index

  rAnnualIndex = rZERO
  do i=1,12
    if ( iMonthlyCount(i) > 0 ) then
      rMonthAvg = rMonthlySum(i) / real(iMonthlyCount(i), c_double)
      if ( rMonthAvg > dpFREEZING ) then
        !> calculate monthly index as per Thornthwaite (1948), p89.
        rMonthIndex = (FtoC(rMonthAvg) / 5_c_double) ** 1.514_c_double
        rAnnualIndex = rAnnualIndex + rMonthIndex
      end if
    end if
  end do

  !> equation 9, Thornthwaite (1948), p. 89
  rExponentA = 6.75E-7 * rAnnualIndex**3 &
               - 7.71E-5 * rAnnualIndex**2 &
               + 1.7921E-2 * rAnnualIndex &
               + 0.49239

!  close (unit=LU_TEMP)
  rewind ( unit=LU_TS )

  ! The following is needed because by the time this routine is called, the first
  ! line of data has already been read in. So we read in only line of data and exit.
  do 

    read ( unit=LU_TS, fmt="(a256)", iostat=iStat ) sBuf
    if ( iStat<0 ) exit            ! Here on EOF
    call Assert ( iStat == 0, "Cannot read record from time-series file", __FILE__, __LINE__ )
    if ( sBuf(1:1) == '#' ) cycle      ! Check comments
    call CleanUpCsv ( sBuf )

    read ( unit=sBuf, fmt=*, iostat=iStat ) iMonth, iDay, iYear, rAvgT, &
                                            rPrecip, rRH, rMaxT, rMinT, &
                                            rWindSpd, rMinRH, rSunPct

    if (iStat/=0) then
      write(UNIT=LU_LOG,FMT=*)"   Skipping: ",trim(sBuf)
      cycle
    else 
      exit
    end if

  enddo

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
  integer (c_int),intent(in) :: iDayOfYear
  real (c_double) :: rPotET, rDecl, rOmega_s, rN, rPotET_adj

  ! [ locals ]
  integer (c_int) :: iCol, iRow
!  real (c_float),dimension(3),parameter :: rHighTPoly = &
!    (/ -5.25625072726565D-03, 1.04170341298537D+00, - 44.3259754866234D+00 /)
  type (T_CELL), pointer :: cel
  real (c_double) :: rTempC


  rDecl = 0.4093_c_double * sin((dpTWOPI * real(iDayOfYear, c_double)/365.0_c_double) - 1.405_c_double )
!  rDecl = solar_declination(iDayOfYear, MODEL_SIM%daysperyear())
  rOmega_s = sunset_angle(real(rLatitude * dpPI_OVER_180, c_double), rDecl)
  rN = daylight_hours(rOmega_s)

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX  ! last subscript in a Fortran array should be the slowest changing
      cel => pGrd%Cells(iCol,iRow)

      rTempC = FtoC(real(cel%rTAvg, c_double))

      if (pGrd%Cells(iCol,iRow)%rTAvg <= rFREEZING ) then
        rPotET = dpZERO
      else if ( cel%rTAvg <= 79.7_c_float ) then

        !> this is based on equation 10, Thornthwaite (1948), divided by the
        !> number of days in the month and converted to inches from mm
        rPotET = (16_c_double * (10_c_double*rTempC / rAnnualIndex)**rExponentA) &
                 / 25.4_c_double / 30_c_double

      else

        !> high temperature equation is equation 4 in Willmott, C.J., Rowe, C.M.
        !> and Mintz, Y., 1985, Climatology of the terrestrial seasonal
        !> water cycle: Journal of Climatology, v. 5, no. 6, p. 589�606.
        rPotET = (-415.85_c_double + 32.24_c_double*rTempC -0.43_c_double*rTempC**2) &
           / 25.4_c_double &
           / 30_c_double

      end if

      !> "adjusted" potential ET is arrived at by scaling according to the number of
      !> daylight hours on a particular day at given latitude
      rPotET_adj = rPotET * rN / 12.0_c_double

      pGrd%Cells(iCol,iRow)%rReferenceET0 = rPotET_adj

    end do

  end do

end subroutine et_tm_ComputeET

end module et_thornthwaite_mather
