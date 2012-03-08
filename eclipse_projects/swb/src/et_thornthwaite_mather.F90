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
  use swb_stats

  implicit none

  !! Module data

  !! Configuration -- input data
  real (kind=T_SGL) :: rLatitude
  real (kind=T_SGL) :: rTanLatitude

  !! Parameters from pre-scan of annual data
  real (kind=T_SGL) :: rAnnualIndex        ! Annual thermal index
  real (kind=T_SGL) :: rExponentA        ! Exponent 'a'

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
  real(kind=T_SGL) :: rMonthAvg,rMonthIndex
  character (len=256) :: sBuf
  character (len=3) :: sMonthName
  logical (kind=T_LOGICAL) :: lMonthEnd
  logical (kind=T_LOGICAL) :: lOpened
  integer (kind=T_INT) :: iLU
  integer (kind=T_INT),dimension(12) :: iMonthlyCount
  real (kind=T_INT),dimension(12) :: rMonthlySum
  ! [ LOCAL CONSTANTS ]
  real (kind=T_SGL),parameter :: rExp=1.514_T_SGL
  real (kind=T_SGL),dimension(4),parameter :: rAPoly = &
      (/ 6.75E-07_T_SGL, -7.71E-05_T_SGL, 1.792E-02_T_SGL, 0.49239E+00_T_SGL /)

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

    rMonthlySum(iMonth) = rMonthlySum(iMonth) + rAvgT
    iMonthlyCount(iMonth) = iMonthlyCount(iMonth) + 1
  end do

  ! Now, compute monthly temperature indices (leave out the no-data months)
  ! And sum to get the annual index

  rAnnualIndex = rZERO
  do i=1,12
    if ( iMonthlyCount(i) > 0 ) then
      rMonthAvg = rMonthlySum(i)/iMonthlyCount(i)
      if ( rMonthAvg > rFREEZING ) then
        rMonthIndex = (( rMonthAvg - rFREEZING )/9.0_T_SGL ) ** rExp
        rAnnualIndex = rAnnualIndex + rMonthIndex
      end if
    end if
  end do

  rExponentA = polynomial( rAnnualIndex, rAPoly )

  close (unit=LU_TEMP)

  write(UNIT=LU_LOG,FMT=*) "  Thornthwaite-Mather annual heat index = ",rAnnualIndex
  write(UNIT=LU_LOG,FMT=*) "  Thornthwaite-Mather exponent 'a' = ",rExponentA

end subroutine et_tm_initialize

subroutine et_tm_ComputeET( pGrd, pConfig, iDayOfYear, rRH, rMinRH, rWindSpd, rSunPct )
  !! Computes the potential ET for each cell, based on the meteorological
  !! data given. Stores cell-by-cell PET values in the model grid.
  !! Note: for the T-M model, it's constant scross the grid
  !!
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  integer (kind=T_INT),intent(in) :: iDayOfYear
  real (kind=T_SGL),intent(in) :: rRH,rMinRH,rWindSpd,rSunPct
  real (kind=T_SGL) :: rDecl,rLCF,rPotET, rTempET

  ! [ locals ]
  integer (kind=T_SGL) :: iAvgT, iCol, iRow
  real (kind=T_SGL),dimension(3),parameter :: rHighTPoly = &
    (/ -5.25625072726565D-03, 1.04170341298537D+00, - 44.3259754866234D+00 /)


  rDecl = 0.4093_T_SGL * sin((dpTWOPI * iDayOfYear/365.0_T_SGL) - 1.405_T_SGL )

  rLCF = (24.0_T_SGL / dpPI) * acos(-rTanLatitude * tan(rDecl)) / 12.0_T_SGL

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX  ! last subscript in a Fortran array should be the slowest changing

      if (pGrd%Cells(iCol,iRow)%rTAvg <= rFREEZING ) then
        rPotET = rZERO
      else if ( pGrd%Cells(iCol,iRow)%rTAvg <= 79.7_T_SGL ) then
!      rPotET = ( 50.0_T_SGL*(rAvgT-rFREEZING) / (9.0_T_SGL * rAnnualIndex) ) ** rExponentA * &
!         rLCF * 0.63_T_SGL / 30.0_T_SGL
         rPotET = ((0.63_T_SGL * rLCF) * ((50.0_T_SGL *  &
                     (pGrd%Cells(iCol,iRow)%rTAvg - rFREEZING) &
                      / (9_T_SGL * rAnnualIndex)) ** rExponentA)) / 30_T_SGL
      else
        rPotET = rLCF * polynomial( pGrd%Cells(iCol,iRow)%rTAvg, rHighTPoly ) / 30.0_T_SGL
      end if

      pGrd%Cells(iCol,iRow)%rSM_PotentialET = rPotET

    end do

  end do

!  write(UNIT=LU_LOG,FMT=*) "=========POTET CALCULATION==========="
!  write(UNIT=LU_STD_OUT,FMT="(A)") &
!      "                                 min          mean           max"
!  call stats_WriteMinMeanMax(LU_STD_OUT,"Potet ET (in)" , pGrd%Cells(:,:)%rSM_PotentialET )
!
!  write(UNIT=LU_LOG,FMT=*) "=========POTET CALCULATION==========="

  return

end subroutine et_tm_ComputeET

end module et_thornthwaite_mather
