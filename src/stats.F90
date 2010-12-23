!> \brief Calculates daily, monthly, and annual statistics
!>        for an SWB model run.

module swb_stats

! SYNOPSIS
!   This module calculates summary statistics (min, mean, max, sum) for
!   all model state and intermediate variables.
!
!!***

  use types
  use swb_grid
  use graph
  use RLE

  implicit none
  save

  ! rVolConvert combines the conversion factor between inches and meters, and
  ! multiplies by the area of a grid cell (assumed to be meters)
  real (kind=T_DBL), public :: rVolConvert

  !! Monthly accumulators - to hold SUM of daily values across all cells
  real (kind=T_DBL), dimension(iNUM_MONTHS,iNUM_STATS,iNUM_VARIABLES), public :: rMonthly

  !! Daily accumulators - to hold SUM of daily values across all cells
  real (kind=T_DBL), dimension(iNUM_STATS,iNUM_VARIABLES), public :: rDaily

  !! Annual accumulators - to hold SUM of daily values across all cells
  real (kind=T_DBL), dimension(iNUM_STATS,iNUM_VARIABLES), public :: rAnnual

contains

!--------------------------------------------------------------------------


subroutine stats_InitializeVolumeConversion(pGrd)

  ![ARGUMENTS]
  type ( T_GENERAL_GRID ),pointer :: pGrd    ! pointer to model grid

  if(pGrd%iLengthUnits == iGRID_LENGTH_UNITS_METERS) then

    rVolConvert = (rONE / 12.0_T_DBL) &
               *(pGrd%rGridCellSize ** 2_T_DBL) &
               * rSQM_to_SQFT &
               / 43560.
    ! multiply by area of grid cell in acres (yields acre-ft of water)

  else if(pGrd%iLengthUnits == iGRID_LENGTH_UNITS_FEET) then

    rVolConvert = (rONE / 12.0_T_DBL) &
               *(pGrd%rGridCellSize ** 2_T_DBL) &
               / 43560.
    ! multiply by area of grid cell in acres (yields acre-ft of water)

  else

    call Assert(lFALSE,"The grid cell length units have not been defined")

  end if

  return

end subroutine stats_InitializeVolumeConversion

!--------------------------------------------------------------------------

subroutine stats_InitializeDailyAccumulators()

  rDaily(iMIN,:) = HUGE(rDaily(iMIN,:))
  rDaily(iMEAN,:) = rZERO
  rDaily(iMAX,:) = -HUGE(rDaily(iMAX,:))
  rDaily(iSUM,:) = rZERO
  rDaily(iLENGTH,:) = rZERO

end subroutine stats_InitializeDailyAccumulators

!--------------------------------------------------------------------------

subroutine stats_InitializeMonthlyAccumulators()

  ! Zero out MONTHLY accumulators
  rMonthly = rZERO

end subroutine stats_InitializeMonthlyAccumulators

!--------------------------------------------------------------------------

subroutine stats_InitializeAnnualAccumulators()

  ! Zero out ANNUAL accumulators
  rAnnual = rZERO

end subroutine stats_InitializeAnnualAccumulators
!--------------------------------------------------------------------------
subroutine stats_FormatTextString(sText,rValue,pConfig, &
      iVarNum, sFormattedText)

  ![ARGUMENTS]
  character(len=*), intent(in) :: sText
  real(kind=T_SGL), intent(in), dimension(iNUM_STATS) :: rValue
  type (T_MODEL_CONFIGURATION), pointer :: pConfig

  integer(kind=T_INT), intent(in) :: iVarNum

  character(len=256), dimension(iNUM_STATS) :: sFormattedText
  ![LOCALS]
  integer(kind=T_INT) :: i
  character(len=256) :: sBuf
  character(len=1) :: sSpace = ' '
  character(len=3) :: sMassBalanceOperator

  ! first write string containing variable names
  if(pConfig%lANSI_Colors) then
    if(rValue(iSUM)>rNEAR_ZERO .or. rValue(iSUM)< -rNEAR_ZERO) then
      write(sFormattedText(1),'(A24,$)') sBOLDWHITE// &
        REPEAT(sSpace,STAT_INFO(iVarNum)%iIndent)//sText
    else
      write(sFormattedText(1),'(A24,$)') sWHITE//&
        REPEAT(sSpace,STAT_INFO(iVarNum)%iIndent)//sText
    end if
  else
    write(sFormattedText(1),'(A20,$)') &
      REPEAT(sSpace,STAT_INFO(iVarNum)%iIndent)//sText
  end if

  do i=iMIN,iMAX
    if(pConfig%lANSI_Colors) then
      if(rValue(i) > rNEAR_ZERO) then
        write(sBuf,'(A7,f14.3,$)') sGREEN,rValue(i)
      else if(rValue(i) < -rZERO) then
        write(sBuf,'(A7,f14.3,$)') sRED,rValue(i)
      else
        write(sBuf,'(A7,f14.3,$)') sWHITE,rValue(i)
      end if
    else
      write(sBuf,'(f14.3,$)') rValue(i)
    end if
    sFormattedText(i+1) = sBuf
  end do

  i=iSUM

  if(STAT_INFO(iVarNum)%iMassBalanceConst==1) then
    sMassBalanceOperator = '(+)'
  elseif(STAT_INFO(iVarNum)%iMassBalanceConst==-1) then
    sMassBalanceOperator = '(-)'
  else
    sMassBalanceOperator = '   '
  end if

  if(rValue(i) < rNEAR_ZERO .and. rValue(i) > -rNEAR_ZERO &
       .or. .not. STAT_INFO(iVarNum)%lShowDailySum) then
    sBuf = ' '
  else
    if(pConfig%lANSI_Colors) then
      if(rValue(i) > rNEAR_ZERO) then
        write(sBuf,'(A7,F14.2,A7,$)') sGREEN,rValue(i),sMassBalanceOperator
      else if(rValue(i) < -rZERO) then
        write(sBuf,'(A7,F14.2,A7,$)') sRED,rValue(i),sMassBalanceOperator
      else
        write(sBuf,'(A7,F14.2,A7,$)') sWHITE,rValue(i),sMassBalanceOperator
      end if
    else
      write(sBuf,'(F14.2,A7,$)') rValue(i),sMassBalanceOperator
    end if
  end if

  sFormattedText(i+1) = sBuf

  return

end subroutine stats_FormatTextString

!--------------------------------------------------------------------------

subroutine stats_WriteDailyAccumulatorValuesCSV(iLU,iMonth,iDay,iYear)

  ![ARGUMENTS]
  integer (kind=T_INT), intent(in) :: iLU
  integer (kind=T_INT), intent(in) :: iMonth,iDay,iYear

  ![LOCALS]
  integer (kind=T_INT) :: i,j

  write(iLU,"(i2.2,A1,i2.2,A1,i4,A1,100(f12.3,A1))") &
                                iMonth,"/",iDay,"/",iYear,",", &
                               (rDaily(iMIN,i),",", &
                                rDaily(iMEAN,i),",", &
                                rDaily(iMAX,i),",",i=1,iNUM_VARIABLES -1), &
                                rDaily(iMIN,iNUM_VARIABLES),",", &
                                rDaily(iMEAN,iNUM_VARIABLES),",", &
                                rDaily(iMAX,iNUM_VARIABLES)

  return


end subroutine stats_WriteDailyAccumulatorValuesCSV

!--------------------------------------------------------------------------

subroutine stats_WriteAnnualAccumulatorValuesCSV(iLU,iYear)

  ![ARGUMENTS]
  integer (kind=T_INT), intent(in) :: iLU
  integer (kind=T_INT), intent(in) :: iYear

  ![LOCALS]
  integer (kind=T_INT) :: i,j

  write(iLU,"(i4,A1,100(f12.3,A1))") &
                                iYear,",", &
                               (rAnnual(iMEAN,i),",", i=1,iNUM_VARIABLES-1), &
                               rAnnual(iMEAN,iNUM_VARIABLES)

  return


end subroutine stats_WriteAnnualAccumulatorValuesCSV

!--------------------------------------------------------------------------

subroutine stats_WriteDailyAccumulatorHeaderCSV(iLU)

  ![ARGUMENTS]
  integer (kind=T_INT), intent(in) :: iLU

  ![LOCALS]
  integer (kind=T_INT) :: i,j

  write(iLU,"(300A)") "Date,",( "MIN " // STAT_INFO(i)%sVARIABLE_NAME // "," // &
                     "MEAN " // STAT_INFO(i)%sVARIABLE_NAME // "," // &
                     "MAX " // STAT_INFO(i)%sVARIABLE_NAME // ",", &
                     i=1,iNUM_VARIABLES - 1), &
                     "MIN " // STAT_INFO(iNUM_VARIABLES)%sVARIABLE_NAME // "," // &
                     "MEAN " // STAT_INFO(iNUM_VARIABLES)%sVARIABLE_NAME // "," // &
                     "MAX " // STAT_INFO(iNUM_VARIABLES)%sVARIABLE_NAME

  flush(unit=iLU)

  return


end subroutine stats_WriteDailyAccumulatorHeaderCSV

!--------------------------------------------------------------------------

subroutine stats_WriteAnnualAccumulatorHeaderCSV(iLU)

  ![ARGUMENTS]
  integer (kind=T_INT), intent(in) :: iLU

  ![LOCALS]
  integer (kind=T_INT) :: i,j

  write(iLU,"(100A)") "Year,",("MEAN " // STAT_INFO(i)%sVARIABLE_NAME // ",", &
                     i=1,iNUM_VARIABLES)

  flush(unit=iLU)

  return


end subroutine stats_WriteAnnualAccumulatorHeaderCSV

!--------------------------------------------------------------------------

subroutine stats_DumpDailyAccumulatorValues(iLU, pConfig)

  ![ARGUMENTS]
  integer (kind=T_INT), intent(in) :: iLU
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings

  ![LOCALS]
  integer (kind=T_INT) :: i,j
  character(len=256),dimension(iNUM_STATS) :: sText
  real (kind=T_DBL),dimension(iNUM_STATS,iNUM_VARIABLES) :: rTempArray
  real (kind=T_DBL) :: rMassBalance

  rTempArray = rDaily

  rMassBalance = SUM(rTempArray(iSUM,:) &
                    * real(STAT_INFO(:)%iMassBalanceConst, kind=T_DBL) ) &
                    * rVolConvert

  rTempArray(iSUM,:) = rTempArray(iSUM,:) * rVolConvert

  if(pConfig%lANSI_Colors .and. iLU==LU_STD_OUT) write(iLU,FMT=*) sYELLOW

  write(UNIT=iLU,FMT="(24x,4(a10,5x))") 'min(in)','mean(in)','max(in)','sum(ac-ft)'

  if(pConfig%lANSI_Colors .and. iLU==LU_STD_OUT) write(iLU,FMT=*) sWHITE

  do i=1,iNUM_VARIABLES
    call stats_FormatTextString(STAT_INFO(i)%sVARIABLE_NAME, &
                REAL(rTempArray(:,i),kind=T_SGL), &
                pConfig, &
                i, &
                sText)

    if(pConfig%lANSI_Colors .and. iLU==LU_STD_OUT) then
      write(iLU,'(A25,3A22,A28)') sText
    else
      write(iLU,'(A20,3A15,A21)') sText
    end if

  end do

  write(UNIT=iLU,FMT="(65x,a14)") REPEAT("-",14)
  write(UNIT=iLU,FMT="(51x,a14,F14.2)") 'Mass Balance:', rMassBalance

  return

end subroutine stats_DumpDailyAccumulatorValues

!--------------------------------------------------------------------------

subroutine stats_DumpMonthlyAccumulatorValues(iLU, iMonth, sMonthName, pConfig)

  ![ARGUMENTS]
  integer (kind=T_INT), intent(in) :: iLU
  integer (kind=T_INT),intent(in) :: iMonth        ! month for which to dump data
  character (len=3), intent(in) :: sMonthName
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings

  ![LOCALS]
  integer (kind=T_INT) :: i,j
  character(len=256),dimension(iNUM_STATS) :: sText
  real (kind=T_DBL),dimension(iNUM_STATS,iNUM_VARIABLES) :: rTempArray
  real (kind=T_DBL) :: rMassBalance

  rTempArray = rMonthly(iMonth,:,:)

  rMassBalance = SUM(rTempArray(iSUM,:)*STAT_INFO(:)%iMassBalanceConst) &
                 * rVolConvert

  rTempArray(iSUM,:) = rTempArray(iSUM,:) * rVolConvert

  if(pConfig%lANSI_Colors) write(iLU,FMT=*) sGREEN


  write(iLU,FMT=*)
  write(iLU,FMT=*) REPEAT('-',80)
  write(iLU,FMT='(a20,a3)') 'MONTHLY SUMMARY for ',sMonthName
  write(iLU,FMT=*) REPEAT('-',80)

  if(pConfig%lANSI_Colors .and. iLU==LU_STD_OUT) write(iLU,FMT=*) sYELLOW
  write(UNIT=iLU,FMT="(24x,4(a10,5x))") 'min(in)','mean(in)','max(in)','sum(ac-ft)'
  if(pConfig%lANSI_Colors .and. iLU==LU_STD_OUT) write(iLU,FMT=*) sWHITE

  do i=1,iNUM_VARIABLES
    if(STAT_INFO(i)%lShowSum) then
      call stats_FormatTextString(STAT_INFO(i)%sVARIABLE_NAME, &
                REAL(rTempArray(:,i),kind=T_SGL), &
                pConfig, &
                i, &
                sText)
      if(pConfig%lANSI_Colors .and. iLU==LU_STD_OUT) then
        write(iLU,'(A25,3A22,A28)') sText
      else
        write(iLU,'(A20,3A15,A21)') sText
      end if
    end if
  end do

  if(pConfig%lANSI_Colors .and. iLU==LU_STD_OUT) write(iLU,FMT=*) sWHITE

  write(UNIT=iLU,FMT="(65x,a14)") REPEAT("-",14)
  write(UNIT=iLU,FMT="(51x,a14,F14.3)") 'Mass Balance:', rMassBalance

  return

end subroutine stats_DumpMonthlyAccumulatorValues

!--------------------------------------------------------------------------

subroutine stats_DumpAnnualAccumulatorValues(iLU, pConfig, iYear)

  ![ARGUMENTS]
  integer (kind=T_INT), intent(in) :: iLU
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  integer (kind=T_INT), intent(in) :: iYear
  ![LOCALS]
  integer (kind=T_INT) :: i,j
  character(len=256),dimension(iNUM_STATS) :: sText
  real (kind=T_DBL),dimension(iNUM_STATS,iNUM_VARIABLES) :: rTempArray
  real (kind=T_DBL) :: rMassBalance

  rTempArray = rAnnual

  rTempArray(iSUM,:) = rTempArray(iSUM,:) * rVolConvert

  write(iLU,FMT=*)
  write(iLU,FMT=*) REPEAT('*',80)
  write(iLU,FMT='(a20,i4)') 'ANNUAL SUMMARY for ',iYear
  write(iLU,FMT=*) REPEAT('*',80)

  if(pConfig%lANSI_Colors .and. iLU==LU_STD_OUT) write(iLU,FMT=*) sYELLOW
  write(UNIT=iLU,FMT="(24x,4(a10,5x))") 'min(in)','mean(in)','max(in)','sum(ac-ft)'
  if(pConfig%lANSI_Colors .and. iLU==LU_STD_OUT) write(iLU,FMT=*) sWHITE

  do i=1,iNUM_VARIABLES
    if(STAT_INFO(i)%lShowSum) then
      call stats_FormatTextString(STAT_INFO(i)%sVARIABLE_NAME, &
                REAL(rTempArray(:,i),kind=T_SGL), &
                pConfig, &
                i, &
                sText)
      if(pConfig%lANSI_Colors .and. iLU==LU_STD_OUT) then
        write(iLU,'(A25,3A22,A28)') sText
      else
        write(iLU,'(A20,3A15,A21)') sText
      end if
    end if
  end do

  rMassBalance = SUM(rTempArray(iSUM,:)*STAT_INFO(:)%iMassBalanceConst)
  write(UNIT=iLU,FMT="(65x,a14)") REPEAT("-",14)
  write(UNIT=iLU,FMT="(51x,a14,F14.3)") 'Mass Balance:', rMassBalance

  return

end subroutine stats_DumpAnnualAccumulatorValues

!--------------------------------------------------------------------------

subroutine stats_UpdateAllAccumulatorsByCell(rValue,iVarNum,iMonthNum,iNumGridCells)

  ![ARGUMENTS]
  real (kind=T_DBL), intent(in) :: rValue
  integer (kind=T_INT), intent(in) :: iVarNum
  integer (kind=T_INT), intent(in) :: iMonthNum
  integer (kind=T_INT), intent(in) :: iNumGridCells

  ![LOCALS]
!  real (kind=T_SGL) :: rMinimum

  call Assert(LOGICAL(iVarNum>=1 .and. iVarNum <= iNUM_VARIABLES,kind=T_LOGICAL), &
    'call to UpdateAllAccumulators failed...inappropriate variable index specified')
  call Assert(LOGICAL(iMonthNum>=1 .and. iMonthNum <= iNUM_MONTHS,kind=T_LOGICAL), &
    'call to UpdateAllAccumulators failed...inappropriate month index specified')

  if(iNumGridCells > 0) then  ! tidy up and divide by the number of grid cells
                              ! NOTE!! rValue is ignored in this instance!!

    rDaily(iMEAN,iVarNum) = rDaily(iSUM,iVarNum) / REAL(iNumGridCells, kind=T_DBL)

    rMonthly(iMonthNum,iMIN,iVarNum) = MIN(rMonthly(iMonthNum,iMIN,iVarNum), &
          rDaily(iMIN,iVarNum))
    rMonthly(iMonthNum,iMEAN,iVarNum) = rMonthly(iMonthNum,iMEAN,iVarNum) + &
          rDaily(iMEAN,iVarNum)
    rMonthly(iMonthNum,iMAX,iVarNum) = MAX(rMonthly(iMonthNum,iMAX,iVarNum), &
          rDaily(iMAX,iVarNum))
    rMonthly(iMonthNum,iSUM,iVarNum) = rMonthly(iMonthNum,iSUM,iVarNum) +  &
          rDaily(iSUM,iVarNum)

    rAnnual(iMIN,iVarNum) = MIN(rAnnual(iMIN,iVarNum), rDaily(iMIN,iVarNum))
    rAnnual(iMEAN,iVarNum) = rAnnual(iMEAN,iVarNum) + rDaily(iMEAN,iVarNum)
    rAnnual(iMAX,iVarNum) = MAX(rAnnual(iMAX,iVarNum), rDaily(iMAX,iVarNum))
    rAnnual(iSUM,iVarNum) = rAnnual(iSUM,iVarNum) + rDaily(iSUM,iVarNum)

    call Assert &
     (LOGICAL(INT(rDaily(iLENGTH,iVarNum),kind=T_INT)==iNumGridCells,kind=T_LOGICAL), &
      "stats.f95: call to UpdateAllAccumulators failed; number of calls" &
      // " must be equal to the number of grid cells.")


  else ! continue to accumulate

    rDaily(iMIN,iVarNum) = min(rDaily(iMIN,iVarNum),rValue)
    rDaily(iMAX,iVarNum) = max(rDaily(iMAX,iVarNum),rValue)
    rDaily(iSUM,iVarNum) = rDaily(iSUM,iVarNum) + rValue
    rDaily(iLENGTH,iVarNum) = rDaily(iLENGTH,iVarNum) + 1

  end if

end subroutine stats_UpdateAllAccumulatorsByCell

!--------------------------------------------------------------------------

subroutine stats_UpdateAllAccumulatorsByGrid(rMin,rMean,rMax,rSum,iVarNum,iMonthNum)

  ![ARGUMENTS]
  real (kind=T_DBL), intent(in) :: rMin,rMean,rMax,rSum
  integer (kind=T_INT), intent(in) :: iVarNum
  integer (kind=T_INT), intent(in) :: iMonthNum

  ![LOCALS]

  call Assert(LOGICAL(iVarNum>=1 .and. iVarNum <= iNUM_VARIABLES,kind=T_LOGICAL), &
    'call to UpdateAllAccumulators failed...inappropriate variable index specified')
  call Assert(LOGICAL(iMonthNum>=1 .and. iMonthNum <= iNUM_MONTHS,kind=T_LOGICAL), &
    'call to UpdateAllAccumulators failed...inappropriate month index specified')

  rDaily(iMIN,iVarNum) = rMin
  rDaily(iMEAN,iVarNum) = rMean
  rDaily(iMAX,iVarNum) = rMax
  rDaily(iSUM,iVarNum) = rSum

  rMonthly(iMonthNum,iMIN,iVarNum) = rMin + rMonthly(iMonthNum,iMIN,iVarNum)
  rMonthly(iMonthNum,iMEAN,iVarNum) = rMonthly(iMonthNum,iMEAN,iVarNum) + rMEAN
  rMonthly(iMonthNum,iMAX,iVarNum) = rMax + rMonthly(iMonthNum,iMAX,iVarNum)
  rMonthly(iMonthNum,iSUM,iVarNum) = rMonthly(iMonthNum,iSUM,iVarNum) + rSum

  rAnnual(iMIN,iVarNum) = rAnnual(iMIN,iVarNum) + rMin
  rAnnual(iMEAN,iVarNum) = rAnnual(iMEAN,iVarNum) + rMEAN
  rAnnual(iMAX,iVarNum) = rAnnual(iMAX,iVarNum) + rMax
  rAnnual(iSUM,iVarNum) = rAnnual(iSUM,iVarNum) + rSUM

  return

end subroutine stats_UpdateAllAccumulatorsByGrid

!--------------------------------------------------------------------------

subroutine stats_CalcMonthlyMeans(iMonthNum, iDaysInMonth)

  ![ARGUMENTS]
  integer(kind=T_INT), intent(in) :: iMonthNum
  integer(kind=T_INT), intent(in) :: iDaysInMonth

  ![LOCALS]
  integer(kind=T_INT) :: i

!  do i=1,iNUM_VARIABLES
!    rMonthly(iMonthNum,iMEAN,i) = rMonthly(iMonthNum,iMEAN,i) / iDaysInMonth
!    write(UNIT=LU_LOG,FMT=*) STAT_INFO(i)%sVARIABLE_NAME, rMonthly(iMonthNum,iMEAN,i),iDaysInMonth
!  end do

end subroutine stats_CalcMonthlyMeans

!--------------------------------------------------------------------------

!> \brief Writes out the min, max, and mean of 2D variable array
!> Second line of brief description of this routine
!> \bug Yikes! A bug!!

subroutine stats_WriteMinMeanMax( iLU, sText, rData , iCount)

  ! [ ARGUMENTS ]
  integer (kind=T_INT), intent(in) :: iLU       ! Fortran logical file unit
  real (kind=T_SGL), dimension(:,:) :: rData   ! Real data
  character (len=*) :: sText
  ! [ LOCALS ]
  integer (kind=T_INT) :: iNumGridCells
  integer (kind=T_INT), optional :: iCount

  if(present(iCount)) then
    ! establish number of cells in model grid
    iNumGridCells = iCount
  else
    ! establish number of cells in model grid
    iNumGridCells = size(rData)
  end if

  if(iNumGridCells>0) then
    write(iLU,"(1x,a,t31,f12.2,t46,f12.2,t61,f12.2)") TRIM(ADJUSTL(sText)), &
       minval(rData),sum(rData)/REAL(iNumGridCells,kind=T_SGL),maxval(rData)
  else
    write(iLU,"(1x,a,t31,f12.2,t46,f12.2,t61,f12.2)") TRIM(ADJUSTL(sText)), &
       minval(rData),sum(rData),maxval(rData)
  endif

  flush(iLU)

  return

end subroutine stats_WriteMinMeanMax

!--------------------------------------------------------------------------

subroutine stats_WriteMSBReport(pGrd,iMonth,iDay,iYear,iDayOfYear)

  type ( T_GENERAL_GRID ),pointer :: pGrd
  integer (kind=T_INT),intent(in) :: iMonth,iDay,iYear,iDayOfYear

  real (kind=T_DBL) :: rDailyMSB


!      rDailyMSB = rDaily(iSUM,iGROSS_PRECIP) &
!                   - rDaily(iSUM,iINTERCEPTION) &
!                   - rDaily(iSUM,iCHG_IN_SNOW_COV) &
!                   - rDaily(iSUM,iCHG_IN_SOIL_MOIST) &
!                   - rDaily(iSUM,iRUNOFF_OUTSIDE) &
!                   - rDaily(iSUM,iREJECTED_RECHARGE) &
!                   - rDaily(iSUM,iACT_ET) &
!                   - rDaily(iSUM,iRECHARGE)

#ifndef STREAM_INTERACTIONS
       rDailyMSB =  rDaily(iSUM,iSNOWFALL) &
                  - rDaily(iSUM,iCHG_IN_SNOW_COV) &
                  + rDaily(iSUM,iNET_PRECIP) &
                  + rDaily(iSUM,iINFLOW) &
                  - rDaily(iSUM,iOUTFLOW) &
                  - rDaily(iSUM,iRUNOFF_OUTSIDE) &
                  - rDaily(iSUM,iREJECTED_RECHARGE) &
                  - rDaily(iSUM,iCHG_IN_SOIL_MOIST) &
                  - rDaily(iSUM,iACT_ET) &
                  - rDaily(iSUM,iRECHARGE)
      write( unit=LU_MSB_REPORT, &
           fmt='(I2,",",I2,",",I4,",",I2,"/",I2,"/",I4,",",I3,",",21(F14.2,","),F14.2)' ) &
                         iMonth,iDay,iYear,iMonth,iDay,iYear,iDayOfyear, &
                         SUM(pGrd%Cells(:,:)%rTAvg)/SIZE(pGrd%Cells(:,:)%rTAvg), &
                         SUM(pGrd%Cells(:,:)%rTMin)/SIZE(pGrd%Cells(:,:)%rTMin), &
                         SUM(pGrd%Cells(:,:)%rTMax)/SIZE(pGrd%Cells(:,:)%rTMax), &
                         SUM(pGrd%Cells(:,:)%rCFGI)/SIZE(pGrd%Cells(:,:)%rCFGI), &
						 MINVAL(pGrd%Cells(:,:)%rAdjCN), &
						 SUM(pGrd%Cells(:,:)%rAdjCN) &
						     / SIZE(pGrd%Cells(:,:)%rAdjCN), &
 						 MAXVAL(pGrd%Cells(:,:)%rAdjCN), &
                         rDaily(iSUM,iGROSS_PRECIP)*rVolConvert, &
                         rDaily(iSUM,iINTERCEPTION)*rVolConvert, &
                         (rDaily(iSUM,iGROSS_PRECIP) - rDaily(iSUM,iINTERCEPTION)) &
                                    *rVolConvert, &
                         rDaily(iSUM,iSNOWFALL)*rVolConvert, &
                         MAX((rDaily(iSUM,iGROSS_PRECIP) - rDaily(iSUM,iINTERCEPTION) &
                           - rDaily(iSUM,iSNOWFALL)),0.0D0)*rVolConvert, &
                         rDaily(iSUM,iSNOWCOVER)*rVolConvert, &
                         rDaily(iSUM,iCHG_IN_SNOW_COV)*rVolConvert, &
                         rDaily(iSUM,iSNOWMELT)*rVolConvert, &
                         rDaily(iSUM,iSOIL_MOISTURE)*rVolConvert, &
                         rDaily(iSUM,iCHG_IN_SOIL_MOIST)*rVolConvert, &
                         rDaily(iSUM,iRUNOFF_OUTSIDE)*rVolConvert, &
                         rDaily(iSUM,iREJECTED_RECHARGE)*rVolConvert, &
                         rDaily(iSUM,iACT_ET)*rVolConvert, &
                         rDaily(iSUM,iRECHARGE)*rVolConvert, &
                         rDailyMSB*rVolConvert
#else
       rDailyMSB =  rDaily(iSUM,iSNOWFALL) &
                  - rDaily(iSUM,iCHG_IN_SNOW_COV) &
                  + rDaily(iSUM,iNET_PRECIP) &
                  + rDaily(iSUM,iINFLOW) &
                  - rDaily(iSUM,iOUTFLOW) &
                  - rDaily(iSUM,iRUNOFF_OUTSIDE) &
                  - rDaily(iSUM,iREJECTED_RECHARGE) &
                  - rDaily(iSUM,iCHG_IN_SOIL_MOIST) &
                  - rDaily(iSUM,iACT_ET) &
                  - rDaily(iSUM,iRECHARGE) &
                  - rDaily(iSUM,iSTREAM_CAPTURE)
      write( unit=LU_MSB_REPORT, &
           fmt='(I2,",",I2,",",I4,",",I2,"/",I2,"/",I4,",",I3,",",22(F14.2,","),F14.2)' ) &
                         iMonth,iDay,iYear,iMonth,iDay,iYear,iDayOfyear, &
                         SUM(pGrd%Cells(:,:)%rTAvg)/SIZE(pGrd%Cells(:,:)%rTAvg), &
                         SUM(pGrd%Cells(:,:)%rTMin)/SIZE(pGrd%Cells(:,:)%rTMin), &
                         SUM(pGrd%Cells(:,:)%rTMax)/SIZE(pGrd%Cells(:,:)%rTMax), &
                         SUM(pGrd%Cells(:,:)%rCFGI)/SIZE(pGrd%Cells(:,:)%rCFGI), &
						 MINVAL(pGrd%Cells(:,:)%rAdjCN), &
						 SUM(pGrd%Cells(:,:)%rAdjCN) &
						     / SIZE(pGrd%Cells(:,:)%rAdjCN), &
 						 MAXVAL(pGrd%Cells(:,:)%rAdjCN), &
                         rDaily(iSUM,iGROSS_PRECIP)*rVolConvert, &
                         rDaily(iSUM,iINTERCEPTION)*rVolConvert, &
                         (rDaily(iSUM,iGROSS_PRECIP) - rDaily(iSUM,iINTERCEPTION)) &
                                    *rVolConvert, &
                         rDaily(iSUM,iSNOWFALL)*rVolConvert, &
                         MAX((rDaily(iSUM,iGROSS_PRECIP) - rDaily(iSUM,iINTERCEPTION) &
                           - rDaily(iSUM,iSNOWFALL)),0.0D0)*rVolConvert, &
                         rDaily(iSUM,iSNOWCOVER)*rVolConvert, &
                         rDaily(iSUM,iCHG_IN_SNOW_COV)*rVolConvert, &
                         rDaily(iSUM,iSNOWMELT)*rVolConvert, &
                         rDaily(iSUM,iSOIL_MOISTURE)*rVolConvert, &
                         rDaily(iSUM,iCHG_IN_SOIL_MOIST)*rVolConvert, &
                         rDaily(iSUM,iRUNOFF_OUTSIDE)*rVolConvert, &
                         rDaily(iSUM,iREJECTED_RECHARGE)*rVolConvert, &
                         rDaily(iSUM,iACT_ET)*rVolConvert, &
                         rDaily(iSUM,iRECHARGE)*rVolConvert, &
                         rDaily(iSUM,iSTREAM_CAPTURE)*rVolConvert, &
                         rDailyMSB*rVolConvert
#endif



	  flush(unit=LU_MSB_REPORT)

end subroutine stats_WriteMSBReport

!--------------------------------------------------------------------------

subroutine stats_RewriteGrids(iNX, iNY, rX0, rY0, rX1, rY1, pConfig, pGraph)

  ![ARGUMENTS]
  integer (kind=T_INT), intent(in) :: iNX, iNY       ! Grid dimensions
  real (kind=T_DBL), intent(in) :: rX0, rY0          ! Lower-left corner (world coords)
  real (kind=T_DBL), intent(in) :: rX1, rY1          ! Upper-right corner (world coords)
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings

  type (T_GRAPH_CONFIGURATION), dimension(:), pointer :: pGraph
     ! pointer to data structure that holds parameters for creating
     ! DISLIN plots

  ![LOCALS]
  type ( T_GENERAL_GRID ),pointer :: pGrd
  integer (kind=T_INT) :: i, j, k, iStat, iDayOfYear, iMonth
  integer (kind=T_INT) :: iDay, iYear, iVar, iVal, iRep, iTemp
  integer (kind=T_INT) :: iStartYear, iEndYear
  integer (kind=T_INT) :: iJulianDay, iNumGridCells, iCount
  character(len=3) :: sMonthName
  real(kind=T_SGL),dimension(iNX*iNY) :: rVal,rValSum,rPad
  real(kind=T_SGL) :: rZA, rZE, rZOR, rZSTEP
  real(kind=T_SGL) :: rZA_TEMP, rZE_TEMP, rZOR_TEMP, rZSTEP_TEMP
  character(len=256) :: sBuf
  character(len=1) sDelimiter
  logical (kind=T_LOGICAL) :: lMonthEnd
  logical (kind=T_LOGICAL) :: lEOF

  iNumGridCells = iNY * iNX
  rPad = -9999_T_SGL

  if(len_trim(pConfig%sOutputFilePrefix)==0) then

    sDelimiter = ""

  else

    sDelimiter = "_"

  end if


  ! we need a grid data structure in order that we might call
  ! the grid-writing subroutines elsewhere in code
  pGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, T_SGL_GRID)

  do k=1,iNUM_VARIABLES

!  call CHDIR( 'output')

    ! if we don't have any output requested for the current variable,
    ! skip the remainder of the loop and go on to the next variable.
    if(STAT_INFO(k)%iDailyOutput == iNONE &
     .and. STAT_INFO(k)%iMonthlyOutput == iNONE &
     .and. STAT_INFO(k)%iAnnualOutput == iNONE)  cycle

    ! ensure any remaining data in buffer is written to the binary file before
    ! rewinding and analyzing
    flush(STAT_INFO(k)%iLU)


!**************************************************************************
! DAILY GRID and PLOT GENERATION
!**************************************************************************

    if(STAT_INFO(k)%iDailyOutput>iNONE) then

      rewind(STAT_INFO(k)%iLU)
      write(STAT_INFO(k)%iLU,POS=iENDHEADER_POS)

      write(LU_LOG, &
         fmt='("CALCULATING DAILY STATS for ",A)') &
           TRIM(STAT_INFO(k)%sVARIABLE_NAME)
      flush(LU_LOG)

      ! we are presuming that the binary files are already open.  SHould
      ! improve error trapping here.  If we fail to read in the header info,
      ! the app should die violently!@!

      do
        read(UNIT=STAT_INFO(k)%iLU,iostat=iStat) iDay,iMonth, iYear, iDayOfYear
        if(iStat /= 0) then
!        write(UNIT=LU_LOG,FMT=*) 'exiting loop in stats.f95 '
!        write(UNIT=LU_LOG,FMT=*) 'iStat = ',iStat
          exit
        end if

#ifdef DEBUG_PRINT
      write(LU_LOG, &
        fmt='("CALCULATING DAILY STATS for ",A,": ",i2.2,"/",i2.2,"/",i4,"  (day ",i3,")")') &
        TRIM(STAT_INFO(k)%sVARIABLE_NAME),iMonth,iDay,iYear,iDayOfYear
        write(UNIT=LU_LOG,FMT=*) 'CALCULATING DAILY STATS for ',TRIM(STAT_INFO(k)%sVARIABLE_NAME), &
         ": ",iMonth,'/',iDay,'/',iYear,'  (day',iDayOfYear,')'
#endif

        call LookupMonth(iMonth, iDay, iYear,iDayOfYear, &
                       sMonthName,lMonthEnd)

        pGrd%rData(:,:)= rZERO

       ! name "RLE_readByte" is misleading, since the return value (rVal)
       ! is actually a vector of all daily values with dimension (iNY*iNX)
       call RLE_readByte(STAT_INFO(k)%iLU,pConfig%iRLE_MULT, &
          pConfig%rRLE_OFFSET, rVal,iNumGridCells,lEOF)
       if(lEOF) exit

       pGrd%rData(:,:)=RESHAPE(rVal,(/iNY,iNX/),PAD=rPad,&
           ORDER=(/2,1/))

#ifdef DEBUG_PRINT
       call stats_WriteMinMeanMax(LU_LOG, TRIM(STAT_INFO(k)%sVARIABLE_NAME), &
          pGrd%rData(:,:))
#endif

       if(STAT_INFO(k)%iDailyOutput==iGRID &
          .or. STAT_INFO(k)%iDailyOutput==iBOTH) then

         write(sBuf,FMT="(A,A,A,A,'_',i4.4,'_',i2.2,'_',i2.2,'.',a)") &
           "output"//pConfig%sSlash//"daily"//pConfig%sSlash, &
           trim(pConfig%sOutputFilePrefix), &
           trim(sDelimiter), &
           TRIM(STAT_INFO(k)%sVARIABLE_NAME), &
           iYear,iMonth,iDay, &
           trim(pConfig%sOutputFileSuffix)

          if ( pConfig%iOutputFormat == OUTPUT_SURFER ) then

            call grid_WriteSurferGrid(TRIM(sBuf), &
              rX0,rX1,rY0,rY1,pGrd%rData(:,:))

          else

            call grid_WriteArcGrid(TRIM(sBuf), &
              rX0,rX1,rY0,rY1,pGrd%rData(:,:))

          end if

       end if

#ifdef GRAPHICS_SUPPORT

       if(STAT_INFO(k)%iDailyOutput==iGRAPH &
          .or. STAT_INFO(k)%iDailyOutput==iBOTH) then

         write(sBuf,FMT="(A,A,A,A,'_',i4.4,'_',i2.2,'_',i2.2,'.',a)") &
           "images"//pConfig%sSlash//"daily"//pConfig%sSlash, &
           trim(pConfig%sOutputFilePrefix), &
           trim(sDelimiter), &
           TRIM(STAT_INFO(k)%sVARIABLE_NAME), &
           iYear,iMonth,iDay, &
           TRIM(pGraph(k)%cCDEV)

          pGraph(k)%cSETFIL = TRIM(sBuf)

          write(sBuf,FMT="(A,' ',i2.2,' ',i2.2,' ',i4.4)") &
            TRIM(STAT_INFO(k)%sVARIABLE_NAME),iMonth,iDay,iYear
          pGraph(k)%cTITLE = TRIM(sBuf)

          if( pGraph(k)%rZE(iDAILY) > pGraph(k)%rZA(iDAILY) .and. &
                pGraph(k)%rZSTEP(iDAILY) > rZERO) then

            pGraph(k)%iTimeFrame = iDAILY

            call makegraph(pGraph,pGrd,k)

          end if

       end if
#endif

      end do

    end if
!**************************************************************************
! MONTHLY GRID and PLOT GENERATION
!**************************************************************************
! now do it again, only this time sum values by month

  if(STAT_INFO(k)%iMonthlyOutput>iNONE) then

    rewind(STAT_INFO(k)%iLU)
    write(STAT_INFO(k)%iLU,POS=iENDHEADER_POS)

    write(LU_LOG, &
       fmt="('CALCULATING MONTHLY SUMS for: ',A)") &
       TRIM(STAT_INFO(k)%sVARIABLE_NAME)
    flush(LU_LOG)


    rValSum = rZERO

    do
      read(UNIT=STAT_INFO(k)%iLU,iostat=iStat) iDay,iMonth, iYear, iDayOfYear
      if(iStat /= 0) then
!        write(UNIT=LU_LOG,FMT=*) 'exiting loop in stats.f95 '
!        write(UNIT=LU_LOG,FMT=*) 'iStat = ',iStat
        exit
      end if

      call LookupMonth(iMonth, iDay, iYear,iDayOfYear, &
                       sMonthName,lMonthEnd)
!
! Must read in a byte from each data set that has been written to disk for
! this timestep
!
     call RLE_readByte(STAT_INFO(k)%iLU,pConfig%iRLE_MULT, &
        pConfig%rRLE_OFFSET,rVal,iNumGridCells,lEOF)
     if(lEOF) exit

     rValSum = rValSum + rVal

     if(lMonthEnd) then

#ifdef DEBUG_PRINT
      write(LU_LOG, &
        fmt="('CALCULATING MONTHLY SUMS for ',A,': ',A,' ',i4)") &
        TRIM(STAT_INFO(k)%sVARIABLE_NAME),YEAR_INFO(iMonth)%sFullName,iYear

       write(UNIT=LU_LOG,FMT=*) 'CALCULATING MONTHLY SUMS for ',TRIM(STAT_INFO(k)%sVARIABLE_NAME), &
          ": ",sMonthName,' ',iYear
#endif

       pGrd%rData(:,:)=RESHAPE(rValSum,(/iNY,iNX/),PAD=rPad,&
          ORDER=(/2,1/))

#ifdef DEBUG_PRINT
       call stats_WriteMinMeanMax(LU_LOG, TRIM(STAT_INFO(k)%sVARIABLE_NAME), &
          pGrd%rData(:,:))
#endif

       if(STAT_INFO(k)%iMonthlyOutput == iGRID &
        .or. STAT_INFO(k)%iMonthlyOutput==iBOTH) then

         write(sBuf,FMT="(A,A,A,A,'_',i4.4,'_',i2.2,'.',a)") &
           "output"//pConfig%sSlash//"monthly"//pConfig%sSlash, &
           TRIM(pConfig%sOutputFilePrefix), &
           trim(sDelimiter), &
           TRIM(STAT_INFO(k)%sVARIABLE_NAME),iYear,iMonth, &
           TRIM(pConfig%sOutputFileSuffix)

         if ( pConfig%iOutputFormat == OUTPUT_SURFER ) then

           call grid_WriteSurferGrid(TRIM(sBuf), &
              rX0,rX1,rY0,rY1,pGrd%rData(:,:))

         else

           call grid_WriteArcGrid(TRIM(sBuf), &
              rX0,rX1,rY0,rY1,pGrd%rData(:,:))

         end if

       end if

#ifdef GRAPHICS_SUPPORT

       if(STAT_INFO(k)%iMonthlyOutput==iGRAPH &
          .or. STAT_INFO(k)%iMonthlyOutput==iBOTH) then

         write(sBuf,FMT="(A,A,A,A,'_',i4.4,'_',i2.2,'.',a)") &
           "images"//pConfig%sSlash//"monthly"//pConfig%sSlash, &
           TRIM(pConfig%sOutputFilePrefix), &
           trim(sDelimiter), &
           TRIM(STAT_INFO(k)%sVARIABLE_NAME),iYear,iMonth, &
           TRIM(pGraph(k)%cCDEV)
         pGraph(k)%cSETFIL = TRIM(sBuf)

         write(sBuf,FMT="(A,' ',A,' ',i4.4)") &
            TRIM(STAT_INFO(k)%sVARIABLE_NAME),TRIM(YEAR_INFO(iMonth)%sFullName),&
              iYear
         pGraph(k)%cTITLE = TRIM(sBuf)

         if( pGraph(k)%rZE(iMONTHLY) > pGraph(k)%rZA(iMONTHLY) &
            .and. pGraph(k)%rZSTEP(iMONTHLY) > rZERO) then

           pGraph(k)%iTimeFrame = iMONTHLY

           call makegraph(pGraph,pGrd,k)

         end if

       end if
#endif

       rValSum = rZERO
     end if

    end do

  endif
!
!**************************************************************************
! ANNUAL GRID and PLOT GENERATION
!**************************************************************************
! now do it again, only this time sum values by YEAR

  if(STAT_INFO(k)%iAnnualOutput>iNONE) then

    rewind(STAT_INFO(k)%iLU)
    write(STAT_INFO(k)%iLU,POS=iENDHEADER_POS)

    rValSum(:) = rZERO

    write(LU_LOG,fmt="('CALCULATING ANNUAL SUMS for ',A)") &
         TRIM(STAT_INFO(k)%sVARIABLE_NAME)
    flush(LU_LOG)

    do
      read(UNIT=STAT_INFO(k)%iLU,iostat=iStat) iDay,iMonth, iYear, iDayOfYear
      if(iStat /= 0) then
!        write(UNIT=LU_LOG,FMT=*) 'exiting loop in stats.f95 '
!        write(UNIT=LU_LOG,FMT=*) 'iStat = ',iStat
        exit
      end if

      call LookupMonth(iMonth, iDay, iYear,iDayOfYear, &
                       sMonthName,lMonthEnd)

      iStartYear = iYear

! Must read in a byte from each data set that has been written to disk for
! this timestep
!
     call RLE_readByte(STAT_INFO(k)%iLU,pConfig%iRLE_MULT, &
         pConfig%rRLE_OFFSET,rVal,iNumGridCells,lEOF)
     if(lEOF) exit

     rValSum = rValSum + rVal

!     call stats_WriteMinMeanMax(LU_LOG, TRIM(STAT_INFO(k)%sVARIABLE_NAME), &
!        RESHAPE(rValSum,(/iNY,iNX/),PAD=rPad,ORDER=(/2,1/)))

!     if(lMonthEnd) then
!     write(LU_LOG, &
!       fmt="('CALCULATING ANNUAL SUMS for ',A,': ',A,' ',i4)") &
!         TRIM(STAT_INFO(k)%sVARIABLE_NAME),YEAR_INFO(iMonth)%sFullName,iYear
!     end if

     if(lMonthEnd .and. iMonth == 12) then

       pGrd%rData(:,:)=RESHAPE(rValSum,(/iNY,iNX/),PAD=rPad,&
           ORDER=(/2,1/))

       call stats_WriteMinMeanMax(LU_LOG, STAT_INFO(k)%sVARIABLE_NAME, &
          pGrd%rData(:,:))

       if(STAT_INFO(k)%iAnnualOutput == iGRID &
        .or. STAT_INFO(k)%iAnnualOutput==iBOTH) then

         write(sBuf,FMT="(A,A,A,A,'_',i4.4,'.',a)") &
           "output"//pConfig%sSlash//"annual"//pConfig%sSlash, &
            TRIM(pConfig%sOutputFilePrefix), &
            trim(sDelimiter), &
            TRIM(STAT_INFO(k)%sVARIABLE_NAME),iYear, &
            TRIM(pConfig%sOutputFileSuffix)

         if ( pConfig%iOutputFormat == OUTPUT_SURFER ) then

           call grid_WriteSurferGrid(TRIM(sBuf), &
              rX0,rX1,rY0,rY1,pGrd%rData(:,:))

         else

           call grid_WriteArcGrid(TRIM(sBuf), &
              rX0,rX1,rY0,rY1,pGrd%rData(:,:))

         end if

       end if

#ifdef GRAPHICS_SUPPORT

       if(STAT_INFO(k)%iAnnualOutput==iGRAPH &
          .or. STAT_INFO(k)%iAnnualOutput==iBOTH) then

         write(sBuf,FMT="(A,A,A,A,'_',i4.4,'.',A)") &
            "images"//pConfig%sSlash//"annual"//pConfig%sSlash, &
            trim(pConfig%sOutputFilePrefix), &
            trim(sDelimiter), &
            TRIM(STAT_INFO(k)%sVARIABLE_NAME),iYear, &
            TRIM(pGraph(k)%cCDEV)
         pGraph(k)%cSETFIL = TRIM(sBuf)

         write(sBuf,FMT="(A,' ',i4.4)") &
            TRIM(STAT_INFO(k)%sVARIABLE_NAME), iYear
         pGraph(k)%cTITLE = TRIM(sBuf)

         if( pGraph(k)%rZE(iANNUAL) > pGraph(k)%rZA(iANNUAL) &
            .and. pGraph(k)%rZSTEP(iANNUAL) > rZERO) then

           pGraph(k)%iTimeFrame = iANNUAL

           call makegraph(pGraph,pGrd,k)

         end if

       end if

#endif

       pGrd%rData(:,:)= rZERO
       rValSum = rZERO

     end if

    end do

  endif

  end do

  return

end subroutine stats_RewriteGrids

!--------------------------------------------------------------------------

subroutine stats_CalcBasinStats(pGrd, pConfig, pGraph)

  type (T_GENERAL_GRID),pointer :: pGrd            ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  type (T_GRAPH_CONFIGURATION), dimension(:), pointer :: pGraph
     ! pointer to data structure that holds parameters for creating
     ! DISLIN plots

  ![LOCALS]
  type ( T_LANDUSE_LOOKUP ),pointer :: pLU  ! pointer to landuse data structure

  type ( T_GENERAL_GRID ),pointer :: pTmpGrd

  integer (kind=T_INT) :: j, k, iStat, iCount, n, iMajorityLU, iMajoritySoil
  integer (kind=T_INT) ::  iLU_Max, iSoil_Max, iNumGridCells
  real (kind=T_SGL) :: rSum, rAvg, rMin, rMax

  character (len=256) :: sBuf
  character (len=1) :: sTab = CHAR(9)

  type (T_GENERAL_GRID),pointer :: input_grd    ! Pointer to temporary grid for I/O

  call assert(associated(pConfig%BMASK), "Cannot calculate basin statistics; " &
    //"must supply a basin mask file to use this feature", &
    trim(__FILE__),__LINE__)

  pTmpGrd => grid_Create(pGrd%iNX, pGrd%iNY, pGrd%rX0, pGrd%rY0, &
      pGrd%rX1, pGrd%rY1, T_SGL_GRID)

  open(UNIT=LU_MASK_STATS_CSV,FILE="SWB_BASIN_STATS.txt",iostat=iStat,STATUS='REPLACE')
  call Assert ( iStat == 0, &
    "Could not open basin mask statistics file")

  open(UNIT=LU_PEST_STATS,FILE="SWB_PEST_STATS.txt",iostat=iStat,STATUS='REPLACE')
  call Assert ( iStat == 0, &
    "Could not open PEST statistics file")

  open(UNIT=LU_PEST_OBS,FILE="SWB_PEST_OBSERVATIONS.txt",iostat=iStat,STATUS='REPLACE')
  call Assert ( iStat == 0, &
    "Could not open PEST observations file")

  open(UNIT=LU_PEST_INS,FILE="SWB_PEST_INSTRUCTIONS.ins",iostat=iStat,STATUS='REPLACE')
  call Assert ( iStat == 0, &
    "Could not open PEST instructions file")

  write(unit=LU_PEST_INS,FMT="(A)") "pif $"
  write(unit=LU_PEST_OBS,FMT="(A)") "* observation data"

  if(pConfig%iStartYearforCalculation<pConfig%iStartYear) &
    pConfig%iStartYearforCalculation = pConfig%iStartYear

  if(pConfig%iEndYearforCalculation>pConfig%iEndYear) &
    pConfig%iEndYearforCalculation = pConfig%iEndYear

  n = pConfig%iEndYearforCalculation - pConfig%iStartYearforCalculation + 1

  pTmpGrd%rData = pGrd%Cells%rSUM_Recharge/n

  write(UNIT=LU_LOG,FMT=*) "Number of years in simulation:",n

  write(UNIT=LU_MASK_STATS_CSV,FMT=*) "USGS_UpstreamOrderNum" &
    //sTab//"Basin_Description"//sTab//"Drainage_area"//sTab &
    //"Majority Land Use"//sTab//"Majority Soil Type"//sTab &
    //"SWB_Mean_Recharge"//sTab//"SWB_Min_Recharge"//sTab &
    //"SWB_Max_Recharge"//sTab//"Baseflow_Recharge"

  do k = 1,size(pConfig%BMASK,1)

    write(UNIT=LU_LOG,FMT=*) k," : Attempting to read mask file: ", &
       TRIM(pConfig%BMASK(k)%sBasinMaskFilename)
    input_grd => grid_Read(TRIM(pConfig%BMASK(k)%sBasinMaskFilename), &
       "ARC_GRID", T_SGL_GRID )
    call Assert( grid_Conform( pGrd, input_grd ), &
              "Non-conforming grid - filename: " &
              // TRIM(pConfig%BMASK(k)%sBasinMaskFilename))

    iCount = COUNT(input_grd%rData>0)

    ! sum of the sum of annual recharge values
    rSum = SUM(pGrd%Cells%rSUM_Recharge,MASK=input_grd%rData>0)
    rMax = MAXVAL(pGrd%Cells%rSUM_Recharge,MASK=input_grd%rData>0) / n
    rMin = MINVAL(pGrd%Cells%rSUM_Recharge,MASK=input_grd%rData>0) / n

    rAvg = rSum / iCount / n

    iMajorityLU = 0
    iMajoritySoil = 0
    iLU_Max = 0
    iSoil_Max = 0

    ! iterate through all land use types
    do j = 1,size(pConfig%LU,1)

        ! establish number of cells in model grid
        iNumGridCells = COUNT( &
          INT(pGrd%Cells%iLanduse,kind=T_INT)==pConfig%LU(j)%iLandUseType &
          .and. input_grd%rData>0 )

        if(iNumGridCells>iLU_Max) then
          iLU_Max = iNumGridCells
          iMajorityLU = j
        end if

    end do

    do j=1,maxval(INT(pGrd%Cells%iSoilGroup,kind=T_INT))

      ! establish number of cells in model grid for THIS combination
      iNumGridCells = COUNT( &
        INT(pGrd%Cells%iSoilGroup,kind=T_INT)==j &
          .and. input_grd%rData>0 )

        if(iNumGridCells>iSoil_Max) then
          iSoil_Max = iNumGridCells
          iMajoritySoil = j
        end if

    end do

    write(UNIT=LU_LOG,FMT="(A)") ""
    write(UNIT=LU_LOG,FMT="(5x,A)") TRIM(pConfig%BMASK(k)%sBasinDescription)
    write(UNIT=LU_LOG,FMT="(5x,'Drainage area (sq mi):', f14.2)") &
        pConfig%BMASK(k)%rDrainageArea
    write(UNIT=LU_LOG,FMT="(5x,'Majority Land Use Type:',a)") &
        TRIM(pConfig%LU(iMajorityLU)%sLanduseDescription)
    write(UNIT=LU_LOG,FMT="(5x,'Majority Soil Type:',i4)") &
        iMajoritySoil
    write(UNIT=LU_LOG,FMT="(8x,A7,i12)") "count:",iCount
    write(UNIT=LU_LOG,FMT="(8x,A7,f14.2)") "sum:",rSum
    write(UNIT=LU_LOG,FMT="(8x,2(A7,f14.2))") "avg:",rAvg,"Qb:",pConfig%BMASK(k)%rQb
    write(UNIT=LU_LOG,FMT="(A)") REPEAT("-",80)
    write(UNIT=LU_MASK_STATS_CSV,FMT="(A,a,A,a,f16.4,3a,i4,a,4(f16.4,a))") &
       TRIM(pConfig%BMASK(k)%sUSGS_UpstreamOrderID),sTab, &
       TRIM(pConfig%BMASK(k)%sBasinDescription),sTab, &
       pConfig%BMASK(k)%rDrainageArea,sTab, &
       TRIM(pConfig%LU(iMajorityLU)%sLanduseDescription), sTab,iMajoritySoil, sTab,&
       rAvg,sTab,rMin,sTab,rMax,sTab,pConfig%BMASK(k)%rQb, sTab

    write(UNIT=LU_PEST_STATS,FMT="(A,f16.6)") &
       ADJUSTL("s"//TRIM(pConfig%BMASK(k)%sUSGS_UpstreamOrderID)),rAvg

    if(pConfig%lWriteExtraPestFiles) then
      write(UNIT=LU_PEST_OBS,FMT="(A,f8.3,f16.6)") &
       ADJUSTL("o"//TRIM(pConfig%BMASK(k)%sUSGS_UpstreamOrderID)), &
       pConfig%BMASK(k)%rPestWeight, pConfig%BMASK(k)%rQb
      write(UNIT=LU_PEST_INS,FMT="(A,A,A)") &
       "$s"//TRIM(pConfig%BMASK(k)%sUSGS_UpstreamOrderID)//"$ w !", &
          ADJUSTL("s"//TRIM(pConfig%BMASK(k)%sUSGS_UpstreamOrderID)),"!"
    end if

    close(UNIT=LU_MASK_FILE)

  end do

  flush(UNIT=LU_MASK_STATS_CSV)
  flush(UNIT=LU_PEST_STATS)
  close(UNIT=LU_MASK_STATS_CSV)
  close(UNIT=LU_PEST_STATS)
  close(unit=LU_PEST_OBS)
  close(unit=LU_PEST_INS)

end subroutine stats_CalcBasinStats

!----------------------------------------------------------------------

subroutine stats_CalcMeanRecharge(pGrd, pConfig, pGraph)

  type (T_GENERAL_GRID),pointer :: pGrd            ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  type (T_GRAPH_CONFIGURATION), dimension(:), pointer :: pGraph
     ! pointer to data structure that holds parameters for creating
     ! DISLIN plots

  type ( T_GENERAL_GRID ),pointer :: pTmpGrd

  integer (kind=T_INT) :: k,n

  character (len=256) :: sBuf

  type (T_GENERAL_GRID),pointer :: input_grd    ! Pointer to temporary grid for I/O

  pTmpGrd => grid_Create(pGrd%iNX, pGrd%iNY, pGrd%rX0, pGrd%rY0, &
      pGrd%rX1, pGrd%rY1, T_SGL_GRID)

  if(pConfig%iStartYearforCalculation<pConfig%iStartYear) &
    pConfig%iStartYearforCalculation = pConfig%iStartYear

  if(pConfig%iEndYearforCalculation>pConfig%iEndYear) &
    pConfig%iEndYearforCalculation = pConfig%iEndYear

  n = pConfig%iEndYearforCalculation - pConfig%iStartYearforCalculation + 1

  pTmpGrd%rData = pGrd%Cells%rSUM_Recharge/n

  write(LU_LOG, FMT="(a,i4.4,'-',i4.4,':')") &
    "Average recharge over period ",pConfig%iStartYearforCalculation,&
      pConfig%iEndYearforCalculation

  call stats_WriteMinMeanMax(LU_LOG, "MEAN RECHARGE", &
          pTmpGrd%rData)

  write(LU_LOG,FMT="('MEAN RECHARGE, EXCLUDING OPEN WATER CELLS:',f14.3)") &
    SUM(pTmpGrd%rData,pGrd%Cells%rSoilWaterCap > rNear_ZERO &
            .and. pGrd%Cells%iLandUse /= pConfig%iOPEN_WATER_LU) / &
    COUNT(pGrd%Cells%rSoilWaterCap > rNear_ZERO &
            .and. pGrd%Cells%iLandUse /= pConfig%iOPEN_WATER_LU)


  write(sBuf,FMT="('output',a,'MEAN_',a,'_',i4.4,'_',i4.4,'.asc')") &
    TRIM(pConfig%sSlash), &
    "RECHARGE",pConfig%iStartYearforCalculation, &
    pConfig%iEndYearforCalculation
  call grid_WriteArcGrid(sBuf,pGrd%rX0,pGrd%rX1,pGrd%rY0,pGrd%rY1, &
      pTmpGrd%rData)


  write(sBuf,FMT="('images',a,'MEAN_',a,i4.4,'_'i4.4,a)") &
            TRIM(pConfig%sSlash), &
            "RECHARGE",pConfig%iStartYearforCalculation, &
            pConfig%iEndYearforCalculation, &
            "."//TRIM(pGraph(iRECHARGE)%cCDEV)
  pGraph(iRECHARGE)%cSETFIL = TRIM(sBuf)

  write(sBuf,FMT="(A,A,' ',i4.4,'-',i4.4)") "MEAN ", &
    "RECHARGE", pConfig%iStartYearforCalculation, &
      pConfig%iEndYearforCalculation

#ifdef GRAPHICS_SUPPORT
  pGraph(iRECHARGE)%cTITLE = TRIM(sBuf)
  pGraph(iRECHARGE)%iTimeFrame = iANNUAL
  call makegraph(pGraph,pTmpGrd,iRECHARGE)
#endif

  write(LU_LOG,FMT=*) REPEAT("-",80)

  return

end subroutine stats_CalcMeanRecharge
!--------------------------------------------------------------------------

subroutine stats_CalcMeanRechargebyLU(pGrd, pConfig, pGraph)

  type (T_GENERAL_GRID),pointer :: pGrd            ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  type (T_GRAPH_CONFIGURATION), dimension(:), pointer :: pGraph
     ! pointer to data structure that holds parameters for creating
     ! DISLIN plots

  type ( T_GENERAL_GRID ),pointer :: pTmpGrd

  ![LOCALS]
  type ( T_LANDUSE_LOOKUP ),pointer :: pLU  ! pointer to landuse data structure
  integer (kind=T_INT) :: iNumGridCells

  integer (kind=T_INT) :: i,k,n

  character (len=256) :: sBuf

  if(pConfig%iStartYearforCalculation<pConfig%iStartYear) &
    pConfig%iStartYearforCalculation = pConfig%iStartYear

  if(pConfig%iEndYearforCalculation>pConfig%iEndYear) &
    pConfig%iEndYearforCalculation = pConfig%iEndYear

  write(LU_LOG, FMT="(a,i4.4,'-',i4.4,':')") &
    "Average recharge over period ", &
      pConfig%iStartYearforCalculation,&
      pConfig%iEndYearforCalculation

  pTmpGrd => grid_Create(pGrd%iNX, pGrd%iNY, pGrd%rX0, pGrd%rY0, &
    pGrd%rX1, pGrd%rY1, T_SGL_GRID)

  if(pConfig%iStartYearforCalculation<0) &
    pConfig%iStartYearforCalculation = pConfig%iStartYear

  if(pConfig%iEndYearforCalculation<0) &
    pConfig%iEndYearforCalculation = pConfig%iEndYear

  n = pConfig%iEndYearforCalculation - pConfig%iStartYearforCalculation + 1

  pTmpGrd%rData = pGrd%Cells%rSUM_Recharge/n

  ! iterate through all land use types
  do k = 1,size(pConfig%LU,1)

        !create pointer to a specific land use type
        pLU => pConfig%LU(k)
		call Assert(LOGICAL(associated(pLU),kind=T_LOGICAL), &
		   "pointer association failed - stats - CalcMeanRechargebyLU")

      ! establish number of cells in model grid
      iNumGridCells = COUNT( &
        INT(pGrd%Cells%iLanduse,kind=T_INT)==pLU%iLandUseType )

      if(iNumGridCells>0) then

        write(LU_LOG,"(a45,3(f12.2,2x))") &
          adjustl(pLU%sLandUseDescription), &
          minval(pTmpGrd%rData, &
          INT(pGrd%Cells%iLanduse,kind=T_INT)==pLU%iLandUseType ), &
          sum(pTmpGrd%rData, &
          INT(pGrd%Cells%iLanduse,kind=T_INT)==pLU%iLandUseType ) &
             /iNumGridCells, &
          maxval(pTmpGrd%rData, &
          INT(pGrd%Cells%iLanduse,kind=T_INT)==pLU%iLandUseType )

        else

          write(LU_LOG,"(a45)") &
           adjustl(pLU%sLandUseDescription)
        end if

  end do

  write(LU_LOG, FMT=*) ""
  write(LU_LOG, FMT="(a,i4.4,'-',i4.4,':')") &
    "Average recharge by landuse and soil type over period ", &
      pConfig%iStartYearforCalculation,&
      pConfig%iEndYearforCalculation

  write(LU_LOG,"(a30,' Soil Type ',4(a12,2x))") &
    "Land Use", "Min (in)","Mean (in)","Max (in)","Count"

  ! Now iterate through all land use types *AND* soil types
  do k = 1,size(pConfig%LU,1)

    do i=1,maxval(INT(pGrd%Cells%iSoilGroup,kind=T_INT))

        !create pointer to a specific land use type
        pLU => pConfig%LU(k)
		call Assert(LOGICAL(associated(pLU),kind=T_LOGICAL), &
		   "pointer association failed - stats - CalcMeanRechargebyLU")

      ! establish number of cells in model grid for THIS combination
      iNumGridCells = COUNT( &
        INT(pGrd%Cells%iLanduse,kind=T_INT)==pLU%iLandUseType &
           .and. INT(pGrd%Cells%iSoilGroup,kind=T_INT)==i)

      if(iNumGridCells>0) then

        write(LU_LOG,"(a45,' Soil:',i2,'| ',3(f12.2,2x),i12)") &
          adjustl(pLU%sLandUseDescription), &
          i, &
          minval(pTmpGrd%rData, &
          INT(pGrd%Cells%iLanduse,kind=T_INT)==pLU%iLandUseType  &
           .and. INT(pGrd%Cells%iSoilGroup,kind=T_INT)==i), &
          sum(pTmpGrd%rData, &
          INT(pGrd%Cells%iLanduse,kind=T_INT)==pLU%iLandUseType  &
           .and. INT(pGrd%Cells%iSoilGroup,kind=T_INT)==i) &
             /iNumGridCells, &
          maxval(pTmpGrd%rData, &
          INT(pGrd%Cells%iLanduse,kind=T_INT)==pLU%iLandUseType  &
           .and. INT(pGrd%Cells%iSoilGroup,kind=T_INT)==i), &
          iNumGridCells


        else

          write(LU_LOG,"(a45,' Soil:',i2,'| ')") &
           adjustl(pLU%sLandUseDescription), i

        end if

    end do

  end do

  write(LU_LOG,FMT=*) REPEAT("-",80)

  return

end subroutine stats_CalcMeanRechargebyLU

!----------------------------------------------------------------------

subroutine stats_write_to_SSF_file(pConfig, iSSFindex, iMonth, iDay, iYear, rValue)

  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  integer (kind=T_INT), intent(in) :: iSSFindex
  integer (kind=T_INT), intent(in) :: iMonth, iDay, iYear
  real (kind=T_SGL), intent(in) :: rValue

  ! [ LOCALS ]
  integer (kind=T_INT) :: i
  integer (kind=T_INT) :: iStat
  character(len=128) :: sBuf
  type (T_SSF_FILES), pointer :: pSSF

!  if(associated(pConfig%SSF_FILES)) then

    ! MUST ensure that we don't attempt to create a pointer to an
    ! element that doesn't exist!!
    call Assert(iSSFindex <= size(pConfig%SSF_FILES) .and. &
        iSSFindex > 0, "Internal programming error - " &
        //"Value for SSFindex falls outside legal bounds: " &
        //TRIM(int2char(iSSFindex)), &
        TRIM( __FILE__ ), __LINE__ )

    pSSF => pConfig%SSF_FILES(iSSFindex)

    open(unit=pSSF%iLU, file=TRIM(pSSF%sFileName),status='OLD', &
        access='APPEND', iostat=iStat)

    sBuf = TRIM(int2char(pSSF%iLU))//"; filename = "//TRIM(pSSF%sFileName)

    call Assert(iStat==0,"Error opening file on unit "//TRIM(sBuf), &
        TRIM(__FILE__),__LINE__)

    write(unit=pSSF%iLU,fmt="(a,2x,i2.2,'/',i2.2,'/',i4.4,2x," &
        //"'23:59:59',2x,f14.4)") &
        TRIM(int2char(pSSF%iColNum))//"_"//TRIM(int2char(pSSF%iRowNum)), &
        iMonth, iDay, iYear, &
        rValue

    close(unit=pSSF%iLU, iostat=iStat)
      call Assert(iStat==0,"Error closing file on unit "//TRIM(sBuf), &
        TRIM(__FILE__),__LINE__)

!  end if

  return

end subroutine stats_write_to_SSF_file

!--------------------------------------------------------------------------

subroutine stats_OpenBinaryFiles(pConfig, pGrd)

  ![ARGUMENTS]
  type (T_MODEL_CONFIGURATION), pointer :: pConfig
  type (T_GENERAL_GRID),pointer :: pGrd                ! pointer to model grid

  ! [ LOCALS ]
  integer(kind=T_INT) :: i

  do i=1,iNUM_VARIABLES

    if(STAT_INFO(i)%iDailyOutput > iNONE &
        .or. STAT_INFO(i)%iMonthlyOutput > iNONE &
        .or. STAT_INFO(i)%iAnnualOutput > iNONE)  then

      open(unit=STAT_INFO(i)%iLU, FILE='output'//pConfig%sSlash// &
        TRIM(pConfig%sOutputFilePrefix) //"_" &
        //TRIM(STAT_INFO(i)%sVARIABLE_NAME) // '.bin',FORM='UNFORMATTED', &
        status='REPLACE',ACCESS='STREAM')

      write(UNIT=STAT_INFO(i)%iLU) pGrd%iNX             ! Number of cells in the x-direction
      write(UNIT=STAT_INFO(i)%iLU) pGrd%iNY             ! Number of cells in the y-direction
      write(UNIT=STAT_INFO(i)%iLU) pGrd%iDataType       ! Type of the grid
      write(UNIT=STAT_INFO(i)%iLU) pGrd%rGridCellSize   ! size of one side of a grid cell
      write(UNIT=STAT_INFO(i)%iLU) pGrd%iLengthUnits    ! length units code
      write(UNIT=STAT_INFO(i)%iLU) i                    ! STAT_INFO variable number
      write(UNIT=STAT_INFO(i)%iLU) pConfig%iRLE_MULT    ! RLE Multiplier
      write(UNIT=STAT_INFO(i)%iLU) pConfig%rRLE_OFFSET  ! RLE Offset
      write(UNIT=STAT_INFO(i)%iLU) pGrd%rX0, pGrd%rX1   ! World-coordinate range in X
      write(UNIT=STAT_INFO(i)%iLU) pGrd%rY0, pGrd%rY1   ! World-coordinate range in Y

      inquire(UNIT=STAT_INFO(i)%iLU,POS=iSTARTDATE_POS)  ! establish location to return to
      ! write placeholders for MM/DD/YYYY of start and end of file
      write(UNIT=STAT_INFO(i)%iLU) 9999_T_INT, 9999_T_INT, 9999_T_INT
      inquire(UNIT=STAT_INFO(i)%iLU,POS=iENDDATE_POS)  ! establish location to return to
      write(UNIT=STAT_INFO(i)%iLU) 9999_T_INT, 9999_T_INT, 9999_T_INT
      inquire(UNIT=STAT_INFO(i)%iLU,POS=iENDHEADER_POS)  ! establish location to return to

      write(unit=LU_LOG,fmt=*) "Opened binary file for " &
        //TRIM(STAT_INFO(i)%sVARIABLE_NAME)//" on unit " &
        //TRIM(int2char(STAT_INFO(i)%iLU))

      write(unit=LU_LOG,fmt=*) "start date offset: "//TRIM(int2char(iSTARTDATE_POS))
      write(unit=LU_LOG,fmt=*) "end date offset: "//TRIM(int2char(iENDDATE_POS))

	end if

  end do

end subroutine stats_OpenBinaryFiles

!--------------------------------------------------------------------------

subroutine stats_CloseBinaryFiles()

  ! [ LOCALS ]
  integer(kind=T_INT) :: i

  do i=1,iNUM_VARIABLES

    if(STAT_INFO(i)%iDailyOutput > iNONE &
        .or. STAT_INFO(i)%iMonthlyOutput > iNONE &
        .or. STAT_INFO(i)%iAnnualOutput > iNONE)  then

      close(unit=STAT_INFO(i)%iLU)

      write(unit=LU_LOG,fmt=*) "Closed binary file for " &
        //TRIM(STAT_INFO(i)%sVARIABLE_NAME)//" on unit " &
        //TRIM(int2char(STAT_INFO(i)%iLU))

	end if

  end do

end subroutine stats_CloseBinaryFiles

!--------------------------------------------------------------------------


end module swb_stats
