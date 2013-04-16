!> @file
!> @brief Contains a single module, @ref swb_stats, which calculates
!> daily, monthly, and annual statistics for an SWB model run.

!> @brief Calculates daily, monthly, and annual statistics
!>    for an SWB model run.
!>
!> @par Calculates daily, monthly, and annual statistics
!> for an SWB model run. Also contains routines to:
!> - Extract data from a compressed binary file in order
!> to output grids or plots
!> - Write to a PEST *.ssf file
!> - Calculate statistics for a subset of the model domain (i.e. catchment, county)
!> - Write daily, annual, and mass balance reports
module swb_stats

  use types
  use swb_grid
  use graph
  use RLE

  implicit none
  save

  ! dpVolConvert combines the conversion factor between inches and meters, and
  ! multiplies by the area of a grid cell (assumed to be meters)
  real (kind=T_DBL), public :: dpVolConvert

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

    dpVolConvert = (dpONE / 12.0_T_DBL) &
               *(real(pGrd%rGridCellSize,kind=T_DBL) ** 2_T_DBL) &
               * dpSQM_to_SQFT &
               / 43560_T_DBL
    ! multiply by area of grid cell in acres (yields acre-ft of water)

  else if(pGrd%iLengthUnits == iGRID_LENGTH_UNITS_FEET) then

    dpVolConvert = (dpONE / 12.0_T_DBL) &
               *(real(pGrd%rGridCellSize,kind=T_DBL) ** 2_T_DBL) &
               / 43560_T_DBL
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
  real(kind=T_SGL), intent(in), dimension(:) :: rValue
  type (T_MODEL_CONFIGURATION), pointer :: pConfig

  integer(kind=T_INT), intent(in) :: iVarNum

  character(len=256), dimension(size(rValue)) :: sFormattedText
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
      else if(rValue(i) < -rNEAR_ZERO) then
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
        write(sBuf,'(A7,F14.1,A7,$)') sGREEN,rValue(i),sMassBalanceOperator
      else if(rValue(i) < -rNEAR_ZERO) then
        write(sBuf,'(A7,F14.1,A7,$)') sRED,rValue(i),sMassBalanceOperator
      else
        write(sBuf,'(A7,F14.1,A7,$)') sWHITE,rValue(i),sMassBalanceOperator
      end if
    else
      write(sBuf,'(F14.1,A7,$)') rValue(i),sMassBalanceOperator
    end if
  end if

  sFormattedText(i+1) = sBuf

  return

end subroutine stats_FormatTextString

!--------------------------------------------------------------------------

subroutine stats_WriteDailyAccumulatorValuesCSV(iLU,iMonth,iDay,iYear, iStatistic)

  ![ARGUMENTS]
  integer (kind=T_INT), intent(in) :: iLU
  integer (kind=T_INT), intent(in) :: iMonth,iDay,iYear
  integer (kind=T_INT), intent(in) :: iStatistic

  ![LOCALS]
  integer (kind=T_INT) :: i,j

  select case(iStatistic)

    case(iMIN)

      write(iLU,"(i2.2,A1,i2.2,A1,i4)", advance="no") iMonth,"/",iDay,"/",iYear
      do i=1,iNUM_VARIABLES
        if(STAT_INFO(i)%lActive) &
          write(iLU,"(A, F12.3)" ,advance="no")  ",",rDaily(iMIN,i)
      enddo

    case(iMEAN)

      write(iLU,"(i2.2,A1,i2.2,A1,i4)", advance="no") iMonth,"/",iDay,"/",iYear
      do i=1,iNUM_VARIABLES
        if(STAT_INFO(i)%lActive) &
          write(iLU,"(A, F12.3)" ,advance="no")  ",",rDaily(iMEAN,i)
      enddo

    case(iMAX)

       write(iLU,"(i2.2,A1,i2.2,A1,i4)", advance="no") iMonth,"/",iDay,"/",iYear
       do i=1,iNUM_VARIABLES
         if(STAT_INFO(i)%lActive) &
           write(iLU,"(A, F12.3)" ,advance="no")  ",",rDaily(iMAX,i)
       enddo

   end select

   write(iLU,"(A)") ""
   flush(iLU)

end subroutine stats_WriteDailyAccumulatorValuesCSV

!--------------------------------------------------------------------------

subroutine stats_WriteAnnualAccumulatorValuesCSV(iLU,iYear)

  ![ARGUMENTS]
  integer (kind=T_INT), intent(in) :: iLU
  integer (kind=T_INT), intent(in) :: iYear

  ![LOCALS]
  integer (kind=T_INT) :: i

   write(iLU,"(i4)", advance="no") iYear
   do i=1,iNUM_VARIABLES
     if(STAT_INFO(i)%lActive .and. STAT_INFO(i)%lShowSum) then
       write(iLU,"(A, F12.3)" ,advance="no")  ",",rAnnual(iSUM,i)
     elseif(STAT_INFO(i)%lActive .and. (.not. STAT_INFO(i)%lShowSum )) then
       write(iLU,"(A, F12.3)" ,advance="no")  ",",rAnnual(iMEAN,i)
     endif
   enddo

   write(iLU,"(A)") ""
   flush(iLU)

end subroutine stats_WriteAnnualAccumulatorValuesCSV

!--------------------------------------------------------------------------

subroutine stats_WriteDailyAccumulatorHeaderCSV(iLU, iStatistic)

  ![ARGUMENTS]
  integer (kind=T_INT), intent(in) :: iLU
  integer (kind=T_INT), intent(in) :: iStatistic

  ![LOCALS]
  integer (kind=T_INT) :: i,j

  write(iLU,"(A)",advance="no") "Mass balance?"
  do i=1,iNUM_VARIABLES
    if(STAT_INFO(i)%lActive) &
     write(iLU,"(A)" ,advance="no")  ","//trim(STAT_INFO(i)%sMSB_Note)
  enddo

  write(iLU,"(/,A)",advance="no") "Statistic"
  do i=1,iNUM_VARIABLES
    if(STAT_INFO(i)%lActive) &
      write(iLU,"(A)" ,advance="no")  ","//trim(STAT_NAMES(iStatistic))
  enddo

  write(iLU,"(/,A)", advance="no") "Date"
  do i=1,iNUM_VARIABLES
    if(STAT_INFO(i)%lActive) &
      write(iLU,"(A)",advance="no") ","//STAT_INFO(i)%sVARIABLE_NAME
  enddo

  write(iLU, "(A)" )  ""

  flush(unit=iLU)

end subroutine stats_WriteDailyAccumulatorHeaderCSV

!--------------------------------------------------------------------------

subroutine stats_WriteAnnualAccumulatorHeaderCSV(iLU)

  ![ARGUMENTS]
  integer (kind=T_INT), intent(in) :: iLU

  ![LOCALS]
  integer (kind=T_INT) :: i

   write(iLU,"(A)",advance="no") "Mass balance?"
   do i=1,iNUM_VARIABLES
     if(STAT_INFO(i)%lActive) &
       write(iLU,"(A)" ,advance="no")  ","//trim(STAT_INFO(i)%sMSB_Note)
   enddo

   write(iLU,"(/,A)",advance="no") "Statistic"
   do i=1,iNUM_VARIABLES
     if(STAT_INFO(i)%lActive .and. STAT_INFO(i)%lShowSum) then
       write(iLU,"(A)" ,advance="no")  ","//trim(STAT_NAMES(iSUM))
     elseif(STAT_INFO(i)%lActive .and. (.not. STAT_INFO(i)%lShowSum )) then
       write(iLU,"(A)" ,advance="no")  ","//trim(STAT_NAMES(iMEAN))
     endif
   enddo

   write(iLU,"(/,A)", advance="no") "Year"
   do i=1,iNUM_VARIABLES
     if(STAT_INFO(i)%lActive) &
       write(iLU,"(A)",advance="no") ","//STAT_INFO(i)%sVARIABLE_NAME
   enddo

  write(iLU, "(A)" )  ""

  flush(unit=iLU)

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
                    * dpVolConvert

  rTempArray(iSUM,:) = rTempArray(iSUM,:) * dpVolConvert

  if(pConfig%lANSI_Colors .and. iLU==LU_STD_OUT) write(iLU,FMT=*) sYELLOW

  write(UNIT=iLU,FMT="(24x,4(a10,5x))") 'min(in)','mean(in)','max(in)','sum(ac-ft)'

  if(pConfig%lANSI_Colors .and. iLU==LU_STD_OUT) write(iLU,FMT=*) sWHITE

  do i=1,iNUM_VARIABLES
    if(STAT_INFO(i)%lActive) then
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

    endif

  end do

  write(UNIT=iLU,FMT="(65x,a14)") REPEAT("-",14)
  write(UNIT=iLU,FMT="(49x,a14,F14.2)") 'Mass Balance:', rMassBalance

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
                 * dpVolConvert

  rTempArray = rMonthly(iMonth,:,:) * dpVolConvert

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
  write(UNIT=iLU,FMT="(51x,a14,F14.0)") 'Mass Balance:', rMassBalance

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

  rTempArray(:,:) = rAnnual(:,:)
  rTempArray(iSUM,:) = rAnnual(iSUM,:) * dpVolConvert

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
  write(UNIT=iLU,FMT="(51x,a14,F14.0)") 'Mass Balance:', rMassBalance

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

!> @brief Calculates and writes out monthly min, max, and mean of 2D variable array.
!> @par
!> @param [in] iMonthNum Numerical index (1-12) of the month for which statistics are reported.
!> @param [in] iDaysInMonth Number of days in the reporting month.
!> @deprecated This subroutine will be eliminated in future version of SWB.
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

!> @brief Writes out the min, max, and mean of 2D variable array.
!> @par
!> @param [in] iLU Fortran logical unit number to write output to.
!> @param [in] sText Descriptive text associated with the statistics.
!> @param [in] rData 2-d array of values for which statistics are calculated.
!> @param [in] iCount <em>{Optional} User-supplied divisor for use in calculating the mean.</em>

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

subroutine stats_OpenMSBReport()

  ! [ LOCALS ]
  integer (kind=T_INT) :: iStat

      open(LU_MSB_REPORT, file='SWB_daily_mass_balance_report.csv', &
        iostat=iStat, status='REPLACE')
      call Assert(iStat == 0, &
         "Problem opening mass balance report file for output.", &
         trim(__FILE__),__LINE__)

      write(LU_MSB_REPORT,FMT="(300A)") 'Month,Day,Year,Date,Day of Year,' &
      // 'Mean Avg Temp,' &
      // 'Mean Min Temp,' &
      // 'Mean Max Temp,' &
      // 'CFGI,' &
      // 'Min Adj CN,' &
      // 'Mean Adj CN,' &
      // 'Max Adj CN,' &
      // 'Gross Precip,' &
      // 'Interception,' &
      // 'Net Rainfall,' &
      // 'Snowfall,' &
      // 'TOTAL Surface Storage (snow),' &
      // 'Change in Surface Storage (snow),' &
      // 'Snowmelt,' &
#ifdef IRRIGATION_MODULE
      // 'Irrigation,' &
#endif
      // 'TOTAL Soil Moisture Storage,' &
      // 'Change in Soil Moisture Storage, Surface Flow Out of Grid,' &
      // 'Rejected Recharge,' &
      // 'Actual Evapotranspiration,' &
      // 'Recharge,' &
#ifdef STREAM_INTERACTIONS
      // 'Stream Capture,' &
#endif
      // 'MASS BALANCE'

end subroutine stats_OpenMSBReport

!------------------------------------------------------------------------------


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

       rDailyMSB =  rDaily(iSUM,iSNOWMELT) &
                  + rDaily(iSUM,iNET_PRECIP) &
                  + rDaily(iSUM,iINFLOW) &
#ifdef IRRIGATION_MODULE
                  + rDaily(iSUM,iIRRIGATION) &
#endif
                  - rDaily(iSUM,iOUTFLOW) &
                  - rDaily(iSUM,iRUNOFF_OUTSIDE) &
                  - rDaily(iSUM,iREJECTED_RECHARGE) &
                  - rDaily(iSUM,iCHG_IN_SOIL_MOIST) &
                  - rDaily(iSUM,iACT_ET) &
#ifdef STREAM_INTERACTIONS
                  - rDaily(iSUM,iSTREAM_CAPTURE) &
#endif
                  - rDaily(iSUM,iRECHARGE)

      write( unit=LU_MSB_REPORT, &
           fmt='(I2.2,",",I2.2,",",I4,",",I2.2,"/",I2.2,"/",I4,",",I3,",",22(F14.2,","),F14.2)' ) &
                         iMonth,iDay,iYear,iMonth,iDay,iYear,iDayOfyear, &
                         SUM(pGrd%Cells(:,:)%rTAvg)/SIZE(pGrd%Cells(:,:)%rTAvg), &
                         SUM(pGrd%Cells(:,:)%rTMin)/SIZE(pGrd%Cells(:,:)%rTMin), &
                         SUM(pGrd%Cells(:,:)%rTMax)/SIZE(pGrd%Cells(:,:)%rTMax), &
                         SUM(pGrd%Cells(:,:)%rCFGI)/SIZE(pGrd%Cells(:,:)%rCFGI), &
						 MINVAL(pGrd%Cells(:,:)%rAdjCN), &
						 SUM(pGrd%Cells(:,:)%rAdjCN) &
						     / SIZE(pGrd%Cells(:,:)%rAdjCN), &
 						 MAXVAL(pGrd%Cells(:,:)%rAdjCN), &
                         rDaily(iSUM,iGROSS_PRECIP)*dpVolConvert, &
                         rDaily(iSUM,iINTERCEPTION)*dpVolConvert, &
                         rDaily(iSUM,iNET_PRECIP) *dpVolConvert, &
                         rDaily(iSUM,iSNOWFALL_SWE)*dpVolConvert, &
                         rDaily(iSUM,iSNOWCOVER)*dpVolConvert, &
                         rDaily(iSUM,iCHG_IN_SNOW_COV)*dpVolConvert, &
                         rDaily(iSUM,iSNOWMELT)*dpVolConvert, &
#ifdef IRRIGATION_MODULE
                         rDaily(iSUM, iIRRIGATION)*dpVolConvert, &
#endif
                         rDaily(iSUM,iSOIL_MOISTURE)*dpVolConvert, &
                         rDaily(iSUM,iCHG_IN_SOIL_MOIST)*dpVolConvert, &
                         rDaily(iSUM,iRUNOFF_OUTSIDE)*dpVolConvert, &
                         rDaily(iSUM,iREJECTED_RECHARGE)*dpVolConvert, &
                         rDaily(iSUM,iACT_ET)*dpVolConvert, &
                         rDaily(iSUM,iRECHARGE)*dpVolConvert, &
#ifdef STREAM_INTERACTIONS
                         rDaily(iSUM,iSTREAM_CAPTURE)*dpVolConvert, &
#endif
                         rDailyMSB*dpVolConvert


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

#ifdef DEBUG_PRINT
  write(LU_LOG, fmt="(a,i6)") trim(__FILE__)//", line number:", __LINE__
#endif

  if(len_trim(pConfig%sOutputFilePrefix)==0) then

    sDelimiter = ""

  else

    sDelimiter = "_"

  end if


  ! we need a grid data structure in order that we might call
  ! the grid-writing subroutines elsewhere in code
  pGrd => grid_Create(iNX, iNY, rX0, rY0, rX1, rY1, T_SGL_GRID)

#ifdef DEBUG_PRINT
  write(LU_LOG, fmt="(a,i6)") trim(__FILE__)//", line number:", __LINE__
#endif


  call stats_OpenBinaryFilesReadOnly(pConfig, pGrd)

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

      rewind(STAT_INFO(k)%iLU,iostat=iStat)
      call assert(iStat == 0, "Could not rewind binary file containing " &
        //trim(STAT_INFO(k)%sVARIABLE_NAME)//" data" ,&
        trim(__FILE__), __LINE__)
      write(STAT_INFO(k)%iLU,POS=iENDHEADER_POS,iostat=iStat)
      call assert(iStat == 0, &
        "Could not advance to first data value in binary file containing " &
        //trim(STAT_INFO(k)%sVARIABLE_NAME)//" data" ,&
        trim(__FILE__), __LINE__)


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

#ifdef DEBUG_PRINT
        write(UNIT=LU_LOG,FMT=*) 'exiting loop in stats.f95 '
        write(UNIT=LU_LOG,FMT=*) 'iStat = ',iStat
#endif
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

#ifdef DEBUG_PRINT
  write(LU_LOG, fmt="(a,i6)") trim(__FILE__)//", line number:", __LINE__
#endif

       ! name "RLE_readByte" is misleading, since the return value (rVal)
       ! is actually a vector of all daily values with dimension (iNY*iNX)
       call RLE_readByte(STAT_INFO(k)%iLU,pConfig%iRLE_MULT, &
          pConfig%rRLE_OFFSET, rVal,iNumGridCells,lEOF)
       if(lEOF) exit

       pGrd%rData(:,:)=RESHAPE(rVal,(/iNY,iNX/),PAD=rPad)

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

    rewind(STAT_INFO(k)%iLU,iostat=iStat)
    call assert(iStat == 0, "Could not rewind binary file containing " &
      //trim(STAT_INFO(k)%sVARIABLE_NAME)//" data" ,&
      trim(__FILE__), __LINE__)
    write(STAT_INFO(k)%iLU,POS=iENDHEADER_POS,iostat=iStat)
    call assert(iStat == 0, &
      "Could not advance to first data value in binary file containing " &
      //trim(STAT_INFO(k)%sVARIABLE_NAME)//" data" ,&
      trim(__FILE__), __LINE__)

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

       pGrd%rData(:,:)=RESHAPE(rValSum,(/iNX,iNY/),PAD=rPad)

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

    rewind(STAT_INFO(k)%iLU,iostat=iStat)
    call assert(iStat == 0, "Could not rewind binary file containing " &
      //trim(STAT_INFO(k)%sVARIABLE_NAME)//" data" ,&
      trim(__FILE__), __LINE__)
    write(STAT_INFO(k)%iLU,POS=iENDHEADER_POS,iostat=iStat)
    call assert(iStat == 0, &
      "Could not advance to first data value in binary file containing " &
      //trim(STAT_INFO(k)%sVARIABLE_NAME)//" data" ,&
      trim(__FILE__), __LINE__)

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

       pGrd%rData(:,:)=RESHAPE(rValSum,(/iNX,iNY/),PAD=rPad)

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

  type (T_GENERAL_GRID),pointer :: input_grd    ! Pointer to temporary grid for I/O

  call assert(associated(pConfig%BMASK), "Cannot calculate basin statistics; " &
    //"must supply a basin mask file to use this feature", &
    trim(__FILE__),__LINE__)

  pTmpGrd => grid_Create(pGrd%iNX, pGrd%iNY, pGrd%rX0, pGrd%rY0, &
      pGrd%rX1, pGrd%rY1, T_SGL_GRID)

  open(LU_MASK_STATS_CSV,FILE="SWB_BASIN_STATS.txt",iostat=iStat,STATUS='REPLACE')
  call Assert ( iStat == 0, &
    "Could not open basin mask statistics file")

  open(LU_PEST_STATS,FILE="SWB_PEST_STATS.txt",iostat=iStat,STATUS='REPLACE')
  call Assert ( iStat == 0, &
    "Could not open PEST statistics file")

  open(LU_PEST_OBS,FILE="SWB_PEST_OBSERVATIONS.txt",iostat=iStat,STATUS='REPLACE')
  call Assert ( iStat == 0, &
    "Could not open PEST observations file")

  open(LU_PEST_INS,FILE="SWB_PEST_INSTRUCTIONS.ins",iostat=iStat,STATUS='REPLACE')
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
    //sTAB//"Basin_Description"//sTAB//"Drainage_area"//sTAB &
    //"Majority Land Use"//sTAB//"Majority Soil Type"//sTAB &
    //"SWB_Mean_Recharge"//sTAB//"SWB_Min_Recharge"//sTAB &
    //"SWB_Max_Recharge"//sTAB//"Baseflow_Recharge"

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
       TRIM(pConfig%BMASK(k)%sUSGS_UpstreamOrderID),sTAB, &
       TRIM(pConfig%BMASK(k)%sBasinDescription),sTAB, &
       pConfig%BMASK(k)%rDrainageArea,sTAB, &
       TRIM(pConfig%LU(iMajorityLU)%sLanduseDescription), sTAB,iMajoritySoil, sTAB,&
       rAvg,sTAB,rMin,sTAB,rMax,sTAB,pConfig%BMASK(k)%rQb, sTAB

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

    open(pSSF%iLU, file=TRIM(pSSF%sFileName),status='OLD', &
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

subroutine stats_SetBinaryFilePosition(pConfig, pGrd)

  ![ARGUMENTS]
  type (T_MODEL_CONFIGURATION), pointer :: pConfig
  type (T_GENERAL_GRID),pointer :: pGrd                ! pointer to model grid

  ! [ LOCALS ]
  integer (kind=T_INT) :: k
  integer (kind=T_INT) :: iPos

  if(pConfig%lFirstDayOfSimulation) then
    ! scan through list of potential output variables; if any
    ! output is desired for a variable, note the current position
    ! within the file, move to the position reserved for the first day's
    ! date, write the date, and return to the position where the data
    ! for the first day will be written
    do k=1,iNUM_VARIABLES
      if(STAT_INFO(k)%iDailyOutput > iNONE &
        .or. STAT_INFO(k)%iMonthlyOutput > iNONE &
        .or. STAT_INFO(k)%iAnnualOutput > iNONE)  then
        inquire(UNIT=STAT_INFO(k)%iLU,POS=iPos)  ! establish location to return to
        write(UNIT=STAT_INFO(k)%iLU,POS=iSTARTDATE_POS) &
          pConfig%iMonth,pConfig%iDay, pConfig%iYear
        write(UNIT=STAT_INFO(k)%iLU,POS=iPos ) ! return to prior location in bin file
      end if
      pConfig%lFirstDayOfSimulation = lFALSE
    end do

  endif

end subroutine stats_SetBinaryFilePosition

!------------------------------------------------------------------------------

subroutine stats_TimestampBinaryFile(pConfig)

  ![ARGUMENTS]
  type (T_MODEL_CONFIGURATION), pointer :: pConfig

  ! [ LOCALS ]
  integer (kind=T_INT) :: k

  ! write timestamp to the unformatted fortran file(s)
  do k=1,iNUM_VARIABLES
    if(STAT_INFO(k)%iDailyOutput > iNONE &
      .or. STAT_INFO(k)%iMonthlyOutput > iNONE &
      .or. STAT_INFO(k)%iAnnualOutput > iNONE)  then
    write(UNIT=STAT_INFO(k)%iLU) pConfig%iDay,pConfig%iMonth, &
      pConfig%iYear, pConfig%iDayOfYear
    end if
  end do

end subroutine stats_TimestampBinaryFile

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

      open(nextunit(STAT_INFO(i)%iLU), FILE='output'//pConfig%sSlash// &
        TRIM(pConfig%sOutputFilePrefix) //"_" &
        //TRIM(STAT_INFO(i)%sVARIABLE_NAME) // '.bin',FORM='UNFORMATTED', &
        status='REPLACE',ACCESS='STREAM')

      write(UNIT=STAT_INFO(i)%iLU) pGrd%iNX             ! Number of cells in the x-direction
      write(UNIT=STAT_INFO(i)%iLU) pGrd%iNY             ! Number of cells in the y-direction
      write(UNIT=STAT_INFO(i)%iLU) T_SGL_GRID           ! Type of the grid
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
      write(unit=LU_LOG,fmt=*) "end-of-header offset: "//TRIM(int2char(iENDHEADER_POS))

      write(UNIT=LU_LOG,fmt="('NX:',i5)") pGrd%iNX             ! Number of cells in the x-direction
      write(UNIT=LU_LOG,fmt="('NY:',i5)") pGrd%iNY             ! Number of cells in the y-direction
      write(UNIT=LU_LOG,fmt="('data type:',i5)") pGrd%iDataType       ! Type of the grid
      write(UNIT=LU_LOG,fmt="('cell size:',f12.3)") pGrd%rGridCellSize   ! size of one side of a grid cell
      write(UNIT=LU_LOG,fmt="('length units:',i5)") pGrd%iLengthUnits    ! length units code
      write(UNIT=LU_LOG,fmt="('SI var:',i5)") i                    ! STAT_INFO variable number
      write(UNIT=LU_LOG,fmt="('RLE_MULT:',i5)") pConfig%iRLE_MULT    ! RLE Multiplier
      write(UNIT=LU_LOG,fmt="('RL__OFFSET:',f12.3)") pConfig%rRLE_OFFSET  ! RLE Offset
      write(UNIT=LU_LOG,fmt="('X0, X1:',f12.3,3x,f12.3)") pGrd%rX0, pGrd%rX1   ! World-coordinate range in X
      write(UNIT=LU_LOG,fmt="('Y0, Y1:',f12.3,3x,f12.3)") pGrd%rY0, pGrd%rY1   ! World-coordinate range in Y

	end if

  end do

end subroutine stats_OpenBinaryFiles

!------------------------------------------------------------------------------

subroutine stats_OpenBinaryFilesReadOnly(pConfig, pGrd)

  ![ARGUMENTS]
  type (T_MODEL_CONFIGURATION), pointer :: pConfig
  type (T_GENERAL_GRID),pointer :: pGrd                ! pointer to model grid

  ! [ LOCALS ]
  integer(kind=T_INT) :: i

  ! variables that are read in from the binary file header
  integer (kind=T_INT) :: iNX
  integer (kind=T_INT) :: iNY
  integer (kind=T_INT) :: iDataType
  real (kind=T_SGL)    :: rGridCellSize
  integer (kind=T_INT) :: iLengthUnits
  integer (kind=T_INT) :: iVariableNumber
  integer (kind=T_INT) :: iRLE_MULT
  real (kind=T_SGL)    :: rRLE_OFFSET
  real (kind=T_DBL)    :: rX0, rX1
  real (kind=T_DBL)    :: rY0, rY1
  integer (kind=T_INT) :: iStartMM, iStartDD, iStartYYYY
  integer (kind=T_INT) :: iEndMM, iEndDD, iEndYYYY

  do i=1,iNUM_VARIABLES

    if(STAT_INFO(i)%iDailyOutput > iNONE &
        .or. STAT_INFO(i)%iMonthlyOutput > iNONE &
        .or. STAT_INFO(i)%iAnnualOutput > iNONE)  then

      open(nextunit(STAT_INFO(i)%iLU), FILE='output'//pConfig%sSlash// &
        TRIM(pConfig%sOutputFilePrefix) //"_" &
        //TRIM(STAT_INFO(i)%sVARIABLE_NAME) // '.bin',FORM='UNFORMATTED', &
        status='OLD',ACCESS='STREAM', ACTION='READWRITE')

      write(unit=LU_LOG,fmt=*) "Opened binary file for " &
        //TRIM(STAT_INFO(i)%sVARIABLE_NAME)//" on unit " &
        //TRIM(int2char(STAT_INFO(i)%iLU))

      write(unit=LU_LOG,fmt=*) "start date offset: "//TRIM(int2char(iSTARTDATE_POS))
      write(unit=LU_LOG,fmt=*) "end date offset: "//TRIM(int2char(iENDDATE_POS))

      read(UNIT=STAT_INFO(i)%iLU) iNX             ! Number of cells in the x-direction
      read(UNIT=STAT_INFO(i)%iLU) iNY             ! Number of cells in the y-direction
      read(UNIT=STAT_INFO(i)%iLU) iDataType       ! Type of the grid
      read(UNIT=STAT_INFO(i)%iLU) rGridCellSize   ! size of one side of a grid cell
      read(UNIT=STAT_INFO(i)%iLU) iLengthUnits    ! length units code
      read(UNIT=STAT_INFO(i)%iLU) iVariableNumber ! STAT_INFO variable number
      read(UNIT=STAT_INFO(i)%iLU) iRLE_MULT       ! RLE Multiplier
      read(UNIT=STAT_INFO(i)%iLU) rRLE_OFFSET     ! RLE Offset
      read(UNIT=STAT_INFO(i)%iLU) rX0, rX1        ! World-coordinate range in X
      read(UNIT=STAT_INFO(i)%iLU) rY0, rY1        ! World-coordinate range in Y
      read(UNIT=STAT_INFO(i)%iLU) iStartMM, iStartDD, iStartYYYY
      read(UNIT=STAT_INFO(i)%iLU) iEndMM, iEndDD, iEndYYYY
      inquire(UNIT=STAT_INFO(i)%iLU,POS=iENDHEADER_POS)  ! establish location to return to

      ! at this point, the file is ready for reading of Day 1 values

	end if

  end do

end subroutine stats_OpenBinaryFilesReadOnly

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
