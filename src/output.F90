!> @file
!> @brief
!> @brief
module output

  use types
  use swb_grid
  use swb_stats
  use RLE

#ifdef NETCDF_SUPPORT
  use netcdf_support
#endif

  implicit none

contains

#ifdef NETCDF_SUPPORT

subroutine output_to_netcdf(pGrd, pConfig, cel, iRow, iCol, iTime, &
  rDailyRejectedRecharge,rNetInflow,rNetInfil,rSM_ActualET, &
  rPrecipMinusPotentET,rMoistureDeficit,rMoistureSurplus, &
  rChangeInStorage,rDailyRecharge)

  type (T_GENERAL_GRID),pointer :: pGrd          ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings

  type ( T_CELL ), pointer :: cel
  integer (kind=T_INT), intent(in) :: iRow
  integer (kind=T_INT), intent(in) :: iCol
  integer (kind=T_INT), intent(in) :: iTime
  real (kind=T_SGL), intent(in) :: rDailyRejectedRecharge
  real (kind=T_SGL), intent(in) :: rNetInflow
  real (kind=T_SGL), intent(in) :: rNetInfil
  real (kind=T_SGL), intent(in) :: rSM_ActualET
  real (kind=T_SGL), intent(in) :: rPrecipMinusPotentET
  real (kind=T_SGL), intent(in) :: rMoistureDeficit
  real (kind=T_SGL), intent(in) :: rMoistureSurplus
  real (kind=T_SGL), intent(in) :: rChangeInStorage
  real (kind=T_SGL), intent(in) :: rDailyRecharge

  ![ LOCALS ]
  integer (kind=T_INT) :: iIndex

  do iIndex=1,iNUM_VARIABLES

    if(.not. STAT_INFO(iIndex)%lActive) cycle

      if(STAT_INFO(iIndex)%iNetCDFOutput > iNONE ) then

        select case(iIndex)
          case(iGROSS_PRECIP)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              cel%rGrossPrecip, pGrd, iRow, iCol, iTime)
          case(iSNOWFALL_SWE)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              cel%rSnowFall_SWE, pGrd, iRow, iCol, iTime)
          case(iSNOWCOVER)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              cel%rSnowCover, pGrd, iRow, iCol, iTime)
          case(iCFGI)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              cel%rCFGI, pGrd, iRow, iCol, iTime)
          case(iMIN_TEMP)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              cel%rTMin, pGrd, iRow, iCol, iTime)
          case(iMAX_TEMP)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              cel%rTMax, pGrd, iRow, iCol, iTime)
          case(iAVG_TEMP)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              cel%rTAvg, pGrd, iRow, iCol, iTime)
          case(iCHG_IN_SNOW_COV)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              cel%rSnowFall_SWE - cel%rSnowmelt, pGrd, iRow, iCol, iTime)
          case(iSNOWMELT)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              cel%rSnowmelt, pGrd, iRow, iCol, iTime)
          case(iINTERCEPTION)
!
!           ==> STAT_INFO(iINTERCEPTION) is updated at the time
!               interception is calculated in subroutine model_ProcessRain
!
          case(iNET_PRECIP)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              cel%rNetRainfall, pGrd, iRow, iCol, iTime)
          case(iINFLOW)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              cel%rInflow, pGrd, iRow, iCol, iTime)
          case(iOUTFLOW)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              cel%rOutflow, pGrd, iRow, iCol, iTime)
          case(iRUNOFF_OUTSIDE)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              cel%rFlowOutofGrid, pGrd, iRow, iCol, iTime)
          case(iREJECTED_RECHARGE)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              rDailyRejectedRecharge, pGrd, iRow, iCol, iTime)
          case(iNET_INFLOW)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              rNetInflow, pGrd, iRow, iCol, iTime)
          case(iNET_INFIL)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              rNetInfil, pGrd, iRow, iCol, iTime)
          case(iREFERENCE_ET)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              cel%rReferenceET0, pGrd, iRow, iCol, iTime)
          case(iCROP_ET)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              cel%rCropETc, pGrd, iRow, iCol, iTime)
          case(iBARE_SOIL_EVAP)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              cel%rBareSoilEvap, pGrd, iRow, iCol, iTime)
          case(iACT_ET)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              rSM_ActualET, pGrd, iRow, iCol, iTime)
          case(iP_MINUS_PET)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              rPrecipMinusPotentET, pGrd, iRow, iCol, iTime)
          case(iSM_DEFICIT)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              rMoistureDeficit, pGrd, iRow, iCol, iTime)
          case(iSM_SURPLUS)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              rMoistureSurplus, pGrd, iRow, iCol, iTime)
          case(iSM_APWL)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              cel%rSM_AccumPotentWatLoss, pGrd, iRow, iCol, iTime)
          case(iSOIL_MOISTURE)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              cel%rSoilMoisture, pGrd, iRow, iCol, iTime)
          case(iCHG_IN_SOIL_MOIST)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              rChangeInStorage, pGrd, iRow, iCol, iTime)
          case(iRECHARGE)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              rDailyRecharge, pGrd, iRow, iCol, iTime)
          case(iGDD)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              cel%rGDD, pGrd, iRow, iCol, iTime)
          case(iIRRIGATION)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              cel%rIrrigationAmount, pGrd, iRow, iCol, iTime)

          case(iIRRIGATION_FROM_GW)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              cel%rIrrigationFromGW, pGrd, iRow, iCol, iTime)

          case(iIRRIGATION_FROM_SW)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              cel%rIrrigationFromSW, pGrd, iRow, iCol, iTime)

          case(iSTREAM_CAPTURE)
            call netcdf_write_variable_byte(iIndex, iNC_OUTPUT, pConfig, &
              cel%rStreamCapture, pGrd, iRow, iCol, iTime)
          case default
            call Assert(lFALSE, "Internal programming error in " &
                //"select case structure",TRIM(__FILE__),__LINE__)

        end select
      end if
    enddo

end subroutine output_to_netcdf

#endif
!----------------------------------------------------------------------

subroutine output_to_SWB_binary(pGrd, pConfig, cel, iRow, iCol, iTime, &
  rDailyRejectedRecharge,rNetInflow,rNetInfil,rSM_ActualET, &
  rPrecipMinusPotentET,rMoistureDeficit,rMoistureSurplus, &
  rChangeInStorage,rDailyRecharge)


  type (T_GENERAL_GRID),pointer :: pGrd          ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings

  type ( T_CELL ), pointer :: cel
  integer (kind=T_INT), intent(in) :: iRow
  integer (kind=T_INT), intent(in) :: iCol
  integer (kind=T_INT), intent(in) :: iTime
  real (kind=T_SGL), intent(in) :: rDailyRejectedRecharge
  real (kind=T_SGL), intent(in) :: rNetInflow
  real (kind=T_SGL), intent(in) :: rNetInfil
  real (kind=T_SGL), intent(in) :: rSM_ActualET
  real (kind=T_SGL), intent(in) :: rPrecipMinusPotentET
  real (kind=T_SGL), intent(in) :: rMoistureDeficit
  real (kind=T_SGL), intent(in) :: rMoistureSurplus
  real (kind=T_SGL), intent(in) :: rChangeInStorage
  real (kind=T_SGL), intent(in) :: rDailyRecharge

  ![ LOCALS ]
  integer (kind=T_INT) :: iIndex

  do iIndex=1,iNUM_VARIABLES

    if(.not. STAT_INFO(iIndex)%lActive) cycle

    if(STAT_INFO(iIndex)%iDailyOutput > iNONE &
      .or. STAT_INFO(iIndex)%iMonthlyOutput > iNONE &
      .or. STAT_INFO(iIndex)%iAnnualOutput > iNONE )  then

      select case(iIndex)

        case(iGROSS_PRECIP)
          call RLE_writeByte(STAT_INFO(iGROSS_PRECIP)%iLU, &
            cel%rGrossPrecip, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iGROSS_PRECIP)
        case(iSNOWFALL_SWE)
          call RLE_writeByte(STAT_INFO(iSNOWFALL_SWE)%iLU, &
            cel%rSnowFall_SWE, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iSNOWFALL_SWE)
        case(iSNOWCOVER)
          call RLE_writeByte(STAT_INFO(iSNOWCOVER)%iLU, &
              cel%rSnowCover, pConfig%iRLE_MULT, &
              pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iSNOWCOVER)
        case(iCFGI)
          call RLE_writeByte(STAT_INFO(iCFGI)%iLU, &
            cel%rCFGI, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iCFGI)
        case(iMIN_TEMP)
          call RLE_writeByte(STAT_INFO(iMIN_TEMP)%iLU, &
            cel%rTMin, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iMIN_TEMP)
        case(iMAX_TEMP)
          call RLE_writeByte(STAT_INFO(iMAX_TEMP)%iLU, &
            cel%rTMax, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iMAX_TEMP)
        case(iAVG_TEMP)
          call RLE_writeByte(STAT_INFO(iAVG_TEMP)%iLU, &
            cel%rTAvg, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iAVG_TEMP)
        case(iCHG_IN_SNOW_COV)
          call RLE_writeByte(STAT_INFO(iCHG_IN_SNOW_COV)%iLU, &
            cel%rSnowFall_SWE-cel%rSnowmelt, &
            pConfig%iRLE_MULT, pConfig%rRLE_OFFSET, &
            pGrd%iNumGridCells, iCHG_IN_SNOW_COV)
        case(iSNOWMELT)
          call RLE_writeByte(STAT_INFO(iSNOWMELT)%iLU, &
            cel%rSnowmelt, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iSnowmelt)
        case(iINTERCEPTION)
          call RLE_writeByte(STAT_INFO(iINTERCEPTION)%iLU, &
            cel%rInterception, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iINTERCEPTION)
        case(iNET_PRECIP)
          call RLE_writeByte(STAT_INFO(iNET_PRECIP)%iLU, &
            cel%rNetRainfall, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iNET_PRECIP)
        case(iINFLOW)
          call RLE_writeByte(STAT_INFO(iINFLOW)%iLU, &
            cel%rInflow, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iINFLOW)
        case(iOUTFLOW)
          call RLE_writeByte(STAT_INFO(iOUTFLOW)%iLU, &
            cel%rOutflow, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iOUTFLOW)
        case(iRUNOFF_OUTSIDE)
          call RLE_writeByte(STAT_INFO(iRUNOFF_OUTSIDE)%iLU, &
            cel%rFlowOutOfGrid, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iRUNOFF_OUTSIDE)
        case(iREJECTED_RECHARGE)
          call RLE_writeByte(STAT_INFO(iREJECTED_RECHARGE)%iLU, &
            rDailyRejectedRecharge, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iREJECTED_RECHARGE)
        case(iNET_INFLOW)
          call RLE_writeByte(STAT_INFO(iNET_INFLOW)%iLU, &
            rNetInflow, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iNET_INFLOW)
        case(iNET_INFIL)
          call RLE_writeByte(STAT_INFO(iNET_INFIL)%iLU, &
            rNetInfil, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iNET_INFIL)
        case(iREFERENCE_ET)
          call RLE_writeByte(STAT_INFO(iREFERENCE_ET)%iLU, &
            cel%rReferenceET0, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iREFERENCE_ET)
        case(iCROP_ET)
          call RLE_writeByte(STAT_INFO(iCROP_ET)%iLU, &
            cel%rCropETc, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iCROP_ET)
        case(iBARE_SOIL_EVAP)
          call RLE_writeByte(STAT_INFO(iBARE_SOIL_EVAP)%iLU, &
            cel%rBareSoilEvap, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iBARE_SOIL_EVAP)
        case(iACT_ET)
          call RLE_writeByte(STAT_INFO(iACT_ET)%iLU, &
            rSM_ActualET, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iACT_ET)
        case(iP_MINUS_PET)
          call RLE_writeByte(STAT_INFO(iP_MINUS_PET)%iLU, &
            rPrecipMinusPotentET, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iP_MINUS_PET)
        case(iSM_DEFICIT)
          call RLE_writeByte(STAT_INFO(iSM_DEFICIT)%iLU, &
            rMoistureDeficit, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iSM_DEFICIT)
        case(iSM_SURPLUS)
          call RLE_writeByte(STAT_INFO(iSM_SURPLUS)%iLU, &
            rMoistureSurplus, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iSM_SURPLUS)
        case(iSM_APWL)
          call RLE_writeByte(STAT_INFO(iSM_APWL)%iLU, &
            cel%rSM_AccumPotentWatLoss, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iSM_APWL)
        case(iSOIL_MOISTURE)
          call RLE_writeByte(STAT_INFO(iSOIL_MOISTURE)%iLU, &
            cel%rSoilMoisture, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iSOIL_MOISTURE)
        case(iCHG_IN_SOIL_MOIST)
          call RLE_writeByte(STAT_INFO(iCHG_IN_SOIL_MOIST)%iLU, &
            rChangeInStorage, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iCHG_IN_SOIL_MOIST)
        case(iRECHARGE)
          call RLE_writeByte(STAT_INFO(iRECHARGE)%iLU, &
            rDailyRecharge, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iRECHARGE)
        case(iGDD)
          call RLE_writeByte(STAT_INFO(iGDD)%iLU, &
            cel%rGDD, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iGDD)
        case(iIRRIGATION)
          call RLE_writeByte(STAT_INFO(iIRRIGATION)%iLU, &
            cel%rIrrigationAmount, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iIRRIGATION)

      case(iIRRIGATION_FROM_GW)
        call RLE_writeByte(STAT_INFO(iIRRIGATION_FROM_GW)%iLU, &
          cel%rIrrigationFromGW, pConfig%iRLE_MULT, &
          pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iIRRIGATION_FROM_GW)

      case(iIRRIGATION_FROM_SW)
        call RLE_writeByte(STAT_INFO(iIRRIGATION_FROM_SW)%iLU, &
          cel%rIrrigationFromSW, pConfig%iRLE_MULT, &
          pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iIRRIGATION_FROM_SW)

      ! NOTE to Vic: the following code block needs to be present in order
      !              that the StreamCapture results will be available
      !              at the end of the run
      !
      ! Thought process behind this gobbledy-gook was that we could minimize the
      ! number of elements within the T_CELL structure by using a temp variable
      ! within this module, writing the values to disk, and then moving on to
      ! the next grid cell.
      !
      ! Unless rStreamCapture is operated on by other modules, it could be
      ! converted to a local module variable

        case(iSTREAM_CAPTURE)
          call RLE_writeByte(STAT_INFO(iSTREAM_CAPTURE)%iLU, &
            REAL(cel%rStreamCapture,kind=T_SGL), pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iSTREAM_CAPTURE)

        case default
          call Assert(lFALSE, "Internal programming error in " &
            //"select case structure",TRIM(__FILE__),__LINE__)

      end select
    end if
  end do   ! end loop over STAT_INFO variables

end subroutine output_to_SWB_binary

subroutine output_to_SSF(pGrd, pConfig, cel, iRow, iCol, &
  iMonth, iDay, iYear, &
  rDailyRejectedRecharge,rNetInflow,rNetInfil,rSM_ActualET, &
  rPrecipMinusPotentET,rMoistureDeficit,rMoistureSurplus, &
  rChangeInStorage,rDailyRecharge)

  type (T_GENERAL_GRID),pointer :: pGrd          ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings

  type ( T_CELL ), pointer :: cel
  integer (kind=T_INT), intent(in) :: iRow
  integer (kind=T_INT), intent(in) :: iCol
  integer (kind=T_INT), intent(in) :: iMonth
  integer (kind=T_INT), intent(in) :: iDay
  integer (kind=T_INT), intent(in) :: iYear
  real (kind=T_SGL), intent(in) :: rDailyRejectedRecharge
  real (kind=T_SGL), intent(in) :: rNetInflow
  real (kind=T_SGL), intent(in) :: rNetInfil
  real (kind=T_SGL), intent(in) :: rSM_ActualET
  real (kind=T_SGL), intent(in) :: rPrecipMinusPotentET
  real (kind=T_SGL), intent(in) :: rMoistureDeficit
  real (kind=T_SGL), intent(in) :: rMoistureSurplus
  real (kind=T_SGL), intent(in) :: rChangeInStorage
  real (kind=T_SGL), intent(in) :: rDailyRecharge

  ! [ LOCALS ]
  integer (kind=T_INT) :: iIndex
  integer (kind=T_INT) :: iVariableNumber

  ! write out SSF files, if requested
  if( cel%iNumFilesSSF > 0)  then

    do iIndex=1,size(pConfig%SSF_FILES)

      if(pConfig%SSF_FILES(iIndex)%iRowNum == iRow &
        .and. pConfig%SSF_FILES(iIndex)%iColNum == iCol) then

        iVariableNumber = pConfig%SSF_FILES(iIndex)%iVarNum

        select case(iVariableNumber)
          case(iGROSS_PRECIP)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rGrossPrecip)
          case(iSNOWFALL_SWE)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rSnowFall_SWE)
          case(iSNOWCOVER)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rSnowCover)
          case(iCFGI)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rCFGI)
          case(iMIN_TEMP)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rTMin)
          case(iMAX_TEMP)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rTMax)
          case(iAVG_TEMP)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rTAvg)
          case(iCHG_IN_SNOW_COV)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rSnowFall_SWE-cel%rSnowmelt)
          case(iSNOWMELT)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rSnowmelt)

          case(iINTERCEPTION)
!
!           ==> STAT_INFO(iINTERCEPTION) is updated at the time
!               interception is calculated in subroutine model_ProcessRain
!
          case(iNET_PRECIP)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rNetRainfall)
          case(iINFLOW)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rInflow)
          case(iOUTFLOW)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rOutflow)
          case(iRUNOFF_OUTSIDE)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rFlowOutOfGrid)
          case(iREJECTED_RECHARGE)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, rDailyRejectedRecharge)
          case(iNET_INFLOW)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, rNetInflow)
          case(iNET_INFIL)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, rNetInfil)
          case(iREFERENCE_ET)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rReferenceET0)
          case(iCROP_ET)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rCropETc)
          case(iBARE_SOIL_EVAP)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rBareSoilEvap)
          case(iACT_ET)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, rSM_ActualET)
          case(iP_MINUS_PET)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, rPrecipMinusPotentET)
          case(iSM_DEFICIT)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, rMoistureDeficit)
          case(iSM_SURPLUS)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, rMoistureSurplus)
          case(iSM_APWL)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rSM_AccumPotentWatLoss)
          case(iSOIL_MOISTURE)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rSoilMoisture)
          case(iCHG_IN_SOIL_MOIST)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, rChangeInStorage)
          case(iRECHARGE)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, rDailyRecharge)
          case(iGDD)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rGDD)
          case(iIRRIGATION)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rIrrigationAmount)

          case(iIRRIGATION_FROM_GW)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rIrrigationFromGW)

          case(iIRRIGATION_FROM_SW)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rIrrigationFromSW)

          case(iSTREAM_CAPTURE)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rStreamCapture)
          case default
            call Assert(lFALSE, "Internal programming error in " &
              //"select case structure",TRIM(__FILE__),__LINE__)

        end select
      end if
    end do
  end if

end subroutine output_to_SSF

!----------------------------------------------------------------------

subroutine output_update_accumulators(cel, iMonth, &
  rDailyRejectedRecharge,rNetInflow,rNetInfil,rSM_ActualET, &
  rPrecipMinusPotentET,rMoistureDeficit,rMoistureSurplus, &
  rChangeInStorage,rDailyRecharge)

  type (T_GENERAL_GRID),pointer :: pGrd          ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings

  type ( T_CELL ), pointer :: cel
  integer (kind=T_INT), intent(in) :: iMonth
  real (kind=T_SGL), intent(in) :: rDailyRejectedRecharge
  real (kind=T_SGL), intent(in) :: rNetInflow
  real (kind=T_SGL), intent(in) :: rNetInfil
  real (kind=T_SGL), intent(in) :: rSM_ActualET
  real (kind=T_SGL), intent(in) :: rPrecipMinusPotentET
  real (kind=T_SGL), intent(in) :: rMoistureDeficit
  real (kind=T_SGL), intent(in) :: rMoistureSurplus
  real (kind=T_SGL), intent(in) :: rChangeInStorage
  real (kind=T_SGL), intent(in) :: rDailyRecharge

  ! the following code block sends the appropriate cell value to the
  ! associated accumulator variables rDaily, rMonthly, and rAnnual,
  ! which are defined in the stats.f95 module
  call stats_UpdateAllAccumulatorsByCell(REAL(rDailyRejectedRecharge,kind=T_DBL), &
    iREJECTED_RECHARGE,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rCFGI,kind=T_DBL), &
    iCFGI,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rTMin,kind=T_DBL), &
    iMIN_TEMP,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rTMax,kind=T_DBL), &
    iMAX_TEMP,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rTAvg,kind=T_DBL), &
    iAVG_TEMP,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(rDailyRecharge,kind=T_DBL), &
    iRECHARGE,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rSoilMoisture,kind=T_DBL), &
    iSOIL_MOISTURE,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rSM_AccumPotentWatLoss,kind=T_DBL), &
    iSM_APWL,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(rNetInfil,kind=T_DBL),&
     iNET_INFIL,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rInterception, kind=T_DBL), &
     iINTERCEPTION,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(rMoistureDeficit,kind=T_DBL), &
     iSM_DEFICIT,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(rMoistureSurplus,kind=T_DBL), &
     iSM_SURPLUS,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(rChangeInStorage,kind=T_DBL), &
     iCHG_IN_SOIL_MOIST,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(rNetInflow,kind=T_DBL),&
       iNET_INFLOW,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rFlowOutOfGrid,kind=T_DBL), &
      iRUNOFF_OUTSIDE,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(rPrecipMinusPotentET,kind=T_DBL), &
      iP_MINUS_PET,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rReferenceET0,kind=T_DBL), &
       iREFERENCE_ET,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rCropETc,kind=T_DBL), &
       iCROP_ET,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rBareSoilEvap,kind=T_DBL), &
       iBARE_SOIL_EVAP,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(rSM_ActualET,kind=T_DBL), &
       iACT_ET,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rInFlow,kind=T_DBL), &
       iINFLOW,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rOutFlow,kind=T_DBL), &
       iOUTFLOW,iMonth,iZERO)

#ifdef STREAM_INTERACTIONS

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rStreamCapture,kind=T_DBL), &
       iSTREAM_CAPTURE,iMonth,iZERO)

#endif

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rGDD,kind=T_DBL), &
       iGDD,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rIrrigationAmount,kind=T_DBL), &
       iIRRIGATION,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rIrrigationFromGW,kind=T_DBL), &
       iIRRIGATION_FROM_GW,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rIrrigationFromSW,kind=T_DBL), &
       iIRRIGATION_FROM_SW,iMonth,iZERO)


end subroutine output_update_accumulators


subroutine output_finalize_accumulators(cel, iMonth, iNumGridCells, &
  rDailyRejectedRecharge,rNetInflow,rNetInfil,rSM_ActualET, &
  rPrecipMinusPotentET,rMoistureDeficit,rMoistureSurplus, &
  rChangeInStorage,rDailyRecharge)

  type ( T_CELL ), pointer :: cel
  integer (kind=T_INT), intent(in) :: iMonth
  integer (kind=T_INT), intent(in) :: iNumGridCells
  real (kind=T_SGL), intent(in) :: rDailyRejectedRecharge
  real (kind=T_SGL), intent(in) :: rNetInflow
  real (kind=T_SGL), intent(in) :: rNetInfil
  real (kind=T_SGL), intent(in) :: rSM_ActualET
  real (kind=T_SGL), intent(in) :: rPrecipMinusPotentET
  real (kind=T_SGL), intent(in) :: rMoistureDeficit
  real (kind=T_SGL), intent(in) :: rMoistureSurplus
  real (kind=T_SGL), intent(in) :: rChangeInStorage
  real (kind=T_SGL), intent(in) :: rDailyRecharge

  !
  ! NOTE: stats_UpdateAllAccumulatorsByCell may be called in one of two ways.
  !   The actual call syntax is:
  !   stats_UpdateAllAccumulatorsByCell(rValue,iVarNum,iMonthNum,iNumGridCells)
  !
  !   1) if called with iNumGridCells == 0, the subroutine adds the value of the
  !      current grid cell to the accumulator
  !
  !   2) if called with iNumGridCells > 0, the subroutine divides the accumulator
  !      by iNumGridCells to yield a mean value over all grid values.
  !
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iRECHARGE,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iCFGI,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iMIN_TEMP,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iMAX_TEMP,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iAVG_TEMP,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iRUNOFF_OUTSIDE,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iREJECTED_RECHARGE,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iSOIL_MOISTURE,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iINTERCEPTION,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iP_MINUS_PET,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iREFERENCE_ET,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iCROP_ET,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iBARE_SOIL_EVAP,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iACT_ET,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iSM_DEFICIT,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iSM_SURPLUS,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iCHG_IN_SOIL_MOIST,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iNET_INFIL,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iSM_APWL,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iNET_INFLOW,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iINFLOW,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iOUTFLOW,iMonth,iNumGridCells)

#ifdef STREAM_INTERACTIONS

  call stats_UpdateAllAccumulatorsByCell(dpZERO, iSTREAM_CAPTURE, &
      iMonth,iNumGridCells)

#endif

  call stats_UpdateAllAccumulatorsByCell(dpZERO, iGDD, &
      iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iIRRIGATION, &
      iMonth,iNumGridCells)

  call stats_UpdateAllAccumulatorsByCell(dpZERO, iIRRIGATION_FROM_GW, &
      iMonth,iNumGridCells)

  call stats_UpdateAllAccumulatorsByCell(dpZERO, iIRRIGATION_FROM_SW, &
      iMonth,iNumGridCells)


end subroutine output_finalize_accumulators


end module output
