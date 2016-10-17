!> @file
!> The output module contains a collection of routines that
!> write output to disk.
!> The output module centralizes routines that write SSF files,
 !> run-length-encoded (RLE) SWB binary files, and statistics files.
module output

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use types
  use swb_grid
  use stats
  use RLE

  implicit none

contains

!----------------------------------------------------------------------

! **************************************************************************************************
! ########  #######     ########  #### ##    ##    ###    ########  ##    ##
!    ##    ##     ##    ##     ##  ##  ###   ##   ## ##   ##     ##  ##  ##
!    ##    ##     ##    ##     ##  ##  ####  ##  ##   ##  ##     ##   ####
!    ##    ##     ##    ########   ##  ## ## ## ##     ## ########     ##
!    ##    ##     ##    ##     ##  ##  ##  #### ######### ##   ##      ##
!    ##    ##     ##    ##     ##  ##  ##   ### ##     ## ##    ##     ##
!    ##     #######     ########  #### ##    ## ##     ## ##     ##    ##
! **************************************************************************************************

subroutine output_to_SWB_binary(pGrd, pConfig, cel, iRow, iCol, iTime, &
  rDailyRejectedRecharge,rNetInflow,rNetInfil,rSM_ActualET, &
  rPrecipMinusPotentET,rMoistureDeficit,rMoistureSurplus, &
  rChangeInStorage )


  type (T_GENERAL_GRID),pointer :: pGrd          ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings

  type ( T_CELL ), pointer :: cel
  integer (kind=c_int), intent(in) :: iRow
  integer (kind=c_int), intent(in) :: iCol
  integer (kind=c_int), intent(in) :: iTime
  real (kind=c_float), intent(in) :: rDailyRejectedRecharge
  real (kind=c_float), intent(in) :: rNetInflow
  real (kind=c_float), intent(in) :: rNetInfil
  real (kind=c_float), intent(in) :: rSM_ActualET
  real (kind=c_float), intent(in) :: rPrecipMinusPotentET
  real (kind=c_float), intent(in) :: rMoistureDeficit
  real (kind=c_float), intent(in) :: rMoistureSurplus
  real (kind=c_float), intent(in) :: rChangeInStorage

  ![ LOCALS ]
  integer (kind=c_int) :: iIndex

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
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iSNOWMELT)

        case(iINTERCEPTION)
          call RLE_writeByte(STAT_INFO(iINTERCEPTION)%iLU, &
            cel%rInterception, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iINTERCEPTION)

        case(iNET_RAINFALL)
          call RLE_writeByte(STAT_INFO(iNET_RAINFALL)%iLU, &
            cel%rNetRainfall, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iNET_RAINFALL)

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

        case(iREFERENCE_ET_ADJ)
          call RLE_writeByte(STAT_INFO(iREFERENCE_ET_ADJ)%iLU, &
            cel%rReferenceET0_adj, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iREFERENCE_ET_ADJ)

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

        case(iACT_ET_SOIL)
          call RLE_writeByte(STAT_INFO(iACT_ET_SOIL)%iLU, &
            cel%rActual_ET_soil, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iACT_ET_SOIL)

        case(iACT_ET_INTERCEPTION)
          call RLE_writeByte(STAT_INFO(iACT_ET_INTERCEPTION)%iLU, &
            cel%rActual_ET_interception, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iACT_ET_INTERCEPTION)

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
            cel%rDailyRecharge, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iRECHARGE)

        case(iGDD)
          call RLE_writeByte(STAT_INFO(iGDD)%iLU, &
            cel%rGDD, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iGDD)

        case(iROOTING_DEPTH)
          call RLE_writeByte(STAT_INFO(iROOTING_DEPTH)%iLU, &
            cel%rCurrentRootingDepth, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iROOTING_DEPTH)

        case(iCROP_COEFFICIENT)
          call RLE_writeByte(STAT_INFO(iCROP_COEFFICIENT)%iLU, &
            cel%rKcb, pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iCROP_COEFFICIENT)

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

        case(iGROWING_SEASON)
          call RLE_writeByte(STAT_INFO(iGROWING_SEASON)%iLU, &
            REAL(cel%iGrowingSeason,kind=c_float), pConfig%iRLE_MULT, &
            pConfig%rRLE_OFFSET, pGrd%iNumGridCells, iGROWING_SEASON)

        case default
          call Assert(lFALSE, "Internal programming error in " &
            //"select case structure",TRIM(__FILE__),__LINE__)

      end select
    end if
  end do   ! end loop over STAT_INFO variables

end subroutine output_to_SWB_binary


! **************************************************************************************************
! ########  #######      ######   ######  ########
!    ##    ##     ##    ##    ## ##    ## ##
!    ##    ##     ##    ##       ##       ##
!    ##    ##     ##     ######   ######  ######
!    ##    ##     ##          ##       ## ##
!    ##    ##     ##    ##    ## ##    ## ##
!    ##     #######      ######   ######  ##
! **************************************************************************************************

subroutine output_to_SSF(pGrd, pConfig, cel, iRow, iCol, &
  iMonth, iDay, iYear, &
  rDailyRejectedRecharge,rNetInflow,rNetInfil,rSM_ActualET, &
  rPrecipMinusPotentET,rMoistureDeficit,rMoistureSurplus, &
  rChangeInStorage )

  type (T_GENERAL_GRID),pointer :: pGrd          ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings

  type ( T_CELL ), pointer :: cel
  integer (kind=c_int), intent(in) :: iRow
  integer (kind=c_int), intent(in) :: iCol
  integer (kind=c_int), intent(in) :: iMonth
  integer (kind=c_int), intent(in) :: iDay
  integer (kind=c_int), intent(in) :: iYear
  real (kind=c_float), intent(in) :: rDailyRejectedRecharge
  real (kind=c_float), intent(in) :: rNetInflow
  real (kind=c_float), intent(in) :: rNetInfil
  real (kind=c_float), intent(in) :: rSM_ActualET
  real (kind=c_float), intent(in) :: rPrecipMinusPotentET
  real (kind=c_float), intent(in) :: rMoistureDeficit
  real (kind=c_float), intent(in) :: rMoistureSurplus
  real (kind=c_float), intent(in) :: rChangeInStorage

  !@todo
  ! figure out why *.SSF files are not written to if they reference the
  ! first column (colnum=1)

  ! [ LOCALS ]
  integer (kind=c_int) :: iIndex
  integer (kind=c_int) :: iVariableNumber

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
          case(iNET_RAINFALL)
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

          case(iREFERENCE_ET_ADJ)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rReferenceET0_adj)

          case(iCROP_ET)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rCropETc)

          case(iBARE_SOIL_EVAP)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rBareSoilEvap)
          case(iACT_ET)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, rSM_ActualET)

          case(iACT_ET_SOIL)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rActual_ET_soil)

          case(iACT_ET_INTERCEPTION)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rActual_ET_interception)

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
              iYear, cel%rDailyRecharge)
          case(iGDD)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rGDD)

          case(iROOTING_DEPTH)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rCurrentRootingDepth)

          case(iCROP_COEFFICIENT)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rKcb)

          case(iIRRIGATION)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rIrrigationAmount)

          case(iIRRIGATION_FROM_GW)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rIrrigationFromGW)

          case(iIRRIGATION_FROM_SW)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, cel%rIrrigationFromSW)

          case(iGROWING_SEASON)
            call stats_write_to_SSF_file(pConfig, iIndex, iMonth, iDay, &
              iYear, real( cel%iGrowingSeason, kind=c_float) )

          case default
            call Assert(lFALSE, "Internal programming error in " &
              //"select case structure",TRIM(__FILE__),__LINE__)

        end select
      end if
    end do
  end if

end subroutine output_to_SSF

!----------------------------------------------------------------------


! **********************************************************************************************************
! ##     ## ########  ########     ###    ######## ########     ######  ########    ###    ########  ######
! ##     ## ##     ## ##     ##   ## ##      ##    ##          ##    ##    ##      ## ##      ##    ##    ##
! ##     ## ##     ## ##     ##  ##   ##     ##    ##          ##          ##     ##   ##     ##    ##
! ##     ## ########  ##     ## ##     ##    ##    ######       ######     ##    ##     ##    ##     ######
! ##     ## ##        ##     ## #########    ##    ##                ##    ##    #########    ##          ##
! ##     ## ##        ##     ## ##     ##    ##    ##          ##    ##    ##    ##     ##    ##    ##    ##
!  #######  ##        ########  ##     ##    ##    ########     ######     ##    ##     ##    ##     ######
! **********************************************************************************************************

subroutine output_update_accumulators(cel, iMonth, &
  rDailyRejectedRecharge,rNetInflow,rNetInfil,rSM_ActualET, &
  rPrecipMinusPotentET,rMoistureDeficit,rMoistureSurplus, &
  rChangeInStorage )

  type (T_GENERAL_GRID),pointer :: pGrd          ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings

  type ( T_CELL ), pointer, intent(inout) :: cel
  integer (kind=c_int), intent(in)        :: iMonth
  real (kind=c_float), intent(in)         :: rDailyRejectedRecharge
  real (kind=c_float), intent(in)         :: rNetInflow
  real (kind=c_float), intent(in)         :: rNetInfil
  real (kind=c_float), intent(in)         :: rSM_ActualET
  real (kind=c_float), intent(in)         :: rPrecipMinusPotentET
  real (kind=c_float), intent(in)         :: rMoistureDeficit
  real (kind=c_float), intent(in)         :: rMoistureSurplus
  real (kind=c_float), intent(in)         :: rChangeInStorage

  ! the following code block sends the appropriate cell value to the
  ! associated accumulator variables rDaily, rMonthly, and rAnnual,
  ! which are defined in the stats.f95 module
  call stats_UpdateAllAccumulatorsByCell(REAL(rDailyRejectedRecharge,kind=c_double), &
    iREJECTED_RECHARGE,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rCFGI,kind=c_double), &
    iCFGI,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rTMin,kind=c_double), &
    iMIN_TEMP,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rTMax,kind=c_double), &
    iMAX_TEMP,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rTAvg,kind=c_double), &
    iAVG_TEMP,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rDailyRecharge,kind=c_double), &
    iRECHARGE,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rSoilMoisture,kind=c_double), &
    iSOIL_MOISTURE,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rSM_AccumPotentWatLoss,kind=c_double), &
    iSM_APWL,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(rNetInfil,kind=c_double),&
     iNET_INFIL,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rInterception, kind=c_double), &
     iINTERCEPTION,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(rMoistureDeficit,kind=c_double), &
     iSM_DEFICIT,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(rMoistureSurplus,kind=c_double), &
     iSM_SURPLUS,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(rChangeInStorage,kind=c_double), &
     iCHG_IN_SOIL_MOIST,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(rNetInflow,kind=c_double),&
       iNET_INFLOW,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rFlowOutOfGrid,kind=c_double), &
      iRUNOFF_OUTSIDE,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(rPrecipMinusPotentET,kind=c_double), &
      iP_MINUS_PET,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rReferenceET0,kind=c_double), &
       iREFERENCE_ET,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rReferenceET0_adj,kind=c_double), &
       iREFERENCE_ET_ADJ,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rCropETc,kind=c_double), &
       iCROP_ET,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rBareSoilEvap,kind=c_double), &
       iBARE_SOIL_EVAP,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(rSM_ActualET,kind=c_double), &
       iACT_ET,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rActual_ET_soil,kind=c_double), &
       iACT_ET_SOIL,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rActual_ET_interception,kind=c_double), &
       iACT_ET_INTERCEPTION,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rInFlow,kind=c_double), &
       iINFLOW,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rOutFlow,kind=c_double), &
       iOUTFLOW,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rGDD,kind=c_double), &
       iGDD,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rCurrentRootingDepth,kind=c_double), &
       iROOTING_DEPTH,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rKcb,kind=c_double), &
       iCROP_COEFFICIENT,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rIrrigationAmount,kind=c_double), &
       iIRRIGATION,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rIrrigationFromGW,kind=c_double), &
       iIRRIGATION_FROM_GW,iMonth,iZERO)

  call stats_UpdateAllAccumulatorsByCell(REAL(cel%rIrrigationFromSW,kind=c_double), &
       iIRRIGATION_FROM_SW,iMonth,iZERO)

end subroutine output_update_accumulators


subroutine output_finalize_accumulators(cel, iMonth, iNumGridCells, &
  rDailyRejectedRecharge,rNetInflow,rNetInfil,rSM_ActualET, &
  rPrecipMinusPotentET,rMoistureDeficit,rMoistureSurplus, &
  rChangeInStorage )

  type ( T_CELL ), pointer :: cel
  integer (kind=c_int), intent(in) :: iMonth
  integer (kind=c_int), intent(in) :: iNumGridCells
  real (kind=c_float), intent(in) :: rDailyRejectedRecharge
  real (kind=c_float), intent(in) :: rNetInflow
  real (kind=c_float), intent(in) :: rNetInfil
  real (kind=c_float), intent(in) :: rSM_ActualET
  real (kind=c_float), intent(in) :: rPrecipMinusPotentET
  real (kind=c_float), intent(in) :: rMoistureDeficit
  real (kind=c_float), intent(in) :: rMoistureSurplus
  real (kind=c_float), intent(in) :: rChangeInStorage
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
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iREFERENCE_ET_ADJ,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iCROP_ET,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iBARE_SOIL_EVAP,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iACT_ET,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iACT_ET_SOIL,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iACT_ET_INTERCEPTION,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iSM_DEFICIT,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iSM_SURPLUS,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iCHG_IN_SOIL_MOIST,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iNET_INFIL,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iSM_APWL,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iNET_INFLOW,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iINFLOW,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iOUTFLOW,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iGDD, iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iROOTING_DEPTH, iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iCROP_COEFFICIENT, iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iIRRIGATION, iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iIRRIGATION_FROM_GW, iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iIRRIGATION_FROM_SW, iMonth,iNumGridCells)

end subroutine output_finalize_accumulators

end module output
