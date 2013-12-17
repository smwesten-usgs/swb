!> @file
!> Contains a single module, @ref water_balance, which estimates runoff by
!> means of the NRCS/SCS curve number method.

!> Performs the actual soil-moisture balance once precip, snowmelt, runoff, and ET have
!> been calculated. Soil moisture may be calculated either by
!> 1) Calulated ET and Thornthwaite and Mathers' (1957) tables;
!> 2) Crop coefficients for all vegetation classes and soil water balance
!>    calculated as per FAO56.
module water_balance

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use types
!
  use sm_thornthwaite_mather
  use swb_grid
  use stats
  use output
  use RLE

  implicit none

contains


! Handles the soil moisture calculations using the model grid 'pGrd' and
! the water-loss table in gWLT
!
! The water loss values are looked up in the gWLT table by interpolation
! and by searching the columns in the grid according to the water capacity
! in each cell.
!
! During the calculations, the model grid receives updates to the
! monthly recharge and annual recharge values.
subroutine calculate_water_balance ( pGrd, pConfig, &
      iDayOfYear, iDay, iMonth, iYear)
  ! [ ARGUMENTS ]
  type (T_GENERAL_GRID),pointer :: pGrd          ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings

  integer (kind=c_int), intent(in) :: iDayOfYear
  integer (kind=c_int), intent(in) :: iDay
  integer (kind=c_int), intent(in) :: iMonth
  integer (kind=c_int), intent(in) :: iYear
  ! [ LOCALS ]
  integer (kind=c_int) :: iCol,iRow,k,l,iTgt_Col,iTgt_Row
  integer (kind=c_int) :: iNumGridCells
  integer (kind=c_int) :: iTime
  real (kind=c_float) :: rPrevious_Soil_Moisture
  real (kind=c_float) :: rPrecipMinusPotentET
  real (kind=c_float) :: rMSB_DailyMassBalance
  real (kind=c_float) :: rMoistureDeficit
  real (kind=c_float) :: rMoistureSurplus
  real (kind=c_float) :: rChangeInStorage
  real (kind=c_float) :: rNetInfil
  real (kind=c_float) :: rNetInflow
  real (kind=c_float) :: rAppliedPotentialET   ! this will be adjusted within loop to
  real (kind=c_float) :: rDailyRecharge           ! account for evap of interception
  real (kind=c_float) :: rStreamCapture
  real (kind=c_float) :: rDailyRejectedRecharge
  real (kind=c_float) :: rMAXIMUM_RECHARGE
  real (kind=c_float) :: rMin, rMean, rMax, rSum
  integer (kind=c_int) :: iRowCount

  type ( T_CELL ),pointer :: cel
  character (len=256) :: sBuf

  !! initialize basic grid cell variables

  iNumGridCells = count(pGrd%iMask /= iINACTIVE_CELL )

  ! call to "julian_day" includes the optional origin term...
  ! return value will be the number of days *SINCE* that origin term
  iTime = julian_day( iYear, iMonth, iDay, pConfig%iStartJulianDay)

  ! array is traversed in column-major order (i.e. processed a column
  ! at a time, which should be more efficient in terms of Fortran
  ! memory management)

  row_idx: do iRow=1,pGrd%iNY
    col_idx: do iCol=1,pGrd%iNX
      cel => pGrd%Cells(iCol,iRow)

  L0: if (pGrd%iMask(iCol, iRow) /= iINACTIVE_CELL) then

        rMAXIMUM_RECHARGE = pConfig%MAX_RECHARGE(cel%iLandUseIndex, cel%iSoilGroup)

        ! calculate net infiltration
        rNetInfil = MAX(rZERO, &
                    cel%rNetRainfall &
                  + cel%rSnowMelt &
                  + cel%rIrrigationAmount &
                  + cel%rInFlow &
                  - cel%rOutFlow &
#ifdef STREAM_INTERACTIONS
                  - cel%rStreamCapture &
#endif
                  - cel%rFlowOutOfGrid)

        ! calculate net inflow to cell
        rNetInflow = MAX(rZERO, &
                    cel%rNetRainfall &
                  + cel%rSnowMelt &
                  + cel%rIrrigationAmount &
!#ifdef STREAM_INTERACTIONS
!                  - cel%rStreamCapture &   ! "Net Inflow" is really just
!#endif                                     ! a diagnostic internal variable
                  + cel%rInFlow )           ! Supposed to be the sum of all
                                            ! water sources to a grid cell
                                            ! *BEFORE* any routing has occurred

        !! calculate difference between potential ET and precipitation
        !! ("precipitation" here includes surface runoff and snowmelt)
        !! A negative value indicates the amount by which precip
        !! fails to satisfy potential water needs of a vegetation-covered area
        !!
        !! NEW: subtracting interception value from potential ET before
        !!      applying the potential ET term elsewhere in the water balance
        !!      assumption is that any intercepted water evaporates on the
        !!      same day as it fell
        !!

      rPrecipMinusPotentET = rNetInfil - cel%rReferenceET0_adj

      MAIN: if(cel%rSoilWaterCap <= rNear_ZERO &
              .or. cel%iLandUse == pConfig%iOPEN_WATER_LU) then
           ! Soil Water Capacity <= 0; OPEN WATER CELLS

            if(rPrecipMinusPotentET <= rZERO) then
            ! Precip is *LESS THAN* Potential ET

              ! all water that comes (precip, interception, inflow) evaporates
              cel%rActualET = rNetInfil

              ! any outflow that has not already been routed elsewhere
              ! is rerouted to flow out of grid
              cel%rFlowOutOfGrid = cel%rFlowOutOfGrid + cel%rOutflow

            else  ! code block L3a: Precip EXCEEDS PotentialET

              ! *PART* of the water that comes in evaporates, the
              ! remainder leaves the grid via surface water features

              cel%rActualET = cel%rReferenceET0_adj
              cel%rFlowOutOfGrid = cel%rFlowOutOfGrid &
                              + rNetInfil - cel%rActualET &
                              + cel%rOutflow

            end if

            cel%rOutflow = rZERO
            rDailyRecharge = rZERO
            cel%rDailyRecharge = rZERO
            cel%rSoilMoisture = rZERO
!          cel%rInterceptionStorage = rZERO
            rMoistureDeficit = rZERO
            rMoistureSurplus = rZERO
            cel%rSM_AccumPotentWatLoss = rZERO
            rChangeInStorage = rZERO
            rDailyRejectedRecharge = rZERO

          else ! MAIN: Execute remainder of block ONLY if non-WATER cell

            ! Zero out current estimate for daily recharge, rejected recharge
            rDailyRecharge = rZERO
            cel%rDailyRecharge = rZERO
            rDailyRejectedRecharge = rZERO

            rPrevious_Soil_Moisture = cel%rSoilMoisture
            ! the following code block (L1) calculates current soil moisture values
            ! and updates the daily soil moisture deficit or surplus terms...
            ! any moisture deficit adds to the accumulated potential water loss term

        L1: if(rPrecipMinusPotentET <= rZERO) then  ! Precip *LESS THAN* PotentET

              ! **precipitation FAILS to meet ET demands....**
              ! add precip minus potential ET term to accumulated
              ! potential water loss; subtract off the portion of
              ! P - PET related to evap from interception

              cel%rSM_AccumPotentWatLoss = MAX(APWL_Cap, &
                                       (cel%rSM_AccumPotentWatLoss &
                                       + rPrecipMinusPotentET ))

              rMoistureDeficit = rZERO
              rMoistureSurplus = rZERO

              ! determine soil moisture given updated accumulated potential water loss
         L1a: if ( cel%rSoilWaterCap > rNEAR_ZERO) then

           L1b: if(pConfig%iConfigureFAO56 /= CONFIG_FAO56_ONE_FACTOR_NONSTANDARD &
                  .and. pConfig%iConfigureFAO56 /= CONFIG_FAO56_TWO_FACTOR_NONSTANDARD ) then

                  if ( pConfig%iConfigureSM == CONFIG_SM_TM_LOOKUP_TABLE ) then

                    ! look up soil moisture in T-M tables
                    cel%rSoilMoisture = grid_Interpolate(gWLT,cel%rSoilWaterCap, &
                      cel%rSM_AccumPotentWatLoss)

                  elseif ( pConfig%iConfigureSM == CONFIG_SM_TM_EQUATIONS ) then

                    ! calculate soil moisture w equation SUMMARIZING T-M tables
                    cel%rSoilMoisture = sm_thornthwaite_mather_soil_storage( &
                      cel%rSoilWaterCap, cel%rSM_AccumPotentWatLoss)

                  else

                    call assert (lFALSE, "If the FAO-56 methodology is *not* " &
                      //"being used, soil moisture retention must be read from " &
                      //"~either standard tables or equations", &
                      trim(__FILE__), __LINE__)

                  endif

             L1c: if(ABS(cel%rSoilMoisture - rPrevious_Soil_Moisture) &
                     > ABS(rPrecipMinusPotentET)) then

                    cel%rSoilMoisture = MAX(rZERO, &
                                      (rPrevious_Soil_Moisture &
                                          + rPrecipMinusPotentET ))

                    ! regardless of what Thornthwaite-Mather tables tell us,
                    ! we are capping the total soil loss at the value of
                    ! precip minus potential ET...   under some conditions
                    ! it seems that the T-M tables dry out the soil at a
                    ! rate that *exceeds* precip minus PET

                  endif L1c

                else  ! L1b: we are calculating soil moisture without T-M
                           ! soil-moisture retention tables (i.e. FAO56)

                  cel%rSoilMoisture = MAX(rZERO, &
                                        cel%rSoilMoisture &
                                      + rPrecipMinusPotentET )

                endif L1b

              endif L1a

            else  ! code block L1: Precip *EXCEEDS* Potential ET

              !! **precipitation EXCEEDS ET demands, recharging soil column**
              !! Precip - Potential ET > 0: add infiltrated water
              !! directly to the soil moisture term

              rMoistureDeficit = rZERO

              rMoistureSurplus = MAX (rZERO, &
                             (cel%rSoilMoisture &
                                + rPrecipMinusPotentET &
                                - cel%rSoilWaterCap))

              cel%rSoilMoisture = MIN(cel%rSoilWaterCap, &
                                (cel%rSoilMoisture &
                                   + rPrecipMinusPotentET))

              !! back-calculate new equivalent accumulated potential water loss term
              !! given current soil moisture

         L1d: if(cel%rSoilWaterCap>rNEAR_ZERO) then

           L1e: if(pConfig%iConfigureSM == CONFIG_SM_TM_LOOKUP_TABLE ) then

                  ! look up APWL in T-M tables
                  cel%rSM_AccumPotentWatLoss = &
                  grid_SearchColumn(gWLT,cel%rSoilWaterCap,cel%rSoilMoisture,-rONE)

                elseif(pConfig%iConfigureSM == CONFIG_SM_TM_EQUATIONS ) then

                  ! detemine APWL from an equation
                  cel%rSM_AccumPotentWatLoss = &
                  sm_thornthwaite_mather_APWL(cel%rSoilWaterCap,REAL(cel%rSoilMoisture, kind=c_double) )

                else  ! L1e: we are *NOT* using T-M soil moisture retention tables or equations

                  cel%rSM_AccumPotentWatLoss = MIN(rZERO, &
                    cel%rSM_AccumPotentWatLoss + cel%rSoilMoisture &
                     - rPrevious_Soil_Moisture)

                endif L1e

              endif L1d

            end if L1

            !! calculate change in soil moisture storage
            rChangeInStorage = cel%rSoilMoisture &
                         - rPrevious_Soil_Moisture

            !! the following code block (L2) updates estimate of ACTUAL ET
        L2: if(rPrecipMinusPotentET <= rZERO) then  ! Precip - Potential ET <= 0

              !! cap actual ET at the estimate for potential ET
              cel%rActualET = MIN( rNetInfil + ABS(rChangeInStorage), &
                                     cel%rReferenceET0_adj)

        		  ! 'rMoistureDeficit' represents the DEFICIT term in the
	  					! original Thornthwaite-Mather calculations. DEFICIT is supposed
		  				! to capture the amount of water demand that *cannot* be met
			  			! by precipitation and the soil reservoir
              rMoistureDeficit = cel%rReferenceET0_adj - cel%rActualET

            else  ! code block L2: Precip - Potential ET > 0

              cel%rActualET = cel%rReferenceET0_adj

             end if L2

            ! ** CALCULATE RECHARGE
            ! based on SOIL MOISTURE SURPLUS

            rDailyRecharge = rMoistureSurplus
            cel%rDailyRecharge = rMoistureSurplus

          endif MAIN

          ! if calculated recharge exceeds the estimated Kv, cap at that value
          ! and characterize the rest as "rejected recharge"
          if(rDailyRecharge > rMAXIMUM_RECHARGE) then

            rDailyRejectedRecharge = rDailyRecharge - rMAXIMUM_RECHARGE
            cel%rDailyRecharge = rMAXIMUM_RECHARGE
            rDailyRecharge = rMAXIMUM_RECHARGE

            ! Now, figure out what to do with any rejected recharge
            if ( cel%iTgt_Col == iROUTE_LEFT_GRID .or. &
              cel%iTgt_Row == iROUTE_LEFT_GRID) then
                cel%rFlowOutOfGrid = cel%rFlowOutOfGrid + rDailyRejectedRecharge
                rDailyRejectedRecharge = rZERO
            elseif ( cel%iTgt_Col == iROUTE_DEPRESSION .or. &
              cel%iTgt_Row == iROUTE_DEPRESSION) then
              ! Don't route any further; the water pools here.
              ! nothing to do; leave rDailyRejectedRecharge value alone
            elseif ( cel%iTgt_Col >0 .and. cel%iTgt_Col <= pGrd%iNX &
              .and. cel%iTgt_Row >0 .and. cel%iTgt_Row <= pGrd%iNY) then

                ! CAUTION!! We must *not* access illegal values for target cells
                if(pGrd%Cells(cel%iTgt_Col,cel%iTgt_Row)%iLandUse == pConfig%iOPEN_WATER_LU &
                 .or. pGrd%Cells(cel%iTgt_Col,cel%iTgt_Row)%rSoilWaterCap<rNEAR_ZERO) then
                 ! Don't route any further; the water enters a surface water feature.
                 ! nothing to do; leave rDailyRejectedRecharge value alone
                elseif(pConfig%iConfigureRunoffMode /= CONFIG_RUNOFF_NO_ROUTING) then
                  ! add cell rejected recharge to target cell inflow
                  pGrd%Cells( cel%iTgt_Col, cel%iTgt_Row)%rInflow = &
                  pGrd%Cells( cel%iTgt_Col, cel%iTgt_Row)%rInflow + &
                  rDailyRejectedRecharge * cel%rRouteFraction
                  cel%rOutflow = &
                    cel%rOutflow + &
                    rDailyRejectedRecharge * cel%rRouteFraction
                  rDailyRejectedRecharge = &
                    rDailyRejectedRecharge * (rONE - cel%rRouteFraction)
                end if
            end if
          end if

          if(cel%rSoilWaterCap > rNear_ZERO) then

            ! update soil moisture as a percentage of soil water capacity
            cel%rSoilMoisturePct = cel%rSoilMoisture / cel%rSoilWaterCap * rHUNDRED

          else

            cel%rSoilMoisturePct = rZERO

          end if

          cel%rMSB = cel%rNetRainfall &
                 + cel%rSnowMelt &
                 + cel%rInFlow &
                 + cel%rIrrigationAmount &
                 - cel%rOutFlow &
#ifdef STREAM_INTERACTIONS
                 - cel%rStreamCapture &
#endif
                 - cel%rFlowOutOfGrid &
                 - rChangeInStorage &
                 - cel%rActualET &
                 - rDailyRecharge &
                 - rDailyRejectedRecharge

          if(cel%rMSB>0.1 .or. cel%rMSB< -0.1 ) then
            write(UNIT=LU_LOG,FMT=*) "** MASS BALANCE ERROR **"

            write(UNIT=LU_LOG,FMT="(/,'  date: ',i2.2,'/',i2.2,'/',i4.4)") &
              iMonth, iDay, iYear
            write(unit=LU_LOG,FMT="('  cell (iRow,iCol) :',i5,i5)") iRow,iCol
            write(unit=LU_LOG,FMT="('  landuse    :',i4)") cel%iLandUse
            write(unit=LU_LOG,FMT="('  soil group :',i4)") cel%iSoilGroup
            if ( cel%iTgt_Col >0 .and. cel%iTgt_Col <= pGrd%iNY &
             .and. cel%iTgt_Row >0 .and. cel%iTgt_Row <= pGrd%iNX) then
              write(unit=LU_LOG,FMT="('  cell (iRow,iCol) :         ',i5,i5)") &
                cel%iTgt_Row, cel%iTgt_Col
              write(unit=LU_LOG,FMT="('  target land use :  ',i4)") &
                pGrd%Cells( cel%iTgt_Col, cel%iTgt_Row)%iLandUse
              write(unit=LU_LOG,FMT="('  target soil group :',i4)") &
                pGrd%Cells( cel%iTgt_Col, cel%iTgt_Row)%iSoilGroup
            endif

            write(UNIT=LU_LOG,FMT="(/,'  MASS BALANCE: ',t32,F14.4)") cel%rMSB

            write(UNIT=LU_LOG,FMT="(/,'  (+) cel%rNetRainfall: ',t32,F14.4)") &
              cel%rNetRainfall
            write(UNIT=LU_LOG,FMT="('  (+) cel%rSnowMelt: ',t32,F14.4)") &
              cel%rSnowMelt
            write(UNIT=LU_LOG,FMT="('  (+) cel%rInterception: ',t32,F14.4)") &
              cel%rInterception
            write(UNIT=LU_LOG,FMT="('  (+) cel%rIrrigationAmount: ',t32,F14.4)") &
              cel%rIrrigationAmount
            write(UNIT=LU_LOG,FMT="('  (+) cel%rInFlow: ',t32,F14.4)") &
              cel%rInFlow
            write(UNIT=LU_LOG,FMT="('  (-) cel%rOutFlow: ',t32,F14.4)") &
              cel%rOutFlow
            write(UNIT=LU_LOG,FMT="('  (-) cel%rFlowOutOfGrid: ',t32,F14.4)") &
              cel%rFlowOutOfGrid
            write(UNIT=LU_LOG,FMT="('  (-) rChangeInStorage: ',t32,F14.4)") &
              rChangeInStorage
            write(UNIT=LU_LOG,FMT="('  (-) cel%rActualET: ',t32,F14.4)") &
              cel%rActualET
            write(UNIT=LU_LOG,FMT="('  (-) rDailyRecharge: ',t32,F14.4)") &
              rDailyRecharge
            write(UNIT=LU_LOG,FMT="('  (-) rDailyRejectedRecharge: ',t32,F14.4)") &
              rDailyRejectedRecharge
#ifdef STREAM_INTERACTIONS
            write(UNIT=LU_LOG,FMT="('  (-) rStreamCapture: ',t32,F14.4)") &
              cel%rStreamCapture
#endif
            write(UNIT=LU_LOG,FMT=*)
            write(UNIT=LU_LOG,FMT="('  rNetInfil: ',t32,F14.4)") &
              rNetInfil
            write(UNIT=LU_LOG,FMT="('  rNetInflow: ',t32,F14.4)") &
              rNetInflow
            write(UNIT=LU_LOG,FMT="('  rPrecipMinusPotentET: ',t32,F14.4)") &
              rPrecipMinusPotentET
            write(UNIT=LU_LOG,FMT="('  rMoistureSurplus: ',t32,F14.4)") &
              rMoistureSurplus
            write(UNIT=LU_LOG,FMT="('  rMoistureDeficit: ',t32,F14.4)") &
              rMoistureDeficit
            write(UNIT=LU_LOG,FMT="('  cel%rSoilMoisture: ',t32,F14.4)") &
              cel%rSoilMoisture
            write(UNIT=LU_LOG,FMT="('  cel%rRouteFraction: ',t32,F14.4)") &
              cel%rRouteFraction

            write(UNIT=LU_LOG,FMT=*) "-----------------------------------------------"
            write(UNIT=LU_LOG,FMT=*)

          endif

        else

          rPrevious_Soil_Moisture = rZERO
          rPrecipMinusPotentET = rZERO
          rMSB_DailyMassBalance = rZERO
          rMoistureDeficit = rZERO
          rMoistureSurplus = rZERO
          rChangeInStorage = rZERO
          rNetInfil = rZERO
          rNetInflow = rZERO
          rAppliedPotentialET = rZERO
          rDailyRecharge = rZERO
          rStreamCapture = rZERO
          rDailyRejectedRecharge = rZERO
          rMAXIMUM_RECHARGE = rZERO

        endif L0

      ! *** CALL TO OUTPUT/ARCHIVE ROUTINES.... ****
      !  for each grid cell we must make a call to RLE_writeByte if
      !  we expect to have graphical or gridded output at a later stage

!#ifdef NETCDF_SUPPORT
!      call output_to_netcdf(pGrd, pConfig, cel, iRow, iCol, iTime, &
!        rDailyRejectedRecharge,rNetInflow,rNetInfil,cel%rActualET, &
!        rPrecipMinusPotentET,rMoistureDeficit,rMoistureSurplus, &
!        rChangeInStorage,rDailyRecharge)
!#endif

      call output_to_SWB_binary(pGrd, pConfig, cel, iRow, iCol, iTime, &
        rDailyRejectedRecharge,rNetInflow,rNetInfil,cel%rActualET, &
        rPrecipMinusPotentET,rMoistureDeficit,rMoistureSurplus, &
        rChangeInStorage,rDailyRecharge)

      call output_to_SSF(pGrd, pConfig, cel, iRow, iCol, &
        iMonth, iDay, iYear, &
        rDailyRejectedRecharge,rNetInflow,rNetInfil,cel%rActualET, &
        rPrecipMinusPotentET,rMoistureDeficit,rMoistureSurplus, &
        rChangeInStorage,rDailyRecharge)

      ! UPDATE MONTHLY and ANNUAL ACCUMULATORS HERE
      if (pGrd%iMask(iCol, iRow) /= iINACTIVE_CELL ) then
        call output_update_accumulators(cel, iMonth, &
          rDailyRejectedRecharge,rNetInflow,rNetInfil,cel%rActualET, &
          rPrecipMinusPotentET,rMoistureDeficit,rMoistureSurplus, &
          rChangeInStorage,rDailyRecharge)

        if(iYear>= pConfig%iStartYearforCalculation .and. &
             iYear<= pConfig%iEndYearforCalculation) then

          cel%rSUM_Recharge = cel%rSUM_Recharge + rDailyRecharge
          cel%rSUM_RejectedRecharge = cel%rSUM_RejectedRecharge + rDailyRejectedRecharge

        end if

      endif

    end do col_idx

  end do row_idx

  ! now issue the following calls to trigger calculation of daily averages
  call output_finalize_accumulators(cel, iMonth, iNumGridCells, &
    rDailyRejectedRecharge,rNetInflow,rNetInfil,cel%rActualET, &
    rPrecipMinusPotentET,rMoistureDeficit,rMoistureSurplus, &
    rChangeInStorage,rDailyRecharge)

end subroutine calculate_water_balance

end module water_balance
