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

  integer (c_int), intent(in) :: iDayOfYear
  integer (c_int), intent(in) :: iDay
  integer (c_int), intent(in) :: iMonth
  integer (c_int), intent(in) :: iYear
  ! [ LOCALS ]
  integer (c_int) :: iCol,iRow,k,l,iTgt_Col,iTgt_Row
  integer (c_int) :: iNumGridCells
  integer (c_int) :: iTime
  real (c_float) :: rPrevious_Soil_Moisture
  real (c_float) :: rPrecipMinusPotentET
  real (c_float) :: rMSB_DailyMassBalance
  real (c_float) :: rMoistureDeficit
  real (c_float) :: rMoistureSurplus
  real (c_float) :: rChangeInStorage
  real (c_float) :: rNetInfil
  real (c_float) :: rNetInflow
  real (c_float) :: rStreamCapture
  real (c_float) :: rSoilMoistureTemp
  real (c_float) :: rMin, rMean, rMax, rSum
  integer (c_int) :: iRowCount

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

      cel%rDailyRecharge = rZERO
      cel%rRejectedRecharge = rZERO
      rPrevious_Soil_Moisture = rZERO
      rPrecipMinusPotentET = rZERO
      rMSB_DailyMassBalance = rZERO
      rChangeInStorage = rZERO
      rNetInfil = rZERO
      rNetInflow = rZERO
      cel%rDailyRecharge = rZERO
      rStreamCapture = rZERO
      cel%rRejectedRecharge = rZERO
      cel%rActual_ET_soil = rZERO

      rMoistureDeficit = rZERO
      rMoistureSurplus = rZERO


    ! 88           ,a8888a,
    ! 88         ,8P"'  `"Y8,
    ! 88        ,8P        Y8,
    ! 88        88          88
    ! 88        88          88
    ! 88        `8b        d8'
    ! 88         `8ba,  ,ad8'
    ! 88888888888  "Y8888P"

  L0: if (pGrd%iMask(iCol, iRow) == iINACTIVE_CELL) then

        ! we're dealing with an inactive cell; zero out terms for storage and analysis

      else  ! we have an *ACTIVE* cell; proceed

        ! by this point in the daily execution, runoff has been converted to
        ! outflow (flow contributing to a downslope cell) and
        ! flow_out_of_grid (flow to an unmodeled surfce water feature or model boundary)

        ! calculate net infiltration
        rNetInfil = MAX(rZERO,                 &
                    cel%rNetRainfall           &
                  + cel%rSnowMelt              &
                  + cel%rIrrigationAmount      &
                  + cel%rInFlow                &
                  - cel%rOutFlow               &
                  - cel%rFlowOutOfGrid)

        ! calculate net inflow to cell
        rNetInflow = MAX(rZERO,                &
                    cel%rNetRainfall           &
                  + cel%rSnowMelt              &
                  + cel%rIrrigationAmount      &
                  + cel%rInFlow )           ! Supposed to be the sum of all
                                            ! water sources to a grid cell
                                            ! *BEFORE* any routing has occurred

        !! calculate difference between potential ET and precipitation
        !! ("precipitation" here includes surface runoff and snowmelt)
        !! A negative value indicates the amount by which precip
        !! fails to satisfy potential water needs of a vegetation-covered area

      !> @NOTE At this point in the calculation, rReferenceET0_adj has been reduced by the amount
      !!       of interception that evaporates, if that option is activated

      ! if FAO-56 being used: rReferenceET0_adj = bare_soil_evap + crop_etc
      !                                           and should already reflect any
      !                                           stress-related limitations
      rPrecipMinusPotentET = rNetInfil - cel%rReferenceET0_adj
      !

      MAIN: if(cel%rSoilWaterCap <= rNear_ZERO &
              .or. cel%iLandUse == pConfig%iOPEN_WATER_LU) then
           ! Soil Water Capacity <= 0; OPEN WATER CELLS

           ! **************************************************************************************************
           ! ##      ##    ###    ######## ######## ########      ######  ######## ##       ##
           ! ##  ##  ##   ## ##      ##    ##       ##     ##    ##    ## ##       ##       ##
           ! ##  ##  ##  ##   ##     ##    ##       ##     ##    ##       ##       ##       ##
           ! ##  ##  ## ##     ##    ##    ######   ########     ##       ######   ##       ##
           ! ##  ##  ## #########    ##    ##       ##   ##      ##       ##       ##       ##
           ! ##  ##  ## ##     ##    ##    ##       ##    ##     ##    ## ##       ##       ##
           !  ###  ###  ##     ##    ##    ######## ##     ##     ######  ######## ######## ########
           ! **************************************************************************************************

            if(rPrecipMinusPotentET <= rZERO) then
            ! Precip is *LESS THAN* Potential ET

              ! all water that comes in (precip, interception, inflow) evaporates
              cel%rActual_ET_soil = rNetInfil

            else  ! code block L3a: Precip EXCEEDS PotentialET

              ! *PART* of the water that comes in evaporates, the
              ! remainder leaves the grid via surface water features

              cel%rActual_ET_soil = cel%rReferenceET0

              !> @TODO check the assumptions behind this formulation.
              cel%rFlowOutOfGrid = cel%rFlowOutOfGrid         &
                                   + rNetInfil                &
                                   - cel%rActual_ET_soil      &
                                   + cel%rOutflow
            end if

            cel%rOutflow = rZERO
            cel%rSoilMoisture = rZERO
            cel%rInterceptionStorage = rZERO
            cel%rSM_AccumPotentWatLoss = rZERO
            rChangeInStorage = rZERO
            cel%rSoilMoisturePct = rZERO

          else ! MAIN: Execute remainder of block ONLY if non-WATER cell

        ! **************************************************************************************************
        ! ##    ##  #######  ########  ##     ##    ###    ##           ######  ######## ##       ##
        ! ###   ## ##     ## ##     ## ###   ###   ## ##   ##          ##    ## ##       ##       ##
        ! ####  ## ##     ## ##     ## #### ####  ##   ##  ##          ##       ##       ##       ##
        ! ## ## ## ##     ## ########  ## ### ## ##     ## ##          ##       ######   ##       ##
        ! ##  #### ##     ## ##   ##   ##     ## ######### ##          ##       ##       ##       ##
        ! ##   ### ##     ## ##    ##  ##     ## ##     ## ##          ##    ## ##       ##       ##
        ! ##    ##  #######  ##     ## ##     ## ##     ## ########     ######  ######## ######## ########
        ! **************************************************************************************************

            rPrevious_Soil_Moisture = cel%rSoilMoisture
            ! the following code block (L1) calculates current soil moisture values
            ! and updates the daily soil moisture deficit or surplus terms...
            ! any moisture deficit adds to the accumulated potential water loss term

        L1: if(rPrecipMinusPotentET <= rZERO) then  ! Precip *LESS THAN* Potential ET

              ! ooooooooo.                     ooooooooo.   oooooooooooo                    .oooo.
              ! `888   `Y88.                   `888   `Y88. `888'     `8         .dP       d8P'`Y8b
              !  888   .d88'                    888   .d88'  888               .dP        888    888
              !  888ooo88P'                     888ooo88P'   888oooo8         dP          888    888
              !  888              8888888       888          888    "         Yb          888    888
              !  888                            888          888       o       `Yb        `88b  d88'
              ! o888o                          o888o        o888ooooood8         `Yb       `Y8bd8P'

              ! **precipitation FAILS to meet ET demands....**
              ! add precip minus potential ET term to accumulated
              ! potential water loss; subtract off the portion of
              ! P - PET related to evap from interception

              cel%rSM_AccumPotentWatLoss = MAX(APWL_Cap, &
                                       (cel%rSM_AccumPotentWatLoss &
                                       + rPrecipMinusPotentET ))

  !                     oooo                                     o8o  oooo                                    o8o               .
  !                     `888                                     `"'  `888                                    `"'             .o8
  !  .ooooo.   .oooo.    888   .ooooo.        .oooo.o  .ooooo.  oooo   888       ooo. .oo.  .oo.    .ooooo.  oooo   .oooo.o .o888oo
  ! d88' `"Y8 `P  )88b   888  d88' `"Y8      d88(  "8 d88' `88b `888   888       `888P"Y88bP"Y88b  d88' `88b `888  d88(  "8   888
  ! 888        .oP"888   888  888            `"Y88b.  888   888  888   888        888   888   888  888   888  888  `"Y88b.    888
  ! 888   .o8 d8(  888   888  888   .o8      o.  )88b 888   888  888   888        888   888   888  888   888  888  o.  )88b   888 .
  ! `Y8bod8P' `Y888""8o o888o `Y8bod8P'      8""888P' `Y8bod8P' o888o o888o      o888o o888o o888o `Y8bod8P' o888o 8""888P'   "888"

              ! determine soil moisture given updated accumulated potential water loss
         L1a: if ( pConfig%iConfigureFAO56 /= CONFIG_FAO56_ONE_FACTOR_NONSTANDARD             &
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

                  L1b: if(ABS(cel%rSoilMoisture - rPrevious_Soil_Moisture) &
                         > ABS(rPrecipMinusPotentET)) then

                     cel%rSoilMoisture = MAX(rZERO,                                              &
                                            (rPrevious_Soil_Moisture + rPrecipMinusPotentET ) )

                    ! regardless of what Thornthwaite-Mather tables tell us,
                    ! we are capping the total soil loss at the value of
                    ! precip minus potential ET...   under some conditions
                    ! it seems that the T-M tables dry out the soil at a
                    ! rate that *exceeds* precip minus PET

                  endif L1b

              else  ! L1a: we are calculating soil moisture without T-M
                         ! soil-moisture retention tables (i.e. FAO56)

                 ! ReferenceET0_adj is adjusted downward as the soil dries owing to the water stress
                 ! coefficient calculations
                 cel%rSoilMoisture = MAX(rZERO,                                              &
                                        (rPrevious_Soil_Moisture + rPrecipMinusPotentET ) )

             endif L1a

              !! calculate change in soil moisture storage
              rChangeInStorage = cel%rSoilMoisture - rPrevious_Soil_Moisture

              !! change in storage will be negative; offset somewhat by whatever
              !! enters as net infiltration
              cel%rActual_ET_soil = rNetInfil - rChangeInStorage

              ! 'rMoistureDeficit' represents the DEFICIT term in the
              ! original Thornthwaite-Mather calculations. DEFICIT is supposed
              ! to capture the amount of water demand that *cannot* be met
              ! by precipitation and the soil reservoir
              rMoistureDeficit = cel%rReferenceET0_adj - cel%rActual_ET_soil

            else  ! code block L1: Precip *EXCEEDS* Potential ET


              ! ooooooooo.                     ooooooooo.   oooooooooooo                    .oooo.
              ! `888   `Y88.                   `888   `Y88. `888'     `8      Yb           d8P'`Y8b
              !  888   .d88'                    888   .d88'  888               `Yb        888    888
              !  888ooo88P'                     888ooo88P'   888oooo8            `Yb      888    888
              !  888              8888888       888          888    "            .dP      888    888
              !  888                            888          888       o       .dP        `88b  d88'
              ! o888o                          o888o        o888ooooood8      dP           `Y8bd8P'


              !! **precipitation EXCEEDS ET demands, recharging soil column**
              !! Precip - Potential ET > 0: add infiltrated water
              !! directly to the soil moisture term

              cel%rActual_ET_soil = cel%rReferenceET0_adj

              rSoilMoistureTemp = cel%rSoilMoisture + rPrecipMinusPotentET

              ! extra water available after ET demands have been met are converted to
              ! Healy's 'net infiltration' (potential recharge)
              rMoistureSurplus = MAX(rZERO, rSoilMoistureTemp - cel%rSoilWaterCap)

              cel%rSoilMoisture = MIN(cel%rSoilWaterCap, rSoilMoistureTemp )

             !! calculate change in soil moisture storage
             !! new soil moisture value is greater than previous, so change in
             !! storage should be positive here
             rChangeInStorage = cel%rSoilMoisture - rPrevious_Soil_Moisture

              !! back-calculate new equivalent accumulated potential water loss term
              !! given current soil moisture


              !       .o.       ooooooooo.   oooooo   oooooo     oooo ooooo
              !      .888.      `888   `Y88.  `888.    `888.     .8'  `888'
              !     .8"888.      888   .d88'   `888.   .8888.   .8'    888
              !    .8' `888.     888ooo88P'     `888  .8'`888. .8'     888
              !   .88ooo8888.    888             `888.8'  `888.8'      888
              !  .8'     `888.   888              `888'    `888'       888       o
              ! o88o     o8888o o888o              `8'      `8'       o888ooooood8


         L1c: if(pConfig%iConfigureSM == CONFIG_SM_TM_LOOKUP_TABLE ) then

                ! look up APWL in T-M tables
                cel%rSM_AccumPotentWatLoss = &
                grid_SearchColumn(gWLT,cel%rSoilWaterCap,real(cel%rSoilMoisture, c_float),-rONE)

              elseif(pConfig%iConfigureSM == CONFIG_SM_TM_EQUATIONS ) then

                ! detemine APWL from an equation
                cel%rSM_AccumPotentWatLoss = &
                sm_thornthwaite_mather_APWL(cel%rSoilWaterCap, cel%rSoilMoisture )

              else  ! L1c: we are *NOT* using T-M soil moisture retention tables or equations

                cel%rSM_AccumPotentWatLoss =                                                      &
                        MIN(rZERO,                                                                &
                        cel%rSM_AccumPotentWatLoss + cel%rSoilMoisture - rPrevious_Soil_Moisture)
              endif L1c

            end if L1

            ! ** CALCULATE RECHARGE
            ! based on SOIL MOISTURE SURPLUS
            cel%rDailyRecharge = rMoistureSurplus


  ! ooo        ooooo                            ooooooooo.                       oooo
  ! `88.       .888'                            `888   `Y88.                     `888
  !  888b     d'888   .oooo.   oooo    ooo       888   .d88'  .ooooo.   .ooooo.   888 .oo.    .oooo.   oooo d8b  .oooooooo  .ooooo.
  !  8 Y88. .P  888  `P  )88b   `88b..8P'        888ooo88P'  d88' `88b d88' `"Y8  888P"Y88b  `P  )88b  `888""8P 888' `88b  d88' `88b
  !  8  `888'   888   .oP"888     Y888'          888`88b.    888ooo888 888        888   888   .oP"888   888     888   888  888ooo888
  !  8    Y     888  d8(  888   .o8"'88b         888  `88b.  888    .o 888   .o8  888   888  d8(  888   888     `88bod8P'  888    .o
  ! o8o        o888o `Y888""8o o88'   888o      o888o  o888o `Y8bod8P' `Y8bod8P' o888o o888o `Y888""8o d888b    `8oooooo.  `Y8bod8P'
  !                                                                                                             d"     YD
  !                                                                                                             "Y88888P'

          ! if calculated recharge exceeds the estimated Kv, cap at that value
          ! and characterize the rest as "rejected recharge"
          if(cel%rDailyRecharge > cel%rMaximumRechargeRate) then

            cel%rRejectedRecharge = cel%rDailyRecharge - cel%rMaximumRechargeRate
            cel%rDailyRecharge = cel%rMaximumRechargeRate

            if ( pConfig%iConfigureRunoffMode == CONFIG_RUNOFF_NO_ROUTING ) then

                cel%rFlowOutOfGrid = cel%rFlowOutOfGrid + cel%rRejectedRecharge

            elseif ( cel%iTgt_Col >0 .and. cel%iTgt_Col <= pGrd%iNX &
              .and. cel%iTgt_Row >0 .and. cel%iTgt_Row <= pGrd%iNY) then

              if(pGrd%Cells(cel%iTgt_Col,cel%iTgt_Row)%iLandUse == pConfig%iOPEN_WATER_LU &
               .or. pGrd%Cells(cel%iTgt_Col,cel%iTgt_Row)%rSoilWaterCap<rNEAR_ZERO) then

                cel%rFlowOutOfGrid = cel%rFlowOutOfGrid + cel%rRejectedRecharge

              else

                ! cell rejected recharge is added to outflow amount
                cel%rOutflow = cel%rOutflow + cel%rRejectedRecharge
                ! add rejected recharge to downslope cell
                pGrd%Cells( cel%iTgt_Col, cel%iTgt_Row)%rInflow =                             &
                            pGrd%Cells( cel%iTgt_Col, cel%iTgt_Row)%rInflow + cel%rOutflow

              endif

            else

                cel%rFlowOutOfGrid = cel%rFlowOutOfGrid + cel%rRejectedRecharge

            end if
          endif

          ! update soil moisture as a percentage of soil water capacity
          cel%rSoilMoisturePct = cel%rSoilMoisture / cel%rSoilWaterCap * rHUNDRED

          cel%rMSB = cel%rNetRainfall              &
                    + cel%rSnowMelt                &
                    + cel%rInFlow                  &
                    + cel%rIrrigationAmount        &
                    - cel%rOutFlow                 &
                    - cel%rFlowOutOfGrid           &
                    - rChangeInStorage             &
                    - cel%rActual_ET_soil          &
                    - cel%rDailyRecharge

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
             write(UNIT=LU_LOG,FMT="('  (-) cel%rActual_ET_soil: ',t32,F14.4)") &
               cel%rActual_ET_soil
             write(UNIT=LU_LOG,FMT="('  (-) cel%rDailyRecharge: ',t32,F14.4)") &
               cel%rDailyRecharge
             write(UNIT=LU_LOG,FMT=*)
             write(UNIT=LU_LOG,FMT="('  cel%rRejectedRecharge: ',t32,F14.4)") &
               cel%rRejectedRecharge
             write(UNIT=LU_LOG,FMT="('  rNetInfil: ',t32,F14.4)") &
               rNetInfil
             write(UNIT=LU_LOG,FMT="('  rNetInflow: ',t32,F14.4)") &
               rNetInflow
             write(UNIT=LU_LOG,FMT="('  rReferenceET: ',t32,F14.4)") &
               cel%rReferenceET0
             write(UNIT=LU_LOG,FMT="('  rReferenceET_adj: ',t32,F14.4)") &
               cel%rReferenceET0_adj
             write(UNIT=LU_LOG,FMT="('  rPrecipMinusPotentET: ',t32,F14.4)") &
               rPrecipMinusPotentET
             write(UNIT=LU_LOG,FMT="('  cel%rSM_AccumPotentWatLoss: ',t32,F14.4)") &
               cel%rSM_AccumPotentWatLoss
             write(UNIT=LU_LOG,FMT="('  rMoistureSurplus: ',t32,F14.4)") &
               rMoistureSurplus
             write(UNIT=LU_LOG,FMT="('  rMoistureDeficit: ',t32,F14.4)") &
               rMoistureDeficit
             write(UNIT=LU_LOG,FMT="('  cel%rSoilMoisture: ',t32,F14.4)") &
               cel%rSoilMoisture
             write(UNIT=LU_LOG,FMT="('  cel%rSoilMoistureCapacity: ',t32,F14.4)") &
               cel%rSoilWaterCap
             write(UNIT=LU_LOG,FMT="('  cel%rSoilMoisturePct: ',t32,F14.4)") &
               cel%rSoilMoisturePct
             write(UNIT=LU_LOG,FMT="('  rPrevious_Soil_Moisture: ',t32,F14.4)") &
               rPrevious_Soil_Moisture
             write(UNIT=LU_LOG,FMT="('  cel%rRouteFraction: ',t32,F14.4)") &
               cel%rRouteFraction

             write(UNIT=LU_LOG,FMT=*) "-----------------------------------------------"
             write(UNIT=LU_LOG,FMT=*)

          endif

        endif MAIN

      endif L0

      ! it only makes sense to add these terms if the soil actual et has been reduced
      ! to account for evaporation of interception
      if ( pConfig%iConfigureActET_Interception == CONFIG_INTERCEPTION_IS_PART_OF_ACTET ) then
        cel%rActualET = cel%rActual_ET_soil + cel%rActual_ET_interception
      else
        cel%rActualET = cel%rActual_ET_soil
      endif

      ! *** CALL TO OUTPUT/ARCHIVE ROUTINES.... ****
      !  for each grid cell we must make a call to RLE_writeByte if
      !  we expect to have graphical or gridded output at a later stage
     call output_to_SWB_binary(pGrd, pConfig, cel, iRow, iCol, iTime, &
        cel%rRejectedRecharge,rNetInflow,rNetInfil,cel%rActualET, &
        rPrecipMinusPotentET,rMoistureDeficit,rMoistureSurplus, &
        rChangeInStorage )

      call output_to_SSF(pGrd, pConfig, cel, iRow, iCol, &
        iMonth, iDay, iYear, &
        cel%rRejectedRecharge,rNetInflow,rNetInfil,cel%rActualET, &
        rPrecipMinusPotentET,rMoistureDeficit,rMoistureSurplus, &
        rChangeInStorage )

      ! UPDATE MONTHLY and ANNUAL ACCUMULATORS HERE
      if (pGrd%iMask(iCol, iRow) /= iINACTIVE_CELL ) then
        call output_update_accumulators(cel, iMonth, &
          cel%rRejectedRecharge,rNetInflow,rNetInfil,cel%rActualET, &
          rPrecipMinusPotentET,rMoistureDeficit,rMoistureSurplus, &
          rChangeInStorage )

        if(iYear>= pConfig%iStartYearforCalculation .and. &
             iYear<= pConfig%iEndYearforCalculation) then

          cel%rSUM_Recharge = cel%rSUM_Recharge + cel%rDailyRecharge
          cel%rSUM_Irrigation = cel%rSUM_Irrigation + cel%rIrrigationAmount
          cel%rSUM_RejectedRecharge = cel%rSUM_RejectedRecharge + cel%rRejectedRecharge

        end if

      endif

    end do col_idx

  end do row_idx

  ! now issue the following calls to trigger calculation of daily averages
  call output_finalize_accumulators(cel, iMonth, iNumGridCells, &
    cel%rRejectedRecharge,rNetInflow,rNetInfil,cel%rActualET, &
    rPrecipMinusPotentET,rMoistureDeficit,rMoistureSurplus, &
    rChangeInStorage )

end subroutine calculate_water_balance

end module water_balance
