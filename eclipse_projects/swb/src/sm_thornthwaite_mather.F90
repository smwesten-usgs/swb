!> @file
!> @brief Contains a single module, @ref sm_thornthwaite_mather, which estimates runoff by
!> means of the NRCS/SCS curve number method.

!> @brief Performs the actual soil-moisture balance once precip, snowmelt, runoff, and ET have
!> been calculated. Soil moisture for a given soil accumulated potential water loss (APWL)
!> is determined by means of Thornthwaite and Mathers' (1957) tables.
module sm_thornthwaite_mather
!!****h* SWB/sm_thornthwaite_mather
! NAME
!
!   sm_thornthwaite_mather.f95 - Soil moisture calculation routines based on Thornthwaite
!   and Mather (1957)
! SYNOPSIS
!   Soil moisture calculation routines based on Thornthwaite
!   and Mather (1957).
!
! NOTES
!
!!***

  use types
  use swb_grid
  use swb_stats
  use RLE

#ifdef NETCDF_SUPPORT
  use netcdf_support
#endif

  implicit none

  real (kind=T_SGL), parameter :: APWL_Cap = -40.69_T_SGL

  !! Module data

  !! Table of data for the Thornthwaite soil moisture balance calculations
  type ( T_GENERAL_GRID ),pointer :: gWLT

  ! parameters that allow the Thornthwaite-Mather tables (1957) to be
  ! represented by a single equation
  real (kind=T_DBL), parameter :: rTM_slope_term = 0.4787691
  real (kind=T_DBL), parameter :: rTM_exp_term = -1.036784

contains

subroutine sm_thornthwaite_mather_Configure ( sRecord )
  !! Configures the module based on command-line options
  ! [ ARGUMENTS ]
  character (len=*),intent(inout) :: sRecord
  ! [ LOCALS ]
  character (len=256) :: sOption

  ! Read the grid for the water-loss table
  call Chomp( sRecord, sOption )
  call Assert( len_trim(sOption) > 0, &
     "No Soil-moisture retention table file was specified", &
     TRIM(__FILE__), __LINE__)
  write(UNIT=LU_LOG,FMT=*)"Reading ",trim(sOption)," for soil-moisture retention information"
  gWLT => grid_Read( sOption, "SURFER", T_SGL_GRID )

  write(UNIT=LU_LOG,FMT=*)"Read in the soil-moisture retention file with the following dimensions:"
  write(UNIT=LU_LOG,FMT=*)"iNX = ",gWLT%iNX
  write(UNIT=LU_LOG,FMT=*)"iNY = ",gWLT%iNY
  write(UNIT=LU_LOG,FMT=*)"iDataType = ",gWLT%iDataType
  write(UNIT=LU_LOG,FMT=*)"rX0, rX1 = ",gWLT%rX0, gWLT%rX1
  write(UNIT=LU_LOG,FMT=*)"rY0, rY1 = ",gWLT%rY0, gWLT%rY1

end subroutine sm_thornthwaite_mather_Configure

!------------------------------------------------------------------------------

subroutine sm_thornthwaite_mather_Initialize ( pGrd, pConfig )
  !! Preconfigures soil moisture for the model grid 'pGrd'.
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  ! [ LOCALS ]
  integer (kind=T_INT) :: iRow,iCol
  type ( T_CELL ),pointer :: cel

  ! Initialize the accumulated water loss for each cell according to the
  ! initial soil moisture.

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX
      cel => pGrd%Cells(iCol,iRow)

      if ( cel%rSoilWaterCap > rNEAR_ZERO ) then

	    !! Constrain rSoilWaterCap to limits of Thornthwaite-Mather tables

        if ( cel%rSoilWaterCap < 0.5_T_SGL ) then
          write(UNIT=LU_LOG,FMT="(a,f10.3,a,i4,',',i4,a)") &
            'Soil water capacity', cel%rSoilWaterCap,' for cell ',iRow,iCol, &
            ' out of range; adjusted to 0.5'
          flush(unit=LU_LOG)
          cel%rSoilWaterCap = 0.51_T_SGL
        else if ( cel%rSoilWaterCap > 17.5_T_SGL ) then
          write(UNIT=LU_LOG,FMT="(a,f10.3,a,i4,',',i4,a)") &
            'Soil water capacity', cel%rSoilWaterCap,' for cell ',iRow,iCol, &
            ' out of range; adjusted to 17.5'
          flush(unit=LU_LOG)
          cel%rSoilWaterCap = 17.49_T_SGL
        end if

		 !! convert input soil moisture (as percent of soil water capacity)
		 !! TO soil moisture in inches
		 cel%rSoilMoisture = (cel%rSoilMoisturePct/rHUNDRED) * cel%rSoilWaterCap

      !! back-calculate initial accumulated potential water loss term
      !! given initial soil moisture

#ifdef TM_TABLE

       ! calculate APWL from T-M table
       cel%rSM_AccumPotentWatLoss = &
         grid_SearchColumn(gWLT,cel%rSoilWaterCap,cel%rSoilMoisture,-rONE)

#else

       ! calculate APWL from equation
       cel%rSM_AccumPotentWatLoss = &
         sm_thornthwaite_mather_APWL(cel%rSoilWaterCap,cel%rSoilMoisture)

#endif

	  end if
    end do
  end do

#ifdef DEBUG_PRINT
 call grid_WriteArcGrid("initial_APWL.asc",dpZERO,dpONE,dpZERO, &
    dpONE,pGrd%Cells(:,:)%rSM_AccumPotentWatLoss)
#endif

  !! initialize Thorntwaite-Mather soil moisture accounting variables

  pGrd%Cells%rSM_PotentialET = rZERO

end subroutine sm_thornthwaite_mather_Initialize

!------------------------------------------------------------------------------

subroutine sm_thornthwaite_mather_UpdateSM ( pGrd, pConfig, &
      iDayOfYear, iDay, iMonth, iYear)
  !! Handles the soil moisture calculations using the model grid 'pGrd' and
  !! the water-loss table in gWLT
  !!
  !! The water loss values are looked up in the gWLT table by interpolation
  !! and by searching the columns in the grid according to the water capacity
  !! in each cell.
  !!
  !! During the calculations, the model grid receives updates to the
  !! monthly recharge and annual recharge values.
  ! [ ARGUMENTS ]
  type (T_GENERAL_GRID),pointer :: pGrd          ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings

  integer (kind=T_INT), intent(in) :: iDayOfYear
  integer (kind=T_INT), intent(in) :: iDay
  integer (kind=T_INT), intent(in) :: iMonth
  integer (kind=T_INT), intent(in) :: iYear
  ! [ LOCALS ]
  integer (kind=T_INT) :: iCol,iRow,k,l,iTgt_Col,iTgt_Row
  integer (kind=T_INT) :: iNumGridCells
  integer (kind=T_INT) :: iTime
  real (kind=T_SGL) :: rPrevious_Soil_Moisture
  real (kind=T_SGL) :: rPrecipMinusPotentET
  real (kind=T_SGL) :: rMSB_DailyMassBalance
  real (kind=T_SGL) :: rMoistureDeficit
  real (kind=T_SGL) :: rMoistureSurplus
  real (kind=T_SGL) :: rChangeInStorage
  real (kind=T_SGL) :: rNetInfil
  real (kind=T_SGL) :: rNetInflow
  real (kind=T_SGL) :: rDailyRecharge
  real (kind=T_SGL) :: rStreamCapture
  real (kind=T_SGL) :: rDailyRejectedRecharge
  real (kind=T_SGL) :: rSM_ActualET
  real (kind=T_SGL) :: rMin, rMean, rMax, rSum
  integer (kind=T_INT) :: iRowCount
  integer (kind=T_INT) :: iRowCounter

  type ( T_CELL ),pointer :: cel
  character (len=256) :: sBuf

  !! initialize basic grid cell variables

  iNumGridCells = pGrd%iNX * pGrd%iNY

  ! call to "julian_day" includes the optional origin term...
  ! return value will be the number of days *SINCE* that origin term
  iTime = julian_day( iYear, iMonth, iDay, pConfig%iStartJulianDay)

  iRowCounter = 0

  ! array is traversed in column-major order (i.e. processed a column
  ! at a time, which should be more efficient in terms of Fortran
  ! memory management)
  row_idx: do iRow=1,pGrd%iNY
    col_idx: do iCol=1,pGrd%iNX
      cel => pGrd%Cells(iCol,iRow)
      iRowCounter = iRowCounter + 1

      ! calculate net infiltration
      rNetInfil = MAX(rZERO, &
                    cel%rNetPrecip &
                  + cel%rSnowMelt &
#ifdef IRRIGATION_MODULE
                  + cel%rIrrigationAmount &
#endif
                  + cel%rInFlow &
                  - cel%rOutFlow &
#ifdef STREAM_INTERACTIONS
                  - cel%rStreamCapture &
#endif
                  - cel%rFlowOutOfGrid)

      ! calculate net inflow to cell
      rNetInflow = MAX(rZERO, &
                    cel%rNetPrecip &
                  + cel%rSnowMelt &
#ifdef IRRIGATION_MODULE
                  + cel%rIrrigationAmount &
#endif
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
      rPrecipMinusPotentET = rNetInfil &
                                  - cel%rSM_PotentialET

	  MAIN: if(cel%rSoilWaterCap <= rNear_ZERO &
!#ifdef STREAM_INTERACTIONS
            ! treat "stream" cells as open water
            ! (SMW: do we want to do this? We're assuming that the
            ! grid resolution is such that streams are represented at
            ! a reasonable scale... in other words, if we have a
            ! *large* grid cell size (~ 1 mile on a side),
            ! it would not be reasonable to treat the underlying grid
            ! cell as open water.
!            .or. cel%iStreamIndex /= 0 &

!#endif
            .or. cel%iLandUse == pConfig%iOPEN_WATER_LU) then

         ! Soil Water Capacity <= 0; OPEN WATER CELLS

          if(rPrecipMinusPotentET <= rZERO) then
                            ! Precip - Potential ET <= 0

            ! all water that comes in as "infiltration" evaporates
            rSM_ActualET = rNetInfil

            ! any outflow that has not already been routed elsewhere
            ! is rerouted to flow out of grid
            cel%rFlowOutOfGrid = cel%rFlowOutOfGrid + cel%rOutflow

          else  ! code block L3a: Precip - Potential ET > 0

            ! all water that comes in evaporates
            ! or leaves the grid via surface water features
            rSM_ActualET = cel%rSM_PotentialET
            cel%rFlowOutOfGrid = cel%rFlowOutOfGrid &
                              + rNetInfil - rSM_ActualET &
                              + cel%rOutflow
         end if

        cel%rOutflow = rZERO
        rDailyRecharge = rZERO
        cel%rDailyRecharge = rZERO
        cel%rSoilMoisture = rZERO
        rMoistureDeficit = rZERO
        rMoistureSurplus = rZERO
        cel%rSM_AccumPotentWatLoss = rZERO
        rChangeInStorage = rZERO
        rDailyRejectedRecharge = rZERO

        if(rSM_ActualET < rZERO) then
          write(UNIT=LU_LOG,FMT=*)"Negative ActET at grid cell (",iCol,",",iRow,"); Landuse: ", &
            cel%iLandUse, "   Soil Type: ", cel%iSoilGroup, &
            "   AWC: ", cel%rSoilWaterCap
          write(UNIT=LU_LOG,FMT=*) "cel%rNetPrecip: ",cel%rNetPrecip
          write(UNIT=LU_LOG,FMT=*) "cel%rSnowMelt: ",cel%rSnowMelt
#ifdef IRRIGATION_MODULE
          write(UNIT=LU_LOG,FMT=*) "cel%rIrrigationAmount: ",cel%rIrrigationAmount
#endif
          write(UNIT=LU_LOG,FMT=*) "cel%rInFlow: ",cel%rInFlow
          write(UNIT=LU_LOG,FMT=*) "cel%rOutFlow: ",cel%rOutFlow
          write(UNIT=LU_LOG,FMT=*) "cel%rFlowOutOfGrid: ",cel%rFlowOutOfGrid
          write(UNIT=LU_LOG,FMT=*) "==> NET INFIL: ",rNetInfil
          write(UNIT=LU_LOG,FMT=*) "    ActET: ",rSM_ActualET
          write(UNIT=LU_LOG,FMT=*) "    PotET: ",cel%rSM_PotentialET

        end if

      else ! MAIN: Execute remainder of block ONLY if non-WATER cell

        ! Zero out current estimate for daily recharge, rejected recharge
        rDailyRecharge = rZERO
        cel%rDailyRecharge = rZERO
        rDailyRejectedRecharge = rZERO

        rPrevious_Soil_Moisture = cel%rSoilMoisture
        ! the following code block (L1) calculates current soil moisture values
        ! and updates the daily soil moisture deficit or surplus terms...
        ! any moisture deficit adds to the accumulated potential water loss term

        L1: if(rPrecipMinusPotentET <= rZERO) then  ! Precip - Potential ET <= 0

          ! **precipitation FAILS to meet ET demands....**
	      ! add precip minus potential ET term to accumulated
          ! potential water loss

          cel%rSM_AccumPotentWatLoss = MAX(APWL_Cap, &
                                     (cel%rSM_AccumPotentWatLoss &
                                     + rPrecipMinusPotentET))

          rMoistureDeficit = rPrecipMinusPotentET
          rMoistureSurplus = rZERO

          ! determine soil moisture given updated accumulated potential water loss
          L1a: if ( cel%rSoilWaterCap > rNEAR_ZERO) then

#ifdef TM_TABLE

            ! look up soil moisture in T-M tables
		      cel%rSoilMoisture = grid_Interpolate(gWLT,cel%rSoilWaterCap, &
                cel%rSM_AccumPotentWatLoss)

#else

            ! calculate soil moisture w equation SUMMARIZING T-M tables
   	      cel%rSoilMoisture = sm_thornthwaite_mather_soil_storage( &
		          cel%rSoilWaterCap, cel%rSM_AccumPotentWatLoss)

#endif

               L1b: if(ABS(cel%rSoilMoisture - rPrevious_Soil_Moisture) &
		               > ABS(rPrecipMinusPotentET)) then

                 cel%rSoilMoisture = MAX(rZERO, &
		                                (rPrevious_Soil_Moisture &
                                        + rPrecipMinusPotentET))

  	    		 ! regardless of what Thornthwaite-Mather tables tell us,
		    	 ! we are capping the total soil loss at the value of
             ! precip minus potential ET...   under some conditions
			    ! it seems that the T-M tables dry out the soil at a
			    ! rate that *exceeds* precip minus PET

               end if L1b

          end if L1a

        else	! code block L1: Precip - Potential ET > 0

		    !! **precipitation EXCEEDS ET demands, recharging soil column**
	       !! Precip - Potential ET > 0: add infiltrated water
		    !! directly to the soil moisture term

          rMoistureDeficit = rZERO

		    rMoistureSurplus = MAX (rZERO, &
		                                (cel%rSoilMoisture &
			                    							+ rPrecipMinusPotentET &
                                        - cel%rSoilWaterCap))

          cel%rSoilMoisture = MIN(cel%rSoilWaterCap, &
		                         (cel%rSoilMoisture + &
		                          rPrecipMinusPotentET))

          !! back-calculate new equivalent accumulated potential water loss term
		    !! given current soil moisture

          L1c: if(cel%rSoilWaterCap>rNEAR_ZERO) then

#ifdef TM_TABLE

             ! look up APWL in T-M tables
             cel%rSM_AccumPotentWatLoss = &
                grid_SearchColumn(gWLT,cel%rSoilWaterCap,cel%rSoilMoisture,-rONE)

#else
             ! detemine APWL from an equation
             cel%rSM_AccumPotentWatLoss = &
               sm_thornthwaite_mather_APWL(cel%rSoilWaterCap,cel%rSoilMoisture)

#endif

          end if L1c

	    end if L1

  	    !! calculate change in soil moisture storage
        rChangeInStorage = cel%rSoilMoisture &
	                       - rPrevious_Soil_Moisture

        !! the following code block (L2) updates estimate of ACTUAL ET

        L2: if(rPrecipMinusPotentET <= rZERO) then  ! Precip - Potential ET <= 0

          !! cap actual ET at the estimate for potential ET
          rSM_ActualET = MIN((rNetInfil + ABS(rChangeInStorage)), &
                                 cel%rSM_PotentialET)

		else  ! code block L2: Precip - Potential ET > 0

          rSM_ActualET = cel%rSM_PotentialET

        end if L2

        ! ** CALCULATE RECHARGE
        ! based on SOIL MOISTURE SURPLUS

        rDailyRecharge = rMoistureSurplus
        cel%rDailyRecharge = rMoistureSurplus

!          if(rDailyRecharge > cel%rMaxRecharge) then
!            rDailyRejectedRecharge = rDailyRecharge - cel%rMaxRecharge
!            cel%rDailyRecharge = cel%rMaxRecharge
!            rDailyRecharge = cel%rMaxRecharge
!          end if

      end if MAIN

      ! if calculated recharge exceeds the estimated Kv, cap at that value
      ! and characterize the rest as "rejected recharge"
      if(rDailyRecharge > cel%rMaxRecharge) then
!       if(rDailyRecharge > 1.0E+28) then
        rDailyRejectedRecharge = rDailyRecharge - cel%rMaxRecharge
        cel%rDailyRecharge = cel%rMaxRecharge
        rDailyRecharge = cel%rMaxRecharge

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

      !
      !
      ! *** CALL TO RUN_LENGTH ENCODING ROUTINE.... ****
      !
      !  for each grid cell we must make a call to RLE_writeByte if
      !  we expect to have graphical or gridded output at a later stage
      !                                                                      !

      do k=1,iNUM_VARIABLES

        if(.not. STAT_INFO(k)%lActive) cycle

#ifdef NETCDF_SUPPORT

        if(STAT_INFO(k)%iNetCDFOutput > iNONE ) then

          select case(k)
            case(iGROSS_PRECIP)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                cel%rGrossPrecip, pGrd, iRow, iCol, iTime)
            case(iSNOWFALL_SWE)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                cel%rSnowFall_SWE, pGrd, iRow, iCol, iTime)
            case(iSNOWCOVER)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                cel%rSnowCover, pGrd, iRow, iCol, iTime)
            case(iCFGI)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                cel%rCFGI, pGrd, iRow, iCol, iTime)
            case(iMIN_TEMP)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                cel%rTMin, pGrd, iRow, iCol, iTime)
            case(iMAX_TEMP)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                cel%rTMax, pGrd, iRow, iCol, iTime)
            case(iAVG_TEMP)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                cel%rTAvg, pGrd, iRow, iCol, iTime)
            case(iCHG_IN_SNOW_COV)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                cel%rSnowFall_SWE - cel%rSnowmelt, pGrd, iRow, iCol, iTime)
            case(iSNOWMELT)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                cel%rSnowmelt, pGrd, iRow, iCol, iTime)
            case(iINTERCEPTION)
!
!           ==> STAT_INFO(iINTERCEPTION) is updated at the time
!               interception is calculated in subroutine model_ProcessRain
!
            case(iNET_PRECIP)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                cel%rNetPrecip, pGrd, iRow, iCol, iTime)
            case(iINFLOW)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                cel%rInflow, pGrd, iRow, iCol, iTime)
            case(iOUTFLOW)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                cel%rOutflow, pGrd, iRow, iCol, iTime)
            case(iRUNOFF_OUTSIDE)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                cel%rFlowOutofGrid, pGrd, iRow, iCol, iTime)
            case(iREJECTED_RECHARGE)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                rDailyRejectedRecharge, pGrd, iRow, iCol, iTime)
            case(iNET_INFLOW)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                rNetInflow, pGrd, iRow, iCol, iTime)
            case(iNET_INFIL)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                rNetInfil, pGrd, iRow, iCol, iTime)
            case(iPOT_ET)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                cel%rSM_PotentialET, pGrd, iRow, iCol, iTime)
            case(iACT_ET)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                rSM_ActualET, pGrd, iRow, iCol, iTime)
            case(iP_MINUS_PET)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                rPrecipMinusPotentET, pGrd, iRow, iCol, iTime)
            case(iSM_DEFICIT)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                rMoistureDeficit, pGrd, iRow, iCol, iTime)
            case(iSM_SURPLUS)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                rMoistureSurplus, pGrd, iRow, iCol, iTime)
            case(iSM_APWL)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                cel%rSM_AccumPotentWatLoss, pGrd, iRow, iCol, iTime)
            case(iSOIL_MOISTURE)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                cel%rSoilMoisture, pGrd, iRow, iCol, iTime)
            case(iCHG_IN_SOIL_MOIST)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                rChangeInStorage, pGrd, iRow, iCol, iTime)
            case(iRECHARGE)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                rDailyRecharge, pGrd, iRow, iCol, iTime)
            case(iGDD)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                cel%rGDD, pGrd, iRow, iCol, iTime)
            case(iIRRIGATION)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                cel%rIrrigationAmount, pGrd, iRow, iCol, iTime)
            case(iSTREAM_CAPTURE)
              call netcdf_write_variable_byte(k, iNC_OUTPUT, pConfig, &
                cel%rStreamCapture, pGrd, iRow, iCol, iTime)

            case default
              call Assert(lFALSE, "Internal programming error in " &
                //"select case structure",TRIM(__FILE__),__LINE__)

          end select
        end if

#endif
! end if NETCDF_SUPPORT

        if(STAT_INFO(k)%iDailyOutput > iNONE &
          .or. STAT_INFO(k)%iMonthlyOutput > iNONE &
          .or. STAT_INFO(k)%iAnnualOutput > iNONE )  then

          select case(k)
            case(iGROSS_PRECIP)
              call RLE_writeByte(STAT_INFO(iGROSS_PRECIP)%iLU, &
                cel%rGrossPrecip, pConfig%iRLE_MULT, &
                 pConfig%rRLE_OFFSET, iNumGridCells, iGROSS_PRECIP)
            case(iSNOWFALL_SWE)
              call RLE_writeByte(STAT_INFO(iSNOWFALL_SWE)%iLU, &
                cel%rSnowFall_SWE, pConfig%iRLE_MULT, &
                 pConfig%rRLE_OFFSET, iNumGridCells, iSNOWFALL_SWE)
            case(iSNOWCOVER)
              call RLE_writeByte(STAT_INFO(iSNOWCOVER)%iLU, &
                cel%rSnowCover, pConfig%iRLE_MULT, &
                 pConfig%rRLE_OFFSET, iNumGridCells, iSNOWCOVER)
            case(iCFGI)
              call RLE_writeByte(STAT_INFO(iCFGI)%iLU, &
                cel%rCFGI, pConfig%iRLE_MULT, &
                 pConfig%rRLE_OFFSET, iNumGridCells, iCFGI)
            case(iMIN_TEMP)
              call RLE_writeByte(STAT_INFO(iMIN_TEMP)%iLU, &
                cel%rTMin, pConfig%iRLE_MULT, &
                 pConfig%rRLE_OFFSET, iNumGridCells, iMIN_TEMP)
            case(iMAX_TEMP)
              call RLE_writeByte(STAT_INFO(iMAX_TEMP)%iLU, &
                cel%rTMax, pConfig%iRLE_MULT, &
                 pConfig%rRLE_OFFSET, iNumGridCells, iMAX_TEMP)
            case(iAVG_TEMP)
              call RLE_writeByte(STAT_INFO(iAVG_TEMP)%iLU, &
                cel%rTAvg, pConfig%iRLE_MULT, &
                 pConfig%rRLE_OFFSET, iNumGridCells, iAVG_TEMP)
            case(iCHG_IN_SNOW_COV)
              call RLE_writeByte(STAT_INFO(iCHG_IN_SNOW_COV)%iLU, &
                cel%rSnowFall_SWE-cel%rSnowmelt, &
                 pConfig%iRLE_MULT, pConfig%rRLE_OFFSET, &
                 iNumGridCells, iCHG_IN_SNOW_COV)
            case(iSNOWMELT)
              call RLE_writeByte(STAT_INFO(iSNOWMELT)%iLU, &
                cel%rSnowmelt, pConfig%iRLE_MULT, &
                 pConfig%rRLE_OFFSET, iNumGridCells, iSnowmelt)

            case(iINTERCEPTION)
!
!           ==> STAT_INFO(iINTERCEPTION) is updated at the time
!               interception is calculated in subroutine model_ProcessRain
!
            case(iNET_PRECIP)
              call RLE_writeByte(STAT_INFO(iNET_PRECIP)%iLU, &
                cel%rNetPrecip, pConfig%iRLE_MULT, &
                 pConfig%rRLE_OFFSET, iNumGridCells, iNET_PRECIP)
            case(iINFLOW)
              call RLE_writeByte(STAT_INFO(iINFLOW)%iLU, &
                cel%rInflow, pConfig%iRLE_MULT, &
                 pConfig%rRLE_OFFSET, iNumGridCells, iINFLOW)
            case(iOUTFLOW)
              call RLE_writeByte(STAT_INFO(iOUTFLOW)%iLU, &
                cel%rOutflow, pConfig%iRLE_MULT, &
                 pConfig%rRLE_OFFSET, iNumGridCells, iOUTFLOW)
            case(iRUNOFF_OUTSIDE)
              call RLE_writeByte(STAT_INFO(iRUNOFF_OUTSIDE)%iLU, &
                cel%rFlowOutOfGrid, pConfig%iRLE_MULT, &
                 pConfig%rRLE_OFFSET, iNumGridCells, iRUNOFF_OUTSIDE)
            case(iREJECTED_RECHARGE)
              call RLE_writeByte(STAT_INFO(iREJECTED_RECHARGE)%iLU, &
                rDailyRejectedRecharge, pConfig%iRLE_MULT, &
                 pConfig%rRLE_OFFSET, iNumGridCells, iREJECTED_RECHARGE)
            case(iNET_INFLOW)
              call RLE_writeByte(STAT_INFO(iNET_INFLOW)%iLU, &
                rNetInflow, pConfig%iRLE_MULT, &
                 pConfig%rRLE_OFFSET, iNumGridCells, iNET_INFLOW)
            case(iNET_INFIL)
              call RLE_writeByte(STAT_INFO(iNET_INFIL)%iLU, &
                rNetInfil, pConfig%iRLE_MULT, &
                 pConfig%rRLE_OFFSET, iNumGridCells, iNET_INFIL)
            case(iPOT_ET)
              call RLE_writeByte(STAT_INFO(iPOT_ET)%iLU, &
                cel%rSM_PotentialET, pConfig%iRLE_MULT, &
                 pConfig%rRLE_OFFSET, iNumGridCells, iPOT_ET)
            case(iACT_ET)
              call RLE_writeByte(STAT_INFO(iACT_ET)%iLU, &
                rSM_ActualET, pConfig%iRLE_MULT, &
                 pConfig%rRLE_OFFSET, iNumGridCells, iACT_ET)
            case(iP_MINUS_PET)
              call RLE_writeByte(STAT_INFO(iP_MINUS_PET)%iLU, &
                rPrecipMinusPotentET, pConfig%iRLE_MULT, &
                 pConfig%rRLE_OFFSET, iNumGridCells, iP_MINUS_PET)
            case(iSM_DEFICIT)
              call RLE_writeByte(STAT_INFO(iSM_DEFICIT)%iLU, &
                rMoistureDeficit, pConfig%iRLE_MULT, &
                 pConfig%rRLE_OFFSET, iNumGridCells, iSM_DEFICIT)
            case(iSM_SURPLUS)
              call RLE_writeByte(STAT_INFO(iSM_SURPLUS)%iLU, &
                rMoistureSurplus, pConfig%iRLE_MULT, &
                 pConfig%rRLE_OFFSET, iNumGridCells, iSM_SURPLUS)
            case(iSM_APWL)
              call RLE_writeByte(STAT_INFO(iSM_APWL)%iLU, &
                cel%rSM_AccumPotentWatLoss, pConfig%iRLE_MULT, &
                 pConfig%rRLE_OFFSET, iNumGridCells, iSM_APWL)
            case(iSOIL_MOISTURE)
              call RLE_writeByte(STAT_INFO(iSOIL_MOISTURE)%iLU, &
                  cel%rSoilMoisture, pConfig%iRLE_MULT, &
                   pConfig%rRLE_OFFSET, iNumGridCells, iSOIL_MOISTURE)
            case(iCHG_IN_SOIL_MOIST)
              call RLE_writeByte(STAT_INFO(iCHG_IN_SOIL_MOIST)%iLU, &
                  rChangeInStorage, pConfig%iRLE_MULT, &
                   pConfig%rRLE_OFFSET, iNumGridCells, iCHG_IN_SOIL_MOIST)
            case(iRECHARGE)
              call RLE_writeByte(STAT_INFO(iRECHARGE)%iLU, &
                  rDailyRecharge, pConfig%iRLE_MULT, &
                   pConfig%rRLE_OFFSET, iNumGridCells, iRECHARGE)
            case(iGDD)
              call RLE_writeByte(STAT_INFO(iGDD)%iLU, &
                  cel%rGDD, pConfig%iRLE_MULT, &
                   pConfig%rRLE_OFFSET, iNumGridCells, iGDD)
            case(iIRRIGATION)
              call RLE_writeByte(STAT_INFO(iIRRIGATION)%iLU, &
                  cel%rIrrigationAmount, pConfig%iRLE_MULT, &
                   pConfig%rRLE_OFFSET, iNumGridCells, iIRRIGATION)


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
                   pConfig%rRLE_OFFSET, iNumGridCells, iSTREAM_CAPTURE)

            case default
              call Assert(lFALSE, "Internal programming error in " &
                //"select case structure",TRIM(__FILE__),__LINE__)

          end select
        end if

      end do   ! end loop over STAT_INFO variables

      ! write out SSF files, if requested
      if( cel%iNumFilesSSF > 0)  then

        do l=1,size(pConfig%SSF_FILES)

          if(pConfig%SSF_FILES(l)%iRowNum == iRow &
             .and. pConfig%SSF_FILES(l)%iColNum == iCol) then

            k = pConfig%SSF_FILES(l)%iVarNum

            select case(k)
              case(iGROSS_PRECIP)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, cel%rGrossPrecip)
              case(iSNOWFALL_SWE)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, cel%rSnowFall_SWE)
              case(iSNOWCOVER)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, cel%rSnowCover)
              case(iCFGI)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, cel%rCFGI)
              case(iMIN_TEMP)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, cel%rTMin)
              case(iMAX_TEMP)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, cel%rTMax)
              case(iAVG_TEMP)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, cel%rTAvg)
              case(iCHG_IN_SNOW_COV)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, cel%rSnowFall_SWE-cel%rSnowmelt)
              case(iSNOWMELT)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, cel%rSnowmelt)

              case(iINTERCEPTION)
!
!           ==> STAT_INFO(iINTERCEPTION) is updated at the time
!               interception is calculated in subroutine model_ProcessRain
!
              case(iNET_PRECIP)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, cel%rNetPrecip)
              case(iINFLOW)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, cel%rInflow)
              case(iOUTFLOW)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, cel%rOutflow)
              case(iRUNOFF_OUTSIDE)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, cel%rFlowOutOfGrid)
              case(iREJECTED_RECHARGE)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, rDailyRejectedRecharge)
              case(iNET_INFLOW)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, rNetInflow)
                  case(iNET_INFIL)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, rNetInfil)
              case(iPOT_ET)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, cel%rSM_PotentialET)
              case(iACT_ET)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, rSM_ActualET)
              case(iP_MINUS_PET)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, rPrecipMinusPotentET)
              case(iSM_DEFICIT)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, rMoistureDeficit)
              case(iSM_SURPLUS)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, rMoistureSurplus)
              case(iSM_APWL)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, cel%rSM_AccumPotentWatLoss)
              case(iSOIL_MOISTURE)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, cel%rSoilMoisture)
              case(iCHG_IN_SOIL_MOIST)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, rChangeInStorage)
              case(iRECHARGE)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, rDailyRecharge)
              case(iGDD)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, cel%rGDD)
              case(iIRRIGATION)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, cel%rIrrigationAmount)
              case(iSTREAM_CAPTURE)
                call stats_write_to_SSF_file(pConfig, l, iMonth, iDay, &
                    iYear, cel%rStreamCapture)

            case default
              call Assert(lFALSE, "Internal programming error in " &
                //"select case structure",TRIM(__FILE__),__LINE__)

            end select
          end if
        end do
      end if


!      end do   ! end loop over STAT_INFO variables

      ! UPDATE MONTHLY and ANNUAL ACCUMULATORS

 	  if(cel%rSoilWaterCap > rNear_ZERO) then

        ! update soil moisture as a percentage of soil water capacity
        cel%rSoilMoisturePct = cel%rSoilMoisture / cel%rSoilWaterCap * rHUNDRED

      else
        cel%rSoilMoisturePct = rZERO
	  end if

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

      call stats_UpdateAllAccumulatorsByCell(REAL(cel%rSM_PotentialET,kind=T_DBL), &
           iPOT_ET,iMonth,iZERO)

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

#ifdef IRRIGATION_MODULE

      call stats_UpdateAllAccumulatorsByCell(REAL(cel%rGDD,kind=T_DBL), &
           iGDD,iMonth,iZERO)
      call stats_UpdateAllAccumulatorsByCell(REAL(cel%rIrrigationAmount,kind=T_DBL), &
           iIRRIGATION,iMonth,iZERO)
#endif

      if(iYear>= pConfig%iStartYearforCalculation .and. &
           iYear<= pConfig%iEndYearforCalculation) then

        cel%rSUM_Recharge = cel%rSUM_Recharge + rDailyRecharge
        cel%rSUM_RejectedRecharge = cel%rSUM_RejectedRecharge + rDailyRejectedRecharge

      end if

      cel%rMSB = cel%rNetPrecip &
                 + cel%rSnowMelt &
                 + cel%rInFlow &
#ifdef IRRIGATION_MODULE
                 + cel%rIrrigationAmount &
#endif
                 - cel%rOutFlow &
#ifdef STREAM_INTERACTIONS
                 - cel%rStreamCapture &
#endif
                 - cel%rFlowOutOfGrid &
                 - rChangeInStorage &
                 - rSM_ActualET &
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

                 write(UNIT=LU_LOG,FMT="(/,'  MASS BALANCE: ',t32,F14.4)") &
                     cel%rMSB

                 write(UNIT=LU_LOG,FMT="(/,'  (+) cel%rNetPrecip: ',t32,F14.4)") &
                     cel%rNetPrecip
                 write(UNIT=LU_LOG,FMT="('  (+) cel%rSnowMelt: ',t32,F14.4)") &
                     cel%rSnowMelt
#ifdef IRRIGATION_MODULE
                 write(UNIT=LU_LOG,FMT="('  (+) cel%rIrrigationAmount: ',t32,F14.4)") &
                     cel%rIrrigationAmount
#endif
                 write(UNIT=LU_LOG,FMT="('  (+) cel%rInFlow: ',t32,F14.4)") &
                     cel%rInFlow
                 write(UNIT=LU_LOG,FMT="('  (-) cel%rOutFlow: ',t32,F14.4)") &
                     cel%rOutFlow
                 write(UNIT=LU_LOG,FMT="('  (-) cel%rFlowOutOfGrid: ',t32,F14.4)") &
                     cel%rFlowOutOfGrid
                 write(UNIT=LU_LOG,FMT="('  (-) rChangeInStorage: ',t32,F14.4)") &
                     rChangeInStorage
                 write(UNIT=LU_LOG,FMT="('  (-) rSM_ActualET: ',t32,F14.4)") &
                     rSM_ActualET
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

                 write(UNIT=LU_LOG,FMT=*) "-----------------------------------------------"
                 write(UNIT=LU_LOG,FMT=*)

      endif

    end do col_idx

  end do row_idx

  ! now issue the following calls to trigger calculation of daily averages
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
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iP_MINUS_PET,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iPOT_ET,iMonth,iNumGridCells)
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

#ifdef IRRIGATION_MODULE
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iGDD, &
      iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO, iIRRIGATION, &
      iMonth,iNumGridCells)
#endif

  return
end subroutine sm_thornthwaite_mather_UpdateSM

!----------------------------------------------------------------------

function sm_thornthwaite_mather_soil_storage(rSWC, rAPWL)  result(rValue)

  real (kind=T_SGL), intent(in) :: rSWC     ! max soil-water capacity (inches)
  real (kind=T_SGL), intent(in) :: rAPWL    ! accum pot. water loss (inches)

  real (kind=T_SGL) :: rValue

  ! equation as implemented in R;
  ! sm.df$y = maximum soil-water capacity
  ! sm.df$x = APWL
  ! 10^(log10(sm.df$y) - (s.opt[[1]]*sm.df$y^e.opt[[1]]) * sm.df$x)

  rValue = rZERO

  if(rSWC > rZERO ) &

    rValue = 10** ( log10(REAL(rSWC,kind=T_DBL)) - &
              ( ABS(REAL(rAPWL,kind=T_DBL)) * rTM_slope_term * rSWC ** rTM_exp_term ) )


end function sm_thornthwaite_mather_soil_storage

function sm_thornthwaite_mather_APWL(rSWC, rSoilStorage)  result(rValue)

  real (kind=T_SGL), intent(in) :: rSWC          ! max soil-water capacity (inches)
  real (kind=T_SGL), intent(in) :: rSoilStorage  ! curr soil storage (inches)

  real (kind=T_SGL) :: rValue

  ! equation as implemented in R;
  ! sm.df$y = maximum soil-water capacity
  ! sm.df$x = APWL
  ! (log10(sm.df$y) - log10(sm.df$pred)) / (s.opt[[1]] * sm.df$y^e.opt[[1]])

rValue = rZERO

  if(rSWC > rZERO .and. rSoilStorage > rZERO) &

    rValue = -( log10(REAL(rSWC,kind=T_DBL)) - log10(REAL(rSoilStorage,kind=T_DBL))) / &
          ( rTM_slope_term * REAL(rSWC,kind=T_DBL)**rTM_exp_term )

end function sm_thornthwaite_mather_APWL

!------------------------------------------------------------------------------

subroutine sm_thornthwaite_mather_UpdatePctSM( pGrd )

  type ( T_GENERAL_GRID ),pointer :: pGrd

  where(pGrd%Cells%rSoilWaterCap > rNEAR_ZERO )

    pGrd%Cells%rSoilMoisturePct = pGrd%Cells%rSoilMoisture  &
       / pGrd%Cells%rSoilWaterCap * 100.

  elsewhere

    pGrd%Cells%rSoilMoisturePct = rZERO

  endwhere

end subroutine sm_thornthwaite_mather_UpdatePctSM

!------------------------------------------------------------------------------

end module sm_thornthwaite_mather
