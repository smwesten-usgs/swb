!> @file
!> @brief  Contains a single module, \ref irrigation, which
!>  provides support for estimating irrigation amounts

!> @brief  Provides support for assessing the effect of irrigation on recharge
!> values by estimating the irrigation required to maintain soil moisture levels
!> for specific crop types.
module irrigation

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use types

  implicit none

  contains

!> @brief This subroutine estimates the amount of water required
!> to keep soil moisture values above the maximum allowable depletion (MAD)
!> for each gridcell.
subroutine irrigation_UpdateAmounts(pGrd, pConfig)
!
!  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other setting

  ! [ LOCALS ]
  integer (kind=c_int) :: iRow, iCol
  integer (kind=c_int) :: iNumCells
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  type ( T_CELL ),pointer :: cel
  real (kind=c_float) :: rDepletionFraction
  real (kind=c_double) :: rDepletionAmount

    ! zero out Irrigation term
    pGrd%Cells%rIrrigationFromGW = rZERO
    pGrd%Cells%rIrrigationFromSW = rZERO
    pGrd%Cells%rIrrigationAmount = rZERO

    ! iterate over cells; add water if soil storage zone is below the
    ! maximum allowable depletion
    do iRow=1,pGrd%iNY
      do iCol=1,pGrd%iNX  ! last index in a Fortan array should be the slowest changing
        cel => pGrd%Cells(iCol,iRow)

        if (pGrd%iMask(iCol, iRow) == iINACTIVE_CELL) cycle

        ! now we run the gauntlet of tests to ensure that we really need
        ! to perform all of the irrigation calculations

        pIRRIGATION => pConfig%IRRIGATION(cel%iLandUseIndex)
        if(pConfig%iDayOfYear < pIRRIGATION%iBeginIrrigation &
          .or. pConfig%iDayOfYear > pIRRIGATION%iEndIrrigation ) cycle

        if( pIRRIGATION%rMAD > 0.99 .or. cel%rSoilWaterCap < rNEAR_ZERO ) cycle

        ! cell is active and irrigation enabled and in season
        rDepletionFraction = min((cel%rSoilWaterCap - cel%rSoilMoisture) &
                               / cel%rTotalAvailableWater, 1.0)

        if(rDepletionFraction > pIRRIGATION%rMAD .and. cel%rGDD > 50 ) then
          rDepletionAmount = cel%rSoilWaterCap - cel%rSoilMoisture
          cel%rIrrigationFromGW = REAL(pIRRIGATION%rFractionOfIrrigationFromGW &
                                      * rDepletionAmount, kind=c_double )

          cel%rIrrigationFromSW = real((1.0 - pIRRIGATION%rFractionOfIrrigationFromGW) &
                                      * rDepletionAmount, kind=c_double )

          !> NOTE!! Formerly the assumption was that any inefficiencies in
          !> delivery were *lost* altogether from the system; changed
          !> as of 30 AUG 2013 so that the extra water is instead applied
          !> to the soil moisture reservoir
          cel%rIrrigationFromGW = cel%rIrrigationFromGW &
             * REAL(pIRRIGATION%rIrrigationEfficiency_GW, kind=c_double )
          cel%rIrrigationFromSW = cel%rIrrigationFromSW &
             * real(pIRRIGATION%rIrrigationEfficiency_SW, kind=c_double )

          cel%rIrrigationAmount = cel%rIrrigationFromGW + cel%rIrrigationFromSW

        else
          cel%rIrrigationAmount = rZERO
          cel%rIrrigationFromGW = rZERO
          cel%rIrrigationFromSW = rZERO
        endif

      enddo  ! loop over columns
    enddo  ! loop over rows

end subroutine irrigation_UpdateAmounts

end module irrigation
