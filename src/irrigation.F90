!> @file
!> @brief  Contains a single module, \ref irrigation, which
!>  provides support for estimating irrigation amounts

!> @brief  Provides support for assessing the effect of irrigation on recharge
!> values by estimating the irrigation required to maintain soil moisture levels
!> for specific crop types.
module irrigation

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
  integer (kind=T_INT) :: iRow, iCol
  integer (kind=T_INT) :: iNumCells
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  type ( T_CELL ),pointer :: cel
  real (kind=T_SGL) :: rPercentDepletion

    ! zero out Irrigation term
    pGrd%Cells%rIrrigationFromGW = rZERO
    pGrd%Cells%rIrrigationFromSW = rZERO

    ! iterate over cells; add water if soil storage zone is below the
    ! maximum allowable depletion
    do iRow=1,pGrd%iNY
      do iCol=1,pGrd%iNX  ! last index in a Fortan array should be the slowest changing
        cel => pGrd%Cells(iCol,iRow)

        ! now we run the gauntlet of tests to ensure that we really need
        ! to perform all of the irrigation calculations
        if ( cel%iActive == iINACTIVE_CELL ) cycle

        pIRRIGATION => pConfig%IRRIGATION(cel%iLandUseIndex)
        if(pConfig%iDayOfYear < pIRRIGATION%iBeginIrrigation &
          .or. pConfig%iDayOfYear > pIRRIGATION%iEndIrrigation ) cycle

        if( pIRRIGATION%rMAD > 99.9 .or. cel%rSoilWaterCap < rNEAR_ZERO ) cycle

        ! cell is active and irrigation enabled and in season
        rPercentDepletion = 100_T_SGL - cel%rSoilMoisturePct

        if(rPercentDepletion > pIRRIGATION%rMAD .and. cel%rGDD > 50 ) then
          cel%rIrrigationAmount = (cel%rSoilWaterCap - cel%rSoilMoisture)
        else
          cel%rIrrigationAmount = rZERO
        endif

      enddo  ! loop over columns
    enddo  ! loop over rows

end subroutine irrigation_UpdateAmounts

end module irrigation
