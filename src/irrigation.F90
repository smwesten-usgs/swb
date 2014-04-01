!> @file
!>  Contains a single module, \ref irrigation, which
!>  provides support for estimating irrigation amounts

!> Provides support for assessing the effect of irrigation on recharge
!> values by estimating the irrigation required to maintain soil moisture levels
!> for specific crop types.
module irrigation

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use types

  implicit none

  contains

!> Estimate the irrigation water required to sustain plant growth.
!!
!! Estimate the irrigation water required in order to
!! keep soil moisture values above the maximum allowable depletion (MAD)
!! for each gridcell.
!!
!! @param[inout] pGrd Pointer to the model grid object
!! @param[in] pConfig Pointer to the configuration data structure (type T_CONFIG).
!!
!! @note The routine et_kc_UpdateCropCoefficient must be called before
!! a call to this subroutine is made in order to ensure that crop coefficients
!! and rooting depths are up-to-date.
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

          !> @todo Must difinitively figure out what to do with water that
          !! is calculated to be used as part of the inefficiency in the
          !! delivery system.

          ! rIrrigationAmount is the value that actually enters the mass balance
!          cel%rIrrigationAmount = cel%rIrrigationFromGW + cel%rIrrigationFromSW

          ! if rIrrigationAmount is calculated here, the extra water reported owing to
          ! inefficiencies is not actually applied anywhere

          ! assume that the inefficiencies in the delivery system do *not*
          ! enter the root zone, as per CROPWAT
          cel%rIrrigationFromGW = cel%rIrrigationFromGW &
             * REAL(pIRRIGATION%rIrrigationEfficiency_GW, kind=c_double )
          cel%rIrrigationFromSW = cel%rIrrigationFromSW &
             * real(pIRRIGATION%rIrrigationEfficiency_SW, kind=c_double )

          ! rIrrigationAmount is the value that actually enters the mass balance
          cel%rIrrigationAmount = cel%rIrrigationFromGW + cel%rIrrigationFromSW

          ! if rIrrigationAmount is calculated here, the extra water reported owing to
          ! inefficiencies **is assumed to make it into the root zone**


        else
          cel%rIrrigationAmount = rZERO
          cel%rIrrigationFromGW = rZERO
          cel%rIrrigationFromSW = rZERO
        endif

      enddo  ! loop over columns
    enddo  ! loop over rows

end subroutine irrigation_UpdateAmounts

end module irrigation
