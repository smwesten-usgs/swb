!> @file
!>  Contains a single module, \ref irrigation, which
!>  provides support for estimating irrigation amounts

!> Provides support for assessing the effect of irrigation on recharge
!> values by estimating the irrigation required to maintain soil moisture levels
!> for specific crop types.
module irrigation

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use types
  use data_factory

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
  integer (c_int) :: iRow, iCol
  integer (c_int) :: iNumCells
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  type ( T_CELL ),pointer :: cel
  real (c_float) :: rDepletionFraction
  real (c_double) :: rDepletionAmount
  real (c_double) :: rIrrigationAmount

    ! zero out Irrigation term
    pGrd%Cells%rIrrigationFromGW = rZERO
    pGrd%Cells%rIrrigationFromSW = rZERO
    pGrd%Cells%rIrrigationAmount = rZERO

    call DAT( IRRIGATED_LAND_MASK_DATA )%getvalues( pGrdBase=pGrd)

    associate ( IRRIGATION_LAND_MASK => pGrd%iData )

      ! iterate over cells; add water if soil storage zone is below the
      ! maximum allowable depletion
      do iRow=1,pGrd%iNY
        do iCol=1,pGrd%iNX  ! last index in a Fortan array should be the slowest changing
          cel => pGrd%Cells(iCol,iRow)

          if (pGrd%iMask(iCol, iRow) == iINACTIVE_CELL) cycle

          ! now we run the gauntlet of tests to ensure that we really need
          ! to perform all of the irrigation calculations

          pIRRIGATION => pConfig%IRRIGATION(cel%iLandUseIndex)

          if (      pConfig%iDayOfYear < pIRRIGATION%iBeginIrrigation   &
               .or. pConfig%iDayOfYear > pIRRIGATION%iEndIrrigation )       cycle

          if (      pIRRIGATION%rMAD > 0.99                     &
               .or. cel%rSoilWaterCap < rNEAR_ZERO              &
               .or. IRRIGATION_LAND_MASK( iCol, iRow)  /= 1 )               cycle

          ! cell is active and irrigation enabled and in season
          rDepletionFraction = min((cel%rSoilWaterCap - cel%rSoilMoisture) &
                                 / cel%rTotalAvailableWater, 1.0)

          if(rDepletionFraction > pIRRIGATION%rMAD .and. cel%rGDD > 50 ) then



    ! code snippet from SWB, 8/15/2013
    ! ! NOTE: we are assuming that the inefficiencies in delivery are *not*
    !       ! added to the soil moisture reservoir
    !       cel%rIrrigationAmount = cel%rIrrigationFromGW + cel%rIrrigationFromSW

    !       cel%rIrrigationFromGW = cel%rIrrigationFromGW &
    !          * REAL(pIRRIGATION%rIrrigationEfficiency_GW, c_double )
    !       cel%rIrrigationFromSW = cel%rIrrigationFromSW &
    !          * real(pIRRIGATION%rIrrigationEfficiency_SW, c_double )

        ! code snippet from SWB, 8/30/2013
        ! if(rDepletionFraction > pIRRIGATION%rMAD .and. cel%rGDD > 50 ) then
        !   rDepletionAmount = cel%rSoilWaterCap - cel%rSoilMoisture
        !   cel%rIrrigationFromGW = REAL(pIRRIGATION%rFractionOfIrrigationFromGW &
        !                               * rDepletionAmount, c_double )

        !   cel%rIrrigationFromSW = real((1.0 - pIRRIGATION%rFractionOfIrrigationFromGW) &
        !                               * rDepletionAmount, c_double )

        !   !> NOTE!! Formerly the assumption was that any inefficiencies in
        !   !> delivery were *lost* altogether from the system; changed
        !   !> as of 30 AUG 2013 so that the extra water is instead applied
        !   !> to the soil moisture reservoir
        !   cel%rIrrigationFromGW = cel%rIrrigationFromGW &
        !      * REAL(pIRRIGATION%rIrrigationEfficiency_GW, c_double )
        !   cel%rIrrigationFromSW = cel%rIrrigationFromSW &
        !      * real(pIRRIGATION%rIrrigationEfficiency_SW, c_double )

        !   cel%rIrrigationAmount = cel%rIrrigationFromGW + cel%rIrrigationFromSW


            if ( pIRRIGATION%iApplication_Scheme == CONFIG_IRRIGATION_APPLICATION_FIELD_CAPACITY ) then

              !! Replenish to field capacity: add all moisture to soil reservoir such that the deficit is
              !!                              completely eliminated; adjust GW and SW irrigation figures
              !!                              upward to adjust for any inefficiencies in water delivery.

              ! calculate amount of water needed to replenish deficit completely
              rDepletionAmount = cel%rSoilWaterCap - cel%rSoilMoisture

              ! assume water associated with inefficient delivery and application is LOST from root zone
              cel%rIrrigationAmount = rDepletionAmount

              ! inflate the GW and SW irrigation amounts to account for inefficient delivery and application
              cel%rIrrigationFromGW = pIRRIGATION%rFractionOfIrrigationFromGW * rDepletionAmount                          &
                                          / REAL(pIRRIGATION%rIrrigationEfficiency_GW, c_double )

              cel%rIrrigationFromSW = (1.0_c_double - pIRRIGATION%rFractionOfIrrigationFromGW) * rDepletionAmount    &
                                          / REAL(pIRRIGATION%rIrrigationEfficiency_SW, c_double )

            elseif ( pIRRIGATION%iApplication_Scheme == CONFIG_IRRIGATION_APPLICATION_FIELD_CAPACITY_RZ ) then

              !! Replenish to field capacity: add all moisture to soil reservoir such that the deficit is
              !!                              completely eliminated; adjust GW and SW irrigation figures
              !!                              upward to adjust for any inefficiencies in water delivery.
              !!                              additional water owing to inefficiencies is *ADDED* to root zone.

              ! calculate amount of water needed to replenish deficit completely
              rDepletionAmount = cel%rSoilWaterCap - cel%rSoilMoisture

              ! inflate the GW and SW irrigation amounts to account for inefficient delivery and application
              cel%rIrrigationFromGW = pIRRIGATION%rFractionOfIrrigationFromGW * rDepletionAmount                          &
                                          / REAL(pIRRIGATION%rIrrigationEfficiency_GW, c_double )

              cel%rIrrigationFromSW = (1.0_c_double - pIRRIGATION%rFractionOfIrrigationFromGW) * rDepletionAmount    &
                                          / REAL(pIRRIGATION%rIrrigationEfficiency_SW, c_double )

              ! assume that this inflated amount of water makes it to the root zone as well.
              cel%rIrrigationAmount = cel%rIrrigationFromSW + cel%rIrrigationFromGW

            elseif ( pIRRIGATION%iApplication_Scheme == CONFIG_IRRIGATION_APPLICATION_CONSTANT_AMNT ) then

              !! Defined amount: assume we know the total amount of water applied; adjust amount that
              !!                 gets to the root zone by efficiency fraction.

              cel%rIrrigationFromGW = REAL(pIRRIGATION%rFractionOfIrrigationFromGW                        &
                                          * pIRRIGATION%rIrrigationAmount, c_double )

              cel%rIrrigationFromSW = real((1.0 - pIRRIGATION%rFractionOfIrrigationFromGW)                &
                                          * pIRRIGATION%rIrrigationAmount, c_double )

              ! ** not all water associated with the APPLICATION AMOUNT is assumed to make it to the root zone
              cel%rIrrigationAmount =   cel%rIrrigationFromGW                                                   &
                                          * REAL(pIRRIGATION%rIrrigationEfficiency_GW, c_double )          &
                                       +    cel%rIrrigationFromSW                                               &
                                          * REAL(pIRRIGATION%rIrrigationEfficiency_SW, c_double )

            else
              cel%rIrrigationAmount = rZERO
              cel%rIrrigationFromGW = rZERO
              cel%rIrrigationFromSW = rZERO
            endif

            !! @NOTE: it is unlikely that the standard published "application efficiency" numbers are applicable
            !!        here. SWB will calculate evaporative, runoff, and deep percolation losses. Efficiencies
            !!        supplied to SWB should be close to 1.0 unless there is a really good reason for them to
            !!        be less...

          else

            cel%rIrrigationAmount = rZERO
            cel%rIrrigationFromGW = rZERO
            cel%rIrrigationFromSW = rZERO

          endif

        enddo  ! loop over columns
      enddo  ! loop over rows

    end associate

end subroutine irrigation_UpdateAmounts

end module irrigation
