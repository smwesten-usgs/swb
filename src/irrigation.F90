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
  integer (kind=c_int) :: iRow, iCol
  integer (kind=c_int) :: iNumCells
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  type ( T_CELL ),pointer :: cel
  real (kind=c_float) :: rDepletionFraction
  real (kind=c_double) :: rDepletionAmount
  real (kind=c_double) :: rIrrigationAmount

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

            !> NEW as of 4/23/2014: irrigation amount can either be the amount
            !! of the current soil moisture deficit *or* a specified maximum
            !! daily amount
            if (pIRRIGATION%rIrrigationAmount <= rNEAR_ZERO) then
              rIrrigationAmount = cel%rSoilWaterCap - cel%rSoilMoisture
            else
              rIrrigationAmount = pIRRIGATION%rIrrigationAmount
            endif

            cel%rIrrigationFromGW = REAL(pIRRIGATION%rFractionOfIrrigationFromGW &
                                        * rIrrigationAmount, kind=c_double )

            cel%rIrrigationFromSW = real((1.0 - pIRRIGATION%rFractionOfIrrigationFromGW) &
                                        * rIrrigationAmount, kind=c_double )

            !> @todo Must difinitively figure out what to do with water that
            !! is calculated to be used as part of the inefficiency in the
            !! delivery system.

            ! rIrrigationAmount is the value that actually enters the mass balance
            ! NOTE!! Currently we are assuming that the amounts from GW and SW are the amounts a grower
            !        would estimate based on pumping rates and times; it is assumed that the inefficiencies
            !        in delivery result in water that bypasses the root zone.
            cel%rIrrigationAmount = cel%rIrrigationFromGW * REAL(pIRRIGATION%rIrrigationEfficiency_GW, kind=c_double ) &
                                  + cel%rIrrigationFromSW * REAL(pIRRIGATION%rIrrigationEfficiency_SW, kind=c_double )

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
