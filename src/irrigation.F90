!> @file
!> @brief  Contains a single module, \ref irrigation, which
!>  provides support for estimating irrigation amounts

!> @brief  Provides support for assessing the effect of irrigation on recharge
!> values by estimating the irrigation required to maintain soil moisture levels
!> for specific crop types.
module irrigation

#ifdef IRRIGATION_MODULE

  use types
  implicit none

  contains

!> @brief This subroutine updates the current Kc value for
!> each entry in the irrigation table
subroutine irrigation_UpdateCropCoefficients(pGrd, pConfig)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other setting

  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  integer (kind=T_INT) :: i

  do i = 1,size(pConfig%IRRIGATION)

    ! isolate the relevant IRRIGATION record for this landuse
    pIRRIGATION => pConfig%IRRIGATION(i)

  enddo  ! loop over landuses



end subroutine irrigation_UpdateCropCoefficients

!> @brief This subroutine estimates the amount of water required
!> to keep soil moisture values above the maximum allowable depletion (MAD)
!> for each landuse and soil type. If the mean soil moisture for a given
!> landuse/soil type falls below the MAD, the routine assumes that irrigation
!> is used to bring the soil up to field capacity.
!> \note If the irrigation module is enabled, potential evapotranspiration is calculated
!> differently: potential ET is considered reference ET (i.e. potential ET for grass),
!> and is modified by means of crop coefficients. This calculation affects irrigated
!> as well as non-irrigated landuses.
subroutine irrigation_UpdateAmounts(pGrd, pConfig)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other setting

  ! [ LOCALS ]
  integer (kind=T_INT) :: iRow, iCol
  integer (kind=T_INT) :: iNumCells
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry

  ! zero out Irrigation term
  pGrd%Cells%rIrrigationAmntGW = rZERO
  pGrd%Cells%rIrrigationAmntSW = rZERO


  ! iterate over cells; add water if soil storage zone is below the
  ! maximum allowable depletion
  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX  ! last index in a Fortan array should be the slowest changing
      cel => pGrd%Cells(iCol,iRow)
      pIRRIGATION => pConfig%IRRIGATION(cel%iLandUseIndex)

  cel%rSM_PotentialET = cel%rSM_PotentialET &
    * calc_crop_coefficient(pIRRIGATION%rKc_Max, pIRRIGATION%rK0, &
      pIRRIGATION%rGDD_Kc_Max, pIRRIGATION%rGDD_Death, pIRRIGATION%rAlpha1, cel%rGDD)

    enddo
  enddo


  ! iterate over each landuse
  do i = 1,size(pConfig%IRRIGATION)

    ! isolate the relevant IRRIGATION record for this landuse
    pIRRIGATION => pConfig%IRRIGATION(i)
    if( pIRRIGATION%rMAD > 99.9 ) cycle

    if(pConfig%iDayOfYear >= pIRRIGATION%iBeginIrrigation &
      .and. pConfig%iDayOfYear <= pIRRIGATION%iEndIrrigation ) then

      ! iterate over each soil type
      do j=1,size(pConfig%ROOTING_DEPTH,2)

        iNumCells = count(pGrd%Cells%iLandUse == pIRRIGATION%iLandUseType &
           .and. pGrd%Cells%iSoilGroup == j)

        if(iNumCells > 0) then

          rMeanGDD =  sum(pGrd%Cells%rGDD, &
             pGrd%Cells%iLandUse == pIRRIGATION%iLandUseType &
             .and. pGrd%Cells%iSoilGroup == j) / real(iNumCells, kind=T_SGL)

          rMeanSoilMoisturePct = sum(pGrd%Cells%rSoilMoisturePct, &
             pGrd%Cells%iLandUse == pIRRIGATION%iLandUseType &
             .and. pGrd%Cells%iSoilGroup == j) / real(iNumCells, kind=T_SGL)

          rMeanSoilMoisture =  sum(pGrd%Cells%rSoilMoisture, &
             pGrd%Cells%iLandUse == pIRRIGATION%iLandUseType &
             .and. pGrd%Cells%iSoilGroup == j) / real(iNumCells, kind=T_SGL)

          rMeanSoilWaterCap =  sum(pGrd%Cells%rSoilWaterCap, &
             pGrd%Cells%iLandUse == pIRRIGATION%iLandUseType &
             .and. pGrd%Cells%iSoilGroup == j) / real(iNumCells, kind=T_SGL)

          rPercentDepletion = 100_T_SGL - rMeanSoilMoisturePct

          if(rPercentDepletion > pIRRIGATION%rMAD .and. rMeanGDD > 50 ) then

            rIrrigationAmount = (rMeanSoilWaterCap - rMeanSoilMoisture)

            where (pGrd%Cells%iLandUse == pIRRIGATION%iLandUseType &
             .and. pGrd%Cells%iSoilGroup == j)

              pGrd%Cells%rIrrigationAmount = rIrrigationAmount

            endwhere

          endif  ! percent soil moisture depletion > MAD

        endif  ! iNumCells > 0

      enddo  ! loop over soil types

    endif  ! we're within the envelope of dates in which irrigation could occur

  enddo  ! loop over landuses


end subroutine update_irrigation_amounts

#endif

end module irrigation
