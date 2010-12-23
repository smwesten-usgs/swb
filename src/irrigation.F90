module irrigation

  use types
  implicit none

  contains

!> @brief This subroutine estimates the amount of water required
!> to keep soil moisture values above the MAD for each landuse and soil type.
!> If the mean soil moisture for a given landuse/soil type falls below the MAD,
!> the routine assumes that irrigation is used to bring the soil up to
!> field capacity.
subroutine update_irrigation_amounts(pGrd, pConfig)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other setting

  ! [ LOCALS ]
  integer (kind=T_INT) :: i, j
  integer (kind=T_INT) :: iNumCells
  real (kind=T_SGL) :: rPercentDepletion
  real (kind=T_SGL) :: rIrrigationAmount
  real (kind=T_SGL) :: rMeanSoilMoisturePct
  real (kind=T_SGL) :: rMeanSoilMoisture
  real (kind=T_SGL) :: rMeanSoilWaterCap
  real (kind=T_SGL) :: rMeanGDD
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry

  ! zero out Irrigation term
  pGrd%Cells%rIrrigationAmount = rZERO
  rIrrigationAMount = rZERO

  ! iterate over each landuse
  do i = 1,size(pConfig%IRRIGATION)

    ! isolate the relevant IRRIGATION record for this landuse
    pIRRIGATION => pConfig%IRRIGATION(i)

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

end module irrigation
