!> @file
!> @brief  Contains a single module, \ref et_crop_coefficients, which
!>  provides support for modifying reference ET through the use of
!> crop coefficients

!> @brief  Provides support for assessing the effect of irrigation on recharge
!> values by estimating the irrigation required to maintain soil moisture levels
!> for specific crop types.
module et_crop_coefficients

  use types
  implicit none

  contains

!------------------------------------------------------------------------------

 !> @brief This subroutine updates the current Kcb for a SINGLE irrigation
 !> table entry
 subroutine et_kc_UpdateCropCoefficient(pIRRIGATION, iThreshold)

  ! [ ARGUMENTS ]
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  integer (kind=T_INT) :: iThreshold

  ! [ LOCALS ]
  integer (kind=T_INT) :: i, j

  real (kind=T_SGL) :: frac

  ! now calculate Kcb for the given landuse
  if(iThreshold > pIRRIGATION%iL_late) then
    pIRRIGATION%rKcb = 0.

  elseif ( iThreshold > pIRRIGATION%iL_mid ) then
    frac = real(iThreshold - pIRRIGATION%iL_mid ) &
      / real( pIRRIGATION%iL_late - pIRRIGATION%iL_mid )
    pIRRIGATION%rKcb =  pIRRIGATION%rKcb_mid * frac &
                         + pIRRIGATION%rKcb_end * (1. - frac)

  elseif ( iThreshold > pIRRIGATION%iL_dev ) then
    pIRRIGATION%rKcb = pIRRIGATION%rKcb_mid

  elseif ( iThreshold > pIRRIGATION%iL_ini ) then
    frac = real(iThreshold - pIRRIGATION%iL_ini) &
        / real( pIRRIGATION%iL_dev - pIRRIGATION%iL_ini )
    pIRRIGATION%rKcb = pIRRIGATION%rKcb_ini * frac &
                           + pIRRIGATION%rKcb_mid * (1. - frac)

  elseif ( iThreshold > pIRRIGATION%iL_plant ) then
    pIRRIGATION%rKcb = pIRRIGATION%rKcb_ini
  else
    pIRRIGATION%rKcb = 0.
  endif

end subroutine et_kc_UpdateCropCoefficient

!----------------------------------------------------------------------

!> @brief This subroutine updates the current Kcb value for
!> each entry in the irrigation table
subroutine et_kc_UpdateCropCoefficients(pGrd, pConfig)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other setting

  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  integer (kind=T_INT) :: i, j
  real (kind=T_SGL) :: f

  do i = 1,size(pConfig%IRRIGATION)

    ! isolate the relevant IRRIGATION record for this landuse
    pIRRIGATION => pConfig%IRRIGATION(i)

    ! now calculate Kcb for the given landuse
    if(pConfig%iDayOfYear > pIRRIGATION%iL_late) then
      pIRRIGATION%rKcb = 0.

    elseif ( pConfig%iDayOfYear > pIRRIGATION%iL_mid ) then
      f = real(pConfig%iDayOfYear - pIRRIGATION%iL_mid ) &
          / real( pIRRIGATION%iL_late - pIRRIGATION%iL_mid )
      pIRRIGATION%rKcb =  pIRRIGATION%rKcb_mid * f &
                           + pIRRIGATION%rKcb_end * (1. - f)

    elseif ( pConfig%iDayOfYear > pIRRIGATION%iL_dev ) then
      pIRRIGATION%rKcb = pIRRIGATION%rKcb_mid

    elseif ( pConfig%iDayOfYear > pIRRIGATION%iL_ini ) then
        f = real(pConfig%iDayOfYear - pIRRIGATION%iL_ini) &
            / real( pIRRIGATION%iL_dev - pIRRIGATION%iL_ini )
        pIRRIGATION%rKcb = pIRRIGATION%rKcb_ini * f &
                             + pIRRIGATION%rKcb_mid * (1. - f)

    elseif ( pConfig%iDayOfYear > pIRRIGATION%iL_plant ) then
      pIRRIGATION%rKcb = pIRRIGATION%rKcb_ini

    else
      pIRRIGATION%rKcb = 0.
    endif

  enddo  ! loop over landuses

end subroutine et_kc_UpdateCropCoefficients

!------------------------------------------------------------------------------

function et_kc_CalcEvaporationReductionCoefficient(rTEW, rREW, &
   rDeficit)  result(rKr)

  ! [ ARGUMENTS ]
  real (kind=T_SGL) :: rTEW
  real (kind=T_SGL) :: rREW
  real (kind=T_SGL) :: rDeficit

  ! [ RESULT ]
  real (kind=T_SGL) :: rKr

  if(rDeficit > rREW .and. rDeficit < rTEW) then
    rKr = (rTEW - rDeficit) / (rTEW - rREW)
  elseif(rDeficit <= rREW) then
    rKr = 1.
  else
    rKr = 0.
  endif

end function et_kc_CalcEvaporationReductionCoefficient

!------------------------------------------------------------------------------

!> @brief This function estimates the fraction of the ground covered by
!> vegetation during the growing season
!> @note Implemented as equation 76, FAO-56, Allen and others
function et_kc_CalcFractionExposedAndWettedSoil( pIRRIGATION )   result (r_few)

  ! [ ARGUMENTS ]
  type (T_IRRIGATION_LOOKUP), pointer :: pIRRIGATION  ! pointer to an irrigation table entry

  ! [ RESULT ]
  real (kind=T_SGL) :: r_few

  ! [ LOCALS ]
  real (kind=T_SGL) :: r_fc
  real (kind=T_SGL) :: rNumerator
  real (kind=T_SGL) :: rDenominator
  real (kind=T_SGL) :: rExponent

  rNumerator = pIRRIGATION%rKcb - pIRRIGATION%rKc_min
  rDenominator = pIRRIGATION%rKc_max - pIRRIGATION%rKc_min
  rExponent = 1.0 + 0.5 * pIRRIGATION%rMeanPlantHeight * rM_PER_FOOT

  r_fc = (rNumerator / rDenominator) ** rExponent

  r_few = 1.0 - r_fc

end function et_kc_CalcFractionExposedAndWettedSoil

!------------------------------------------------------------------------------

!> @brief This function estimates Kr, the bare surface evaporation
!> coefficient
!> @note Implemented as equation 71, FAO-56, Allen and others
function et_kc_CalcSurfaceEvaporationCoefficient( pIRRIGATION, &
      rKr )     result(rKe)

  ! [ ARGUMENTS ]
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  real (kind=T_SGL) :: rKr

  ! [ RESULT ]
  real (kind=T_SGL) :: rKe

  rKe = rKr * ( pIRRIGATION%rKc_max - pIRRIGATION%rKcb )

end function et_kc_CalcSurfaceEvaporationCoefficient

!------------------------------------------------------------------------------

!> @brief This function estimates Ks, water stress coefficient
!> @note Implemented as equation 84, FAO-56, Allen and others
function et_kc_CalcWaterStressCoefficient( pIRRIGATION, &
                                           rDeficit, &
                                           cel)        result(rKs)

  ! [ ARGUMENTS ]
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  real (kind=T_SGL) :: rDeficit
  type (T_CELL), pointer :: cel

  ! [ RESULT ]
  real (kind=T_SGL) :: rKs

  rKs = ( cel%rSoilWaterCap - rDeficit ) &
        / ( (1.0 - pIRRIGATION%rDepletionFraction) * cel%rSoilWaterCap )

end function et_kc_CalcWaterStressCoefficient

!------------------------------------------------------------------------------

subroutine et_kc_ApplyCropCoefficients(pGrd, pConfig)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other setting

  ! [ LOCALS ]
  integer (kind=T_INT) :: iRow, iCol
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  type (T_CELL),pointer :: cel
  real (kind=T_SGL) :: rTEW      ! Total evaporable water
  real (kind=T_SGL) :: rREW      ! Readily evaporable water
  real (kind=T_SGL) :: rKr       ! Evaporation reduction coefficient
  real (kind=T_SGL) :: rDeficit  ! Soil moisture deficit
  real (kind=T_SGL) :: r_few     ! Fraction exposed and wetted soil
  real (kind=T_SGL) :: rKe       ! Surface evaporation coefficient
  real (kind=T_SGL) :: rKs       ! Water stress coefficient

   ! update the crop coefficients
!   call et_kc_UpdateCropCoefficients(pGrd, pConfig)

   ! iterate over cells; update evaporation coefficients,
   ! calculate Kc, and apply to ET0
   do iRow=1,pGrd%iNY
     do iCol=1,pGrd%iNX  ! last index in a Fortan array should be the slowest changing
       cel => pGrd%Cells(iCol, iRow)
       if ( cel%iActive == iINACTIVE_CELL ) cycle
       if(cel%rReferenceET0 < rNEAR_ZERO) cycle
       if(cel%rSoilWaterCap <= rNear_ZERO &
            .or. cel%iLandUse == pConfig%iOPEN_WATER_LU) cycle

       pIRRIGATION => pConfig%IRRIGATION(cel%iLandUseIndex)
       rREW = pConfig%READILY_EVAPORABLE_WATER(cel%iLandUseIndex, cel%iSoilGroup)
       rTEW = pConfig%TOTAL_EVAPORABLE_WATER(cel%iLandUseIndex, cel%iSoilGroup)
       ! for purposes of calculating evaporation, we will include water sitting on
       ! leaves as available for evaporation along with soil moisture in the
       ! top few inches of soil
       rDeficit = MAX(rZERO, cel%rSoilWaterCap - cel%rSoilMoisture + cel%rInterception)
       rKr = et_kc_CalcEvaporationReductionCoefficient(rTEW, rREW, rDeficit)
       r_few = et_kc_CalcFractionExposedAndWettedSoil( pIRRIGATION )
       rKe = min(et_kc_CalcSurfaceEvaporationCoefficient( pIRRIGATION, &
                   rKr ), r_few * pIRRIGATION%rKc_max )
       rKs = et_kc_CalcWaterStressCoefficient( pIRRIGATION, rDeficit, cel)
       if(pIRRIGATION%lUnitsAreDOY) then
         call et_kc_UpdateCropCoefficient(pIRRIGATION, pConfig%iDayOfYear)
       else
         call et_kc_UpdateCropCoefficient(pIRRIGATION, INT(cel%rGDD, kind=T_INT))
       endif
       cel%rCrop_ET = cel%rReferenceET0 * (rKe + pIRRIGATION%rKcb * rKs)
       if(rand() < 0.05 ) &
         write(*,fmt="(i8,1x,12f12.3,1x)") cel%iLandUse, rREW, rTEW, rDeficit, rKr, r_few, &
            cel%rReferenceET0, rKe, rKs, pIRRIGATION%rKcb, cel%rCrop_ET
     enddo
   enddo

end subroutine et_kc_ApplyCropCoefficients

end module et_crop_coefficients
