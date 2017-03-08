!> @file
!>  Contains a single module, \ref et_crop_coefficients, which
!>  provides support for modifying reference ET through the use of
!> crop coefficients

!>  Provide support for assessing the effect of irrigation on recharge
!> values by estimating the irrigation required to maintain soil moisture levels
!> for specific crop types.
module et_crop_coefficients

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use types
  use sm_thornthwaite_mather
  implicit none

  contains

!------------------------------------------------------------------------------

 !> Update the current basal crop corfficient (Kcb) for
 !! a SINGLE irrigation table entry
 !!
 !! @param[inout] pIRRIGATION pointer to a single line of information in the irrigation file.
 !! @param[in] iThreshold either the current day of year or the number of growing degree days.
 !! @retval rKcb Basal crop coefficient given the irrigation table entries and the current threshold values.
 function et_kc_UpdateCropCoefficient(pIRRIGATION, iThreshold)  result(rKcb)

  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  integer (kind=c_int), intent(in)   :: iThreshold
  real (kind=c_float)                :: rKcb

  ! [ LOCALS ]
  integer (kind=c_int) :: i, j

  real (kind=c_double) :: frac

  ! now calculate Kcb for the given landuse
  if(iThreshold > pIRRIGATION%iL_late) then
    rKcb = pIRRIGATION%rKcb_min

  elseif ( iThreshold > pIRRIGATION%iL_mid ) then
    frac = real(iThreshold - pIRRIGATION%iL_mid, kind=c_double ) &
      / real( pIRRIGATION%iL_late - pIRRIGATION%iL_mid, kind=c_double )
    rKcb =  pIRRIGATION%rKcb_mid * (1_c_double - frac) &
                         + pIRRIGATION%rKcb_end * frac

  elseif ( iThreshold > pIRRIGATION%iL_dev ) then
    rKcb = pIRRIGATION%rKcb_mid

  elseif ( iThreshold > pIRRIGATION%iL_ini ) then
    frac = real(iThreshold - pIRRIGATION%iL_ini) &
        / real( pIRRIGATION%iL_dev - pIRRIGATION%iL_ini )
    rKcb = pIRRIGATION%rKcb_ini * (1_c_double - frac) &
                           + pIRRIGATION%rKcb_mid * frac

  elseif ( iThreshold >= pIRRIGATION%iL_plant ) then
    rKcb = pIRRIGATION%rKcb_ini
  else
    rKcb = pIRRIGATION%rKcb_min
  endif

end function et_kc_UpdateCropCoefficient

!------------------------------------------------------------------------------

!>
function et_kc_CalcEvaporationReductionCoefficient(rTEW, rREW, rDeficit)  result(rKr)

  ! [ ARGUMENTS ]
  real (kind=c_float) :: rTEW
  real (kind=c_float) :: rREW
  real (kind=c_float) :: rDeficit

  ! [ RESULT ]
  real (kind=c_float) :: rKr

  if ( rDeficit <= rREW ) then

    rKr = 1.0

  elseif ( rDeficit < rTEW ) then

    rKr = (rTEW - rDeficit) / (rTEW - rREW)

  else

    rKr = 0.

  endif

end function et_kc_CalcEvaporationReductionCoefficient

!------------------------------------------------------------------------------

!> This function estimates the fraction of the ground covered by
!> vegetation during the growing season
!> @note Implemented as equation 76, FAO-56, Allen and others
function et_kc_CalcFractionExposedAndWettedSoil( pIRRIGATION, rKcb )   result (r_few)

  ! [ ARGUMENTS ]
  type (T_IRRIGATION_LOOKUP), pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  real (kind=c_float)                 :: rKcb

  ! [ RESULT ]
  real (kind=c_float) :: r_few

  ! [ LOCALS ]
  real (kind=c_float) :: r_fc
  real (kind=c_float) :: rNumerator
  real (kind=c_float) :: rDenominator
  real (kind=c_float) :: rExponent

!  rNumerator = pIRRIGATION%rKcb - pIRRIGATION%rKcb_min
!
! BUG? if Kcb is tracked for each cell, the value contained in the irrigation table is undefined(?)
!
  rNumerator = rKcb - pIRRIGATION%rKcb_min
  rDenominator = pIRRIGATION%rKcb_mid - pIRRIGATION%rKcb_min
  rExponent = 1.0 + 0.5 * pIRRIGATION%rMeanPlantHeight * rM_PER_FOOT

  if(rDenominator > rNEAR_ZERO) then
    r_fc = (rNumerator / rDenominator) ** rExponent
  else
    r_fc = 1.0
  endif

  r_few = 1.0 - r_fc

  if (r_few < 0.) r_few = 0.0
  if (r_few > 1.) r_few = 1.0

end function et_kc_CalcFractionExposedAndWettedSoil

!------------------------------------------------------------------------------

!> Calculate the effective root zone depth.
!!
!! Calculate the effective root zone depth give then current stage
!! of plant growth, the soil type, and the crop type.
!!
!! @param[in] pIRRIGATION pointer to a specific line of the irrigation
!!     lookup data structure.
!! @param[in] rZr_max The maximum rooting depth for this crop; currently this
!!     is supplied to this function as the rooting depth associated with the
!!     landuse/soil type found in the landuse lookup table.
!! @param[in] iThreshold Numeric value (either the GDD or the DOY) defining
!!     the time that the crop is planted.
!! @retval rZr_i current active rooting depth.
!! @note Implemented as equation 8-1 (Annex 8), FAO-56, Allen and others.
function et_kc_CalcEffectiveRootDepth(pIRRIGATION, rZr_max, rKcb) 	result(rZr_i)

  ! [ ARGUMENTS ]
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  real (kind=c_float) :: rZr_max
  real (kind=c_float) :: rKcb

  ! [ RESULT ]
  real (kind=c_float) :: rZr_i

  ! [ LOCALS ]
  ! 0.328 feet equals 0.1 meters, which is seems to be the standard
  ! initial rooting depth in the FAO-56 methodology
  real (kind=c_float), parameter :: rZr_min = 0.328

  if ( pIRRIGATION%rKcb_mid - pIRRIGATION%rKcb_ini < 0.1) then
    ! this is needed because for areas like forests, where the
     ! Kcb_ini and Kcb_mid are nearly the same, we assume that root depths are
     ! constant
    rZr_i = rZr_max

  elseif( rKcb > pIRRIGATION%rKcb_min ) then

    rZr_i = rZr_min + (rZr_max - rZr_min) * ( rKcb - pIRRIGATION%rKcb_min ) &
                                           / ( pIRRIGATION%rKcb_max -  pIRRIGATION%rKcb_min )

  else

    rZr_i = rZr_min

  endif

end function et_kc_CalcEffectiveRootDepth

!------------------------------------------------------------------------------

!> This function estimates Ke, the bare surface evaporation
!> coefficient
!> @note Implemented as equation 71, FAO-56, Allen and others
function et_kc_CalcSurfaceEvaporationCoefficient( pIRRIGATION, rKcb, rKr )     result(rKe)

  ! [ ARGUMENTS ]
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  real (kind=c_float) :: rKcb
  real (kind=c_float) :: rKr

  ! [ RESULT ]
  real (kind=c_float) :: rKe

  rKe = rKr * ( pIRRIGATION%rKcb_max - rKcb )

end function et_kc_CalcSurfaceEvaporationCoefficient

!------------------------------------------------------------------------------

!> This subroutine updates the total available water (TAW)
!> (water within the rootzone) for a gridcell
subroutine et_kc_CalcTotalAvailableWater( pIRRIGATION, cel)

  ! [ ARGUMENTS ]
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  type (T_CELL), pointer :: cel

  cel%rTotalAvailableWater = cel%rCurrentRootingDepth * cel%rSoilWaterCapInput
  cel%rReadilyAvailableWater = cel%rTotalAvailableWater * pIRRIGATION%rDepletionFraction

  end subroutine et_kc_CalcTotalAvailableWater

!------------------------------------------------------------------------------

!> This function estimates Ks, water stress coefficient
!> @note Implemented as equation 84, FAO-56, Allen and others
function et_kc_CalcWaterStressCoefficient( pIRRIGATION, &
                                           rDeficit, &
                                           cel)        result(rKs)

  ! [ ARGUMENTS ]
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  real (kind=c_float) :: rDeficit
  type (T_CELL), pointer :: cel

  ! [ RESULT ]
  real (kind=c_float) :: rKs

  if (rDeficit < cel%rReadilyAvailableWater) then

    rKs = rONE

  elseif (rDeficit < cel%rTotalAvailableWater) then

    rKs = ( cel%rTotalAvailableWater - rDeficit )                                    &
             / ( cel%rTotalAvailableWater - cel%rReadilyAvailableWater + 1.0e-6 )
  else
    rKs = rZERO
  endif

end function et_kc_CalcWaterStressCoefficient

!------------------------------------------------------------------------------

subroutine et_kc_ApplyCropCoefficients(pGrd, pConfig)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other setting

  ! [ LOCALS ]
  integer (kind=c_int) :: iRow, iCol
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  type (T_CELL),pointer :: cel
  real (kind=c_float) :: rTEW          ! Total evaporable water
  real (kind=c_float) :: rREW          ! Readily evaporable water
  real (kind=c_float) :: rDeficit      ! Soil moisture deficit
  real (kind=c_float) :: rDepthOfEvap  ! Depth of evaporation
  real (kind=c_float) :: r_few         ! Fraction exposed and wetted soil
  real (kind=c_float) :: rZr_max       ! Maximum rooting depth

  integer (kind=c_int) :: iLBound
  integer (kind=c_int) :: iUBound

  iLBound = lbound( pConfig%IRRIGATION, 1 )
  iUBound = ubound( pConfig%IRRIGATION, 1 )

   ! iterate over cells; update evaporation coefficients,
   ! calculate Kc, and apply to ET0
   do iRow=1,pGrd%iNY
     do iCol=1,pGrd%iNX  ! last index in a Fortran array should be the slowest changing
       cel => pGrd%Cells(iCol, iRow)

       if ( pGrd%iMask(iCol, iRow) == iINACTIVE_CELL ) cycle

!       if(cel%rReferenceET0 < rNEAR_ZERO) cycle
       if(cel%rSoilWaterCap <= rNear_ZERO &
            .or. cel%iLandUse == pConfig%iOPEN_WATER_LU) cycle


       if ( cel%iIrrigationTableIndex < iLBound                        &
              .or. cel%iIrrigationTableIndex > iUBound )               &
         call assert( lFALSE, "Index out of bounds. Index value: "//   &
           asCharacter(cel%iIrrigationTableIndex),                     &
           __FILE__, __LINE__ )

       ! point to the line in the irrigation table pertaining to landuse of current cell
       pIRRIGATION => pConfig%IRRIGATION(cel%iIrrigationTableIndex)

       rZr_max = pConfig%ROOTING_DEPTH(cel%iLandUseIndex,cel%iSoilGroup)

       ! update crop coefficient and current rooting depth
       if(pIRRIGATION%lUnitsAreDOY) then

         cel%rKcb = et_kc_UpdateCropCoefficient(pIRRIGATION, pConfig%iDayOfYear)

       else

         cel%rKcb = et_kc_UpdateCropCoefficient(pIRRIGATION, INT(cel%rGDD, kind=c_int))

       endif

       cel%rCurrentRootingDepth = et_kc_CalcEffectiveRootDepth(pIRRIGATION, rZr_max, cel%rKcb )
       rREW = pConfig%READILY_EVAPORABLE_WATER(cel%iLandUseIndex, cel%iSoilGroup)
       rTEW = pConfig%TOTAL_EVAPORABLE_WATER(cel%iLandUseIndex, cel%iSoilGroup)

       ! Deficit is defined in the sense of Thornthwaite and Mather
!       rDeficit = MAX(rZERO, cel%rSoilWaterCap - cel%rSoilMoisture)

       ! following call updates the total available water (TAW) and
       ! readily available water (RAW) on the basis of the current
       ! plant root depth
       call et_kc_CalcTotalAvailableWater( pIRRIGATION, cel)

       ! Deficit is defined in the sense of Thornthwaite and Mather, and
       ! is calculated relative to the CURRENT ROOTING DEPTH
       ! ********* ????????
!       rDeficit = MAX(rZERO, cel%rTotalAvailableWater - cel%rSoilMoisture)
       rDeficit = MAX(rZERO, cel%rSoilWaterCap - cel%rSoilMoisture)
       rDepthOfEvap = MAX(rZERO, cel%rSoilWaterCap - cel%rSoilMoisture)

       ! "STANDARD" vs "NONSTANDARD": in the FAO56 publication the term
       ! "STANDARD" is used to refer to crop ET requirements under
       ! ideal conditions (i.e. plants not stressed due to scarcity
       ! of water. "NONSTANDARD" is the term used to describe ET requirements
       ! when plants are under stress, when water is scarce.

       if ( pConfig%iConfigureFAO56 == CONFIG_FAO56_TWO_FACTOR_NONSTANDARD ) then
         ! we are using the full FAO56 soil water balance approach, *INCLUDING*
         ! the adjustments for nonstandard growing conditions (e.g. plant
         ! stress and resulting decrease in ET during dry conditions).

         cel%rKr = et_kc_CalcEvaporationReductionCoefficient(rTEW, rREW, rDepthOfEvap)
         r_few = et_kc_CalcFractionExposedAndWettedSoil( pIRRIGATION, cel%rKcb )
         cel%rKe = min(et_kc_CalcSurfaceEvaporationCoefficient( pIRRIGATION, cel%rKcb, cel%rKr ), &
                   r_few * pIRRIGATION%rKcb_mid )

         cel%rKs = et_kc_CalcWaterStressCoefficient( pIRRIGATION, rDeficit, cel)

         cel%rBareSoilEvap = cel%rReferenceET0 * cel%rKe
         cel%rCropETc = cel%rReferenceET0 * (cel%rKcb * cel%rKs)

       elseif ( pConfig%iConfigureFAO56 == CONFIG_FAO56_ONE_FACTOR_NONSTANDARD ) then
         ! we are using the full FAO56 soil water balance approach, *INCLUDING*
         ! the adjustments for nonstandard growing conditions (e.g. plant
         ! stress and resulting decrease in ET during dry conditions).
         ! *EXCLUDING* explicit calculation of BareSoilEvap

         cel%rKs = et_kc_CalcWaterStressCoefficient( pIRRIGATION, rDeficit, cel)

         cel%rBareSoilEvap = rZERO
         cel%rCropETc = cel%rReferenceET0 * (cel%rKcb * cel%rKs)

       elseif ( pConfig%iConfigureFAO56 == CONFIG_FAO56_TWO_FACTOR_STANDARD ) then

         ! if we are not using the full FAO56 soil water balance approach,
         ! we should just adjust the potential ET by the crop coefficient.
         ! The Thornthwaite-Mather soil moisture retention tables already
         ! account for the fact that water becomes more difficult to extract
         ! as the APWL increases...

         ! NO reductions in Kc due to water availability

         cel%rKr = et_kc_CalcEvaporationReductionCoefficient(rTEW, rREW, rDepthOfEvap)
         r_few = et_kc_CalcFractionExposedAndWettedSoil( pIRRIGATION, cel%rKcb )
         cel%rKe = min(et_kc_CalcSurfaceEvaporationCoefficient( pIRRIGATION, cel%rKcb, cel%rKr ), &
                   r_few * pIRRIGATION%rKcb_mid )

         cel%rBareSoilEvap = cel%rReferenceET0 * cel%rKe
         cel%rCropETc = cel%rReferenceET0 * cel%rKcb

       elseif ( pConfig%iConfigureFAO56 == CONFIG_FAO56_ONE_FACTOR_STANDARD ) then

         ! NO reductions in Kc due to water availability
         ! NO explicit calculation of BareSoilEvap
         ! no real calculations required because we're applying the crop coefficient directly

         cel%rBareSoilEvap = rZERO
         cel%rCropETc = cel%rReferenceET0 * cel%rKcb

       else

         call assert(lFALSE, "Programming error - unknown FAO56 configuration option", &
           trim(__FILE__), __LINE__)

       endif

       ! "Adjusted" Reference ET is the general term being used in the water balance
       cel%rReferenceET0_adj = cel%rCropETc + cel%rBareSoilEvap

     enddo
   enddo

end subroutine et_kc_ApplyCropCoefficients

end module et_crop_coefficients
