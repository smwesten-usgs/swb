!> @file
!> @brief  Contains a single module, \ref et_crop_coefficients, which
!>  provides support for modifying reference ET through the use of
!> crop coefficients

!> @brief  Provides support for assessing the effect of irrigation on recharge
!> values by estimating the irrigation required to maintain soil moisture levels
!> for specific crop types.
module et_crop_coefficients

  use types
	use sm_thornthwaite_mather
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

  real (kind=T_DBL) :: frac

  ! now calculate Kcb for the given landuse
  if(iThreshold > pIRRIGATION%iL_late) then
    pIRRIGATION%rKcb = pIRRIGATION%rKcb_min

  elseif ( iThreshold > pIRRIGATION%iL_mid ) then
    frac = real(iThreshold - pIRRIGATION%iL_mid, kind=T_DBL ) &
      / real( pIRRIGATION%iL_late - pIRRIGATION%iL_mid, kind=T_DBL )
    pIRRIGATION%rKcb =  pIRRIGATION%rKcb_mid * (1_T_DBL - frac) &
                         + pIRRIGATION%rKcb_end * frac

  elseif ( iThreshold > pIRRIGATION%iL_dev ) then
    pIRRIGATION%rKcb = pIRRIGATION%rKcb_mid

  elseif ( iThreshold > pIRRIGATION%iL_ini ) then
    frac = real(iThreshold - pIRRIGATION%iL_ini) &
        / real( pIRRIGATION%iL_dev - pIRRIGATION%iL_ini )
    pIRRIGATION%rKcb = pIRRIGATION%rKcb_ini * (1_T_DBL - frac) &
                           + pIRRIGATION%rKcb_mid * frac

  elseif ( iThreshold > pIRRIGATION%iL_plant ) then
    pIRRIGATION%rKcb = pIRRIGATION%rKcb_ini
  else
    pIRRIGATION%rKcb = pIRRIGATION%rKcb_min
  endif

end subroutine et_kc_UpdateCropCoefficient

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

  rNumerator = pIRRIGATION%rKcb - pIRRIGATION%rKcb_min
  rDenominator = pIRRIGATION%rKcb_mid - pIRRIGATION%rKcb_min
  rExponent = 1.0 + 0.5 * pIRRIGATION%rMeanPlantHeight * rM_PER_FOOT

  if(rDenominator > rNEAR_ZERO) then
    r_fc = (rNumerator / rDenominator) ** rExponent
  else
    r_fc = 1.0
  endif

  r_few = 1.0 - r_fc

end function et_kc_CalcFractionExposedAndWettedSoil

!------------------------------------------------------------------------------

!> @brief This function calculates the effective root zone depth given the
!> stage of plant growth, the soil type, and the land cover type.
!> @note Implemented as equation 8-1 (Annex 8), FAO-56, Allen and others.

function et_kc_CalcEffectiveRootDepth(pIRRIGATION, rZr_max, iThreshold) 	result(rZr_i)

  ! [ ARGUMENTS ]
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
	real (kind=T_SGL) :: rZr_max
	integer (kind=T_INT) :: iThreshold ! either GDD or DOY

  ! [ RESULT ]
  real (kind=T_SGL) :: rZr_i

	! [ LOCALS ]
	real (kind=T_SGL), parameter :: rZr_min = 0.328

	if ( pIRRIGATION%rKcb_mid - pIRRIGATION%rKcb_ini < 0.1) then
	  ! this is needed because for areas like forests, where the
		 ! Kcb_ini and Kcb_mid are nearly the same, we assume that root depths are
		 ! constant
	  rZr_i = rZr_max

	elseif(iThreshold < pIRRIGATION%iL_plant) then

	  rZr_i = rZr_min

	else

    rZr_i = rZr_min + (rZr_max - rZr_min) * ( real(iThreshold) - real(pIRRIGATION%iL_plant)) &
                                           / ( real(pIrrigation%iL_dev) -  real(pIRRIGATION%iL_plant) )

    print *, "EFFRTDEPTH: ",pIRRIGATION%rKcb, ( real(iThreshold) - real(pIRRIGATION%iL_plant)) &
                                           / ( real(pIrrigation%iL_dev) -  real(pIRRIGATION%iL_plant) )
  endif

end function et_kc_CalcEffectiveRootDepth

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

  rKe = rKr * ( pIRRIGATION%rKcb_mid - pIRRIGATION%rKcb )

end function et_kc_CalcSurfaceEvaporationCoefficient

!------------------------------------------------------------------------------

!> @brief This subroutine updates the total available water (TAW)
!> (water within the rootzone) for a gridcell

subroutine et_kc_CalcTotalAvailableWater( pIRRIGATION, cel)

  ! [ ARGUMENTS ]
  type (T_IRRIGATION_LOOKUP),pointer :: pIRRIGATION  ! pointer to an irrigation table entry
  type (T_CELL), pointer :: cel

  cel%rTotalAvailableWater = cel%rCurrentRootingDepth * cel%rSoilWaterCapInput
  cel%rReadilyAvailableWater = cel%rTotalAvailableWater * pIRRIGATION%rDepletionFraction

  return

end subroutine et_kc_CalcTotalAvailableWater

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

  if (rDeficit < cel%rReadilyAvailableWater) then
    rKs = rONE
  elseif (rDeficit < cel%rTotalAvailableWater) then

    rKs = ( cel%rTotalAvailableWater - rDeficit + 1.0e-6) &
          / ( (1.0 - pIRRIGATION%rDepletionFraction) &
          * (cel%rTotalAvailableWater + 1.0e-6) )

  print *, "Ks: ",rKs

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
	real (kind=T_SGL) :: rZr_max   ! Maximum rooting depth

   ! iterate over cells; update evaporation coefficients,
   ! calculate Kc, and apply to ET0
   do iRow=1,pGrd%iNY
     do iCol=1,pGrd%iNX  ! last index in a Fortran array should be the slowest changing
       cel => pGrd%Cells(iCol, iRow)

!       if ( cel%iActive == iINACTIVE_CELL ) cycle
!       if(cel%rReferenceET0 < rNEAR_ZERO) cycle
!       if(cel%rSoilWaterCap <= rNear_ZERO &
!            .or. cel%iLandUse == pConfig%iOPEN_WATER_LU) cycle

       pIRRIGATION => pConfig%IRRIGATION(cel%iIrrigationTableIndex)
			 cel%rKcb = pIRRIGATION%rKcb

			 rZr_max = pConfig%ROOTING_DEPTH(cel%iLandUseIndex,cel%iSoilGroup)

       if(pIRRIGATION%lUnitsAreDOY) then
         call et_kc_UpdateCropCoefficient(pIRRIGATION, pConfig%iDayOfYear)

				 if(pConfig%iDayOfYear < pIRRIGATION%iL_dev) then
				   cel%rCurrentRootingDepth = et_kc_CalcEffectiveRootDepth(pIRRIGATION, &
				     rZr_max, pConfig%iDayOfYear)
!					 cel%rSoilWaterCap = cel%rCurrentRootingDepth * cel%rSoilWaterCapInput
				 endif

       else
         call et_kc_UpdateCropCoefficient(pIRRIGATION, INT(cel%rGDD, kind=T_INT))

				 if(int(cel%rGDD, kind=T_INT) < pIRRIGATION%iL_dev) then
				   cel%rCurrentRootingDepth = et_kc_CalcEffectiveRootDepth(pIRRIGATION, &
				     rZr_max,INT(cel%rGDD, kind=T_INT))
!					 cel%rSoilWaterCap = cel%rCurrentRootingDepth * cel%rSoilWaterCapInput
				 endif

       endif

       rREW = pConfig%READILY_EVAPORABLE_WATER(cel%iLandUseIndex, cel%iSoilGroup)
       rTEW = pConfig%TOTAL_EVAPORABLE_WATER(cel%iLandUseIndex, cel%iSoilGroup)
       rDeficit = MAX(rZERO, cel%rSoilWaterCap - cel%rSoilMoisture)

       !> "STANDARD" vs "NONSTANDARD": in the FAO56 publication the term
       !> "STANDARD" is used to refer to crop ET requirements under
       !> ideal conditions (i.e. plants not stressed due to scarcity
       !. of water. "NONSTANDARD" is the term used to describe ET requirements
       !> when plants are under stress, when water is scarce.

       if ( pConfig%iConfigureFAO56 == CONFIG_FAO56_CROP_COEFFICIENTS_NONSTANDARD ) then
         ! we are using the full FAO56 soil water balance approach, *INCLUDING*
				 ! the adjustments for nonstandard growing conditions (e.g. plant
				 ! stress and resulting decrease in ET during dry conditions).

         rKr = et_kc_CalcEvaporationReductionCoefficient(rTEW, rREW, rDeficit)
         r_few = et_kc_CalcFractionExposedAndWettedSoil( pIRRIGATION )
         rKe = min(et_kc_CalcSurfaceEvaporationCoefficient( pIRRIGATION, &
                 rKr ), r_few * pIRRIGATION%rKcb_mid )

         call et_kc_CalcTotalAvailableWater( pIRRIGATION, cel)
         print *, "(",iRow, iCol,") TAW:",cel%rTotalAvailableWater
         rKs = et_kc_CalcWaterStressCoefficient( pIRRIGATION, rDeficit, cel)
         cel%rBareSoilEvap = cel%rReferenceET0 * rKe

         cel%rCropETc = cel%rReferenceET0 * (pIRRIGATION%rKcb * rKs) ! &

         ! this is the general term being used in the water balance
         cel%rReferenceET0_adj = cel%rCropETc + cel%rBareSoilEvap

       elseif ( pConfig%iConfigureFAO56 == CONFIG_FAO56_CROP_COEFFICIENTS_STANDARD ) then

         ! if we are not using the full FAO56 soil water balance approach,
         ! we should just adjust the potential ET by the crop coefficient.
         ! The Thornthwaite-Mather soil moisture retention tables already
         ! account for the fact that water becomes more difficult to extract
         ! as the APWL increases...

				 ! NO reductions in Kc due to water availability

         rKr = et_kc_CalcEvaporationReductionCoefficient(rTEW, rREW, rDeficit)
         r_few = et_kc_CalcFractionExposedAndWettedSoil( pIRRIGATION )
         rKe = min(et_kc_CalcSurfaceEvaporationCoefficient( pIRRIGATION, &
                 rKr ), r_few * pIRRIGATION%rKcb_mid )

         cel%rBareSoilEvap = cel%rReferenceET0 * rKe

         cel%rReferenceET0_adj = cel%rReferenceET0 * pIRRIGATION%rKcb + cel%rBareSoilEvap
         cel%rCropETc = cel%rReferenceET0_adj

			 else

			   call assert(lFALSE, "Programming error - unknown FAO56 configuration option", &
				   trim(__FILE__), __LINE__)

       endif
     enddo
   enddo

end subroutine et_kc_ApplyCropCoefficients

end module et_crop_coefficients
