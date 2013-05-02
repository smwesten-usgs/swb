!> @file
!> @brief Contains a single module, @ref sm_thornthwaite_mather, which estimates runoff by
!> means of the NRCS/SCS curve number method.

!> @brief Performs the actual soil-moisture balance once precip, snowmelt, runoff, and ET have
!> been calculated. Soil moisture for a given soil accumulated potential water loss (APWL)
!> is determined by means of Thornthwaite and Mathers' (1957) tables.
module sm_thornthwaite_mather
!!****h* SWB/sm_thornthwaite_mather
! NAME
!
!   sm_thornthwaite_mather.f95 - Soil moisture calculation routines based on Thornthwaite
!   and Mather (1957)
! SYNOPSIS
!   Soil moisture calculation routines based on Thornthwaite
!   and Mather (1957).
!
! NOTES
!
!!***

  use types
  use swb_grid
  use stats
  use RLE

  implicit none

  real (kind=T_SGL), parameter :: APWL_Cap = -40.69_T_SGL

  !! Module data

  !! Table of data for the Thornthwaite soil moisture balance calculations
  type ( T_GENERAL_GRID ),pointer :: gWLT

  ! parameters that allow the Thornthwaite-Mather tables (1957) to be
  ! represented by a single equation
  real (kind=T_DBL), parameter :: rTM_slope_term = 0.478769194198665_T_DBL
  real (kind=T_DBL), parameter :: rTM_exp_term = -1.03678439421169_T_DBL

contains

subroutine sm_thornthwaite_mather_Configure ( sRecord )
  !! Configures the module based on command-line options
  ! [ ARGUMENTS ]
  character (len=*),intent(inout) :: sRecord
  ! [ LOCALS ]
  character (len=256) :: sOption

  ! Read the grid for the water-loss table
  call Chomp( sRecord, sOption )
  call Assert( len_trim(sOption) > 0, &
     "No Soil-moisture retention table file was specified", &
     TRIM(__FILE__), __LINE__)
  write(UNIT=LU_LOG,FMT=*)"Reading ",trim(sOption)," for soil-moisture retention information"
  gWLT => grid_Read( sOption, "SURFER", DATATYPE_REAL )

  write(UNIT=LU_LOG,FMT=*)"Read in the soil-moisture retention file with the following dimensions:"
  write(UNIT=LU_LOG,FMT=*)"iNX = ",gWLT%iNX
  write(UNIT=LU_LOG,FMT=*)"iNY = ",gWLT%iNY
  write(UNIT=LU_LOG,FMT=*)"iDataType = ",gWLT%iDataType
  write(UNIT=LU_LOG,FMT=*)"rX0, rX1 = ",gWLT%rX0, gWLT%rX1
  write(UNIT=LU_LOG,FMT=*)"rY0, rY1 = ",gWLT%rY0, gWLT%rY1

end subroutine sm_thornthwaite_mather_Configure

!------------------------------------------------------------------------------

subroutine sm_thornthwaite_mather_Initialize ( pGrd, pConfig )
  !! Preconfigures soil moisture for the model grid 'pGrd'.
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
                                                   ! model options, flags, and other settings
  ! [ LOCALS ]
  integer (kind=T_INT) :: iRow,iCol
  type ( T_CELL ),pointer :: cel

  ! Initialize the accumulated water loss for each cell according to the
  ! initial soil moisture.

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX

      cel => pGrd%Cells(iCol,iRow)

      if ( cel%rSoilWaterCap > rNEAR_ZERO ) then

      !! Constrain rSoilWaterCap to limits of Thornthwaite-Mather tables

        if ( cel%rSoilWaterCap < 0.5_T_SGL ) then
          write(UNIT=LU_LOG,FMT="(a,f10.3,a,i4,',',i4,a)") &
            'Soil water capacity', cel%rSoilWaterCap,' for cell ',iRow,iCol, &
            ' out of range; adjusted to 0.5'
          flush(unit=LU_LOG)
          cel%rSoilWaterCap = 0.51_T_SGL
        else if ( cel%rSoilWaterCap > 17.5_T_SGL ) then
          write(UNIT=LU_LOG,FMT="(a,f10.3,a,i4,',',i4,a)") &
            'Soil water capacity', cel%rSoilWaterCap,' for cell ',iRow,iCol, &
            ' out of range; adjusted to 17.5'
          flush(unit=LU_LOG)
          cel%rSoilWaterCap = 17.49_T_SGL
        end if

     !! convert input soil moisture (as percent of soil water capacity)
     !! TO soil moisture in inches
     cel%rSoilMoisture = (cel%rSoilMoisturePct/rHUNDRED) * cel%rSoilWaterCap

      !! back-calculate initial accumulated potential water loss term
      !! given initial soil moisture

#ifdef TM_TABLE

       ! calculate APWL from T-M table
       cel%rSM_AccumPotentWatLoss = &
         grid_SearchColumn(pGrd=gWLT, &
                           rXval=cel%rSoilWaterCap, &
                           rZval=cel%rSoilMoisture, &
                           rNoData=-rONE)

#else

       ! calculate APWL from equation
       cel%rSM_AccumPotentWatLoss = &
         sm_thornthwaite_mather_APWL(cel%rSoilWaterCap,real(cel%rSoilMoisture, kind=T_DBL) )

#endif

    end if
    end do
  end do

#ifdef DEBUG_PRINT
 call grid_WriteArcGrid("initial_APWL.asc",dpZERO,dpONE,dpZERO, &
    dpONE,pGrd%Cells(:,:)%rSM_AccumPotentWatLoss)
#endif

  !! initialize Thorntwaite-Mather soil moisture accounting variables

  pGrd%Cells%rReferenceET0 = rZERO

end subroutine sm_thornthwaite_mather_Initialize


!----------------------------------------------------------------------

function sm_thornthwaite_mather_soil_storage(rSWC, rAPWL)  result(dpValue)

  real (kind=T_SGL), intent(in) :: rSWC     ! max soil-water capacity (inches)
  real (kind=T_SGL), intent(in) :: rAPWL    ! accum pot. water loss (inches)

  real (kind=T_DBL) :: dpValue

  ! equation as implemented in R;
  ! sm.df$y = maximum soil-water capacity
  ! sm.df$x = APWL
  ! 10^(log10(sm.df$y) - (s.opt[[1]]*sm.df$y^e.opt[[1]]) * sm.df$x)

  dpValue = dpZERO

  if(rSWC > rZERO ) &

    dpValue = 10_T_DBL**( log10(REAL(rSWC,kind=T_DBL)) - &
              ( ABS(REAL(rAPWL,kind=T_DBL)) * rTM_slope_term &
              * real(rSWC, kind=T_DBL)**rTM_exp_term ) )


end function sm_thornthwaite_mather_soil_storage

!------------------------------------------------------------------------------

function sm_thornthwaite_mather_APWL(rSWC, dpSoilStorage)  result(dpValue)

  real (kind=T_SGL), intent(in) :: rSWC          ! max soil-water capacity (inches)
  real (kind=T_DBL), intent(in) :: dpSoilStorage  ! curr soil storage (inches)

  real (kind=T_DBL) :: dpValue

  ! equation as implemented in R;
  ! sm.df$y = maximum soil-water capacity
  ! sm.df$x = APWL
  ! (log10(sm.df$y) - log10(sm.df$pred)) / (s.opt[[1]] * sm.df$y^e.opt[[1]])

  dpValue = dpZERO

  if(rSWC > rZERO .and. dpSoilStorage > dpZERO) &

    dpValue = -( log10(REAL(rSWC,kind=T_DBL)) - log10(dpSoilStorage)) / &
          ( rTM_slope_term * REAL(rSWC,kind=T_DBL)**rTM_exp_term )

end function sm_thornthwaite_mather_APWL

!------------------------------------------------------------------------------

subroutine sm_thornthwaite_mather_UpdatePctSM( pGrd )

  type ( T_GENERAL_GRID ),pointer :: pGrd

  where(pGrd%Cells%rSoilWaterCap > rNEAR_ZERO )

    pGrd%Cells%rSoilMoisturePct = pGrd%Cells%rSoilMoisture  &
       / pGrd%Cells%rSoilWaterCap * 100.

  elsewhere

    pGrd%Cells%rSoilMoisturePct = rZERO

  endwhere

end subroutine sm_thornthwaite_mather_UpdatePctSM

!------------------------------------------------------------------------------

end module sm_thornthwaite_mather
