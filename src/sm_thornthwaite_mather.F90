!> @file
!> Contains a single module, @ref sm_thornthwaite_mather, which estimates runoff by
!> means of the NRCS/SCS curve number method.

!> Performs the actual soil-moisture balance once precip, snowmelt, runoff, and ET have
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

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use types
  use swb_grid
  use stats
  use RLE

  implicit none

  real (c_float), parameter :: APWL_Cap = -40.69_c_float

  !! Module data

  !! Table of data for the Thornthwaite soil moisture balance calculations
  type ( T_GENERAL_GRID ),pointer :: gWLT

  ! parameters that allow the Thornthwaite-Mather tables (1957) to be
  ! represented by a single equation
  real (c_double), parameter :: rTM_slope_term = 0.478769194198665_c_double
  real (c_double), parameter :: rTM_exp_term = -1.03678439421169_c_double

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
  integer (c_int) :: iRow,iCol
  type ( T_CELL ),pointer :: cel

  ! Initialize the accumulated water loss for each cell according to the
  ! initial soil moisture.

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX

      cel => pGrd%Cells(iCol,iRow)

      if (pGrd%iMask(iCol, iRow) == iINACTIVE_CELL) cycle

      if ( cel%rSoilWaterCap > rNEAR_ZERO ) then

        if (pConfig%iConfigureSM == CONFIG_SM_TM_LOOKUP_TABLE) then

        !! Constrain rSoilWaterCap to limits of Thornthwaite-Mather tables

          if ( cel%rSoilWaterCap < 0.5_c_float ) then
            write(UNIT=LU_LOG,FMT="(a,f10.3,a,i4,',',i4,a)") &
              'Soil water capacity', cel%rSoilWaterCap,' for cell ',iRow,iCol, &
              ' out of range; adjusted to 0.5'
            flush(unit=LU_LOG)
            cel%rSoilWaterCap = 0.51_c_float
          else if ( cel%rSoilWaterCap > 17.5_c_float ) then
             write(UNIT=LU_LOG,FMT="(a,f10.3,a,i4,',',i4,a)") &
              'Soil water capacity', cel%rSoilWaterCap,' for cell ',iRow,iCol, &
              ' out of range; adjusted to 17.5'
            flush(unit=LU_LOG)
            cel%rSoilWaterCap = 17.49_c_float
          end if

          !! convert input soil moisture (as percent of soil water capacity)
          !! TO soil moisture in inches
          cel%rSoilMoisture = (cel%rSoilMoisturePct/rHUNDRED) * cel%rSoilWaterCap

          ! calculate APWL from T-M table
          cel%rSM_AccumPotentWatLoss = grid_SearchColumn(pGrd=gWLT,                         &
                                               rXval=cel%rSoilWaterCap,                     &
                                               rZval=real(cel%rSoilMoisture, c_float), &
                                               rNoData=-rONE)

        elseif ( pConfig%iConfigureSM == CONFIG_SM_TM_EQUATIONS ) then

          !! convert input soil moisture (as percent of soil water capacity)
          !! TO soil moisture in inches
          cel%rSoilMoisture = (cel%rSoilMoisturePct/rHUNDRED) * cel%rSoilWaterCap

         ! calculate APWL from equation
         cel%rSM_AccumPotentWatLoss = &
           sm_thornthwaite_mather_APWL(cel%rSoilWaterCap,real(cel%rSoilMoisture, c_double) )

        endif

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

  real (c_float), intent(in) :: rSWC     ! max soil-water capacity (inches)
  real (c_float), intent(in) :: rAPWL    ! accum pot. water loss (inches)

  real (c_double) :: dpValue

  ! equation as implemented in R;
  ! sm.df$y = maximum soil-water capacity
  ! sm.df$x = APWL
  ! 10^(log10(sm.df$y) - (s.opt[[1]]*sm.df$y^e.opt[[1]]) * sm.df$x)

  dpValue = dpZERO

  if(rSWC > rZERO ) &

    dpValue = 10_c_double**( log10(REAL(rSWC,c_double)) - &
              ( ABS(REAL(rAPWL,c_double)) * rTM_slope_term &
              * real(rSWC, c_double)**rTM_exp_term ) )


end function sm_thornthwaite_mather_soil_storage

!------------------------------------------------------------------------------

function sm_thornthwaite_mather_APWL(rSWC, dpSoilStorage)  result(dpValue)

  real (c_float), intent(in) :: rSWC          ! max soil-water capacity (inches)
  real (c_double), intent(in) :: dpSoilStorage  ! curr soil storage (inches)

  real (c_double) :: dpValue

  ! equation as implemented in R;
  ! sm.df$y = maximum soil-water capacity
  ! sm.df$x = APWL
  ! (log10(sm.df$y) - log10(sm.df$pred)) / (s.opt[[1]] * sm.df$y^e.opt[[1]])

  dpValue = dpZERO

  if(rSWC > rZERO .and. dpSoilStorage > dpZERO) &

    dpValue = -( log10(REAL(rSWC,c_double)) - log10(dpSoilStorage)) / &
          ( rTM_slope_term * REAL(rSWC,c_double)**rTM_exp_term )

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
