!> @file
!!  Contains a single module, @ref interception_linear, which
!!  calculates daily interception values by means of a simple linear relation.


!>  Calculates daily canopy interception by means of a simple linear relation.
module interception_exponential

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use interception_base_class, only : T_INTERCEPTION_BASE_CLASS

  implicit none

  private
  
  type, extends(T_INTERCEPTION_BASE_CLASS), public :: T_INTERCEPTION_EXPONENTIAL

    private

    real (kind=c_float) :: fConstant_C
    real (kind=c_float) :: fConstant_D

    contains

      procedure, public :: initialize => interception_initialize_sub

      procedure, public :: compute => interception_compute_fn

  end type T_INTERCEPTION_EXPONENTIAL

contains

  subroutine interception_initialize_sub(this, fConstant_1, fConstant_2 )

    class (T_INTERCEPTION_EXPONENTIAL), intent(inout)      :: this
    real (kind=c_float), intent(in)                :: fConstant_1
    real (kind=c_float), intent(in)                :: fConstant_2

    this%fConstant_C = fConstant_1
    this%fConstant_D = fConstant_2

  end subroutine interception_initialize_sub

  !------------------------------------------------------------------------------

  function interception_compute_fn( this, fRainfall, fFog)    result( fInterception )

    class (T_INTERCEPTION_EXPONENTIAL), intent(in)      :: this
    real (kind=c_float), intent(in)                :: fRainfall
    real (kind=c_float), intent(in), optional      :: fFog
    real (kind=c_float)                            :: fInterception

    ! [ LOCALS ]
    real (kind=c_float) :: fRatio
    real (kind=c_float) :: fRainfallPlusFog

    fRainfallPlusFog = fRainfall + fFog

    if ( fRainfallPlusFog > 0_c_float ) then

      fRatio = this%fConstant_C * exp( this%fConstant_D / fRainfallPlusFog )

      fInterception = fRainfall - ( fRainfallPlusFog * fRatio )

    else

      fInterception = 0_c_float

    endif

  end function interception_compute_fn


end module interception_exponential
