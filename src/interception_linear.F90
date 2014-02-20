!> @file
!!  Contains a single module, @ref interception_linear, which
!!  calculates daily interception values by means of a simple linear relation.


!>  Calculates daily canopy interception by means of a simple linear relation.
module interception_linear

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use interception_base_class, only : T_INTERCEPTION_BASE_CLASS

  implicit none

  private
  
  type, extends(T_INTERCEPTION_BASE_CLASS), public :: T_INTERCEPTION_LINEAR

    private

    real (kind=c_float) :: fConstant_A
    real (kind=c_float) :: fConstant_B

    contains

      procedure, public :: initialize => interception_initialize_sub

      procedure, public :: compute => interception_compute_fn

  end type T_INTERCEPTION_LINEAR

contains

  subroutine interception_initialize_sub(this, fConstant_1, fConstant_2 )

    class (T_INTERCEPTION_LINEAR), intent(inout)      :: this
    real (kind=c_float), intent(in)                :: fConstant_1
    real (kind=c_float), intent(in)                :: fConstant_2

    this%fConstant_A = fConstant_1
    this%fConstant_B = fConstant_2

  end subroutine interception_initialize_sub

  !------------------------------------------------------------------------------

  function interception_compute_fn( this, fRainfall, fFog)    result( fInterception )

    class (T_INTERCEPTION_LINEAR), intent(in)      :: this
    real (kind=c_float), intent(in)                :: fRainfall
    real (kind=c_float), intent(in), optional      :: fFog
    real (kind=c_float)                            :: fInterception

    ! [ LOCALS ]
    real (kind=c_float) :: fRatio

    fRatio = this%fConstant_A * fFog + this%fConstant_B

    fInterception = fRainfall - ( fRainfall * fRatio )

  end function interception_compute_fn


end module interception_linear
