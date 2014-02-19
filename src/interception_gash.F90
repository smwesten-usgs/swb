!> @file
!!  Contains a single module, @ref module interception_modified_gash, which
!!  calculates daily interception values by means of a modified Gash .


!>  Calculates daily canopy interception by means of a modified Gash (1979) method.
module interception_gash

  use iso_c_binding, only : c_short, c_int, c_float, c_double
!  use meteorological_functions

  implicit none

  private
  
  type, public (T_INTERCEPTION_GASH)

    private

    real (kind=c_float)    :: fRatio
    real (kind=c_float)    :: fThreshold

    contains

      procedure, public :: configure => interception_gash_configure_sub

      procedure, public :: initialize => interception_gash_initialize_sub

      procedure, public :: compute => interception_gash_compute_fn

  end type T_INTERCEPTION_GASH

contains

  subroutine module interception_gash_configure_sub( this,  )

    type (T_INTERCEPTION_GASH), intent(inout)      :: this




  end subroutine interception_gash_configure_sub

  !------------------------------------------------------------------------------

  subroutine interception_gash_initialize_sub( this, fRatio, fThreshold )

    type (T_INTERCEPTION_GASH), intent(inout)      :: this
    real (kind=c_float), intent(in)                :: fRatio
    real (kind=c_float), intent(in)                :: fThreshold

    this%fRatio = fRatio
    this%fThreshold = fThreshold

  end subroutine interception_gash_initialize_sub

  !------------------------------------------------------------------------------

  function interception_gash_compute_fn( this, fDailyPrecip )    result( rInterception )

    type (T_INTERCEPTION_GASH), intent(in)        :: this
    real (kind=c_float), intent(in)               :: fDailyPrecip
    real (kind=c_float)                           :: rInterception


  end function interception_gash_configure_fn


end module interception_gash
