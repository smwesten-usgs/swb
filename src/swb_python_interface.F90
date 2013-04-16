module swb_python_interface

  use types
  use swb_grid
  use stats
  use runoff_curve_number
  use et_thornthwaite_mather
  use et_turc
  use et_hargreaves
  use et_jensen_haise
  use et_blaney_criddle
  use sm_thornthwaite_mather
  use snow
  use irrigation
  use netcdf_support
  use control
  implicit none

  !f2py integer (kind=4), dimension(:), allocatable :: iMonth
  !f2py integer (kind=4), dimension(:), allocatable :: iDay
  !f2py integer (kind=4), dimension(:), allocatable :: iYear
  !f2py integer (kind=4), dimension(:), allocatable :: iHour
  !f2py integer (kind=4), dimension(:), allocatable :: iMinute
  !f2py integer (kind=4), dimension(:), allocatable :: iSecond
  !f2py integer (kind=4), dimension(:), allocatable :: iJulianDay
  integer (kind=4), dimension(:), allocatable :: iMonth
  integer (kind=4), dimension(:), allocatable :: iDay
  integer (kind=4), dimension(:), allocatable :: iYear
  integer (kind=4), dimension(:), allocatable :: iHour
  integer (kind=4), dimension(:), allocatable :: iMinute
  integer (kind=4), dimension(:), allocatable :: iSecond
  integer (kind=4), dimension(:), allocatable :: iJulianDay

  !f2py intent(hide) :: pConfig
  type (T_MODEL_CONFIGURATION), pointer :: pConfig

  !f2py intent(hide) :: pGrd
  type (T_GENERAL_GRID),pointer :: pGrd      ! Grid of model cells

  !f2py intent(hide) :: pGraph
  type (T_GRAPH_CONFIGURATION), dimension(:),pointer :: pGraph
                                            ! pointer to data structure that
                                                   ! holds parameters for creating
                                                   ! DISLIN plots


contains

  subroutine startswb(sControlFile)

    !f2py character(len=*),intent(in) :: sControlFile
    character(len=*), intent(in) :: sControlFile

    ! pass control to control module
    call control_setModelOptions(sControlFile)

  end subroutine startswb

end module swb_python_interface
