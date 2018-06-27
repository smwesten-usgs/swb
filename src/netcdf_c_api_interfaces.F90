!> @file
!>  This module provides Fortran interfaces to the NetCDF C API.

!> Provides Fortran interfaces to the NetCDF C API. This approach
!> is much more straightforward than using the old Fortran 90 NetCDF interface,
!> which was built on top of the Fortran 77 interface, which (finally) was
!> built atop the C interface.
!>
!> @note
!> old interface code didn't take advantage of the iso_c_binding types
!> c_size_t or c_prtdiff_t, but rather used pointers (type c_ptr) to
!> pass by reference. This appears to be unneccessary:

!  interface
!    function nc_get_vars_short(ncid, varid, startp, countp, stridep, vars) bind(c)
!
!      import :: c_int, c_ptr, c_short, c_ptrdiff_t, c_size_t
!
!      integer (kind=c_int), value             :: ncid, varid
!
!      integer (kind=c_size_t)                            :: startp(*)
!      integer (kind=c_size_t)                            :: countp(*)
!      integer (kind=c_ptrdiff_t)                         :: stridep(*)

!!!      type (c_ptr), value                     :: startp
!!!      type (c_ptr), value                     :: countp
!!!      type (c_ptr), value                     :: stridep
!      integer (kind=c_short), intent(out)     :: vars(*)

!      integer (kind=c_int)                             :: nc_get_vars_short

!    end function nc_get_vars_short
!  end interface
!>
!> @todo
!> change "type (c_ptr), value" entries to "integer (kind=c_size_t)"
!> or similar, depending on the size of the integer declared in the
!> C code

module netcdf_c_api_interfaces

  use iso_c_binding

  implicit none

!  integer, parameter :: c_ptrdiff_t = 8

  interface
    function nc_create(path, cmode, ncidp) bind(c)

      import :: c_char, c_int

      character (kind=c_char), intent(in)  :: path(*)
      integer (kind=c_int),    value       :: cmode
      integer (kind=c_int),    intent(out) :: ncidp

      integer (kind=c_int)                 :: nc_create

    end function nc_create
  end interface

  interface
    function nc_def_var_deflate(ncid, varid, shuffle, deflate, deflate_level) bind(c)

      import :: c_int

      integer (kind=c_int), value :: ncid
      integer (kind=c_int), value :: varid
      integer (kind=c_int), value :: shuffle
      integer (kind=c_int), value :: deflate
      integer (kind=c_int), value :: deflate_level

      integer (kind=c_int)        :: nc_def_var_deflate

    end function nc_def_var_deflate
  end interface

  interface
    function nc_def_var(ncid, name, xtype, ndims, dimidsp, varidp) bind(c)

      import :: c_int, c_char

      integer (kind=c_int),    value       :: ncid
      character (kind=c_char), intent(in)  :: name(*)
      integer (kind=c_int),    value       :: xtype
      integer (kind=c_int),    value       :: ndims
      integer (kind=c_int),    intent(in)  :: dimidsp(*)
      integer (kind=c_int),    intent(out) :: varidp

      integer (kind=c_int)                 :: nc_def_var

    end function nc_def_var
  end interface

  interface
    function nc_def_dim(ncid, name, lenv, dimidp) bind(c)

      import :: c_int, c_size_t, c_char

      integer (kind=c_int),    value       :: ncid
      character (kind=c_char), intent(in)  :: name(*)
      integer (kind=c_size_t), value       :: lenv
      integer (kind=c_int),    intent(out) :: dimidp

      integer (kind=c_int)                 :: nc_def_dim

    end function nc_def_dim
  end interface

  interface
    function nc_enddef(ncid) bind(c)

      import :: c_int

      integer (kind=c_int), value :: ncid

      integer (kind=c_int)        :: nc_enddef

    end function nc_enddef
  end interface


  interface
    function nc_get_var_short(ncid, varid, sp) bind(c)

      import :: c_int, c_short

      integer (kind=c_int), value          :: ncid, varid
      integer (kind=c_short), intent(out)  :: sp(*)

      integer (kind=c_int)                 :: nc_get_var_short

    end function nc_get_var_short
  end interface

  interface
    function nc_put_var_short(ncid, varid, sp) bind(c)

      import :: c_int, c_short

      integer (kind=c_int), value      :: ncid, varid
      integer (kind=c_short), intent(in) :: sp(*)

      integer (kind=c_int)             :: nc_put_var_short

     end function nc_put_var_short
  end interface


  interface
    function nc_get_var_int(ncid, varid, ip) bind(c)

      import :: c_int

      integer (kind=c_int), value       :: ncid, varid
      integer (kind=c_int), intent(out) :: ip(*)

      integer (kind=c_int)              :: nc_get_var_int

    end function nc_get_var_int
  end interface

  interface
    function nc_put_var_int(ncid, varid, ip) bind(c)

      import :: c_int

      integer (kind=c_int), value      :: ncid, varid
      integer (kind=c_int), intent(in) :: ip(*)

      integer (kind=c_int)             :: nc_put_var_int

    end function nc_put_var_int
  end interface


  interface
    function nc_get_var_float(ncid, varid, rp) bind(c)

      import :: c_int, c_float

      integer (kind=c_int), value       :: ncid, varid
      real (kind=c_float), intent(out) :: rp(*)

      integer (kind=c_int)              :: nc_get_var_float

    end function nc_get_var_float
  end interface

  interface
    function nc_put_var_float(ncid, varid, rp) bind(c)

      import :: c_int, c_float

      integer (kind=c_int), value      :: ncid, varid
      real (kind=c_float),  intent(in) :: rp(*)

      integer (kind=c_int)             :: nc_put_var_float

    end function nc_put_var_float
  end interface


  interface
    function nc_get_var_double(ncid, varid, dp) bind(c)

      import :: c_int, c_double

      integer (kind=c_int), value         :: ncid, varid
      real (kind=c_double), intent(out)  :: dp(*)

      integer (kind=c_int)                :: nc_get_var_double

    end function nc_get_var_double
  end interface

  interface
    function nc_put_var_double(ncid, varid, dp) bind(c)

      import :: c_int, c_double

      integer (kind=c_int), value      :: ncid, varid
      real (kind=c_double), intent(in) :: dp(*)

      integer (kind=c_int)             :: nc_put_var_double

    end function nc_put_var_double
  end interface


  interface
    function nc_get_vars_short(ncid, varid, startp, countp, stridep, vars) bind(c)

      import :: c_int, c_ptr, c_short, c_ptrdiff_t, c_size_t

      integer (kind=c_int), value             :: ncid, varid

      integer (kind=c_size_t)                            :: startp(*)
      integer (kind=c_size_t)                            :: countp(*)
      integer (kind=c_ptrdiff_t)                         :: stridep(*)

!      type (c_ptr), value                     :: startp
!      type (c_ptr), value                     :: countp
!      type (c_ptr), value                     :: stridep
      integer (kind=c_short), intent(out)     :: vars(*)

      integer (kind=c_int)                             :: nc_get_vars_short

    end function nc_get_vars_short
  end interface

  interface
    function nc_put_vars_short(ncid, varid, startp, countp, stridep, vars) bind(c)

      import :: c_int, c_ptrdiff_t, c_size_t, c_short

      integer (kind=c_int), value         :: ncid, varid
!      type(c_ptr),         value         :: startp, countp, stridep
      integer (kind=c_size_t)                            :: startp(*)
      integer (kind=c_size_t)                            :: countp(*)
      integer (kind=c_ptrdiff_t)                         :: stridep(*)

      integer (kind=c_short), intent(in)  :: vars(*)

      integer (kind=c_int)                :: nc_put_vars_short

    end function nc_put_vars_short
  end interface


  interface
    function nc_get_vars_int(ncid, varid, startp, countp, stridep, vars) bind(c)

      import :: c_int, c_ptrdiff_t, c_size_t

      integer (kind=c_int), value             :: ncid, varid
!      type (c_ptr), value                     :: startp
!      type (c_ptr), value                     :: countp
!      type (c_ptr), value                     :: stridep
      integer (kind=c_size_t)                            :: startp(*)
      integer (kind=c_size_t)                            :: countp(*)
      integer (kind=c_ptrdiff_t)                         :: stridep(*)

      integer (kind=c_int), intent(out)       :: vars(*)

      integer (kind=c_int)                    :: nc_get_vars_int

    end function nc_get_vars_int
  end interface

  interface
    function nc_put_vars_int(ncid, varid, startp, countp, stridep, vars) bind(c)

      import :: c_int, c_ptrdiff_t, c_size_t

      integer (kind=c_int), value      :: ncid, varid
!      type (c_ptr),         value      :: startp, countp, stridep
      integer (kind=c_size_t)                            :: startp(*)
      integer (kind=c_size_t)                            :: countp(*)
      integer (kind=c_ptrdiff_t)                         :: stridep(*)

      integer (kind=c_int), intent(in) :: vars(*)

      integer (kind=c_int)             :: nc_put_vars_int

    end function nc_put_vars_int
  end interface




  interface
    function nc_get_vars_float(ncid, varid, startp, countp, stridep, vars) bind(c)

      import :: c_int, c_ptrdiff_t, c_size_t, c_float

      integer (kind=c_int), value             :: ncid, varid
!      type (c_ptr), value                     :: startp
!      type (c_ptr), value                     :: countp
!      type (c_ptr), value                     :: stridep

      integer (kind=c_size_t)                            :: startp(*)
      integer (kind=c_size_t)                            :: countp(*)
      integer (kind=c_ptrdiff_t)                         :: stridep(*)

      real (kind=c_float), intent(out)        :: vars(*)

      integer (kind=c_int)                             :: nc_get_vars_float

    end function nc_get_vars_float
  end interface

  interface
    function nc_put_vars_float(ncid, varid, startp, countp, stridep, vars) bind(c)

      import :: c_int, c_float, c_ptrdiff_t, c_size_t

      integer (kind=c_int), value      :: ncid, varid
!      type(c_ptr),         value      :: startp, countp, stridep
      integer (kind=c_size_t)                            :: startp(*)
      integer (kind=c_size_t)                            :: countp(*)
      integer (kind=c_ptrdiff_t)                         :: stridep(*)

      real (kind=c_float),  intent(in) :: vars(*)

      integer (kind=c_int)             :: nc_put_vars_float

    end function nc_put_vars_float
  end interface


  interface
    function nc_get_vars_double(ncid, varid, startp, countp, stridep, vars) bind(c)

      import :: c_int, c_double, c_size_t, c_ptrdiff_t

      integer (kind=c_int), value             :: ncid, varid
!      type (c_ptr), value                     :: startp
!      type (c_ptr), value                     :: countp
!      type (c_ptr), value                     :: stridep
      integer (kind=c_size_t)                            :: startp(*)
      integer (kind=c_size_t)                            :: countp(*)
      integer (kind=c_ptrdiff_t)                         :: stridep(*)

      real (kind=c_double), intent(out)       :: vars(*)

      integer (kind=c_int)                    :: nc_get_vars_double

    end function nc_get_vars_double
  end interface

  interface
    function nc_put_vars_double(ncid, varid, startp, countp, stridep, vars) bind(c)

      import :: c_int, c_double, c_ptrdiff_t, c_size_t

      integer (kind=c_int), value      :: ncid, varid
!      type(c_ptr),         value      :: startp, countp, stridep
      integer (kind=c_size_t)          :: startp(*)
      integer (kind=c_size_t)          :: countp(*)
      integer (kind=c_ptrdiff_t)       :: stridep(*)

      real (kind=c_double), intent(in) :: vars(*)

      integer (kind=c_int)             :: nc_put_vars_double

    end function nc_put_vars_double
  end interface


  interface
    function nc_get_att_text(ncid, varid, name, ip) bind(c)

      import :: c_int, c_char

      integer (kind=c_int),    value       :: ncid, varid
      character (kind=c_char), intent(in)  :: name(*)
      character (kind=c_char), intent(out) :: ip(*)

      integer (kind=c_int)                 :: nc_get_att_text

    end function nc_get_att_text
  end interface

  interface
    function nc_get_att_int(ncid, varid, name, ip)   bind(c)

      import :: c_int, c_char

      integer (kind=c_int),    value       :: ncid, varid
      character (kind=c_char), intent(in)  :: name(*)
      integer (kind=c_int),    intent(out) :: ip(*)

      integer (kind=c_int)                 :: nc_get_att_int

    end function nc_get_att_int
  end interface

  interface
    function nc_get_att_short(ncid, varid, name, ip)   bind(c)

      import :: c_int, c_char, c_short

      integer (kind=c_int),    value          :: ncid, varid
      character (kind=c_char), intent(in)     :: name(*)
      integer (kind=c_short),    intent(out)  :: ip(*)

      integer (kind=c_int)                    :: nc_get_att_short

    end function nc_get_att_short
  end interface

  interface
    function nc_get_att_float(ncid, varid, name, ip)   bind(c)

      import :: c_int, c_float, c_char

      integer (kind=c_int),    value         :: ncid, varid
      character (kind=c_char), intent(in)    :: name(*)
      real (kind=c_float),     intent(out)   :: ip(*)

      integer (kind=c_int)                   :: nc_get_att_float

    end function nc_get_att_float
  end interface

  interface
    function nc_get_att_double(ncid, varid, name, ip)   bind(c)

      import :: c_int, c_double, c_char

      integer (kind=c_int),    value         :: ncid, varid
      character (kind=c_char), intent(in)    :: name(*)
      real (kind=c_double),     intent(out)  :: ip(*)

      integer (kind=c_int)                   :: nc_get_att_double

    end function nc_get_att_double
  end interface

  interface
    function nc_inq_attname(ncid, varid, attnum, name)   bind(c)

      import :: c_int, c_char

      integer (kind=c_int),    value          :: ncid, varid, attnum
      character (kind=c_char), intent(inout)  :: name(*)

      integer (kind=c_int)                    :: nc_inq_attname

    end function nc_inq_attname
  end interface

  interface
    function nc_inq_att(ncid, varid, name, xtypep, lenp)  bind(c)

      import :: c_int, c_size_t, c_char

      integer (kind=c_int),    value        :: ncid, varid
      character (kind=c_char), intent(in)   :: name(*)
      integer (kind=c_int),    intent(out)  :: xtypep
      integer (kind=c_size_t), intent(out)  :: lenp

      integer (kind=c_int)                  :: nc_inq_att

    end function nc_inq_att
  end interface

  interface
    function nc_inq_var(ncid, varid, name, xtypep, ndimsp, dimidsp, nattsp) &
                     bind(c)

      import :: c_int, c_char

      integer (kind=c_int),    value       :: ncid
      integer (kind=c_int),    value       :: varid
      character (kind=c_char)             :: name(*)
      integer (kind=c_int),    intent(out) :: xtypep
      integer (kind=c_int),    intent(out) :: ndimsp
      integer (kind=c_int),    intent(out) :: dimidsp(*)
      integer (kind=c_int),    intent(out) :: nattsp

      integer (kind=c_int)                 :: nc_inq_var

    end function nc_inq_var
  end interface


  interface
    function nc_inq_dim(ncid, dimid, name, lenp) bind(c)

      import :: c_int, c_size_t, c_char

      integer (kind=c_int),    value         :: ncid
      integer (kind=c_int),    value         :: dimid
      character (kind=c_char), intent(inout) :: name(*)
      integer (kind=c_size_t), intent(out)   :: lenp

      integer (kind=c_int)                   :: nc_inq_dim

    end function nc_inq_dim
  end interface

  interface
    function nc_strerror(ncerr) bind(c)

      import :: c_int, c_ptr

      integer (kind=c_int), value :: ncerr

      type(c_ptr)                :: nc_strerror

    end function nc_strerror
  end interface

  interface
    function nc_inq_format(ncid, formatp) bind(c)

      import :: c_int

      integer (kind=c_int), value       :: ncid
      integer (kind=c_int), intent(out) :: formatp

      integer (kind=c_int)              :: nc_inq_format

    end function nc_inq_format
  end interface


  interface
    function nc_inq_dimname(ncid, dimid, name) bind(c)

      import :: c_int, c_char

      integer (kind=c_int),    value         :: ncid
      integer (kind=c_int),    value         :: dimid
      character (kind=c_char), intent(inout) :: name(*)

      integer (kind=c_int)                   :: nc_inq_dimname

    end function nc_inq_dimname
  end interface

  interface
    function nc_inq_unlimdim(ncid, unlimdimidp) bind(c)

      import :: c_int

      integer (kind=c_int), value       :: ncid
      integer (kind=c_int), intent(out) :: unlimdimidp

      integer (kind=c_int)              :: nc_inq_unlimdim

    end function nc_inq_unlimdim
  end interface

  interface
   function nc_inq_ndims(ncid, ndimsp) bind(c)

     import :: c_int

     integer (kind=c_int), value       :: ncid
     integer (kind=c_int), intent(out) ::  ndimsp

     integer (kind=c_int)              :: nc_inq_ndims

   end function nc_inq_ndims
  end interface
  !---------------------------------- nc_inq_nvars ------------------------------
  interface
    function nc_inq_nvars(ncid, nvarsp) bind(c)

      import :: c_int

      integer (kind=c_int), value       :: ncid
      integer (kind=c_int), intent(out) ::  nvarsp

      integer (kind=c_int)              :: nc_inq_nvars

    end function nc_inq_nvars
  end interface
!---------------------------------- nc_inq_natts ------------------------------
  interface
    function nc_inq_natts(ncid, ngattsp) bind(c)

      import :: c_int

      integer (kind=c_int), value       :: ncid
      integer (kind=c_int), intent(out) :: ngattsp

      integer (kind=c_int)              :: nc_inq_natts

    end function nc_inq_natts
  end interface

!----------------------------------------------------------------------

  interface
    function nc_put_att_text(ncid, varid, name, nlen, tp) bind(c)

      import :: c_int, c_size_t, c_char

      integer (kind=c_int),    value      :: ncid, varid
      integer (kind=c_size_t), value      :: nlen
      character (kind=c_char), intent(in) :: name(*)
      character (kind=c_char), intent(in) :: tp(*)

      integer (kind=c_int)                :: nc_put_att_text

    end function nc_put_att_text
  end interface

!----------------------------------------------------------------------

  interface
    function nc_put_att_int(ncid, varid, name, xtype, nlen, ip)   bind(c)

      import :: c_int, c_size_t, c_char

      integer (kind=c_int),    value      :: ncid, varid
      integer (kind=c_size_t), value      :: nlen
      integer (kind=c_int),    value      :: xtype
      character (kind=c_char), intent(in) :: name(*)
      integer (kind=c_int),    intent(in) :: ip(*)

      integer (kind=c_int)                :: nc_put_att_int

    end function nc_put_att_int
  end interface

!----------------------------------------------------------------------

  interface
    function nc_put_att_float(ncid, varid, name, xtype, nlen, fp)   bind(c)

      import :: c_int, c_size_t, c_float, c_char

      integer (kind=c_int),    value      :: ncid, varid
      integer (kind=c_size_t), value      :: nlen
      integer (kind=c_int),    value      :: xtype
      character (kind=c_char), intent(in) :: name(*)
      real (kind=c_float),     intent(in) :: fp(*)

      integer (kind=c_int) :: nc_put_att_float

    end function nc_put_att_float
  end interface

!----------------------------------------------------------------------

  interface
    function nc_put_att_double(ncid, varid, name, xtype, nlen, dp) bind(c)

      import :: c_int, c_size_t, c_double, c_char

      integer (kind=c_int),    value      :: ncid, varid
      integer (kind=c_size_t), value      :: nlen
      integer (kind=c_int),    value      :: xtype
      character (kind=c_char), intent(in) :: name(*)
      real (kind=c_double),    intent(in) :: dp(*)

      integer (kind=c_int)                :: nc_put_att_double

    end function nc_put_att_double
  end interface

!----------------------------------------------------------------------

  interface
    function nc_copy_att(ncid_in, varid_in, name, ncid_out, varid_out )  bind(c)

      import :: c_int, c_char

      integer (kind=c_int),    value      :: ncid_in, varid_in, varid_out, &
                                            ncid_out
      character (kind=c_char), intent(in) :: name(*)

      integer (kind=c_int)                :: nc_copy_att

    end function nc_copy_att
  end interface



  interface
    function nc_open(path, mode, ncidp) BIND(C)

      import :: c_char, c_int

      character (kind=c_char), intent(in)  :: path(*)
      integer (kind=c_int), value          :: mode
      integer (kind=c_int), intent(out)    :: ncidp

      integer (kind=c_int)                 :: nc_open

    end function nc_open
  end interface

  interface
    function nc__open(path, mode, chunksizehintp, ncidp) BIND(C)

      import :: c_char, c_int, c_size_t

      character (kind=c_char), intent(in)  :: path(*)
      integer (kind=c_int),    value       :: mode
      integer (kind=c_size_t), intent(in)  :: chunksizehintp
      integer (kind=c_int),    intent(out) :: ncidp

      integer (kind=c_int)                 :: nc__open

    end function nc__open
  end interface

  interface
    function nc_close(ncid) bind(c)

      import :: c_int

      integer (kind=c_int), value :: ncid

      integer (kind=c_int)        :: nc_close

    end function nc_close
  end interface

end module netcdf_c_api_interfaces
