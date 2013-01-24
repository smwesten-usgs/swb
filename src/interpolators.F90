module interpolators

implicit none

  LOGICAL, PUBLIC :: &
       &    l_first_call_akima = .TRUE. , &  !: whether it is the first time or not that AKIMA_2D is called
       &    l_always_first_call  = .FALSE.
  !!
  INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: ixy_pos !: table storing input/output grids mapping

  !! needed for SOSIE routines...
  INTEGER, PARAMETER  :: nsys = 16 !: Dimmension of the linear sytem to solve


contains

subroutine shepard_interp_2d ( nd, xd, yd, zd, p, ni, xi, yi, zi )

!*****************************************************************************80
!
!! SHEPARD_INTERP_2D evaluates a 2D Shepard interpolant.
!
!  Discussion:
!
!    This code should be vectorized.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Donald Shepard,
!    A two-dimensional interpolation function for irregularly spaced data,
!    ACM '68: Proceedings of the 1968 23rd ACM National Conference,
!    ACM, pages 517-524, 1969.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ND, the number of data points.
!
!    Input, real ( kind = 8 ) XD(ND), YD(ND), the data points.
!
!    Input, real ( kind = 8 ) ZD(ND), the data values.
!
!    Input, real ( kind = 8 ) P, the power.
!
!    Input, integer ( kind = 4 ) NI, the number of interpolation points.
!
!    Input, real ( kind = 8 ) XI(NI), YI(NI), the interpolation points.
!
!    Output, real ( kind = 8 ) ZI(NI), the interpolated values.
!
  implicit none

  integer ( kind = 4 ) nd
  integer ( kind = 4 ) ni

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) p
  real ( kind = 8 ) s
  real ( kind = 8 ) w(nd)
  real ( kind = 8 ) xd(nd)
  real ( kind = 8 ) xi(ni)
  real ( kind = 8 ) yd(nd)
  real ( kind = 8 ) yi(ni)
  integer ( kind = 4 ) z
  real ( kind = 8 ) zd(nd)
  real ( kind = 8 ) zi(ni)

  do i = 1, ni

    if ( p == 0.0D+00 ) then

      w(1:nd) = 1.0D+00 / real ( nd, kind = 8 )

    else

      z = -1
      do j = 1, nd
        w(j) = sqrt ( ( xi(i) - xd(j) ) ** 2 + ( yi(i) - yd(j) ) ** 2 )
        if ( w(j) == 0.0D+00 ) then
          z = j
          exit
        end if
      end do

      if ( z /= -1 ) then
        w(1:nd) = 0.0D+00
        w(z) = 1.0D+00
      else
        w(1:nd) = 1.0D+00 / w(1:nd) ** p
        s = sum ( w )
        w(1:nd) = w(1:nd) / s
      end if

    end if

    zi(i) = dot_product ( w, zd )

  end do

end subroutine shepard_interp_2d

subroutine pwl_interp_2d ( nxd, nyd, xd, yd, zd, ni, xi, yi, zi )

!*****************************************************************************80
!
!! PWL_INTERP_2D: piecewise linear interpolant to data defined on a 2D grid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 October 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NXD, NYD, the number of X and Y data values.
!
!    Input, real ( kind = 8 ) XD(NXD), YD(NYD), the sorted X and Y data.
!
!    Input, real ( kind = 8 ) ZD(NXD,NYD), the Z data.
!
!    Input, integer ( kind = 4 ) NI, the number of interpolation points.
!
!    Input, real ( kind = 8 ) XI(NI), YI(NI), the coordinates of the
!    interpolation points.
!
!    Output, real ( kind = 8 ) ZI(NI), the value of the interpolant.
!

  implicit none

  integer ( kind = 4 ) ni
  integer ( kind = 4 ) nxd
  integer ( kind = 4 ) nyd

  real ( kind = 8 ) alpha
  real ( kind = 8 ) beta
  real ( kind = 8 ) det
  real ( kind = 8 ) dxa
  real ( kind = 8 ) dxb
  real ( kind = 8 ) dxi
  real ( kind = 8 ) dya
  real ( kind = 8 ) dyb
  real ( kind = 8 ) dyi
  real ( kind = 8 ) gamma
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) r8_huge
  integer ( kind = 4 ) r8vec_bracket5
  real ( kind = 8 ) xd(nxd)
  real ( kind = 8 ) xi(ni)
  real ( kind = 8 ) yd(nyd)
  real ( kind = 8 ) yi(ni)
  real ( kind = 8 ) zd(nxd,nyd)
  real ( kind = 8 ) zi(ni)

  do k = 1, ni

    i = r8vec_bracket5 ( nxd, xd, xi(k) )
    if ( i == -1 ) then
      zi(k) = r8_huge ( )
      cycle
    end if

    j = r8vec_bracket5 ( nyd, yd, yi(k) )
    if ( j == -1 ) then
      zi(k) = r8_huge ( )
      cycle
    end if

    if ( yi(k) < yd(j+1) &
      + ( yd(j) - yd(j+1) ) * ( xi(i) - xd(i) ) / ( xd(i+1) - xd(i) ) ) then

      dxa = xd(i+1) - xd(i)
      dya = yd(j)   - yd(j)

      dxb = xd(i)   - xd(i)
      dyb = yd(j+1) - yd(j)

      dxi = xi(k)   - xd(i)
      dyi = yi(k)   - yd(j)

      det = dxa * dyb - dya * dxb

      alpha = ( dxi * dyb - dyi * dxb ) / det
      beta =  ( dxa * dyi - dya * dxi ) / det
      gamma = 1.0D+00 - alpha - beta

      zi(k) = alpha * zd(i+1,j) + beta * zd(i,j+1) + gamma * zd(i,j)

    else

      dxa = xd(i)   - xd(i+1)
      dya = yd(j+1) - yd(j+1)

      dxb = xd(i+1) - xd(i+1)
      dyb = yd(j)   - yd(j+1)

      dxi = xi(k)   - xd(i+1)
      dyi = yi(k)   - yd(j+1)

      det = dxa * dyb - dya * dxb

      alpha = ( dxi * dyb - dyi * dxb ) / det
      beta =  ( dxa * dyi - dya * dxi ) / det
      gamma = 1.0D+00 - alpha - beta

      zi(k) = alpha * zd(i,j+1) + beta * zd(i+1,j) + gamma * zd(i+1,j+1)

    end if

  end do

end subroutine pwl_interp_2d

function r8vec_bracket5 ( nd, xd, xi )

!*****************************************************************************80
!
!! R8VEC_BRACKET5 brackets data between successive entries of a sorted R8VEC.
!
!  Discussion:
!
!    We assume XD is sorted.
!
!    If XI is contained in the interval [XD(1),XD(N)], then the returned
!    value B indicates that XI is contained in [ XD(B), XD(B+1) ].
!
!    If XI is not contained in the interval [XD(1),XD(N)], then B = -1.
!
!    This code implements a version of binary search which is perhaps more
!    understandable than the usual ones.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 October 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ND, the number of data values.
!
!    Input, real ( kind = 8 ) XD(N), the sorted data.
!
!    Input, real ( kind = 8 ) XD, the query value.
!
!    Output, integer ( kind = 4 ) R8VEC_BRACKET5, the bracket information.
!
  implicit none

  integer ( kind = 4 ) nd

  integer ( kind = 4 ) b
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) r
  integer ( kind = 4 ) r8vec_bracket5
  real ( kind = 8 ) xd(nd)
  real ( kind = 8 ) xi

  if ( xi < xd(1) .or. xd(nd) < xi ) then

    b = -1

  else

    l = 1
    r = nd

    do while ( l + 1 < r )
      m = ( l + r ) / 2
      if ( xi < xd(m) ) then
        r = m
      else
        l = m
      end if
    end do

    b = l

  end if

  r8vec_bracket5 = b

end function r8vec_bracket5


  SUBROUTINE AKIMA_2D(k_ew_per, X10, Y10, Z1, X20, Y20, Z2, icall)
    !!
    !!
    !!================================================================
    !!
    !! INPUT :     k_ew_per : east-west periodicity
    !!                        k_ew_per = -1  --> no periodicity
    !!                        k_ew_per >= 0  --> periodicity with overlap of k_ew_per points
    !!             X10   : 2D input longitude array (ni*nj) or (ni*1) (must be regular!)
    !!             Y10   : 2D input latitude  array (ni*nj) or (nj*1) (must be regular!)
    !!             Z1    : input field on input grid
    !!
    !!             X20   : 2D output longitude array (ni*nj) or (ni*1) (can be irregular)
    !!             Y20   : 2D output latitude  array (ni*nj) or (nj*1) (can be irregular)
    !!
    !! OUTPUT :
    !!             Z2    : input field on output grid
    !!
    !! input  :   icall  : if=1, will always force 'l_first_call_akima' to .TRUE. |OPTIONAL|
    !!
    !!==============================================================================================
    !!
    !!
    !! Input/Output arguments
    !! ======================
    !!
    INTEGER,  INTENT(in)                 :: k_ew_per
    REAL(8), DIMENSION(:,:), INTENT(in)  :: X10, Y10                     !: input coordinates
    REAL(4), DIMENSION(:,:), INTENT(in)  :: Z1                           !: input data
    REAL(8), DIMENSION(:,:), INTENT(in)  :: X20, Y20
    REAL(4), DIMENSION(:,:), INTENT(out) :: Z2
    INTEGER,       OPTIONAL, INTENT(in)  :: icall
    !!
    !!
    !!
    !! Local variables
    !! ===============
    !!
    INTEGER :: nx1, ny1, nx2, ny2, ji, jj
    !!
    INTEGER, PARAMETER :: n_extd = 4    ! input grid extension
    !!
    LOGICAL :: l_x_found, l_y_found
    !!
    INTEGER :: &
         &     ji1, jj1, ji2, jj2,   &
         &     ni1, nj1
    !!
    REAL(8) :: r8_res
    !!
    !!
    REAL(8), DIMENSION(4)         ::    v4
    !!
    REAL(8), DIMENSION(:,:,:), ALLOCATABLE ::  poly
    !!
    REAL(8), DIMENSION(:,:), ALLOCATABLE ::    &
         &    X1, Y1, X2, Y2,   &
         &    Z_in , lon_in , lat_in
    !!
    REAL(8), DIMENSION(:,:), ALLOCATABLE ::    &
         &    Z_in_p4 , lon_in_p4 , lat_in_p4, &
         &    slpx, slpy, slpxy
    !!
    REAL(8) :: &
         &  px2, py2, &
         &  min_lon1, max_lon1, min_lat1, max_lat1,   &
         &  min_lon2, max_lon2, min_lat2, max_lat2
    !!
    CHARACTER(len=2) :: ctype

    IF ( present(icall) ) THEN
       IF ( icall == 1 ) THEN
          l_first_call_akima = .TRUE.
          l_always_first_call  = .TRUE.
       END IF
    END IF


    !!
    !! Create 2D (ni*nj) arrays out of 1d (ni*1 and nj*1) arrays if needed:
    !!
    ctype = TEST_XYZ(X10, Y10, Z1)
    nx1 = size(Z1,1) ; ny1 = size(Z1,2)
    ALLOCATE ( X1(nx1,ny1) , Y1(nx1,ny1) )
    !!
    IF ( ctype == '1d' ) THEN
       DO jj=1, ny1
          X1(:,jj) = X10(:,1)
       END DO
       DO ji=1, nx1
          Y1(ji,:) = Y10(:,1)
       END DO
    ELSE
       X1 = X10 ; Y1 = Y10
    END IF
    !!
    !!
    ctype = '00'
    ctype = TEST_XYZ(X20, Y20, Z2)
    nx2 = size(Z2,1) ; ny2 = size(Z2,2)
    ALLOCATE ( X2(nx2,ny2) , Y2(nx2,ny2) )
    !!
    IF ( ctype == '1d' ) THEN
       DO jj=1, ny2
          X2(:,jj) = X20(:,1)
       END DO
       DO ji=1, nx2
          Y2(ji,:) = Y20(:,1)
       END DO
    ELSE
       X2 = X20 ; Y2 = Y20
    END IF
    !!
    !!
    !!
    !!                       S T A R T
    !!
    !!
    !! In order to produce clean interpolation we extend initial space domains :
    ni1 = nx1 + n_extd  ;   nj1 = ny1 + n_extd
    !!
    ALLOCATE ( Z_in(ni1,nj1)       , lon_in(ni1,nj1) ,       lat_in(ni1,nj1),        &
         &     Z_in_p4(ni1+4,nj1+4), lon_in_p4(ni1+4,nj1+4), lat_in_p4(ni1+4,nj1+4), &
         &     slpx(ni1,nj1)       , slpy(ni1,nj1)         , slpxy(ni1,nj1),         &
         &     poly(ni1-1,nj1-1,nsys)                             )
    !!
    !!
    IF ( l_first_call_akima ) ALLOCATE ( ixy_pos(nx2, ny2, 2) )
    !!
    !!
    !! For cleaner results adding the extra band at 4 boundaries :
    CALL FILL_EXTRA_BANDS(k_ew_per, X1, Y1, REAL(Z1,8), lon_in, lat_in, Z_in)
    !!
    DEALLOCATE (X1, Y1)


    !! C r e a t i n g   e x t e n d e d   a r r a y s  (for slopes computation):
    !! ---------------------------------------------------------------------------
    CALL FILL_EXTRA_BANDS(-1, lon_in,   lat_in,   Z_in,    &
         &                    lon_in_p4, lat_in_p4, Z_in_p4)



    !! C o m p u t a t i o n   o f   p a r t i a l   d e r i v a t i v e s  :
    !! ----------------------------------------------------------------------
    !!
    CALL par_der(ni1, nj1, ni1+4, nj1+4, lon_in_p4, lat_in_p4, Z_in_p4, &
         &       slpx, slpy, slpxy)
    !!
    DEALLOCATE ( lon_in_p4, lat_in_p4, Z_in_p4 )
    !!
    !!
    CALL build_pol(ni1, nj1, lon_in, lat_in, Z_in, slpx, slpy, slpxy, poly)
    !!
    DEALLOCATE ( slpx, slpy, slpxy )
    !!
    !!
    !! Checking if output grid does not overlap input grid :
    min_lon1 = minval(lon_in) ;  max_lon1 = maxval(lon_in)
    min_lat1 = minval(lat_in) ;  max_lat1 = maxval(lat_in)
    min_lon2 = minval(X2)     ;  max_lon2 = maxval(X2)
    min_lat2 = minval(Y2)     ;  max_lat2 = maxval(Y2)
    !!
    !!
    !!
    DO ji2 = 1, nx2
       DO jj2 = 1, ny2
          !!
          !! The coordinates of current target point are (px2,py2) :
          px2 = X2(ji2,jj2) ;  py2 = Y2(ji2,jj2)
          !!
          !! Checking if this belongs to input domain :
          IF (   ( (px2 >= min_lon1).and.(px2 <= max_lon1)).and.  &
               & ( (py2 >= min_lat1).and.(py2 <= max_lat1))  )   THEN
             !!
             !!
             !! Laboriuously scanning the entire input grid to find
             !! location of treated point
             !! Let's find the 4 points of input grid that surrounds (px2,py2)
             !! Only if this is  the first time step (storing into ixy_pos) :
             IF ( l_first_call_akima ) THEN
                !!
                ji1 = 1 ; jj1 = 1
                !!
                l_x_found = .FALSE. ;  l_y_found = .FALSE.
                !!
                DO WHILE ( .NOT. (l_x_found .AND. l_y_found) )
                   !!
                   l_x_found = .FALSE.
                   !!
                   DO WHILE ( .NOT. l_x_found )
                      !!
                      IF (ji1 < ni1) THEN
                         !!
                         IF ((lon_in(ji1,jj1) <= px2).and.(lon_in(ji1+1,jj1) > px2)) THEN
                            l_x_found = .TRUE.
                         ELSE
                            ji1 = ji1+1
                         END IF
                         !!
                      ELSE   ! ji1 = ni1
                         ji1 = ji1-1  ! we are at the top need to use former pol.
                         l_x_found = .TRUE.
                      END IF
                      !!
                   END DO
                   !!
                   l_y_found = .FALSE.
                   !!
                   DO WHILE ( .NOT. l_y_found )
                      !!
                      IF ( jj1 < nj1 ) THEN
                         !!
                         IF ((lat_in(ji1,jj1) <= py2).and.(lat_in(ji1,jj1+1) > py2)) THEN
                            l_y_found = .TRUE.
                         ELSE
                            jj1 = jj1 + 1
                            l_x_found = .FALSE.
                            l_y_found = .TRUE. ! just so that we exit the loop on l_y_found
                         END IF
                         !!
                      ELSE   ! jj1 == nj1
                         jj1 = nj1-1        ! we are using polynome at (ji,nj1-1)
                         l_y_found = .TRUE. ! for extreme right boundary
                      END IF
                      !!
                   END DO
                   !!
                END DO
                !!
                !!      !3333            CONTINUE
                !!
                ixy_pos(ji2,jj2,:) = (/ ji1, jj1 /)
                !!
             ELSE
                !!
                !! We know the right location from time = 1 :
                ji1 = ixy_pos(ji2,jj2,1)
                jj1 = ixy_pos(ji2,jj2,2)
                !!
             END IF  !* IF ( l_first_call_akima )
             !!
             !!
             !! It's time to interpolate!
             !! =========================
             !!
             Z2(ji2,jj2) = REAL(pol_val(px2 - lon_in(ji1,jj1), py2 - lat_in(ji1,jj1), &
                  &                poly(ji1,jj1,:) ) , 4)   ! back to real(4)
             !!
          ELSE
             Z2(ji2,jj2) = 0.  ! point is not on input domain!
          END IF
          !!
       END DO
    END DO

    !! Deallocation :
    DEALLOCATE ( Z_in , lon_in , lat_in, poly, X2, Y2 )

    l_first_call_akima = .FALSE.

    IF ( l_always_first_call ) THEN
       DEALLOCATE ( ixy_pos )
       l_first_call_akima = .TRUE.
    END IF

  END SUBROUTINE AKIMA_2D

  !! =========================================================================
  !!
  !!
  !! ########################
  !! LOCAL PRIVATE ROUTINES :
  !! ########################
  !!
  !!
  SUBROUTINE par_der(ni, nj, nip4, njp4, ZX, ZY, ZZ, dZdX, dZdY, d2ZdXdY)
    !!
    !! Partial derivatives
    !!
    INTEGER, INTENT(in) :: ni, nj, nip4, njp4
    !!
    REAL(8), DIMENSION(nip4,njp4), INTENT(in) :: &
         &              ZX, &
         &              ZY, &
         &              ZZ
    !!
    !!
    REAL(8), DIMENSION(ni,nj)    , INTENT(out) ::   &
         &               dZdX ,     &
         &               dZdY ,     &
         &               d2ZdXdY
    !!
    !! Local variables :
    INTEGER  :: ji, jj
    REAL(8) :: m1, m2, m3, m4
    REAL(8) :: Wx2, Wx3, Wy2, Wy3
    REAL(8) :: d22, e22, d23, e23, d42, e32, d43, e33
    !!
    !!
    !!
    !! Treating middle of array ( at least to points away from the bordures ) :
    !!-------------------------------------------------------------------------
    DO jj=1, nj
       DO ji=1, ni
          !!
          !!   SLOPE / X :
          !!   ***********
          m1 = (ZZ(ji+1,jj+2) - ZZ(ji,  jj+2))/(ZX(ji+1,jj+2) - ZX(ji,  jj+2))
          m2 = (ZZ(ji+2,jj+2) - ZZ(ji+1,jj+2))/(ZX(ji+2,jj+2) - ZX(ji+1,jj+2))
          m3 = (ZZ(ji+3,jj+2) - ZZ(ji+2,jj+2))/(ZX(ji+3,jj+2) - ZX(ji+2,jj+2))
          m4 = (ZZ(ji+4,jj+2) - ZZ(ji+3,jj+2))/(ZX(ji+4,jj+2) - ZX(ji+3,jj+2))
          !!
          IF ( (m1 == m2).and.(m3 == m4) ) THEN
             dZdX(ji,jj) = 0.5*(m2 + m3)
          ELSE
             Wx2 = ABS(m4 - m3)
             Wx3 = ABS(m2 - m1)
             dZdX(ji,jj) = ( Wx2*m2 + Wx3*m3 ) / ( Wx2 + Wx3 )
          END IF
          !!
          !!
          !!   SLOPE / Y :
          !!   ***********
          m1 = (ZZ(ji+2,jj+1) - ZZ(ji+2,jj  ))/(ZY(ji+2,jj+1) - ZY(ji+2,jj  ))
          m2 = (ZZ(ji+2,jj+2) - ZZ(ji+2,jj+1))/(ZY(ji+2,jj+2) - ZY(ji+2,jj+1))
          m3 = (ZZ(ji+2,jj+3) - ZZ(ji+2,jj+2))/(ZY(ji+2,jj+3) - ZY(ji+2,jj+2))
          m4 = (ZZ(ji+2,jj+4) - ZZ(ji+2,jj+3))/(ZY(ji+2,jj+4) - ZY(ji+2,jj+3))
          !!
          !!
          IF ( (m1 == m2).and.(m3 == m4) ) THEN
             dZdY(ji,jj) = 0.5*(m2 + m3)
          ELSE
             Wy2 =  ABS(m4 - m3)
             Wy3 =  ABS(m2 - m1)
             dZdY(ji,jj) =   ( Wy2*m2 + Wy3*m3 ) / ( Wy2 + Wy3 )
          END IF
          !!
          !!
          !!   CROSS DERIVATIVE /XY :
          !!   **********************
          !! d22 = d(i-1,j-1) = [ z(i-1,j)-z(i-1,j-1) ] / [ y(j) - y(j-1) ]
          d22 = (ZZ(ji+1,jj+2) - ZZ(ji+1,jj+1))/(ZY(ji+1,jj+2) - ZY(ji+1,jj+1))
          !!
          !! d23 = d(i-1 , j) = [ z(i-1,j+1)-z(i-1,j) ] / [ y(j+1) - y(j) ]
          d23 = (ZZ(ji+1,jj+3) - ZZ(ji+1,jj+2))/(ZY(ji+1,jj+3) - ZY(ji+1,jj+2))
          !!
          !! d42 = d(i+1 , j-1) = [ z(i+1 , j) - z(i+1 , j-1) ] / [ y(j) - y(j-1) ]
          d42 = (ZZ(ji+3,jj+2) - ZZ(ji+3,jj+1))/(ZY(ji+3,jj+2) - ZY(ji+3,jj+1))
          !!
          !! d43 = d(i+1 , j) = [ z(i+1 , j+1)-z(i+1 , j) ] / [ y(j+1) - y(j) ]
          d43 = (ZZ(ji+3,jj+3) - ZZ(ji+3,jj+2))/(ZY(ji+3,jj+3) - ZY(ji+3,jj+2))
          !!
          !!
          !! e22  = [ m2 - d22 ] / [ x(i) - x(i-1) ]
          e22  = ( m2 - d22 ) / ( ZX(ji+2,jj+1) - ZX(ji+1,jj+1) )
          !! e23  = [ m3 - d23 ] / [ x(i) - x(i-1) ]
          e23  = ( m3 - d23 ) / ( ZX(ji+2,jj+2) - ZX(ji+1,jj+2) )
          !! e32  = [ d42 - m2 ] / [ x(i+1) - x(i) ]
          e32  = ( d42 - m2 ) / ( ZX(ji+3,jj+2) - ZX(ji+2,jj+2) )
          !! e33  = [ d43 - m3 ] / [ x(i+1) - x(i) ]
          e33  = ( d43 - m3 ) / ( ZX(ji+3,jj+2) - ZX(ji+2,jj+2) )
          !!
          !!
          IF ( ((Wx2 == 0).and.(Wx3 == 0)).or.((Wy2 == 0).and.(Wy3 == 0)) ) THEN
             IF ( (Wx2 == 0).and.(Wx3 == 0) ) THEN
                Wx2 = 1.  ; Wx3 = 1.
             END IF
             !!
             IF ( (Wy2 == 0).and.(Wy3 == 0) ) THEN
                Wy2 = 1.  ; Wy3 = 1.
             END IF
             !!
          END IF
          !!
          !!
          d2ZdXdY(ji,jj) = ( Wx2*(Wy2*e22 + Wy3*e23) + Wx3*(Wy2*e32 + Wy3*e33) )  &
               &           / ( (Wx2 + Wx3) * (Wy2 + Wy3) )
          !!
          !!
       END DO
    END DO
    !!
  END SUBROUTINE par_der
  !!
  !!
  !!
  !!################################################################################
  !!
  !!
  !!
  !!
  !!
  !!
  !!
  !!
  !!#######################################################################
  !!
  !!
  !!
  SUBROUTINE build_pol(ni, nj, zx, zy, zz, sx, sy, sxy, poly)
    !!
    !!==================================================================
    !!
    !! Compute the 16 coefficients of the polynomes used to interpolate
    !!
    !!==================================================================
    !!
    INTEGER, INTENT(in) :: ni, nj
    REAL(8), DIMENSION(ni,nj), INTENT(in) :: zx, zy, zz, sx, sy, sxy
    REAL(8), DIMENSION(ni-1,nj-1,nsys), INTENT(out) ::   poly
    !!
    !! Local variables :
    !! -----------------
    REAL(8), DIMENSION(nsys-4) :: V12X
    REAL(8), DIMENSION(nsys)   :: VX
    INTEGER                     :: ji, jj
    !!
    REAL(8) :: &
         &   x, x2, x3, y, y2, y3, xy, &
         &   b1, b2, b3, b4, b5, b6, b7, b8, &
         &   b9, b10, b11, b12, b13, b14, b15, b16, &
         !!
         &   c1, c2, c3, c4, c5, c6, c7, c8, &
         &   c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, &
         &   d1, d2, d3, d4, d5, d6, d7, d8, d9, &
         &   f1, f2, f3, f4, f5, f6

    !!
    !!
    DO ji=1, ni-1
       DO jj=1, nj-1
          !!
          VX = 0.
          !!
          !! Local dx and dy :
          x = zx(ji+1,jj) - zx(ji,jj)
          y = zy(ji,jj+1) - zy(ji,jj)
          !!
          x2 = x*x ; x3 = x2*x
          y2 = y*y ; y3 = y2*y
          xy = x*y
          !!
          !! Vector B, value at each points, d/dx, d/dy and d2/dxdy :
          b1  =  zz(ji,jj) ; b2  =  zz(ji+1,jj) ;  b3  =  zz(ji+1,jj+1) ; b4  =  zz(ji,jj+1)
          b5  =  sx(ji,jj) ; b6  =  sx(ji+1,jj) ;  b7  =  sx(ji+1,jj+1) ; b8  =  sx(ji,jj+1)
          b9  =  sy(ji,jj) ; b10 =  sy(ji+1,jj) ;  b11 =  sy(ji+1,jj+1) ; b12 =  sy(ji,jj+1)
          b13 = sxy(ji,jj) ; b14 = sxy(ji+1,jj) ;  b15 = sxy(ji+1,jj+1) ; b16 = sxy(ji,jj+1)
          !!
          !!
          !!
          !!
!
! System 16x16 :
! ==============
!
!             (/ 0.    0.    0.   0.  0.    0.    0.   0. 0.   0.   0.  0. 0.  0. 0. 1. /)
!             (/ 0.    0.    0.   x^3  0.    0.    0.   x^2 0.   0.   0.  x  0.  0. 0. 1. /)
!             (/ x^3*y^3 x^3*y^2 x^3*y x^3  x^2*y^3 x^2*y^2 x^2*y x^2 x*y^3 x*y^2 x*y x  y^3  y^2 y  1. /)
!             (/ 0.    0.    0.   0.  0.    0.    0.   0. 0.   0.   0.  0. y^3  y^2 y  1. /)
!             (/ 0.      0.      0.     0.   0.     0.     0.    0.  0. 0. 0. 1. 0. 0. 0. 0. /)
!             (/ 0.      0.      0.     3*x^2 0.     0.     0.    2*x 0. 0. 0. 1. 0. 0. 0. 0. /)
!             (/ 3*x^2*y^3 3*x^2*y^2 3*x^2*y 3*x^2 2*x*y^3 2*x*y^2 2*x*y 2*x y^3 y^2 y  1. 0. 0. 0. 0. /)
!         A = (/ 0.      0.      0.     0.   0.     0.     0.    0.  y^3 y^2 y  1. 0. 0. 0. 0. /)
!             (/ 0.      0.     0.  0. 0.      0.     0. 0. 0.     0.    0. 0. 0.   0.  1. 0. /)
!             (/ 0.      0.     x^3  0. 0.      0.     x^2 0. 0.     0.    x  0. 0.   0.  1. 0. /)
!             (/ 3*x^3*y^2 2*x^3*y x^3  0. 3*x^2*y^2 2*x^2*y x^2 0. 3*x*y^2 2*x*y x  0. 3*y^2 2*y 1. 0. /)
!             (/ 0.      0.     0.  0. 0.      0.     0. 0. 0.     0.    0. 0. 3*y^2 2*y 1. 0. /)
!             (/ 0.      0.     0.   0. 0.     0.    0.  0. 0.   0.  1. 0. 0. 0. 0. 0. /)
!             (/ 0.      0.     3*x^2 0. 0.     0.    2*x 0. 0.   0.  1. 0. 0. 0. 0. 0. /)
!             (/ 9*x^2*y^2 6*x^2*y 3*x^2 0. 6*x*y^2 4*x*y 2*x 0. 3*y^2 2*y 1. 0. 0. 0. 0. 0. /)
!             (/ 0.      0.     0.   0. 0.     0.    0.  0. 3*y^2 2*y 1. 0. 0. 0. 0. 0. /)
!
!
! X = (/ a33, a32, a31, a30, a23, a22, a21, a20, a13, a12, a11, a10, a03, a02, a01, a00 /)
!
! B = (/ b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16  /)
!
!
          !!
          !!
          !!
          !! 1) D'abord, calculer les 4 inconnsues (du système à 16 éqs)
          !! qui dépendent directement des second membres bj
          !! (avant de faire la modification des bj):
          vx(11) = b13 ; vx(12) = b5 ; vx(15) = b9 ; vx(16) = b1
          !!
          !!
          !! 2) Ensuite, mettre à échelle les seconds membres bj
          !! (b1 à b4 restent inchangés):
          b5 = x*b5    ; b6 = x*b6    ; b7 = x*b7    ; b8 = x*b8
          b9 = y*b9    ; b10 = y*b10  ; b11 = y*b11  ; b12 = y*b12
          b13 = xy*b13 ; b14 = xy*b14 ; b15 = xy*b15 ; b16 = xy*b16
          !!
          !!
          !! 3) Puis, résoudre le système avec x=1 et y=1
          !! (ou bien utiliser ta solution symbolique avec x=1 et y=1),
          !! en remarquant que, dans ce cas particulier,
          !! certaines combinaisons apparaissent souvent,
          !! et qu'il vaut mieux les calculer d'abord:
          !!
          !! a) Premierement:
          c1=b1-b2      ; c2=b3-b4      ; c3=b5+b6
          c4=b7+b8      ; c5=b9-b10     ; c6=b11-b12
          c7=b13+b14    ; c8=b15+b16    ; c9=2*b5+b6
          c10=b7+2*b8   ; c11=2*b13+b14 ; c12=b15+2*b16
          c13=b5-b8     ; c14=b1-b4     ; c15=b13+b16
          c16= 2*b13 + b16 ; c17=b9+b12    ; c18=2*b9+b12
          !!
          !! b) Deuxiemement:
          d1=c1+c2   ; d2=c3-c4   ; d3=c5-c6
          d4=c7+c8   ; d5=c9-c10  ; d6=2*c5-c6
          d7=2*c7+c8 ; d8=c11+c12 ; d9=2*c11+c12
          !!
          !! c) Troisiemement:
          f1=2*d1+d2 ; f2=2*d3+d4 ; f3=2*d6+d7
          f4=3*d1+d5 ; f5=3*d3+d8 ; f6=3*d6+d9
          !!
          !! d) De sorte que la solution s'écrit simplement:
          vx(1)=2*f1+f2       ; vx(2)=-(3*f1+f3) ; vx(3)=2*c5+c7
          vx(4)=2*c1+c3       ; vx(5)=-(2*f4+f5) ; vx(6)=3*f4+f6
          vx(7)=-(3*c5+c11)   ; vx(8)=-(3*c1+c9) ; vx(9)=2*c13+c15
          vx(10)=-(3*c13+c16) ; vx(13)=2*c14+c17 ; vx(14)=-(3*c14+c18)
          !!
          !! On remarque même que les seules mulitplications qui apparaissent
          !! sont des doublements ou des triplements, qu'on pourrait donc
          !! remplacer par des additions, par ex.: cc14=c14+c14, puis ccc14=cc14+c14,
          !! et donc résoudre le systeme simplifié sans aucune multiplication
          !! ni division, ce qui est tout de même assez émouvant :-)
          !!
          !! 4) Et finalement, il ne reste plus qu'à remettre à échelle la solution:
          vx(1)=vx(1)/(x3*y3) ; vx(2)=vx(2)/(x3*y2) ; vx(3)=vx(3)/(x3*y) ; vx(4)=vx(4)/x3
          vx(5)=vx(5)/(x2*y3) ; vx(6)=vx(6)/(x2*y2) ; vx(7)=vx(7)/(x2*y) ; vx(8)=vx(8)/x2
          vx(9)=vx(9)/(x*y3)  ; vx(10)=vx(10)/(x*y2); vx(13)=vx(13)/y3   ; vx(14)=vx(14)/y2
          !!
          !! Bien sûr, je n'ai pas vérifié et j'ai pu faire des erreurs de calcul
          !! ou de recopiage, mais le principe me semble bon, et on aboutit
          !! à un nombre d'opérations:
          !! Etape 2: 9x
          !! Etape 3: 45+, 26x (ou même simplement 69+)
          !! Etape 4: 8x, 12/
          !! Soit un total de: 45+, 43x, 12/ pour résoudre le systeme,
          !! au lieu de: 84+, 140x, 16/ pour la solution symbolique de SOSIE.
          !!
          !! Je suis conscient que ça ne sert sans doute pas à grand chose
          !! car le calcul n'est fait que pour chaque point de la grille de départ
          !! (si je comprends bien) et ce coût n'était probablement déjà plus dominant.
          !! Il était déjà largement dépassé (j'imagine) par le coût d'évaluation
          !! des polynômes aux noeuds de la grille d'arrivée, du moins quand
          !! sa résolution est beaucoup plus fine que celle de la grille de départ.
          !!





          !! Storing the 16 coefficients of the polynome for point [ji,jj] :
          poly(ji,jj,:) = VX(:)
          !!
       END DO
    END DO
    !!
  END SUBROUTINE build_pol
  !!
  !!
  !!
  !!
  !!
  FUNCTION pol_val(x, y, V)
    !!
    !!======================================================
    !!
    !! Give value of polynome pol_val(x,y), polynome coefficients
    !! are stored in to vector V
    !!
    !! Optimizing by using Horner's scheme (http://en.wikipedia.org/wiki/Horner_scheme)
    !!
    !!  "It has been shown that the Horner scheme is optimal,
    !!  in the sense that any algorithm to evaluate an arbitrary polynomial
    !!  must use at least as many operations.
    !!  That the number of additions required is minimal was shown
    !!  by Alexander Ostrowski in 1954; that the number of multiplications
    !!  is minimal by Victor Pan."
    !!
    !! => we perform the minimum number of multiplications possible: 15 !
    !!
    !! Big thanks to Jean-Michel Brankart (Jean-Michel.Brankart@hmg.inpg.fr) for
    !! adapting this scheme to Sosie.
    !!
    !!=====================================================================
    !!
    REAL(8)              :: pol_val
    REAL(8), INTENT(in)                  :: x, y
    REAL(8), DIMENSION(nsys), INTENT(in) :: V
    !!
    REAL(8) :: p1, p2, p3, p4
    !!
    p1      = ( ( V(1)  * y + V(2)  ) * y + V(3)  ) * y + V(4)
    p2      = ( ( V(5)  * y + V(6)  ) * y + V(7)  ) * y + V(8)
    p3      = ( ( V(9)  * y + V(10) ) * y + V(11) ) * y + V(12)
    p4      = ( ( V(13) * y + V(14) ) * y + V(15) ) * y + V(16)
    pol_val = ( ( p1    * x + p2    ) * x + p3    ) * x + p4
    !!
  END FUNCTION pol_val

  FUNCTION TEST_XYZ(rx, ry, rz)
    !!
    !! Testing if 2D coordinates or 1D, and if match shape of data...
    !!
    CHARACTER(len=2) :: TEST_XYZ
    !!
    REAL(8), DIMENSION(:,:), INTENT(in) :: rx, ry
    REAL(4), DIMENSION(:,:), INTENT(in) :: rz
    !!
    INTEGER :: ix1, ix2, iy1, iy2, iz1, iz2
    !!
    ix1 = size(rx,1) ; ix2 = size(rx,2)
    iy1 = size(ry,1) ; iy2 = size(ry,2)
    iz1 = size(rz,1) ; iz2 = size(rz,2)
    !!
    IF ( (ix2 == 1).AND.(iy2 == 1) ) THEN
       !!
       IF ( (ix1 == iz1).AND.(iy1 == iz2) ) THEN
          TEST_XYZ = '1d'
       ELSE
          PRINT *, 'ERROR, mod_drown.F90 = >TEST_XYZ 1 : longitude and latitude array do not match data!'
          PRINT *, ''; STOP
       END IF
       !!
    ELSE
       IF ( (ix1 == iz1).AND.(iy1 == iz1).AND.(ix2 == iz2).AND.(iy2 == iz2) ) THEN
          TEST_XYZ = '2d'
       ELSE
          PRINT *, 'ERROR, mod_drown.F90 = >TEST_XYZ 2 : longitude and latitude array do not match data!'
          PRINT *, ''; STOP
       END IF
    END IF
    !!
    !!
  END FUNCTION TEST_XYZ

end module interpolators
