    module mod_nasa_quat
    use, intrinsic :: iso_fortran_env
    implicit none

    integer ( int32 ), parameter :: dim_num = 3
    
    contains

    ! *****************************************************************************
    ! ** REAL FUNCTIONS                                                         **
    ! *****************************************************************************

    
    pure function r8_cross_f(a,b) result(c)
    real(real64), intent(in) :: a(3), b(3)
    real(real64) :: c(3)
        c = [ a(2)*b(3) - a(3)*b(2), &
              a(3)*b(1) - a(1)*b(3), &
              a(1)*b(2) - a(2)*b(1) ] 
    end function
    
    pure function quat_cross_f(a,b) result(c)
    real(real64), intent(in) :: a(4), b(4)
    real(real64) :: c(4)
        c = [ 0.0_real64, r8_cross_f(a(2:4), b(2:4)) ]
    end function
    
    ! *****************************************************************************
    ! ** QUATERNION FUNCTIONS                                                    **
    ! *****************************************************************************
    
    subroutine quat_normal_01 ( seed, q ) 
    use mod_common, only : pi
    use mod_fortran, only : call_uniform_array_v
    !*****************************************************************************80
    !
    !! Q8_NORMAL_01 returns a normally distributed quaternion.
    !
    !  Discussion:
    !
    !    The normal distribution with mean 0 and variance 1 is used.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    22 July 2018
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input/output, integer ( int32 ) SEED, a seed for the random number
    !    generator.
    !
    !    Output, real ( real64 ) Q(4), the sampled quaternion.
    !
    implicit none

    integer ( int32 ), intent(inout) :: seed
    real ( real64 ), intent(out) :: q(4)
    real ( real64 ) r(4)
    !real ( real64 ), parameter :: pi = 3.141592653589793D+00

    call call_uniform_array_v ( 4, seed, r )

    q(1:3:2) = &
        sqrt ( - 2.0D+00 * log ( r(1:3:2) ) ) * cos ( 2.0D+00 * pi * r(2:4:2) )

    q(2:4:2) = &
        sqrt ( - 2.0D+00 * log ( r(1:3:2) ) ) * sin ( 2.0D+00 * pi * r(2:4:2) )
    
    return
    end    
    
    pure subroutine quat_scalar(q, s)
    real(real64), intent(in) :: q(4)
    real(real64), intent(out) :: s

    s = q(1)

    end subroutine
    
    pure subroutine quat_vector(q, v) 
    real(real64), intent(in) :: q(4)
    real(real64), intent(out) :: v(3)

    v = q(2:4)

    end subroutine

    ! *****************************************************************************
    ! ** QUATERNION FUNCTIONS                                                    **
    ! *****************************************************************************

    pure subroutine quat_conjugate ( q, q2 ) 
    !*****************************************************************************80
    !
    !! Q8_CONJUGATE conjugates a quaternion.
    !
    !  Discussion:
    !
    !    A quaternion is a quadruplet (A,B,C,D) of real numbers, which
    !    may be written as
    !
    !      Q = A + Bi + Cj + Dk.
    !
    !    The conjugate of Q is
    !
    !      conj ( Q ) = A - Bi - Cj - Dk.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    04 August 2018
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real ( real64 ) Q(4), the quaternion to be conjugated.
    !
    !    Output, real ( real64 ) Q2(4), the conjugated quaternion.
    !
    implicit none

    real ( real64 ), intent(in) :: q(4)
    real ( real64 ), intent(out) :: q2(4)

    q2(1) =  q(1)
    q2(2:4) = -q(2:4)

    return
    end
    
    pure subroutine quat_exponentiate ( q1, q2 )    
    !*****************************************************************************80
    !
    !! Q8_EXPONENTIATE exponentiates a quaternion.
    !
    !  Discussion:
    !
    !    A quaternion is a quadruplet (A,B,C,D) of real numbers, which
    !    may be written as
    !
    !      Q = A + Bi + Cj + Dk.
    !
    !    The exponential of Q can be set by
    !      V = sqrt ( B^2 + C^2 + D^2 )
    !      e^Q = e^A * ( cos ( ||V|| ) + V/||V|| sin ||V|| )
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    04 August 2018
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real ( real64 ) Q1(4), the quaternions to exponentiate.
    !
    !    Output, real ( real64 ) Q2(4), the exponential of the quaternion.
    !
    implicit none

    real ( real64 ), intent(in) :: q1(4)
    real ( real64 ), intent(out) :: q2(4)
    real ( real64 ) v(3)
    real ( real64 ) v_norm

    v = q1(2:4)
    v_norm = sqrt ( sum ( v(1:3) ** 2 ) )

    q2(1) = cos ( v_norm )
    if ( v_norm /= 0.0D+00 ) then
        q2(2:4) = sin ( v_norm ) * v / v_norm
    else
        q2(2:4) = 0.0D+00
    end if

    q2 = exp ( q1(1) ) * q2

    return
    end
    pure subroutine quat_inverse ( q, q2 )
    !*****************************************************************************80
    !
    !! Q8_INVERSE inverts a quaternion.
    !
    !  Discussion:
    !
    !    A quaternion is a quadruplet (A,B,C,D) of real numbers, which
    !    may be written as
    !
    !      Q = A + Bi + Cj + Dk.
    !
    !    The inverse of Q is
    !
    !      inverse ( Q ) = conjugate ( Q ) / ( norm ( Q ) )^2.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    04 August 2018
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real ( real64 ) Q(4), the quaternion to be inverted.
    !
    !    Output, real ( real64 ) Q2(4), the inverse of the input quaternion.
    !
    implicit none

    real ( real64 ), intent(in) :: q(4)
    real ( real64 ), intent(out) :: q2(4)

    q2(1:4) = q(1:4) / sum ( q(1:4) ** 2 )
    q2(2:4) = - q2(2:4)

    return
    end    
    
    pure subroutine quat_multiply ( q1, q2, q3 )
    !*****************************************************************************80
    !
    !! Q8_MULTIPLY multiplies two quaternions.
    !
    !  Discussion:
    !
    !    A quaternion is a quadruplet (A,B,C,D) of real numbers, which
    !    may be written as
    !
    !      Q = A + Bi + Cj + Dk.
    !
    !    To multiply two quaternions, use the relationships:
    !
    !      i * j = -j * i = k
    !      j * k = -k * j = i
    !      k * i = -i * k = j
    !      i * i =  j * j = k * k = -1
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    04 August 2018
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real ( real64 ) Q1(4), Q2(4), the quaternions to be multiplied.
    !
    !    Output, real ( real64 ) Q3(4), the product of the two quaternions.
    !
    implicit none

    real ( real64 ), intent(in) :: q1(4)
    real ( real64 ), intent(in) :: q2(4)
    real ( real64 ), intent(out) :: q3(4)

    q3(1) = q1(1) * q2(1) - q1(2) * q2(2) - q1(3) * q2(3) - q1(4) * q2(4)
    q3(2) = q1(1) * q2(2) + q1(2) * q2(1) + q1(3) * q2(4) - q1(4) * q2(3)
    q3(3) = q1(1) * q2(3) - q1(2) * q2(4) + q1(3) * q2(1) + q1(4) * q2(2)
    q3(4) = q1(1) * q2(4) + q1(2) * q2(3) - q1(3) * q2(2) + q1(4) * q2(1)

    return
    end
    pure subroutine quat_multiply2 ( q1, q2, q3 )
    
    !*****************************************************************************80
    !
    !! Q8_MULTIPLY2 multiplies two quaternions using a matrix format.
    !
    !  Discussion:
    !
    !    A quaternion is a quadruplet (A,B,C,D) of real numbers, which
    !    may be written as
    !
    !      Q = A + Bi + Cj + Dk.
    !
    !    To multiply two quaternions, use the relationships:
    !
    !      i * j = -j * i = k
    !      j * k = -k * j = i
    !      k * i = -i * k = j
    !      i * i =  j * j = k * k = -1
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    22 July 2018
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real ( real64 ) Q1(4), Q2(4), the quaternions to be multiplied.
    !
    !    Output, real ( real64 ) Q3(4), the product of the two quaternions.
    !
    implicit none

    real ( real64 ), intent(in) :: q1(4)
    real ( real64 ), intent(in) :: q2(4)
    real ( real64 ), intent(out) :: q3(4)
    real ( real64 ) qm(4,4)
    !
    !  The matrix entries are listed by column, not row.
    !
    qm = reshape ( (/ &
        q1(1),  q1(2),  q1(3),  q1(4), &
        -q1(2), +q1(1), +q1(4), -q1(3), &
        -q1(3), -q1(4), +q1(1), +q1(2), &
        -q1(4), +q1(3), -q1(2), +q1(1) /), (/ 4, 4 /) )

    q3 = matmul ( qm, q2 )

    return
    end
    pure function quat_norm ( q ) 
    
    !*****************************************************************************80
    !
    !! Q8_NORM computes the norm of a quaternion.
    !
    !  Discussion:
    !
    !    A quaternion is a quadruplet (A,B,C,D) of real numbers, which
    !    may be written as
    !
    !      Q = A + Bi + Cj + Dk.
    !
    !    The norm of Q is
    !
    !      norm ( Q ) = sqrt ( A * A + B * B + C * C + D * D ).
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    03 August 2018
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real ( real64 ) Q(4), the quaternion.
    !
    !    Output, real ( real64 ) Q8_NORM, the norm of the quaternion.
    !
    implicit none

    real ( real64 ), intent(in) :: q(4)
    real ( real64 ) quat_norm

    quat_norm = sqrt ( sum ( q(1:4) ** 2 ) )

    return
    end
    pure subroutine quat_dot ( q1, q2, s )
    !*****************************************************************************80
    !
    !! Q8_DOT computes the inner product of two quaternions.
    !
    !  Discussion:
    !
    !    A quaternion is a quadruplet (A,B,C,D) of real numbers, which
    !    may be written as
    !
    !      Q = A + Bi + Cj + Dk.
    !
    !    The dot of Q1 and Q2 is
    !
    !      dot ( Q1, Q2 ) = A1 * A2 + B1 * B2 + C1 * C2 + D1 * D2.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    08 August 2024
    !
    !  Author:
    !
    !    John Alexiou
    !
    !  Parameters:
    !
    !    Input, real ( real64 ) Q1(4), the first quaternion.
    !
    !    Input, real ( real64 ) Q2(4), the second quaternion.
    !
    !    Output, real ( real64 ) S, the dot of the two quaternions.
    !
    implicit none

    real ( real64 ), intent(in) :: q1(4), q2(4)
    real ( real64 ), intent(out) :: s

    s = dot_product(q1, q2)

    return
    end
    pure subroutine quat_cross ( q1, q2, v )
    !*****************************************************************************80
    !
    !! Q8_DOT computes the cross product of two quaternions.
    !
    !  Discussion:
    !
    !    A quaternion is a quadruplet (A,B,C,D) of real numbers, which
    !    may be written as
    !
    !      Q = A + Bi + Cj + Dk.
    !
    !    The dot of Q1 and Q2 is
    !
    !      cross ( Q1, Q2 ) = [ Y1*Z2-Z1*Y2, Z1*X2-X1*Z2, X1*Y2-Y1*X2 ]
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    08 August 2024
    !
    !  Author:
    !
    !    John Alexiou
    !
    !  Parameters:
    !
    !    Input, real ( real64 ) Q1(4), the first quaternion.
    !
    !    Input, real ( real64 ) Q2(4), the second quaternion.
    !
    !    Output, real ( real64 ) V, the cross of the two quaternions.
    !
    implicit none

    real ( real64 ), intent(in) :: q1(4), q2(4)
    real ( real64 ), intent(out) :: v(3)

        v = [ &
              q1(3)*q2(4) - q1(4)*q2(3), &
              q1(4)*q2(2) - q1(2)*q2(4), &
              q1(2)*q2(3) - q1(3)*q2(2) ]

    return
    end
    
    pure subroutine quat_derivative(q,omg,qp)
    real ( real64 ), intent(in)  :: q(4), omg(3)
    real ( real64 ), intent(out) :: qp(4)
    real ( real64 ) :: q_omg(4)
    
        q_omg = [0.0_real64, omg/2]
        call quat_multiply(q, q_omg, qp)
    
    return
    end

    ! *****************************************************************************
    ! ** ROTATION FUNCTIONS                                                      **
    ! *****************************************************************************
    
    subroutine rotate_normal_01 ( seed, q ) 
    use mod_common, only : pi
    use mod_fortran, only : call_uniform_array_v
    !*****************************************************************************80
    !
    !! Q8_NORMAL_01 returns a normally distributed quaternion.
    !
    !  Discussion:
    !
    !    The normal distribution with mean 0 and variance 1 is used.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    22 July 2018
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input/output, integer ( int32 ) SEED, a seed for the random number
    !    generator.
    !
    !    Output, real ( real64 ) Q(4), the sampled quaternion.
    !
    implicit none

    integer ( int32 ), intent(inout) :: seed
    real ( real64 ), intent(out) :: q(4)
    real ( real64 ) r(4), qt(4)
    !real ( real64 ), parameter :: pi = 3.141592653589793D+00

    call call_uniform_array_v ( 4, seed, r )

    qt(1:3:2) = &
        sqrt ( - 2.0D+00 * log ( r(1:3:2) ) ) * cos ( 2.0D+00 * pi * r(2:4:2) )

    qt(2:4:2) = &
        sqrt ( - 2.0D+00 * log ( r(1:3:2) ) ) * sin ( 2.0D+00 * pi * r(2:4:2) )
    
        call rotate_normalize(qt, q)

    return
    end    
    
    pure subroutine rotate_normalize(qt,q) 
    real ( real64 ), intent(in) :: qt(4)
    real ( real64 ), intent(out) :: q(4)
    real ( real64 ) :: m
        m = quat_norm(qt)
        if( m>0.0D+00) then
            q = qt/m
        else
            q = qt
        end if
    return
    end    
    
    pure subroutine rotate_axis_vector ( axis, angle, v, w ) 
    !*****************************************************************************80
    !
    !! ROTATION_AXIS_VECTOR rotates a vector around an axis vector in 3D.
    !
    !  Discussion:
    !
    !    Thanks to Cody Farnell for correcting some mistakes in an earlier
    !    version of this routine.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    18 May 2007
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real ( real64 ) AXIS(3), the axis vector for the rotate.
    !
    !    Input, real ( real64 ) ANGLE, the angle, in radians, of the rotate.
    !
    !    Input, real ( real64 ) V(3), the vector to be rotated.
    !
    !    Output, real ( real64 ) W(3), the rotated vector.
    !
    implicit none

    real ( real64 ), intent(in), value :: angle
    real ( real64 ), intent(in) :: axis(dim_num)
    real ( real64 ) axis_norm
    real ( real64 ) dot
    real ( real64 ) norm
    real ( real64 ) normal(dim_num)
    real ( real64 ) normal_component
    real ( real64 ) normal2(dim_num)
    real ( real64 ) parallel(dim_num)
    real ( real64 ) rot(dim_num)
    real ( real64 ) u(dim_num)
    real ( real64 ), intent(in) :: v(dim_num)
    real ( real64 ), intent(out) :: w(dim_num)
    !
    !  Compute the length of the rotate axis.
    !
    u(1:dim_num) = axis(1:dim_num)

    axis_norm = sqrt ( sum ( u(1:dim_num) ** 2 ) )

    if ( axis_norm == 0.0D+00 ) then
        w(1:dim_num) = 0.0D+00
        return
    end if

    u(1:dim_num) = u(1:dim_num) / axis_norm
    !
    !  Compute the dot product of the vector and the unit rotate axis.
    !
    dot = dot_product ( u(1:dim_num), v(1:dim_num) )
    !
    !  Compute the parallel component of the vector.
    !
    parallel(1:dim_num) = dot * u(1:dim_num)
    !
    !  Compute the normal component of the vector.
    !
    normal(1:dim_num) = v(1:dim_num) - parallel(1:dim_num)

    normal_component = sqrt ( sum ( normal(1:dim_num) ** 2 ) )

    if ( normal_component == 0.0D+00 ) then
        w(1:dim_num) = parallel(1:dim_num)
        return
    end if

    normal(1:dim_num) = normal(1:dim_num) / normal_component
    !
    !  Compute a second vector, lying in the plane, perpendicular
    !  to V, and forming a right-handed system, as the cross product
    !  of the first two vectors.
    !
    normal2(1) = u(2) * normal(3) - u(3) * normal(2)
    normal2(2) = u(3) * normal(1) - u(1) * normal(3)
    normal2(3) = u(1) * normal(2) - u(2) * normal(1)

    norm = sqrt ( sum ( normal2(1:dim_num) ** 2 ) )

    normal2(1:dim_num) = normal2(1:dim_num) / norm
    !
    !  Rotate the normal component by the angle.
    !
    rot(1:dim_num) = normal_component * ( &
        cos ( angle ) * normal(1:dim_num) &
        + sin ( angle ) * normal2(1:dim_num) )
    !
    !  The rotated vector is the parallel component plus the rotated component.
    !
    w(1:dim_num) = parallel(1:dim_num) + rot(1:dim_num)

    return
    end
    pure subroutine rotate_axis2mat ( axis, angle, a ) 
    !*****************************************************************************80
    !
    !! ROTATION_AXIS2MAT converts a rotate from axis to matrix format in 3D.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    27 July 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Reference:
    !
    !    James Foley, Andries van Dam, Steven Feiner, John Hughes,
    !    Computer Graphics, Principles and Practice,
    !    Second Edition,
    !    Addison Wesley, 1990.
    !
    !  Parameters:
    !
    !    Input, real ( real64 ) AXIS(3), the axis vector which remains
    !    unchanged by the rotate.
    !
    !    Input, real ( real64 ) ANGLE, the angular measurement of the
    !    rotate about the axis, in radians.
    !
    !    Output, real ( real64 ) A(3,3), the rotate matrix.
    !
    implicit none

    !integer ( int32 ), parameter :: dim_num = 3

    real ( real64 ), intent(out) :: a(dim_num,dim_num)
    real ( real64 ), intent(in), value :: angle
    real ( real64 ), intent(in) :: axis(dim_num)
    real ( real64 ) axis_norm
    real ( real64 ) ca
    real ( real64 ) sa
    real ( real64 ) v1
    real ( real64 ) v2
    real ( real64 ) v3

    v1 = axis(1)
    v2 = axis(2)
    v3 = axis(3)

    axis_norm = sqrt ( sum ( axis(1:dim_num) ** 2 ) )

    if ( axis_norm == 0.0D+00 ) then
        a(1:dim_num,1:dim_num) = 0.0D+00
        return
    end if

    v1 = v1 / axis_norm
    v2 = v2 / axis_norm
    v3 = v3 / axis_norm

    ca = cos ( angle )
    sa = sin ( angle )

    a(1,1) =                    v1 * v1 + ca * ( 1.0D+00 - v1 * v1 )
    a(1,2) = ( 1.0D+00 - ca ) * v1 * v2 - sa * v3
    a(1,3) = ( 1.0D+00 - ca ) * v1 * v3 + sa * v2

    a(2,1) = ( 1.0D+00 - ca ) * v2 * v1 + sa * v3
    a(2,2) =                    v2 * v2 + ca * ( 1.0D+00 - v2 * v2 )
    a(2,3) = ( 1.0D+00 - ca ) * v2 * v3 - sa * v1

    a(3,1) = ( 1.0D+00 - ca ) * v3 * v1 - sa * v2
    a(3,2) = ( 1.0D+00 - ca ) * v3 * v2 + sa * v1
    a(3,3) =                    v3 * v3 + ca * ( 1.0D+00 - v3 * v3 )

    return
    end
    pure subroutine rotate_axis2quat ( axis, angle, q ) 
    !*****************************************************************************80
    !
    !! ROTATION_AXIS2QUAT converts rotate from axis to quaternion form in 3D.
    !
    !  Discussion:
    !
    !    A rotate quaternion Q has the form:
    !
    !      Q = A + Bi + Cj + Dk
    !
    !    where A, B, C and D are real numbers, and i, j, and k are to be regarded
    !    as symbolic constant basis vectors, similar to the role of the "i"
    !    in the representation of imaginary numbers.
    !
    !    A is the cosine of half of the angle of rotate.  (B,C,D) is a
    !    unit vector pointing in the direction of the axis of rotate.
    !    Rotation multiplication and inversion can be carried out using
    !    this format and the usual rules for quaternion multiplication
    !    and inversion.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    24 July 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real ( real64 ) AXIS(3), the axis vector which remains
    !    unchanged by the rotate.
    !
    !    Input, real ( real64 ) ANGLE, the angular measurement of the
    !    rotate about the axis, in radians.
    !
    !    Output, real ( real64 ) Q(4), the quaternion representing the rotate.
    !
    implicit none

    !integer ( int32 ), parameter :: dim_num = 3

    real ( real64 ), intent(in)  :: axis(dim_num)
    real ( real64 ), intent(in), value  :: angle
    real ( real64 ), intent(out) :: q(4)
    real ( real64 )              :: axis_norm

    axis_norm = sqrt ( sum ( axis(1:dim_num) ** 2 ) )

    if ( axis_norm == 0.0D+00 ) then
        q(1) = 1.0D+00
        q(2:4) = 0.0D+00
        return
    end if

    q(1)   = cos ( 0.5D+00 * angle )
    q(2:4) = sin ( 0.5D+00 * angle ) * axis(1:3) / axis_norm

    return
    end
    pure subroutine rotate_mat_vector ( a, v, w ) 
    !*****************************************************************************80
    !
    !! ROTATION_MAT_VECTOR applies a marix rotate to a vector in 3d.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    30 July 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real ( real64 ) A(3,3), the matrix defining the rotate.
    !
    !    Input, real ( real64 ) V(3), the vector to be rotated.
    !
    !    Output, real ( real64 ) W(3), the rotated vector.
    !
    implicit none

    !integer ( int32 ), parameter :: dim_num = 3

    real ( real64 ), intent(in) :: a(dim_num,dim_num)
    real ( real64 ), intent(in) :: v(dim_num)
    real ( real64 ), intent(out) :: w(dim_num)

    w(1:dim_num) = matmul ( a(1:dim_num,1:dim_num), v(1:dim_num) )

    return
    end
    
    pure subroutine rotate_mat2axis ( a, axis, angle ) 
    use mod_common, only : acos_scalar
    !*****************************************************************************80
    !
    !! ROTATION_MAT2AXIS converts a rotate from matrix to axis format in 3D.
    !
    !  Discussion:
    !
    !    The computation is based on the fact that a rotate matrix must
    !    have an eigenvector corresponding to the eigenvalue of 1, hence:
    !
    !      ( A - I ) * v = 0.
    !
    !    The eigenvector V is the axis of rotate.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    27 July 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Reference:
    !
    !    Jack Kuipers,
    !    Quaternions and Rotation Sequences,
    !    Princeton, 1998.
    !
    !  Parameters:
    !
    !    Input, real ( real64 ) A(3,3), the rotate matrix.
    !
    !    Output, real ( real64 ) AXIS(3), the axis vector which remains
    !    unchanged by the rotate.
    !
    !    Output, real ( real64 ) ANGLE, the angular measurement of the
    !    rotate about the axis, in radians.
    !
    implicit none

    !integer ( int32 ), parameter :: dim_num = 3

    real ( real64 ), intent(in) :: a(dim_num,dim_num)
    real ( real64 ), intent(out) :: axis(dim_num)
    real ( real64 ), intent(out) :: angle
    real ( real64 ) axis_norm
    !
    !  Compute the normalized axis of rotate.
    !
    axis(1) = a(3,2) - a(2,3)
    axis(2) = a(1,3) - a(3,1)
    axis(3) = a(2,1) - a(1,2)

    axis_norm = sqrt ( sum ( axis(1:dim_num) ** 2 ) )

    if ( axis_norm == 0.0D+00 ) then
        !write ( *, '(a)' ) ' '
        !write ( *, '(a)' ) 'ROTATION_MAT2AXIS - Fatal error!'
        !write ( *, '(a)' ) '  A is not a rotate matrix,'
        !write ( *, '(a)' ) '  or there are multiple axes of rotate.'
        error stop '  A is not a rotate matrix, or there are multiple axes of rotate.'
    end if

    axis(1:dim_num) = axis(1:dim_num) / axis_norm
    !
    !  Find the angle.
    !
    angle = acos_scalar ( 0.5D+00 * ( a(1,1) + a(2,2) + a(3,3) - 1.0D+00 ) )

    return
    end
    pure subroutine rotate_mat2quat ( a, q ) 
    use mod_common, only : acos_scalar
    !*****************************************************************************80
    !
    !! ROTATION_MAT2QUAT converts rotate from matrix to quaternion format.
    !
    !  Discussion:
    !
    !    The computation is based on the fact that a rotate matrix must
    !    have an eigenvector corresponding to the eigenvalue of 1, hence:
    !
    !      ( A - I ) * v = 0.
    !
    !    The eigenvector V is the axis of rotate.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    27 July 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Reference:
    !
    !    Jack Kuipers,
    !    Quaternions and Rotation Sequences,
    !    Princeton, 1998.
    !
    !  Parameters:
    !
    !    Input, real ( real64 ) A(3,3), the rotate matrix.
    !
    !    Output, real ( real64 ) Q(4), the quaternion representing the rotate.
    !
    implicit none

    !integer ( int32 ), parameter :: dim_num = 3

    real ( real64 ), intent(in) :: a(dim_num,dim_num)
    real ( real64 ), intent(out) :: q(4)
    real ( real64 ) angle
    real ( real64 ) axis(dim_num)
    real ( real64 ) axis_norm
    real ( real64 ) cos_phi
    real ( real64 ) sin_phi
    !
    !  Compute the normalized axis of rotate.
    !
    axis(1) = a(3,2) - a(2,3)
    axis(2) = a(1,3) - a(3,1)
    axis(3) = a(2,1) - a(1,2)

    axis_norm = sqrt ( sum ( axis(1:dim_num) ** 2 ) )

    if ( axis_norm == 0.0D+00 ) then
        !write ( *, '(a)' ) ' '
        !write ( *, '(a)' ) 'ROTATION_MAT2QUAT - Fatal error!'
        !write ( *, '(a)' ) '  A is not a rotate matrix,'
        !write ( *, '(a)' ) '  or there are multiple axes of rotate.'
        error stop '  A is not a rotate matrix, or there are multiple axes of rotate.'
    end if

    axis(1:dim_num) = axis(1:dim_num) / axis_norm
    !
    !  Compute the angle.
    !
    angle = acos_scalar ( 0.5D+00 * ( a(1,1) + a(2,2) + a(3,3) - 1.0D+00 ) )
    !
    !  Compute the quaternion.
    !
    cos_phi = cos ( 0.5D+00 * angle )

    sin_phi = sqrt ( 1.0D+00 - cos_phi * cos_phi )
    
    q(1)   = cos_phi
    q(2:4) = sin_phi * axis(1:3)

    return
    end
    
    pure subroutine rotate_quat_vector ( q, v, w ) 
    !*****************************************************************************80
    !
    !! ROTATION_QUAT_VECTOR applies a quaternion rotate to a vector in 3D.
    !
    !  Discussion:
    !
    !    If Q is a unit quaternion that encodes a rotate of ANGLE
    !    radians about the vector AXIS, then for an arbitrary real
    !    vector V, the result W of the rotate on V can be written as:
    !
    !      W = Q * V * Conj(Q)
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    29 July 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real ( real64 ) Q(4), the quaternion defining the rotate.
    !
    !    Input, real ( real64 ) V(3), the vector to be rotated.
    !
    !    Output, real ( real64 ) W(3), the rotated vector.
    !
    implicit none

    !integer ( int32 ), parameter :: dim_num = 3

    real ( real64 ), intent(in) :: q(4)
    real ( real64 ), intent(in) :: v(dim_num)
    real ( real64 ), intent(out) :: w(dim_num)

    w(1) = &
        ( 2.0D+00 * ( q(1) * q(1) + q(2) * q(2) ) - 1.0D+00 ) * v(1) &
        +   2.0D+00 * ( q(2) * q(3) - q(1) * q(4) )             * v(2) &
        +   2.0D+00 * ( q(2) * q(4) + q(1) * q(3) )             * v(3)

    w(2) = &
        2.0D+00 * ( q(2) * q(3) + q(1) * q(4) )             * v(1) &
        + ( 2.0D+00 * ( q(1) * q(1) + q(3) * q(3) ) - 1.0D+00 ) * v(2) &
        +   2.0D+00 * ( q(3) * q(4) - q(1) * q(2) )             * v(3)

    w(3) = &
        2.0D+00 * ( q(2) * q(4) - q(1) * q(3) )             * v(1) &
        +   2.0D+00 * ( q(3) * q(4) + q(1) * q(2) )             * v(2) &
        + ( 2.0D+00 * ( q(1) * q(1) + q(4) * q(4) ) - 1.0D+00 ) * v(3)

    return
    end
    pure subroutine rotate_quat_vector_inv ( q1, v, w, inverse ) 
    !*****************************************************************************80
    !
    !! ROTATION_QUAT_VECTOR applies a quaternion rotate to a vector in 3D.
    !
    !  Discussion:
    !
    !    If Q is a unit quaternion that encodes a rotate of ANGLE
    !    radians about the vector AXIS, then for an arbitrary real
    !    vector V, the result W of the rotate on V can be written as:
    !
    !      W = Q * V * Conj(Q)
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    29 July 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real ( real64 ) Q(4), the quaternion defining the rotate.
    !
    !    Input, real ( real64 ) V(3), the vector to be rotated.
    !
    !    Output, real ( real64 ) W(3), the rotated vector.
    !
    implicit none

    !integer ( int32 ), parameter :: dim_num = 3

    real ( real64 ), intent(in) :: q1(4)
    real ( real64 ), intent(in) :: v(dim_num)
    real ( real64 ), intent(out) :: w(dim_num)
    logical, intent(in), value :: inverse
    real ( real64 ) :: q(4)
    
    if( inverse ) then    
        q(1) = q1(1)
        q(2:4) = -q1(2:4)
    else
        q = q1
    end if

    w(1) = &
        ( 2.0D+00 * ( q(1) * q(1) + q(2) * q(2) ) - 1.0D+00 ) * v(1) &
        +   2.0D+00 * ( q(2) * q(3) - q(1) * q(4) )           * v(2) &
        +   2.0D+00 * ( q(2) * q(4) + q(1) * q(3) )           * v(3)

    w(2) = &
        2.0D+00 * ( q(2) * q(3) + q(1) * q(4) )                 * v(1) &
        + ( 2.0D+00 * ( q(1) * q(1) + q(3) * q(3) ) - 1.0D+00 ) * v(2) &
        +   2.0D+00 * ( q(3) * q(4) - q(1) * q(2) )             * v(3)

    w(3) = &
        2.0D+00 * ( q(2) * q(4) - q(1) * q(3) )                 * v(1) &
        +   2.0D+00 * ( q(3) * q(4) + q(1) * q(2) )             * v(2) &
        + ( 2.0D+00 * ( q(1) * q(1) + q(4) * q(4) ) - 1.0D+00 ) * v(3)

    return
    end
    pure subroutine rotate_quat2axis ( q, axis, angle ) 
    !*****************************************************************************80
    !
    !! ROTATION_QUAT2AXIS converts rotate from quaternion to axis form in 3D.
    !
    !  Discussion:
    !
    !    A rotate quaternion Q has the form:
    !
    !      Q = A + Bi + Cj + Dk
    !
    !    where A, B, C and D are real numbers, and i, j, and k are to be regarded
    !    as symbolic constant basis vectors, similar to the role of the "i"
    !    in the representation of imaginary numbers.
    !
    !    A is the cosine of half of the angle of rotate.  (B,C,D) is a
    !    vector pointing in the direction of the axis of rotate.
    !    Rotation multiplication and inversion can be carried out using
    !    this format and the usual rules for quaternion multiplication
    !    and inversion.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    02 December 2000
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real ( real64 ) Q(4), the quaternion representing the rotate.
    !
    !    Output, real ( real64 ) AXIS(3), the axis vector which remains
    !    unchanged by the rotate.
    !
    !    Output, real ( real64 ) ANGLE, the angular measurement of the
    !    rotate about the axis, in radians.
    !
    implicit none

    !integer ( int32 ), parameter :: dim_num = 3

    real ( real64 ), intent(in) :: q(4)
    real ( real64 ), intent(out) :: axis(dim_num)
    real ( real64 ), intent(out) :: angle
    !logical, intent(in), optional, value :: inverse
    real ( real64 ) cos_phi
    real ( real64 ) sin_phi

    sin_phi = sqrt ( sum ( q(2:4) ** 2 ) )

    cos_phi = q(1)

    angle = 2.0D+00 * atan2 ( sin_phi, cos_phi )

    if ( sin_phi == 0.0D+00 ) then
        axis(1:dim_num) = (/ 1.0D+00, 0.0D+00, 0.0D+00 /)
    else
        axis(1:dim_num) = q(2:4) / sin_phi
    end if

    return
    end
    pure subroutine rotate_quat2mat ( q, a) 
    !*****************************************************************************80
    !
    !! ROTATION_QUAT2MAT converts rotate from quaternion to matrix form in 3D.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    27 July 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Reference:
    !
    !    James Foley, Andries van Dam, Steven Feiner, John Hughes,
    !    Computer Graphics, Principles and Practice,
    !    Second Edition,
    !    Addison Wesley, 1990.
    !
    !  Parameters:
    !
    !    Input, real ( real64 ) Q(4), the quaternion representing the rotate.
    !
    !    Output, real ( real64 ) A(3,3), the rotate matrix.
    !
    implicit none

    !integer ( int32 ), parameter :: dim_num = 3

    real ( real64 ), intent(in) :: q(4)
    real ( real64 ), intent(out) :: a(dim_num,dim_num)
    !logical, intent(in), optional, value :: inverse
    real ( real64 ) angle
    real ( real64 ) ca
    real ( real64 ) cos_phi
    real ( real64 ) sa
    real ( real64 ) sin_phi
    real ( real64 ) v1
    real ( real64 ) v2
    real ( real64 ) v3

    sin_phi = sqrt ( sum ( q(2:4) ** 2 ) )
    
    cos_phi = q(1)

    angle = 2.0D+00 * atan2 ( sin_phi, cos_phi )

    if ( sin_phi == 0.0D+00 ) then
        v1 = 1.0D+00
        v2 = 0.0D+00
        v3 = 0.0D+00
    else
        v1 = q(2) / sin_phi
        v2 = q(3) / sin_phi
        v3 = q(4) / sin_phi
    end if

    ca = cos ( angle )
    sa = sin ( angle )

    a(1,1) =                    v1 * v1 + ca * ( 1.0D+00 - v1 * v1 )
    a(1,2) = ( 1.0D+00 - ca ) * v1 * v2 - sa * v3
    a(1,3) = ( 1.0D+00 - ca ) * v1 * v3 + sa * v2

    a(2,1) = ( 1.0D+00 - ca ) * v2 * v1 + sa * v3
    a(2,2) =                    v2 * v2 + ca * ( 1.0D+00 - v2 * v2 )
    a(2,3) = ( 1.0D+00 - ca ) * v2 * v3 - sa * v1

    a(3,1) = ( 1.0D+00 - ca ) * v3 * v1 - sa * v2
    a(3,2) = ( 1.0D+00 - ca ) * v3 * v2 + sa * v1
    a(3,3) =                    v3 * v3 + ca * ( 1.0D+00 - v3 * v3 )

    return
    end
    pure subroutine rotate_quat2mat_inv ( q, a, inverse) 
    !*****************************************************************************80
    !
    !! ROTATION_QUAT2MAT converts rotate from quaternion to matrix form in 3D.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    27 July 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Reference:
    !
    !    James Foley, Andries van Dam, Steven Feiner, John Hughes,
    !    Computer Graphics, Principles and Practice,
    !    Second Edition,
    !    Addison Wesley, 1990.
    !
    !  Parameters:
    !
    !    Input, real ( real64 ) Q(4), the quaternion representing the rotate.
    !
    !    Output, real ( real64 ) A(3,3), the rotate matrix.
    !
    !    Input, logical INVERSE, true if use the inverse rotate
    !
    implicit none

    !integer ( int32 ), parameter :: dim_num = 3

    real ( real64 ), intent(in) :: q(4)
    real ( real64 ), intent(out) :: a(dim_num,dim_num)
    logical, intent(in), value :: inverse
    real ( real64 ) angle
    real ( real64 ) ca
    real ( real64 ) cos_phi
    real ( real64 ) sa
    real ( real64 ) sin_phi
    real ( real64 ) v1
    real ( real64 ) v2
    real ( real64 ) v3

    sin_phi = sqrt ( sum ( q(2:4) ** 2 ) )
    cos_phi = q(1)

    angle = 2.0D+00 * atan2 ( sin_phi, cos_phi )

    if ( sin_phi == 0.0D+00 ) then
        v1 = 1.0D+00
        v2 = 0.0D+00
        v3 = 0.0D+00
    else
        v1 = q(2) / sin_phi
        v2 = q(3) / sin_phi
        v3 = q(4) / sin_phi
    end if

    ca = cos ( angle )
    sa = sin ( angle )
    
    if( inverse ) then
        sa = -sa
    end if

    a(1,1) = v1 * v1 + ca * ( 1.0D+00 - v1 * v1 )
    a(2,2) = v2 * v2 + ca * ( 1.0D+00 - v2 * v2 )
    a(3,3) = v3 * v3 + ca * ( 1.0D+00 - v3 * v3 )
    
    a(1,2) = ( 1.0D+00 - ca ) * v1 * v2 - sa * v3
    a(2,1) = ( 1.0D+00 - ca ) * v2 * v1 + sa * v3
    
    a(1,3) = ( 1.0D+00 - ca ) * v1 * v3 + sa * v2
    a(3,1) = ( 1.0D+00 - ca ) * v3 * v1 - sa * v2

    a(2,3) = ( 1.0D+00 - ca ) * v2 * v3 - sa * v1
    a(3,2) = ( 1.0D+00 - ca ) * v3 * v2 + sa * v1    

    return
    end
    
    end module

