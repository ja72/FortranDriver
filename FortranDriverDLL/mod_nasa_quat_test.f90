    module mod_nasa_quat_test
    use mod_nasa_quat_print
    implicit none
    contains
    
    ! *****************************************************************************
    ! ** TEST FUNCTIONS                                                         **
    ! *****************************************************************************

    subroutine quat_conjugate_test ( )

    !*****************************************************************************80
    !
    !! Q8_CONJUGATE_TEST tests Q8_CONJUGATE.
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
    implicit none

    integer ( int32 ) i
    integer ( int32 ) seed
    real ( real64 ) q1(4)
    real ( real64 ) q2(4)

    seed = 123456789

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'Q8_CONJUGATE_TEST'
    write ( *, '(a)' ) '  Q8_CONJUGATE conjugates a quaternion;'

    do i = 1, 5

        call quat_normal_01 ( seed, q1 )
        call quat_conjugate ( q1, q2 )

        write ( *, '(a)' ) ''
        call quat_transpose_print ( q1, '  q1 = quat_normal_01 ( seed ):' )
        call quat_transpose_print ( q2, '  q2 = quat_conjugate ( q1 ):  ' )

    end do

    return
    end
    subroutine quat_exponentiate_test ( )

    !*****************************************************************************80
    !
    !! Q8_EXPONENTIATE_TEST tests Q8_EXPONENTIATE.
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
    implicit none

    integer ( int32 ) i
    real ( real64 ) q1(4)
    real ( real64 ) q2(4)
    integer ( int32 ) seed

    seed = 123456789

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'Q8_EXPONENTIATE_TEST'
    write ( *, '(a)' ) '  Q8_EXPONENTIATE exponentiates a quaternion'

    do i = 1, 5

        call quat_normal_01 ( seed, q1 )
        call quat_exponentiate ( q1, q2 )

        write ( *, '(a)' ) ''
        call quat_transpose_print ( q1, '  q1 = quat_normal_01 ( seed ):' )
        call quat_transpose_print ( q2, '  q2 = quat_exponentiate ( q1 ):' )

    end do

    return
    end
    subroutine quat_inverse_test ( )

    !*****************************************************************************80
    !
    !! Q8_INVERSE_TEST tests Q8_INVERSE.
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
    implicit none

    integer ( int32 ) i
    real ( real64 ) q1(4)
    real ( real64 ) q2(4)
    real ( real64 ) q3(4)
    integer ( int32 ) seed

    seed = 123456789

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'Q8_INVERSE_TEST'
    write ( *, '(a)' ) '  Q8_INVERSE inverts a quaternion'

    do i = 1, 5

        call quat_normal_01 ( seed, q1 )
        call quat_inverse ( q1, q2 )
        call quat_multiply ( q1, q2, q3 )

        write ( *, '(a)' ) ''
        call quat_transpose_print ( q1, '  q1 = quat_normal_01 ( seed ):' )
        call quat_transpose_print ( q2, '  q2 = quat_inverse ( q1 ):    ' )
        call quat_transpose_print ( q3, '  q3 = quat_multiply ( q1, q2 ):    ' )

    end do

    return
    end
    subroutine quat_multiply_test ( )

    !*****************************************************************************80
    !
    !! Q8_MULTIPLY_TEST tests Q8_MULTIPLY.
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
    implicit none

    integer ( int32 ) i
    real ( real64 ) q1(4)
    real ( real64 ) q2(4)
    real ( real64 ) q3(4)
    integer ( int32 ) seed

    seed = 123456789

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'Q8_MULTIPLY_TEST'
    write ( *, '(a)' ) '  Q8_MULTIPLY multiplies two quaternions'

    do i = 1, 5

        call quat_normal_01 ( seed, q1 )
        call quat_normal_01 ( seed, q2 )
        call quat_multiply ( q1, q2, q3 )

        write ( *, '(a)' ) ''
        call quat_transpose_print ( q1, '  q1 = quat_normal_01 ( seed ) :' )
        call quat_transpose_print ( q2, '  q2 = quat_normal_01 ( seed ) :' )
        call quat_transpose_print ( q3, '  q3 = quat_multiply ( q1, q2 ):' )

    end do

    return
    end
    subroutine quat_multiply2_test ( )

    !*****************************************************************************80
    !
    !! Q8_MULTIPLY2_TEST tests Q8_MULTIPLY2.
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
    implicit none

    integer ( int32 ) i
    real ( real64 ) q1(4)
    real ( real64 ) q2(4)
    real ( real64 ) q3(4)
    integer ( int32 ) seed

    seed = 123456789

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'Q8_MULTIPLY2_TEST'
    write ( *, '(a)' ) '  Q8_MULTIPLY2 multiplies two quaternions using a matrix'

    do i = 1, 5

        call quat_normal_01 ( seed, q1 )
        call quat_normal_01 ( seed, q2 )
        call quat_multiply2 ( q1, q2, q3 )

        write ( *, '(a)' ) ''
        call quat_transpose_print ( q1, '  q1 = quat_normal_01 ( seed )  :' )
        call quat_transpose_print ( q2, '  q2 = quat_normal_01 ( seed )  :' )
        call quat_transpose_print ( q3, '  q3 = quat_multiply2 ( q1, q2 ):' )

    end do

    return
    end
    subroutine quat_normal_01_test ( )

    !*****************************************************************************80
    !
    !! Q8_NORMAL_01_TEST tests Q8_NORMAL_01.
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
    implicit none

    integer ( int32 ) i
    character ( len = 80 ) label
    real ( real64 ) q(4)
    integer ( int32 ) seed

    seed = 123456789

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'Q8_NORMAL_01_TEST'
    write ( *, '(a)' ) '  Q8_NORMAL_01 computes a normally distributed quaternion.'
    write ( *, '(a)' ) ''

    do i = 1, 5
        call quat_normal_01 ( seed, q )
        write ( label, '(a,i2)' ) '  Sample #', i
        call quat_transpose_print ( q, label )
    end do

    return
    end
    subroutine quat_norm_test ( )

    !*****************************************************************************80
    !
    !! Q8_NORM_TEST tests Q8_NORM.
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
    implicit none

    integer ( int32 ) i
    real ( real64 ) q(4)
    integer ( int32 ) seed
    real ( real64 ) value

    seed = 123456789

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'Q8_NORM_TEST'
    write ( *, '(a)' ) '  Q8_NORM computes the norm of a quaternion.'

    do i = 1, 5
        write ( *, '(a)' ) ''
        call quat_normal_01 ( seed, q )
        call quat_transpose_print ( q, '  q = quat_normal_01(seed):' )
        value = quat_norm ( q )
        write ( *, '(a,g14.6)' ) '  quat_norm(q) = ', value
    end do

    return
    end
    subroutine quat_transpose_print_test ( )

    !*****************************************************************************80
    !
    !! Q8_TRANSPOSE_PRINT_TEST tests Q8_TRANSPOSE_PRINT.
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
    implicit none

    integer ( int32 ) seed
    real ( real64 ) q(4)

    seed = 123456789

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'Q8_TRANSPOSE_PRINT_TEST'
    write ( *, '(a)' ) '  Q8_TRANSPOSE_PRINT prints a quaternion "transposed",'
    write ( *, '(a)' ) '  that is, writing it as a row vector.'

    call quat_normal_01 ( seed, q )

    call quat_transpose_print ( q, '  The quaternion:' )

    return
    end
    subroutine acos_scalar_test ( )

    !*****************************************************************************80
    !
    !! R8_ACOS_TEST tests R8_ACOS.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    25 July 2014
    !
    !  Author:
    !
    !    John Burkardt
    !
    implicit none

    real ( real64 ) c
    integer ( int32 ) test

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'R8_ACOS_TEST'
    write ( *, '(a)' ) '  R8_ACOS computes the arc-cosine of an angle.'
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '       C            R8_ACOS(C)        ACOS(C)'
    write ( *, '(a)' ) ''

    do test = -1, 13

        c = real ( test - 6, real64 ) / real ( 6, real64 )

        if ( -1.0D+00 <= c .and. c <= 1.0D+00 ) then
            write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) &
                c, acos_scalar ( c ), acos ( c )
        else
            write ( *, '(2x,g14.6,2x,g14.6)' ) &
                c, acos_scalar ( c )
        end if

    end do

    return
    end
    subroutine r8mat_print_test ( )

    !*****************************************************************************80
    !
    !! R8MAT_PRINT_TEST tests R8MAT_PRINT.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    31 August 2014
    !
    !  Author:
    !
    !    John Burkardt
    !
    implicit none

    integer ( int32 ), parameter :: m = 6
    integer ( int32 ), parameter :: n = 4

    real ( real64 ) a(m,n)
    integer ( int32 ) i
    integer ( int32 ) j

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'R8MAT_PRINT_TEST'
    write ( *, '(a)' ) '  R8MAT_PRINT prints an R8MAT.'

    do j = 1, n
        do i = 1, m
            a(i,j) = real ( 10 * i + j, real64 )
        end do
    end do

    call r8mat_print ( m, n, a, '  The R8MAT:' )

    return
    end
    subroutine r8mat_print_some_test ( )

    !*****************************************************************************80
    !
    !! R8MAT_PRINT_SOME_TEST tests R8MAT_PRINT_SOME.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    31 August 2014
    !
    !  Author:
    !
    !    John Burkardt
    !
    implicit none

    integer ( int32 ), parameter :: m = 6
    integer ( int32 ), parameter :: n = 4

    real ( real64 ) a(m,n)
    integer ( int32 ) i
    integer ( int32 ) j

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'R8MAT_PRINT_SOME_TEST'
    write ( *, '(a)' ) '  R8MAT_PRINT_SOME prints some of an R8MAT.'

    do j = 1, n
        do i = 1, m
            a(i,j) = real ( 10 * i + j, real64 )
        end do
    end do

    call r8mat_print_some ( m, n, a, 2, 1, 4, 2, &
        '  The R8MAT, rows 2:4, cols 1:2:' )

    return
    end
    subroutine r8vec_print_test ( )

    !*****************************************************************************80
    !
    !! R8VEC_PRINT_TEST tests R8VEC_PRINT.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    31 August 2014
    !
    !  Author:
    !
    !    John Burkardt
    !
    implicit none

    integer ( int32 ), parameter :: n = 4

    real ( real64 ), dimension ( n ) :: a = (/ &
        123.456D+00, 0.000005D+00, -1.0D+06, 3.14159265D+00 /)

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'R8VEC_PRINT_TEST'
    write ( *, '(a)' ) '  R8VEC_PRINT prints an R8VEC.'

    call r8vec_print ( n, a, '  The R8VEC:' )

    return
    end
    subroutine r8vec_uniform_01_test ( )
    use mod_fortran, only : call_uniform_array_v
    !*****************************************************************************80
    !
    !! R8VEC_UNIFORM_01_TEST tests R8VEC_UNIFORM_01.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    25 June 2010
    !
    !  Author:
    !
    !    John Burkardt
    !
    implicit none

    integer ( int32 ), parameter :: n = 20

    real ( real64 ) r(n)
    integer ( int32 ) seed

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'R8VEC_UNIFORM_01_TEST'
    write ( *, '(a)' ) '  R8VEC_UNIFORM_01 returns a random R8VEC '
    write ( *, '(a)' ) '  with entries in [0,1].'

    seed = 123456789

    write ( *, '(a)' ) ''
    write ( *, '(a,i12)' ) '  Input SEED = ', seed

    call call_uniform_array_v ( n, seed, r )

    call r8vec_print ( n, r, '  Random R8VEC:' )

    return
    end
    subroutine rotate_axis2mat_test ( )

    !*****************************************************************************80
    !
    !! ROTATION_AXIS2MAT_TEST tests ROTATION_AXIS2MAT.
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
    implicit none

    real ( real64 ) a(3,3)
    real ( real64 ) angle
    real ( real64 ) axis(3)
    real ( real64 ) v(3)
    real ( real64 ) w(3)

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'ROTATION_AXIS2MAT_TEST'
    write ( *, '(a)' ) '  ROTATION_AXIS2MAT converts a rotate axis to a matrix.'

    v = (/ 1.0D+00, 4.0D+00, 10.0D+00 /)
    call r8vec_print ( 3, v, '  The vector V:' )

    axis = (/ 0.2361737D+00, -0.8814124D+00, -0.4090649D+00 /)
    call r8vec_print ( 3, axis, '  The rotate axis:' )

    angle = 1.159804D+00
    write ( *, '(a)' ) ''
    write ( *, '(a,g14.6)' ) '  The rotate angle is ', angle

    call rotate_axis2mat ( axis, angle, a )

    call r8mat_print ( 3, 3, a, '  The rotate matrix A:' )

    w = matmul ( a, v )

    call r8vec_print ( 3, w, '  The rotated vector W = A * V:' )
    !
    !  Test an axis vector that does not have unit length.
    !
    v = (/ 1.0D+00, 1.0D+00, 1.0D+00 /)
    call r8vec_print ( 3, v, '  The vector V:' )

    axis = (/ 0.0D+00, 0.0D+00, 2.0D+00 /)
    call r8vec_print ( 3, axis, '  The rotate axis:' )

    angle = 90.0D+00
    angle = degrees_to_radians ( angle )

    write ( *, '(a)' ) ''
    write ( *, '(a,g14.6)' ) '  The rotate angle is ', angle

    call rotate_axis2mat ( axis, angle, a )

    call r8mat_print ( 3, 3, a, '  The rotate matrix A:' )

    w = matmul ( a, v )

    call r8vec_print ( 3, w, '  The rotated vector W = A * V:' )

    return
    end
    subroutine rotate_axis2quat_test ( )

    !*****************************************************************************80
    !
    !! ROTATION_AXIS2QUAT_TEST tests ROTATION_AXIS2QUAT.
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
    implicit none

    real ( real64 ) angle
    real ( real64 ) axis(3)
    real ( real64 ) q(4)
    real ( real64 ) v(3)
    real ( real64 ) w(3)

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'ROTATION_AXIS2QUAT_TEST'
    write ( *, '(a)' ) '  ROTATION_AXIS2QUAT converts a rotate axis to a quaternion.'

    v = (/ 1.0D+00, 4.0D+00, 10.0D+00 /)
    call r8vec_print ( 3, v, '  The vector V:' )

    axis = (/ 0.2361737D+00, -0.8814124D+00, -0.4090649D+00 /)
    call r8vec_print ( 3, axis, '  The rotate axis:' )

    angle = 1.159804D+00
    write ( *, '(a)' ) ''
    write ( *, '(a,g14.6)' ) '  The rotate angle is ', angle

    call rotate_axis2quat ( axis, angle, q )

    call r8vec_print ( 4, q, '  The rotate quaternion Q:' )

    call rotate_quat_vector ( q, v, w )

    call r8vec_print ( 3, w, '  The rotated vector W:' )
    !
    !  Another test of ROTATION_AXIS_VECTOR with an axis vector
    !  that does not have unit length.
    !
    v = (/ 1.0D+00, 1.0D+00, 1.0D+00 /)
    call r8vec_print ( 3, v, '  The vector V:' )

    axis = (/ 0.0D+00, 0.0D+00, 2.0D+00 /)
    call r8vec_print ( 3, axis, '  The rotate axis:' )

    angle = 90.0D+00
    angle = degrees_to_radians ( angle )

    write ( *, '(a)' ) ''
    write ( *, '(a,g14.6)' ) '  The rotate angle is ', angle
    call rotate_axis2quat ( axis, angle, q )

    call r8vec_print ( 4, q, '  The rotate quaternion Q:' )

    call rotate_quat_vector ( q, v, w )

    call r8vec_print ( 3, w, '  The rotated vector W:' )

    return
    end
    subroutine rotate_axis_vector_test ( )

    !*****************************************************************************80
    !
    !! ROTATION_AXIS_VECTOR_TEST tests ROTATION_AXIS_VECTOR.
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
    implicit none

    real ( real64 ) angle
    real ( real64 ) axis(3)
    real ( real64 ) v(3)
    real ( real64 ) w(3)

    angle = 1.159804D+00
    axis = (/ 0.2361737D+00, -0.8814124D+00, -0.4090649D+00 /)
    v = (/ 1.0D+00, 4.0D+00, 10.0D+00 /)

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'ROTATION_AXIS_VECTOR_TEST'
    write ( *, '(a)' ) '  ROTATION_AXIS_VECTOR applies an axis'
    write ( *, '(a)' ) '  rotate to a vector.'

    call r8vec_print ( 3, v, '  The vector:' )

    call r8vec_print ( 3, axis, '  The rotate axis:' )

    write ( *, '(a)' ) ''
    write ( *, '(a,g14.6)' ) '  The rotate angle is ', angle

    call rotate_axis_vector ( axis, angle, v, w )

    call r8vec_print ( 3, w, '  The rotated vector:' )
    !
    !  Another test of ROTATION_AXIS_VECTOR with an axis vector
    !  that does not have unit length.
    !
    v = (/ 1.0D+00, 1.0D+00, 1.0D+00 /)

    call r8vec_print ( 3, v, '  The vector:' )

    axis = (/ 0.0D+00, 0.0D+00, 2.0D+00 /)

    call r8vec_print ( 3, axis, '  The rotate axis:' )

    angle = 90.0D+00
    angle = degrees_to_radians ( angle )

    write ( *, '(a)' ) ''
    write ( *, '(a,g14.6)' ) '  The rotate angle is ', angle

    call rotate_axis_vector ( axis, angle, v, w )

    call r8vec_print ( 3, w, '  The rotated vector:' )

    return
    end
    subroutine rotate_mat2axis_test ( )

    !*****************************************************************************80
    !
    !! ROTATION_MAT2AXIS_TEST tests ROTATION_MAT2AXIS.
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
    implicit none

    real ( real64 ) a(3,3)
    real ( real64 ) angle
    real ( real64 ) axis(3)

    a = reshape ( (/ &
        0.43301269D+00, -0.5D+00,        0.75D+00, &
        0.25D+00,        0.86602539D+00, 0.43301269D+00, &
        -0.86602539D+00,  0.0D+00,        0.5D+00 /), (/ 3, 3 /) )

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'ROTATION_MAT2AXIS_TEST'
    write ( *, '(a)' ) '  ROTATION_MAT2AXIS computes a rotate axis'
    write ( *, '(a)' ) '  and angle from a rotate matrix.'
    write ( *, '(a)' ) '  ROTATION_AXIS2MAT computes a rotate matrix'
    write ( *, '(a)' ) '  from a rotate axis and angle.'

    call r8mat_print ( 3, 3, a, '  The rotate matrix:' )

    call rotate_mat2axis ( a, axis, angle )

    call r8vec_print ( 3, axis, '  The rotate axis:' )

    write ( *, '(a)' ) ''
    write ( *, '(a,g14.6)' ) '  The rotate angle is ', angle

    call rotate_axis2mat ( axis, angle, a )

    call r8mat_print ( 3, 3, a, '  The recovered rotate matrix:' )

    return
    end
    subroutine rotate_mat2quat_test ( )

    !*****************************************************************************80
    !
    !! ROTATION_MAT2QUAT_TEST tests ROTATION_MAT2QUAT.
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
    implicit none

    real ( real64 ) a(3,3)
    real ( real64 ) q(4)

    a = reshape ( (/ &
        0.43301269D+00, -0.5D+00,        0.75D+00, &
        0.25D+00,        0.86602539D+00, 0.43301269D+00, &
        -0.86602539D+00,  0.0D+00,        0.5D+00 /), (/ 3, 3 /) )

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'ROTATION_MAT2QUAT_TEST'
    write ( *, '(a)' ) '  ROTATION_MAT2QUAT computes a quaternion'
    write ( *, '(a)' ) '  from a rotate matrix.'
    write ( *, '(a)' ) '  ROTATION_QUAT2MAT computes a rotate matrix'
    write ( *, '(a)' ) '  from a quaternion.'

    call r8mat_print ( 3, 3, a, '  The rotate matrix:' )

    call rotate_mat2quat ( a, q )

    call r8vec_print ( 4, q, '  The rotate quaternion Q:' )

    call rotate_quat2mat ( q, a )

    call r8mat_print ( 3, 3, a, '  The recovered rotate matrix:' )

    return
    end
    subroutine rotate_mat_vector_test ( )

    !*****************************************************************************80
    !
    !! ROTATION_MAT_VECTOR_TEST tests ROTATION_MAT_VECTOR.
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
    implicit none

    real ( real64 ) a(3,3)
    real ( real64 ) v(3)
    real ( real64 ) w(3)

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'ROTATION_MAT_VECTOR_TEST'
    write ( *, '(a)' ) '  ROTATION_MAT_VECTOR applies a matrix'
    write ( *, '(a)' ) '  rotate to a vector.'

    a = reshape ( (/ &
        0.43301269D+00, -0.5D+00,        0.75D+00, &
        0.25D+00,        0.86602539D+00, 0.43301269D+00, &
        -0.86602539D+00,  0.0D+00,        0.5D+00 /), (/ 3, 3 /) )

    call r8mat_print ( 3, 3, a, '  The rotate matrix:' )

    v = (/ 1.0D+00, 4.0D+00, 10.0D+00 /)
    call r8vec_print ( 3, v, '  The vector V:' )

    call rotate_mat_vector ( a, v, w )
    call r8vec_print ( 3, w, '  The rotated vector W = A * V:' )

    return
    end
    subroutine rotate_quat2axis_test ( )

    !*****************************************************************************80
    !
    !! ROTATION_QUAT2AXIS_TEST tests ROTATION_QUAT2AXIS.
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
    implicit none

    real ( real64 ) angle
    real ( real64 ) axis(3)
    real ( real64 ) q(4)

    q = (/ 0.836516, 0.12941, -0.482963, -0.224144 /)

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'ROTATION_QUAT2AXIS_TEST'
    write ( *, '(a)' ) '  ROTATION_QUAT2AXIS computes a rotate axis'
    write ( *, '(a)' ) '  and angle from a rotate quaternion.'
    write ( *, '(a)' ) '  ROTATION_AXIS2QUAT computes a rotate'
    write ( *, '(a)' ) '  quaternion from a rotate axis and angle.'

    call r8vec_print ( 4, q, '  The rotate quaternion:' )

    call rotate_quat2axis ( q, axis, angle )

    call r8vec_print ( 3, axis, '  The rotate axis:' )

    write ( *, '(a)' ) ''
    write ( *, '(a,g14.6)' ) '  The rotate angle is ', angle

    call rotate_axis2quat ( axis, angle, q )

    call r8vec_print ( 4, q, '  The recovered rotate quaternion:' )

    return
    end
    subroutine rotate_quat2mat_test ( )

    !*****************************************************************************80
    !
    !! ROTATION_QUAT2MAT_TEST tests ROTATION_QUAT2MAT.
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
    implicit none

    real ( real64 ) a(3,3)
    real ( real64 ) q(4)

    q = (/ 0.836516D+00, 0.12941D+00, -0.482963D+00, -0.224144D+00 /)

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'ROTATION_QUAT2MAT_TEST'
    write ( *, '(a)' ) '  ROTATION_QUAT2MAT computes a rotate axis'
    write ( *, '(a)' ) '  from a rotate quaternion.'
    write ( *, '(a)' ) '  ROTATION_MAT2QUAT computes a rotate'
    write ( *, '(a)' ) '  quaternion from a rotate matrix.'

    call r8vec_print ( 4, q, '  The rotate quaternion:' )

    call rotate_quat2mat ( q, a )

    call r8mat_print ( 3, 3, a, '  The rotate matrix:' )

    call rotate_mat2quat ( a, q )

    call r8vec_print ( 4, q, '  The recovered rotate quaternion:' )

    return
    end
    
    subroutine rotate_quat_vector_test ( )

    !*****************************************************************************80
    !
    !! ROTATION_QUAT_VECTOR_TEST tests ROTATION_QUAT_VECTOR.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !   04 August 2018
    !
    !  Author:
    !
    !    John Burkardt
    !
    implicit none

    real ( real64 ) q(4)
    real ( real64 ) v(3)
    real ( real64 ) w(3)

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'ROTATION_QUAT_VECTOR_TEST'
    write ( *, '(a)' ) '  ROTATION_QUAT_VECTOR applies a quaternion'
    write ( *, '(a)' ) '  rotate to a vector.'

    q = (/ 0.836516D+00, 0.12941D+00, -0.482963D+00, -0.224144D+00 /)
    call r8vec_print ( 4, q, '  The rotate quaternion:' )

    v = (/ 1.0D+00, 4.0D+00, 10.0D+00 /)
    call r8vec_print ( 3, v, '  The vector V:' )

    call rotate_quat_vector ( q, v, w )
    call r8vec_print ( 3, w, '  The rotated vector:' )

    return
    end

    subroutine call_quat_test_all() bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_quat_test_all

    !*****************************************************************************80
    !
    !! MAIN is the main program for QUATERNIONS_TEST.
    !
    !  Discussion:
    !
    !    QUATERNIONS_TEST tests the QUATERNIONS library.
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
    implicit none

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'QUATERNIONS_TEST'
    write ( *, '(a)' ) '  FORTRAN90 version'
    write ( *, '(a)' ) '  Test the QUATERNIONS library.'

    call quat_conjugate_test ( )
    call quat_exponentiate_test ( )
    call quat_inverse_test ( )
    call quat_multiply_test ( )
    call quat_multiply2_test ( )
    call quat_norm_test ( )
    call quat_normal_01_test ( )
    call quat_transpose_print_test ( )

    call acos_scalar_test ( )

    call r8mat_print_test ( )
    call r8mat_print_some_test ( )

    call r8vec_print_test ( )
    call r8vec_uniform_01_test ( )

    call rotate_axis_vector_test ( )

    call rotate_axis2mat_test ( )
    call rotate_axis2quat_test ( )

    call rotate_mat_vector_test ( )
    call rotate_mat2axis_test ( )
    call rotate_mat2quat_test ( )

    call rotate_quat_vector_test ( )
    call rotate_quat2axis_test ( )
    call rotate_quat2mat_test ( )
    !
    !  Terminate.
    !
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'QUATERNIONS_TEST'
    write ( *, '(a)' ) '  Normal end of execution.'
    write ( *, '(a)' ) ''


    end subroutine
    
    end module
