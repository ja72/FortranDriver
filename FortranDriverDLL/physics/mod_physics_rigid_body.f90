
    module mod_physics_rigid_body
    use mod_physics_spatial
    use mod_nasa_quat

    type, bind(c) :: quat4
        real(real64) :: data(4)
    end type

    type(quat4), parameter :: q_eye = quat4( [1._real64, 0._real64, 0._real64, 0._real64] )
    type(quat4), parameter :: q_ux_ = quat4( [0._real64, 1._real64, 0._real64, 0._real64] )
    type(quat4), parameter :: q_uy_ = quat4( [0._real64, 0._real64, 1._real64, 0._real64] )
    type(quat4), parameter :: q_uz_ = quat4( [0._real64, 0._real64, 0._real64, 1._real64] )

    interface quat4
    module procedure :: quat4_zeros, quat4_values
    module procedure :: quat4_from_vec3, quat4_from_scalar
    end interface
    
    interface eye
    module procedure :: quat4_identity
    end interface

    type, bind(c) :: rigidbody
        real(real64) :: mass
        real(real64) :: I_1, I_2, I_3
    end type
    
    type, bind(c) :: vector6
        real(real64) :: data(6)
    end type
    
    type, bind(c) :: matrix6
        real(real64) :: data(6,6)
    end type
    
    type(vector6), parameter :: o6_ = vector6( [ &
        0._real64, 0._real64, 0._real64, 0._real64, 0._real64, 0._real64] )
    
    type(matrix6), parameter :: zero6_ = matrix6( [ &
        0._real64, 0._real64, 0._real64, 0._real64, 0._real64, 0._real64, &
        0._real64, 0._real64, 0._real64, 0._real64, 0._real64, 0._real64, &
        0._real64, 0._real64, 0._real64, 0._real64, 0._real64, 0._real64, &
        0._real64, 0._real64, 0._real64, 0._real64, 0._real64, 0._real64, &
        0._real64, 0._real64, 0._real64, 0._real64, 0._real64, 0._real64, &
        0._real64, 0._real64, 0._real64, 0._real64, 0._real64, 0._real64 ] )
    
    type(matrix6), parameter :: eye6_ = matrix6( [ &
        1._real64, 0._real64, 0._real64, 0._real64, 0._real64, 0._real64, &
        0._real64, 1._real64, 0._real64, 0._real64, 0._real64, 0._real64, &
        0._real64, 0._real64, 1._real64, 0._real64, 0._real64, 0._real64, &
        0._real64, 0._real64, 0._real64, 1._real64, 0._real64, 0._real64, &
        0._real64, 0._real64, 0._real64, 0._real64, 1._real64, 0._real64, &
        0._real64, 0._real64, 0._real64, 0._real64, 0._real64, 1._real64 ] )

    abstract interface

    pure subroutine rb_sum_loads(t,pos,ori,vel,omg,frc,tau)
    import
    !DEC$ ATTRIBUTES VALUE :: t
    real( real64  ), intent(in), value :: t
    type( vector3 ), intent(in) :: pos, vel, omg
    type( quat4 ), intent(in) :: ori
    type( vector3 ), intent(out) :: frc, tau
    end subroutine

    end interface
    
    interface operator (+)
    module procedure add_quat4_quat4
    module procedure add_vec6_vec6
    module procedure add_mat6_mat6
    module procedure add_scalar_mat6, add_mat6_scalar
    end interface
    interface operator (-)
    module procedure neg_quat4
    module procedure neg_vec6
    module procedure neg_mat6
    module procedure sub_quat4_quat4
    module procedure sub_vec6_vec6
    module procedure sub_mat6_mat6
    module procedure sub_scalar_mat6, sub_mat6_scalar
    end interface
    interface operator (*)
    module procedure mul_quat4_scalar, mul_scalar_quat4
    module procedure mul_quat4_quat4
    module procedure mul_vec6_scalar, mul_scalar_vec6
    module procedure mul_mat6_scalar, mul_scalar_mat6
    module procedure mul_vec6_mat6, mul_mat6_vec6
    module procedure mul_mat6_mat6
    end interface
    interface operator (/)
    module procedure div_quat4_scalar
    module procedure div_vec6_scalar
    module procedure div_mat6_scalar
    end interface
    interface operator (.i.)
    module procedure dot_quat4_quat4
    module procedure dot_vec6_vec6
    module procedure dot_mat6_mat6
    end interface
    interface operator (.o.)
    module procedure outer_vec6_vec6
    end interface
    interface operator (.x.)
    module procedure cross_quat4_quat4
    end interface
    interface trace
    module procedure trace_mat6
    end interface
    interface transpose
    module procedure trans_mat6
    end interface
    interface det
    module procedure det_mat6
    end interface
    interface inv
    module procedure inv_mat6
    end interface
    interface solve
    module procedure :: solve_mat6_vec6, solve_mat6_mat6
    end interface
    
    interface rot
        module procedure quat4_axis_angle, quat4_from_vec3
        module procedure quat4_to_matrix, quat4_from_matrix
        module procedure quat4_rotate_vec3, quat4_rotate_diag
    end interface
    
    interface twist
        module procedure vec6_twist_pos_pitch
        module procedure vec6_twist_pos
        module procedure vec6_twist_pure
    end interface
    interface wrench
        module procedure vec6_wrench_pos_pitch
        module procedure vec6_wrench_pos
        module procedure vec6_wrench_pure
    end interface

    contains
    
! *****************************************************************************
! GENERAL
! *****************************************************************************
    
    pure function cross_array(a,b) result(v)
    real(real64), intent(in) :: a(3), b(3)
    real(real64) :: v(3)
        v = [ a(2)*b(3) - a(3)*b(2), &
              a(3)*b(1) - a(1)*b(3), &
              a(1)*b(2) - a(2)*b(1) ]
    return
    end
    pure function cross_op(a) result(m)
    real(real64), intent(in) :: a(3)
    real(real64) :: m(3,3)
        m = reshape( &
            [ 0._real64,      a(3),     -a(2), &
              -a(3)    , 0._real64,      a(1), &
               a(2)    ,     -a(1), 0._real64  ], [3,3] )
    return
    end
    
! *****************************************************************************
! QUATERNIONS
! *****************************************************************************
    pure function quat4_zeros()          result(x) bind(c)
    !dec$ attributes dllexport :: quat4_zeros
    type(quat4) :: x
    x%data = 0._real64
    end function
    pure function quat4_identity()       result(x) bind(c)
    !dec$ attributes dllexport :: quat4_identity
    type(quat4) :: x
    x%data = [1._real64, 0._real64, 0._real64, 0._real64]
    end function
    pure function quat4_scalar_vec3(s,v)       result(x) bind(c)
    !dec$ attributes dllexport :: quat4_scalar_vec3
    real( real64 ), intent(in), value :: s
    type( vector3 ), intent(in) :: v
    type(quat4) :: x
    x%data = [s, v%data]
    end function
    function quat4_uniform(seed)         result(x) bind(c)
    !dec$ attributes dllexport :: quat4_uniform
    use mod_fortran
    type(quat4) :: x
    integer ( int32 ), intent(inout) :: seed
    call rotate_normal_01(seed, x%data)
    end function    
    pure function quat4_values(w,x,y,z)  result(q) bind(c)
    !dec$ attributes dllexport :: quat4_values
    type(quat4) :: q
    real(real64), intent(in), value :: w,x,y,z
    q%data = [w, x, y, z]
    end function
    pure function quat4_from_array(values)  result(q) bind(c)
    !dec$ attributes dllexport :: quat4_from_array
    type(quat4) :: q
    real(real64), intent(in) :: values(4)
    integer :: i
    forall(i=1:4)
        q%data(i) = values(i)
    end forall
    end function
    pure function quat4_axis_angle(axis,angle) result(q) bind(c)
    !dec$ attributes dllexport :: quat4_axis_angle
    use mod_nasa_quat
    type(quat4) :: q
    type(vector3), intent(in) :: axis
    real(real64), intent(in), value :: angle
    call rotate_axis2quat(axis%data, angle, q%data)
    return
    end    
    pure function quat4_from_scalar(s) result(q) bind(c)
    !dec$ attributes dllexport :: quat4_from_scalar
    use mod_nasa_quat
    type(quat4) :: q
    real(real64), intent(in) :: s
        q%data = [s, 0._real64, 0._real64, 0._real64 ]
    return
    end
    pure function quat4_from_vec3(v) result(q) bind(c)
    !dec$ attributes dllexport :: quat4_from_vec3
    use mod_nasa_quat
    type(quat4) :: q
    type(vector3), intent(in) :: v
        q%data = [ 0._real64, v%data ]
    return
    end
    
    pure function quat4_scalar(q)        result(s) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: quat4_scalar
    type(quat4), intent(in) :: q
    real(real64) :: s
    s = q%data(1)
    return
    end
    pure function quat4_vector(q)        result(v) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: quat4_vector
    type(quat4), intent(in) :: q
    type(vector3) :: v
    v%data = q%data(2:4)
    return
    end
    pure function norm_quat4(q)          result(s) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: norm_quat4
    use mod_nasa_quat, only : quat_norm
    type(quat4), intent(in) :: q
    real(real64) :: s
    s = quat_norm(q%data)
    return
    end
    pure function quat4_conjugate(q)     result(g) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: quat4_conjugate
    use mod_nasa_quat, only : quat_norm
    type(quat4), intent(in) :: q
    type(quat4) :: g
    g%data = [ q%data(1), -q%data(2:4) ]
    return
    end    
    pure function quat4_exp(q)           result(g) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: quat4_exp
    use mod_nasa_quat, only : quat_norm
    type(quat4), intent(in) :: q
    type(quat4) :: g
    call quat_exponentiate(q%data, g%data)
    return
    end
    pure function neg_quat4(v) result(u) bind(c)
    !dec$ attributes dllexport :: neg_quat4
    type( quat4 ), intent(in) :: v
    type( quat4 ) :: u
    u%data = - v%data
    end function
    pure function mul_scalar_quat4(a,b)  result(z) bind(c)
    !dec$ attributes dllexport :: mul_scalar_quat4
    real(real64), intent(in),value :: a
    type(quat4), intent(in) :: b
    type(quat4) :: z
    z%data = a * b%data
    end function
    pure function mul_quat4_scalar(a,b)  result(z) bind(c)
    !dec$ attributes dllexport :: mul_quat4_scalar
    type(quat4), intent(in) :: a
    real(real64), intent(in), value :: b
    type(quat4) :: z
    z%data = a%data * b
    end function
    pure function div_quat4_scalar(a,b)  result(z) bind(c)
    !dec$ attributes dllexport :: div_quat4_scalar
    type(quat4), intent(in) :: a
    real(real64), intent(in), value :: b
    type(quat4) :: z
    z%data = a%data / b
    end function
    pure function add_quat4_quat4(a,b)   result(z) bind(c)
    !dec$ attributes dllexport :: add_quat4_quat4
    type(quat4), intent(in) :: a
    type(quat4), intent(in) :: b
    type(quat4) :: z
    z%data = a%data + b%data
    end function
    pure function sub_quat4_quat4(a,b)   result(z) bind(c)
    !dec$ attributes dllexport :: sub_quat4_quat4
    type(quat4), intent(in) :: a
    type(quat4), intent(in) :: b
    type(quat4) :: z
    z%data = a%data - b%data
    end function
    pure function mul_quat4_quat4(a,b)   result(z) bind(c)
    !dec$ attributes dllexport :: mul_quat4_quat4
    use mod_nasa_quat
    type(quat4), intent(in) :: a
    type(quat4), intent(in) :: b
    type(quat4) :: z
    call quat_multiply(a%data, b%data, z%data)
    end function
    pure function cross_quat4_quat4(a,b) result(z) bind(c)
    !dec$ attributes dllexport :: cross_quat4_quat4
    use mod_nasa_quat
    type(quat4), intent(in) :: a
    type(quat4), intent(in) :: b
    type(vector3) :: z
    call quat_cross(a%data, b%data, z%data)
    end function
    pure function dot_quat4_quat4(a,b) result(z) bind(c)
    !dec$ attributes dllexport :: dot_quat4_quat4
    use mod_nasa_quat
    type(quat4), intent(in) :: a
    type(quat4), intent(in) :: b
    real(real64) :: z
    call quat_dot(a%data, b%data, z)
    end function    
    pure function quat4_to_matrix(q, inverse) result(R) bind(c)
    !dec$ attributes dllexport :: quat4_to_matrix
    use mod_nasa_quat
    type(quat4), intent(in) :: q
    logical, intent(in), value :: inverse
    type(matrix3) :: R
    call rotate_quat2mat_inv(q%data, R%data, inverse)
    end function
    pure function quat4_from_matrix(R)   result(q) bind(c)
    !dec$ attributes dllexport :: quat4_from_matrix
    use mod_nasa_quat
    type(matrix3), intent(in) :: R
    type(quat4) :: q
    call rotate_mat2quat(R%data, q%data)
    end function    
    pure subroutine quat4_to_axis_angle(q, axis, angle) bind(c)
    !dec$ attributes dllexport :: quat4_to_axis_angle
    use mod_nasa_quat
    type(quat4), intent(in) :: q
    type(vector3), intent(out) :: axis
    real( real64 ), intent(out) :: angle
    call rotate_quat2axis(q%data, axis%data, angle)
    return
    end 
    
    pure function quat4_inverse(q) result(q_inv) bind(c)
    !dec$ attributes dllexport :: quat4_inverse    
    type(quat4), intent(in) :: q
    type(quat4) :: q_inv
    call quat_inverse ( q%data, q_inv%data )
    return
    end 
    
    pure function quat4_normalize(q) result(q_unit) bind(c)
    !dec$ attributes dllexport :: quat4_normalize  
    type(quat4), intent(in) :: q
    type(quat4) :: q_unit
    call rotate_normalize(q%data, q_unit%data)
    return
    end 
    
    pure function quat4_rotate_vec3(q,v,inverse) result(w) bind(c)
    !dec$ attributes dllexport :: quat4_rotate_vec3
    ! rotates a vector v based on the rotation stored in
    ! the quaternion q, and optionally the inverse rotation.
    type( quat4 ), intent(in) :: q
    type( vector3 ), intent(in) :: v
    logical, intent(in) :: inverse
    type( vector3 ) :: w    
        call rotate_quat_vector_inv(q%data, v%data, w%data, inverse)
    return
    end 
                
    pure function quat4_rotate_diag(q, d) result(a) bind(c)
    !dec$ attributes dllexport :: quat4_rotate_diag
    ! rotates a diagonal matrix d into the inertial frame using
    ! the congruent transformation A=trans(R)*D*R, where R is the 
    ! rotation matrix derived from the quaternion q.
    !
    type(quat4), intent(in) :: q
    real( real64 ), intent(in) :: d(3)
    type(matrix3) :: a, R
        call rotate_quat2mat_inv(q%data, R%data, .false.)
        a = mat3_rotate_diag(R, d)
    return
    end 
    
    pure subroutine call_quat4_to_array(a,c) bind(c)
    !dec$ attributes dllexport :: call_quat4_to_array
    type( quat4  ), intent(in) ::a
    real( real64 ), intent(out) :: c(4)
        c = a%data
    end 
    
! *****************************************************************************
! SCREWS
! *****************************************************************************

    pure function vec6_zeros() result(v) bind(c)
    !dec$ attributes dllexport :: vec6_zeros
    type(vector6) :: v
        v = o6_
    return
    end
    pure function mat6_zeros() result(m) bind(c)
    !dec$ attributes dllexport :: mat6_zeros
    type(matrix6) :: m
        m = zero6_
    return
    end
    pure function mat6_eye() result(m) bind(c)
    !dec$ attributes dllexport :: mat6_eye
    type(matrix6) :: m
        m = eye6_
    return
    end
    
    pure function vec6_block(a,b) result(v) bind(c)
    !dec$ attributes dllexport :: vec6_block
    use mod_physics_spatial, only : vector3
    type(vector3), intent(in) :: a,b
    type(vector6) :: v
        v%data(1:3) = a%data
        v%data(4:6) = b%data
    return
    end
    pure function mat6_block(a,b,c,d) result(m) bind(c)
    !dec$ attributes dllexport :: mat6_block
    use mod_physics_spatial, only : matrix3
    type(matrix3), intent(in) :: a,b,c,d
    type(matrix6) :: m
        m%data(1:3, 1:3) = a%data
        m%data(4:6, 1:3) = c%data
        m%data(1:3, 4:6) = b%data
        m%data(4:6, 4:6) = d%data
    return
    end
    
    pure function norm_vec6(a) bind(c) result(s)
    !DEC$ ATTRIBUTES DLLEXPORT :: norm_vec6
    use mod_array_inv, only : norm_array_v
    type(vector6), intent(in) :: a
    real(real64) :: s
    s = norm_array_v(6, a%data)
    return
    end
    pure function norm_mat6(a) bind(c) result(s)
    !DEC$ ATTRIBUTES DLLEXPORT :: norm_mat6
    use mod_array_inv, only : norm_array_m
    type(matrix6), intent(in) :: a
    real(real64) :: s
    s = norm_array_m(6, 6, a%data)
    return
    end
    
    pure function vec6_first(a) result(v) bind(c)
    !dec$ attributes dllexport :: vec6_first
    type(vector6), intent(in) :: a
    type(vector3) :: v
    v%data = a%data(1:3)
    end function
    pure function vec6_second(a) result(v) bind(c)
    !dec$ attributes dllexport :: vec6_second
    type(vector6), intent(in) :: a
    type(vector3) :: v
    v%data = a%data(4:6)
    end function
    pure function neg_vec6(a) result(v) bind(c)
    !dec$ attributes dllexport :: neg_vec6
    type(vector6), intent(in) :: a
    type(vector6) :: v
    v%data = - a%data
    end function
    pure function mul_scalar_vec6(a, b) result(v) bind(c)
    !dec$ attributes dllexport :: mul_scalar_vec6
    real(real64), intent(in) :: a
    type(vector6), intent(in) :: b
    type(vector6) :: v
        v%data = a * b%data
    end function
    pure function mul_vec6_scalar(a, b) result(v) bind(c)
    !dec$ attributes dllexport :: mul_vec6_scalar
    type(vector6), intent(in) :: a
    real(real64), intent(in) :: b    
    type(vector6) :: v
        v%data = a%data * b
    end function
    pure function div_vec6_scalar(a, b) result(v) bind(c)
    !dec$ attributes dllexport :: div_vec6_scalar
    type(vector6), intent(in) :: a
    real(real64), intent(in) :: b    
    type(vector6) :: v
        v%data = a%data / b
    end function
    pure function add_vec6_vec6(a, b) result(v) bind(c)
    !dec$ attributes dllexport :: add_vec6_vec6
    type(vector6), intent(in) :: a, b
    type(vector6) :: v
        v%data = a%data + b%data
    end function
    pure function sub_vec6_vec6(a, b) result(v) bind(c)
    !dec$ attributes dllexport :: sub_vec6_vec6
    type(vector6), intent(in) :: a, b
    type(vector6) :: v
        v%data = a%data - b%data
    end function    
    pure function dot_vec6_vec6(a, b) result(s) bind(c)
    !dec$ attributes dllexport :: dot_vec6_vec6
    type(vector6), intent(in) :: a, b
    real(real64) :: s
    s = dot_product(a%data, b%data)
    end function
    pure function outer_vec6_vec6(a, b) result(r) bind(c)
    !dec$ attributes dllexport :: outer_vec6_vec6
    type(vector6), intent(in) :: a, b
    type(matrix6) :: r
    integer :: i, j
    forall (i=1:6)
        forall(j=1:6) r%data(i,j) = a%data(i)*b%data(j)
    end forall
    end function
    
    pure function mat6_scalar(s) result(v) bind(c)
    !dec$ attributes dllexport :: mat6_scalar
    real( real64 ), intent(in) :: s
    type(matrix6) :: v    
        v%data = s * eye6_%data    
    end function
    
    pure function neg_mat6(a) result(v) bind(c)
    !dec$ attributes dllexport :: neg_mat6
    type(matrix6), intent(in) :: a
    type(matrix6) :: v
    v%data = - a%data
    end function
    pure function mul_scalar_mat6(a, b) result(v) bind(c)
    !dec$ attributes dllexport :: mul_scalar_mat6
    real(real64), intent(in) :: a
    type(matrix6), intent(in) :: b
    type(matrix6) :: v
        v%data = a * b%data
    end function
    pure function mul_mat6_scalar(a, b) result(v) bind(c)
    !dec$ attributes dllexport :: mul_mat6_scalar
    type(matrix6), intent(in) :: a
    real(real64), intent(in) :: b    
    type(matrix6) :: v
        v%data = a%data * b
    end function
    pure function div_mat6_scalar(a, b) result(v) bind(c)
    !dec$ attributes dllexport :: div_mat6_scalar
    type(matrix6), intent(in) :: a
    real(real64), intent(in) :: b    
    type(matrix6) :: v
        v%data = a%data / b
    end function
    pure function add_mat6_mat6(a, b) result(v) bind(c)
    !dec$ attributes dllexport :: add_mat6_mat6
    type(matrix6), intent(in) :: a, b
    type(matrix6) :: v
        v%data = a%data + b%data
    end function
    pure function sub_mat6_mat6(a, b) result(v) bind(c)
    !dec$ attributes dllexport :: sub_mat6_mat6
    type(matrix6), intent(in) :: a, b
    type(matrix6) :: v
        v%data = a%data - b%data
    end function    
    pure function add_scalar_mat6(a, b) result(v) bind(c)
    !dec$ attributes dllexport :: add_scalar_mat6
    real(real64), intent(in) :: a
    type(matrix6), intent(in) :: b
    type(matrix6) :: v
        v%data = a*eye6_%data + b%data
    end function
    pure function add_mat6_scalar(b, a) result(v) bind(c)
    !dec$ attributes dllexport :: add_mat6_scalar
    real(real64), intent(in) :: a
    type(matrix6), intent(in) :: b
    type(matrix6) :: v
        v%data =  b%data + a*eye6_%data
    end function
    pure function sub_scalar_mat6(a, b) result(v) bind(c)
    !dec$ attributes dllexport :: sub_scalar_mat6
    real(real64), intent(in) :: a
    type(matrix6), intent(in) :: b
    type(matrix6) :: v
        v%data = a*eye6_%data - b%data
    end function
    pure function sub_mat6_scalar(b, a) result(v) bind(c)
    !dec$ attributes dllexport :: sub_mat6_scalar
    real(real64), intent(in) :: a
    type(matrix6), intent(in) :: b
    type(matrix6) :: v
        v%data =  b%data - a*eye6_%data
    end function
    
    pure function mul_mat6_vec6(a, b) result(v) bind(c)
    !dec$ attributes dllexport :: mul_mat6_vec6
    type(matrix6), intent(in) :: a
    type(vector6), intent(in) :: b
    type(vector6) :: v    
        v%data = matmul( a%data, b%data )
    return 
    end
    pure function mul_vec6_mat6(b, a) result(v) bind(c)
    !dec$ attributes dllexport :: mul_vec6_mat6
    type(vector6), intent(in) :: b
    type(matrix6), intent(in) :: a
    type(vector6) :: v    
        v%data = matmul( b%data, a%data )
    return 
    end
    
    pure function mul_mat6_mat6(a, b) result(v) bind(c)
    !dec$ attributes dllexport :: mul_mat6_mat6
    type(matrix6), intent(in) :: a, b
    type(matrix6) :: v
        v%data = matmul( a%data, b%data)
    end function    
    pure function dot_mat6_mat6(a, b) result(v) bind(c)
    !dec$ attributes dllexport :: dot_mat6_mat6
    type(matrix6), intent(in) :: a, b
    type(matrix6) :: v
        v%data = matmul( transpose(a%data), b%data)
    end function    
    pure function trans_mat6(a) result(t) bind(c)
    !dec$ attributes dllexport :: trans_mat6
    type(matrix6), intent(in) :: a
    type(matrix6) :: t
    t%data = transpose(a%data)
    end function
    pure function trace_mat6(a) result(t) bind(c)
    !dec$ attributes dllexport :: trace_mat6
    type(matrix6), intent(in) :: a
    real(real64) :: t
    t = trace(a%data)
    end function
    pure function det_mat6(a) result(d) bind(c)
    !dec$ attributes dllexport :: det_mat6
    type(matrix6), intent(in) :: a
    real(real64) :: d
    d = det(a%data)
    end function
    pure function inv_mat6(a) result(t) bind(c)
    !dec$ attributes dllexport :: inv_mat6
    use mod_array_inv, only : inv_array6
    type(matrix6), intent(in) :: a
    type(matrix6) :: t
        t%data = inv_array6(a%data) 
    end function
    pure function solve_mat6_vec6(a,b) result(x) bind(c)
    !dec$ attributes dllexport :: solve_mat6_vec6
    type(matrix6), intent(in) :: a
    type(vector6), intent(in) :: b
    type(vector6) :: x
    x = vector6( solve(a%data, b%data) )
    end function
    pure function solve_mat6_mat6(a, b) result(x) bind(c)
    !dec$ attributes dllexport :: solve_mat6_mat6
    type(matrix6), intent(in) :: a, b
    type(matrix6) :: x
    x = matrix6( solve(a%data, b%data) )
    end function
    
    pure function vec6_twist_pos_pitch(a,r,h) result(v) bind(c)
    !dec$ attributes dllexport :: vec6_twist_pos_pitch
    type(vector3), intent(in) :: a
    type(vector3), intent(in) :: r
    real(real64), intent(in), value :: h
    type(vector6) :: v
        v%data = [ cross_array(r%data, a%data) + h * a%data, a%data ]
    end function
    pure function vec6_twist_pos(a,r) result(v) bind(c)
    !dec$ attributes dllexport :: vec6_twist_pos
    type(vector3), intent(in) :: a
    type(vector3), intent(in) :: r
    type(vector6) :: v
        v%data = [ cross_array(r%data, a%data) , a%data ]
    end function
    pure function vec6_twist_pure(a) result(v) bind(c)
    !dec$ attributes dllexport :: vec6_twist_pure
    type(vector3), intent(in) :: a
    type(vector6) :: v
        v%data = [ a%data, [0._real64, 0._real64, 0._real64] ]
    end function

    pure function vec6_wrench_pos_pitch(a,r,h) result(v) bind(c)
    !dec$ attributes dllexport :: vec6_wrench_pos_pitch
    type(vector3), intent(in) :: a
    type(vector3), intent(in) :: r
    real(real64), intent(in), value :: h
    type(vector6) :: v
        v%data = [ a%data, cross_array(r%data, a%data) + h * a%data ]
    end function
    pure function vec6_wrench_pos(a,r) result(v) bind(c)
    !dec$ attributes dllexport :: vec6_wrench_pos
    type(vector3), intent(in) :: a
    type(vector3), intent(in) :: r
    type(vector6) :: v
        v%data = [ a%data, cross_array(r%data, a%data) ]
    end function
    pure function vec6_wrench_pure(a) result(v) bind(c)
    !dec$ attributes dllexport :: vec6_wrench_pure
    type(vector3), intent(in) :: a
    type(vector6) :: v
        v%data = [ [0._real64, 0._real64, 0._real64], a%data  ]
    end function
    
    pure function cross_twist_twist(a,b) result(v) bind(c)
    !dec$ attributes dllexport :: cross_twist_twist
    type(vector6), intent(in) :: a, b    
    type(vector6) :: v
        v%data = [ &
            cross_array(a%data(4:6), b%data(1:3)) + cross_array(a%data(1:3), b%data(4:6)), &
            cross_array(a%data(4:6), b%data(4:6)) ]
    return
    end 
    pure function cross_twist_wrench(a,b) result(v) bind(c)
    !dec$ attributes dllexport :: cross_twist_wrench
    type(vector6), intent(in) :: a, b    
    type(vector6) :: v
        v%data = [ &
            cross_array(a%data(4:6), b%data(1:3)), &
            cross_array(a%data(1:3), b%data(1:3)) + cross_array(a%data(4:6), b%data(4:6)) ]
    return
    end 

    pure function cross_twist_twist_op(a) result(m) bind(c)
    !dec$ attributes dllexport :: cross_twist_twist_op
    use mod_physics_spatial, only : vector3, zero3_, cross_vec3_op
    type(vector6), intent(in) :: a 
    type(matrix6) :: m
    type(matrix3) :: wx, vx
        vx = cross_vec3_op( vec6_first(a) )
        wx = cross_vec3_op( vec6_second(a) )
        m = mat6_block( wx, vx, zero3_, wx)            
    return
    end 
    pure function cross_twist_wrench_op(a) result(m) bind(c)
    !dec$ attributes dllexport :: cross_twist_wrench_op
    use mod_physics_spatial, only : vector3, zero3_, cross_vec3_op
    type(vector6), intent(in) :: a 
    type(matrix6) :: m
    type(matrix3) :: wx, vx
        vx = cross_vec3_op( vec6_first(a) )
        wx = cross_vec3_op( vec6_second(a) )
        m = mat6_block( wx, zero3_, vx, wx)            
    return
    end 
    
    pure subroutine call_vec6_to_array(a,c) bind(c)
    !dec$ attributes dllexport :: call_vec6_to_array
    type(vector6), intent(in) ::a
    real( real64 ), intent(out) :: c(6)
        c = a%data
    end 
    pure subroutine call_mat6_to_array(a,c) bind(c)
    !dec$ attributes dllexport :: call_mat6_to_array
    type(matrix6), intent(in) ::a
    real( real64 ), intent(out) :: c(6,6)
        c = a%data
    end     
    
    
! *****************************************************************************
! RIGID BODIES
! *****************************************************************************
    pure subroutine rb_get_state(rb,pos,q,vee,omg,y) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: rb_get_state
    type( rigidbody ) , intent(in)  :: rb
    type( vector3 )   , intent(in)  :: pos, vee, omg
    type( quat4 )     , intent(in)  :: q
    real( real64 )    , intent(out) :: y(13)
    real( real64 )                  :: m, d(3)
    type( matrix3 )                 :: R, I
    type( vector3 )                 :: mom, ang

    call rotate_quat2mat_inv(q%data, r%data, .true.)

    m = rb%mass
    d = [ rb%I_1, rb%I_2, rb%I_3 ]

    call rotate_quat2mat_inv(q%data, r%data, .false.)
    I = mat3_rotate_diag(r, d)

    mom%data = m * vee%data
    ang%data = matmul(I%data, omg%data)

    y(1:3)  = pos%data
    y(4:7)  = q%data
    y(8:10) = mom%data
    y(11:13)= ang%data

    end subroutine
    pure subroutine rb_set_state(rb,y,pos,q,vee,omg) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: rb_set_state
    type( rigidbody ), intent(in) :: rb
    real( real64 ), intent(in)    :: y(13)
    type( vector3 ), intent(out)  :: pos, vee, omg
    type( quat4 ), intent(out)    :: q
    real( real64 )                :: m, d(3)
    type( matrix3 )               :: R, I_inv
    type( vector3 )               :: mom, ang

    pos%data = y(1:3)
    q  %data = y(4:7)
    mom%data = y(8:10)
    ang%data = y(11:13)

    call rotate_quat2mat_inv(q%data, r%data, .false.)

    m = rb%mass
    d = [ rb%I_1, rb%I_2, rb%I_3 ]

    I_inv = mat3_rotate_diag(r, 1/d)
    vee%data = mom%data / m
    omg%data = matmul(I_inv%data, ang%data)

    end subroutine
    pure subroutine rb_state_derivative(rb,t,y,f,yp)
    !DEC$ ATTRIBUTES DLLEXPORT :: rb_state_derivative
    !DEC$ ATTRIBUTES ALIAS: 'rb_state_derivative' :: rb_state_derivative
    !DEC$ ATTRIBUTES VALUE :: t
    !!DEC$ ATTRIBUTES REFERENCE :: f, y, yp
    use mod_nasa_quat
    type(rigidbody), intent(in) :: rb
    real(real64), intent(in)    :: t, y(13)
    real(real64), intent(out)   :: yp(13)
    procedure(rb_sum_loads)     :: f
    type(vector3) :: pos, vee, omg
    type(quat4)   :: q, qp
    type(vector3) :: frc, tau

    ! Get motion parameters from state 'Y'
    call rb_set_state(rb, y, pos, q, vee, omg)
    ! Get quaterion derivative from rot. velocity 'omg'
    call quat_derivative(q%data, omg%data, qp%data)
    ! Get applied forces and torques (external)
    call f(t, pos, q, vee, omg, frc, tau)

    ! Derivative of state vector
    yp = [ vee%data, qp%data, frc%data, tau%data ]

    end subroutine
    
    pure function rb_get_inertia_matrix(rb, pos, q, inverse) result(H) bind(c)
    !dec$ attributes dllexport :: rb_get_inertia_matrix
    use mod_physics_spatial, only : cross,neg_mat3
    type( rigidbody ), intent(in) :: rb
    type( vector3   ), intent(in) :: pos
    type( quat4     ), intent(in) :: q
    logical, intent(in) :: inverse
    real( real64 ) :: mmoi(3), m
    type(matrix6) :: H
    type(matrix3) :: cx, ncx, Ic
    
    !tex:Spatial Inertia
    ! $$ {\bf I}=\begin{bmatrix}m & -m\vec{c}\times\\
    !m\vec{c}\times & {\rm I}_{C}-m\vec{c}\times\vec{c}\times
    !\end{bmatrix}$$
    !
    ! Spatial Mobility
    ! $${\bf I}^{-1}=\begin{bmatrix}\tfrac{1}{m}-\vec{c}\times{\rm I}_{C}^{-1}\vec{c}\times & \vec{c}\times{\rm I}_{C}^{-1}\\
    !-{\rm I}_{C}^{-1}\vec{c}\times & {\rm I}_{C}^{-1}
    !\end{bmatrix}$$
    
        m = rb%mass
        mmoi = [ rb%I_1, rb%I_2, rb%I_3 ]
        cx = cross(pos)
        ncx = -cx
        if( .not. inverse ) then
            ! Ic is MMOI here
            Ic = quat4_rotate_diag(q, mmoi)
            H = mat6_block( &
                m*eye3_ , &
                m*ncx, &
                m*cx, &
                Ic + m*cx*ncx )
        else
            ! Ic is inverse MMOI here
            Ic = quat4_rotate_diag(q, 1/mmoi)
            H = mat6_block( &
                (1/m)*eye3_ + cx*Ic*ncx, &
                cx*Ic, &
                Ic*ncx, & !(-1._real64)*Ic*cx, &
                Ic )
        end if        
    return
    end
    
    end module
    
