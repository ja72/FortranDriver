
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
    end interface
    
    interface eye
    module procedure :: quat4_identity
    end interface

    type, bind(c) :: rigidbody
        real(real64) :: mass
        real(real64) :: Ixx, Iyy, Izz
    end type

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

    contains

    ! quat4
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
    pure function quat4_array(values)  result(q) bind(c)
    !dec$ attributes dllexport :: quat4_array
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
    pure function mul_scalar_quat4(a,b)  result(z) bind(c)
    !dec$ attributes dllexport :: mul_scalar_quat4
    real(real64), intent(in),value :: a
    type(quat4), intent(in) :: b
    type(quat4) :: z
    z%data = a * b%data
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
    pure function inner_quat4_quat4(a,b) result(z) bind(c)
    !dec$ attributes dllexport :: inner_quat4_quat4
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
    d = [ rb%Ixx, rb%Iyy, rb%Izz ]

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
    d = [ rb%Ixx, rb%Iyy, rb%Izz ]

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

    end module