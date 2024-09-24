module mod_rigid_bodies
use mod_nasa_quat

    type, bind(c) :: rigidbody
        real(real64) :: mass
        real(real64) :: Ixx, Iyy, Izz
    end type

    abstract interface

    pure subroutine rb_sum_loads(t,pos,ori,vel,omg,frc,tau)
    import
    !DEC$ ATTRIBUTES VALUE :: t
    real(real64), intent(in) :: t
    real(real64), intent(in) :: pos(3), ori(4), vel(3), omg(3)
    real(real64), intent(out) :: frc(3), tau(3)
    end subroutine

    end interface
    

    contains
    
    pure subroutine rotation_q8_diag2mat(q, d, a) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: rotation_q8_diag2mat
    ! 
    !*****************************************************************************80
    !
    !! ROTATION_Q8_DIAG2MAT rotates a diagonal matrix into the inertial frame using
    !  the congruent transformation A=trans(R)*D*R
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    07 July 2024
    !
    !  Author:
    !
    !    John Alexiou
    !
    !
    !  Parameters:
    !
    !    Input, real ( real64 ) Q(4), the quaternion representing the rotation.
    !
    !    Input, real ( real64 ) D(3), the three diagonal terms of the diagonal matrix
    !
    !    Output, real ( real64 ) A(3,3), the resulting matrix.
    !
    implicit none
    real( real64 ), intent(in) :: q(4)
    real ( real64 ), intent(in) :: d(dim_num)
    real ( real64 ), intent(out) :: a(dim_num,dim_num)
    real ( real64 ) :: R(dim_num,dim_num)    
        call rotation_quat2mat_inv(q, r, .false.)        
        call rotation_rot_diag2mat(r, d, a)                
    end subroutine

    pure subroutine rotation_rot_diag2mat(R, d, a) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: rotation_rot_diag2mat
    ! 
    !*****************************************************************************80
    !
    !! ROTATION_ROT_DIAG2MAT rotates a diagonal matrix into the inertial frame using
    !  the congruent transformation A=trans(R)*D*R
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    07 July 2024
    !
    !  Author:
    !
    !    John Alexiou
    !
    !
    !  Parameters:
    !
    !    Input, real ( real64 ) R(3,3), the rotation matrix.
    !
    !    Input, real ( real64 ) D(3), the three diagonal terms of the diagonal matrix
    !
    !    Output, real ( real64 ) A(3,3), the resulting matrix.
    !
    implicit none
    real( real64 ), intent(in) :: R(dim_num,dim_num)
    real ( real64 ), intent(in) :: d(dim_num)
    real ( real64 ), intent(out) :: a(dim_num,dim_num)
    integer :: i
            
        forall(i=1:dim_num)
            a(i,:) = d(i) * r(:,i)
        end forall
        
        a = matmul(r, a)
    
    end subroutine
    
    
    pure subroutine rb_get_state(rb,pos,q,vee,omg,y) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: rb_get_state
    type(rigidbody), intent(in) :: rb
    real(real64), intent(in) :: pos(3), q(4), vee(3), omg(3)
    real(real64), intent(out) :: y(13)
    real(real64) :: R(3,3), m, d(3), I(3,3), mom(3), ang(3)

    call rotation_quat2mat_inv(q, r, .true.)

    m = rb%mass
    d = [ rb%Ixx, rb%Iyy, rb%Izz ]
    
    call rotation_quat2mat_inv(q, r, .false.)
    call rotation_rot_diag2mat(r, d, I)
    
    mom = m * vee
    ang = matmul(I, omg)

    y(1:3)  = pos
    y(4:7)  = q
    y(8:10) = mom
    y(11:13)= ang

    end subroutine
    
    pure subroutine rb_set_state(rb,y,pos,q,vee,omg) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: rb_set_state
    type(rigidbody), intent(in) :: rb
    real(real64), intent(in) :: y(13)
    real(real64), intent(out) :: pos(3), q(4), vee(3), omg(3)
    real(real64) :: R(3,3), m, d(3), I_inv(3,3), mom(3), ang(3)
            
    pos = y(1:3)
    q   = y(4:7)
    mom = y(8:10)
    ang = y(11:13)
    
    call rotation_quat2mat_inv(q, r, .false.)    

    m = rb%mass
    d = [ rb%Ixx, rb%Iyy, rb%Izz ]

    call rotation_rot_diag2mat(r, 1/d, I_inv)
    
    vee = mom / m
    omg = matmul(I_inv, ang)
    
    end subroutine
    
    pure subroutine rb_state_derivative(rb,t,y,f,yp)
    !DEC$ ATTRIBUTES DLLEXPORT :: rb_state_derivative
    !DEC$ ATTRIBUTES ALIAS: 'rb_state_derivative' :: rb_state_derivative
    !DEC$ ATTRIBUTES VALUE :: t
    !!DEC$ ATTRIBUTES REFERENCE :: f, y, yp
    use mod_nasa_quat
    type(rigidbody), intent(in) :: rb
    real(real64), intent(in) :: t, y(13)
    real(real64), intent(out) :: yp(13)
    procedure(rb_sum_loads) :: f

    real(real64) :: pos(3), q(4), vee(3), omg(3)
    real(real64) :: frc(3), tau(3), qp(4)
    
    ! Get motion parameters from state 'Y'
    call rb_set_state(rb, y, pos, q, vee, omg)
    ! Get quaterion derivative from rot. velocity 'omg'
    call q8_derivative(q, omg, qp)
    ! Get applied forces and torques (external)
    call f(t, pos, q, vee, omg, frc, tau)

    ! Derivative of state vector 
    yp = [ vee, qp, frc, tau ]

    end subroutine
    

end module