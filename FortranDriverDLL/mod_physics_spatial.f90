    module mod_physics_spatial
    use mod_array_inv
    
    type, bind(c) :: vector3
        real(real64) :: data(3)
    end type
    type, bind(c) :: matrix3
        real(real64) :: data(3,3)
    end type
    interface vector3
    module procedure :: vec3_zeros, vec3_values
    end interface
    interface matrix3
    module procedure :: mat3_zeros, mat3_values
    end interface
    
    type(vector3), parameter :: o3_ = vector3( [0._real64, 0._real64, 0._real64] )
    type(vector3), parameter :: i3_ = vector3( [1._real64, 0._real64, 0._real64] )
    type(vector3), parameter :: j3_ = vector3( [0._real64, 1._real64, 0._real64] )
    type(vector3), parameter :: k3_ = vector3( [0._real64, 0._real64, 1._real64] )

    type(matrix3), parameter :: zero3_ = matrix3( reshape( &
        [0._real64, 0._real64, 0._real64, &
        0._real64, 0._real64, 0._real64, &
        0._real64, 0._real64, 0._real64], [3,3] ) )

    type(matrix3), parameter :: eye3_ = matrix3( reshape( &
        [1._real64, 0._real64, 0._real64, &
        0._real64, 1._real64, 0._real64, &
        0._real64, 0._real64, 1._real64], [3,3] ) )

    type(matrix3), parameter :: ones3_ = matrix3( reshape( &
        [1._real64, 1._real64, 1._real64, &
        1._real64, 1._real64, 1._real64, &
        1._real64, 1._real64, 1._real64], [3,3] ) )
    
    interface operator (+)
    module procedure add_vec3_vec3
    module procedure add_mat3_mat3
    module procedure add_scalar_mat3, add_mat3_scalar
    end interface
    interface operator (-)
    module procedure sub_vec3_vec3
    module procedure sub_mat3_mat3
    module procedure sub_scalar_mat3, sub_mat3_scalar
    end interface
    interface operator (*)
    module procedure mul_vec3_scalar, mul_scalar_vec3
    module procedure mul_mat3_scalar, mul_scalar_mat3
    module procedure mul_vec3_array, mul_array_vec3
    module procedure mul_vec3_mat3, mul_mat3_vec3
    module procedure mul_mat3_mat3
    end interface
    interface operator (/)
    module procedure div_vec3_scalar
    module procedure div_mat3_scalar
    end interface
    interface assignment (=)
    module procedure array_to_vec3, vec3_to_array
    module procedure array_to_mat3, mat3_to_array
    end interface
    interface operator (.i.)
    module procedure inner_vec3_vec3
    module procedure inner_mat3_mat3
    end interface
    interface operator (.x.)
    module procedure cross_vec3_vec3
    end interface
    interface operator (.o.)
    module procedure outer_vec3_vec3
    end interface
    interface cross
    module procedure cross_vec3_vec3
    module procedure cross_vec3_op
    end interface
    interface operator (.t.)
    module procedure transpose_mat3, inner_mat3_mat3
    end interface
    interface transpose
    module procedure transpose_mat3
    end interface
    interface trace
    module procedure trace_mat3
    end interface
    interface det
    module procedure :: determinant_mat3
    end interface
    interface inv
    module procedure :: inverse_mat3
    end interface
    interface solve
    module procedure :: solve_mat3_vec3, solve_mat3_mat3
    end interface
    
    contains
    
    ! vector3
    pure function vec3_zeros() result(x) bind(c)
    !dec$ attributes dllexport :: vec3_zeros
    type(vector3) :: x
    x%data = 0._real64
    end function
    pure function vec3_ux() result(x) bind(c)
    !dec$ attributes dllexport :: vec3_ux
    type(vector3) :: x
    x%data(1) = 1._real64
    x%data(2) = 0._real64
    x%data(3) = 0._real64
    end function
    pure function vec3_uy() result(x) bind(c)
    !dec$ attributes dllexport :: vec3_uy
    type(vector3) :: x
    x%data(1) = 0._real64
    x%data(2) = 1._real64
    x%data(3) = 0._real64
    end function
    pure function vec3_uz() result(x) bind(c)
    !dec$ attributes dllexport :: vec3_uz
    type(vector3) :: x
    x%data(1) = 0._real64
    x%data(2) = 0._real64
    x%data(3) = 1._real64
    end function
    pure function vec3_ones() result(x) bind(c)
    !dec$ attributes dllexport :: vec3_ones
    type(vector3) :: x
    x%data = 1._real64
    end function
    pure function mat3_zeros() result(x) bind(c)
    !dec$ attributes dllexport :: mat3_zeros
    type(matrix3) :: x
    x%data = 0._real64
    end function
    pure function mat3_eye() result(x) bind(c)
    !dec$ attributes dllexport :: mat3_eye
    type(matrix3) :: x
    integer :: i
    x%data = 0._real64
    forall(i=1:3)
        x%data(i,i) = 1._real64
    end forall
    end function
    pure function mat3_ones() result(x) bind(c)
    !dec$ attributes dllexport :: mat3_ones
    type(matrix3) :: x
    x%data = 1._real64
    end function
    pure function vec3_values(x,y,z) bind(c) result(v)
    !dec$ attributes dllexport :: vec3_values
    type(vector3) :: v
    real(real64), intent(in), value :: x,y,z
    v%data = [x, y, z]
    end function
    pure function mat3_values(a11,a12,a13,a21,a22,a23,a31,a32,a33) bind(c) result(v)
    !dec$ attributes dllexport :: mat3_values
    type(matrix3) :: v
    real(real64), intent(in), value :: a11,a12,a13,a21,a22,a23,a31,a32,a33
    v%data = reshape( [a11,a21,a31, a12,a22,a32, a13,a23,a33], [3,3] )
    end function
    pure subroutine array_to_vec3(v, a) bind(c)
    !!dec$ attributes dllexport :: array_to_vec3
    type(vector3), intent(out) :: v
    real(real64), intent(in) :: a(3)
    v%data = a
    end subroutine
    pure subroutine vec3_to_array(a, v) bind(c)
    !!dec$ attributes dllexport :: vec3_to_array
    real(real64), intent(out) :: a(3)
    type(vector3), intent(in) :: v
    a = v%data
    end subroutine
    pure subroutine array_to_mat3(mx, a) bind(c)
    !!dec$ attributes dllexport :: array_to_mat3
    type(matrix3), intent(out) :: mx
    real(real64), intent(in) :: a(3,3)
    mx%data = a
    end subroutine
    pure subroutine mat3_to_array(a, mx) bind(c)
    !!dec$ attributes dllexport :: mat3_to_array
    real(real64), intent(out) :: a(3,3)
    type(matrix3), intent(in) :: mx
    a = mx%data
    end subroutine
    function vec3_uniform(seed) result(x) bind(c)
    !dec$ attributes dllexport :: vec3_uniform
    use mod_fortran
    type(vector3) :: x
    integer ( int32 ), intent(inout) :: seed
    real(real64) :: s
    call call_uniform_array_v(3, seed, x%data)
    s = norm2(x%data)
    x%data = x%data/s
    end function
    function mat3_uniform(seed) result(x) bind(c)
    !dec$ attributes dllexport :: mat3_uniform
    use mod_fortran
    type(matrix3) :: x
    integer ( int32 ), intent(inout) :: seed
    real(real64) :: s
    call call_uniform_array_m(3, 3, seed, x%data)
    s = norm2(x%data)
    x%data = x%data/s
    end function
    pure function norm_vec3(q) bind(c) result(s)
    !DEC$ ATTRIBUTES DLLEXPORT :: norm_vec3
    use mod_array_inv, only : norm_array_v
    type(vector3), intent(in) :: q
    real(real64) :: s
    s = norm_array_v(3, q%data)
    return
    end
    pure function add_vec3_vec3(a, b) result(r) bind(c)
    !dec$ attributes dllexport :: add_vec3_vec3
    type(vector3), intent(in) :: a,b
    type(vector3) :: r
    r%data = a%data + b%data
    end function
    pure function sub_vec3_vec3(a, b) result(r) bind(c)
    !dec$ attributes dllexport :: sub_vec3_vec3
    type(vector3), intent(in) :: a,b
    type(vector3) :: r
    r%data = a%data - b%data
    end function
    pure function add_mat3_mat3(a, b) result(r) bind(c)
    !dec$ attributes dllexport :: add_mat3_mat3
    type(matrix3), intent(in) :: a,b
    type(matrix3) :: r
    r%data = a%data + b%data
    end function
    pure function add_scalar_mat3(s,m) result(u) bind(c)
    !dec$ attributes dllexport :: add_scalar_mat3
    real(real64), intent(in), value :: s
    type(matrix3), intent(in) :: m
    type(matrix3) :: u
    integer :: i
        u%data = m%data
        forall (i=1:3)
            u%data(i,i) = s + u%data(i,i)
        end forall
    end function
    pure function add_mat3_scalar(m,s) result(u) bind(c)
    !dec$ attributes dllexport :: add_mat3_scalar
    type(matrix3), intent(in) :: m
    real(real64), intent(in), value :: s
    type(matrix3) :: u
    integer :: i
        u%data = m%data
        forall (i=1:3)
            u%data(i,i) = u%data(i,i) + s
        end forall
    end function
    pure function sub_mat3_mat3(a, b) result(r) bind(c)
    !dec$ attributes dllexport :: sub_mat3_mat3
    type(matrix3), intent(in) :: a, b
    type(matrix3) :: r
    r%data = a%data - b%data
    end function
    pure function sub_scalar_mat3(s, m) result(u) bind(c)
    !dec$ attributes dllexport :: sub_scalar_mat3
    real(real64), intent(in), value :: s
    type(matrix3), intent(in) :: m
    type(matrix3) :: u
    integer :: i
        u%data = -m%data
        forall (i=1:3)
            u%data(i,i) = s + u%data(i,i)
        end forall
    end function
    pure function sub_mat3_scalar(m, s) result(u) bind(c)
    !dec$ attributes dllexport :: sub_mat3_scalar
    type(matrix3), intent(in) :: m
    real(real64), intent(in), value :: s
    type(matrix3) :: u
    integer :: i
        u%data = m%data
        forall (i=1:3)
            u%data(i,i) = u%data(i,i) -s
        end forall
    end function
    pure function neg_vec3(v) result(u) bind(c)
    !dec$ attributes dllexport :: neg_vec3
    type(vector3), intent(in) :: v
    type(vector3) :: u
    u%data = - v%data
    end function
    pure function neg_mat3(v) result(u) bind(c)
    !dec$ attributes dllexport :: neg_mat3
    type(matrix3), intent(in) :: v
    type(matrix3) :: u
    u%data = - v%data
    end function    
    pure function mul_vec3_scalar(v, s) result(u) bind(c)
    !dec$ attributes dllexport :: mul_vec3_scalar
    type(vector3), intent(in) :: v
    real(real64), intent(in), value :: s
    type(vector3) :: u
    u%data = v%data * s
    end function
    pure function mul_scalar_vec3(s, v) result(u) bind(c)
    !dec$ attributes dllexport :: mul_scalar_vec3
    real(real64), intent(in), value :: s
    type(vector3), intent(in) :: v
    type(vector3) :: u
    u%data = s * v%data
    end function
    pure function mul_mat3_scalar(m, s) result(u) bind(c)
    !dec$ attributes dllexport :: mul_mat3_scalar
    type(matrix3), intent(in) :: m
    real(real64), intent(in) :: s
    type(matrix3) :: u
    u%data = m%data * s
    end function
    pure function mul_scalar_mat3(s, m) result(u) bind(c)
    !dec$ attributes dllexport :: mul_scalar_mat3
    real(real64), intent(in) :: s
    type(matrix3), intent(in) :: m
    type(matrix3) :: u
    u%data = s * m%data
    end function
    pure function mul_mat3_mat3(w, m) result(u) bind(c)
    !dec$ attributes dllexport :: mul_mat3_mat3
    type(matrix3), intent(in) :: w
    type(matrix3), intent(in) :: m
    type(matrix3) :: u
    u%data = matmul(w%data, m%data)
    end function
    pure function mul_array_vec3(a, v) result(u) 
    type(vector3), intent(in) :: v
    real(real64), intent(in) :: a(3,3)
    type(vector3) :: u
    u%data = matmul( a, v%data)
    end function
    pure function mul_vec3_array(v, a) result(u) 
    type(vector3), intent(in) :: v
    real(real64), intent(in) :: a(3,3)
    type(vector3) :: u
    u%data = matmul( v%data, a)
    end function
    pure function mul_mat3_vec3(a, v) result(u) bind(c)
    !dec$ attributes dllexport :: mul_mat3_vec3
    type(matrix3), intent(in) :: a
    type(vector3), intent(in) :: v
    type(vector3) :: u
    u%data = matmul( a%data, v%data)
    end function
    pure function mul_vec3_mat3(v, a) result(u) bind(c)
    !dec$ attributes dllexport :: mul_vec3_mat3
    type(vector3), intent(in) :: v
    type(matrix3), intent(in) :: a
    type(vector3) :: u
    u%data = matmul( v%data, a%data)
    end function
    pure function div_vec3_scalar(v, d) result(u) bind(c)
    !dec$ attributes dllexport :: div_vec3_scalar
    type(vector3), intent(in) :: v
    real(real64), intent(in), value :: d
    type(vector3) :: u
    u%data = (1._real64/d) * v%data
    end function
    pure function div_mat3_scalar(m, d) result(u) bind(c)
    !dec$ attributes dllexport :: div_mat3_scalar
    type(matrix3), intent(in) :: m
    real(real64), intent(in), value :: d
    type(matrix3) :: u
    u%data = (1._real64/d) * m%data
    end function
    pure function transpose_mat3(a) result(t) bind(c)
    !dec$ attributes dllexport :: transpose_mat3
    type(matrix3), intent(in) :: a
    type(matrix3) :: t
    t%data = transpose(a%data)
    end function
    pure function inner_mat3_mat3(w, m) result(u) bind(c)
    !dec$ attributes dllexport :: inner_mat3_mat3
    type(matrix3), intent(in) :: w
    type(matrix3), intent(in) :: m
    type(matrix3) :: u
    u%data = matmul(transpose(w%data), m%data)
    end function
    pure function trace_mat3(a) result(t) bind(c)
    !dec$ attributes dllexport :: trace_mat3
    type(matrix3), intent(in) :: a
    real(real64) :: t
    t = trace(a%data)
    end function
    pure function determinant_mat3(a) result(d) bind(c)
    !dec$ attributes dllexport :: determinant_mat3
    type(matrix3), intent(in) :: a
    real(real64) :: d
    d = det(a%data)
    end function
    pure function inverse_mat3(a) result(b) bind(c)
    !dec$ attributes dllexport :: inverse_mat3
    type(matrix3), intent(in) :: a
    type(matrix3) :: b
    b%data = inv(a%data)
    end function
    pure function solve_mat3_vec3(a,b) result(x) bind(c)
    !dec$ attributes dllexport :: solve_mat3_vec3
    type(matrix3), intent(in) :: a
    type(vector3), intent(in) :: b
    type(vector3) :: x
    x = vector3( solve(a%data, b%data) )
    end function
    pure function solve_mat3_mat3(a, b) result(x) bind(c)
    !dec$ attributes dllexport :: solve_mat3_mat3
    type(matrix3), intent(in) :: a, b
    type(matrix3) :: x
    x = matrix3( solve(a%data, b%data) )
    end function
    pure function inner_vec3_vec3(a, b) result(s) bind(c)
    !dec$ attributes dllexport :: inner_vec3_vec3
    type(vector3), intent(in) :: a, b
    real(real64) :: s
    s = dot_product(a%data, b%data)
    end function
    pure function outer_vec3_vec3(a, b) result(r) bind(c)
    !dec$ attributes dllexport :: outer_vec3_vec3
    type(vector3), intent(in) :: a, b
    type(matrix3) :: r
    integer :: i, j
    forall (i=1:3)
        forall(j=1:3) r%data(i,j) = a%data(i)*b%data(j)
    end forall
    end function
    pure function cross_vec3_vec3(a, b) result(c) bind(c)
    !dec$ attributes dllexport :: cross_vec3_vec3
    type(vector3), intent(in) ::a, b
    type(vector3) :: c
    c%data = [ &
        a%data(2)*b%data(3)-a%data(3)*b%data(2), &
        a%data(3)*b%data(1)-a%data(1)*b%data(3), &
        a%data(1)*b%data(2)-a%data(2)*b%data(1) ]
    end function
    pure function cross_vec3_op(a) result(c) bind(c)
    !dec$ attributes dllexport :: cross_vec3_op
    type(vector3), intent(in) ::a
    type(matrix3) :: c
    real(real64) :: x,y,z

    !tex:Constructs the 3×3 skew symmetric cross product
    ! operator from a vector $\boldsymbol{v}=\pmatrix{x&y&z}$
    ! $$ \mathrm{cr}(\boldsymbol{v})=\pmatrix{0 & -z & y \\ z & 0 & -x \\ -y & x & 0}$$

    x = a%data(1)
    y = a%data(2)
    z = a%data(3)
    c%data = reshape( [0._real64,  z, -y, &
        -z, 0._real64,  x, &
        y, -x, 0._real64], [3,3])
    end function
    
    function vec3_angle(a,b) result(t) bind(c)
    !dec$ attributes dllexport :: vec3_angle
    type( vector3 ), intent(in) :: a, b
    reaL(real64) :: t, ma, mb, ab
    
        ! |a.b| = |a| |b| cos(t)
        ! |a×b| = |a| |b| sin(t)
        ma = norm2(a%data)
        mb = norm2(b%data)
    
        if( ma == 0._real64 .or. mb == 0._real64 ) then
            t = 0._real64
            return
        end if
    
        ab = dot_product(a%data, b%data)
        t = acos(ab/(ma*mb))
        
    end function
    
    ! 
    pure function mat3_rotate_vec3(R, v) result(w) bind(c)
    !dec$ attributes dllexport :: mat3_rotate_vec3
    ! rotates a diagonal matrix d into the inertial frame using
    ! the congruent transformation A=trans(R)*D*R, where R is the 
    ! rotation matrix.
    !
    type( matrix3 ), intent(in) :: R
    type( vector3 ), intent(in) :: v
    type( vector3 ) :: w
    
        w%data = matmul( R%data, v%data )
    
    return
    end 
    
    pure function mat3_rotate_diag(R, d) result(a) bind(c)
    !dec$ attributes dllexport :: mat3_rotate_diag
    ! rotates a diagonal matrix d into the inertial frame using
    ! the congruent transformation A=trans(R)*D*R, where R is the 
    ! rotation matrix.
    !
    type( matrix3 ), intent(in) :: R
    real( real64  ), intent(in) :: d(3)
    type( matrix3 ) :: a
    integer :: i
    forall(i=1:3)
        a%data(i,:) = d(i) * R%data(:,i)
    end forall
    a%data = matmul(R%data, a%data)
    
    return
    end 
    
    pure subroutine call_vec3_to_array(a,c) bind(c)
    !dec$ attributes dllexport :: call_vec3_to_array
    type(vector3), intent(in) ::a
    real( real64 ), intent(out) :: c(3)
        c = a%data
    end 
    pure subroutine call_mat3_to_array(a,c) bind(c)
    !dec$ attributes dllexport :: call_mat3_to_array
    type(matrix3), intent(in) ::a
    real( real64 ), intent(out) :: c(3,3)
        c = a%data
    end     
    
    end module
    
