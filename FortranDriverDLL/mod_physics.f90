module mod_physics
use mod_array_inv
use mod_nasa_quat

    type, bind(c) :: vector2
        real(real64) :: data(2)
    end type
    type, bind(c) :: vector3
        real(real64) :: data(3)
    end type
    type, bind(c) :: matrix2
        real(real64) :: data(2,2)
    end type
    type, bind(c) :: matrix3
        real(real64) :: data(3,3)
    end type
    
    type(vector2), parameter :: o2_ = vector2( [0._real64, 0._real64] )
    type(vector2), parameter :: i2_ = vector2( [1._real64, 0._real64] )
    type(vector2), parameter :: j2_ = vector2( [0._real64, 1._real64] )
    
    type(vector3), parameter :: o3_ = vector3( [0._real64, 0._real64, 0._real64] )
    type(vector3), parameter :: i3_ = vector3( [1._real64, 0._real64, 0._real64] )
    type(vector3), parameter :: j3_ = vector3( [0._real64, 1._real64, 0._real64] )
    type(vector3), parameter :: k3_ = vector3( [0._real64, 0._real64, 1._real64] )
    
    type(matrix2), parameter :: zero2_ = matrix2( reshape( &
        [0._real64, 0._real64, &
         0._real64, 0._real64], [2,2] ) )
    
    type(matrix2), parameter :: eye2_ = matrix2( reshape( &
        [1._real64, 0._real64, &
         0._real64, 1._real64], [2,2] ) )
    
    type(matrix3), parameter :: zero3_ = matrix3( reshape( &
        [0._real64, 0._real64, 0._real64, &
         0._real64, 0._real64, 0._real64, &
         0._real64, 0._real64, 0._real64], [3,3] ) )
    
    type(matrix3), parameter :: eye3_ = matrix3( reshape( &
        [1._real64, 0._real64, 0._real64, &
         0._real64, 1._real64, 0._real64, &
         0._real64, 0._real64, 1._real64], [3,3] ) )
    
    interface vector2
        module procedure :: vec2_zero
    end interface
    interface matrix2
        module procedure :: mat2_zero
    end interface
    interface vector3
        module procedure :: vec3_zero
    end interface
    interface matrix3
        module procedure :: mat3_zero
    end interface    
    
    interface operator (+)
        ! vector2 operators
        module procedure add_vec2_vec2
        module procedure add_mat2_mat2
        module procedure add_scalar_mat2, add_mat2_scalar
        
        ! vector3 operators
        module procedure add_vec3_vec3
        module procedure add_mat3_mat3
        module procedure add_scalar_mat3, add_mat3_scalar
    end interface
    
    interface operator (-)
        ! vector2 operators
        module procedure sub_vec2_vec2
        module procedure sub_mat2_mat2
        module procedure sub_scalar_mat2, sub_mat2_scalar
        
        ! vector3 operators
        module procedure sub_vec3_vec3
        module procedure sub_mat3_mat3
        module procedure sub_scalar_mat3, sub_mat3_scalar
    end interface
    
    interface operator (*)
        ! vector2 operators
        module procedure mul_vec2_scalar, mul_scalar_vec2
        module procedure mul_mat2_scalar, mul_scalar_mat2
        module procedure mul_vec2_array, mul_array_vec2
        module procedure mul_vec2_mat2, mul_mat2_vec2
        module procedure mul_mat2_mat2
        
        ! vector3 operators
        module procedure mul_vec3_scalar, mul_scalar_vec3
        module procedure mul_mat3_scalar, mul_scalar_mat3
        module procedure mul_vec3_array, mul_array_vec3
        module procedure mul_vec3_mat3, mul_mat3_vec3
        module procedure mul_mat3_mat3
    end interface
    
    interface operator (/)
        module procedure div_vec2_scalar
        module procedure div_vec3_scalar
        module procedure div_mat2_scalar
        module procedure div_mat3_scalar
    end interface

    interface assignment (=)
        module procedure array_to_vec2, vec2_to_array
        module procedure array_to_vec3, vec3_to_array
        module procedure array_to_mat2, mat2_to_array
        module procedure array_to_mat3, mat3_to_array
    end interface
    
    interface operator (.i.)
        module procedure inner_vec2_vec2
        module procedure inner_mat2_mat2
        module procedure inner_vec3_vec3
        module procedure inner_mat3_mat3
    end interface
    interface operator (.x.)
        module procedure cross_scalar_vec2
        module procedure cross_vec2_scalar
        module procedure cross_vec2_vec2
        module procedure cross_vec3_vec3
    end interface
    interface operator (.o.)
        module procedure outer_vec2_vec2
        module procedure outer_vec3_vec3
    end interface
    
    interface cross
        module procedure cross_scalar_vec2
        module procedure cross_vec2_scalar
        module procedure cross_vec2_vec2
        module procedure cross_vec3_vec3
        module procedure cross_vec3_op
    end interface
    
    interface operator (.t.)
        module procedure transpose_mat2, transpose_mat3
        module procedure inner_mat2_mat2, inner_mat3_mat3
    end interface        
    interface transpose
        module procedure transpose_mat2, transpose_mat3
    end interface    
    interface trace
        module procedure trace_mat2, trace_mat3
    end interface    
    interface det
        module procedure :: determinant_mat2, determinant_mat3
    end interface
    interface inv
        module procedure :: inverse_mat2, inverse_mat3
    end interface
    interface solve
        module procedure :: solve_mat2_vec2, solve_mat3_vec3
        module procedure :: solve_mat2_mat2, solve_mat3_mat3
    end interface
    
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
        
    ! vector2
    pure subroutine array_to_vec2(v,a) bind(c)
    !dec$ attributes dllexport :: array_to_vec2
    type(vector2), intent(out) :: v
    real(real64), intent(in) :: a(2)
        v%data = a
    end subroutine
    
    pure subroutine vec2_to_array(a,v) bind(c)
    !dec$ attributes dllexport :: vec2_to_array
    real(real64), intent(out) :: a(2)
    type(vector2), intent(in) :: v
        a = v%data
    end subroutine
    
    pure subroutine array_to_mat2(mx,a) bind(c)
    !dec$ attributes dllexport :: array_to_mat2
    type(matrix2), intent(out) :: mx
    real(real64), intent(in) :: a(2,2)
        mx%data = a
    end subroutine
    
    pure subroutine mat2_to_array(a, mx) bind(c)
    !dec$ attributes dllexport :: mat2_to_array
    real(real64), intent(out) :: a(2,2)
    type(matrix2), intent(in) :: mx
        a = mx%data
    end subroutine
    
    pure function vec2_zero() result(x) bind(c)
    !dec$ attributes dllexport :: vec2_zero
    type(vector2) :: x
        x%data = 0._real64
    end function
    
    pure function mat2_zero() result(x) bind(c)
    !dec$ attributes dllexport :: mat2_zero
    type(matrix2) :: x
        x%data = 0._real64
    end function
    
    pure function add_vec2_vec2(a, b) result(r) bind(c)
    !dec$ attributes dllexport :: add_vec2_vec2
        type(vector2), intent(in), value :: a,b
        type(vector2) :: r
        r%data = a%data + b%data
    end function
    
    pure function sub_vec2_vec2(a, b) result(r) bind(c)
    !dec$ attributes dllexport :: sub_vec2_vec2
        type(vector2), intent(in) :: a,b
        type(vector2) :: r
        r%data = a%data - b%data
    end function
    
    pure function add_mat2_mat2(a, b) result(r) bind(c)
    !dec$ attributes dllexport :: add_mat2_mat2
        type(matrix2), intent(in) :: a,b
        type(matrix2) :: r
        r%data = a%data + b%data
    end function
    
    pure function add_scalar_mat2(s,m) result(u) bind(c)
    !dec$ attributes dllexport :: add_scalar_mat2
    real(real64), intent(in),value :: s
    type(matrix2), intent(in) :: m
    type(matrix2) :: u
        u%data = array_scalar(2, s)  + m%data
    end function
    
    pure function add_mat2_scalar(m,s) result(u) bind(c)
    !dec$ attributes dllexport :: add_mat2_scalar
    type(matrix2), intent(in) :: m
    real(real64), intent(in),value :: s
    type(matrix2) :: u
        u%data = m%data + array_scalar(2, s)
    end function
    
    pure function sub_mat2_mat2(a, b) result(r) bind(c)
    !dec$ attributes dllexport :: sub_mat2_mat2
        type(matrix2), intent(in) :: a,b
        type(matrix2) :: r
        r%data = a%data - b%data
    end function
    
    pure function sub_scalar_mat2(s,m) result(u) bind(c)
    !dec$ attributes dllexport :: sub_scalar_mat2
    real(real64), intent(in),value :: s
    type(matrix2), intent(in) :: m
    type(matrix2) :: u
        u%data = array_scalar(2, s) - m%data
    end function
    
    pure function sub_mat2_scalar(m,s) result(u) bind(c)
    !dec$ attributes dllexport :: sub_mat2_scalar
    type(matrix2), intent(in) :: m
    real(real64), intent(in),value :: s
    type(matrix2) :: u
        u%data = m%data - array_scalar(2, s)
    end function
    
    pure function mul_vec2_scalar(v, s) result(u) bind(c)
    !dec$ attributes dllexport :: mul_vec2_scalar
    type(vector2), intent(in) :: v
    real(real64), intent(in), value :: s
    type(vector2) :: u
        u%data = v%data * s
    end function    
    
    pure function mul_scalar_vec2(s, v) result(u) bind(c)
    !dec$ attributes dllexport :: mul_scalar_vec2
    real(real64), intent(in), value :: s
    type(vector2), intent(in) :: v
    type(vector2) :: u
        u%data = s * v%data
    end function
    
    pure function mul_mat2_scalar(m, s) result(u) bind(c)
    !dec$ attributes dllexport :: mul_mat2_scalar
    type(matrix2), intent(in) :: m
    real(real64), intent(in), value :: s
    type(matrix2) :: u
        u%data = m%data * s
    end function
    
    pure function mul_scalar_mat2(s, m) result(u) bind(c)
    !dec$ attributes dllexport :: mul_scalar_mat2
    real(real64), intent(in), value :: s
    type(matrix2), intent(in) :: m
    type(matrix2) :: u
        u%data = s * m%data
    end function
    
    pure function mul_mat2_mat2(w, m) result(u) bind(c)
    !dec$ attributes dllexport :: mul_mat2_mat2
    type(matrix2), intent(in) :: w
    type(matrix2), intent(in) :: m
    type(matrix2) :: u
        u%data = matmul(w%data, m%data)
    end function
    
    pure function mul_array_vec2(a, v) result(u) bind(c)
    !dec$ attributes dllexport :: mul_array_vec2
    type(vector2), intent(in) :: v
    real(real64), intent(in) :: a(2,2)
    type(vector2) :: u
        u%data = matmul( a, v%data)
    end function
    pure function mul_vec2_array(v, a) result(u) bind(c)
    !dec$ attributes dllexport :: mul_vec2_array
    type(vector2), intent(in) :: v
    real(real64), intent(in) :: a(2,2)
    type(vector2) :: u
        u%data = matmul( v%data, a)
    end function
    pure function mul_mat2_vec2(a, v) result(u) bind(c)
    !dec$ attributes dllexport :: mul_mat2_vec2
    type(matrix2), intent(in) :: a
    type(vector2), intent(in) :: v
    type(vector2) :: u
        u%data = matmul( a%data, v%data)
    end function
    pure function mul_vec2_mat2(v, a) result(u) bind(c)
    !dec$ attributes dllexport :: mul_vec2_mat2
    type(vector2), intent(in) :: v
    type(matrix2), intent(in) :: a
    type(vector2) :: u
        u%data = matmul( v%data, a%data)
    end function
    
    pure function div_vec2_scalar(v, d) result(u) bind(c)
    !dec$ attributes dllexport :: div_vec2_scalar
    type(vector2), intent(in) :: v
    real(real64), intent(in), value :: d
    type(vector2) :: u
        u%data = (1._real64/d) * v%data
    end function
    
    pure function div_mat2_scalar(m, d) result(u) bind(c)
    !dec$ attributes dllexport :: div_mat2_scalar
    type(matrix2), intent(in) :: m
    real(real64), intent(in),value :: d
    type(matrix2) :: u
        u%data = (1._real64/d) * m%data / d
    end function
        
    pure function transpose_mat2(a) result(t) bind(c)
    !dec$ attributes dllexport :: transpose_mat2
    type(matrix2), intent(in) :: a
    type(matrix2) :: t
        t%data = transpose(a%data)
    end function
    
    pure function trace_mat2(a) result(t) bind(c)
    !dec$ attributes dllexport :: trace_mat2
    type(matrix2), intent(in) :: a
    real(real64) :: t
        t = trace(a%data)
    end function
    
    pure function determinant_mat2(a) result(d) bind(c)
    !dec$ attributes dllexport :: determinant_mat2
    type(matrix2), intent(in) :: a
    real(real64) :: d
        d = det(a%data)
    end function   
    
    pure function inner_mat2_mat2(w, m) result(u) bind(c)
    !dec$ attributes dllexport :: inner_mat2_mat2
    type(matrix2), intent(in) :: w
    type(matrix2), intent(in) :: m
    type(matrix2) :: u
        u%data = matmul(transpose(w%data), m%data)
    end function
    
    pure function inverse_mat2(a) result(b) bind(c)
    !dec$ attributes dllexport :: inverse_mat2
    type(matrix2), intent(in) :: a
    type(matrix2) :: b
        b%data = inv(a%data)
    end function
    
    pure function solve_mat2_vec2(a,b) result(x) bind(c)
    !dec$ attributes dllexport :: solve_mat2_vec2
    type(matrix2), intent(in) :: a
    type(vector2), intent(in) :: b
    type(vector2) :: x
        x = vector2( solve(a%data, b%data) )
    end function
    
    pure function solve_mat2_mat2(a,b) result(x) bind(c)
    !dec$ attributes dllexport :: solve_mat2_mat2
    type(matrix2), intent(in) :: a,b
    type(matrix2) :: x
        x = matrix2( solve(a%data, b%data) )
    end function
        
    pure function inner_vec2_vec2(a,b) result(s) bind(c)
    !dec$ attributes dllexport :: inner_vec2_vec2
    type(vector2), intent(in) :: a, b
    real(real64) :: s
        s = dot_product(a%data, b%data)
    end function
    
    pure function outer_vec2_vec2(a,b) result(r) bind(c)
    !dec$ attributes dllexport :: outer_vec2_vec2
    type(vector2), intent(in) :: a, b
    type(matrix2) :: r
    integer :: i, j
        forall (i=1:2)
          forall(j=1:2) r%data(i,j) = a%data(i)*b%data(j)
        end forall        
    end function
    
    pure function cross_vec2_vec2(a,b) result(c) bind(c)
    !dec$ attributes dllexport :: cross_vec2_vec2
    type(vector2), intent(in) ::a
    type(vector2), intent(in) ::b
    real(real64) :: c
        c = a%data(1)*b%data(2)-a%data(2)*b%data(1)
    end function

    pure function cross_vec2_scalar(a,b) result(c) bind(c)
    !dec$ attributes dllexport :: cross_vec2_scalar
    type(vector2), intent(in) ::a
    real(real64), intent(in), value ::b
    type(vector2) :: c                
        c%data = [ &
            a%data(2)*b, &
            -a%data(1)*b ]        
    end function
    
    pure function cross_scalar_vec2(a,b) result(c) bind(c)
    !dec$ attributes dllexport :: cross_scalar_vec2
    real(real64), intent(in), value ::a
    type(vector2), intent(in) ::b
    type(vector2) :: c                
        c%data = [ &
            -a*b%data(2), &
            a*b%data(2) ]        
    end function
    
    ! vector3
    pure subroutine array_to_vec3(v, a) bind(c)
    !dec$ attributes dllexport :: array_to_vec3
    type(vector3), intent(out) :: v
    real(real64), intent(in) :: a(3)
        v%data = a
    end subroutine

    pure subroutine vec3_to_array(a, v) bind(c)
    !dec$ attributes dllexport :: vec3_to_array
    real(real64), intent(out) :: a(3)
    type(vector3), intent(in) :: v
        a = v%data
    end subroutine    
    
    pure subroutine array_to_mat3(mx, a) bind(c)
    !dec$ attributes dllexport :: array_to_mat3
    type(matrix3), intent(out) :: mx
    real(real64), intent(in) :: a(3,3)
        mx%data = a
    end subroutine

    pure subroutine mat3_to_array(a, mx) bind(c)
    !dec$ attributes dllexport :: mat3_to_array
    real(real64), intent(out) :: a(3,3)
    type(matrix3), intent(in) :: mx
        a = mx%data
    end subroutine

    pure function vec3_zero() result(x) bind(c)
    !dec$ attributes dllexport :: vec3_zero
    type(vector3) :: x
        x%data = 0._real64
    end function
    
    pure function mat3_zero() result(x) bind(c)
    !dec$ attributes dllexport :: mat3_zero
    type(matrix3) :: x
        x%data = 0._real64
    end function
    
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
        u%data = array_scalar(3, s)  + m%data
    end function
    
    pure function add_mat3_scalar(m,s) result(u) bind(c)
    !dec$ attributes dllexport :: add_mat3_scalar
    type(matrix3), intent(in) :: m
    real(real64), intent(in), value :: s
    type(matrix3) :: u
        u%data = m%data + array_scalar(3, s)
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
        u%data = array_scalar(3, s) - m%data
    end function
    
    pure function sub_mat3_scalar(m, s) result(u) bind(c)
    !dec$ attributes dllexport :: sub_mat3_scalar
    type(matrix3), intent(in) :: m
    real(real64), intent(in), value :: s
    type(matrix3) :: u
        u%data = m%data - array_scalar(3, s)
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
    
    pure function mul_array_vec3(a, v) result(u) bind(c)
    !dec$ attributes dllexport :: mul_array_vec3
    type(vector3), intent(in) :: v
    real(real64), intent(in) :: a(3,3)
    type(vector3) :: u
        u%data = matmul( a, v%data)
    end function
    
    pure function mul_vec3_array(v, a) result(u) bind(c)
    !dec$ attributes dllexport :: mul_vec3_array
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
    real(real64), parameter :: O = 0
    
        !tex:Constructs the 3×3 skew symmetric cross product
        ! operator from a vector $\boldsymbol{v}=\pmatrix{x&y&z}$
        ! $$ \mathrm{cr}(\boldsymbol{v})=\pmatrix{0 & -z & y \\ z & 0 & -x \\ -y & x & 0}$$    
    
        x = a%data(1)
        y = a%data(2)
        z = a%data(3)
        c%data = reshape([O,z,-y, -z,O,x, y,-x,O], [3,3])    
    end function
    
    
    ! rigid bodies
    pure subroutine rotate_quat_diag2mat(q, d, a) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: rotate_quat_diag2mat
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
    !    Input, real ( real64 ) Q(4), the quaternion representing the rotate.
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
        call rotate_quat2mat_inv(q, r, .false.)        
        call rotate_rot_diag2mat(r, d, a)                
    end subroutine

    pure subroutine rotate_rot_diag2mat(R, d, a) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: rotate_rot_diag2mat
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
    !    Input, real ( real64 ) R(3,3), the rotate matrix.
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

    call rotate_quat2mat_inv(q, r, .true.)

    m = rb%mass
    d = [ rb%Ixx, rb%Iyy, rb%Izz ]
    
    call rotate_quat2mat_inv(q, r, .false.)
    call rotate_rot_diag2mat(r, d, I)
    
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
    
    call rotate_quat2mat_inv(q, r, .false.)    

    m = rb%mass
    d = [ rb%Ixx, rb%Iyy, rb%Izz ]

    call rotate_rot_diag2mat(r, 1/d, I_inv)
    
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
    call quat_derivative(q, omg, qp)
    ! Get applied forces and torques (external)
    call f(t, pos, q, vee, omg, frc, tau)

    ! Derivative of state vector 
    yp = [ vee, qp, frc, tau ]

    end subroutine
    

end module