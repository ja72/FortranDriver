    module mod_physics_planar
    use mod_array_inv
    type, bind(c) :: vector2
        real(real64) :: data(2)
    end type
    type, bind(c) :: matrix2
        real(real64) :: data(2,2)
    end type

    interface vector2
    module procedure :: vec2_zeros, vec2_values
    end interface
    interface matrix2
    module procedure :: mat2_zeros, mat2_values
    end interface

    type(vector2), parameter :: o2_ = vector2( [0._real64, 0._real64] )
    type(vector2), parameter :: i2_ = vector2( [1._real64, 0._real64] )
    type(vector2), parameter :: j2_ = vector2( [0._real64, 1._real64] )

    type(matrix2), parameter :: zero2_ = matrix2( reshape( &
        [0._real64, 0._real64, &
        0._real64, 0._real64], [2,2] ) )

    type(matrix2), parameter :: eye2_ = matrix2( reshape( &
        [1._real64, 0._real64, &
        0._real64, 1._real64], [2,2] ) )

    interface operator (+)
    module procedure add_vec2_vec2
    module procedure add_mat2_mat2
    module procedure add_scalar_mat2, add_mat2_scalar
    end interface
    interface operator (-)
    module procedure sub_vec2_vec2
    module procedure sub_mat2_mat2
    module procedure sub_scalar_mat2, sub_mat2_scalar
    end interface
    interface operator (*)
    module procedure mul_vec2_scalar, mul_scalar_vec2
    module procedure mul_mat2_scalar, mul_scalar_mat2
    module procedure mul_vec2_array, mul_array_vec2
    module procedure mul_vec2_mat2, mul_mat2_vec2
    module procedure mul_mat2_mat2
    end interface
    interface operator (/)
    module procedure div_vec2_scalar
    module procedure div_mat2_scalar
    end interface
    interface assignment (=)
    module procedure array_to_vec2, vec2_to_array
    module procedure array_to_mat2, mat2_to_array
    end interface
    interface operator (.i.)
    module procedure inner_vec2_vec2
    module procedure inner_mat2_mat2
    end interface
    interface operator (.x.)
    module procedure cross_scalar_vec2
    module procedure cross_vec2_scalar
    module procedure cross_vec2_vec2
    end interface
    interface operator (.o.)
    module procedure outer_vec2_vec2
    end interface
    interface cross
    module procedure cross_scalar_vec2
    module procedure cross_vec2_scalar
    module procedure cross_vec2_vec2
    end interface
    interface operator (.t.)
    module procedure transpose_mat2
    module procedure inner_mat2_mat2
    end interface
    interface transpose
    module procedure transpose_mat2
    end interface
    interface trace
    module procedure trace_mat2
    end interface
    interface det
    module procedure :: determinant_mat2
    end interface
    interface inv
    module procedure :: inverse_mat2
    end interface
    interface solve
    module procedure :: solve_mat2_vec2, solve_mat2_mat2
    end interface

    contains
    ! vector2
    pure function vec2_zeros() result(x) bind(c)
    !dec$ attributes dllexport :: vec2_zeros
    type(vector2) :: x
    x%data = 0._real64
    end function
    pure function vec2_ux() result(x) bind(c)
    !dec$ attributes dllexport :: vec2_ux
    type(vector2) :: x
    x%data(1) = 1._real64
    x%data(2) = 0._real64
    end function
    pure function vec2_uy() result(x) bind(c)
    !dec$ attributes dllexport :: vec2_uy
    type(vector2) :: x
    x%data(1) = 0._real64
    x%data(2) = 1._real64
    end function
    pure function vec2_ones() result(x) bind(c)
    !dec$ attributes dllexport :: vec2_ones
    type(vector2) :: x
    x%data = 1._real64
    end function
    pure function mat2_zeros() result(x) bind(c)
    !dec$ attributes dllexport :: mat2_zeros
    type(matrix2) :: x
    x%data = 0._real64
    end function
    pure function mat2_eye() result(x) bind(c)
    !dec$ attributes dllexport :: mat2_eye
    type(matrix2) :: x
    integer :: i
    x%data = 0._real64
    forall(i=1:2)
        x%data(i,i) = 1._real64
    end forall
    end function
    pure function mat2_ones() result(x) bind(c)
    !dec$ attributes dllexport :: mat2_ones
    type(matrix2) :: x
    x%data = 1._real64
    end function
    pure function vec2_values(x,y) bind(c) result(v)
    !dec$ attributes dllexport :: vec2_values
    type(vector2) :: v
    real(real64), intent(in), value :: x,y
    v%data = [x, y]
    end function
    pure function mat2_values(a11,a12,a21,a22) bind(c) result(v)
    !dec$ attributes dllexport :: mat2_values
    type(matrix2) :: v
    real(real64), intent(in), value :: a11,a12,a21,a22
    v%data = reshape( [a11,a21,a12,a22], [2,2] )
    end function
    pure subroutine array_to_vec2(v,a) bind(c)
    !!dec$ attributes dllexport :: array_to_vec2
    type(vector2), intent(out) :: v
    real(real64), intent(in) :: a(2)
    v%data = a
    end subroutine
    pure subroutine vec2_to_array(a,v) bind(c)
    !!dec$ attributes dllexport :: vec2_to_array
    real(real64), intent(out) :: a(2)
    type(vector2), intent(in) :: v
    a = v%data
    end subroutine
    pure subroutine array_to_mat2(mx,a) bind(c)
    !!dec$ attributes dllexport :: array_to_mat2
    type(matrix2), intent(out) :: mx
    real(real64), intent(in) :: a(2,2)
    mx%data = a
    end subroutine
    pure subroutine mat2_to_array(a, mx) bind(c)
    !!dec$ attributes dllexport :: mat2_to_array
    real(real64), intent(out) :: a(2,2)
    type(matrix2), intent(in) :: mx
    a = mx%data
    end subroutine
    function vec2_uniform(seed) result(x) bind(c)
    !dec$ attributes dllexport :: vec2_uniform
    use mod_fortran
    type(vector2) :: x
    integer ( int32 ), intent(inout) :: seed
    real(real64) :: s
    call call_uniform_array_v(2, seed, x%data)
    s = norm2(x%data)
    x%data = x%data/s
    end function
    function mat2_uniform(seed) result(x) bind(c)
    !dec$ attributes dllexport :: mat2_uniform
    use mod_fortran
    type(matrix2) :: x
    integer ( int32 ), intent(inout) :: seed
    real(real64) :: s
    call call_uniform_array_m(2, 2, seed, x%data)
    s = norm2(x%data)
    x%data = x%data/s
    end function
    pure function norm_vec2(q) bind(c) result(s)
    !DEC$ ATTRIBUTES DLLEXPORT :: norm_vec2
    use mod_array_inv, only : norm_array_v
    type(vector2), intent(in) :: q
    real(real64) :: s
    s = norm_array_v(2, q%data)
    return
    end
    pure function add_vec2_vec2(a, b) result(r) bind(c)
    !dec$ attributes dllexport :: add_vec2_vec2
    type(vector2), intent(in) :: a,b
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
    integer :: i
        u%data = m%data
        forall (i=1:2)
            u%data(i,i) = s + u%data(i,i)
        end forall
    end function
    pure function add_mat2_scalar(m,s) result(u) bind(c)
    !dec$ attributes dllexport :: add_mat2_scalar
    type(matrix2), intent(in) :: m
    real(real64), intent(in),value :: s
    type(matrix2) :: u
    integer :: i
        u%data = m%data
        forall (i=1:2)
            u%data(i,i) = u%data(i,i) + s
        end forall
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
    integer :: i
        u%data = -m%data
        forall (i=1:2)
            u%data(i,i) = s + u%data(i,i)
        end forall
    end function
    pure function sub_mat2_scalar(m,s) result(u) bind(c)
    !dec$ attributes dllexport :: sub_mat2_scalar
    type(matrix2), intent(in) :: m
    real(real64), intent(in),value :: s
    type(matrix2) :: u
    integer :: i
        u%data = m%data
        forall (i=1:2)
            u%data(i,i) = u%data(i,i)-s
        end forall
    end function
    pure function neg_vec2(v) result(u) bind(c)
    !dec$ attributes dllexport :: neg_vec2
    type(vector2), intent(in) :: v
    type(vector2) :: u
    u%data = - v%data
    end function
    pure function neg_mat2(v) result(u) bind(c)
    !dec$ attributes dllexport :: neg_mat2
    type(matrix2), intent(in) :: v
    type(matrix2) :: u
    u%data = - v%data
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
    pure function mul_array_vec2(a, v) result(u)
    type(vector2), intent(in) :: v
    real(real64), intent(in) :: a(2,2)
    type(vector2) :: u
    u%data = matmul( a, v%data)
    end function
    pure function mul_vec2_array(v, a) result(u)
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

    pure subroutine call_vec2_to_array(a,c) bind(c)
    !dec$ attributes dllexport :: call_vec2_to_array
    type(vector2), intent(in) ::a
    real( real64 ), intent(out) :: c(2)
        c = a%data
    end 
    pure subroutine call_mat2_to_array(a,c) bind(c)
    !dec$ attributes dllexport :: call_mat2_to_array
    type(matrix2), intent(in) ::a
    real( real64 ), intent(out) :: c(2,2)
        c = a%data
    end     
    
    end module
