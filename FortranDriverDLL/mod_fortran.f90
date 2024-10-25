    module mod_fortran
    use, intrinsic :: iso_fortran_env
    !use, intrinsic :: iso_c_binding
    implicit none

    integer ( int32 ), parameter :: i4_huge = 2147483647

    enum, bind(c)
        enumerator :: by_row = 0
        enumerator :: by_column = 1
    end enum


    abstract interface

    pure subroutine f_progress(i, n)
    import
    !DEC$ ATTRIBUTES VALUE :: i, n
    integer, intent(in) :: i, n
    end subroutine

    end interface

    contains

    subroutine call_test_dowork(n, m, A, progressCallBack) !bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_test_dowork
    !DEC$ ATTRIBUTES ALIAS: 'call_test_dowork' :: call_test_dowork
    !DEC$ ATTRIBUTES VALUE :: n, m
    procedure(f_progress) :: progressCallBack
    integer, intent(in) :: n, m
    real(real64), intent(inout) :: A(n, m)
    real(real64) :: B(n, m)

    integer :: j

    call RANDOM_SEED()

    call RANDOM_NUMBER(B)

    do j = 1, m
        A(:, j) = j*A(:, j) + B(:,j)
        call progressCallBack(j, m)
    end do

    return

    end subroutine
    

    ! *** VECTOR OPERATIONS ***
    pure subroutine call_array_zeros_v(n,A) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_array_zeros_v
    integer, intent(in), value :: n
    real(real64), intent(out) :: A(n)
        A = 0.0_real64
    end subroutine

    subroutine call_uniform_array_v( n, seed, r ) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_uniform_array_v
    !*****************************************************************************80
    !
    !! R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
    !
    !  Discussion:
    !
    !    An R8VEC is a vector of R8's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    13 August 2014
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Reference:
    !
    !    Paul Bratley, Bennett Fox, Linus Schrage,
    !    A Guide to Simulation,
    !    Springer Verlag, pages 201-202, 1983.
    !
    !    Bennett Fox,
    !    Algorithm 647:
    !    Implementation and Relative Efficiency of Quasirandom
    !    Sequence Generators,
    !    ACM Transactions on Mathematical Software,
    !    Volume 12, Number 4, pages 362-376, 1986.
    !
    !    Peter Lewis, Allen Goodman, James Miller
    !    A Pseudo-Random Number Generator for the System/360,
    !    IBM Systems Journal,
    !    Volume 8, pages 136-143, 1969.
    !
    !  Parameters:
    !
    !    Input, integer ( int32 ) N, the number of entries in the vector.
    !
    !    Input/output, integer ( int32 ) SEED, the "seed" value, which
    !    should NOT be 0.  On output, SEED has been updated.
    !
    !    Output, real ( real64 ) R(N), the vector of pseudorandom values.
    !
    implicit none
    integer ( int32 ), intent(in), value :: n
    integer ( int32 ), intent(inout) :: seed
    real ( real64 ), intent(out) :: r(n)

    integer ( int32 ) i
    !integer ( int32 ), parameter :: i4_huge = 2147483647
    integer ( int32 ) k

    if ( seed == 0 ) then
        call SYSTEM_CLOCK(seed)
    end if

    do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed < 0 ) then
            seed = seed + i4_huge
        end if

        r(i) = real ( seed, real64 ) * 4.656612875D-10

    end do

    return
    end

    subroutine call_uniform_array_m( n, m, seed, r ) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_uniform_array_m
    implicit none
    integer ( int32 ), intent(in), value :: n, m
    integer ( int32 ), intent(inout) :: seed
    real ( real64 ), intent(out) :: r(n,m)
    real ( real64 ) :: temp(n*m)

    call call_uniform_array_v(n*m, seed, temp )
    r = reshape( temp, [n,m] )

    end subroutine

    subroutine call_random_array_v(n,x,y,A) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_random_array_v
    integer, intent(in),value :: n
    real(real64), intent(in),value :: x, y
    real(real64), intent(out) :: A(n)

    call RANDOM_NUMBER(A)
    A = x + (y-x) * A

    end subroutine

    pure subroutine call_elem_array_v(n,i,x,A) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_elem_array_v
    integer, intent(in), value :: n, i
    real(real64), intent(in), value :: x
    real(real64), intent(out) :: A(n)

    A = 0.0_real64
    A(i) = x

    end subroutine

    pure subroutine call_add_array_v(n,x,y,z) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_add_array_v
    integer, intent(in), value :: n
    real(real64), intent(in) :: x(n), y(n)
    real(real64), intent(out) :: z(n)

    z = x + y

    end subroutine

    pure subroutine call_sub_array_v(n,x,y,z) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_sub_array_v
    integer, intent(in), value :: n
    real(real64), intent(in) :: x(n), y(n)
    real(real64), intent(out) :: z(n)

    z = x - y

    end subroutine

    pure subroutine call_scale_array_v(n,f,x,r) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_scale_array_v
    integer, intent(in), value :: n
    real(real64), intent(in), value :: f
    real(real64), intent(in) :: x(n)
    real(real64), intent(out) :: r(n)

    r = f * x

    end subroutine

    pure subroutine call_round_array_v(n,x,d,r) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_round_array_v
    integer, intent(in), value :: n
    real(real64), intent(in) :: x(n)
    integer, intent(in),value :: d
    real(real64), intent(out) :: r(n)
    real(real64) :: f,a

    a = min( 15, max(1,d))
    f = 10._real64 ** a
    r = floor( x * f + 0.5_real64)/f

    end subroutine

    pure subroutine call_round_array_m(n,m,x,d,r) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_round_array_m
    integer, intent(in), value :: n, m
    real(real64), intent(in) :: x(n, m)
    integer, intent(in),value :: d
    real(real64), intent(out) :: r(n,m)
    real(real64) :: f,a

    a = min( 15, max(1,d))
    f = 10._real64 ** a
    r = floor( x * f + 0.5_real64)/f

    end subroutine

    pure subroutine call_inner_array_v(n,x,y,z) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_inner_array_v
    integer, intent(in), value :: n
    real(real64), intent(in) :: x(n), y(n)
    real(real64), intent(out) :: z
    z = dot_product(x, y)
    end subroutine
    
    pure subroutine call_inner_array_m(n,m,k,x,y,z) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_inner_array_m
    integer, intent(in), value :: n, m, k
    real(real64), intent(in) :: x(n,m), y(n,k)
    real(real64), intent(out) :: z(m,k)
    z = matmul(transpose(x), y)
    end subroutine
    
    pure subroutine call_outer_array_v(n,m,x,y,A) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_outer_array_v
    use mod_array_inv, only : outer_array_v
    integer, intent(in), value :: n,m
    real(real64), intent(in) :: x(n), y(m)
    real(real64), intent(out) :: A(n,m)
        A = outer_array_v(x, y)
    end subroutine

    pure subroutine call_mul_array_mv(n,m,A,x,b) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_mul_array_mv
    integer, intent(in), value :: n, m
    real(real64), intent(in) :: A(n,m), x(m)
    real(real64), intent(out) :: b(n)
    b = matmul(A, x)
    end subroutine

    pure subroutine call_mul_array_vm(n,m,x,A,b) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_mul_array_vm
    integer, intent(in), value :: n, m
    real(real64), intent(in) :: A(n,m), x(m)
    real(real64), intent(out) :: b(n)
    b = matmul(x, A)
    end subroutine
    
    pure subroutine call_mul_array_mm(n,m,k,A,x,b) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_mul_array_mm
    integer, intent(in), value :: n, m, k
    real(real64), intent(in) :: A(n,m), x(m,k)
    real(real64), intent(out) :: b(n,k)

    b = matmul(A, x)

    end subroutine

    pure subroutine call_solve_array_mv(n,A,b,x) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_solve_array_mv
    use mod_array_inv
    integer, intent(in), value :: n
    real(real64), intent(in) :: A(n,n), b(n)
    real(real64), intent(out) :: x(n)

    x = solve_array_mv(A, b)

    end subroutine


    ! *** MATRIX OPERATIONS ***
    
    ! Set a column of a matrix from a vector
    !   n: the size of the vector (#rows)   [I]     integer, value
    !   m: the matrix column count          [I]     integer, value
    !   vector: An array of data to use     [I]     real(n)
    !   j : the column index to use         [I]     integer, value
    !   A: the resulting matrix             [I/O]   real(n,m)
    pure subroutine call_array_set_column(n, m, vector, j, A) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_array_set_column
    integer, intent(in), value :: n, m, j
    real(real64), intent(in) :: vector(n)
    real(real64), intent(inout) :: A(n, m)
        A(:, j) = vector
    end subroutine
    
    ! Set a row of a matrix from a vector
    !   n: the matrix row count             [I]     integer, value
    !   m: the size of the vector (#rows)   [I]     integer, value
    !   vector: An array of data to use     [I]     real(n)
    !   i : the row index to use            [I]     integer, value
    !   A: the resulting matrix             [I/O]   real(n,m)
    pure subroutine call_array_set_row(n, m, vector, i, A) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_array_set_row
    integer, intent(in), value :: n, m, i
    real(real64), intent(in) :: vector(m)
    real(real64), intent(inout) :: A(n, m)
        A(i, :) = vector
    end subroutine

    pure subroutine call_fill_array_m(n,m,values,order,A) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_fill_array_m
    integer, intent(in), value :: n, m, order
    real(real64), intent(in) :: values(n*m)
    real(real64), intent(out) :: A(n, m)
    if( order == by_row ) then
        A = reshape( values, [n,m], order = [2,1])
    else
        A = reshape( values, [n,m] )
    end if
    end subroutine

    subroutine call_random_array_m(n,m,x,y,A) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_random_array_m
    integer, intent(in), value :: n, m
    real(real64), intent(in), value :: x, y
    real(real64), intent(out) :: A(n, m)

    call RANDOM_NUMBER(A)
    A = x + (y-x) * A

    end subroutine
!
    pure subroutine call_array_zeros_m(n,m,A) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_array_zeros_m
    integer, intent(in), value :: n, m
    real(real64), intent(out) :: A(n,m)
        A = 0.0_real64
    end subroutine
    pure subroutine call_array_diag_m(n,x,A) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_array_diag_m
    integer, intent(in), value :: n
    real(real64), intent(in) :: x(n)
    real(real64), intent(out) :: A(n,n)
    integer :: i

    A = 0.0_real64
    forall(i=1:n)
        A(i,i) = x(i)
    end forall

    end subroutine

    pure subroutine call_array_scalar_m(n,m,x,A) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_array_scalar_m
    integer, intent(in), value :: n, m
    real(real64), intent(in), value :: x
    real(real64), intent(out) :: A(n,m)
    integer :: i,k
    k = min(n,m)
    A = 0.0_real64
    forall(i=1:k)
        A(i,i) = x
    end forall
    end subroutine

    pure subroutine call_add_array_m(n,m,x,y,z) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_add_array_m
    integer, intent(in), value :: n, m
    real(real64), intent(in) :: x(n,m), y(n,m)
    real(real64), intent(out) :: z(n,m)

    z = x + y

    end subroutine

    pure subroutine call_sub_array_m(n,m,x,y,z) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_sub_array_m
    integer, intent(in), value :: n, m
    real(real64), intent(in) :: x(n,m), y(n,m)
    real(real64), intent(out) :: z(n,m)

    z = x - y

    end subroutine

    pure subroutine call_scale_array_m(n,m,s,x,z) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_scale_array_m
    integer, intent(in), value :: n, m
    real(real64), intent(in), value :: s
    real(real64), intent(in) :: x(n,m)
    real(real64), intent(out) :: z(n,m)

    z = s * x

    end subroutine

    pure subroutine call_transpose_array_m(n,m,A,At) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_transpose_array_m
    integer, intent(in), value :: n, m
    real(real64), intent(in) :: A(n,m)
    real(real64), intent(out) :: At(m,n)

    At = transpose(A)

    end subroutine

    pure subroutine call_reshape_array_mv(n,m,A,k,V) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_reshape_array_mv
    integer, intent(in), value :: n, m, k
    real(real64), intent(in) :: A(n,m)
    real(real64), intent(out) :: V(k)

    V = reshape(A, [k])

    end subroutine

    pure subroutine call_reshape_array_vm(n,V,k,l,order,A) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_reshape_array_vm
    integer, intent(in), value :: n, k, l
    real(real64), intent(in) :: V(n)
    integer, intent(in), value :: order
    real(real64), intent(out) :: A(k,l)

    if( order == by_row ) then
        A = reshape(V, [k,l], order=[2,1])
    else
        A = reshape(V, [k,l])
    end if
    end subroutine

    pure subroutine call_reshape_array_mm(n,m,A,k,l,B) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_reshape_array_mm
    integer, intent(in), value :: n, m, k, l
    real(real64), intent(in) :: A(n,m)
    real(real64), intent(out) :: B(k,l)

    B = reshape(A, [k,l])

    end subroutine

    pure subroutine call_slice_array_v(n,A,i1,i2,B) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_slice_array_v
    integer, intent(in), value :: n, i1,i2
    real(real64), intent(in) :: A(n)
    real(real64), intent(out) :: B(i2-i1+1)
        B = A(i1:i2)
    end subroutine
    
    pure subroutine call_slice_array_m(n,m,A,i1,i2,j1,j2,B) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_slice_array_m
    integer, intent(in), value :: n, m, i1,i2,j1,j2
    real(real64), intent(in) :: A(n,m)
    real(real64), intent(out) :: B(i2-i1+1,j2-j1+1)
        B = A(i1:i2, j1:j2)
    end subroutine
    
    pure subroutine call_slice_rows_array_m(n,m,A,i,b) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_slice_rows_array_m
    integer, intent(in), value :: n, m, i
    real(real64), intent(in) :: A(n,m)
    real(real64), intent(out) :: b(m)
        b = A(i, :)
    end subroutine
    pure subroutine call_slice_cols_array_m(n,m,A,j,b) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_slice_cols_array_m
    integer, intent(in), value :: n, m, j
    real(real64), intent(in) :: A(n,m)
    real(real64), intent(out) :: b(n)
        b = A(:, j)
    end subroutine

    pure subroutine call_slice_row_array_m(n,m,A,i,j1,j2,b) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_slice_row_array_m
    integer, intent(in), value :: n, m, i,j1,j2
    real(real64), intent(in) :: A(n,m)
    real(real64), intent(out) :: b(j2-j1+1)
        b = A(i, j1:j2)
    end subroutine
    pure subroutine call_slice_col_array_m(n,m,A,i1,i2,j,b) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_slice_col_array_m
    integer, intent(in), value :: n, m, i1,i2,j
    real(real64), intent(in) :: A(n,m)
    real(real64), intent(out) :: b(i2-i1+1)
        b = A(i1:i2, j)
    end subroutine

    pure subroutine call_solve_array_mm(n,k,A,b,x) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_solve_array_mm
    use mod_array_inv
    integer, intent(in), value :: n,k
    real(real64), intent(in) :: A(n,n), b(n,k)
    real(real64), intent(out) :: x(n,k)

    x = solve_array_mm(A, b)

    end subroutine

    pure subroutine call_determinant_array_m(n,A,d) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_determinant_array_m
    use mod_array_inv
    integer, intent(in), value :: n
    real(real64), intent(in) :: A(n,n)
    real(real64), intent(out) :: d

    d = determinant_array_m(A)

    end subroutine

    pure subroutine call_inverse_array_m(n,A,A_inv) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_inverse_array_m
    use mod_array_inv
    integer, intent(in), value :: n
    real(real64), intent(in) :: A(n,n)
    real(real64), intent(out) :: A_inv(n,n)

    A_inv = inverse_array_m(A)

    end subroutine
    
    end module

