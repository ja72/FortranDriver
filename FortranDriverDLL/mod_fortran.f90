    module mod_fortran
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    implicit none
    
    integer ( int32 ), parameter :: i4_huge = 2147483647    

    abstract interface

    pure subroutine actionrefint(i, n)
    import
    !DEC$ ATTRIBUTES VALUE :: i, n
    integer, intent(in) :: i, n
    end subroutine
    end interface
    
    
    contains
    
    

    subroutine DoWork(n,m, A, progressCallBack)
    !DEC$ ATTRIBUTES DLLEXPORT :: DoWork
    !DEC$ ATTRIBUTES ALIAS: 'DoWork' :: DoWork
    !DEC$ ATTRIBUTES VALUE :: n, m
    !DEC$ ATTRIBUTES REFERENCE :: A, progressCallBack
    
    procedure(actionrefint) :: progressCallBack
    integer, intent(in) :: n, m
    !real(real64), dimension(n, n), intent(inout) :: A(:,:)
    real(real64), intent(inout) :: A(n, m)
    real(real64) :: B(n, m)
    
    integer :: j
    
    call RANDOM_SEED()

    call RANDOM_NUMBER(B)
    
    do j = 1, m
        call progressCallBack(j, m)
        A(:, j) = j*A(:, j) + B(:,j)
    end do

    return

    end subroutine
    
! *** VECTOR OPERATIONS ***
    
    subroutine array_uniform_v( n, seed, r ) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_uniform_v
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
    
    
    subroutine array_rand_v(n,x,y,A)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_rand_v
    !DEC$ ATTRIBUTES ALIAS: 'array_rand_v' :: array_rand_v
    !DEC$ ATTRIBUTES VALUE :: n, x, y
    !DEC$ ATTRIBUTES REFERENCE :: A
    integer, intent(in) :: n
    real(real64), intent(in) :: x, y
    real(real64), intent(out) :: A(n)

        call RANDOM_NUMBER(A)        
        A = x + (y-x) * A
    
    end subroutine
        
    pure subroutine array_elem_v(n,i,x,A)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_elem_v
    !DEC$ ATTRIBUTES ALIAS: 'array_elem_v' :: array_elem_v
    !DEC$ ATTRIBUTES VALUE :: n, i, x
    !DEC$ ATTRIBUTES REFERENCE :: A
    integer, intent(in) :: n,i
    real(real64), intent(in) :: x
    real(real64), intent(out) :: A(n)
    
        A = 0.0_real64
        A(i) = x
    
    end subroutine
    
    pure subroutine array_add_v(n,x,y,z)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_add_v
    !DEC$ ATTRIBUTES ALIAS: 'array_add_v' :: array_add_v
    !DEC$ ATTRIBUTES VALUE :: n
    !DEC$ ATTRIBUTES REFERENCE :: x,y,z
    integer, intent(in) :: n
    real(real64), intent(in) :: x(n), y(n)
    real(real64), intent(out) :: z(n)
    
        z = x + y
    
    end subroutine
    
    pure subroutine array_subtract_v(n,x,y,z)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_subtract_v
    !DEC$ ATTRIBUTES ALIAS: 'array_subtract_v' :: array_subtract_v
    !DEC$ ATTRIBUTES VALUE :: n
    !DEC$ ATTRIBUTES REFERENCE :: x,y,z
    integer, intent(in) :: n
    real(real64), intent(in) :: x(n), y(n)
    real(real64), intent(out) :: z(n)
    
        z = x - y
    
    end subroutine
    
    pure subroutine array_scale_v(n,f,x,r)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_scale_v
    !DEC$ ATTRIBUTES ALIAS: 'array_scale_v' :: array_scale_v
    !DEC$ ATTRIBUTES VALUE :: n, f
    !DEC$ ATTRIBUTES REFERENCE :: x, r
    integer, intent(in) :: n
    real(real64), intent(in) :: f, x(n)
    real(real64), intent(out) :: r(n)
    
        r = f * x
    
    end subroutine        
    
    pure subroutine array_dot_v(n,x,y,z)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_dot_v
    !DEC$ ATTRIBUTES ALIAS: 'array_dot_v' :: array_dot_v
    !DEC$ ATTRIBUTES VALUE :: n
    !DEC$ ATTRIBUTES REFERENCE :: x, y, z
    integer, intent(in) :: n
    real(real64), intent(in) :: x(n), y(n)
    real(real64), intent(out) :: z
    
        z = dot_product(x, y)    
            
    end subroutine
    
    pure function array_norm_v(n,x) result(s)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_norm_v
    integer, intent(in), value :: n
    real(real64), intent(in) :: x(n)
    real(real64) :: s
        s = norm2(x)
    return
    end
    
    pure function array_norm_m(n,m,x) result(s)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_norm_m
    integer, intent(in), value :: n,m
    real(real64), intent(in) :: x(n,m)    
    real(real64) :: s
        s = norm2(x)
    return
    end
        
    pure subroutine array_product_mv(n,m,A,x,b)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_product_mv
    !DEC$ ATTRIBUTES ALIAS: 'array_product_mv' :: array_product_mv
    !DEC$ ATTRIBUTES VALUE :: n, m
    !DEC$ ATTRIBUTES REFERENCE :: A, b, x
    integer, intent(in) :: n,m
    real(real64), intent(in) :: A(n,m), x(m)
    real(real64), intent(out) :: b(n)
    
        b = matmul(A, x)    
            
    end subroutine
    
    pure subroutine array_product_vm(n,m,x,A,b)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_product_vm
    !DEC$ ATTRIBUTES ALIAS: 'array_product_vm' :: array_product_vm
    !DEC$ ATTRIBUTES VALUE :: n, m
    !DEC$ ATTRIBUTES REFERENCE :: A, b, x
    integer, intent(in) :: n,m
    real(real64), intent(in) :: A(n,m), x(m)
    real(real64), intent(out) :: b(n)
    
        b = matmul(x, A)    
            
    end subroutine
    
    
    pure subroutine array_solve_mv(n,m,A,b,x)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_solve_mv
    !DEC$ ATTRIBUTES ALIAS: 'array_solve_mv' :: array_solve_mv
    !DEC$ ATTRIBUTES VALUE :: n, m
    !DEC$ ATTRIBUTES REFERENCE :: A, b, x
    use mod_array_inv
    integer, intent(in) :: n,m
    real(real64), intent(in) :: A(n,m), b(n)
    real(real64), intent(out) :: x(m)
    real(real64) :: A2(m,m), b2(m), At(m,n)    
    
        if( n > m ) then
            At = transpose(A)
            b2 = matmul(At, b)
            A2 = matmul(At, A)
            x = mat_solve_vec(A2, b2)
        else
            x = mat_solve_vec(A, b)
        end if
            
    end subroutine
    
! *** MATRIX OPERATIONS ***
    
    subroutine array_rand_m(n,m,x,y,A)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_rand_m
    !DEC$ ATTRIBUTES ALIAS: 'array_rand_m' :: array_rand_m
    !DEC$ ATTRIBUTES VALUE :: n, m, x, y
    !DEC$ ATTRIBUTES REFERENCE :: A
    integer, intent(in) :: n, m
    real(real64), intent(in) :: x, y
    real(real64), intent(out) :: A(n, m)

        call RANDOM_NUMBER(A)        
        A = x + (y-x) * A
    
    end subroutine

    pure subroutine array_diag_m(n,x,A)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_diag_m
    !DEC$ ATTRIBUTES ALIAS: 'array_diag_m' :: array_diag_m
    !DEC$ ATTRIBUTES VALUE :: n
    !DEC$ ATTRIBUTES REFERENCE :: x, A
    integer, intent(in) :: n
    real(real64), intent(in) :: x(n)
    real(real64), intent(out) :: A(n,n)
    integer :: i
    
        A = 0.0_real64
        forall(i=1:n)
            A(i,i) = x(i)
        end forall
    
    end subroutine
    
    pure subroutine array_scalar_m(n,m,x,A)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_scalar_m
    !DEC$ ATTRIBUTES ALIAS: 'array_scalar_m' :: array_scalar_m
    !DEC$ ATTRIBUTES VALUE :: n,m,x
    !DEC$ ATTRIBUTES REFERENCE :: A
    integer, intent(in) :: n,m
    real(real64), intent(in) :: x
    real(real64), intent(out) :: A(n,m)
    integer :: i,k
        k = min(n,m)
        A = 0.0_real64
        forall(i=1:k)
            A(i,i) = x
        end forall    
    end subroutine
    
    pure subroutine array_add_m(n,m,x,y,z)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_add_m
    !DEC$ ATTRIBUTES ALIAS: 'array_add_m' :: array_add_m
    !DEC$ ATTRIBUTES VALUE :: n, m
    !DEC$ ATTRIBUTES REFERENCE :: x,y,z
    integer, intent(in) :: n,m
    real(real64), intent(in) :: x(n,m), y(n,m)
    real(real64), intent(out) :: z(n,m)
    
        z = x + y
    
    end subroutine
    
    pure subroutine array_subtract_m(n,m,x,y,z)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_subtract_m
    !DEC$ ATTRIBUTES ALIAS: 'array_subtract_m' :: array_subtract_m
    !DEC$ ATTRIBUTES VALUE :: n,m
    !DEC$ ATTRIBUTES REFERENCE :: x,y,z
    integer, intent(in) :: n,m
    real(real64), intent(in) :: x(n,m), y(n,m)
    real(real64), intent(out) :: z(n,m)
    
        z = x - y
    
    end subroutine
    
    pure subroutine array_scale_m(n,m,x,y,z)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_scale_m
    !DEC$ ATTRIBUTES ALIAS: 'array_scale_m' :: array_scale_m
    !DEC$ ATTRIBUTES VALUE :: n, m, x
    !DEC$ ATTRIBUTES REFERENCE :: y,z
    integer, intent(in) :: n, m
    real(real64), intent(in) :: x, y(n,m)
    real(real64), intent(out) :: z(n,m)
    
        z = x * y
    
    end subroutine
    
    pure subroutine array_tansp_m(n,m,A,At)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_tansp_m
    !DEC$ ATTRIBUTES ALIAS: 'array_tansp_m' :: array_tansp_m
    !DEC$ ATTRIBUTES VALUE :: n, m
    !DEC$ ATTRIBUTES REFERENCE :: A, At
    integer, intent(in) :: n, m
    real(real64), intent(in) :: A(n,m)
    real(real64), intent(out) :: At(m,n)
    
        At = transpose(A)
    
    end subroutine    
    
    pure subroutine array_reshape_mm(n,m,A,k,l,B)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_reshape_mm
    !DEC$ ATTRIBUTES ALIAS: 'array_reshape_mm' :: array_reshape_mm
    !DEC$ ATTRIBUTES VALUE :: n, m, k, l
    !DEC$ ATTRIBUTES REFERENCE :: A, B
    integer, intent(in) :: n, m, k, l
    real(real64), intent(in) :: A(n,m)
    real(real64), intent(out) :: B(k,l)
    
        B = reshape(A, [k,l])
        
    end subroutine
    
    pure subroutine array_slice_v(n,A,i1,i2,B)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_slice_v
    !DEC$ ATTRIBUTES ALIAS: 'array_slice_v' :: array_slice_v
    !DEC$ ATTRIBUTES VALUE :: n, i1,i2
    !DEC$ ATTRIBUTES REFERENCE :: A, B
    integer, intent(in) :: n, i1,i2
    real(real64), intent(in) :: A(n)
    real(real64), intent(out) :: B(i2-i1+1)
    
        B = A(i1:i2)
        
    end subroutine
    pure subroutine array_slice_m(n,m,A,i1,i2,j1,j2,B)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_slice_m
    !DEC$ ATTRIBUTES ALIAS: 'array_slice_m' :: array_slice_m
    !DEC$ ATTRIBUTES VALUE :: n, m, i1,i2,j1,j2
    !DEC$ ATTRIBUTES REFERENCE :: A, B
    integer, intent(in) :: n, m, i1,i2,j1,j2
    real(real64), intent(in) :: A(n,m)
    real(real64), intent(out) :: B(i2-i1+1,j2-j1+1)
    
        B = A(i1:i2, j1:j2)
        
    end subroutine
    
    pure subroutine array_product_mm(n,m,k,A,x,b)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_product_mm
    !DEC$ ATTRIBUTES ALIAS: 'array_product_mm' :: array_product_mm
    !DEC$ ATTRIBUTES VALUE :: n, m, k
    !DEC$ ATTRIBUTES REFERENCE :: A, b, x
    integer, intent(in) :: n,m,k
    real(real64), intent(in) :: A(n,m), x(m,k)
    real(real64), intent(out) :: b(n,k)
    
        b = matmul(A, x)    
            
    end subroutine
    
    pure subroutine array_solve_mm(n,m,k,A,b,x)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_solve_mm
    !DEC$ ATTRIBUTES ALIAS: 'array_solve_mm' :: array_solve_mm
    !DEC$ ATTRIBUTES VALUE :: n, m, k
    !DEC$ ATTRIBUTES REFERENCE :: A, b, x
    use mod_array_inv
    integer, intent(in) :: n,m,k
    real(real64), intent(in) :: A(n,m), b(n,k)
    real(real64), intent(out) :: x(m,k)
    real(real64) :: A2(m,m), b2(m,k), At(m,n)    
    
        if( n > m ) then
            At = transpose(A)
            b2 = matmul(At, b)
            A2 = matmul(At, A)
            x = mat_solve_mat(A2, b2)
        else
            x = mat_solve_mat(A, b)
        end if
            
    end subroutine
    
    pure subroutine array_det_m(n,A,d)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_det_m
    !DEC$ ATTRIBUTES ALIAS: 'array_det_m' :: array_det_m
    !DEC$ ATTRIBUTES VALUE :: n
    !DEC$ ATTRIBUTES REFERENCE :: A, d
    use mod_array_inv
    integer, intent(in) :: n
    real(real64), intent(in) :: A(n,n)
    real(real64), intent(out) :: d
    
        d = mat_det(A)
    
    end subroutine
    
    
    pure subroutine array_inverse_m(n,A,B)
    !DEC$ ATTRIBUTES DLLEXPORT :: array_inverse_m
    !DEC$ ATTRIBUTES ALIAS: 'array_inverse_m' :: array_inverse_m
    !DEC$ ATTRIBUTES VALUE :: n
    !DEC$ ATTRIBUTES REFERENCE :: A, B
    use mod_array_inv
    integer, intent(in) :: n
    real(real64), intent(in) :: A(n,n)
    real(real64), intent(out) :: B(n,n)
    
        B = mat_inv(A)
    
    end subroutine
    
    
    end module

