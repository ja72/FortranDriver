    module mod_fortran
    use, intrinsic :: iso_fortran_env
    implicit none

    abstract interface

    subroutine actionrefint(i, n)
    import
    !DEC$ ATTRIBUTES VALUE :: i, n
    integer, intent(in) :: i, n
    end subroutine
    end interface
    
    interface solve
        module procedure :: solve1, solve2
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
    
    subroutine dot1(n,x,y,z)
    !DEC$ ATTRIBUTES DLLEXPORT :: dot1
    !DEC$ ATTRIBUTES ALIAS: 'Dot1' :: dot1
    !DEC$ ATTRIBUTES VALUE :: n
    !DEC$ ATTRIBUTES REFERENCE :: A, b, x
    use mod_array_inv
    integer, intent(in) :: n
    real(real64), intent(in) :: x(n), y(n)
    real(real64), intent(out) :: z
    
        z = dot_product(x, y)    
            
    end subroutine

    
    subroutine product1(n,m,A,x,b)
    !DEC$ ATTRIBUTES DLLEXPORT :: product1
    !DEC$ ATTRIBUTES ALIAS: 'Product1' :: product1
    !DEC$ ATTRIBUTES VALUE :: n, m
    !DEC$ ATTRIBUTES REFERENCE :: A, b, x
    use mod_array_inv
    integer, intent(in) :: n,m
    real(real64), intent(in) :: A(n,m), x(m)
    real(real64), intent(out) :: b(n)
    
        b = matmul(A, x)    
            
    end subroutine
    
    subroutine product2(n,m,k,A,x,b)
    !DEC$ ATTRIBUTES DLLEXPORT :: product2
    !DEC$ ATTRIBUTES ALIAS: 'Product2' :: product2
    !DEC$ ATTRIBUTES VALUE :: n, m, k
    !DEC$ ATTRIBUTES REFERENCE :: A, b, x
    use mod_array_inv
    integer, intent(in) :: n,m,k
    real(real64), intent(in) :: A(n,m), x(m,k)
    real(real64), intent(out) :: b(n,k)
    
        b = matmul(A, x)    
            
    end subroutine
    
    subroutine solve1(n,m,A,b,x)
    !DEC$ ATTRIBUTES DLLEXPORT :: solve1
    !DEC$ ATTRIBUTES ALIAS: 'Solve1' :: solve1
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
    
    subroutine solve2(n,m,k,A,b,x)
    !DEC$ ATTRIBUTES DLLEXPORT :: solve2
    !DEC$ ATTRIBUTES ALIAS: 'Solve2' :: solve2
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

    end module

