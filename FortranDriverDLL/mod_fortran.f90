    module mod_fortran
    use, intrinsic :: iso_fortran_env
    implicit none

    abstract interface

    subroutine actionrefint(i,n)
    import
    !DEC$ ATTRIBUTES REFERENCE :: i, n
    integer, intent(in) :: i, n
    end subroutine
    end interface
    
    interface solve
        module procedure :: solve1, solve2
    end interface

    contains

    subroutine DoWork(n, A, progressCallBack)
    !DEC$ ATTRIBUTES DLLEXPORT :: DoWork
    !DEC$ ATTRIBUTES ALIAS: 'DoWork' :: DoWork
    !DEC$ ATTRIBUTES REFERENCE :: n, A, progressCallBack

    procedure(actionrefint) :: progressCallBack
    integer, intent(in) :: n
    !real(real64), dimension(n, n), intent(inout) :: A(:,:)
    real(real64), intent(inout) :: A(n,n)

    integer :: i

    do i = 1, n
        call progressCallBack(i,n)
        A(i, i) = i**2
    end do

    return

    end subroutine
    
    subroutine solve1(n,m,A,b,x)
    !DEC$ ATTRIBUTES DLLEXPORT :: solve1
    !DEC$ ATTRIBUTES ALIAS: 'solve1' :: solve1
    !DEC$ ATTRIBUTES REFERENCE :: n, m, A, b, x
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
    !DEC$ ATTRIBUTES ALIAS: 'solve2' :: solve2
    !DEC$ ATTRIBUTES REFERENCE :: n, m, k, A, b, x
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

