  
module mod_array_inv
use mod_common, only : real64, tiny
implicit none


    interface trace
        module procedure :: trace_array_m
    end interface
    interface det
        module procedure :: det_array_m
    end interface
    interface inv
        module procedure :: inv_array_m
    end interface
    interface solve
        module procedure :: solve_array_mv, solve_array_mm
        module procedure :: lui_solve_array_v, lui_solve_array_m
    end interface
            
    contains
    
    
    pure function array_zeros1(n) result(A)
    !!dec$ attributes dllexport :: array_zeros1
    !!dec$ attributes alias : 'array_zeros1' :: array_zeros1
    integer, intent(in),value :: n
    real(real64) :: A(n,n)
    
        A = 0.0_real64
    
    end function
    
    pure function array_zeros2(n,m) result(A)
    !!dec$ attributes dllexport :: array_zeros2
    !!dec$ attributes alias : 'array_zeros2' :: array_zeros2
    integer, intent(in),value :: n, m
    real(real64) :: A(n,m)
    
        A = 0.0_real64
    
    end function
    
    pure function array_eye(n) result(A)
    !MOD_ARRAY_INV_mp_MAT_EYE
    !!dec$ attributes dllexport :: array_eye
    !!dec$ attributes alias : 'array_eye' :: array_eye
    integer, intent(in),value :: n
    real(real64) :: A(n,n)
    integer :: i
    
        A = 0._real64
        do concurrent (i=1:n)
            A(i,i) = 1._real64
        end do
    end function
        
    pure function norm_array_v(n,x) result(s) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: norm_array_v
    integer, intent(in), value :: n
    real(real64), intent(in) :: x(n)
    real(real64) :: s
    s = norm2(x)    
    end function

    pure function norm_array_m(n,m,x) result(s) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: norm_array_m
    integer, intent(in), value :: n,m
    real(real64), intent(in) :: x(n,m)
    real(real64) :: s
    s = norm2(x)    
    end function
            
    pure function outer_array_v(a,b) result(ab) 
    real(real64), intent(in) :: a(:), b(:)
    real(real64) :: ab(size(a),size(b))
    integer :: n,m,i,j
        n = size(a)
        m = size(b)
        
        !do concurrent (i=1:n, j=1:m)
        !    ab(i,j) = a(i)*b(j)
        !end do
        
        AB = spread(source = a, dim = 2, ncopies = m) * &
             spread(source = b, dim = 1, ncopies = n)        
    end function
            
    pure function trace_array_m(A) result(t)
    real(real64), intent(in) :: A(:,:)
    real(real64) :: t
    integer :: n, m, i
    
        n = size(A, 1)
        m = size(A, 2)
        
        if(n /= m) then
            error stop "Expecting square matrix."
        end if
        
        select case(n)
        case(1)  
            t = A(1,1)
        case(2)
            t = A(1,1) + A(2,2)
        case(3)
            t = A(1,1) + A(2,2) + A(3,3)
        case(4)
            t = A(1,1) + A(2,2) + A(3,3) + A(4,4)
        case(5)
            t = A(1,1) + A(2,2) + A(3,3) + A(4,4) + A(5,5)
        case(6)
            t = A(1,1) + A(2,2) + A(3,3) + A(4,4) + A(5,5) + A(6,6)
        case default
            t = 0._real64
            do concurrent(i=1:n)
                t = t + A(i,i)
            end do
        end select
    end function
    
    pure function det_array_m(A) result(d)
    real(real64), intent(in) :: A(:,:)
    real(real64) :: d
    integer :: n, m
    
        n = size(A, 1)
        m = size(A, 2)
        
        if(n /= m) then
            error stop "Expecting square matrix."
        end if
        
        select case(n)
        case(1)  
            d = A(1,1)
        case(2)
            d = det_array2_m(A)
        case(3)
            d = det_array3_m(A)
        case(4)
            d = det_array4_m(A)
        case default
            d = det_array_lu(A)
        end select
    end function
    
    pure function inv_array_m(A) result(B)
    real(real64), intent(in) :: A(:,:)    
    real(real64) :: B(size(A,1),size(A,2))
    integer :: n, m
    
        n = size(A, 1)
        m = size(A, 2)
        
        if(n /= m) then
            error stop "Expecting square matrix."
        end if
        
        select case(n)
        case (1)
            B = 1/A
        case (2)
            B = inv_array2(A)
        case (3)
            B = inv_array3(A)
        case (4)
            B = inv_array4(A)
        case (5)
            B = inv_array5(A)
        case (6)
            B = inv_array6(A)
        case default
            B = inv_lu(A)
        end select
    end function
    
    pure function solve_array_mv(A, b) result(x)
    real(real64), intent(in) :: A(:,:), b(:)
    real(real64) :: x(size(A,1))
    integer :: n,m
    
        n = size(A, 1)
        m = size(A, 2)
        
        if(n /= m) then
            error stop "Expecting square matrix."
        end if

        select case(n)
        case (1)
            x = b/A(1,1)
        case (2)   
            x = solve_array2_v(A, b)
        case (3)
            x = solve_array3_v(A, b)
        case (4)
            x = solve_array4_v(A, b)
        case (5)
            x = solve_block5_v(A, b)
        case (6)
            x = solve_block6_v(A, b)
        case default
            x = solve_lu_v(A, b)
        end select
    end function
    
    pure function solve_array_mm(A, B) result(X)
    real(real64), intent(in) :: A(:,:), B(:,:)
    real(real64) :: X(size(B,1), size(B,2))
    integer :: n
        n = size(A, 1)
        select case(n)
        case (1)
            x = B/A(1,1)
        case (2)   
            x = solve_array2_m(A, B)
        case (3)
            x = solve_array3_m(A, B)
        case (4)
            x = solve_array4_m(A, B)
        case (5)
            x = solve_block5_m(A, B)
        case (6)
            x = solve_block6_m(A, B)
        case default
            X = solve_lu_m(A, B)
        end select
        
    end function
    
    pure function lui_solve_array_v(A, b) result(x)
    use mod_lu
    class(lu_info(*)), intent(in) :: A
    real(real64), intent(in) :: b(:)
    real(real64) :: x(size(b))
        x = A%solve(b)
    end function
    
    pure function lui_solve_array_m(A, B) result(x)
    use mod_lu
    class(lu_info(*)), intent(in) :: A
    real(real64), intent(in) :: b(:,:)
    real(real64) :: x(size(b,1),size(b,2))
        x = A%solve(b)
    end function
    
    pure function det_array2_m(A) result(d)
    implicit real(real64) (T)
    real(real64) :: d
    real(real64), intent(in) :: A(2,2)
    real(real64) :: t2, t5
        t2 = A(1,1)*A(2,2)
        t5 = A(1,2)*A(2,1)
        d = t2-t5
    end function
    
    pure function inv_array2(A, d_known) result(A_inv)
    real(real64), intent(in) :: A(2,2)        
    real(real64), intent(in), optional :: d_known
    real(real64) :: A_inv(2,2), d_inv, d
        if(present(d_known)) then
            d = d_known
        else
            d = det_array2_m(A)
        end if
        if( abs(d) <= tiny ) then
            error stop "Matrix is singular."
        end if
        d_inv = 1/d
        A_inv(1,1) =  A(2,2)*d_inv
        A_inv(1,2) = -A(1,2)*d_inv
        A_inv(2,1) = -A(2,1)*d_inv
        A_inv(2,2) =  A(1,1)*d_inv        
    end function
    
    pure function solve_array2_v(A,b,d_known) result(x)
    real(real64), intent(in) :: A(2,2), b(2)
    real(real64), intent(in), optional :: d_known
    real(real64) :: x(2), d, s(2)
        if(present(d_known)) then
            d = d_known
        else
            d = det_array2_m(A)
        end if
        if( abs(d) <= tiny ) then
            error stop "Matrix is singular."
        end if
        s = b/d
        x(1) = (A(2,2)*s(1) - A(1,2)*s(2))
        x(2) = (A(1,1)*s(2) - A(2,1)*s(1))    
    end function
    
    pure function solve_array2_m(A, B, d_known) result(x)
    real(real64), intent(in) :: A(2,2), B(:,:)
    real(real64), intent(in), optional :: d_known
    real(real64) :: x(2,size(b,2)), d
    integer :: h, j
        if(size(b,1)/=2) then
            error stop "Expecting 2 rows in B."
        end if
        h = size(B, 2)
        if(present(d_known)) then
            d = d_known
        else
            d = det_array2_m(A)
        end if
        if( abs(d) <= tiny ) then
            error stop "Matrix is singular."
        end if
        do j=1, h
            x(:, j) = solve_array2_v(A, B(:,j), d)
        end do
    end function
    
    pure function det_array3_m(A) result(d)
    implicit real(real64) (T)
    real(real64) :: d
    real(real64), intent(in) :: A(3,3)
    real(real64) :: t2, t3, t4, t7, t8, t9
        t2 = A(1,1)*A(2,2)*A(3,3)
        t3 = A(1,2)*A(2,3)*A(3,1)
        t4 = A(1,3)*A(2,1)*A(3,2)
        t7 = A(1,1)*A(2,3)*A(3,2)
        t8 = A(1,2)*A(2,1)*A(3,3)
        t9 = A(1,3)*A(2,2)*A(3,1)
        d = t2+t3+t4-t7-t8-t9
    end function
    
    pure function inv_array3(A, d_known) result(B)
    real(real64), intent(in) :: A(3,3)
    real(real64), intent(in), optional :: d_known
    real(real64) :: B(3,3), d_inv, d
        if(present(d_known)) then
            d = d_known
        else
            d = det_array3_m(A)
        end if
        if( abs(d) <= tiny ) then
            error stop "Matrix is singular."
        end if
        d_inv = 1/d
        B(1,1) =  d_inv*(A(2,2)*A(3,3)-A(2,3)*A(3,2))
        B(1,2) = -d_inv*(A(1,2)*A(3,3)-A(1,3)*A(3,2))
        B(1,3) =  d_inv*(A(1,2)*A(2,3)-A(1,3)*A(2,2))
        B(2,1) = -d_inv*(A(2,1)*A(3,3)-A(2,3)*A(3,1))
        B(2,2) =  d_inv*(A(1,1)*A(3,3)-A(1,3)*A(3,1))
        B(2,3) = -d_inv*(A(1,1)*A(2,3)-A(1,3)*A(2,1))
        B(3,1) =  d_inv*(A(2,1)*A(3,2)-A(2,2)*A(3,1))
        B(3,2) = -d_inv*(A(1,1)*A(3,2)-A(1,2)*A(3,1))
        B(3,3) =  d_inv*(A(1,1)*A(2,2)-A(1,2)*A(2,1))    
        
    end function
    
    pure function solve_array3_v(A, b, d_known) result(x)
    real(real64), intent(in) :: A(3,3), b(3)
    real(real64), intent(in), optional :: d_known
    real(real64) :: x(3), d, s(3)
        if(present(d_known)) then
            d = d_known
        else
            d = det_array3_m(A)
        end if
        if( abs(d) <= tiny ) then
            error stop "Matrix is singular."
        end if
        s = b/d
        x(1) = (A(1,2)*(A(2,3)*s(3)-A(3,3)*s(2))+A(1,3)*(A(3,2)*s(2)-A(2,2)*s(3))+s(1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2)))
        x(2) = (A(1,1)*(A(3,3)*s(2)-A(2,3)*s(3))+A(1,3)*(A(2,1)*s(3)-A(3,1)*s(2))-s(1)*(A(2,1)*A(3,3)-A(2,3)*A(3,1)))
        x(3) = (A(1,1)*(A(2,2)*s(3)-A(3,2)*s(2))+A(1,2)*(A(3,1)*s(2)-A(2,1)*s(3))+s(1)*(A(2,1)*A(3,2)-A(2,2)*A(3,1)))
    end function
    
    pure function solve_array3_m(A, B, d_known) result(x)
    real(real64), intent(in) :: A(3,3), B(:,:)
    real(real64), intent(in), optional :: d_known
    real(real64) :: x(3,size(b,2)), d
    integer :: h, j
        if( size(B,1) /= 3) then
            error stop "Expecting 3 rows in B."
        end if
        h = size(B,2)
        if(present(d_known)) then
            d = d_known
        else
            d = det_array3_m(A)
        end if
        if( abs(d) <= tiny ) then
            error stop "Matrix is singular."
        end if
        do j = 1, h
            x(:,j) = solve_array3_v(A, b(:,j), d)
        end do
    end function
    
    pure function det_array4_m(A) result(d)
    implicit real(real64) (T)
    real(real64) :: d
    real(real64), intent(in) :: A(4,4)
    real(real64) :: t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13
    real(real64) :: t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27
    
      t2 = A(1,1)*A(2,2)*A(3,3)*A(4,4)
      t3 = A(1,1)*A(2,3)*A(3,4)*A(4,2)
      t4 = A(1,1)*A(2,4)*A(3,2)*A(4,3)
      t5 = A(1,2)*A(2,1)*A(3,4)*A(4,3)
      t6 = A(1,2)*A(2,3)*A(3,1)*A(4,4)
      t7 = A(1,2)*A(2,4)*A(3,3)*A(4,1)
      t8 = A(1,3)*A(2,1)*A(3,2)*A(4,4)
      t9 = A(1,3)*A(2,2)*A(3,4)*A(4,1)
      t10 = A(1,3)*A(2,4)*A(3,1)*A(4,2)
      t11 = A(1,4)*A(2,1)*A(3,3)*A(4,2)
      t12 = A(1,4)*A(2,2)*A(3,1)*A(4,3)
      t13 = A(1,4)*A(2,3)*A(3,2)*A(4,1)
      t16 = A(1,1)*A(2,2)*A(3,4)*A(4,3)
      t17 = A(1,1)*A(2,3)*A(3,2)*A(4,4)
      t18 = A(1,1)*A(2,4)*A(3,3)*A(4,2)
      t19 = A(1,2)*A(2,1)*A(3,3)*A(4,4)
      t20 = A(1,2)*A(2,3)*A(3,4)*A(4,1)
      t21 = A(1,2)*A(2,4)*A(3,1)*A(4,3)
      t22 = A(1,3)*A(2,1)*A(3,4)*A(4,2)
      t23 = A(1,3)*A(2,2)*A(3,1)*A(4,4)
      t24 = A(1,3)*A(2,4)*A(3,2)*A(4,1)
      t25 = A(1,4)*A(2,1)*A(3,2)*A(4,3)
      t26 = A(1,4)*A(2,2)*A(3,3)*A(4,1)
      t27 = A(1,4)*A(2,3)*A(3,1)*A(4,2)
      d = t2+t3+t4+t5+t6+t7+t8+t9+t10+t11+t12+t13-t16-t17-t18-t19-t20-t21-t22-t23-t24-t25-t26-t27
    end function
    
    
    pure function inv_array4(A, d_known) result(A_inv)
    real(real64) :: A_inv(4,4), d_inv, d
    real(real64), intent(in) :: A(4,4)
    real(real64), intent(in), optional :: d_known
        if(present(d_known)) then
            d = d_known
        else
            d = det_array4_m(A)
        end if
        if( abs(d) <= tiny ) then
            error stop "Matrix is singular."
        end if
        d_inv = 1/d
        A_inv(1,1) =  d_inv*(A(2,2)*A(3,3)*A(4,4)-A(2,2)*A(3,4)*A(4,3)-A(2,3)*A(3,2)*A(4,4)+A(2,3)*A(3,4)*A(4,2)+A(2,4)*A(3,2)*A(4,3)-A(2,4)*A(3,3)*A(4,2))
        A_inv(1,2) = -d_inv*(A(1,2)*A(3,3)*A(4,4)-A(1,2)*A(3,4)*A(4,3)-A(1,3)*A(3,2)*A(4,4)+A(1,3)*A(3,4)*A(4,2)+A(1,4)*A(3,2)*A(4,3)-A(1,4)*A(3,3)*A(4,2))
        A_inv(1,3) =  d_inv*(A(1,2)*A(2,3)*A(4,4)-A(1,2)*A(2,4)*A(4,3)-A(1,3)*A(2,2)*A(4,4)+A(1,3)*A(2,4)*A(4,2)+A(1,4)*A(2,2)*A(4,3)-A(1,4)*A(2,3)*A(4,2))        
        A_inv(1,4) = -d_inv*(A(1,2)*A(2,3)*A(3,4)-A(1,2)*A(2,4)*A(3,3)-A(1,3)*A(2,2)*A(3,4)+A(1,3)*A(2,4)*A(3,2)+A(1,4)*A(2,2)*A(3,3)-A(1,4)*A(2,3)*A(3,2))
        
        A_inv(2,1) = -d_inv*(A(2,1)*A(3,3)*A(4,4)-A(2,1)*A(3,4)*A(4,3)-A(2,3)*A(3,1)*A(4,4)+A(2,3)*A(3,4)*A(4,1)+A(2,4)*A(3,1)*A(4,3)-A(2,4)*A(3,3)*A(4,1))
        A_inv(2,2) =  d_inv*(A(1,1)*A(3,3)*A(4,4)-A(1,1)*A(3,4)*A(4,3)-A(1,3)*A(3,1)*A(4,4)+A(1,3)*A(3,4)*A(4,1)+A(1,4)*A(3,1)*A(4,3)-A(1,4)*A(3,3)*A(4,1))
        A_inv(2,3) = -d_inv*(A(1,1)*A(2,3)*A(4,4)-A(1,1)*A(2,4)*A(4,3)-A(1,3)*A(2,1)*A(4,4)+A(1,3)*A(2,4)*A(4,1)+A(1,4)*A(2,1)*A(4,3)-A(1,4)*A(2,3)*A(4,1))
        A_inv(2,4) =  d_inv*(A(1,1)*A(2,3)*A(3,4)-A(1,1)*A(2,4)*A(3,3)-A(1,3)*A(2,1)*A(3,4)+A(1,3)*A(2,4)*A(3,1)+A(1,4)*A(2,1)*A(3,3)-A(1,4)*A(2,3)*A(3,1))
        
        A_inv(3,1) =  d_inv*(A(2,1)*A(3,2)*A(4,4)-A(2,1)*A(3,4)*A(4,2)-A(2,2)*A(3,1)*A(4,4)+A(2,2)*A(3,4)*A(4,1)+A(2,4)*A(3,1)*A(4,2)-A(2,4)*A(3,2)*A(4,1))
        A_inv(3,2) = -d_inv*(A(1,1)*A(3,2)*A(4,4)-A(1,1)*A(3,4)*A(4,2)-A(1,2)*A(3,1)*A(4,4)+A(1,2)*A(3,4)*A(4,1)+A(1,4)*A(3,1)*A(4,2)-A(1,4)*A(3,2)*A(4,1))
        A_inv(3,3) =  d_inv*(A(1,1)*A(2,2)*A(4,4)-A(1,1)*A(2,4)*A(4,2)-A(1,2)*A(2,1)*A(4,4)+A(1,2)*A(2,4)*A(4,1)+A(1,4)*A(2,1)*A(4,2)-A(1,4)*A(2,2)*A(4,1))
        A_inv(3,4) = -d_inv*(A(1,1)*A(2,2)*A(3,4)-A(1,1)*A(2,4)*A(3,2)-A(1,2)*A(2,1)*A(3,4)+A(1,2)*A(2,4)*A(3,1)+A(1,4)*A(2,1)*A(3,2)-A(1,4)*A(2,2)*A(3,1))
        
        A_inv(4,1) = -d_inv*(A(2,1)*A(3,2)*A(4,3)-A(2,1)*A(3,3)*A(4,2)-A(2,2)*A(3,1)*A(4,3)+A(2,2)*A(3,3)*A(4,1)+A(2,3)*A(3,1)*A(4,2)-A(2,3)*A(3,2)*A(4,1))
        A_inv(4,2) =  d_inv*(A(1,1)*A(3,2)*A(4,3)-A(1,1)*A(3,3)*A(4,2)-A(1,2)*A(3,1)*A(4,3)+A(1,2)*A(3,3)*A(4,1)+A(1,3)*A(3,1)*A(4,2)-A(1,3)*A(3,2)*A(4,1))
        A_inv(4,3) = -d_inv*(A(1,1)*A(2,2)*A(4,3)-A(1,1)*A(2,3)*A(4,2)-A(1,2)*A(2,1)*A(4,3)+A(1,2)*A(2,3)*A(4,1)+A(1,3)*A(2,1)*A(4,2)-A(1,3)*A(2,2)*A(4,1))
        A_inv(4,4) =  d_inv*(A(1,1)*A(2,2)*A(3,3)-A(1,1)*A(2,3)*A(3,2)-A(1,2)*A(2,1)*A(3,3)+A(1,2)*A(2,3)*A(3,1)+A(1,3)*A(2,1)*A(3,2)-A(1,3)*A(2,2)*A(3,1))

    end function

    pure function solve_array4_v(A, b, d_known) result(x)
    real(real64), intent(in) :: A(4,4), b(4)
    real(real64), intent(in), optional :: d_known
    real(real64) :: x(4), d, s(4)
    
        !x = matmul(inv_array4(A), b)
        
        if(present(d_known)) then
            d = d_known
        else
            d = det_array4_m(A)
        end if
        if( abs(d) <= tiny ) then
            error stop "Matrix is singular."
        end if
        !d_inv = 1/d
        s = b/d
        x(1) =  s(1)*(A(2,2)*A(3,3)*A(4,4)-A(2,2)*A(3,4)*A(4,3)-A(2,3)*A(3,2)*A(4,4)+A(2,3)*A(3,4)*A(4,2)+A(2,4)*A(3,2)*A(4,3)-A(2,4)*A(3,3)*A(4,2)) &
               -s(2)*(A(1,2)*A(3,3)*A(4,4)-A(1,2)*A(3,4)*A(4,3)-A(1,3)*A(3,2)*A(4,4)+A(1,3)*A(3,4)*A(4,2)+A(1,4)*A(3,2)*A(4,3)-A(1,4)*A(3,3)*A(4,2)) &
               +s(3)*(A(1,2)*A(2,3)*A(4,4)-A(1,2)*A(2,4)*A(4,3)-A(1,3)*A(2,2)*A(4,4)+A(1,3)*A(2,4)*A(4,2)+A(1,4)*A(2,2)*A(4,3)-A(1,4)*A(2,3)*A(4,2)) &
               -s(4)*(A(1,2)*A(2,3)*A(3,4)-A(1,2)*A(2,4)*A(3,3)-A(1,3)*A(2,2)*A(3,4)+A(1,3)*A(2,4)*A(3,2)+A(1,4)*A(2,2)*A(3,3)-A(1,4)*A(2,3)*A(3,2)) 
                                                                                                                                                     
        x(2) = -s(1)*(A(2,1)*A(3,3)*A(4,4)-A(2,1)*A(3,4)*A(4,3)-A(2,3)*A(3,1)*A(4,4)+A(2,3)*A(3,4)*A(4,1)+A(2,4)*A(3,1)*A(4,3)-A(2,4)*A(3,3)*A(4,1)) &
               +s(2)*(A(1,1)*A(3,3)*A(4,4)-A(1,1)*A(3,4)*A(4,3)-A(1,3)*A(3,1)*A(4,4)+A(1,3)*A(3,4)*A(4,1)+A(1,4)*A(3,1)*A(4,3)-A(1,4)*A(3,3)*A(4,1)) &
               -s(3)*(A(1,1)*A(2,3)*A(4,4)-A(1,1)*A(2,4)*A(4,3)-A(1,3)*A(2,1)*A(4,4)+A(1,3)*A(2,4)*A(4,1)+A(1,4)*A(2,1)*A(4,3)-A(1,4)*A(2,3)*A(4,1)) &
               +s(4)*(A(1,1)*A(2,3)*A(3,4)-A(1,1)*A(2,4)*A(3,3)-A(1,3)*A(2,1)*A(3,4)+A(1,3)*A(2,4)*A(3,1)+A(1,4)*A(2,1)*A(3,3)-A(1,4)*A(2,3)*A(3,1)) 
                                                                                                                                                     
        x(3) = +s(1)*(A(2,1)*A(3,2)*A(4,4)-A(2,1)*A(3,4)*A(4,2)-A(2,2)*A(3,1)*A(4,4)+A(2,2)*A(3,4)*A(4,1)+A(2,4)*A(3,1)*A(4,2)-A(2,4)*A(3,2)*A(4,1)) &
               -s(2)*(A(1,1)*A(3,2)*A(4,4)-A(1,1)*A(3,4)*A(4,2)-A(1,2)*A(3,1)*A(4,4)+A(1,2)*A(3,4)*A(4,1)+A(1,4)*A(3,1)*A(4,2)-A(1,4)*A(3,2)*A(4,1)) &
               +s(3)*(A(1,1)*A(2,2)*A(4,4)-A(1,1)*A(2,4)*A(4,2)-A(1,2)*A(2,1)*A(4,4)+A(1,2)*A(2,4)*A(4,1)+A(1,4)*A(2,1)*A(4,2)-A(1,4)*A(2,2)*A(4,1)) &
               -s(4)*(A(1,1)*A(2,2)*A(3,4)-A(1,1)*A(2,4)*A(3,2)-A(1,2)*A(2,1)*A(3,4)+A(1,2)*A(2,4)*A(3,1)+A(1,4)*A(2,1)*A(3,2)-A(1,4)*A(2,2)*A(3,1)) 
                                                                                                                                                     
        x(4) = -s(1)*(A(2,1)*A(3,2)*A(4,3)-A(2,1)*A(3,3)*A(4,2)-A(2,2)*A(3,1)*A(4,3)+A(2,2)*A(3,3)*A(4,1)+A(2,3)*A(3,1)*A(4,2)-A(2,3)*A(3,2)*A(4,1)) &
               +s(2)*(A(1,1)*A(3,2)*A(4,3)-A(1,1)*A(3,3)*A(4,2)-A(1,2)*A(3,1)*A(4,3)+A(1,2)*A(3,3)*A(4,1)+A(1,3)*A(3,1)*A(4,2)-A(1,3)*A(3,2)*A(4,1)) &
               -s(3)*(A(1,1)*A(2,2)*A(4,3)-A(1,1)*A(2,3)*A(4,2)-A(1,2)*A(2,1)*A(4,3)+A(1,2)*A(2,3)*A(4,1)+A(1,3)*A(2,1)*A(4,2)-A(1,3)*A(2,2)*A(4,1)) &
               +s(4)*(A(1,1)*A(2,2)*A(3,3)-A(1,1)*A(2,3)*A(3,2)-A(1,2)*A(2,1)*A(3,3)+A(1,2)*A(2,3)*A(3,1)+A(1,3)*A(2,1)*A(3,2)-A(1,3)*A(2,2)*A(3,1)) 
                
    end function
    
    pure function solve_array4_m(A,B, d_known) result(x)
    real(real64), intent(in) :: A(4,4), B(:,:)
    real(real64), intent(in), optional :: d_known
    real(real64) :: x(4, size(B, 2)), d
    integer :: m, h, j
        m = size(B,1)
        if( m/= 4) then
            error stop "Expecting a 4 rows in B."
        end if
        h = size(B, 2)
        if(present(d_known)) then
            d = d_known
        else
            d = det_array4_m(A)
        end if
        do j=1, h
            x(:,j) = solve_array4_v(A, B(:,j), d)
        end do
    end function
    
    pure function inv_array5(A) result(A_inv)
    real(real64), intent(in) :: A(5,5)    
    real(real64) :: A_inv(5,5), I(5,5)
    
        I = array_eye(5)
        A_inv = solve(A, I)
    
    end function
    
    
    pure function solve_block5_v(A, b) result(x)
    real(real64), intent(in) :: A(5,5), b(5)
    real(real64) :: x(5), x_1(3), x_2(2)
    real(real64) :: A_11(3,3), A_12(3,2), A_21(2,3), A_22(2,2)
    real(real64) :: B_12(3,2), B_21(2,3)
    real(real64) :: b_1(3), b_2(2), y_1(3), y_2(2)
    
        A_11 = A(1:3, 1:3)
        A_12 = A(1:3, 4:5)
        A_21 = A(4:5, 1:3)
        A_22 = A(4:5, 4:5)
        b_1 = b(1:3)
        b_2 = b(4:5)
        
        y_1 = solve(A_11, b_1)
        y_2 = solve(A_22, b_2)
        B_12 = solve(A_11, A_12)
        B_21 = solve(A_22, A_21)
        x_1 = solve(A_11 - matmul(A_12,B_21), b_1 - matmul(A_12,y_2))
        x_2 = solve(A_22 - matmul(A_21,B_12), b_2 - matmul(A_21,y_1))
        
        x = [x_1, x_2]
        
    end function
    
    pure function solve_block5_m(A, B) result(x)
    real(real64), intent(in) :: A(5,5), B(:,:)
    real(real64) :: x(5, size(B, 2)), x_1(3, size(B, 2)), x_2(2, size(B, 2))
    real(real64) :: A_11(3,3), A_12(3,2), A_21(2,3), A_22(2,2)
    real(real64) :: B_12(3,2), B_21(2,3)
    real(real64) :: b_1(3,size(B, 2)), b_2(2,size(B, 2)), y_1(3,size(B, 2)), y_2(2,size(B, 2))
    integer :: m, h
        m = size(B, 1)
        if( m/= 5) then
            error stop "Expecting a 5 rows in B."
        end if
        h = size(B, 2)
        
        A_11 = A(1:3, 1:3)
        A_12 = A(1:3, 4:5)
        A_21 = A(4:5, 1:3)
        A_22 = A(4:5, 4:5)
        b_1 = b(1:3,:)
        b_2 = b(4:5,:)
        
        y_1 = solve(A_11, b_1)
        y_2 = solve(A_22, b_2)
        B_12 = solve(A_11, A_12)
        B_21 = solve(A_22, A_21)
        x_1 = solve(A_11 - matmul(A_12,B_21), b_1 - matmul(A_12,y_2))
        x_2 = solve(A_22 - matmul(A_21,B_12), b_2 - matmul(A_21,y_1))
        
        x(1:3,:) = x_1
        x(4:5,:) = x_2
    end function

    pure function inv_array6(A) result(A_inv)
    real(real64), intent(in) :: A(6,6)    
    real(real64) :: A_inv(6,6), I(6,6)
    
        I = array_eye(6)
        A_inv = solve(A, I)
    
    end function
    
    pure function solve_block6_v(A, b) result(x)
    real(real64), intent(in) :: A(6,6), b(6)
    real(real64) :: x(6), x_1(3), x_2(3)
    real(real64) :: A_11(3,3), A_12(3,3), A_21(3,3), A_22(3,3)
    real(real64) :: B_12(3,3), B_21(3,3)
    real(real64) :: b_1(3), b_2(3), y_1(3), y_2(3)
    
        A_11 = A(1:3, 1:3)
        A_12 = A(1:3, 4:6)
        A_21 = A(4:6, 1:3)
        A_22 = A(4:6, 4:6)
        b_1 = b(1:3)
        b_2 = b(4:6)
    
        y_1 = solve(A_11, b_1)
        y_2 = solve(A_22, b_2)
        B_12 = solve(A_11, A_12)
        B_21 = solve(A_22, A_21)
        x_1 = solve(A_11 - matmul(A_12,B_21), b_1 - matmul(A_12,y_2))
        x_2 = solve(A_22 - matmul(A_21,B_12), b_2 - matmul(A_21,y_1))
        
        x = [x_1, x_2]
        
    end function
    
    pure function solve_block6_m(A, B) result(x)
    real(real64), intent(in) :: A(6,6), B(:,:)
    real(real64) :: x(6, size(B, 2)), x_1(3, size(B, 2)), x_2(3, size(B, 2))
    real(real64) :: A_11(3,3), A_12(3,3), A_21(3,3), A_22(3,3)
    real(real64) :: B_12(3,3), B_21(3,3)
    real(real64) :: b_1(3,size(B, 2)), b_2(3,size(B, 2)), y_1(3,size(B, 2)), y_2(3,size(B, 2))
    integer :: m, h
        m = size(B, 1)
        if( m/= 6) then
            error stop "Expecting a 6 rows in B."
        end if
        h = size(B, 2)
        
        A_11 = A(1:3, 1:3)
        A_12 = A(1:3, 4:6)
        A_21 = A(4:6, 1:3)
        A_22 = A(4:6, 4:6)
        b_1 = b(1:3,:)
        b_2 = b(4:6,:)
        
        y_1 = solve(A_11, b_1)
        y_2 = solve(A_22, b_2)
        B_12 = solve(A_11, A_12)
        B_21 = solve(A_22, A_21)
        x_1 = solve(A_11 - matmul(A_12,B_21), b_1 - matmul(A_12,y_2))
        x_2 = solve(A_22 - matmul(A_21,B_12), b_2 - matmul(A_21,y_1))
        
        x(1:3,:) = x_1
        x(4:6,:) = x_2
    end function
    
    pure function det_array_lu(A) result(d)
    real(real64), intent(in) :: A(:,:)
    real(real64) :: d
    integer :: n
        n = size(A,1)
        if( n/= size(A,2)) then
            error stop "Expecting a square matrix."
        end if
        d = det_array_lu_fixed(n, A)
    end function
    
    pure function det_array_lu_fixed(n, A) result(d)
    use mod_lu
    integer, intent(in) :: n
    real(real64), intent(in) :: A(n,n)
    real(real64) :: d
    type(lu_info(n)) :: lui
    
        if( n == 1 ) then
            d = A(1,1)
        end if

        lui = lu(n, A)
        d = lui%det()

    end function
    
    pure function inv_lu(A) result(A_inv)
    real(real64), intent(in) :: A(:,:)
    real(real64) :: A_inv(size(A,1),size(A,2))
    integer :: n
    
        n = size(A,1)
        if( n/= size(A,2)) then
            error stop "Expecting a square matrix."
        end if
        A_inv = inv_lu_fix(n, A)
            
    end function
    pure function inv_lu_fix(n, A) result(A_inv)
    use mod_lu
    integer, intent(in) :: n
    real(real64), intent(in) :: A(n,n)
    real(real64) :: A_inv(n,n)
    type(lu_info(n)) :: lui
    
        if(n == 1) then
            A_inv = 1.0_real64/A(1, 1) 
            return
        end if

        lui = lu(n, A)
        A_inv = lui%inv()
            
    end function
    
    pure function solve_lu_v(A, b) result(x)
    real(real64), intent(in) :: A(:,:), b(:)
    real(real64) :: x(size(b))
    integer :: n
        n = size(A, 1)
        if( n/= size(A,2)) then
            error stop "Expecting a square matrix."
        end if
        if( n/= size(b)) then
            error stop "Incompatible matrix sizes."
        end if
        x = solve_lu_v_fix(n, A, b)
    end function
    
    pure function solve_lu_v_fix(n, A, b) result(x)
    use mod_lu
    integer, intent(in) :: n
    real(real64), intent(in) :: A(n,n), b(n)
    real(real64) :: x(n)
    type(lu_info(n)) :: lui
    
        if(n == 1) then
            x = b/A(1, 1) 
            return
        end if
    
        lui = lu(n, A)
        x = lui%solve(b)
        
    end function
    
    pure function solve_lu_m(A, b) result(x)
    real(real64), intent(in) :: A(:,:), b(:,:)
    real(real64) :: x(size(b,1),size(b,2))
    integer :: n, k
        n = size(A, 1)
        k = size(B, 2)
        if( n/= size(A,2)) then
            error stop "Expecting a square matrix."
        end if
        if( n/= size(b, 1)) then
            error stop "Incompatible matrix sizes."
        end if
        x = solve_lu_m_fix(n, k, A, b)
    end function
    
    pure function solve_lu_m_fix(n, k, A, b) result(x)
    use mod_lu
    integer, intent(in) :: n, k
    real(real64), intent(in) :: A(n,n), b(n,k)
    real(real64) :: x(n,k)
    type(lu_info(n)) :: lui
    
        if(n == 1) then
            x = b/A(1, 1) 
            return
        end if
        
        lui = lu(n, A)
        x = lui%solve(b)
        
    end function
        
    
end module