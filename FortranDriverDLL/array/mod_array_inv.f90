  
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
    integer :: n,m
        n = size(a)
        m = size(b)
                
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
        case(6)
            d = det_array6_m(A)
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
        case (6)
            x = solve_array6_v(A, b)
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
        case (6)
            x = solve_array6_m(A, B)
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
    
    !pure function det_array3_m(A) result(d)
    !implicit real(real64) (T)
    !real(real64) :: d
    !real(real64), intent(in) :: A(3,3)
    !real(real64) :: t2, t3, t4, t7, t8, t9
    !    t2 = A(1,1)*A(2,2)*A(3,3)
    !    t3 = A(1,2)*A(2,3)*A(3,1)
    !    t4 = A(1,3)*A(2,1)*A(3,2)
    !    t7 = A(1,1)*A(2,3)*A(3,2)
    !    t8 = A(1,2)*A(2,1)*A(3,3)
    !    t9 = A(1,3)*A(2,2)*A(3,1)
    !    d = t2+t3+t4-t7-t8-t9
    !end function
    
    pure function det_array3_m(A) result(d)
    real(real64), intent(in) :: A(3,3)
    real(real64) :: d
    real(real64) :: A_11,A_12,A_13
    real(real64) :: A_21,A_22,A_23
    real(real64) :: A_31,A_32,A_33
    real(real64) :: t2,t3,t4,t5,t6,t7,t8,t9,t10
            
        A_11 = A(1,1)
        A_21 = A(2,1)
        A_31 = A(3,1)
        A_12 = A(1,2)
        A_22 = A(2,2)
        A_32 = A(3,2)
        A_13 = A(1,3)
        A_23 = A(2,3)
        A_33 = A(3,3)
      
        t2 = A_11*A_22*A_33
        t3 = A_11*A_23*A_32
        t4 = A_12*A_21*A_33
        t5 = A_12*A_23*A_31
        t6 = A_13*A_21*A_32
        t7 = A_13*A_22*A_31
        t8 = -t3
        t9 = -t4
        t10 = -t7
        d = t2+t5+t6+t8+t9+t10
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
    
    pure function det_array6_m(A) result(d)
    real(real64) :: d
    real(real64), intent(in) :: A(6,6)
    real(real64) :: a11, a12, a13, a14, a15, a16
    real(real64) :: a21, a22, a23, a24, a25, a26
    real(real64) :: a31, a32, a33, a34, a35, a36
    real(real64) :: a41, a42, a43, a44, a45, a46
    real(real64) :: a51, a52, a53, a54, a55, a56
    real(real64) :: a61, a62, a63, a64, a65, a66
    
    a11 = A(1,1)
    a21 = A(2,1)
    a31 = A(3,1)
    a41 = A(4,1)
    a51 = A(5,1)
    a61 = A(6,1)

    a12 = A(1,2)
    a22 = A(2,2)
    a32 = A(3,2)
    a42 = A(4,2)
    a52 = A(5,2)
    a62 = A(6,2)

    a13 = A(1,3)
    a23 = A(2,3)
    a33 = A(3,3)
    a43 = A(4,3)
    a53 = A(5,3)
    a63 = A(6,3)

    a14 = A(1,4)
    a24 = A(2,4)
    a34 = A(3,4)
    a44 = A(4,4)
    a54 = A(5,4)
    a64 = A(6,4)

    a15 = A(1,5)
    a25 = A(2,5)
    a35 = A(3,5)
    a45 = A(4,5)
    a55 = A(5,5)
    a65 = A(6,5)

    a16 = A(1,6)
    a26 = A(2,6)
    a36 = A(3,6)
    a46 = A(4,6)
    a56 = A(5,6)
    a66 = A(6,6)


    d = (a11*a22*a33*a44*a55* &
      a66 - a11*a22*a33*a44*a56*a65 - a11*a22*a33*a45*a54*a66 + a11*a22 &
      *a33*a45*a56*a64 + a11*a22*a33*a46*a54*a65 - a11*a22*a33*a46*a55* &
      a64 - a11*a22*a34*a43*a55*a66 + a11*a22*a34*a43*a56*a65 + a11*a22 &
      *a34*a45*a53*a66 - a11*a22*a34*a45*a56*a63 - a11*a22*a34*a46*a53* &
      a65 + a11*a22*a34*a46*a55*a63 + a11*a22*a35*a43*a54*a66 - a11*a22 &
      *a35*a43*a56*a64 - a11*a22*a35*a44*a53*a66 + a11*a22*a35*a44*a56* &
      a63 + a11*a22*a35*a46*a53*a64 - a11*a22*a35*a46*a54*a63 - a11*a22 &
      *a36*a43*a54*a65 + a11*a22*a36*a43*a55*a64 + a11*a22*a36*a44*a53* &
      a65 - a11*a22*a36*a44*a55*a63 - a11*a22*a36*a45*a53*a64 + a11*a22 &
      *a36*a45*a54*a63 - a11*a23*a32*a44*a55*a66 + a11*a23*a32*a44*a56* &
      a65 + a11*a23*a32*a45*a54*a66 - a11*a23*a32*a45*a56*a64 - a11*a23 &
      *a32*a46*a54*a65 + a11*a23*a32*a46*a55*a64 + a11*a23*a34*a42*a55* &
      a66 - a11*a23*a34*a42*a56*a65 - a11*a23*a34*a45*a52*a66 + a11*a23 &
      *a34*a45*a56*a62 + a11*a23*a34*a46*a52*a65 - a11*a23*a34*a46*a55* &
      a62 - a11*a23*a35*a42*a54*a66 + a11*a23*a35*a42*a56*a64 + a11*a23 &
      *a35*a44*a52*a66 - a11*a23*a35*a44*a56*a62 - a11*a23*a35*a46*a52* &
      a64 + a11*a23*a35*a46*a54*a62 + a11*a23*a36*a42*a54*a65 - a11*a23 &
      *a36*a42*a55*a64 - a11*a23*a36*a44*a52*a65 + a11*a23*a36*a44*a55* &
      a62 + a11*a23*a36*a45*a52*a64 - a11*a23*a36*a45*a54*a62 + a11*a24 &
      *a32*a43*a55*a66 - a11*a24*a32*a43*a56*a65 - a11*a24*a32*a45*a53* &
      a66 + a11*a24*a32*a45*a56*a63 + a11*a24*a32*a46*a53*a65 - a11*a24 &
      *a32*a46*a55*a63 - a11*a24*a33*a42*a55*a66 + a11*a24*a33*a42*a56* &
      a65 + a11*a24*a33*a45*a52*a66 - a11*a24*a33*a45*a56*a62 - a11*a24 &
      *a33*a46*a52*a65 + a11*a24*a33*a46*a55*a62 + a11*a24*a35*a42*a53* &
      a66 - a11*a24*a35*a42*a56*a63 - a11*a24*a35*a43*a52*a66 + a11*a24 &
      *a35*a43*a56*a62 + a11*a24*a35*a46*a52*a63 - a11*a24*a35*a46*a53* &
      a62 - a11*a24*a36*a42*a53*a65 + a11*a24*a36*a42*a55*a63 + a11*a24 &
      *a36*a43*a52*a65 - a11*a24*a36*a43*a55*a62 - a11*a24*a36*a45*a52* &
      a63 + a11*a24*a36*a45*a53*a62 - a11*a25*a32*a43*a54*a66 + a11*a25 &
      *a32*a43*a56*a64 + a11*a25*a32*a44*a53*a66 - a11*a25*a32*a44*a56* &
      a63 - a11*a25*a32*a46*a53*a64 + a11*a25*a32*a46*a54*a63 + a11*a25 &
      *a33*a42*a54*a66 - a11*a25*a33*a42*a56*a64 - a11*a25*a33*a44*a52* &
      a66 + a11*a25*a33*a44*a56*a62 + a11*a25*a33*a46*a52*a64 - a11*a25 &
      *a33*a46*a54*a62 - a11*a25*a34*a42*a53*a66 + a11*a25*a34*a42*a56* &
      a63 + a11*a25*a34*a43*a52*a66 - a11*a25*a34*a43*a56*a62 - a11*a25 &
      *a34*a46*a52*a63 + a11*a25*a34*a46*a53*a62 + a11*a25*a36*a42*a53* &
      a64 - a11*a25*a36*a42*a54*a63 - a11*a25*a36*a43*a52*a64 + a11*a25 &
      *a36*a43*a54*a62 + a11*a25*a36*a44*a52*a63 - a11*a25*a36*a44*a53* &
      a62 + a11*a26*a32*a43*a54*a65 - a11*a26*a32*a43*a55*a64 - a11*a26 &
      *a32*a44*a53*a65 + a11*a26*a32*a44*a55*a63 + a11*a26*a32*a45*a53* &
      a64 - a11*a26*a32*a45*a54*a63 - a11*a26*a33*a42*a54*a65 + a11*a26 &
      *a33*a42*a55*a64 + a11*a26*a33*a44*a52*a65 - a11*a26*a33*a44*a55* &
      a62 - a11*a26*a33*a45*a52*a64 + a11*a26*a33*a45*a54*a62 + a11*a26 &
      *a34*a42*a53*a65 - a11*a26*a34*a42*a55*a63 - a11*a26*a34*a43*a52* &
      a65 + a11*a26*a34*a43*a55*a62 + a11*a26*a34*a45*a52*a63 - a11*a26 &
      *a34*a45*a53*a62 - a11*a26*a35*a42*a53*a64 + a11*a26*a35*a42*a54* &
      a63 + a11*a26*a35*a43*a52*a64 - a11*a26*a35*a43*a54*a62 - a11*a26 &
      *a35*a44*a52*a63 + a11*a26*a35*a44*a53*a62 - a12*a21*a33*a44*a55* &
      a66 + a12*a21*a33*a44*a56*a65 + a12*a21*a33*a45*a54*a66 - a12*a21 &
      *a33*a45*a56*a64 - a12*a21*a33*a46*a54*a65 + a12*a21*a33*a46*a55* &
      a64 + a12*a21*a34*a43*a55*a66 - a12*a21*a34*a43*a56*a65 - a12*a21 &
      *a34*a45*a53*a66 + a12*a21*a34*a45*a56*a63 + a12*a21*a34*a46*a53* &
      a65 - a12*a21*a34*a46*a55*a63 - a12*a21*a35*a43*a54*a66 + a12*a21 &
      *a35*a43*a56*a64 + a12*a21*a35*a44*a53*a66 - a12*a21*a35*a44*a56* &
      a63 - a12*a21*a35*a46*a53*a64 + a12*a21*a35*a46*a54*a63 + a12*a21 &
      *a36*a43*a54*a65 - a12*a21*a36*a43*a55*a64 - a12*a21*a36*a44*a53* &
      a65 + a12*a21*a36*a44*a55*a63 + a12*a21*a36*a45*a53*a64 - a12*a21 &
      *a36*a45*a54*a63 + a12*a23*a31*a44*a55*a66 - a12*a23*a31*a44*a56* &
      a65 - a12*a23*a31*a45*a54*a66 + a12*a23*a31*a45*a56*a64 + a12*a23 &
      *a31*a46*a54*a65 - a12*a23*a31*a46*a55*a64 - a12*a23*a34*a41*a55* &
      a66 + a12*a23*a34*a41*a56*a65 + a12*a23*a34*a45*a51*a66 - a12*a23 &
      *a34*a45*a56*a61 - a12*a23*a34*a46*a51*a65 + a12*a23*a34*a46*a55* &
      a61 + a12*a23*a35*a41*a54*a66 - a12*a23*a35*a41*a56*a64 - a12*a23 &
      *a35*a44*a51*a66 + a12*a23*a35*a44*a56*a61 + a12*a23*a35*a46*a51* &
      a64 - a12*a23*a35*a46*a54*a61 - a12*a23*a36*a41*a54*a65 + a12*a23 &
      *a36*a41*a55*a64 + a12*a23*a36*a44*a51*a65 - a12*a23*a36*a44*a55* &
      a61 - a12*a23*a36*a45*a51*a64 + a12*a23*a36*a45*a54*a61 - a12*a24 &
      *a31*a43*a55*a66 + a12*a24*a31*a43*a56*a65 + a12*a24*a31*a45*a53* &
      a66 - a12*a24*a31*a45*a56*a63 - a12*a24*a31*a46*a53*a65 + a12*a24 &
      *a31*a46*a55*a63 + a12*a24*a33*a41*a55*a66 - a12*a24*a33*a41*a56* &
      a65 - a12*a24*a33*a45*a51*a66 + a12*a24*a33*a45*a56*a61 + a12*a24 &
      *a33*a46*a51*a65 - a12*a24*a33*a46*a55*a61 - a12*a24*a35*a41*a53* &
      a66 + a12*a24*a35*a41*a56*a63 + a12*a24*a35*a43*a51*a66 - a12*a24 &
      *a35*a43*a56*a61 - a12*a24*a35*a46*a51*a63 + a12*a24*a35*a46*a53* &
      a61 + a12*a24*a36*a41*a53*a65 - a12*a24*a36*a41*a55*a63 - a12*a24 &
      *a36*a43*a51*a65 + a12*a24*a36*a43*a55*a61 + a12*a24*a36*a45*a51* &
      a63 - a12*a24*a36*a45*a53*a61 + a12*a25*a31*a43*a54*a66 - a12*a25 &
      *a31*a43*a56*a64 - a12*a25*a31*a44*a53*a66 + a12*a25*a31*a44*a56* &
      a63 + a12*a25*a31*a46*a53*a64 - a12*a25*a31*a46*a54*a63 - a12*a25 &
      *a33*a41*a54*a66 + a12*a25*a33*a41*a56*a64 + a12*a25*a33*a44*a51* &
      a66 - a12*a25*a33*a44*a56*a61 - a12*a25*a33*a46*a51*a64 + a12*a25 &
      *a33*a46*a54*a61 + a12*a25*a34*a41*a53*a66 - a12*a25*a34*a41*a56* &
      a63 - a12*a25*a34*a43*a51*a66 + a12*a25*a34*a43*a56*a61 + a12*a25 &
      *a34*a46*a51*a63 - a12*a25*a34*a46*a53*a61 - a12*a25*a36*a41*a53* &
      a64 + a12*a25*a36*a41*a54*a63 + a12*a25*a36*a43*a51*a64 - a12*a25 &
      *a36*a43*a54*a61 - a12*a25*a36*a44*a51*a63 + a12*a25*a36*a44*a53* &
      a61 - a12*a26*a31*a43*a54*a65 + a12*a26*a31*a43*a55*a64 + a12*a26 &
      *a31*a44*a53*a65 - a12*a26*a31*a44*a55*a63 - a12*a26*a31*a45*a53* &
      a64 + a12*a26*a31*a45*a54*a63 + a12*a26*a33*a41*a54*a65 - a12*a26 &
      *a33*a41*a55*a64 - a12*a26*a33*a44*a51*a65 + a12*a26*a33*a44*a55* &
      a61 + a12*a26*a33*a45*a51*a64 - a12*a26*a33*a45*a54*a61 - a12*a26 &
      *a34*a41*a53*a65 + a12*a26*a34*a41*a55*a63 + a12*a26*a34*a43*a51* &
      a65 - a12*a26*a34*a43*a55*a61 - a12*a26*a34*a45*a51*a63 + a12*a26 &
      *a34*a45*a53*a61 + a12*a26*a35*a41*a53*a64 - a12*a26*a35*a41*a54* &
      a63 - a12*a26*a35*a43*a51*a64 + a12*a26*a35*a43*a54*a61 + a12*a26 &
      *a35*a44*a51*a63 - a12*a26*a35*a44*a53*a61 + a13*a21*a32*a44*a55* &
      a66 - a13*a21*a32*a44*a56*a65 - a13*a21*a32*a45*a54*a66 + a13*a21 &
      *a32*a45*a56*a64 + a13*a21*a32*a46*a54*a65 - a13*a21*a32*a46*a55* &
      a64 - a13*a21*a34*a42*a55*a66 + a13*a21*a34*a42*a56*a65 + a13*a21 &
      *a34*a45*a52*a66 - a13*a21*a34*a45*a56*a62 - a13*a21*a34*a46*a52* &
      a65 + a13*a21*a34*a46*a55*a62 + a13*a21*a35*a42*a54*a66 - a13*a21 &
      *a35*a42*a56*a64 - a13*a21*a35*a44*a52*a66 + a13*a21*a35*a44*a56* &
      a62 + a13*a21*a35*a46*a52*a64 - a13*a21*a35*a46*a54*a62 - a13*a21 &
      *a36*a42*a54*a65 + a13*a21*a36*a42*a55*a64 + a13*a21*a36*a44*a52* &
      a65 - a13*a21*a36*a44*a55*a62 - a13*a21*a36*a45*a52*a64 + a13*a21 &
      *a36*a45*a54*a62 - a13*a22*a31*a44*a55*a66 + a13*a22*a31*a44*a56* &
      a65 + a13*a22*a31*a45*a54*a66 - a13*a22*a31*a45*a56*a64 - a13*a22 &
      *a31*a46*a54*a65 + a13*a22*a31*a46*a55*a64 + a13*a22*a34*a41*a55* &
      a66 - a13*a22*a34*a41*a56*a65 - a13*a22*a34*a45*a51*a66 + a13*a22 &
      *a34*a45*a56*a61 + a13*a22*a34*a46*a51*a65 - a13*a22*a34*a46*a55* &
      a61 - a13*a22*a35*a41*a54*a66 + a13*a22*a35*a41*a56*a64 + a13*a22 &
      *a35*a44*a51*a66 - a13*a22*a35*a44*a56*a61 - a13*a22*a35*a46*a51* &
      a64 + a13*a22*a35*a46*a54*a61 + a13*a22*a36*a41*a54*a65 - a13*a22 &
      *a36*a41*a55*a64 - a13*a22*a36*a44*a51*a65 + a13*a22*a36*a44*a55* &
      a61 + a13*a22*a36*a45*a51*a64 - a13*a22*a36*a45*a54*a61 + a13*a24 &
      *a31*a42*a55*a66 - a13*a24*a31*a42*a56*a65 - a13*a24*a31*a45*a52* &
      a66 + a13*a24*a31*a45*a56*a62 + a13*a24*a31*a46*a52*a65 - a13*a24 &
      *a31*a46*a55*a62 - a13*a24*a32*a41*a55*a66 + a13*a24*a32*a41*a56* &
      a65 + a13*a24*a32*a45*a51*a66 - a13*a24*a32*a45*a56*a61 - a13*a24 &
      *a32*a46*a51*a65 + a13*a24*a32*a46*a55*a61 + a13*a24*a35*a41*a52* &
      a66 - a13*a24*a35*a41*a56*a62 - a13*a24*a35*a42*a51*a66 + a13*a24 &
      *a35*a42*a56*a61 + a13*a24*a35*a46*a51*a62 - a13*a24*a35*a46*a52* &
      a61 - a13*a24*a36*a41*a52*a65 + a13*a24*a36*a41*a55*a62 + a13*a24 &
      *a36*a42*a51*a65 - a13*a24*a36*a42*a55*a61 - a13*a24*a36*a45*a51* &
      a62 + a13*a24*a36*a45*a52*a61 - a13*a25*a31*a42*a54*a66 + a13*a25 &
      *a31*a42*a56*a64 + a13*a25*a31*a44*a52*a66 - a13*a25*a31*a44*a56* &
      a62 - a13*a25*a31*a46*a52*a64 + a13*a25*a31*a46*a54*a62 + a13*a25 &
      *a32*a41*a54*a66 - a13*a25*a32*a41*a56*a64 - a13*a25*a32*a44*a51* &
      a66 + a13*a25*a32*a44*a56*a61 + a13*a25*a32*a46*a51*a64 - a13*a25 &
      *a32*a46*a54*a61 - a13*a25*a34*a41*a52*a66 + a13*a25*a34*a41*a56* &
      a62 + a13*a25*a34*a42*a51*a66 - a13*a25*a34*a42*a56*a61 - a13*a25 &
      *a34*a46*a51*a62 + a13*a25*a34*a46*a52*a61 + a13*a25*a36*a41*a52* &
      a64 - a13*a25*a36*a41*a54*a62 - a13*a25*a36*a42*a51*a64 + a13*a25 &
      *a36*a42*a54*a61 + a13*a25*a36*a44*a51*a62 - a13*a25*a36*a44*a52* &
      a61 + a13*a26*a31*a42*a54*a65 - a13*a26*a31*a42*a55*a64 - a13*a26 &
      *a31*a44*a52*a65 + a13*a26*a31*a44*a55*a62 + a13*a26*a31*a45*a52* &
      a64 - a13*a26*a31*a45*a54*a62 - a13*a26*a32*a41*a54*a65 + a13*a26 &
      *a32*a41*a55*a64 + a13*a26*a32*a44*a51*a65 - a13*a26*a32*a44*a55* &
      a61 - a13*a26*a32*a45*a51*a64 + a13*a26*a32*a45*a54*a61 + a13*a26 &
      *a34*a41*a52*a65 - a13*a26*a34*a41*a55*a62 - a13*a26*a34*a42*a51* &
      a65 + a13*a26*a34*a42*a55*a61 + a13*a26*a34*a45*a51*a62 - a13*a26 &
      *a34*a45*a52*a61 - a13*a26*a35*a41*a52*a64 + a13*a26*a35*a41*a54* &
      a62 + a13*a26*a35*a42*a51*a64 - a13*a26*a35*a42*a54*a61 - a13*a26 &
      *a35*a44*a51*a62 + a13*a26*a35*a44*a52*a61 - a14*a21*a32*a43*a55* &
      a66 + a14*a21*a32*a43*a56*a65 + a14*a21*a32*a45*a53*a66 - a14*a21 &
      *a32*a45*a56*a63 - a14*a21*a32*a46*a53*a65 + a14*a21*a32*a46*a55* &
      a63 + a14*a21*a33*a42*a55*a66 - a14*a21*a33*a42*a56*a65 - a14*a21 &
      *a33*a45*a52*a66 + a14*a21*a33*a45*a56*a62 + a14*a21*a33*a46*a52* &
      a65 - a14*a21*a33*a46*a55*a62 - a14*a21*a35*a42*a53*a66 + a14*a21 &
      *a35*a42*a56*a63 + a14*a21*a35*a43*a52*a66 - a14*a21*a35*a43*a56* &
      a62 - a14*a21*a35*a46*a52*a63 + a14*a21*a35*a46*a53*a62 + a14*a21 &
      *a36*a42*a53*a65 - a14*a21*a36*a42*a55*a63 - a14*a21*a36*a43*a52* &
      a65 + a14*a21*a36*a43*a55*a62 + a14*a21*a36*a45*a52*a63 - a14*a21 &
      *a36*a45*a53*a62 + a14*a22*a31*a43*a55*a66 - a14*a22*a31*a43*a56* &
      a65 - a14*a22*a31*a45*a53*a66 + a14*a22*a31*a45*a56*a63 + a14*a22 &
      *a31*a46*a53*a65 - a14*a22*a31*a46*a55*a63 - a14*a22*a33*a41*a55* &
      a66 + a14*a22*a33*a41*a56*a65 + a14*a22*a33*a45*a51*a66 - a14*a22 &
      *a33*a45*a56*a61 - a14*a22*a33*a46*a51*a65 + a14*a22*a33*a46*a55* &
      a61 + a14*a22*a35*a41*a53*a66 - a14*a22*a35*a41*a56*a63 - a14*a22 &
      *a35*a43*a51*a66 + a14*a22*a35*a43*a56*a61 + a14*a22*a35*a46*a51* &
      a63 - a14*a22*a35*a46*a53*a61 - a14*a22*a36*a41*a53*a65 + a14*a22 &
      *a36*a41*a55*a63 + a14*a22*a36*a43*a51*a65 - a14*a22*a36*a43*a55* &
      a61 - a14*a22*a36*a45*a51*a63 + a14*a22*a36*a45*a53*a61 - a14*a23 &
      *a31*a42*a55*a66 + a14*a23*a31*a42*a56*a65 + a14*a23*a31*a45*a52* &
      a66 - a14*a23*a31*a45*a56*a62 - a14*a23*a31*a46*a52*a65 + a14*a23 &
      *a31*a46*a55*a62 + a14*a23*a32*a41*a55*a66 - a14*a23*a32*a41*a56* &
      a65 - a14*a23*a32*a45*a51*a66 + a14*a23*a32*a45*a56*a61 + a14*a23 &
      *a32*a46*a51*a65 - a14*a23*a32*a46*a55*a61 - a14*a23*a35*a41*a52* &
      a66 + a14*a23*a35*a41*a56*a62 + a14*a23*a35*a42*a51*a66 - a14*a23 &
      *a35*a42*a56*a61 - a14*a23*a35*a46*a51*a62 + a14*a23*a35*a46*a52* &
      a61 + a14*a23*a36*a41*a52*a65 - a14*a23*a36*a41*a55*a62 - a14*a23 &
      *a36*a42*a51*a65 + a14*a23*a36*a42*a55*a61 + a14*a23*a36*a45*a51* &
      a62 - a14*a23*a36*a45*a52*a61 + a14*a25*a31*a42*a53*a66 - a14*a25 &
      *a31*a42*a56*a63 - a14*a25*a31*a43*a52*a66 + a14*a25*a31*a43*a56* &
      a62 + a14*a25*a31*a46*a52*a63 - a14*a25*a31*a46*a53*a62 - a14*a25 &
      *a32*a41*a53*a66 + a14*a25*a32*a41*a56*a63 + a14*a25*a32*a43*a51* &
      a66 - a14*a25*a32*a43*a56*a61 - a14*a25*a32*a46*a51*a63 + a14*a25 &
      *a32*a46*a53*a61 + a14*a25*a33*a41*a52*a66 - a14*a25*a33*a41*a56* &
      a62 - a14*a25*a33*a42*a51*a66 + a14*a25*a33*a42*a56*a61 + a14*a25 &
      *a33*a46*a51*a62 - a14*a25*a33*a46*a52*a61 - a14*a25*a36*a41*a52* &
      a63 + a14*a25*a36*a41*a53*a62 + a14*a25*a36*a42*a51*a63 - a14*a25 &
      *a36*a42*a53*a61 - a14*a25*a36*a43*a51*a62 + a14*a25*a36*a43*a52* &
      a61 - a14*a26*a31*a42*a53*a65 + a14*a26*a31*a42*a55*a63 + a14*a26 &
      *a31*a43*a52*a65 - a14*a26*a31*a43*a55*a62 - a14*a26*a31*a45*a52* &
      a63 + a14*a26*a31*a45*a53*a62 + a14*a26*a32*a41*a53*a65 - a14*a26 &
      *a32*a41*a55*a63 - a14*a26*a32*a43*a51*a65 + a14*a26*a32*a43*a55* &
      a61 + a14*a26*a32*a45*a51*a63 - a14*a26*a32*a45*a53*a61 - a14*a26 &
      *a33*a41*a52*a65 + a14*a26*a33*a41*a55*a62 + a14*a26*a33*a42*a51* &
      a65 - a14*a26*a33*a42*a55*a61 - a14*a26*a33*a45*a51*a62 + a14*a26 &
      *a33*a45*a52*a61 + a14*a26*a35*a41*a52*a63 - a14*a26*a35*a41*a53* &
      a62 - a14*a26*a35*a42*a51*a63 + a14*a26*a35*a42*a53*a61 + a14*a26 &
      *a35*a43*a51*a62 - a14*a26*a35*a43*a52*a61 + a15*a21*a32*a43*a54* &
      a66 - a15*a21*a32*a43*a56*a64 - a15*a21*a32*a44*a53*a66 + a15*a21 &
      *a32*a44*a56*a63 + a15*a21*a32*a46*a53*a64 - a15*a21*a32*a46*a54* &
      a63 - a15*a21*a33*a42*a54*a66 + a15*a21*a33*a42*a56*a64 + a15*a21 &
      *a33*a44*a52*a66 - a15*a21*a33*a44*a56*a62 - a15*a21*a33*a46*a52* &
      a64 + a15*a21*a33*a46*a54*a62 + a15*a21*a34*a42*a53*a66 - a15*a21 &
      *a34*a42*a56*a63 - a15*a21*a34*a43*a52*a66 + a15*a21*a34*a43*a56* &
      a62 + a15*a21*a34*a46*a52*a63 - a15*a21*a34*a46*a53*a62 - a15*a21 &
      *a36*a42*a53*a64 + a15*a21*a36*a42*a54*a63 + a15*a21*a36*a43*a52* &
      a64 - a15*a21*a36*a43*a54*a62 - a15*a21*a36*a44*a52*a63 + a15*a21 &
      *a36*a44*a53*a62 - a15*a22*a31*a43*a54*a66 + a15*a22*a31*a43*a56* &
      a64 + a15*a22*a31*a44*a53*a66 - a15*a22*a31*a44*a56*a63 - a15*a22 &
      *a31*a46*a53*a64 + a15*a22*a31*a46*a54*a63 + a15*a22*a33*a41*a54* &
      a66 - a15*a22*a33*a41*a56*a64 - a15*a22*a33*a44*a51*a66 + a15*a22 &
      *a33*a44*a56*a61 + a15*a22*a33*a46*a51*a64 - a15*a22*a33*a46*a54* &
      a61 - a15*a22*a34*a41*a53*a66 + a15*a22*a34*a41*a56*a63 + a15*a22 &
      *a34*a43*a51*a66 - a15*a22*a34*a43*a56*a61 - a15*a22*a34*a46*a51* &
      a63 + a15*a22*a34*a46*a53*a61 + a15*a22*a36*a41*a53*a64 - a15*a22 &
      *a36*a41*a54*a63 - a15*a22*a36*a43*a51*a64 + a15*a22*a36*a43*a54* &
      a61 + a15*a22*a36*a44*a51*a63 - a15*a22*a36*a44*a53*a61 + a15*a23 &
      *a31*a42*a54*a66 - a15*a23*a31*a42*a56*a64 - a15*a23*a31*a44*a52* &
      a66 + a15*a23*a31*a44*a56*a62 + a15*a23*a31*a46*a52*a64 - a15*a23 &
      *a31*a46*a54*a62 - a15*a23*a32*a41*a54*a66 + a15*a23*a32*a41*a56* &
      a64 + a15*a23*a32*a44*a51*a66 - a15*a23*a32*a44*a56*a61 - a15*a23 &
      *a32*a46*a51*a64 + a15*a23*a32*a46*a54*a61 + a15*a23*a34*a41*a52* &
      a66 - a15*a23*a34*a41*a56*a62 - a15*a23*a34*a42*a51*a66 + a15*a23 &
      *a34*a42*a56*a61 + a15*a23*a34*a46*a51*a62 - a15*a23*a34*a46*a52* &
      a61 - a15*a23*a36*a41*a52*a64 + a15*a23*a36*a41*a54*a62 + a15*a23 &
      *a36*a42*a51*a64 - a15*a23*a36*a42*a54*a61 - a15*a23*a36*a44*a51* &
      a62 + a15*a23*a36*a44*a52*a61 - a15*a24*a31*a42*a53*a66 + a15*a24 &
      *a31*a42*a56*a63 + a15*a24*a31*a43*a52*a66 - a15*a24*a31*a43*a56* &
      a62 - a15*a24*a31*a46*a52*a63 + a15*a24*a31*a46*a53*a62 + a15*a24 &
      *a32*a41*a53*a66 - a15*a24*a32*a41*a56*a63 - a15*a24*a32*a43*a51* &
      a66 + a15*a24*a32*a43*a56*a61 + a15*a24*a32*a46*a51*a63 - a15*a24 &
      *a32*a46*a53*a61 - a15*a24*a33*a41*a52*a66 + a15*a24*a33*a41*a56* &
      a62 + a15*a24*a33*a42*a51*a66 - a15*a24*a33*a42*a56*a61 - a15*a24 &
      *a33*a46*a51*a62 + a15*a24*a33*a46*a52*a61 + a15*a24*a36*a41*a52* &
      a63 - a15*a24*a36*a41*a53*a62 - a15*a24*a36*a42*a51*a63 + a15*a24 &
      *a36*a42*a53*a61 + a15*a24*a36*a43*a51*a62 - a15*a24*a36*a43*a52* &
      a61 + a15*a26*a31*a42*a53*a64 - a15*a26*a31*a42*a54*a63 - a15*a26 &
      *a31*a43*a52*a64 + a15*a26*a31*a43*a54*a62 + a15*a26*a31*a44*a52* &
      a63 - a15*a26*a31*a44*a53*a62 - a15*a26*a32*a41*a53*a64 + a15*a26 &
      *a32*a41*a54*a63 + a15*a26*a32*a43*a51*a64 - a15*a26*a32*a43*a54* &
      a61 - a15*a26*a32*a44*a51*a63 + a15*a26*a32*a44*a53*a61 + a15*a26 &
      *a33*a41*a52*a64 - a15*a26*a33*a41*a54*a62 - a15*a26*a33*a42*a51* &
      a64 + a15*a26*a33*a42*a54*a61 + a15*a26*a33*a44*a51*a62 - a15*a26 &
      *a33*a44*a52*a61 - a15*a26*a34*a41*a52*a63 + a15*a26*a34*a41*a53* &
      a62 + a15*a26*a34*a42*a51*a63 - a15*a26*a34*a42*a53*a61 - a15*a26 &
      *a34*a43*a51*a62 + a15*a26*a34*a43*a52*a61 - a16*a21*a32*a43*a54* &
      a65 + a16*a21*a32*a43*a55*a64 + a16*a21*a32*a44*a53*a65 - a16*a21 &
      *a32*a44*a55*a63 - a16*a21*a32*a45*a53*a64 + a16*a21*a32*a45*a54* &
      a63 + a16*a21*a33*a42*a54*a65 - a16*a21*a33*a42*a55*a64 - a16*a21 &
      *a33*a44*a52*a65 + a16*a21*a33*a44*a55*a62 + a16*a21*a33*a45*a52* &
      a64 - a16*a21*a33*a45*a54*a62 - a16*a21*a34*a42*a53*a65 + a16*a21 &
      *a34*a42*a55*a63 + a16*a21*a34*a43*a52*a65 - a16*a21*a34*a43*a55* &
      a62 - a16*a21*a34*a45*a52*a63 + a16*a21*a34*a45*a53*a62 + a16*a21 &
      *a35*a42*a53*a64 - a16*a21*a35*a42*a54*a63 - a16*a21*a35*a43*a52* &
      a64 + a16*a21*a35*a43*a54*a62 + a16*a21*a35*a44*a52*a63 - a16*a21 &
      *a35*a44*a53*a62 + a16*a22*a31*a43*a54*a65 - a16*a22*a31*a43*a55* &
      a64 - a16*a22*a31*a44*a53*a65 + a16*a22*a31*a44*a55*a63 + a16*a22 &
      *a31*a45*a53*a64 - a16*a22*a31*a45*a54*a63 - a16*a22*a33*a41*a54* &
      a65 + a16*a22*a33*a41*a55*a64 + a16*a22*a33*a44*a51*a65 - a16*a22 &
      *a33*a44*a55*a61 - a16*a22*a33*a45*a51*a64 + a16*a22*a33*a45*a54* &
      a61 + a16*a22*a34*a41*a53*a65 - a16*a22*a34*a41*a55*a63 - a16*a22 &
      *a34*a43*a51*a65 + a16*a22*a34*a43*a55*a61 + a16*a22*a34*a45*a51* &
      a63 - a16*a22*a34*a45*a53*a61 - a16*a22*a35*a41*a53*a64 + a16*a22 &
      *a35*a41*a54*a63 + a16*a22*a35*a43*a51*a64 - a16*a22*a35*a43*a54* &
      a61 - a16*a22*a35*a44*a51*a63 + a16*a22*a35*a44*a53*a61 - a16*a23 &
      *a31*a42*a54*a65 + a16*a23*a31*a42*a55*a64 + a16*a23*a31*a44*a52* &
      a65 - a16*a23*a31*a44*a55*a62 - a16*a23*a31*a45*a52*a64 + a16*a23 &
      *a31*a45*a54*a62 + a16*a23*a32*a41*a54*a65 - a16*a23*a32*a41*a55* &
      a64 - a16*a23*a32*a44*a51*a65 + a16*a23*a32*a44*a55*a61 + a16*a23 &
      *a32*a45*a51*a64 - a16*a23*a32*a45*a54*a61 - a16*a23*a34*a41*a52* &
      a65 + a16*a23*a34*a41*a55*a62 + a16*a23*a34*a42*a51*a65 - a16*a23 &
      *a34*a42*a55*a61 - a16*a23*a34*a45*a51*a62 + a16*a23*a34*a45*a52* &
      a61 + a16*a23*a35*a41*a52*a64 - a16*a23*a35*a41*a54*a62 - a16*a23 &
      *a35*a42*a51*a64 + a16*a23*a35*a42*a54*a61 + a16*a23*a35*a44*a51* &
      a62 - a16*a23*a35*a44*a52*a61 + a16*a24*a31*a42*a53*a65 - a16*a24 &
      *a31*a42*a55*a63 - a16*a24*a31*a43*a52*a65 + a16*a24*a31*a43*a55* &
      a62 + a16*a24*a31*a45*a52*a63 - a16*a24*a31*a45*a53*a62 - a16*a24 &
      *a32*a41*a53*a65 + a16*a24*a32*a41*a55*a63 + a16*a24*a32*a43*a51* &
      a65 - a16*a24*a32*a43*a55*a61 - a16*a24*a32*a45*a51*a63 + a16*a24 &
      *a32*a45*a53*a61 + a16*a24*a33*a41*a52*a65 - a16*a24*a33*a41*a55* &
      a62 - a16*a24*a33*a42*a51*a65 + a16*a24*a33*a42*a55*a61 + a16*a24 &
      *a33*a45*a51*a62 - a16*a24*a33*a45*a52*a61 - a16*a24*a35*a41*a52* &
      a63 + a16*a24*a35*a41*a53*a62 + a16*a24*a35*a42*a51*a63 - a16*a24 &
      *a35*a42*a53*a61 - a16*a24*a35*a43*a51*a62 + a16*a24*a35*a43*a52* &
      a61 - a16*a25*a31*a42*a53*a64 + a16*a25*a31*a42*a54*a63 + a16*a25 &
      *a31*a43*a52*a64 - a16*a25*a31*a43*a54*a62 - a16*a25*a31*a44*a52* &
      a63 + a16*a25*a31*a44*a53*a62 + a16*a25*a32*a41*a53*a64 - a16*a25 &
      *a32*a41*a54*a63 - a16*a25*a32*a43*a51*a64 + a16*a25*a32*a43*a54* &
      a61 + a16*a25*a32*a44*a51*a63 - a16*a25*a32*a44*a53*a61 - a16*a25 &
      *a33*a41*a52*a64 + a16*a25*a33*a41*a54*a62 + a16*a25*a33*a42*a51* &
      a64 - a16*a25*a33*a42*a54*a61 - a16*a25*a33*a44*a51*a62 + a16*a25 &
      *a33*a44*a52*a61 + a16*a25*a34*a41*a52*a63 - a16*a25*a34*a41*a53* &
      a62 - a16*a25*a34*a42*a51*a63 + a16*a25*a34*a42*a53*a61 + a16*a25 &
      *a34*a43*a51*a62 - a16*a25*a34*a43*a52*a61)
    
    end function

    pure function inv_array6(A, d_known) result(A_inv)
    real(real64), intent(in) :: A(6,6)    
    real(real64), intent(in), optional :: d_known
    real(real64) :: A_inv(6,6)

    real(real64) :: a11, a12, a13, a14, a15, a16
    real(real64) :: a21, a22, a23, a24, a25, a26
    real(real64) :: a31, a32, a33, a34, a35, a36
    real(real64) :: a41, a42, a43, a44, a45, a46
    real(real64) :: a51, a52, a53, a54, a55, a56
    real(real64) :: a61, a62, a63, a64, a65, a66
    real(real64) :: d, d_inv

    a11 = A(1,1)
    a21 = A(2,1)
    a31 = A(3,1)
    a41 = A(4,1)
    a51 = A(5,1)
    a61 = A(6,1)

    a12 = A(1,2)
    a22 = A(2,2)
    a32 = A(3,2)
    a42 = A(4,2)
    a52 = A(5,2)
    a62 = A(6,2)

    a13 = A(1,3)
    a23 = A(2,3)
    a33 = A(3,3)
    a43 = A(4,3)
    a53 = A(5,3)
    a63 = A(6,3)

    a14 = A(1,4)
    a24 = A(2,4)
    a34 = A(3,4)
    a44 = A(4,4)
    a54 = A(5,4)
    a64 = A(6,4)

    a15 = A(1,5)
    a25 = A(2,5)
    a35 = A(3,5)
    a45 = A(4,5)
    a55 = A(5,5)
    a65 = A(6,5)

    a16 = A(1,6)
    a26 = A(2,6)
    a36 = A(3,6)
    a46 = A(4,6)
    a56 = A(5,6)
    a66 = A(6,6)
    
    if(present(d_known)) then
        d = d_known
    else
        d = det_array6_m(A)
    end if
    if( abs(d) <= tiny ) then
        error stop "Matrix is singular."
    end if

    d_inv = 1.D+00/d

    A_inv(1, 1) = (a22*a33*a44*a55*a66 - a22*a33*a44*a56* &
      a65 - a22*a33*a45*a54*a66 + a22*a33*a45*a56*a64 + a22*a33*a46*a54 &
      *a65 - a22*a33*a46*a55*a64 - a22*a34*a43*a55*a66 + a22*a34*a43* &
      a56*a65 + a22*a34*a45*a53*a66 - a22*a34*a45*a56*a63 - a22*a34*a46 &
      *a53*a65 + a22*a34*a46*a55*a63 + a22*a35*a43*a54*a66 - a22*a35* &
      a43*a56*a64 - a22*a35*a44*a53*a66 + a22*a35*a44*a56*a63 + a22*a35 &
      *a46*a53*a64 - a22*a35*a46*a54*a63 - a22*a36*a43*a54*a65 + a22* &
      a36*a43*a55*a64 + a22*a36*a44*a53*a65 - a22*a36*a44*a55*a63 - a22 &
      *a36*a45*a53*a64 + a22*a36*a45*a54*a63 - a23*a32*a44*a55*a66 + &
      a23*a32*a44*a56*a65 + a23*a32*a45*a54*a66 - a23*a32*a45*a56*a64 - &
      a23*a32*a46*a54*a65 + a23*a32*a46*a55*a64 + a23*a34*a42*a55*a66 - &
      a23*a34*a42*a56*a65 - a23*a34*a45*a52*a66 + a23*a34*a45*a56*a62 + &
      a23*a34*a46*a52*a65 - a23*a34*a46*a55*a62 - a23*a35*a42*a54*a66 + &
      a23*a35*a42*a56*a64 + a23*a35*a44*a52*a66 - a23*a35*a44*a56*a62 - &
      a23*a35*a46*a52*a64 + a23*a35*a46*a54*a62 + a23*a36*a42*a54*a65 - &
      a23*a36*a42*a55*a64 - a23*a36*a44*a52*a65 + a23*a36*a44*a55*a62 + &
      a23*a36*a45*a52*a64 - a23*a36*a45*a54*a62 + a24*a32*a43*a55*a66 - &
      a24*a32*a43*a56*a65 - a24*a32*a45*a53*a66 + a24*a32*a45*a56*a63 + &
      a24*a32*a46*a53*a65 - a24*a32*a46*a55*a63 - a24*a33*a42*a55*a66 + &
      a24*a33*a42*a56*a65 + a24*a33*a45*a52*a66 - a24*a33*a45*a56*a62 - &
      a24*a33*a46*a52*a65 + a24*a33*a46*a55*a62 + a24*a35*a42*a53*a66 - &
      a24*a35*a42*a56*a63 - a24*a35*a43*a52*a66 + a24*a35*a43*a56*a62 + &
      a24*a35*a46*a52*a63 - a24*a35*a46*a53*a62 - a24*a36*a42*a53*a65 + &
      a24*a36*a42*a55*a63 + a24*a36*a43*a52*a65 - a24*a36*a43*a55*a62 - &
      a24*a36*a45*a52*a63 + a24*a36*a45*a53*a62 - a25*a32*a43*a54*a66 + &
      a25*a32*a43*a56*a64 + a25*a32*a44*a53*a66 - a25*a32*a44*a56*a63 - &
      a25*a32*a46*a53*a64 + a25*a32*a46*a54*a63 + a25*a33*a42*a54*a66 - &
      a25*a33*a42*a56*a64 - a25*a33*a44*a52*a66 + a25*a33*a44*a56*a62 + &
      a25*a33*a46*a52*a64 - a25*a33*a46*a54*a62 - a25*a34*a42*a53*a66 + &
      a25*a34*a42*a56*a63 + a25*a34*a43*a52*a66 - a25*a34*a43*a56*a62 - &
      a25*a34*a46*a52*a63 + a25*a34*a46*a53*a62 + a25*a36*a42*a53*a64 - &
      a25*a36*a42*a54*a63 - a25*a36*a43*a52*a64 + a25*a36*a43*a54*a62 + &
      a25*a36*a44*a52*a63 - a25*a36*a44*a53*a62 + a26*a32*a43*a54*a65 - &
      a26*a32*a43*a55*a64 - a26*a32*a44*a53*a65 + a26*a32*a44*a55*a63 + &
      a26*a32*a45*a53*a64 - a26*a32*a45*a54*a63 - a26*a33*a42*a54*a65 + &
      a26*a33*a42*a55*a64 + a26*a33*a44*a52*a65 - a26*a33*a44*a55*a62 - &
      a26*a33*a45*a52*a64 + a26*a33*a45*a54*a62 + a26*a34*a42*a53*a65 - &
      a26*a34*a42*a55*a63 - a26*a34*a43*a52*a65 + a26*a34*a43*a55*a62 + &
      a26*a34*a45*a52*a63 - a26*a34*a45*a53*a62 - a26*a35*a42*a53*a64 + &
      a26*a35*a42*a54*a63 + a26*a35*a43*a52*a64 - a26*a35*a43*a54*a62 - &
      a26*a35*a44*a52*a63 + a26*a35*a44*a53*a62)*d_inv

    A_inv(2, 1) = (-a21*a33*a44*a55*a66 + a21*a33*a44*a56* &
      a65 + a21*a33*a45*a54*a66 - a21*a33*a45*a56*a64 - a21*a33*a46*a54 &
      *a65 + a21*a33*a46*a55*a64 + a21*a34*a43*a55*a66 - a21*a34*a43* &
      a56*a65 - a21*a34*a45*a53*a66 + a21*a34*a45*a56*a63 + a21*a34*a46 &
      *a53*a65 - a21*a34*a46*a55*a63 - a21*a35*a43*a54*a66 + a21*a35* &
      a43*a56*a64 + a21*a35*a44*a53*a66 - a21*a35*a44*a56*a63 - a21*a35 &
      *a46*a53*a64 + a21*a35*a46*a54*a63 + a21*a36*a43*a54*a65 - a21* &
      a36*a43*a55*a64 - a21*a36*a44*a53*a65 + a21*a36*a44*a55*a63 + a21 &
      *a36*a45*a53*a64 - a21*a36*a45*a54*a63 + a23*a31*a44*a55*a66 - &
      a23*a31*a44*a56*a65 - a23*a31*a45*a54*a66 + a23*a31*a45*a56*a64 + &
      a23*a31*a46*a54*a65 - a23*a31*a46*a55*a64 - a23*a34*a41*a55*a66 + &
      a23*a34*a41*a56*a65 + a23*a34*a45*a51*a66 - a23*a34*a45*a56*a61 - &
      a23*a34*a46*a51*a65 + a23*a34*a46*a55*a61 + a23*a35*a41*a54*a66 - &
      a23*a35*a41*a56*a64 - a23*a35*a44*a51*a66 + a23*a35*a44*a56*a61 + &
      a23*a35*a46*a51*a64 - a23*a35*a46*a54*a61 - a23*a36*a41*a54*a65 + &
      a23*a36*a41*a55*a64 + a23*a36*a44*a51*a65 - a23*a36*a44*a55*a61 - &
      a23*a36*a45*a51*a64 + a23*a36*a45*a54*a61 - a24*a31*a43*a55*a66 + &
      a24*a31*a43*a56*a65 + a24*a31*a45*a53*a66 - a24*a31*a45*a56*a63 - &
      a24*a31*a46*a53*a65 + a24*a31*a46*a55*a63 + a24*a33*a41*a55*a66 - &
      a24*a33*a41*a56*a65 - a24*a33*a45*a51*a66 + a24*a33*a45*a56*a61 + &
      a24*a33*a46*a51*a65 - a24*a33*a46*a55*a61 - a24*a35*a41*a53*a66 + &
      a24*a35*a41*a56*a63 + a24*a35*a43*a51*a66 - a24*a35*a43*a56*a61 - &
      a24*a35*a46*a51*a63 + a24*a35*a46*a53*a61 + a24*a36*a41*a53*a65 - &
      a24*a36*a41*a55*a63 - a24*a36*a43*a51*a65 + a24*a36*a43*a55*a61 + &
      a24*a36*a45*a51*a63 - a24*a36*a45*a53*a61 + a25*a31*a43*a54*a66 - &
      a25*a31*a43*a56*a64 - a25*a31*a44*a53*a66 + a25*a31*a44*a56*a63 + &
      a25*a31*a46*a53*a64 - a25*a31*a46*a54*a63 - a25*a33*a41*a54*a66 + &
      a25*a33*a41*a56*a64 + a25*a33*a44*a51*a66 - a25*a33*a44*a56*a61 - &
      a25*a33*a46*a51*a64 + a25*a33*a46*a54*a61 + a25*a34*a41*a53*a66 - &
      a25*a34*a41*a56*a63 - a25*a34*a43*a51*a66 + a25*a34*a43*a56*a61 + &
      a25*a34*a46*a51*a63 - a25*a34*a46*a53*a61 - a25*a36*a41*a53*a64 + &
      a25*a36*a41*a54*a63 + a25*a36*a43*a51*a64 - a25*a36*a43*a54*a61 - &
      a25*a36*a44*a51*a63 + a25*a36*a44*a53*a61 - a26*a31*a43*a54*a65 + &
      a26*a31*a43*a55*a64 + a26*a31*a44*a53*a65 - a26*a31*a44*a55*a63 - &
      a26*a31*a45*a53*a64 + a26*a31*a45*a54*a63 + a26*a33*a41*a54*a65 - &
      a26*a33*a41*a55*a64 - a26*a33*a44*a51*a65 + a26*a33*a44*a55*a61 + &
      a26*a33*a45*a51*a64 - a26*a33*a45*a54*a61 - a26*a34*a41*a53*a65 + &
      a26*a34*a41*a55*a63 + a26*a34*a43*a51*a65 - a26*a34*a43*a55*a61 - &
      a26*a34*a45*a51*a63 + a26*a34*a45*a53*a61 + a26*a35*a41*a53*a64 - &
      a26*a35*a41*a54*a63 - a26*a35*a43*a51*a64 + a26*a35*a43*a54*a61 + &
      a26*a35*a44*a51*a63 - a26*a35*a44*a53*a61)*d_inv

    A_inv(3, 1) = (a21*a32*a44*a55*a66 - a21*a32*a44*a56* &
      a65 - a21*a32*a45*a54*a66 + a21*a32*a45*a56*a64 + a21*a32*a46*a54 &
      *a65 - a21*a32*a46*a55*a64 - a21*a34*a42*a55*a66 + a21*a34*a42* &
      a56*a65 + a21*a34*a45*a52*a66 - a21*a34*a45*a56*a62 - a21*a34*a46 &
      *a52*a65 + a21*a34*a46*a55*a62 + a21*a35*a42*a54*a66 - a21*a35* &
      a42*a56*a64 - a21*a35*a44*a52*a66 + a21*a35*a44*a56*a62 + a21*a35 &
      *a46*a52*a64 - a21*a35*a46*a54*a62 - a21*a36*a42*a54*a65 + a21* &
      a36*a42*a55*a64 + a21*a36*a44*a52*a65 - a21*a36*a44*a55*a62 - a21 &
      *a36*a45*a52*a64 + a21*a36*a45*a54*a62 - a22*a31*a44*a55*a66 + &
      a22*a31*a44*a56*a65 + a22*a31*a45*a54*a66 - a22*a31*a45*a56*a64 - &
      a22*a31*a46*a54*a65 + a22*a31*a46*a55*a64 + a22*a34*a41*a55*a66 - &
      a22*a34*a41*a56*a65 - a22*a34*a45*a51*a66 + a22*a34*a45*a56*a61 + &
      a22*a34*a46*a51*a65 - a22*a34*a46*a55*a61 - a22*a35*a41*a54*a66 + &
      a22*a35*a41*a56*a64 + a22*a35*a44*a51*a66 - a22*a35*a44*a56*a61 - &
      a22*a35*a46*a51*a64 + a22*a35*a46*a54*a61 + a22*a36*a41*a54*a65 - &
      a22*a36*a41*a55*a64 - a22*a36*a44*a51*a65 + a22*a36*a44*a55*a61 + &
      a22*a36*a45*a51*a64 - a22*a36*a45*a54*a61 + a24*a31*a42*a55*a66 - &
      a24*a31*a42*a56*a65 - a24*a31*a45*a52*a66 + a24*a31*a45*a56*a62 + &
      a24*a31*a46*a52*a65 - a24*a31*a46*a55*a62 - a24*a32*a41*a55*a66 + &
      a24*a32*a41*a56*a65 + a24*a32*a45*a51*a66 - a24*a32*a45*a56*a61 - &
      a24*a32*a46*a51*a65 + a24*a32*a46*a55*a61 + a24*a35*a41*a52*a66 - &
      a24*a35*a41*a56*a62 - a24*a35*a42*a51*a66 + a24*a35*a42*a56*a61 + &
      a24*a35*a46*a51*a62 - a24*a35*a46*a52*a61 - a24*a36*a41*a52*a65 + &
      a24*a36*a41*a55*a62 + a24*a36*a42*a51*a65 - a24*a36*a42*a55*a61 - &
      a24*a36*a45*a51*a62 + a24*a36*a45*a52*a61 - a25*a31*a42*a54*a66 + &
      a25*a31*a42*a56*a64 + a25*a31*a44*a52*a66 - a25*a31*a44*a56*a62 - &
      a25*a31*a46*a52*a64 + a25*a31*a46*a54*a62 + a25*a32*a41*a54*a66 - &
      a25*a32*a41*a56*a64 - a25*a32*a44*a51*a66 + a25*a32*a44*a56*a61 + &
      a25*a32*a46*a51*a64 - a25*a32*a46*a54*a61 - a25*a34*a41*a52*a66 + &
      a25*a34*a41*a56*a62 + a25*a34*a42*a51*a66 - a25*a34*a42*a56*a61 - &
      a25*a34*a46*a51*a62 + a25*a34*a46*a52*a61 + a25*a36*a41*a52*a64 - &
      a25*a36*a41*a54*a62 - a25*a36*a42*a51*a64 + a25*a36*a42*a54*a61 + &
      a25*a36*a44*a51*a62 - a25*a36*a44*a52*a61 + a26*a31*a42*a54*a65 - &
      a26*a31*a42*a55*a64 - a26*a31*a44*a52*a65 + a26*a31*a44*a55*a62 + &
      a26*a31*a45*a52*a64 - a26*a31*a45*a54*a62 - a26*a32*a41*a54*a65 + &
      a26*a32*a41*a55*a64 + a26*a32*a44*a51*a65 - a26*a32*a44*a55*a61 - &
      a26*a32*a45*a51*a64 + a26*a32*a45*a54*a61 + a26*a34*a41*a52*a65 - &
      a26*a34*a41*a55*a62 - a26*a34*a42*a51*a65 + a26*a34*a42*a55*a61 + &
      a26*a34*a45*a51*a62 - a26*a34*a45*a52*a61 - a26*a35*a41*a52*a64 + &
      a26*a35*a41*a54*a62 + a26*a35*a42*a51*a64 - a26*a35*a42*a54*a61 - &
      a26*a35*a44*a51*a62 + a26*a35*a44*a52*a61)*d_inv

    A_inv(4, 1) = (-a21*a32*a43*a55*a66 + a21*a32*a43*a56* &
      a65 + a21*a32*a45*a53*a66 - a21*a32*a45*a56*a63 - a21*a32*a46*a53 &
      *a65 + a21*a32*a46*a55*a63 + a21*a33*a42*a55*a66 - a21*a33*a42* &
      a56*a65 - a21*a33*a45*a52*a66 + a21*a33*a45*a56*a62 + a21*a33*a46 &
      *a52*a65 - a21*a33*a46*a55*a62 - a21*a35*a42*a53*a66 + a21*a35* &
      a42*a56*a63 + a21*a35*a43*a52*a66 - a21*a35*a43*a56*a62 - a21*a35 &
      *a46*a52*a63 + a21*a35*a46*a53*a62 + a21*a36*a42*a53*a65 - a21* &
      a36*a42*a55*a63 - a21*a36*a43*a52*a65 + a21*a36*a43*a55*a62 + a21 &
      *a36*a45*a52*a63 - a21*a36*a45*a53*a62 + a22*a31*a43*a55*a66 - &
      a22*a31*a43*a56*a65 - a22*a31*a45*a53*a66 + a22*a31*a45*a56*a63 + &
      a22*a31*a46*a53*a65 - a22*a31*a46*a55*a63 - a22*a33*a41*a55*a66 + &
      a22*a33*a41*a56*a65 + a22*a33*a45*a51*a66 - a22*a33*a45*a56*a61 - &
      a22*a33*a46*a51*a65 + a22*a33*a46*a55*a61 + a22*a35*a41*a53*a66 - &
      a22*a35*a41*a56*a63 - a22*a35*a43*a51*a66 + a22*a35*a43*a56*a61 + &
      a22*a35*a46*a51*a63 - a22*a35*a46*a53*a61 - a22*a36*a41*a53*a65 + &
      a22*a36*a41*a55*a63 + a22*a36*a43*a51*a65 - a22*a36*a43*a55*a61 - &
      a22*a36*a45*a51*a63 + a22*a36*a45*a53*a61 - a23*a31*a42*a55*a66 + &
      a23*a31*a42*a56*a65 + a23*a31*a45*a52*a66 - a23*a31*a45*a56*a62 - &
      a23*a31*a46*a52*a65 + a23*a31*a46*a55*a62 + a23*a32*a41*a55*a66 - &
      a23*a32*a41*a56*a65 - a23*a32*a45*a51*a66 + a23*a32*a45*a56*a61 + &
      a23*a32*a46*a51*a65 - a23*a32*a46*a55*a61 - a23*a35*a41*a52*a66 + &
      a23*a35*a41*a56*a62 + a23*a35*a42*a51*a66 - a23*a35*a42*a56*a61 - &
      a23*a35*a46*a51*a62 + a23*a35*a46*a52*a61 + a23*a36*a41*a52*a65 - &
      a23*a36*a41*a55*a62 - a23*a36*a42*a51*a65 + a23*a36*a42*a55*a61 + &
      a23*a36*a45*a51*a62 - a23*a36*a45*a52*a61 + a25*a31*a42*a53*a66 - &
      a25*a31*a42*a56*a63 - a25*a31*a43*a52*a66 + a25*a31*a43*a56*a62 + &
      a25*a31*a46*a52*a63 - a25*a31*a46*a53*a62 - a25*a32*a41*a53*a66 + &
      a25*a32*a41*a56*a63 + a25*a32*a43*a51*a66 - a25*a32*a43*a56*a61 - &
      a25*a32*a46*a51*a63 + a25*a32*a46*a53*a61 + a25*a33*a41*a52*a66 - &
      a25*a33*a41*a56*a62 - a25*a33*a42*a51*a66 + a25*a33*a42*a56*a61 + &
      a25*a33*a46*a51*a62 - a25*a33*a46*a52*a61 - a25*a36*a41*a52*a63 + &
      a25*a36*a41*a53*a62 + a25*a36*a42*a51*a63 - a25*a36*a42*a53*a61 - &
      a25*a36*a43*a51*a62 + a25*a36*a43*a52*a61 - a26*a31*a42*a53*a65 + &
      a26*a31*a42*a55*a63 + a26*a31*a43*a52*a65 - a26*a31*a43*a55*a62 - &
      a26*a31*a45*a52*a63 + a26*a31*a45*a53*a62 + a26*a32*a41*a53*a65 - &
      a26*a32*a41*a55*a63 - a26*a32*a43*a51*a65 + a26*a32*a43*a55*a61 + &
      a26*a32*a45*a51*a63 - a26*a32*a45*a53*a61 - a26*a33*a41*a52*a65 + &
      a26*a33*a41*a55*a62 + a26*a33*a42*a51*a65 - a26*a33*a42*a55*a61 - &
      a26*a33*a45*a51*a62 + a26*a33*a45*a52*a61 + a26*a35*a41*a52*a63 - &
      a26*a35*a41*a53*a62 - a26*a35*a42*a51*a63 + a26*a35*a42*a53*a61 + &
      a26*a35*a43*a51*a62 - a26*a35*a43*a52*a61)*d_inv

    A_inv(5, 1) = (a21*a32*a43*a54*a66 - a21*a32*a43*a56* &
      a64 - a21*a32*a44*a53*a66 + a21*a32*a44*a56*a63 + a21*a32*a46*a53 &
      *a64 - a21*a32*a46*a54*a63 - a21*a33*a42*a54*a66 + a21*a33*a42* &
      a56*a64 + a21*a33*a44*a52*a66 - a21*a33*a44*a56*a62 - a21*a33*a46 &
      *a52*a64 + a21*a33*a46*a54*a62 + a21*a34*a42*a53*a66 - a21*a34* &
      a42*a56*a63 - a21*a34*a43*a52*a66 + a21*a34*a43*a56*a62 + a21*a34 &
      *a46*a52*a63 - a21*a34*a46*a53*a62 - a21*a36*a42*a53*a64 + a21* &
      a36*a42*a54*a63 + a21*a36*a43*a52*a64 - a21*a36*a43*a54*a62 - a21 &
      *a36*a44*a52*a63 + a21*a36*a44*a53*a62 - a22*a31*a43*a54*a66 + &
      a22*a31*a43*a56*a64 + a22*a31*a44*a53*a66 - a22*a31*a44*a56*a63 - &
      a22*a31*a46*a53*a64 + a22*a31*a46*a54*a63 + a22*a33*a41*a54*a66 - &
      a22*a33*a41*a56*a64 - a22*a33*a44*a51*a66 + a22*a33*a44*a56*a61 + &
      a22*a33*a46*a51*a64 - a22*a33*a46*a54*a61 - a22*a34*a41*a53*a66 + &
      a22*a34*a41*a56*a63 + a22*a34*a43*a51*a66 - a22*a34*a43*a56*a61 - &
      a22*a34*a46*a51*a63 + a22*a34*a46*a53*a61 + a22*a36*a41*a53*a64 - &
      a22*a36*a41*a54*a63 - a22*a36*a43*a51*a64 + a22*a36*a43*a54*a61 + &
      a22*a36*a44*a51*a63 - a22*a36*a44*a53*a61 + a23*a31*a42*a54*a66 - &
      a23*a31*a42*a56*a64 - a23*a31*a44*a52*a66 + a23*a31*a44*a56*a62 + &
      a23*a31*a46*a52*a64 - a23*a31*a46*a54*a62 - a23*a32*a41*a54*a66 + &
      a23*a32*a41*a56*a64 + a23*a32*a44*a51*a66 - a23*a32*a44*a56*a61 - &
      a23*a32*a46*a51*a64 + a23*a32*a46*a54*a61 + a23*a34*a41*a52*a66 - &
      a23*a34*a41*a56*a62 - a23*a34*a42*a51*a66 + a23*a34*a42*a56*a61 + &
      a23*a34*a46*a51*a62 - a23*a34*a46*a52*a61 - a23*a36*a41*a52*a64 + &
      a23*a36*a41*a54*a62 + a23*a36*a42*a51*a64 - a23*a36*a42*a54*a61 - &
      a23*a36*a44*a51*a62 + a23*a36*a44*a52*a61 - a24*a31*a42*a53*a66 + &
      a24*a31*a42*a56*a63 + a24*a31*a43*a52*a66 - a24*a31*a43*a56*a62 - &
      a24*a31*a46*a52*a63 + a24*a31*a46*a53*a62 + a24*a32*a41*a53*a66 - &
      a24*a32*a41*a56*a63 - a24*a32*a43*a51*a66 + a24*a32*a43*a56*a61 + &
      a24*a32*a46*a51*a63 - a24*a32*a46*a53*a61 - a24*a33*a41*a52*a66 + &
      a24*a33*a41*a56*a62 + a24*a33*a42*a51*a66 - a24*a33*a42*a56*a61 - &
      a24*a33*a46*a51*a62 + a24*a33*a46*a52*a61 + a24*a36*a41*a52*a63 - &
      a24*a36*a41*a53*a62 - a24*a36*a42*a51*a63 + a24*a36*a42*a53*a61 + &
      a24*a36*a43*a51*a62 - a24*a36*a43*a52*a61 + a26*a31*a42*a53*a64 - &
      a26*a31*a42*a54*a63 - a26*a31*a43*a52*a64 + a26*a31*a43*a54*a62 + &
      a26*a31*a44*a52*a63 - a26*a31*a44*a53*a62 - a26*a32*a41*a53*a64 + &
      a26*a32*a41*a54*a63 + a26*a32*a43*a51*a64 - a26*a32*a43*a54*a61 - &
      a26*a32*a44*a51*a63 + a26*a32*a44*a53*a61 + a26*a33*a41*a52*a64 - &
      a26*a33*a41*a54*a62 - a26*a33*a42*a51*a64 + a26*a33*a42*a54*a61 + &
      a26*a33*a44*a51*a62 - a26*a33*a44*a52*a61 - a26*a34*a41*a52*a63 + &
      a26*a34*a41*a53*a62 + a26*a34*a42*a51*a63 - a26*a34*a42*a53*a61 - &
      a26*a34*a43*a51*a62 + a26*a34*a43*a52*a61)*d_inv

    A_inv(6, 1) = (-a21*a32*a43*a54*a65 + a21*a32*a43*a55* &
      a64 + a21*a32*a44*a53*a65 - a21*a32*a44*a55*a63 - a21*a32*a45*a53 &
      *a64 + a21*a32*a45*a54*a63 + a21*a33*a42*a54*a65 - a21*a33*a42* &
      a55*a64 - a21*a33*a44*a52*a65 + a21*a33*a44*a55*a62 + a21*a33*a45 &
      *a52*a64 - a21*a33*a45*a54*a62 - a21*a34*a42*a53*a65 + a21*a34* &
      a42*a55*a63 + a21*a34*a43*a52*a65 - a21*a34*a43*a55*a62 - a21*a34 &
      *a45*a52*a63 + a21*a34*a45*a53*a62 + a21*a35*a42*a53*a64 - a21* &
      a35*a42*a54*a63 - a21*a35*a43*a52*a64 + a21*a35*a43*a54*a62 + a21 &
      *a35*a44*a52*a63 - a21*a35*a44*a53*a62 + a22*a31*a43*a54*a65 - &
      a22*a31*a43*a55*a64 - a22*a31*a44*a53*a65 + a22*a31*a44*a55*a63 + &
      a22*a31*a45*a53*a64 - a22*a31*a45*a54*a63 - a22*a33*a41*a54*a65 + &
      a22*a33*a41*a55*a64 + a22*a33*a44*a51*a65 - a22*a33*a44*a55*a61 - &
      a22*a33*a45*a51*a64 + a22*a33*a45*a54*a61 + a22*a34*a41*a53*a65 - &
      a22*a34*a41*a55*a63 - a22*a34*a43*a51*a65 + a22*a34*a43*a55*a61 + &
      a22*a34*a45*a51*a63 - a22*a34*a45*a53*a61 - a22*a35*a41*a53*a64 + &
      a22*a35*a41*a54*a63 + a22*a35*a43*a51*a64 - a22*a35*a43*a54*a61 - &
      a22*a35*a44*a51*a63 + a22*a35*a44*a53*a61 - a23*a31*a42*a54*a65 + &
      a23*a31*a42*a55*a64 + a23*a31*a44*a52*a65 - a23*a31*a44*a55*a62 - &
      a23*a31*a45*a52*a64 + a23*a31*a45*a54*a62 + a23*a32*a41*a54*a65 - &
      a23*a32*a41*a55*a64 - a23*a32*a44*a51*a65 + a23*a32*a44*a55*a61 + &
      a23*a32*a45*a51*a64 - a23*a32*a45*a54*a61 - a23*a34*a41*a52*a65 + &
      a23*a34*a41*a55*a62 + a23*a34*a42*a51*a65 - a23*a34*a42*a55*a61 - &
      a23*a34*a45*a51*a62 + a23*a34*a45*a52*a61 + a23*a35*a41*a52*a64 - &
      a23*a35*a41*a54*a62 - a23*a35*a42*a51*a64 + a23*a35*a42*a54*a61 + &
      a23*a35*a44*a51*a62 - a23*a35*a44*a52*a61 + a24*a31*a42*a53*a65 - &
      a24*a31*a42*a55*a63 - a24*a31*a43*a52*a65 + a24*a31*a43*a55*a62 + &
      a24*a31*a45*a52*a63 - a24*a31*a45*a53*a62 - a24*a32*a41*a53*a65 + &
      a24*a32*a41*a55*a63 + a24*a32*a43*a51*a65 - a24*a32*a43*a55*a61 - &
      a24*a32*a45*a51*a63 + a24*a32*a45*a53*a61 + a24*a33*a41*a52*a65 - &
      a24*a33*a41*a55*a62 - a24*a33*a42*a51*a65 + a24*a33*a42*a55*a61 + &
      a24*a33*a45*a51*a62 - a24*a33*a45*a52*a61 - a24*a35*a41*a52*a63 + &
      a24*a35*a41*a53*a62 + a24*a35*a42*a51*a63 - a24*a35*a42*a53*a61 - &
      a24*a35*a43*a51*a62 + a24*a35*a43*a52*a61 - a25*a31*a42*a53*a64 + &
      a25*a31*a42*a54*a63 + a25*a31*a43*a52*a64 - a25*a31*a43*a54*a62 - &
      a25*a31*a44*a52*a63 + a25*a31*a44*a53*a62 + a25*a32*a41*a53*a64 - &
      a25*a32*a41*a54*a63 - a25*a32*a43*a51*a64 + a25*a32*a43*a54*a61 + &
      a25*a32*a44*a51*a63 - a25*a32*a44*a53*a61 - a25*a33*a41*a52*a64 + &
      a25*a33*a41*a54*a62 + a25*a33*a42*a51*a64 - a25*a33*a42*a54*a61 - &
      a25*a33*a44*a51*a62 + a25*a33*a44*a52*a61 + a25*a34*a41*a52*a63 - &
      a25*a34*a41*a53*a62 - a25*a34*a42*a51*a63 + a25*a34*a42*a53*a61 + &
      a25*a34*a43*a51*a62 - a25*a34*a43*a52*a61)*d_inv

    A_inv(1, 2) = (-a12*a33*a44*a55*a66 + a12*a33*a44*a56* &
      a65 + a12*a33*a45*a54*a66 - a12*a33*a45*a56*a64 - a12*a33*a46*a54 &
      *a65 + a12*a33*a46*a55*a64 + a12*a34*a43*a55*a66 - a12*a34*a43* &
      a56*a65 - a12*a34*a45*a53*a66 + a12*a34*a45*a56*a63 + a12*a34*a46 &
      *a53*a65 - a12*a34*a46*a55*a63 - a12*a35*a43*a54*a66 + a12*a35* &
      a43*a56*a64 + a12*a35*a44*a53*a66 - a12*a35*a44*a56*a63 - a12*a35 &
      *a46*a53*a64 + a12*a35*a46*a54*a63 + a12*a36*a43*a54*a65 - a12* &
      a36*a43*a55*a64 - a12*a36*a44*a53*a65 + a12*a36*a44*a55*a63 + a12 &
      *a36*a45*a53*a64 - a12*a36*a45*a54*a63 + a13*a32*a44*a55*a66 - &
      a13*a32*a44*a56*a65 - a13*a32*a45*a54*a66 + a13*a32*a45*a56*a64 + &
      a13*a32*a46*a54*a65 - a13*a32*a46*a55*a64 - a13*a34*a42*a55*a66 + &
      a13*a34*a42*a56*a65 + a13*a34*a45*a52*a66 - a13*a34*a45*a56*a62 - &
      a13*a34*a46*a52*a65 + a13*a34*a46*a55*a62 + a13*a35*a42*a54*a66 - &
      a13*a35*a42*a56*a64 - a13*a35*a44*a52*a66 + a13*a35*a44*a56*a62 + &
      a13*a35*a46*a52*a64 - a13*a35*a46*a54*a62 - a13*a36*a42*a54*a65 + &
      a13*a36*a42*a55*a64 + a13*a36*a44*a52*a65 - a13*a36*a44*a55*a62 - &
      a13*a36*a45*a52*a64 + a13*a36*a45*a54*a62 - a14*a32*a43*a55*a66 + &
      a14*a32*a43*a56*a65 + a14*a32*a45*a53*a66 - a14*a32*a45*a56*a63 - &
      a14*a32*a46*a53*a65 + a14*a32*a46*a55*a63 + a14*a33*a42*a55*a66 - &
      a14*a33*a42*a56*a65 - a14*a33*a45*a52*a66 + a14*a33*a45*a56*a62 + &
      a14*a33*a46*a52*a65 - a14*a33*a46*a55*a62 - a14*a35*a42*a53*a66 + &
      a14*a35*a42*a56*a63 + a14*a35*a43*a52*a66 - a14*a35*a43*a56*a62 - &
      a14*a35*a46*a52*a63 + a14*a35*a46*a53*a62 + a14*a36*a42*a53*a65 - &
      a14*a36*a42*a55*a63 - a14*a36*a43*a52*a65 + a14*a36*a43*a55*a62 + &
      a14*a36*a45*a52*a63 - a14*a36*a45*a53*a62 + a15*a32*a43*a54*a66 - &
      a15*a32*a43*a56*a64 - a15*a32*a44*a53*a66 + a15*a32*a44*a56*a63 + &
      a15*a32*a46*a53*a64 - a15*a32*a46*a54*a63 - a15*a33*a42*a54*a66 + &
      a15*a33*a42*a56*a64 + a15*a33*a44*a52*a66 - a15*a33*a44*a56*a62 - &
      a15*a33*a46*a52*a64 + a15*a33*a46*a54*a62 + a15*a34*a42*a53*a66 - &
      a15*a34*a42*a56*a63 - a15*a34*a43*a52*a66 + a15*a34*a43*a56*a62 + &
      a15*a34*a46*a52*a63 - a15*a34*a46*a53*a62 - a15*a36*a42*a53*a64 + &
      a15*a36*a42*a54*a63 + a15*a36*a43*a52*a64 - a15*a36*a43*a54*a62 - &
      a15*a36*a44*a52*a63 + a15*a36*a44*a53*a62 - a16*a32*a43*a54*a65 + &
      a16*a32*a43*a55*a64 + a16*a32*a44*a53*a65 - a16*a32*a44*a55*a63 - &
      a16*a32*a45*a53*a64 + a16*a32*a45*a54*a63 + a16*a33*a42*a54*a65 - &
      a16*a33*a42*a55*a64 - a16*a33*a44*a52*a65 + a16*a33*a44*a55*a62 + &
      a16*a33*a45*a52*a64 - a16*a33*a45*a54*a62 - a16*a34*a42*a53*a65 + &
      a16*a34*a42*a55*a63 + a16*a34*a43*a52*a65 - a16*a34*a43*a55*a62 - &
      a16*a34*a45*a52*a63 + a16*a34*a45*a53*a62 + a16*a35*a42*a53*a64 - &
      a16*a35*a42*a54*a63 - a16*a35*a43*a52*a64 + a16*a35*a43*a54*a62 + &
      a16*a35*a44*a52*a63 - a16*a35*a44*a53*a62)*d_inv

    A_inv(2, 2) = (a11*a33*a44*a55*a66 - a11*a33*a44*a56* &
      a65 - a11*a33*a45*a54*a66 + a11*a33*a45*a56*a64 + a11*a33*a46*a54 &
      *a65 - a11*a33*a46*a55*a64 - a11*a34*a43*a55*a66 + a11*a34*a43* &
      a56*a65 + a11*a34*a45*a53*a66 - a11*a34*a45*a56*a63 - a11*a34*a46 &
      *a53*a65 + a11*a34*a46*a55*a63 + a11*a35*a43*a54*a66 - a11*a35* &
      a43*a56*a64 - a11*a35*a44*a53*a66 + a11*a35*a44*a56*a63 + a11*a35 &
      *a46*a53*a64 - a11*a35*a46*a54*a63 - a11*a36*a43*a54*a65 + a11* &
      a36*a43*a55*a64 + a11*a36*a44*a53*a65 - a11*a36*a44*a55*a63 - a11 &
      *a36*a45*a53*a64 + a11*a36*a45*a54*a63 - a13*a31*a44*a55*a66 + &
      a13*a31*a44*a56*a65 + a13*a31*a45*a54*a66 - a13*a31*a45*a56*a64 - &
      a13*a31*a46*a54*a65 + a13*a31*a46*a55*a64 + a13*a34*a41*a55*a66 - &
      a13*a34*a41*a56*a65 - a13*a34*a45*a51*a66 + a13*a34*a45*a56*a61 + &
      a13*a34*a46*a51*a65 - a13*a34*a46*a55*a61 - a13*a35*a41*a54*a66 + &
      a13*a35*a41*a56*a64 + a13*a35*a44*a51*a66 - a13*a35*a44*a56*a61 - &
      a13*a35*a46*a51*a64 + a13*a35*a46*a54*a61 + a13*a36*a41*a54*a65 - &
      a13*a36*a41*a55*a64 - a13*a36*a44*a51*a65 + a13*a36*a44*a55*a61 + &
      a13*a36*a45*a51*a64 - a13*a36*a45*a54*a61 + a14*a31*a43*a55*a66 - &
      a14*a31*a43*a56*a65 - a14*a31*a45*a53*a66 + a14*a31*a45*a56*a63 + &
      a14*a31*a46*a53*a65 - a14*a31*a46*a55*a63 - a14*a33*a41*a55*a66 + &
      a14*a33*a41*a56*a65 + a14*a33*a45*a51*a66 - a14*a33*a45*a56*a61 - &
      a14*a33*a46*a51*a65 + a14*a33*a46*a55*a61 + a14*a35*a41*a53*a66 - &
      a14*a35*a41*a56*a63 - a14*a35*a43*a51*a66 + a14*a35*a43*a56*a61 + &
      a14*a35*a46*a51*a63 - a14*a35*a46*a53*a61 - a14*a36*a41*a53*a65 + &
      a14*a36*a41*a55*a63 + a14*a36*a43*a51*a65 - a14*a36*a43*a55*a61 - &
      a14*a36*a45*a51*a63 + a14*a36*a45*a53*a61 - a15*a31*a43*a54*a66 + &
      a15*a31*a43*a56*a64 + a15*a31*a44*a53*a66 - a15*a31*a44*a56*a63 - &
      a15*a31*a46*a53*a64 + a15*a31*a46*a54*a63 + a15*a33*a41*a54*a66 - &
      a15*a33*a41*a56*a64 - a15*a33*a44*a51*a66 + a15*a33*a44*a56*a61 + &
      a15*a33*a46*a51*a64 - a15*a33*a46*a54*a61 - a15*a34*a41*a53*a66 + &
      a15*a34*a41*a56*a63 + a15*a34*a43*a51*a66 - a15*a34*a43*a56*a61 - &
      a15*a34*a46*a51*a63 + a15*a34*a46*a53*a61 + a15*a36*a41*a53*a64 - &
      a15*a36*a41*a54*a63 - a15*a36*a43*a51*a64 + a15*a36*a43*a54*a61 + &
      a15*a36*a44*a51*a63 - a15*a36*a44*a53*a61 + a16*a31*a43*a54*a65 - &
      a16*a31*a43*a55*a64 - a16*a31*a44*a53*a65 + a16*a31*a44*a55*a63 + &
      a16*a31*a45*a53*a64 - a16*a31*a45*a54*a63 - a16*a33*a41*a54*a65 + &
      a16*a33*a41*a55*a64 + a16*a33*a44*a51*a65 - a16*a33*a44*a55*a61 - &
      a16*a33*a45*a51*a64 + a16*a33*a45*a54*a61 + a16*a34*a41*a53*a65 - &
      a16*a34*a41*a55*a63 - a16*a34*a43*a51*a65 + a16*a34*a43*a55*a61 + &
      a16*a34*a45*a51*a63 - a16*a34*a45*a53*a61 - a16*a35*a41*a53*a64 + &
      a16*a35*a41*a54*a63 + a16*a35*a43*a51*a64 - a16*a35*a43*a54*a61 - &
      a16*a35*a44*a51*a63 + a16*a35*a44*a53*a61)*d_inv

    A_inv(3, 2) = (-a11*a32*a44*a55*a66 + a11*a32*a44*a56* &
      a65 + a11*a32*a45*a54*a66 - a11*a32*a45*a56*a64 - a11*a32*a46*a54 &
      *a65 + a11*a32*a46*a55*a64 + a11*a34*a42*a55*a66 - a11*a34*a42* &
      a56*a65 - a11*a34*a45*a52*a66 + a11*a34*a45*a56*a62 + a11*a34*a46 &
      *a52*a65 - a11*a34*a46*a55*a62 - a11*a35*a42*a54*a66 + a11*a35* &
      a42*a56*a64 + a11*a35*a44*a52*a66 - a11*a35*a44*a56*a62 - a11*a35 &
      *a46*a52*a64 + a11*a35*a46*a54*a62 + a11*a36*a42*a54*a65 - a11* &
      a36*a42*a55*a64 - a11*a36*a44*a52*a65 + a11*a36*a44*a55*a62 + a11 &
      *a36*a45*a52*a64 - a11*a36*a45*a54*a62 + a12*a31*a44*a55*a66 - &
      a12*a31*a44*a56*a65 - a12*a31*a45*a54*a66 + a12*a31*a45*a56*a64 + &
      a12*a31*a46*a54*a65 - a12*a31*a46*a55*a64 - a12*a34*a41*a55*a66 + &
      a12*a34*a41*a56*a65 + a12*a34*a45*a51*a66 - a12*a34*a45*a56*a61 - &
      a12*a34*a46*a51*a65 + a12*a34*a46*a55*a61 + a12*a35*a41*a54*a66 - &
      a12*a35*a41*a56*a64 - a12*a35*a44*a51*a66 + a12*a35*a44*a56*a61 + &
      a12*a35*a46*a51*a64 - a12*a35*a46*a54*a61 - a12*a36*a41*a54*a65 + &
      a12*a36*a41*a55*a64 + a12*a36*a44*a51*a65 - a12*a36*a44*a55*a61 - &
      a12*a36*a45*a51*a64 + a12*a36*a45*a54*a61 - a14*a31*a42*a55*a66 + &
      a14*a31*a42*a56*a65 + a14*a31*a45*a52*a66 - a14*a31*a45*a56*a62 - &
      a14*a31*a46*a52*a65 + a14*a31*a46*a55*a62 + a14*a32*a41*a55*a66 - &
      a14*a32*a41*a56*a65 - a14*a32*a45*a51*a66 + a14*a32*a45*a56*a61 + &
      a14*a32*a46*a51*a65 - a14*a32*a46*a55*a61 - a14*a35*a41*a52*a66 + &
      a14*a35*a41*a56*a62 + a14*a35*a42*a51*a66 - a14*a35*a42*a56*a61 - &
      a14*a35*a46*a51*a62 + a14*a35*a46*a52*a61 + a14*a36*a41*a52*a65 - &
      a14*a36*a41*a55*a62 - a14*a36*a42*a51*a65 + a14*a36*a42*a55*a61 + &
      a14*a36*a45*a51*a62 - a14*a36*a45*a52*a61 + a15*a31*a42*a54*a66 - &
      a15*a31*a42*a56*a64 - a15*a31*a44*a52*a66 + a15*a31*a44*a56*a62 + &
      a15*a31*a46*a52*a64 - a15*a31*a46*a54*a62 - a15*a32*a41*a54*a66 + &
      a15*a32*a41*a56*a64 + a15*a32*a44*a51*a66 - a15*a32*a44*a56*a61 - &
      a15*a32*a46*a51*a64 + a15*a32*a46*a54*a61 + a15*a34*a41*a52*a66 - &
      a15*a34*a41*a56*a62 - a15*a34*a42*a51*a66 + a15*a34*a42*a56*a61 + &
      a15*a34*a46*a51*a62 - a15*a34*a46*a52*a61 - a15*a36*a41*a52*a64 + &
      a15*a36*a41*a54*a62 + a15*a36*a42*a51*a64 - a15*a36*a42*a54*a61 - &
      a15*a36*a44*a51*a62 + a15*a36*a44*a52*a61 - a16*a31*a42*a54*a65 + &
      a16*a31*a42*a55*a64 + a16*a31*a44*a52*a65 - a16*a31*a44*a55*a62 - &
      a16*a31*a45*a52*a64 + a16*a31*a45*a54*a62 + a16*a32*a41*a54*a65 - &
      a16*a32*a41*a55*a64 - a16*a32*a44*a51*a65 + a16*a32*a44*a55*a61 + &
      a16*a32*a45*a51*a64 - a16*a32*a45*a54*a61 - a16*a34*a41*a52*a65 + &
      a16*a34*a41*a55*a62 + a16*a34*a42*a51*a65 - a16*a34*a42*a55*a61 - &
      a16*a34*a45*a51*a62 + a16*a34*a45*a52*a61 + a16*a35*a41*a52*a64 - &
      a16*a35*a41*a54*a62 - a16*a35*a42*a51*a64 + a16*a35*a42*a54*a61 + &
      a16*a35*a44*a51*a62 - a16*a35*a44*a52*a61)*d_inv

    A_inv(4, 2) = (a11*a32*a43*a55*a66 - a11*a32*a43*a56* &
      a65 - a11*a32*a45*a53*a66 + a11*a32*a45*a56*a63 + a11*a32*a46*a53 &
      *a65 - a11*a32*a46*a55*a63 - a11*a33*a42*a55*a66 + a11*a33*a42* &
      a56*a65 + a11*a33*a45*a52*a66 - a11*a33*a45*a56*a62 - a11*a33*a46 &
      *a52*a65 + a11*a33*a46*a55*a62 + a11*a35*a42*a53*a66 - a11*a35* &
      a42*a56*a63 - a11*a35*a43*a52*a66 + a11*a35*a43*a56*a62 + a11*a35 &
      *a46*a52*a63 - a11*a35*a46*a53*a62 - a11*a36*a42*a53*a65 + a11* &
      a36*a42*a55*a63 + a11*a36*a43*a52*a65 - a11*a36*a43*a55*a62 - a11 &
      *a36*a45*a52*a63 + a11*a36*a45*a53*a62 - a12*a31*a43*a55*a66 + &
      a12*a31*a43*a56*a65 + a12*a31*a45*a53*a66 - a12*a31*a45*a56*a63 - &
      a12*a31*a46*a53*a65 + a12*a31*a46*a55*a63 + a12*a33*a41*a55*a66 - &
      a12*a33*a41*a56*a65 - a12*a33*a45*a51*a66 + a12*a33*a45*a56*a61 + &
      a12*a33*a46*a51*a65 - a12*a33*a46*a55*a61 - a12*a35*a41*a53*a66 + &
      a12*a35*a41*a56*a63 + a12*a35*a43*a51*a66 - a12*a35*a43*a56*a61 - &
      a12*a35*a46*a51*a63 + a12*a35*a46*a53*a61 + a12*a36*a41*a53*a65 - &
      a12*a36*a41*a55*a63 - a12*a36*a43*a51*a65 + a12*a36*a43*a55*a61 + &
      a12*a36*a45*a51*a63 - a12*a36*a45*a53*a61 + a13*a31*a42*a55*a66 - &
      a13*a31*a42*a56*a65 - a13*a31*a45*a52*a66 + a13*a31*a45*a56*a62 + &
      a13*a31*a46*a52*a65 - a13*a31*a46*a55*a62 - a13*a32*a41*a55*a66 + &
      a13*a32*a41*a56*a65 + a13*a32*a45*a51*a66 - a13*a32*a45*a56*a61 - &
      a13*a32*a46*a51*a65 + a13*a32*a46*a55*a61 + a13*a35*a41*a52*a66 - &
      a13*a35*a41*a56*a62 - a13*a35*a42*a51*a66 + a13*a35*a42*a56*a61 + &
      a13*a35*a46*a51*a62 - a13*a35*a46*a52*a61 - a13*a36*a41*a52*a65 + &
      a13*a36*a41*a55*a62 + a13*a36*a42*a51*a65 - a13*a36*a42*a55*a61 - &
      a13*a36*a45*a51*a62 + a13*a36*a45*a52*a61 - a15*a31*a42*a53*a66 + &
      a15*a31*a42*a56*a63 + a15*a31*a43*a52*a66 - a15*a31*a43*a56*a62 - &
      a15*a31*a46*a52*a63 + a15*a31*a46*a53*a62 + a15*a32*a41*a53*a66 - &
      a15*a32*a41*a56*a63 - a15*a32*a43*a51*a66 + a15*a32*a43*a56*a61 + &
      a15*a32*a46*a51*a63 - a15*a32*a46*a53*a61 - a15*a33*a41*a52*a66 + &
      a15*a33*a41*a56*a62 + a15*a33*a42*a51*a66 - a15*a33*a42*a56*a61 - &
      a15*a33*a46*a51*a62 + a15*a33*a46*a52*a61 + a15*a36*a41*a52*a63 - &
      a15*a36*a41*a53*a62 - a15*a36*a42*a51*a63 + a15*a36*a42*a53*a61 + &
      a15*a36*a43*a51*a62 - a15*a36*a43*a52*a61 + a16*a31*a42*a53*a65 - &
      a16*a31*a42*a55*a63 - a16*a31*a43*a52*a65 + a16*a31*a43*a55*a62 + &
      a16*a31*a45*a52*a63 - a16*a31*a45*a53*a62 - a16*a32*a41*a53*a65 + &
      a16*a32*a41*a55*a63 + a16*a32*a43*a51*a65 - a16*a32*a43*a55*a61 - &
      a16*a32*a45*a51*a63 + a16*a32*a45*a53*a61 + a16*a33*a41*a52*a65 - &
      a16*a33*a41*a55*a62 - a16*a33*a42*a51*a65 + a16*a33*a42*a55*a61 + &
      a16*a33*a45*a51*a62 - a16*a33*a45*a52*a61 - a16*a35*a41*a52*a63 + &
      a16*a35*a41*a53*a62 + a16*a35*a42*a51*a63 - a16*a35*a42*a53*a61 - &
      a16*a35*a43*a51*a62 + a16*a35*a43*a52*a61)*d_inv

    A_inv(5, 2) = (-a11*a32*a43*a54*a66 + a11*a32*a43*a56* &
      a64 + a11*a32*a44*a53*a66 - a11*a32*a44*a56*a63 - a11*a32*a46*a53 &
      *a64 + a11*a32*a46*a54*a63 + a11*a33*a42*a54*a66 - a11*a33*a42* &
      a56*a64 - a11*a33*a44*a52*a66 + a11*a33*a44*a56*a62 + a11*a33*a46 &
      *a52*a64 - a11*a33*a46*a54*a62 - a11*a34*a42*a53*a66 + a11*a34* &
      a42*a56*a63 + a11*a34*a43*a52*a66 - a11*a34*a43*a56*a62 - a11*a34 &
      *a46*a52*a63 + a11*a34*a46*a53*a62 + a11*a36*a42*a53*a64 - a11* &
      a36*a42*a54*a63 - a11*a36*a43*a52*a64 + a11*a36*a43*a54*a62 + a11 &
      *a36*a44*a52*a63 - a11*a36*a44*a53*a62 + a12*a31*a43*a54*a66 - &
      a12*a31*a43*a56*a64 - a12*a31*a44*a53*a66 + a12*a31*a44*a56*a63 + &
      a12*a31*a46*a53*a64 - a12*a31*a46*a54*a63 - a12*a33*a41*a54*a66 + &
      a12*a33*a41*a56*a64 + a12*a33*a44*a51*a66 - a12*a33*a44*a56*a61 - &
      a12*a33*a46*a51*a64 + a12*a33*a46*a54*a61 + a12*a34*a41*a53*a66 - &
      a12*a34*a41*a56*a63 - a12*a34*a43*a51*a66 + a12*a34*a43*a56*a61 + &
      a12*a34*a46*a51*a63 - a12*a34*a46*a53*a61 - a12*a36*a41*a53*a64 + &
      a12*a36*a41*a54*a63 + a12*a36*a43*a51*a64 - a12*a36*a43*a54*a61 - &
      a12*a36*a44*a51*a63 + a12*a36*a44*a53*a61 - a13*a31*a42*a54*a66 + &
      a13*a31*a42*a56*a64 + a13*a31*a44*a52*a66 - a13*a31*a44*a56*a62 - &
      a13*a31*a46*a52*a64 + a13*a31*a46*a54*a62 + a13*a32*a41*a54*a66 - &
      a13*a32*a41*a56*a64 - a13*a32*a44*a51*a66 + a13*a32*a44*a56*a61 + &
      a13*a32*a46*a51*a64 - a13*a32*a46*a54*a61 - a13*a34*a41*a52*a66 + &
      a13*a34*a41*a56*a62 + a13*a34*a42*a51*a66 - a13*a34*a42*a56*a61 - &
      a13*a34*a46*a51*a62 + a13*a34*a46*a52*a61 + a13*a36*a41*a52*a64 - &
      a13*a36*a41*a54*a62 - a13*a36*a42*a51*a64 + a13*a36*a42*a54*a61 + &
      a13*a36*a44*a51*a62 - a13*a36*a44*a52*a61 + a14*a31*a42*a53*a66 - &
      a14*a31*a42*a56*a63 - a14*a31*a43*a52*a66 + a14*a31*a43*a56*a62 + &
      a14*a31*a46*a52*a63 - a14*a31*a46*a53*a62 - a14*a32*a41*a53*a66 + &
      a14*a32*a41*a56*a63 + a14*a32*a43*a51*a66 - a14*a32*a43*a56*a61 - &
      a14*a32*a46*a51*a63 + a14*a32*a46*a53*a61 + a14*a33*a41*a52*a66 - &
      a14*a33*a41*a56*a62 - a14*a33*a42*a51*a66 + a14*a33*a42*a56*a61 + &
      a14*a33*a46*a51*a62 - a14*a33*a46*a52*a61 - a14*a36*a41*a52*a63 + &
      a14*a36*a41*a53*a62 + a14*a36*a42*a51*a63 - a14*a36*a42*a53*a61 - &
      a14*a36*a43*a51*a62 + a14*a36*a43*a52*a61 - a16*a31*a42*a53*a64 + &
      a16*a31*a42*a54*a63 + a16*a31*a43*a52*a64 - a16*a31*a43*a54*a62 - &
      a16*a31*a44*a52*a63 + a16*a31*a44*a53*a62 + a16*a32*a41*a53*a64 - &
      a16*a32*a41*a54*a63 - a16*a32*a43*a51*a64 + a16*a32*a43*a54*a61 + &
      a16*a32*a44*a51*a63 - a16*a32*a44*a53*a61 - a16*a33*a41*a52*a64 + &
      a16*a33*a41*a54*a62 + a16*a33*a42*a51*a64 - a16*a33*a42*a54*a61 - &
      a16*a33*a44*a51*a62 + a16*a33*a44*a52*a61 + a16*a34*a41*a52*a63 - &
      a16*a34*a41*a53*a62 - a16*a34*a42*a51*a63 + a16*a34*a42*a53*a61 + &
      a16*a34*a43*a51*a62 - a16*a34*a43*a52*a61)*d_inv

    A_inv(6, 2) = (a11*a32*a43*a54*a65 - a11*a32*a43*a55* &
      a64 - a11*a32*a44*a53*a65 + a11*a32*a44*a55*a63 + a11*a32*a45*a53 &
      *a64 - a11*a32*a45*a54*a63 - a11*a33*a42*a54*a65 + a11*a33*a42* &
      a55*a64 + a11*a33*a44*a52*a65 - a11*a33*a44*a55*a62 - a11*a33*a45 &
      *a52*a64 + a11*a33*a45*a54*a62 + a11*a34*a42*a53*a65 - a11*a34* &
      a42*a55*a63 - a11*a34*a43*a52*a65 + a11*a34*a43*a55*a62 + a11*a34 &
      *a45*a52*a63 - a11*a34*a45*a53*a62 - a11*a35*a42*a53*a64 + a11* &
      a35*a42*a54*a63 + a11*a35*a43*a52*a64 - a11*a35*a43*a54*a62 - a11 &
      *a35*a44*a52*a63 + a11*a35*a44*a53*a62 - a12*a31*a43*a54*a65 + &
      a12*a31*a43*a55*a64 + a12*a31*a44*a53*a65 - a12*a31*a44*a55*a63 - &
      a12*a31*a45*a53*a64 + a12*a31*a45*a54*a63 + a12*a33*a41*a54*a65 - &
      a12*a33*a41*a55*a64 - a12*a33*a44*a51*a65 + a12*a33*a44*a55*a61 + &
      a12*a33*a45*a51*a64 - a12*a33*a45*a54*a61 - a12*a34*a41*a53*a65 + &
      a12*a34*a41*a55*a63 + a12*a34*a43*a51*a65 - a12*a34*a43*a55*a61 - &
      a12*a34*a45*a51*a63 + a12*a34*a45*a53*a61 + a12*a35*a41*a53*a64 - &
      a12*a35*a41*a54*a63 - a12*a35*a43*a51*a64 + a12*a35*a43*a54*a61 + &
      a12*a35*a44*a51*a63 - a12*a35*a44*a53*a61 + a13*a31*a42*a54*a65 - &
      a13*a31*a42*a55*a64 - a13*a31*a44*a52*a65 + a13*a31*a44*a55*a62 + &
      a13*a31*a45*a52*a64 - a13*a31*a45*a54*a62 - a13*a32*a41*a54*a65 + &
      a13*a32*a41*a55*a64 + a13*a32*a44*a51*a65 - a13*a32*a44*a55*a61 - &
      a13*a32*a45*a51*a64 + a13*a32*a45*a54*a61 + a13*a34*a41*a52*a65 - &
      a13*a34*a41*a55*a62 - a13*a34*a42*a51*a65 + a13*a34*a42*a55*a61 + &
      a13*a34*a45*a51*a62 - a13*a34*a45*a52*a61 - a13*a35*a41*a52*a64 + &
      a13*a35*a41*a54*a62 + a13*a35*a42*a51*a64 - a13*a35*a42*a54*a61 - &
      a13*a35*a44*a51*a62 + a13*a35*a44*a52*a61 - a14*a31*a42*a53*a65 + &
      a14*a31*a42*a55*a63 + a14*a31*a43*a52*a65 - a14*a31*a43*a55*a62 - &
      a14*a31*a45*a52*a63 + a14*a31*a45*a53*a62 + a14*a32*a41*a53*a65 - &
      a14*a32*a41*a55*a63 - a14*a32*a43*a51*a65 + a14*a32*a43*a55*a61 + &
      a14*a32*a45*a51*a63 - a14*a32*a45*a53*a61 - a14*a33*a41*a52*a65 + &
      a14*a33*a41*a55*a62 + a14*a33*a42*a51*a65 - a14*a33*a42*a55*a61 - &
      a14*a33*a45*a51*a62 + a14*a33*a45*a52*a61 + a14*a35*a41*a52*a63 - &
      a14*a35*a41*a53*a62 - a14*a35*a42*a51*a63 + a14*a35*a42*a53*a61 + &
      a14*a35*a43*a51*a62 - a14*a35*a43*a52*a61 + a15*a31*a42*a53*a64 - &
      a15*a31*a42*a54*a63 - a15*a31*a43*a52*a64 + a15*a31*a43*a54*a62 + &
      a15*a31*a44*a52*a63 - a15*a31*a44*a53*a62 - a15*a32*a41*a53*a64 + &
      a15*a32*a41*a54*a63 + a15*a32*a43*a51*a64 - a15*a32*a43*a54*a61 - &
      a15*a32*a44*a51*a63 + a15*a32*a44*a53*a61 + a15*a33*a41*a52*a64 - &
      a15*a33*a41*a54*a62 - a15*a33*a42*a51*a64 + a15*a33*a42*a54*a61 + &
      a15*a33*a44*a51*a62 - a15*a33*a44*a52*a61 - a15*a34*a41*a52*a63 + &
      a15*a34*a41*a53*a62 + a15*a34*a42*a51*a63 - a15*a34*a42*a53*a61 - &
      a15*a34*a43*a51*a62 + a15*a34*a43*a52*a61)*d_inv

    A_inv(1, 3) = (a12*a23*a44*a55*a66 - a12*a23*a44*a56* &
      a65 - a12*a23*a45*a54*a66 + a12*a23*a45*a56*a64 + a12*a23*a46*a54 &
      *a65 - a12*a23*a46*a55*a64 - a12*a24*a43*a55*a66 + a12*a24*a43* &
      a56*a65 + a12*a24*a45*a53*a66 - a12*a24*a45*a56*a63 - a12*a24*a46 &
      *a53*a65 + a12*a24*a46*a55*a63 + a12*a25*a43*a54*a66 - a12*a25* &
      a43*a56*a64 - a12*a25*a44*a53*a66 + a12*a25*a44*a56*a63 + a12*a25 &
      *a46*a53*a64 - a12*a25*a46*a54*a63 - a12*a26*a43*a54*a65 + a12* &
      a26*a43*a55*a64 + a12*a26*a44*a53*a65 - a12*a26*a44*a55*a63 - a12 &
      *a26*a45*a53*a64 + a12*a26*a45*a54*a63 - a13*a22*a44*a55*a66 + &
      a13*a22*a44*a56*a65 + a13*a22*a45*a54*a66 - a13*a22*a45*a56*a64 - &
      a13*a22*a46*a54*a65 + a13*a22*a46*a55*a64 + a13*a24*a42*a55*a66 - &
      a13*a24*a42*a56*a65 - a13*a24*a45*a52*a66 + a13*a24*a45*a56*a62 + &
      a13*a24*a46*a52*a65 - a13*a24*a46*a55*a62 - a13*a25*a42*a54*a66 + &
      a13*a25*a42*a56*a64 + a13*a25*a44*a52*a66 - a13*a25*a44*a56*a62 - &
      a13*a25*a46*a52*a64 + a13*a25*a46*a54*a62 + a13*a26*a42*a54*a65 - &
      a13*a26*a42*a55*a64 - a13*a26*a44*a52*a65 + a13*a26*a44*a55*a62 + &
      a13*a26*a45*a52*a64 - a13*a26*a45*a54*a62 + a14*a22*a43*a55*a66 - &
      a14*a22*a43*a56*a65 - a14*a22*a45*a53*a66 + a14*a22*a45*a56*a63 + &
      a14*a22*a46*a53*a65 - a14*a22*a46*a55*a63 - a14*a23*a42*a55*a66 + &
      a14*a23*a42*a56*a65 + a14*a23*a45*a52*a66 - a14*a23*a45*a56*a62 - &
      a14*a23*a46*a52*a65 + a14*a23*a46*a55*a62 + a14*a25*a42*a53*a66 - &
      a14*a25*a42*a56*a63 - a14*a25*a43*a52*a66 + a14*a25*a43*a56*a62 + &
      a14*a25*a46*a52*a63 - a14*a25*a46*a53*a62 - a14*a26*a42*a53*a65 + &
      a14*a26*a42*a55*a63 + a14*a26*a43*a52*a65 - a14*a26*a43*a55*a62 - &
      a14*a26*a45*a52*a63 + a14*a26*a45*a53*a62 - a15*a22*a43*a54*a66 + &
      a15*a22*a43*a56*a64 + a15*a22*a44*a53*a66 - a15*a22*a44*a56*a63 - &
      a15*a22*a46*a53*a64 + a15*a22*a46*a54*a63 + a15*a23*a42*a54*a66 - &
      a15*a23*a42*a56*a64 - a15*a23*a44*a52*a66 + a15*a23*a44*a56*a62 + &
      a15*a23*a46*a52*a64 - a15*a23*a46*a54*a62 - a15*a24*a42*a53*a66 + &
      a15*a24*a42*a56*a63 + a15*a24*a43*a52*a66 - a15*a24*a43*a56*a62 - &
      a15*a24*a46*a52*a63 + a15*a24*a46*a53*a62 + a15*a26*a42*a53*a64 - &
      a15*a26*a42*a54*a63 - a15*a26*a43*a52*a64 + a15*a26*a43*a54*a62 + &
      a15*a26*a44*a52*a63 - a15*a26*a44*a53*a62 + a16*a22*a43*a54*a65 - &
      a16*a22*a43*a55*a64 - a16*a22*a44*a53*a65 + a16*a22*a44*a55*a63 + &
      a16*a22*a45*a53*a64 - a16*a22*a45*a54*a63 - a16*a23*a42*a54*a65 + &
      a16*a23*a42*a55*a64 + a16*a23*a44*a52*a65 - a16*a23*a44*a55*a62 - &
      a16*a23*a45*a52*a64 + a16*a23*a45*a54*a62 + a16*a24*a42*a53*a65 - &
      a16*a24*a42*a55*a63 - a16*a24*a43*a52*a65 + a16*a24*a43*a55*a62 + &
      a16*a24*a45*a52*a63 - a16*a24*a45*a53*a62 - a16*a25*a42*a53*a64 + &
      a16*a25*a42*a54*a63 + a16*a25*a43*a52*a64 - a16*a25*a43*a54*a62 - &
      a16*a25*a44*a52*a63 + a16*a25*a44*a53*a62)*d_inv

    A_inv(2, 3) = (-a11*a23*a44*a55*a66 + a11*a23*a44*a56* &
      a65 + a11*a23*a45*a54*a66 - a11*a23*a45*a56*a64 - a11*a23*a46*a54 &
      *a65 + a11*a23*a46*a55*a64 + a11*a24*a43*a55*a66 - a11*a24*a43* &
      a56*a65 - a11*a24*a45*a53*a66 + a11*a24*a45*a56*a63 + a11*a24*a46 &
      *a53*a65 - a11*a24*a46*a55*a63 - a11*a25*a43*a54*a66 + a11*a25* &
      a43*a56*a64 + a11*a25*a44*a53*a66 - a11*a25*a44*a56*a63 - a11*a25 &
      *a46*a53*a64 + a11*a25*a46*a54*a63 + a11*a26*a43*a54*a65 - a11* &
      a26*a43*a55*a64 - a11*a26*a44*a53*a65 + a11*a26*a44*a55*a63 + a11 &
      *a26*a45*a53*a64 - a11*a26*a45*a54*a63 + a13*a21*a44*a55*a66 - &
      a13*a21*a44*a56*a65 - a13*a21*a45*a54*a66 + a13*a21*a45*a56*a64 + &
      a13*a21*a46*a54*a65 - a13*a21*a46*a55*a64 - a13*a24*a41*a55*a66 + &
      a13*a24*a41*a56*a65 + a13*a24*a45*a51*a66 - a13*a24*a45*a56*a61 - &
      a13*a24*a46*a51*a65 + a13*a24*a46*a55*a61 + a13*a25*a41*a54*a66 - &
      a13*a25*a41*a56*a64 - a13*a25*a44*a51*a66 + a13*a25*a44*a56*a61 + &
      a13*a25*a46*a51*a64 - a13*a25*a46*a54*a61 - a13*a26*a41*a54*a65 + &
      a13*a26*a41*a55*a64 + a13*a26*a44*a51*a65 - a13*a26*a44*a55*a61 - &
      a13*a26*a45*a51*a64 + a13*a26*a45*a54*a61 - a14*a21*a43*a55*a66 + &
      a14*a21*a43*a56*a65 + a14*a21*a45*a53*a66 - a14*a21*a45*a56*a63 - &
      a14*a21*a46*a53*a65 + a14*a21*a46*a55*a63 + a14*a23*a41*a55*a66 - &
      a14*a23*a41*a56*a65 - a14*a23*a45*a51*a66 + a14*a23*a45*a56*a61 + &
      a14*a23*a46*a51*a65 - a14*a23*a46*a55*a61 - a14*a25*a41*a53*a66 + &
      a14*a25*a41*a56*a63 + a14*a25*a43*a51*a66 - a14*a25*a43*a56*a61 - &
      a14*a25*a46*a51*a63 + a14*a25*a46*a53*a61 + a14*a26*a41*a53*a65 - &
      a14*a26*a41*a55*a63 - a14*a26*a43*a51*a65 + a14*a26*a43*a55*a61 + &
      a14*a26*a45*a51*a63 - a14*a26*a45*a53*a61 + a15*a21*a43*a54*a66 - &
      a15*a21*a43*a56*a64 - a15*a21*a44*a53*a66 + a15*a21*a44*a56*a63 + &
      a15*a21*a46*a53*a64 - a15*a21*a46*a54*a63 - a15*a23*a41*a54*a66 + &
      a15*a23*a41*a56*a64 + a15*a23*a44*a51*a66 - a15*a23*a44*a56*a61 - &
      a15*a23*a46*a51*a64 + a15*a23*a46*a54*a61 + a15*a24*a41*a53*a66 - &
      a15*a24*a41*a56*a63 - a15*a24*a43*a51*a66 + a15*a24*a43*a56*a61 + &
      a15*a24*a46*a51*a63 - a15*a24*a46*a53*a61 - a15*a26*a41*a53*a64 + &
      a15*a26*a41*a54*a63 + a15*a26*a43*a51*a64 - a15*a26*a43*a54*a61 - &
      a15*a26*a44*a51*a63 + a15*a26*a44*a53*a61 - a16*a21*a43*a54*a65 + &
      a16*a21*a43*a55*a64 + a16*a21*a44*a53*a65 - a16*a21*a44*a55*a63 - &
      a16*a21*a45*a53*a64 + a16*a21*a45*a54*a63 + a16*a23*a41*a54*a65 - &
      a16*a23*a41*a55*a64 - a16*a23*a44*a51*a65 + a16*a23*a44*a55*a61 + &
      a16*a23*a45*a51*a64 - a16*a23*a45*a54*a61 - a16*a24*a41*a53*a65 + &
      a16*a24*a41*a55*a63 + a16*a24*a43*a51*a65 - a16*a24*a43*a55*a61 - &
      a16*a24*a45*a51*a63 + a16*a24*a45*a53*a61 + a16*a25*a41*a53*a64 - &
      a16*a25*a41*a54*a63 - a16*a25*a43*a51*a64 + a16*a25*a43*a54*a61 + &
      a16*a25*a44*a51*a63 - a16*a25*a44*a53*a61)*d_inv

    A_inv(3, 3) = (a11*a22*a44*a55*a66 - a11*a22*a44*a56* &
      a65 - a11*a22*a45*a54*a66 + a11*a22*a45*a56*a64 + a11*a22*a46*a54 &
      *a65 - a11*a22*a46*a55*a64 - a11*a24*a42*a55*a66 + a11*a24*a42* &
      a56*a65 + a11*a24*a45*a52*a66 - a11*a24*a45*a56*a62 - a11*a24*a46 &
      *a52*a65 + a11*a24*a46*a55*a62 + a11*a25*a42*a54*a66 - a11*a25* &
      a42*a56*a64 - a11*a25*a44*a52*a66 + a11*a25*a44*a56*a62 + a11*a25 &
      *a46*a52*a64 - a11*a25*a46*a54*a62 - a11*a26*a42*a54*a65 + a11* &
      a26*a42*a55*a64 + a11*a26*a44*a52*a65 - a11*a26*a44*a55*a62 - a11 &
      *a26*a45*a52*a64 + a11*a26*a45*a54*a62 - a12*a21*a44*a55*a66 + &
      a12*a21*a44*a56*a65 + a12*a21*a45*a54*a66 - a12*a21*a45*a56*a64 - &
      a12*a21*a46*a54*a65 + a12*a21*a46*a55*a64 + a12*a24*a41*a55*a66 - &
      a12*a24*a41*a56*a65 - a12*a24*a45*a51*a66 + a12*a24*a45*a56*a61 + &
      a12*a24*a46*a51*a65 - a12*a24*a46*a55*a61 - a12*a25*a41*a54*a66 + &
      a12*a25*a41*a56*a64 + a12*a25*a44*a51*a66 - a12*a25*a44*a56*a61 - &
      a12*a25*a46*a51*a64 + a12*a25*a46*a54*a61 + a12*a26*a41*a54*a65 - &
      a12*a26*a41*a55*a64 - a12*a26*a44*a51*a65 + a12*a26*a44*a55*a61 + &
      a12*a26*a45*a51*a64 - a12*a26*a45*a54*a61 + a14*a21*a42*a55*a66 - &
      a14*a21*a42*a56*a65 - a14*a21*a45*a52*a66 + a14*a21*a45*a56*a62 + &
      a14*a21*a46*a52*a65 - a14*a21*a46*a55*a62 - a14*a22*a41*a55*a66 + &
      a14*a22*a41*a56*a65 + a14*a22*a45*a51*a66 - a14*a22*a45*a56*a61 - &
      a14*a22*a46*a51*a65 + a14*a22*a46*a55*a61 + a14*a25*a41*a52*a66 - &
      a14*a25*a41*a56*a62 - a14*a25*a42*a51*a66 + a14*a25*a42*a56*a61 + &
      a14*a25*a46*a51*a62 - a14*a25*a46*a52*a61 - a14*a26*a41*a52*a65 + &
      a14*a26*a41*a55*a62 + a14*a26*a42*a51*a65 - a14*a26*a42*a55*a61 - &
      a14*a26*a45*a51*a62 + a14*a26*a45*a52*a61 - a15*a21*a42*a54*a66 + &
      a15*a21*a42*a56*a64 + a15*a21*a44*a52*a66 - a15*a21*a44*a56*a62 - &
      a15*a21*a46*a52*a64 + a15*a21*a46*a54*a62 + a15*a22*a41*a54*a66 - &
      a15*a22*a41*a56*a64 - a15*a22*a44*a51*a66 + a15*a22*a44*a56*a61 + &
      a15*a22*a46*a51*a64 - a15*a22*a46*a54*a61 - a15*a24*a41*a52*a66 + &
      a15*a24*a41*a56*a62 + a15*a24*a42*a51*a66 - a15*a24*a42*a56*a61 - &
      a15*a24*a46*a51*a62 + a15*a24*a46*a52*a61 + a15*a26*a41*a52*a64 - &
      a15*a26*a41*a54*a62 - a15*a26*a42*a51*a64 + a15*a26*a42*a54*a61 + &
      a15*a26*a44*a51*a62 - a15*a26*a44*a52*a61 + a16*a21*a42*a54*a65 - &
      a16*a21*a42*a55*a64 - a16*a21*a44*a52*a65 + a16*a21*a44*a55*a62 + &
      a16*a21*a45*a52*a64 - a16*a21*a45*a54*a62 - a16*a22*a41*a54*a65 + &
      a16*a22*a41*a55*a64 + a16*a22*a44*a51*a65 - a16*a22*a44*a55*a61 - &
      a16*a22*a45*a51*a64 + a16*a22*a45*a54*a61 + a16*a24*a41*a52*a65 - &
      a16*a24*a41*a55*a62 - a16*a24*a42*a51*a65 + a16*a24*a42*a55*a61 + &
      a16*a24*a45*a51*a62 - a16*a24*a45*a52*a61 - a16*a25*a41*a52*a64 + &
      a16*a25*a41*a54*a62 + a16*a25*a42*a51*a64 - a16*a25*a42*a54*a61 - &
      a16*a25*a44*a51*a62 + a16*a25*a44*a52*a61)*d_inv

    A_inv(4, 3) = (-a11*a22*a43*a55*a66 + a11*a22*a43*a56* &
      a65 + a11*a22*a45*a53*a66 - a11*a22*a45*a56*a63 - a11*a22*a46*a53 &
      *a65 + a11*a22*a46*a55*a63 + a11*a23*a42*a55*a66 - a11*a23*a42* &
      a56*a65 - a11*a23*a45*a52*a66 + a11*a23*a45*a56*a62 + a11*a23*a46 &
      *a52*a65 - a11*a23*a46*a55*a62 - a11*a25*a42*a53*a66 + a11*a25* &
      a42*a56*a63 + a11*a25*a43*a52*a66 - a11*a25*a43*a56*a62 - a11*a25 &
      *a46*a52*a63 + a11*a25*a46*a53*a62 + a11*a26*a42*a53*a65 - a11* &
      a26*a42*a55*a63 - a11*a26*a43*a52*a65 + a11*a26*a43*a55*a62 + a11 &
      *a26*a45*a52*a63 - a11*a26*a45*a53*a62 + a12*a21*a43*a55*a66 - &
      a12*a21*a43*a56*a65 - a12*a21*a45*a53*a66 + a12*a21*a45*a56*a63 + &
      a12*a21*a46*a53*a65 - a12*a21*a46*a55*a63 - a12*a23*a41*a55*a66 + &
      a12*a23*a41*a56*a65 + a12*a23*a45*a51*a66 - a12*a23*a45*a56*a61 - &
      a12*a23*a46*a51*a65 + a12*a23*a46*a55*a61 + a12*a25*a41*a53*a66 - &
      a12*a25*a41*a56*a63 - a12*a25*a43*a51*a66 + a12*a25*a43*a56*a61 + &
      a12*a25*a46*a51*a63 - a12*a25*a46*a53*a61 - a12*a26*a41*a53*a65 + &
      a12*a26*a41*a55*a63 + a12*a26*a43*a51*a65 - a12*a26*a43*a55*a61 - &
      a12*a26*a45*a51*a63 + a12*a26*a45*a53*a61 - a13*a21*a42*a55*a66 + &
      a13*a21*a42*a56*a65 + a13*a21*a45*a52*a66 - a13*a21*a45*a56*a62 - &
      a13*a21*a46*a52*a65 + a13*a21*a46*a55*a62 + a13*a22*a41*a55*a66 - &
      a13*a22*a41*a56*a65 - a13*a22*a45*a51*a66 + a13*a22*a45*a56*a61 + &
      a13*a22*a46*a51*a65 - a13*a22*a46*a55*a61 - a13*a25*a41*a52*a66 + &
      a13*a25*a41*a56*a62 + a13*a25*a42*a51*a66 - a13*a25*a42*a56*a61 - &
      a13*a25*a46*a51*a62 + a13*a25*a46*a52*a61 + a13*a26*a41*a52*a65 - &
      a13*a26*a41*a55*a62 - a13*a26*a42*a51*a65 + a13*a26*a42*a55*a61 + &
      a13*a26*a45*a51*a62 - a13*a26*a45*a52*a61 + a15*a21*a42*a53*a66 - &
      a15*a21*a42*a56*a63 - a15*a21*a43*a52*a66 + a15*a21*a43*a56*a62 + &
      a15*a21*a46*a52*a63 - a15*a21*a46*a53*a62 - a15*a22*a41*a53*a66 + &
      a15*a22*a41*a56*a63 + a15*a22*a43*a51*a66 - a15*a22*a43*a56*a61 - &
      a15*a22*a46*a51*a63 + a15*a22*a46*a53*a61 + a15*a23*a41*a52*a66 - &
      a15*a23*a41*a56*a62 - a15*a23*a42*a51*a66 + a15*a23*a42*a56*a61 + &
      a15*a23*a46*a51*a62 - a15*a23*a46*a52*a61 - a15*a26*a41*a52*a63 + &
      a15*a26*a41*a53*a62 + a15*a26*a42*a51*a63 - a15*a26*a42*a53*a61 - &
      a15*a26*a43*a51*a62 + a15*a26*a43*a52*a61 - a16*a21*a42*a53*a65 + &
      a16*a21*a42*a55*a63 + a16*a21*a43*a52*a65 - a16*a21*a43*a55*a62 - &
      a16*a21*a45*a52*a63 + a16*a21*a45*a53*a62 + a16*a22*a41*a53*a65 - &
      a16*a22*a41*a55*a63 - a16*a22*a43*a51*a65 + a16*a22*a43*a55*a61 + &
      a16*a22*a45*a51*a63 - a16*a22*a45*a53*a61 - a16*a23*a41*a52*a65 + &
      a16*a23*a41*a55*a62 + a16*a23*a42*a51*a65 - a16*a23*a42*a55*a61 - &
      a16*a23*a45*a51*a62 + a16*a23*a45*a52*a61 + a16*a25*a41*a52*a63 - &
      a16*a25*a41*a53*a62 - a16*a25*a42*a51*a63 + a16*a25*a42*a53*a61 + &
      a16*a25*a43*a51*a62 - a16*a25*a43*a52*a61)*d_inv
      
    A_inv(5, 3) = (a11*a22*a43*a54*a66 - a11*a22*a43*a56* &
      a64 - a11*a22*a44*a53*a66 + a11*a22*a44*a56*a63 + a11*a22*a46*a53 &
      *a64 - a11*a22*a46*a54*a63 - a11*a23*a42*a54*a66 + a11*a23*a42* &
      a56*a64 + a11*a23*a44*a52*a66 - a11*a23*a44*a56*a62 - a11*a23*a46 &
      *a52*a64 + a11*a23*a46*a54*a62 + a11*a24*a42*a53*a66 - a11*a24* &
      a42*a56*a63 - a11*a24*a43*a52*a66 + a11*a24*a43*a56*a62 + a11*a24 &
      *a46*a52*a63 - a11*a24*a46*a53*a62 - a11*a26*a42*a53*a64 + a11* &
      a26*a42*a54*a63 + a11*a26*a43*a52*a64 - a11*a26*a43*a54*a62 - a11 &
      *a26*a44*a52*a63 + a11*a26*a44*a53*a62 - a12*a21*a43*a54*a66 + &
      a12*a21*a43*a56*a64 + a12*a21*a44*a53*a66 - a12*a21*a44*a56*a63 - &
      a12*a21*a46*a53*a64 + a12*a21*a46*a54*a63 + a12*a23*a41*a54*a66 - &
      a12*a23*a41*a56*a64 - a12*a23*a44*a51*a66 + a12*a23*a44*a56*a61 + &
      a12*a23*a46*a51*a64 - a12*a23*a46*a54*a61 - a12*a24*a41*a53*a66 + &
      a12*a24*a41*a56*a63 + a12*a24*a43*a51*a66 - a12*a24*a43*a56*a61 - &
      a12*a24*a46*a51*a63 + a12*a24*a46*a53*a61 + a12*a26*a41*a53*a64 - &
      a12*a26*a41*a54*a63 - a12*a26*a43*a51*a64 + a12*a26*a43*a54*a61 + &
      a12*a26*a44*a51*a63 - a12*a26*a44*a53*a61 + a13*a21*a42*a54*a66 - &
      a13*a21*a42*a56*a64 - a13*a21*a44*a52*a66 + a13*a21*a44*a56*a62 + &
      a13*a21*a46*a52*a64 - a13*a21*a46*a54*a62 - a13*a22*a41*a54*a66 + &
      a13*a22*a41*a56*a64 + a13*a22*a44*a51*a66 - a13*a22*a44*a56*a61 - &
      a13*a22*a46*a51*a64 + a13*a22*a46*a54*a61 + a13*a24*a41*a52*a66 - &
      a13*a24*a41*a56*a62 - a13*a24*a42*a51*a66 + a13*a24*a42*a56*a61 + &
      a13*a24*a46*a51*a62 - a13*a24*a46*a52*a61 - a13*a26*a41*a52*a64 + &
      a13*a26*a41*a54*a62 + a13*a26*a42*a51*a64 - a13*a26*a42*a54*a61 - &
      a13*a26*a44*a51*a62 + a13*a26*a44*a52*a61 - a14*a21*a42*a53*a66 + &
      a14*a21*a42*a56*a63 + a14*a21*a43*a52*a66 - a14*a21*a43*a56*a62 - &
      a14*a21*a46*a52*a63 + a14*a21*a46*a53*a62 + a14*a22*a41*a53*a66 - &
      a14*a22*a41*a56*a63 - a14*a22*a43*a51*a66 + a14*a22*a43*a56*a61 + &
      a14*a22*a46*a51*a63 - a14*a22*a46*a53*a61 - a14*a23*a41*a52*a66 + &
      a14*a23*a41*a56*a62 + a14*a23*a42*a51*a66 - a14*a23*a42*a56*a61 - &
      a14*a23*a46*a51*a62 + a14*a23*a46*a52*a61 + a14*a26*a41*a52*a63 - &
      a14*a26*a41*a53*a62 - a14*a26*a42*a51*a63 + a14*a26*a42*a53*a61 + &
      a14*a26*a43*a51*a62 - a14*a26*a43*a52*a61 + a16*a21*a42*a53*a64 - &
      a16*a21*a42*a54*a63 - a16*a21*a43*a52*a64 + a16*a21*a43*a54*a62 + &
      a16*a21*a44*a52*a63 - a16*a21*a44*a53*a62 - a16*a22*a41*a53*a64 + &
      a16*a22*a41*a54*a63 + a16*a22*a43*a51*a64 - a16*a22*a43*a54*a61 - &
      a16*a22*a44*a51*a63 + a16*a22*a44*a53*a61 + a16*a23*a41*a52*a64 - &
      a16*a23*a41*a54*a62 - a16*a23*a42*a51*a64 + a16*a23*a42*a54*a61 + &
      a16*a23*a44*a51*a62 - a16*a23*a44*a52*a61 - a16*a24*a41*a52*a63 + &
      a16*a24*a41*a53*a62 + a16*a24*a42*a51*a63 - a16*a24*a42*a53*a61 - &
      a16*a24*a43*a51*a62 + a16*a24*a43*a52*a61)*d_inv

    A_inv(6, 3) = (-a11*a22*a43*a54*a65 + a11*a22*a43*a55* &
      a64 + a11*a22*a44*a53*a65 - a11*a22*a44*a55*a63 - a11*a22*a45*a53 &
      *a64 + a11*a22*a45*a54*a63 + a11*a23*a42*a54*a65 - a11*a23*a42* &
      a55*a64 - a11*a23*a44*a52*a65 + a11*a23*a44*a55*a62 + a11*a23*a45 &
      *a52*a64 - a11*a23*a45*a54*a62 - a11*a24*a42*a53*a65 + a11*a24* &
      a42*a55*a63 + a11*a24*a43*a52*a65 - a11*a24*a43*a55*a62 - a11*a24 &
      *a45*a52*a63 + a11*a24*a45*a53*a62 + a11*a25*a42*a53*a64 - a11* &
      a25*a42*a54*a63 - a11*a25*a43*a52*a64 + a11*a25*a43*a54*a62 + a11 &
      *a25*a44*a52*a63 - a11*a25*a44*a53*a62 + a12*a21*a43*a54*a65 - &
      a12*a21*a43*a55*a64 - a12*a21*a44*a53*a65 + a12*a21*a44*a55*a63 + &
      a12*a21*a45*a53*a64 - a12*a21*a45*a54*a63 - a12*a23*a41*a54*a65 + &
      a12*a23*a41*a55*a64 + a12*a23*a44*a51*a65 - a12*a23*a44*a55*a61 - &
      a12*a23*a45*a51*a64 + a12*a23*a45*a54*a61 + a12*a24*a41*a53*a65 - &
      a12*a24*a41*a55*a63 - a12*a24*a43*a51*a65 + a12*a24*a43*a55*a61 + &
      a12*a24*a45*a51*a63 - a12*a24*a45*a53*a61 - a12*a25*a41*a53*a64 + &
      a12*a25*a41*a54*a63 + a12*a25*a43*a51*a64 - a12*a25*a43*a54*a61 - &
      a12*a25*a44*a51*a63 + a12*a25*a44*a53*a61 - a13*a21*a42*a54*a65 + &
      a13*a21*a42*a55*a64 + a13*a21*a44*a52*a65 - a13*a21*a44*a55*a62 - &
      a13*a21*a45*a52*a64 + a13*a21*a45*a54*a62 + a13*a22*a41*a54*a65 - &
      a13*a22*a41*a55*a64 - a13*a22*a44*a51*a65 + a13*a22*a44*a55*a61 + &
      a13*a22*a45*a51*a64 - a13*a22*a45*a54*a61 - a13*a24*a41*a52*a65 + &
      a13*a24*a41*a55*a62 + a13*a24*a42*a51*a65 - a13*a24*a42*a55*a61 - &
      a13*a24*a45*a51*a62 + a13*a24*a45*a52*a61 + a13*a25*a41*a52*a64 - &
      a13*a25*a41*a54*a62 - a13*a25*a42*a51*a64 + a13*a25*a42*a54*a61 + &
      a13*a25*a44*a51*a62 - a13*a25*a44*a52*a61 + a14*a21*a42*a53*a65 - &
      a14*a21*a42*a55*a63 - a14*a21*a43*a52*a65 + a14*a21*a43*a55*a62 + &
      a14*a21*a45*a52*a63 - a14*a21*a45*a53*a62 - a14*a22*a41*a53*a65 + &
      a14*a22*a41*a55*a63 + a14*a22*a43*a51*a65 - a14*a22*a43*a55*a61 - &
      a14*a22*a45*a51*a63 + a14*a22*a45*a53*a61 + a14*a23*a41*a52*a65 - &
      a14*a23*a41*a55*a62 - a14*a23*a42*a51*a65 + a14*a23*a42*a55*a61 + &
      a14*a23*a45*a51*a62 - a14*a23*a45*a52*a61 - a14*a25*a41*a52*a63 + &
      a14*a25*a41*a53*a62 + a14*a25*a42*a51*a63 - a14*a25*a42*a53*a61 - &
      a14*a25*a43*a51*a62 + a14*a25*a43*a52*a61 - a15*a21*a42*a53*a64 + &
      a15*a21*a42*a54*a63 + a15*a21*a43*a52*a64 - a15*a21*a43*a54*a62 - &
      a15*a21*a44*a52*a63 + a15*a21*a44*a53*a62 + a15*a22*a41*a53*a64 - &
      a15*a22*a41*a54*a63 - a15*a22*a43*a51*a64 + a15*a22*a43*a54*a61 + &
      a15*a22*a44*a51*a63 - a15*a22*a44*a53*a61 - a15*a23*a41*a52*a64 + &
      a15*a23*a41*a54*a62 + a15*a23*a42*a51*a64 - a15*a23*a42*a54*a61 - &
      a15*a23*a44*a51*a62 + a15*a23*a44*a52*a61 + a15*a24*a41*a52*a63 - &
      a15*a24*a41*a53*a62 - a15*a24*a42*a51*a63 + a15*a24*a42*a53*a61 + &
      a15*a24*a43*a51*a62 - a15*a24*a43*a52*a61)*d_inv

    A_inv(1, 4) = (-a12*a23*a34*a55*a66 + a12*a23*a34*a56* &
      a65 + a12*a23*a35*a54*a66 - a12*a23*a35*a56*a64 - a12*a23*a36*a54 &
      *a65 + a12*a23*a36*a55*a64 + a12*a24*a33*a55*a66 - a12*a24*a33* &
      a56*a65 - a12*a24*a35*a53*a66 + a12*a24*a35*a56*a63 + a12*a24*a36 &
      *a53*a65 - a12*a24*a36*a55*a63 - a12*a25*a33*a54*a66 + a12*a25* &
      a33*a56*a64 + a12*a25*a34*a53*a66 - a12*a25*a34*a56*a63 - a12*a25 &
      *a36*a53*a64 + a12*a25*a36*a54*a63 + a12*a26*a33*a54*a65 - a12* &
      a26*a33*a55*a64 - a12*a26*a34*a53*a65 + a12*a26*a34*a55*a63 + a12 &
      *a26*a35*a53*a64 - a12*a26*a35*a54*a63 + a13*a22*a34*a55*a66 - &
      a13*a22*a34*a56*a65 - a13*a22*a35*a54*a66 + a13*a22*a35*a56*a64 + &
      a13*a22*a36*a54*a65 - a13*a22*a36*a55*a64 - a13*a24*a32*a55*a66 + &
      a13*a24*a32*a56*a65 + a13*a24*a35*a52*a66 - a13*a24*a35*a56*a62 - &
      a13*a24*a36*a52*a65 + a13*a24*a36*a55*a62 + a13*a25*a32*a54*a66 - &
      a13*a25*a32*a56*a64 - a13*a25*a34*a52*a66 + a13*a25*a34*a56*a62 + &
      a13*a25*a36*a52*a64 - a13*a25*a36*a54*a62 - a13*a26*a32*a54*a65 + &
      a13*a26*a32*a55*a64 + a13*a26*a34*a52*a65 - a13*a26*a34*a55*a62 - &
      a13*a26*a35*a52*a64 + a13*a26*a35*a54*a62 - a14*a22*a33*a55*a66 + &
      a14*a22*a33*a56*a65 + a14*a22*a35*a53*a66 - a14*a22*a35*a56*a63 - &
      a14*a22*a36*a53*a65 + a14*a22*a36*a55*a63 + a14*a23*a32*a55*a66 - &
      a14*a23*a32*a56*a65 - a14*a23*a35*a52*a66 + a14*a23*a35*a56*a62 + &
      a14*a23*a36*a52*a65 - a14*a23*a36*a55*a62 - a14*a25*a32*a53*a66 + &
      a14*a25*a32*a56*a63 + a14*a25*a33*a52*a66 - a14*a25*a33*a56*a62 - &
      a14*a25*a36*a52*a63 + a14*a25*a36*a53*a62 + a14*a26*a32*a53*a65 - &
      a14*a26*a32*a55*a63 - a14*a26*a33*a52*a65 + a14*a26*a33*a55*a62 + &
      a14*a26*a35*a52*a63 - a14*a26*a35*a53*a62 + a15*a22*a33*a54*a66 - &
      a15*a22*a33*a56*a64 - a15*a22*a34*a53*a66 + a15*a22*a34*a56*a63 + &
      a15*a22*a36*a53*a64 - a15*a22*a36*a54*a63 - a15*a23*a32*a54*a66 + &
      a15*a23*a32*a56*a64 + a15*a23*a34*a52*a66 - a15*a23*a34*a56*a62 - &
      a15*a23*a36*a52*a64 + a15*a23*a36*a54*a62 + a15*a24*a32*a53*a66 - &
      a15*a24*a32*a56*a63 - a15*a24*a33*a52*a66 + a15*a24*a33*a56*a62 + &
      a15*a24*a36*a52*a63 - a15*a24*a36*a53*a62 - a15*a26*a32*a53*a64 + &
      a15*a26*a32*a54*a63 + a15*a26*a33*a52*a64 - a15*a26*a33*a54*a62 - &
      a15*a26*a34*a52*a63 + a15*a26*a34*a53*a62 - a16*a22*a33*a54*a65 + &
      a16*a22*a33*a55*a64 + a16*a22*a34*a53*a65 - a16*a22*a34*a55*a63 - &
      a16*a22*a35*a53*a64 + a16*a22*a35*a54*a63 + a16*a23*a32*a54*a65 - &
      a16*a23*a32*a55*a64 - a16*a23*a34*a52*a65 + a16*a23*a34*a55*a62 + &
      a16*a23*a35*a52*a64 - a16*a23*a35*a54*a62 - a16*a24*a32*a53*a65 + &
      a16*a24*a32*a55*a63 + a16*a24*a33*a52*a65 - a16*a24*a33*a55*a62 - &
      a16*a24*a35*a52*a63 + a16*a24*a35*a53*a62 + a16*a25*a32*a53*a64 - &
      a16*a25*a32*a54*a63 - a16*a25*a33*a52*a64 + a16*a25*a33*a54*a62 + &
      a16*a25*a34*a52*a63 - a16*a25*a34*a53*a62)*d_inv

    A_inv(2, 4) = (a11*a23*a34*a55*a66 - a11*a23*a34*a56* &
      a65 - a11*a23*a35*a54*a66 + a11*a23*a35*a56*a64 + a11*a23*a36*a54 &
      *a65 - a11*a23*a36*a55*a64 - a11*a24*a33*a55*a66 + a11*a24*a33* &
      a56*a65 + a11*a24*a35*a53*a66 - a11*a24*a35*a56*a63 - a11*a24*a36 &
      *a53*a65 + a11*a24*a36*a55*a63 + a11*a25*a33*a54*a66 - a11*a25* &
      a33*a56*a64 - a11*a25*a34*a53*a66 + a11*a25*a34*a56*a63 + a11*a25 &
      *a36*a53*a64 - a11*a25*a36*a54*a63 - a11*a26*a33*a54*a65 + a11* &
      a26*a33*a55*a64 + a11*a26*a34*a53*a65 - a11*a26*a34*a55*a63 - a11 &
      *a26*a35*a53*a64 + a11*a26*a35*a54*a63 - a13*a21*a34*a55*a66 + &
      a13*a21*a34*a56*a65 + a13*a21*a35*a54*a66 - a13*a21*a35*a56*a64 - &
      a13*a21*a36*a54*a65 + a13*a21*a36*a55*a64 + a13*a24*a31*a55*a66 - &
      a13*a24*a31*a56*a65 - a13*a24*a35*a51*a66 + a13*a24*a35*a56*a61 + &
      a13*a24*a36*a51*a65 - a13*a24*a36*a55*a61 - a13*a25*a31*a54*a66 + &
      a13*a25*a31*a56*a64 + a13*a25*a34*a51*a66 - a13*a25*a34*a56*a61 - &
      a13*a25*a36*a51*a64 + a13*a25*a36*a54*a61 + a13*a26*a31*a54*a65 - &
      a13*a26*a31*a55*a64 - a13*a26*a34*a51*a65 + a13*a26*a34*a55*a61 + &
      a13*a26*a35*a51*a64 - a13*a26*a35*a54*a61 + a14*a21*a33*a55*a66 - &
      a14*a21*a33*a56*a65 - a14*a21*a35*a53*a66 + a14*a21*a35*a56*a63 + &
      a14*a21*a36*a53*a65 - a14*a21*a36*a55*a63 - a14*a23*a31*a55*a66 + &
      a14*a23*a31*a56*a65 + a14*a23*a35*a51*a66 - a14*a23*a35*a56*a61 - &
      a14*a23*a36*a51*a65 + a14*a23*a36*a55*a61 + a14*a25*a31*a53*a66 - &
      a14*a25*a31*a56*a63 - a14*a25*a33*a51*a66 + a14*a25*a33*a56*a61 + &
      a14*a25*a36*a51*a63 - a14*a25*a36*a53*a61 - a14*a26*a31*a53*a65 + &
      a14*a26*a31*a55*a63 + a14*a26*a33*a51*a65 - a14*a26*a33*a55*a61 - &
      a14*a26*a35*a51*a63 + a14*a26*a35*a53*a61 - a15*a21*a33*a54*a66 + &
      a15*a21*a33*a56*a64 + a15*a21*a34*a53*a66 - a15*a21*a34*a56*a63 - &
      a15*a21*a36*a53*a64 + a15*a21*a36*a54*a63 + a15*a23*a31*a54*a66 - &
      a15*a23*a31*a56*a64 - a15*a23*a34*a51*a66 + a15*a23*a34*a56*a61 + &
      a15*a23*a36*a51*a64 - a15*a23*a36*a54*a61 - a15*a24*a31*a53*a66 + &
      a15*a24*a31*a56*a63 + a15*a24*a33*a51*a66 - a15*a24*a33*a56*a61 - &
      a15*a24*a36*a51*a63 + a15*a24*a36*a53*a61 + a15*a26*a31*a53*a64 - &
      a15*a26*a31*a54*a63 - a15*a26*a33*a51*a64 + a15*a26*a33*a54*a61 + &
      a15*a26*a34*a51*a63 - a15*a26*a34*a53*a61 + a16*a21*a33*a54*a65 - &
      a16*a21*a33*a55*a64 - a16*a21*a34*a53*a65 + a16*a21*a34*a55*a63 + &
      a16*a21*a35*a53*a64 - a16*a21*a35*a54*a63 - a16*a23*a31*a54*a65 + &
      a16*a23*a31*a55*a64 + a16*a23*a34*a51*a65 - a16*a23*a34*a55*a61 - &
      a16*a23*a35*a51*a64 + a16*a23*a35*a54*a61 + a16*a24*a31*a53*a65 - &
      a16*a24*a31*a55*a63 - a16*a24*a33*a51*a65 + a16*a24*a33*a55*a61 + &
      a16*a24*a35*a51*a63 - a16*a24*a35*a53*a61 - a16*a25*a31*a53*a64 + &
      a16*a25*a31*a54*a63 + a16*a25*a33*a51*a64 - a16*a25*a33*a54*a61 - &
      a16*a25*a34*a51*a63 + a16*a25*a34*a53*a61)*d_inv

    A_inv(3, 4) = (-a11*a22*a34*a55*a66 + a11*a22*a34*a56* &
      a65 + a11*a22*a35*a54*a66 - a11*a22*a35*a56*a64 - a11*a22*a36*a54 &
      *a65 + a11*a22*a36*a55*a64 + a11*a24*a32*a55*a66 - a11*a24*a32* &
      a56*a65 - a11*a24*a35*a52*a66 + a11*a24*a35*a56*a62 + a11*a24*a36 &
      *a52*a65 - a11*a24*a36*a55*a62 - a11*a25*a32*a54*a66 + a11*a25* &
      a32*a56*a64 + a11*a25*a34*a52*a66 - a11*a25*a34*a56*a62 - a11*a25 &
      *a36*a52*a64 + a11*a25*a36*a54*a62 + a11*a26*a32*a54*a65 - a11* &
      a26*a32*a55*a64 - a11*a26*a34*a52*a65 + a11*a26*a34*a55*a62 + a11 &
      *a26*a35*a52*a64 - a11*a26*a35*a54*a62 + a12*a21*a34*a55*a66 - &
      a12*a21*a34*a56*a65 - a12*a21*a35*a54*a66 + a12*a21*a35*a56*a64 + &
      a12*a21*a36*a54*a65 - a12*a21*a36*a55*a64 - a12*a24*a31*a55*a66 + &
      a12*a24*a31*a56*a65 + a12*a24*a35*a51*a66 - a12*a24*a35*a56*a61 - &
      a12*a24*a36*a51*a65 + a12*a24*a36*a55*a61 + a12*a25*a31*a54*a66 - &
      a12*a25*a31*a56*a64 - a12*a25*a34*a51*a66 + a12*a25*a34*a56*a61 + &
      a12*a25*a36*a51*a64 - a12*a25*a36*a54*a61 - a12*a26*a31*a54*a65 + &
      a12*a26*a31*a55*a64 + a12*a26*a34*a51*a65 - a12*a26*a34*a55*a61 - &
      a12*a26*a35*a51*a64 + a12*a26*a35*a54*a61 - a14*a21*a32*a55*a66 + &
      a14*a21*a32*a56*a65 + a14*a21*a35*a52*a66 - a14*a21*a35*a56*a62 - &
      a14*a21*a36*a52*a65 + a14*a21*a36*a55*a62 + a14*a22*a31*a55*a66 - &
      a14*a22*a31*a56*a65 - a14*a22*a35*a51*a66 + a14*a22*a35*a56*a61 + &
      a14*a22*a36*a51*a65 - a14*a22*a36*a55*a61 - a14*a25*a31*a52*a66 + &
      a14*a25*a31*a56*a62 + a14*a25*a32*a51*a66 - a14*a25*a32*a56*a61 - &
      a14*a25*a36*a51*a62 + a14*a25*a36*a52*a61 + a14*a26*a31*a52*a65 - &
      a14*a26*a31*a55*a62 - a14*a26*a32*a51*a65 + a14*a26*a32*a55*a61 + &
      a14*a26*a35*a51*a62 - a14*a26*a35*a52*a61 + a15*a21*a32*a54*a66 - &
      a15*a21*a32*a56*a64 - a15*a21*a34*a52*a66 + a15*a21*a34*a56*a62 + &
      a15*a21*a36*a52*a64 - a15*a21*a36*a54*a62 - a15*a22*a31*a54*a66 + &
      a15*a22*a31*a56*a64 + a15*a22*a34*a51*a66 - a15*a22*a34*a56*a61 - &
      a15*a22*a36*a51*a64 + a15*a22*a36*a54*a61 + a15*a24*a31*a52*a66 - &
      a15*a24*a31*a56*a62 - a15*a24*a32*a51*a66 + a15*a24*a32*a56*a61 + &
      a15*a24*a36*a51*a62 - a15*a24*a36*a52*a61 - a15*a26*a31*a52*a64 + &
      a15*a26*a31*a54*a62 + a15*a26*a32*a51*a64 - a15*a26*a32*a54*a61 - &
      a15*a26*a34*a51*a62 + a15*a26*a34*a52*a61 - a16*a21*a32*a54*a65 + &
      a16*a21*a32*a55*a64 + a16*a21*a34*a52*a65 - a16*a21*a34*a55*a62 - &
      a16*a21*a35*a52*a64 + a16*a21*a35*a54*a62 + a16*a22*a31*a54*a65 - &
      a16*a22*a31*a55*a64 - a16*a22*a34*a51*a65 + a16*a22*a34*a55*a61 + &
      a16*a22*a35*a51*a64 - a16*a22*a35*a54*a61 - a16*a24*a31*a52*a65 + &
      a16*a24*a31*a55*a62 + a16*a24*a32*a51*a65 - a16*a24*a32*a55*a61 - &
      a16*a24*a35*a51*a62 + a16*a24*a35*a52*a61 + a16*a25*a31*a52*a64 - &
      a16*a25*a31*a54*a62 - a16*a25*a32*a51*a64 + a16*a25*a32*a54*a61 + &
      a16*a25*a34*a51*a62 - a16*a25*a34*a52*a61)*d_inv

    A_inv(4, 4) = (a11*a22*a33*a55*a66 - a11*a22*a33*a56* &
      a65 - a11*a22*a35*a53*a66 + a11*a22*a35*a56*a63 + a11*a22*a36*a53 &
      *a65 - a11*a22*a36*a55*a63 - a11*a23*a32*a55*a66 + a11*a23*a32* &
      a56*a65 + a11*a23*a35*a52*a66 - a11*a23*a35*a56*a62 - a11*a23*a36 &
      *a52*a65 + a11*a23*a36*a55*a62 + a11*a25*a32*a53*a66 - a11*a25* &
      a32*a56*a63 - a11*a25*a33*a52*a66 + a11*a25*a33*a56*a62 + a11*a25 &
      *a36*a52*a63 - a11*a25*a36*a53*a62 - a11*a26*a32*a53*a65 + a11* &
      a26*a32*a55*a63 + a11*a26*a33*a52*a65 - a11*a26*a33*a55*a62 - a11 &
      *a26*a35*a52*a63 + a11*a26*a35*a53*a62 - a12*a21*a33*a55*a66 + &
      a12*a21*a33*a56*a65 + a12*a21*a35*a53*a66 - a12*a21*a35*a56*a63 - &
      a12*a21*a36*a53*a65 + a12*a21*a36*a55*a63 + a12*a23*a31*a55*a66 - &
      a12*a23*a31*a56*a65 - a12*a23*a35*a51*a66 + a12*a23*a35*a56*a61 + &
      a12*a23*a36*a51*a65 - a12*a23*a36*a55*a61 - a12*a25*a31*a53*a66 + &
      a12*a25*a31*a56*a63 + a12*a25*a33*a51*a66 - a12*a25*a33*a56*a61 - &
      a12*a25*a36*a51*a63 + a12*a25*a36*a53*a61 + a12*a26*a31*a53*a65 - &
      a12*a26*a31*a55*a63 - a12*a26*a33*a51*a65 + a12*a26*a33*a55*a61 + &
      a12*a26*a35*a51*a63 - a12*a26*a35*a53*a61 + a13*a21*a32*a55*a66 - &
      a13*a21*a32*a56*a65 - a13*a21*a35*a52*a66 + a13*a21*a35*a56*a62 + &
      a13*a21*a36*a52*a65 - a13*a21*a36*a55*a62 - a13*a22*a31*a55*a66 + &
      a13*a22*a31*a56*a65 + a13*a22*a35*a51*a66 - a13*a22*a35*a56*a61 - &
      a13*a22*a36*a51*a65 + a13*a22*a36*a55*a61 + a13*a25*a31*a52*a66 - &
      a13*a25*a31*a56*a62 - a13*a25*a32*a51*a66 + a13*a25*a32*a56*a61 + &
      a13*a25*a36*a51*a62 - a13*a25*a36*a52*a61 - a13*a26*a31*a52*a65 + &
      a13*a26*a31*a55*a62 + a13*a26*a32*a51*a65 - a13*a26*a32*a55*a61 - &
      a13*a26*a35*a51*a62 + a13*a26*a35*a52*a61 - a15*a21*a32*a53*a66 + &
      a15*a21*a32*a56*a63 + a15*a21*a33*a52*a66 - a15*a21*a33*a56*a62 - &
      a15*a21*a36*a52*a63 + a15*a21*a36*a53*a62 + a15*a22*a31*a53*a66 - &
      a15*a22*a31*a56*a63 - a15*a22*a33*a51*a66 + a15*a22*a33*a56*a61 + &
      a15*a22*a36*a51*a63 - a15*a22*a36*a53*a61 - a15*a23*a31*a52*a66 + &
      a15*a23*a31*a56*a62 + a15*a23*a32*a51*a66 - a15*a23*a32*a56*a61 - &
      a15*a23*a36*a51*a62 + a15*a23*a36*a52*a61 + a15*a26*a31*a52*a63 - &
      a15*a26*a31*a53*a62 - a15*a26*a32*a51*a63 + a15*a26*a32*a53*a61 + &
      a15*a26*a33*a51*a62 - a15*a26*a33*a52*a61 + a16*a21*a32*a53*a65 - &
      a16*a21*a32*a55*a63 - a16*a21*a33*a52*a65 + a16*a21*a33*a55*a62 + &
      a16*a21*a35*a52*a63 - a16*a21*a35*a53*a62 - a16*a22*a31*a53*a65 + &
      a16*a22*a31*a55*a63 + a16*a22*a33*a51*a65 - a16*a22*a33*a55*a61 - &
      a16*a22*a35*a51*a63 + a16*a22*a35*a53*a61 + a16*a23*a31*a52*a65 - &
      a16*a23*a31*a55*a62 - a16*a23*a32*a51*a65 + a16*a23*a32*a55*a61 + &
      a16*a23*a35*a51*a62 - a16*a23*a35*a52*a61 - a16*a25*a31*a52*a63 + &
      a16*a25*a31*a53*a62 + a16*a25*a32*a51*a63 - a16*a25*a32*a53*a61 - &
      a16*a25*a33*a51*a62 + a16*a25*a33*a52*a61)*d_inv

    A_inv(5, 4) = (-a11*a22*a33*a54*a66 + a11*a22*a33*a56* &
      a64 + a11*a22*a34*a53*a66 - a11*a22*a34*a56*a63 - a11*a22*a36*a53 &
      *a64 + a11*a22*a36*a54*a63 + a11*a23*a32*a54*a66 - a11*a23*a32* &
      a56*a64 - a11*a23*a34*a52*a66 + a11*a23*a34*a56*a62 + a11*a23*a36 &
      *a52*a64 - a11*a23*a36*a54*a62 - a11*a24*a32*a53*a66 + a11*a24* &
      a32*a56*a63 + a11*a24*a33*a52*a66 - a11*a24*a33*a56*a62 - a11*a24 &
      *a36*a52*a63 + a11*a24*a36*a53*a62 + a11*a26*a32*a53*a64 - a11* &
      a26*a32*a54*a63 - a11*a26*a33*a52*a64 + a11*a26*a33*a54*a62 + a11 &
      *a26*a34*a52*a63 - a11*a26*a34*a53*a62 + a12*a21*a33*a54*a66 - &
      a12*a21*a33*a56*a64 - a12*a21*a34*a53*a66 + a12*a21*a34*a56*a63 + &
      a12*a21*a36*a53*a64 - a12*a21*a36*a54*a63 - a12*a23*a31*a54*a66 + &
      a12*a23*a31*a56*a64 + a12*a23*a34*a51*a66 - a12*a23*a34*a56*a61 - &
      a12*a23*a36*a51*a64 + a12*a23*a36*a54*a61 + a12*a24*a31*a53*a66 - &
      a12*a24*a31*a56*a63 - a12*a24*a33*a51*a66 + a12*a24*a33*a56*a61 + &
      a12*a24*a36*a51*a63 - a12*a24*a36*a53*a61 - a12*a26*a31*a53*a64 + &
      a12*a26*a31*a54*a63 + a12*a26*a33*a51*a64 - a12*a26*a33*a54*a61 - &
      a12*a26*a34*a51*a63 + a12*a26*a34*a53*a61 - a13*a21*a32*a54*a66 + &
      a13*a21*a32*a56*a64 + a13*a21*a34*a52*a66 - a13*a21*a34*a56*a62 - &
      a13*a21*a36*a52*a64 + a13*a21*a36*a54*a62 + a13*a22*a31*a54*a66 - &
      a13*a22*a31*a56*a64 - a13*a22*a34*a51*a66 + a13*a22*a34*a56*a61 + &
      a13*a22*a36*a51*a64 - a13*a22*a36*a54*a61 - a13*a24*a31*a52*a66 + &
      a13*a24*a31*a56*a62 + a13*a24*a32*a51*a66 - a13*a24*a32*a56*a61 - &
      a13*a24*a36*a51*a62 + a13*a24*a36*a52*a61 + a13*a26*a31*a52*a64 - &
      a13*a26*a31*a54*a62 - a13*a26*a32*a51*a64 + a13*a26*a32*a54*a61 + &
      a13*a26*a34*a51*a62 - a13*a26*a34*a52*a61 + a14*a21*a32*a53*a66 - &
      a14*a21*a32*a56*a63 - a14*a21*a33*a52*a66 + a14*a21*a33*a56*a62 + &
      a14*a21*a36*a52*a63 - a14*a21*a36*a53*a62 - a14*a22*a31*a53*a66 + &
      a14*a22*a31*a56*a63 + a14*a22*a33*a51*a66 - a14*a22*a33*a56*a61 - &
      a14*a22*a36*a51*a63 + a14*a22*a36*a53*a61 + a14*a23*a31*a52*a66 - &
      a14*a23*a31*a56*a62 - a14*a23*a32*a51*a66 + a14*a23*a32*a56*a61 + &
      a14*a23*a36*a51*a62 - a14*a23*a36*a52*a61 - a14*a26*a31*a52*a63 + &
      a14*a26*a31*a53*a62 + a14*a26*a32*a51*a63 - a14*a26*a32*a53*a61 - &
      a14*a26*a33*a51*a62 + a14*a26*a33*a52*a61 - a16*a21*a32*a53*a64 + &
      a16*a21*a32*a54*a63 + a16*a21*a33*a52*a64 - a16*a21*a33*a54*a62 - &
      a16*a21*a34*a52*a63 + a16*a21*a34*a53*a62 + a16*a22*a31*a53*a64 - &
      a16*a22*a31*a54*a63 - a16*a22*a33*a51*a64 + a16*a22*a33*a54*a61 + &
      a16*a22*a34*a51*a63 - a16*a22*a34*a53*a61 - a16*a23*a31*a52*a64 + &
      a16*a23*a31*a54*a62 + a16*a23*a32*a51*a64 - a16*a23*a32*a54*a61 - &
      a16*a23*a34*a51*a62 + a16*a23*a34*a52*a61 + a16*a24*a31*a52*a63 - &
      a16*a24*a31*a53*a62 - a16*a24*a32*a51*a63 + a16*a24*a32*a53*a61 + &
      a16*a24*a33*a51*a62 - a16*a24*a33*a52*a61)*d_inv

    A_inv(6, 4) = (a11*a22*a33*a54*a65 - a11*a22*a33*a55* &
      a64 - a11*a22*a34*a53*a65 + a11*a22*a34*a55*a63 + a11*a22*a35*a53 &
      *a64 - a11*a22*a35*a54*a63 - a11*a23*a32*a54*a65 + a11*a23*a32* &
      a55*a64 + a11*a23*a34*a52*a65 - a11*a23*a34*a55*a62 - a11*a23*a35 &
      *a52*a64 + a11*a23*a35*a54*a62 + a11*a24*a32*a53*a65 - a11*a24* &
      a32*a55*a63 - a11*a24*a33*a52*a65 + a11*a24*a33*a55*a62 + a11*a24 &
      *a35*a52*a63 - a11*a24*a35*a53*a62 - a11*a25*a32*a53*a64 + a11* &
      a25*a32*a54*a63 + a11*a25*a33*a52*a64 - a11*a25*a33*a54*a62 - a11 &
      *a25*a34*a52*a63 + a11*a25*a34*a53*a62 - a12*a21*a33*a54*a65 + &
      a12*a21*a33*a55*a64 + a12*a21*a34*a53*a65 - a12*a21*a34*a55*a63 - &
      a12*a21*a35*a53*a64 + a12*a21*a35*a54*a63 + a12*a23*a31*a54*a65 - &
      a12*a23*a31*a55*a64 - a12*a23*a34*a51*a65 + a12*a23*a34*a55*a61 + &
      a12*a23*a35*a51*a64 - a12*a23*a35*a54*a61 - a12*a24*a31*a53*a65 + &
      a12*a24*a31*a55*a63 + a12*a24*a33*a51*a65 - a12*a24*a33*a55*a61 - &
      a12*a24*a35*a51*a63 + a12*a24*a35*a53*a61 + a12*a25*a31*a53*a64 - &
      a12*a25*a31*a54*a63 - a12*a25*a33*a51*a64 + a12*a25*a33*a54*a61 + &
      a12*a25*a34*a51*a63 - a12*a25*a34*a53*a61 + a13*a21*a32*a54*a65 - &
      a13*a21*a32*a55*a64 - a13*a21*a34*a52*a65 + a13*a21*a34*a55*a62 + &
      a13*a21*a35*a52*a64 - a13*a21*a35*a54*a62 - a13*a22*a31*a54*a65 + &
      a13*a22*a31*a55*a64 + a13*a22*a34*a51*a65 - a13*a22*a34*a55*a61 - &
      a13*a22*a35*a51*a64 + a13*a22*a35*a54*a61 + a13*a24*a31*a52*a65 - &
      a13*a24*a31*a55*a62 - a13*a24*a32*a51*a65 + a13*a24*a32*a55*a61 + &
      a13*a24*a35*a51*a62 - a13*a24*a35*a52*a61 - a13*a25*a31*a52*a64 + &
      a13*a25*a31*a54*a62 + a13*a25*a32*a51*a64 - a13*a25*a32*a54*a61 - &
      a13*a25*a34*a51*a62 + a13*a25*a34*a52*a61 - a14*a21*a32*a53*a65 + &
      a14*a21*a32*a55*a63 + a14*a21*a33*a52*a65 - a14*a21*a33*a55*a62 - &
      a14*a21*a35*a52*a63 + a14*a21*a35*a53*a62 + a14*a22*a31*a53*a65 - &
      a14*a22*a31*a55*a63 - a14*a22*a33*a51*a65 + a14*a22*a33*a55*a61 + &
      a14*a22*a35*a51*a63 - a14*a22*a35*a53*a61 - a14*a23*a31*a52*a65 + &
      a14*a23*a31*a55*a62 + a14*a23*a32*a51*a65 - a14*a23*a32*a55*a61 - &
      a14*a23*a35*a51*a62 + a14*a23*a35*a52*a61 + a14*a25*a31*a52*a63 - &
      a14*a25*a31*a53*a62 - a14*a25*a32*a51*a63 + a14*a25*a32*a53*a61 + &
      a14*a25*a33*a51*a62 - a14*a25*a33*a52*a61 + a15*a21*a32*a53*a64 - &
      a15*a21*a32*a54*a63 - a15*a21*a33*a52*a64 + a15*a21*a33*a54*a62 + &
      a15*a21*a34*a52*a63 - a15*a21*a34*a53*a62 - a15*a22*a31*a53*a64 + &
      a15*a22*a31*a54*a63 + a15*a22*a33*a51*a64 - a15*a22*a33*a54*a61 - &
      a15*a22*a34*a51*a63 + a15*a22*a34*a53*a61 + a15*a23*a31*a52*a64 - &
      a15*a23*a31*a54*a62 - a15*a23*a32*a51*a64 + a15*a23*a32*a54*a61 + &
      a15*a23*a34*a51*a62 - a15*a23*a34*a52*a61 - a15*a24*a31*a52*a63 + &
      a15*a24*a31*a53*a62 + a15*a24*a32*a51*a63 - a15*a24*a32*a53*a61 - &
      a15*a24*a33*a51*a62 + a15*a24*a33*a52*a61)*d_inv

    A_inv(1, 5) = (a12*a23*a34*a45*a66 - a12*a23*a34*a46* &
      a65 - a12*a23*a35*a44*a66 + a12*a23*a35*a46*a64 + a12*a23*a36*a44 &
      *a65 - a12*a23*a36*a45*a64 - a12*a24*a33*a45*a66 + a12*a24*a33* &
      a46*a65 + a12*a24*a35*a43*a66 - a12*a24*a35*a46*a63 - a12*a24*a36 &
      *a43*a65 + a12*a24*a36*a45*a63 + a12*a25*a33*a44*a66 - a12*a25* &
      a33*a46*a64 - a12*a25*a34*a43*a66 + a12*a25*a34*a46*a63 + a12*a25 &
      *a36*a43*a64 - a12*a25*a36*a44*a63 - a12*a26*a33*a44*a65 + a12* &
      a26*a33*a45*a64 + a12*a26*a34*a43*a65 - a12*a26*a34*a45*a63 - a12 &
      *a26*a35*a43*a64 + a12*a26*a35*a44*a63 - a13*a22*a34*a45*a66 + &
      a13*a22*a34*a46*a65 + a13*a22*a35*a44*a66 - a13*a22*a35*a46*a64 - &
      a13*a22*a36*a44*a65 + a13*a22*a36*a45*a64 + a13*a24*a32*a45*a66 - &
      a13*a24*a32*a46*a65 - a13*a24*a35*a42*a66 + a13*a24*a35*a46*a62 + &
      a13*a24*a36*a42*a65 - a13*a24*a36*a45*a62 - a13*a25*a32*a44*a66 + &
      a13*a25*a32*a46*a64 + a13*a25*a34*a42*a66 - a13*a25*a34*a46*a62 - &
      a13*a25*a36*a42*a64 + a13*a25*a36*a44*a62 + a13*a26*a32*a44*a65 - &
      a13*a26*a32*a45*a64 - a13*a26*a34*a42*a65 + a13*a26*a34*a45*a62 + &
      a13*a26*a35*a42*a64 - a13*a26*a35*a44*a62 + a14*a22*a33*a45*a66 - &
      a14*a22*a33*a46*a65 - a14*a22*a35*a43*a66 + a14*a22*a35*a46*a63 + &
      a14*a22*a36*a43*a65 - a14*a22*a36*a45*a63 - a14*a23*a32*a45*a66 + &
      a14*a23*a32*a46*a65 + a14*a23*a35*a42*a66 - a14*a23*a35*a46*a62 - &
      a14*a23*a36*a42*a65 + a14*a23*a36*a45*a62 + a14*a25*a32*a43*a66 - &
      a14*a25*a32*a46*a63 - a14*a25*a33*a42*a66 + a14*a25*a33*a46*a62 + &
      a14*a25*a36*a42*a63 - a14*a25*a36*a43*a62 - a14*a26*a32*a43*a65 + &
      a14*a26*a32*a45*a63 + a14*a26*a33*a42*a65 - a14*a26*a33*a45*a62 - &
      a14*a26*a35*a42*a63 + a14*a26*a35*a43*a62 - a15*a22*a33*a44*a66 + &
      a15*a22*a33*a46*a64 + a15*a22*a34*a43*a66 - a15*a22*a34*a46*a63 - &
      a15*a22*a36*a43*a64 + a15*a22*a36*a44*a63 + a15*a23*a32*a44*a66 - &
      a15*a23*a32*a46*a64 - a15*a23*a34*a42*a66 + a15*a23*a34*a46*a62 + &
      a15*a23*a36*a42*a64 - a15*a23*a36*a44*a62 - a15*a24*a32*a43*a66 + &
      a15*a24*a32*a46*a63 + a15*a24*a33*a42*a66 - a15*a24*a33*a46*a62 - &
      a15*a24*a36*a42*a63 + a15*a24*a36*a43*a62 + a15*a26*a32*a43*a64 - &
      a15*a26*a32*a44*a63 - a15*a26*a33*a42*a64 + a15*a26*a33*a44*a62 + &
      a15*a26*a34*a42*a63 - a15*a26*a34*a43*a62 + a16*a22*a33*a44*a65 - &
      a16*a22*a33*a45*a64 - a16*a22*a34*a43*a65 + a16*a22*a34*a45*a63 + &
      a16*a22*a35*a43*a64 - a16*a22*a35*a44*a63 - a16*a23*a32*a44*a65 + &
      a16*a23*a32*a45*a64 + a16*a23*a34*a42*a65 - a16*a23*a34*a45*a62 - &
      a16*a23*a35*a42*a64 + a16*a23*a35*a44*a62 + a16*a24*a32*a43*a65 - &
      a16*a24*a32*a45*a63 - a16*a24*a33*a42*a65 + a16*a24*a33*a45*a62 + &
      a16*a24*a35*a42*a63 - a16*a24*a35*a43*a62 - a16*a25*a32*a43*a64 + &
      a16*a25*a32*a44*a63 + a16*a25*a33*a42*a64 - a16*a25*a33*a44*a62 - &
      a16*a25*a34*a42*a63 + a16*a25*a34*a43*a62)*d_inv

    A_inv(2, 5) = (-a11*a23*a34*a45*a66 + a11*a23*a34*a46* &
      a65 + a11*a23*a35*a44*a66 - a11*a23*a35*a46*a64 - a11*a23*a36*a44 &
      *a65 + a11*a23*a36*a45*a64 + a11*a24*a33*a45*a66 - a11*a24*a33* &
      a46*a65 - a11*a24*a35*a43*a66 + a11*a24*a35*a46*a63 + a11*a24*a36 &
      *a43*a65 - a11*a24*a36*a45*a63 - a11*a25*a33*a44*a66 + a11*a25* &
      a33*a46*a64 + a11*a25*a34*a43*a66 - a11*a25*a34*a46*a63 - a11*a25 &
      *a36*a43*a64 + a11*a25*a36*a44*a63 + a11*a26*a33*a44*a65 - a11* &
      a26*a33*a45*a64 - a11*a26*a34*a43*a65 + a11*a26*a34*a45*a63 + a11 &
      *a26*a35*a43*a64 - a11*a26*a35*a44*a63 + a13*a21*a34*a45*a66 - &
      a13*a21*a34*a46*a65 - a13*a21*a35*a44*a66 + a13*a21*a35*a46*a64 + &
      a13*a21*a36*a44*a65 - a13*a21*a36*a45*a64 - a13*a24*a31*a45*a66 + &
      a13*a24*a31*a46*a65 + a13*a24*a35*a41*a66 - a13*a24*a35*a46*a61 - &
      a13*a24*a36*a41*a65 + a13*a24*a36*a45*a61 + a13*a25*a31*a44*a66 - &
      a13*a25*a31*a46*a64 - a13*a25*a34*a41*a66 + a13*a25*a34*a46*a61 + &
      a13*a25*a36*a41*a64 - a13*a25*a36*a44*a61 - a13*a26*a31*a44*a65 + &
      a13*a26*a31*a45*a64 + a13*a26*a34*a41*a65 - a13*a26*a34*a45*a61 - &
      a13*a26*a35*a41*a64 + a13*a26*a35*a44*a61 - a14*a21*a33*a45*a66 + &
      a14*a21*a33*a46*a65 + a14*a21*a35*a43*a66 - a14*a21*a35*a46*a63 - &
      a14*a21*a36*a43*a65 + a14*a21*a36*a45*a63 + a14*a23*a31*a45*a66 - &
      a14*a23*a31*a46*a65 - a14*a23*a35*a41*a66 + a14*a23*a35*a46*a61 + &
      a14*a23*a36*a41*a65 - a14*a23*a36*a45*a61 - a14*a25*a31*a43*a66 + &
      a14*a25*a31*a46*a63 + a14*a25*a33*a41*a66 - a14*a25*a33*a46*a61 - &
      a14*a25*a36*a41*a63 + a14*a25*a36*a43*a61 + a14*a26*a31*a43*a65 - &
      a14*a26*a31*a45*a63 - a14*a26*a33*a41*a65 + a14*a26*a33*a45*a61 + &
      a14*a26*a35*a41*a63 - a14*a26*a35*a43*a61 + a15*a21*a33*a44*a66 - &
      a15*a21*a33*a46*a64 - a15*a21*a34*a43*a66 + a15*a21*a34*a46*a63 + &
      a15*a21*a36*a43*a64 - a15*a21*a36*a44*a63 - a15*a23*a31*a44*a66 + &
      a15*a23*a31*a46*a64 + a15*a23*a34*a41*a66 - a15*a23*a34*a46*a61 - &
      a15*a23*a36*a41*a64 + a15*a23*a36*a44*a61 + a15*a24*a31*a43*a66 - &
      a15*a24*a31*a46*a63 - a15*a24*a33*a41*a66 + a15*a24*a33*a46*a61 + &
      a15*a24*a36*a41*a63 - a15*a24*a36*a43*a61 - a15*a26*a31*a43*a64 + &
      a15*a26*a31*a44*a63 + a15*a26*a33*a41*a64 - a15*a26*a33*a44*a61 - &
      a15*a26*a34*a41*a63 + a15*a26*a34*a43*a61 - a16*a21*a33*a44*a65 + &
      a16*a21*a33*a45*a64 + a16*a21*a34*a43*a65 - a16*a21*a34*a45*a63 - &
      a16*a21*a35*a43*a64 + a16*a21*a35*a44*a63 + a16*a23*a31*a44*a65 - &
      a16*a23*a31*a45*a64 - a16*a23*a34*a41*a65 + a16*a23*a34*a45*a61 + &
      a16*a23*a35*a41*a64 - a16*a23*a35*a44*a61 - a16*a24*a31*a43*a65 + &
      a16*a24*a31*a45*a63 + a16*a24*a33*a41*a65 - a16*a24*a33*a45*a61 - &
      a16*a24*a35*a41*a63 + a16*a24*a35*a43*a61 + a16*a25*a31*a43*a64 - &
      a16*a25*a31*a44*a63 - a16*a25*a33*a41*a64 + a16*a25*a33*a44*a61 + &
      a16*a25*a34*a41*a63 - a16*a25*a34*a43*a61)*d_inv

    A_inv(3, 5) = (a11*a22*a34*a45*a66 - a11*a22*a34*a46* &
      a65 - a11*a22*a35*a44*a66 + a11*a22*a35*a46*a64 + a11*a22*a36*a44 &
      *a65 - a11*a22*a36*a45*a64 - a11*a24*a32*a45*a66 + a11*a24*a32* &
      a46*a65 + a11*a24*a35*a42*a66 - a11*a24*a35*a46*a62 - a11*a24*a36 &
      *a42*a65 + a11*a24*a36*a45*a62 + a11*a25*a32*a44*a66 - a11*a25* &
      a32*a46*a64 - a11*a25*a34*a42*a66 + a11*a25*a34*a46*a62 + a11*a25 &
      *a36*a42*a64 - a11*a25*a36*a44*a62 - a11*a26*a32*a44*a65 + a11* &
      a26*a32*a45*a64 + a11*a26*a34*a42*a65 - a11*a26*a34*a45*a62 - a11 &
      *a26*a35*a42*a64 + a11*a26*a35*a44*a62 - a12*a21*a34*a45*a66 + &
      a12*a21*a34*a46*a65 + a12*a21*a35*a44*a66 - a12*a21*a35*a46*a64 - &
      a12*a21*a36*a44*a65 + a12*a21*a36*a45*a64 + a12*a24*a31*a45*a66 - &
      a12*a24*a31*a46*a65 - a12*a24*a35*a41*a66 + a12*a24*a35*a46*a61 + &
      a12*a24*a36*a41*a65 - a12*a24*a36*a45*a61 - a12*a25*a31*a44*a66 + &
      a12*a25*a31*a46*a64 + a12*a25*a34*a41*a66 - a12*a25*a34*a46*a61 - &
      a12*a25*a36*a41*a64 + a12*a25*a36*a44*a61 + a12*a26*a31*a44*a65 - &
      a12*a26*a31*a45*a64 - a12*a26*a34*a41*a65 + a12*a26*a34*a45*a61 + &
      a12*a26*a35*a41*a64 - a12*a26*a35*a44*a61 + a14*a21*a32*a45*a66 - &
      a14*a21*a32*a46*a65 - a14*a21*a35*a42*a66 + a14*a21*a35*a46*a62 + &
      a14*a21*a36*a42*a65 - a14*a21*a36*a45*a62 - a14*a22*a31*a45*a66 + &
      a14*a22*a31*a46*a65 + a14*a22*a35*a41*a66 - a14*a22*a35*a46*a61 - &
      a14*a22*a36*a41*a65 + a14*a22*a36*a45*a61 + a14*a25*a31*a42*a66 - &
      a14*a25*a31*a46*a62 - a14*a25*a32*a41*a66 + a14*a25*a32*a46*a61 + &
      a14*a25*a36*a41*a62 - a14*a25*a36*a42*a61 - a14*a26*a31*a42*a65 + &
      a14*a26*a31*a45*a62 + a14*a26*a32*a41*a65 - a14*a26*a32*a45*a61 - &
      a14*a26*a35*a41*a62 + a14*a26*a35*a42*a61 - a15*a21*a32*a44*a66 + &
      a15*a21*a32*a46*a64 + a15*a21*a34*a42*a66 - a15*a21*a34*a46*a62 - &
      a15*a21*a36*a42*a64 + a15*a21*a36*a44*a62 + a15*a22*a31*a44*a66 - &
      a15*a22*a31*a46*a64 - a15*a22*a34*a41*a66 + a15*a22*a34*a46*a61 + &
      a15*a22*a36*a41*a64 - a15*a22*a36*a44*a61 - a15*a24*a31*a42*a66 + &
      a15*a24*a31*a46*a62 + a15*a24*a32*a41*a66 - a15*a24*a32*a46*a61 - &
      a15*a24*a36*a41*a62 + a15*a24*a36*a42*a61 + a15*a26*a31*a42*a64 - &
      a15*a26*a31*a44*a62 - a15*a26*a32*a41*a64 + a15*a26*a32*a44*a61 + &
      a15*a26*a34*a41*a62 - a15*a26*a34*a42*a61 + a16*a21*a32*a44*a65 - &
      a16*a21*a32*a45*a64 - a16*a21*a34*a42*a65 + a16*a21*a34*a45*a62 + &
      a16*a21*a35*a42*a64 - a16*a21*a35*a44*a62 - a16*a22*a31*a44*a65 + &
      a16*a22*a31*a45*a64 + a16*a22*a34*a41*a65 - a16*a22*a34*a45*a61 - &
      a16*a22*a35*a41*a64 + a16*a22*a35*a44*a61 + a16*a24*a31*a42*a65 - &
      a16*a24*a31*a45*a62 - a16*a24*a32*a41*a65 + a16*a24*a32*a45*a61 + &
      a16*a24*a35*a41*a62 - a16*a24*a35*a42*a61 - a16*a25*a31*a42*a64 + &
      a16*a25*a31*a44*a62 + a16*a25*a32*a41*a64 - a16*a25*a32*a44*a61 - &
      a16*a25*a34*a41*a62 + a16*a25*a34*a42*a61)*d_inv

    A_inv(4, 5) = (-a11*a22*a33*a45*a66 + a11*a22*a33*a46* &
      a65 + a11*a22*a35*a43*a66 - a11*a22*a35*a46*a63 - a11*a22*a36*a43 &
      *a65 + a11*a22*a36*a45*a63 + a11*a23*a32*a45*a66 - a11*a23*a32* &
      a46*a65 - a11*a23*a35*a42*a66 + a11*a23*a35*a46*a62 + a11*a23*a36 &
      *a42*a65 - a11*a23*a36*a45*a62 - a11*a25*a32*a43*a66 + a11*a25* &
      a32*a46*a63 + a11*a25*a33*a42*a66 - a11*a25*a33*a46*a62 - a11*a25 &
      *a36*a42*a63 + a11*a25*a36*a43*a62 + a11*a26*a32*a43*a65 - a11* &
      a26*a32*a45*a63 - a11*a26*a33*a42*a65 + a11*a26*a33*a45*a62 + a11 &
      *a26*a35*a42*a63 - a11*a26*a35*a43*a62 + a12*a21*a33*a45*a66 - &
      a12*a21*a33*a46*a65 - a12*a21*a35*a43*a66 + a12*a21*a35*a46*a63 + &
      a12*a21*a36*a43*a65 - a12*a21*a36*a45*a63 - a12*a23*a31*a45*a66 + &
      a12*a23*a31*a46*a65 + a12*a23*a35*a41*a66 - a12*a23*a35*a46*a61 - &
      a12*a23*a36*a41*a65 + a12*a23*a36*a45*a61 + a12*a25*a31*a43*a66 - &
      a12*a25*a31*a46*a63 - a12*a25*a33*a41*a66 + a12*a25*a33*a46*a61 + &
      a12*a25*a36*a41*a63 - a12*a25*a36*a43*a61 - a12*a26*a31*a43*a65 + &
      a12*a26*a31*a45*a63 + a12*a26*a33*a41*a65 - a12*a26*a33*a45*a61 - &
      a12*a26*a35*a41*a63 + a12*a26*a35*a43*a61 - a13*a21*a32*a45*a66 + &
      a13*a21*a32*a46*a65 + a13*a21*a35*a42*a66 - a13*a21*a35*a46*a62 - &
      a13*a21*a36*a42*a65 + a13*a21*a36*a45*a62 + a13*a22*a31*a45*a66 - &
      a13*a22*a31*a46*a65 - a13*a22*a35*a41*a66 + a13*a22*a35*a46*a61 + &
      a13*a22*a36*a41*a65 - a13*a22*a36*a45*a61 - a13*a25*a31*a42*a66 + &
      a13*a25*a31*a46*a62 + a13*a25*a32*a41*a66 - a13*a25*a32*a46*a61 - &
      a13*a25*a36*a41*a62 + a13*a25*a36*a42*a61 + a13*a26*a31*a42*a65 - &
      a13*a26*a31*a45*a62 - a13*a26*a32*a41*a65 + a13*a26*a32*a45*a61 + &
      a13*a26*a35*a41*a62 - a13*a26*a35*a42*a61 + a15*a21*a32*a43*a66 - &
      a15*a21*a32*a46*a63 - a15*a21*a33*a42*a66 + a15*a21*a33*a46*a62 + &
      a15*a21*a36*a42*a63 - a15*a21*a36*a43*a62 - a15*a22*a31*a43*a66 + &
      a15*a22*a31*a46*a63 + a15*a22*a33*a41*a66 - a15*a22*a33*a46*a61 - &
      a15*a22*a36*a41*a63 + a15*a22*a36*a43*a61 + a15*a23*a31*a42*a66 - &
      a15*a23*a31*a46*a62 - a15*a23*a32*a41*a66 + a15*a23*a32*a46*a61 + &
      a15*a23*a36*a41*a62 - a15*a23*a36*a42*a61 - a15*a26*a31*a42*a63 + &
      a15*a26*a31*a43*a62 + a15*a26*a32*a41*a63 - a15*a26*a32*a43*a61 - &
      a15*a26*a33*a41*a62 + a15*a26*a33*a42*a61 - a16*a21*a32*a43*a65 + &
      a16*a21*a32*a45*a63 + a16*a21*a33*a42*a65 - a16*a21*a33*a45*a62 - &
      a16*a21*a35*a42*a63 + a16*a21*a35*a43*a62 + a16*a22*a31*a43*a65 - &
      a16*a22*a31*a45*a63 - a16*a22*a33*a41*a65 + a16*a22*a33*a45*a61 + &
      a16*a22*a35*a41*a63 - a16*a22*a35*a43*a61 - a16*a23*a31*a42*a65 + &
      a16*a23*a31*a45*a62 + a16*a23*a32*a41*a65 - a16*a23*a32*a45*a61 - &
      a16*a23*a35*a41*a62 + a16*a23*a35*a42*a61 + a16*a25*a31*a42*a63 - &
      a16*a25*a31*a43*a62 - a16*a25*a32*a41*a63 + a16*a25*a32*a43*a61 + &
      a16*a25*a33*a41*a62 - a16*a25*a33*a42*a61)*d_inv

    A_inv(5, 5) = (a11*a22*a33*a44*a66 - a11*a22*a33*a46* &
      a64 - a11*a22*a34*a43*a66 + a11*a22*a34*a46*a63 + a11*a22*a36*a43 &
      *a64 - a11*a22*a36*a44*a63 - a11*a23*a32*a44*a66 + a11*a23*a32* &
      a46*a64 + a11*a23*a34*a42*a66 - a11*a23*a34*a46*a62 - a11*a23*a36 &
      *a42*a64 + a11*a23*a36*a44*a62 + a11*a24*a32*a43*a66 - a11*a24* &
      a32*a46*a63 - a11*a24*a33*a42*a66 + a11*a24*a33*a46*a62 + a11*a24 &
      *a36*a42*a63 - a11*a24*a36*a43*a62 - a11*a26*a32*a43*a64 + a11* &
      a26*a32*a44*a63 + a11*a26*a33*a42*a64 - a11*a26*a33*a44*a62 - a11 &
      *a26*a34*a42*a63 + a11*a26*a34*a43*a62 - a12*a21*a33*a44*a66 + &
      a12*a21*a33*a46*a64 + a12*a21*a34*a43*a66 - a12*a21*a34*a46*a63 - &
      a12*a21*a36*a43*a64 + a12*a21*a36*a44*a63 + a12*a23*a31*a44*a66 - &
      a12*a23*a31*a46*a64 - a12*a23*a34*a41*a66 + a12*a23*a34*a46*a61 + &
      a12*a23*a36*a41*a64 - a12*a23*a36*a44*a61 - a12*a24*a31*a43*a66 + &
      a12*a24*a31*a46*a63 + a12*a24*a33*a41*a66 - a12*a24*a33*a46*a61 - &
      a12*a24*a36*a41*a63 + a12*a24*a36*a43*a61 + a12*a26*a31*a43*a64 - &
      a12*a26*a31*a44*a63 - a12*a26*a33*a41*a64 + a12*a26*a33*a44*a61 + &
      a12*a26*a34*a41*a63 - a12*a26*a34*a43*a61 + a13*a21*a32*a44*a66 - &
      a13*a21*a32*a46*a64 - a13*a21*a34*a42*a66 + a13*a21*a34*a46*a62 + &
      a13*a21*a36*a42*a64 - a13*a21*a36*a44*a62 - a13*a22*a31*a44*a66 + &
      a13*a22*a31*a46*a64 + a13*a22*a34*a41*a66 - a13*a22*a34*a46*a61 - &
      a13*a22*a36*a41*a64 + a13*a22*a36*a44*a61 + a13*a24*a31*a42*a66 - &
      a13*a24*a31*a46*a62 - a13*a24*a32*a41*a66 + a13*a24*a32*a46*a61 + &
      a13*a24*a36*a41*a62 - a13*a24*a36*a42*a61 - a13*a26*a31*a42*a64 + &
      a13*a26*a31*a44*a62 + a13*a26*a32*a41*a64 - a13*a26*a32*a44*a61 - &
      a13*a26*a34*a41*a62 + a13*a26*a34*a42*a61 - a14*a21*a32*a43*a66 + &
      a14*a21*a32*a46*a63 + a14*a21*a33*a42*a66 - a14*a21*a33*a46*a62 - &
      a14*a21*a36*a42*a63 + a14*a21*a36*a43*a62 + a14*a22*a31*a43*a66 - &
      a14*a22*a31*a46*a63 - a14*a22*a33*a41*a66 + a14*a22*a33*a46*a61 + &
      a14*a22*a36*a41*a63 - a14*a22*a36*a43*a61 - a14*a23*a31*a42*a66 + &
      a14*a23*a31*a46*a62 + a14*a23*a32*a41*a66 - a14*a23*a32*a46*a61 - &
      a14*a23*a36*a41*a62 + a14*a23*a36*a42*a61 + a14*a26*a31*a42*a63 - &
      a14*a26*a31*a43*a62 - a14*a26*a32*a41*a63 + a14*a26*a32*a43*a61 + &
      a14*a26*a33*a41*a62 - a14*a26*a33*a42*a61 + a16*a21*a32*a43*a64 - &
      a16*a21*a32*a44*a63 - a16*a21*a33*a42*a64 + a16*a21*a33*a44*a62 + &
      a16*a21*a34*a42*a63 - a16*a21*a34*a43*a62 - a16*a22*a31*a43*a64 + &
      a16*a22*a31*a44*a63 + a16*a22*a33*a41*a64 - a16*a22*a33*a44*a61 - &
      a16*a22*a34*a41*a63 + a16*a22*a34*a43*a61 + a16*a23*a31*a42*a64 - &
      a16*a23*a31*a44*a62 - a16*a23*a32*a41*a64 + a16*a23*a32*a44*a61 + &
      a16*a23*a34*a41*a62 - a16*a23*a34*a42*a61 - a16*a24*a31*a42*a63 + &
      a16*a24*a31*a43*a62 + a16*a24*a32*a41*a63 - a16*a24*a32*a43*a61 - &
      a16*a24*a33*a41*a62 + a16*a24*a33*a42*a61)*d_inv

    A_inv(6, 5) = (-a11*a22*a33*a44*a65 + a11*a22*a33*a45* &
      a64 + a11*a22*a34*a43*a65 - a11*a22*a34*a45*a63 - a11*a22*a35*a43 &
      *a64 + a11*a22*a35*a44*a63 + a11*a23*a32*a44*a65 - a11*a23*a32* &
      a45*a64 - a11*a23*a34*a42*a65 + a11*a23*a34*a45*a62 + a11*a23*a35 &
      *a42*a64 - a11*a23*a35*a44*a62 - a11*a24*a32*a43*a65 + a11*a24* &
      a32*a45*a63 + a11*a24*a33*a42*a65 - a11*a24*a33*a45*a62 - a11*a24 &
      *a35*a42*a63 + a11*a24*a35*a43*a62 + a11*a25*a32*a43*a64 - a11* &
      a25*a32*a44*a63 - a11*a25*a33*a42*a64 + a11*a25*a33*a44*a62 + a11 &
      *a25*a34*a42*a63 - a11*a25*a34*a43*a62 + a12*a21*a33*a44*a65 - &
      a12*a21*a33*a45*a64 - a12*a21*a34*a43*a65 + a12*a21*a34*a45*a63 + &
      a12*a21*a35*a43*a64 - a12*a21*a35*a44*a63 - a12*a23*a31*a44*a65 + &
      a12*a23*a31*a45*a64 + a12*a23*a34*a41*a65 - a12*a23*a34*a45*a61 - &
      a12*a23*a35*a41*a64 + a12*a23*a35*a44*a61 + a12*a24*a31*a43*a65 - &
      a12*a24*a31*a45*a63 - a12*a24*a33*a41*a65 + a12*a24*a33*a45*a61 + &
      a12*a24*a35*a41*a63 - a12*a24*a35*a43*a61 - a12*a25*a31*a43*a64 + &
      a12*a25*a31*a44*a63 + a12*a25*a33*a41*a64 - a12*a25*a33*a44*a61 - &
      a12*a25*a34*a41*a63 + a12*a25*a34*a43*a61 - a13*a21*a32*a44*a65 + &
      a13*a21*a32*a45*a64 + a13*a21*a34*a42*a65 - a13*a21*a34*a45*a62 - &
      a13*a21*a35*a42*a64 + a13*a21*a35*a44*a62 + a13*a22*a31*a44*a65 - &
      a13*a22*a31*a45*a64 - a13*a22*a34*a41*a65 + a13*a22*a34*a45*a61 + &
      a13*a22*a35*a41*a64 - a13*a22*a35*a44*a61 - a13*a24*a31*a42*a65 + &
      a13*a24*a31*a45*a62 + a13*a24*a32*a41*a65 - a13*a24*a32*a45*a61 - &
      a13*a24*a35*a41*a62 + a13*a24*a35*a42*a61 + a13*a25*a31*a42*a64 - &
      a13*a25*a31*a44*a62 - a13*a25*a32*a41*a64 + a13*a25*a32*a44*a61 + &
      a13*a25*a34*a41*a62 - a13*a25*a34*a42*a61 + a14*a21*a32*a43*a65 - &
      a14*a21*a32*a45*a63 - a14*a21*a33*a42*a65 + a14*a21*a33*a45*a62 + &
      a14*a21*a35*a42*a63 - a14*a21*a35*a43*a62 - a14*a22*a31*a43*a65 + &
      a14*a22*a31*a45*a63 + a14*a22*a33*a41*a65 - a14*a22*a33*a45*a61 - &
      a14*a22*a35*a41*a63 + a14*a22*a35*a43*a61 + a14*a23*a31*a42*a65 - &
      a14*a23*a31*a45*a62 - a14*a23*a32*a41*a65 + a14*a23*a32*a45*a61 + &
      a14*a23*a35*a41*a62 - a14*a23*a35*a42*a61 - a14*a25*a31*a42*a63 + &
      a14*a25*a31*a43*a62 + a14*a25*a32*a41*a63 - a14*a25*a32*a43*a61 - &
      a14*a25*a33*a41*a62 + a14*a25*a33*a42*a61 - a15*a21*a32*a43*a64 + &
      a15*a21*a32*a44*a63 + a15*a21*a33*a42*a64 - a15*a21*a33*a44*a62 - &
      a15*a21*a34*a42*a63 + a15*a21*a34*a43*a62 + a15*a22*a31*a43*a64 - &
      a15*a22*a31*a44*a63 - a15*a22*a33*a41*a64 + a15*a22*a33*a44*a61 + &
      a15*a22*a34*a41*a63 - a15*a22*a34*a43*a61 - a15*a23*a31*a42*a64 + &
      a15*a23*a31*a44*a62 + a15*a23*a32*a41*a64 - a15*a23*a32*a44*a61 - &
      a15*a23*a34*a41*a62 + a15*a23*a34*a42*a61 + a15*a24*a31*a42*a63 - &
      a15*a24*a31*a43*a62 - a15*a24*a32*a41*a63 + a15*a24*a32*a43*a61 + &
      a15*a24*a33*a41*a62 - a15*a24*a33*a42*a61)*d_inv
      
    A_inv(1, 6) = (-a12*a23*a34*a45*a56 + a12*a23*a34*a46* &
      a55 + a12*a23*a35*a44*a56 - a12*a23*a35*a46*a54 - a12*a23*a36*a44 &
      *a55 + a12*a23*a36*a45*a54 + a12*a24*a33*a45*a56 - a12*a24*a33* &
      a46*a55 - a12*a24*a35*a43*a56 + a12*a24*a35*a46*a53 + a12*a24*a36 &
      *a43*a55 - a12*a24*a36*a45*a53 - a12*a25*a33*a44*a56 + a12*a25* &
      a33*a46*a54 + a12*a25*a34*a43*a56 - a12*a25*a34*a46*a53 - a12*a25 &
      *a36*a43*a54 + a12*a25*a36*a44*a53 + a12*a26*a33*a44*a55 - a12* &
      a26*a33*a45*a54 - a12*a26*a34*a43*a55 + a12*a26*a34*a45*a53 + a12 &
      *a26*a35*a43*a54 - a12*a26*a35*a44*a53 + a13*a22*a34*a45*a56 - &
      a13*a22*a34*a46*a55 - a13*a22*a35*a44*a56 + a13*a22*a35*a46*a54 + &
      a13*a22*a36*a44*a55 - a13*a22*a36*a45*a54 - a13*a24*a32*a45*a56 + &
      a13*a24*a32*a46*a55 + a13*a24*a35*a42*a56 - a13*a24*a35*a46*a52 - &
      a13*a24*a36*a42*a55 + a13*a24*a36*a45*a52 + a13*a25*a32*a44*a56 - &
      a13*a25*a32*a46*a54 - a13*a25*a34*a42*a56 + a13*a25*a34*a46*a52 + &
      a13*a25*a36*a42*a54 - a13*a25*a36*a44*a52 - a13*a26*a32*a44*a55 + &
      a13*a26*a32*a45*a54 + a13*a26*a34*a42*a55 - a13*a26*a34*a45*a52 - &
      a13*a26*a35*a42*a54 + a13*a26*a35*a44*a52 - a14*a22*a33*a45*a56 + &
      a14*a22*a33*a46*a55 + a14*a22*a35*a43*a56 - a14*a22*a35*a46*a53 - &
      a14*a22*a36*a43*a55 + a14*a22*a36*a45*a53 + a14*a23*a32*a45*a56 - &
      a14*a23*a32*a46*a55 - a14*a23*a35*a42*a56 + a14*a23*a35*a46*a52 + &
      a14*a23*a36*a42*a55 - a14*a23*a36*a45*a52 - a14*a25*a32*a43*a56 + &
      a14*a25*a32*a46*a53 + a14*a25*a33*a42*a56 - a14*a25*a33*a46*a52 - &
      a14*a25*a36*a42*a53 + a14*a25*a36*a43*a52 + a14*a26*a32*a43*a55 - &
      a14*a26*a32*a45*a53 - a14*a26*a33*a42*a55 + a14*a26*a33*a45*a52 + &
      a14*a26*a35*a42*a53 - a14*a26*a35*a43*a52 + a15*a22*a33*a44*a56 - &
      a15*a22*a33*a46*a54 - a15*a22*a34*a43*a56 + a15*a22*a34*a46*a53 + &
      a15*a22*a36*a43*a54 - a15*a22*a36*a44*a53 - a15*a23*a32*a44*a56 + &
      a15*a23*a32*a46*a54 + a15*a23*a34*a42*a56 - a15*a23*a34*a46*a52 - &
      a15*a23*a36*a42*a54 + a15*a23*a36*a44*a52 + a15*a24*a32*a43*a56 - &
      a15*a24*a32*a46*a53 - a15*a24*a33*a42*a56 + a15*a24*a33*a46*a52 + &
      a15*a24*a36*a42*a53 - a15*a24*a36*a43*a52 - a15*a26*a32*a43*a54 + &
      a15*a26*a32*a44*a53 + a15*a26*a33*a42*a54 - a15*a26*a33*a44*a52 - &
      a15*a26*a34*a42*a53 + a15*a26*a34*a43*a52 - a16*a22*a33*a44*a55 + &
      a16*a22*a33*a45*a54 + a16*a22*a34*a43*a55 - a16*a22*a34*a45*a53 - &
      a16*a22*a35*a43*a54 + a16*a22*a35*a44*a53 + a16*a23*a32*a44*a55 - &
      a16*a23*a32*a45*a54 - a16*a23*a34*a42*a55 + a16*a23*a34*a45*a52 + &
      a16*a23*a35*a42*a54 - a16*a23*a35*a44*a52 - a16*a24*a32*a43*a55 + &
      a16*a24*a32*a45*a53 + a16*a24*a33*a42*a55 - a16*a24*a33*a45*a52 - &
      a16*a24*a35*a42*a53 + a16*a24*a35*a43*a52 + a16*a25*a32*a43*a54 - &
      a16*a25*a32*a44*a53 - a16*a25*a33*a42*a54 + a16*a25*a33*a44*a52 + &
      a16*a25*a34*a42*a53 - a16*a25*a34*a43*a52)*d_inv

    A_inv(2, 6) = (a11*a23*a34*a45*a56 - a11*a23*a34*a46* &
      a55 - a11*a23*a35*a44*a56 + a11*a23*a35*a46*a54 + a11*a23*a36*a44 &
      *a55 - a11*a23*a36*a45*a54 - a11*a24*a33*a45*a56 + a11*a24*a33* &
      a46*a55 + a11*a24*a35*a43*a56 - a11*a24*a35*a46*a53 - a11*a24*a36 &
      *a43*a55 + a11*a24*a36*a45*a53 + a11*a25*a33*a44*a56 - a11*a25* &
      a33*a46*a54 - a11*a25*a34*a43*a56 + a11*a25*a34*a46*a53 + a11*a25 &
      *a36*a43*a54 - a11*a25*a36*a44*a53 - a11*a26*a33*a44*a55 + a11* &
      a26*a33*a45*a54 + a11*a26*a34*a43*a55 - a11*a26*a34*a45*a53 - a11 &
      *a26*a35*a43*a54 + a11*a26*a35*a44*a53 - a13*a21*a34*a45*a56 + &
      a13*a21*a34*a46*a55 + a13*a21*a35*a44*a56 - a13*a21*a35*a46*a54 - &
      a13*a21*a36*a44*a55 + a13*a21*a36*a45*a54 + a13*a24*a31*a45*a56 - &
      a13*a24*a31*a46*a55 - a13*a24*a35*a41*a56 + a13*a24*a35*a46*a51 + &
      a13*a24*a36*a41*a55 - a13*a24*a36*a45*a51 - a13*a25*a31*a44*a56 + &
      a13*a25*a31*a46*a54 + a13*a25*a34*a41*a56 - a13*a25*a34*a46*a51 - &
      a13*a25*a36*a41*a54 + a13*a25*a36*a44*a51 + a13*a26*a31*a44*a55 - &
      a13*a26*a31*a45*a54 - a13*a26*a34*a41*a55 + a13*a26*a34*a45*a51 + &
      a13*a26*a35*a41*a54 - a13*a26*a35*a44*a51 + a14*a21*a33*a45*a56 - &
      a14*a21*a33*a46*a55 - a14*a21*a35*a43*a56 + a14*a21*a35*a46*a53 + &
      a14*a21*a36*a43*a55 - a14*a21*a36*a45*a53 - a14*a23*a31*a45*a56 + &
      a14*a23*a31*a46*a55 + a14*a23*a35*a41*a56 - a14*a23*a35*a46*a51 - &
      a14*a23*a36*a41*a55 + a14*a23*a36*a45*a51 + a14*a25*a31*a43*a56 - &
      a14*a25*a31*a46*a53 - a14*a25*a33*a41*a56 + a14*a25*a33*a46*a51 + &
      a14*a25*a36*a41*a53 - a14*a25*a36*a43*a51 - a14*a26*a31*a43*a55 + &
      a14*a26*a31*a45*a53 + a14*a26*a33*a41*a55 - a14*a26*a33*a45*a51 - &
      a14*a26*a35*a41*a53 + a14*a26*a35*a43*a51 - a15*a21*a33*a44*a56 + &
      a15*a21*a33*a46*a54 + a15*a21*a34*a43*a56 - a15*a21*a34*a46*a53 - &
      a15*a21*a36*a43*a54 + a15*a21*a36*a44*a53 + a15*a23*a31*a44*a56 - &
      a15*a23*a31*a46*a54 - a15*a23*a34*a41*a56 + a15*a23*a34*a46*a51 + &
      a15*a23*a36*a41*a54 - a15*a23*a36*a44*a51 - a15*a24*a31*a43*a56 + &
      a15*a24*a31*a46*a53 + a15*a24*a33*a41*a56 - a15*a24*a33*a46*a51 - &
      a15*a24*a36*a41*a53 + a15*a24*a36*a43*a51 + a15*a26*a31*a43*a54 - &
      a15*a26*a31*a44*a53 - a15*a26*a33*a41*a54 + a15*a26*a33*a44*a51 + &
      a15*a26*a34*a41*a53 - a15*a26*a34*a43*a51 + a16*a21*a33*a44*a55 - &
      a16*a21*a33*a45*a54 - a16*a21*a34*a43*a55 + a16*a21*a34*a45*a53 + &
      a16*a21*a35*a43*a54 - a16*a21*a35*a44*a53 - a16*a23*a31*a44*a55 + &
      a16*a23*a31*a45*a54 + a16*a23*a34*a41*a55 - a16*a23*a34*a45*a51 - &
      a16*a23*a35*a41*a54 + a16*a23*a35*a44*a51 + a16*a24*a31*a43*a55 - &
      a16*a24*a31*a45*a53 - a16*a24*a33*a41*a55 + a16*a24*a33*a45*a51 + &
      a16*a24*a35*a41*a53 - a16*a24*a35*a43*a51 - a16*a25*a31*a43*a54 + &
      a16*a25*a31*a44*a53 + a16*a25*a33*a41*a54 - a16*a25*a33*a44*a51 - &
      a16*a25*a34*a41*a53 + a16*a25*a34*a43*a51)*d_inv

    A_inv(3, 6) = (-a11*a22*a34*a45*a56 + a11*a22*a34*a46* &
      a55 + a11*a22*a35*a44*a56 - a11*a22*a35*a46*a54 - a11*a22*a36*a44 &
      *a55 + a11*a22*a36*a45*a54 + a11*a24*a32*a45*a56 - a11*a24*a32* &
      a46*a55 - a11*a24*a35*a42*a56 + a11*a24*a35*a46*a52 + a11*a24*a36 &
      *a42*a55 - a11*a24*a36*a45*a52 - a11*a25*a32*a44*a56 + a11*a25* &
      a32*a46*a54 + a11*a25*a34*a42*a56 - a11*a25*a34*a46*a52 - a11*a25 &
      *a36*a42*a54 + a11*a25*a36*a44*a52 + a11*a26*a32*a44*a55 - a11* &
      a26*a32*a45*a54 - a11*a26*a34*a42*a55 + a11*a26*a34*a45*a52 + a11 &
      *a26*a35*a42*a54 - a11*a26*a35*a44*a52 + a12*a21*a34*a45*a56 - &
      a12*a21*a34*a46*a55 - a12*a21*a35*a44*a56 + a12*a21*a35*a46*a54 + &
      a12*a21*a36*a44*a55 - a12*a21*a36*a45*a54 - a12*a24*a31*a45*a56 + &
      a12*a24*a31*a46*a55 + a12*a24*a35*a41*a56 - a12*a24*a35*a46*a51 - &
      a12*a24*a36*a41*a55 + a12*a24*a36*a45*a51 + a12*a25*a31*a44*a56 - &
      a12*a25*a31*a46*a54 - a12*a25*a34*a41*a56 + a12*a25*a34*a46*a51 + &
      a12*a25*a36*a41*a54 - a12*a25*a36*a44*a51 - a12*a26*a31*a44*a55 + &
      a12*a26*a31*a45*a54 + a12*a26*a34*a41*a55 - a12*a26*a34*a45*a51 - &
      a12*a26*a35*a41*a54 + a12*a26*a35*a44*a51 - a14*a21*a32*a45*a56 + &
      a14*a21*a32*a46*a55 + a14*a21*a35*a42*a56 - a14*a21*a35*a46*a52 - &
      a14*a21*a36*a42*a55 + a14*a21*a36*a45*a52 + a14*a22*a31*a45*a56 - &
      a14*a22*a31*a46*a55 - a14*a22*a35*a41*a56 + a14*a22*a35*a46*a51 + &
      a14*a22*a36*a41*a55 - a14*a22*a36*a45*a51 - a14*a25*a31*a42*a56 + &
      a14*a25*a31*a46*a52 + a14*a25*a32*a41*a56 - a14*a25*a32*a46*a51 - &
      a14*a25*a36*a41*a52 + a14*a25*a36*a42*a51 + a14*a26*a31*a42*a55 - &
      a14*a26*a31*a45*a52 - a14*a26*a32*a41*a55 + a14*a26*a32*a45*a51 + &
      a14*a26*a35*a41*a52 - a14*a26*a35*a42*a51 + a15*a21*a32*a44*a56 - &
      a15*a21*a32*a46*a54 - a15*a21*a34*a42*a56 + a15*a21*a34*a46*a52 + &
      a15*a21*a36*a42*a54 - a15*a21*a36*a44*a52 - a15*a22*a31*a44*a56 + &
      a15*a22*a31*a46*a54 + a15*a22*a34*a41*a56 - a15*a22*a34*a46*a51 - &
      a15*a22*a36*a41*a54 + a15*a22*a36*a44*a51 + a15*a24*a31*a42*a56 - &
      a15*a24*a31*a46*a52 - a15*a24*a32*a41*a56 + a15*a24*a32*a46*a51 + &
      a15*a24*a36*a41*a52 - a15*a24*a36*a42*a51 - a15*a26*a31*a42*a54 + &
      a15*a26*a31*a44*a52 + a15*a26*a32*a41*a54 - a15*a26*a32*a44*a51 - &
      a15*a26*a34*a41*a52 + a15*a26*a34*a42*a51 - a16*a21*a32*a44*a55 + &
      a16*a21*a32*a45*a54 + a16*a21*a34*a42*a55 - a16*a21*a34*a45*a52 - &
      a16*a21*a35*a42*a54 + a16*a21*a35*a44*a52 + a16*a22*a31*a44*a55 - &
      a16*a22*a31*a45*a54 - a16*a22*a34*a41*a55 + a16*a22*a34*a45*a51 + &
      a16*a22*a35*a41*a54 - a16*a22*a35*a44*a51 - a16*a24*a31*a42*a55 + &
      a16*a24*a31*a45*a52 + a16*a24*a32*a41*a55 - a16*a24*a32*a45*a51 - &
      a16*a24*a35*a41*a52 + a16*a24*a35*a42*a51 + a16*a25*a31*a42*a54 - &
      a16*a25*a31*a44*a52 - a16*a25*a32*a41*a54 + a16*a25*a32*a44*a51 + &
      a16*a25*a34*a41*a52 - a16*a25*a34*a42*a51)*d_inv

    A_inv(4, 6) = (a11*a22*a33*a45*a56 - a11*a22*a33*a46* &
      a55 - a11*a22*a35*a43*a56 + a11*a22*a35*a46*a53 + a11*a22*a36*a43 &
      *a55 - a11*a22*a36*a45*a53 - a11*a23*a32*a45*a56 + a11*a23*a32* &
      a46*a55 + a11*a23*a35*a42*a56 - a11*a23*a35*a46*a52 - a11*a23*a36 &
      *a42*a55 + a11*a23*a36*a45*a52 + a11*a25*a32*a43*a56 - a11*a25* &
      a32*a46*a53 - a11*a25*a33*a42*a56 + a11*a25*a33*a46*a52 + a11*a25 &
      *a36*a42*a53 - a11*a25*a36*a43*a52 - a11*a26*a32*a43*a55 + a11* &
      a26*a32*a45*a53 + a11*a26*a33*a42*a55 - a11*a26*a33*a45*a52 - a11 &
      *a26*a35*a42*a53 + a11*a26*a35*a43*a52 - a12*a21*a33*a45*a56 + &
      a12*a21*a33*a46*a55 + a12*a21*a35*a43*a56 - a12*a21*a35*a46*a53 - &
      a12*a21*a36*a43*a55 + a12*a21*a36*a45*a53 + a12*a23*a31*a45*a56 - &
      a12*a23*a31*a46*a55 - a12*a23*a35*a41*a56 + a12*a23*a35*a46*a51 + &
      a12*a23*a36*a41*a55 - a12*a23*a36*a45*a51 - a12*a25*a31*a43*a56 + &
      a12*a25*a31*a46*a53 + a12*a25*a33*a41*a56 - a12*a25*a33*a46*a51 - &
      a12*a25*a36*a41*a53 + a12*a25*a36*a43*a51 + a12*a26*a31*a43*a55 - &
      a12*a26*a31*a45*a53 - a12*a26*a33*a41*a55 + a12*a26*a33*a45*a51 + &
      a12*a26*a35*a41*a53 - a12*a26*a35*a43*a51 + a13*a21*a32*a45*a56 - &
      a13*a21*a32*a46*a55 - a13*a21*a35*a42*a56 + a13*a21*a35*a46*a52 + &
      a13*a21*a36*a42*a55 - a13*a21*a36*a45*a52 - a13*a22*a31*a45*a56 + &
      a13*a22*a31*a46*a55 + a13*a22*a35*a41*a56 - a13*a22*a35*a46*a51 - &
      a13*a22*a36*a41*a55 + a13*a22*a36*a45*a51 + a13*a25*a31*a42*a56 - &
      a13*a25*a31*a46*a52 - a13*a25*a32*a41*a56 + a13*a25*a32*a46*a51 + &
      a13*a25*a36*a41*a52 - a13*a25*a36*a42*a51 - a13*a26*a31*a42*a55 + &
      a13*a26*a31*a45*a52 + a13*a26*a32*a41*a55 - a13*a26*a32*a45*a51 - &
      a13*a26*a35*a41*a52 + a13*a26*a35*a42*a51 - a15*a21*a32*a43*a56 + &
      a15*a21*a32*a46*a53 + a15*a21*a33*a42*a56 - a15*a21*a33*a46*a52 - &
      a15*a21*a36*a42*a53 + a15*a21*a36*a43*a52 + a15*a22*a31*a43*a56 - &
      a15*a22*a31*a46*a53 - a15*a22*a33*a41*a56 + a15*a22*a33*a46*a51 + &
      a15*a22*a36*a41*a53 - a15*a22*a36*a43*a51 - a15*a23*a31*a42*a56 + &
      a15*a23*a31*a46*a52 + a15*a23*a32*a41*a56 - a15*a23*a32*a46*a51 - &
      a15*a23*a36*a41*a52 + a15*a23*a36*a42*a51 + a15*a26*a31*a42*a53 - &
      a15*a26*a31*a43*a52 - a15*a26*a32*a41*a53 + a15*a26*a32*a43*a51 + &
      a15*a26*a33*a41*a52 - a15*a26*a33*a42*a51 + a16*a21*a32*a43*a55 - &
      a16*a21*a32*a45*a53 - a16*a21*a33*a42*a55 + a16*a21*a33*a45*a52 + &
      a16*a21*a35*a42*a53 - a16*a21*a35*a43*a52 - a16*a22*a31*a43*a55 + &
      a16*a22*a31*a45*a53 + a16*a22*a33*a41*a55 - a16*a22*a33*a45*a51 - &
      a16*a22*a35*a41*a53 + a16*a22*a35*a43*a51 + a16*a23*a31*a42*a55 - &
      a16*a23*a31*a45*a52 - a16*a23*a32*a41*a55 + a16*a23*a32*a45*a51 + &
      a16*a23*a35*a41*a52 - a16*a23*a35*a42*a51 - a16*a25*a31*a42*a53 + &
      a16*a25*a31*a43*a52 + a16*a25*a32*a41*a53 - a16*a25*a32*a43*a51 - &
      a16*a25*a33*a41*a52 + a16*a25*a33*a42*a51)*d_inv

    A_inv(5, 6) = (-a11*a22*a33*a44*a56 + a11*a22*a33*a46* &
      a54 + a11*a22*a34*a43*a56 - a11*a22*a34*a46*a53 - a11*a22*a36*a43 &
      *a54 + a11*a22*a36*a44*a53 + a11*a23*a32*a44*a56 - a11*a23*a32* &
      a46*a54 - a11*a23*a34*a42*a56 + a11*a23*a34*a46*a52 + a11*a23*a36 &
      *a42*a54 - a11*a23*a36*a44*a52 - a11*a24*a32*a43*a56 + a11*a24* &
      a32*a46*a53 + a11*a24*a33*a42*a56 - a11*a24*a33*a46*a52 - a11*a24 &
      *a36*a42*a53 + a11*a24*a36*a43*a52 + a11*a26*a32*a43*a54 - a11* &
      a26*a32*a44*a53 - a11*a26*a33*a42*a54 + a11*a26*a33*a44*a52 + a11 &
      *a26*a34*a42*a53 - a11*a26*a34*a43*a52 + a12*a21*a33*a44*a56 - &
      a12*a21*a33*a46*a54 - a12*a21*a34*a43*a56 + a12*a21*a34*a46*a53 + &
      a12*a21*a36*a43*a54 - a12*a21*a36*a44*a53 - a12*a23*a31*a44*a56 + &
      a12*a23*a31*a46*a54 + a12*a23*a34*a41*a56 - a12*a23*a34*a46*a51 - &
      a12*a23*a36*a41*a54 + a12*a23*a36*a44*a51 + a12*a24*a31*a43*a56 - &
      a12*a24*a31*a46*a53 - a12*a24*a33*a41*a56 + a12*a24*a33*a46*a51 + &
      a12*a24*a36*a41*a53 - a12*a24*a36*a43*a51 - a12*a26*a31*a43*a54 + &
      a12*a26*a31*a44*a53 + a12*a26*a33*a41*a54 - a12*a26*a33*a44*a51 - &
      a12*a26*a34*a41*a53 + a12*a26*a34*a43*a51 - a13*a21*a32*a44*a56 + &
      a13*a21*a32*a46*a54 + a13*a21*a34*a42*a56 - a13*a21*a34*a46*a52 - &
      a13*a21*a36*a42*a54 + a13*a21*a36*a44*a52 + a13*a22*a31*a44*a56 - &
      a13*a22*a31*a46*a54 - a13*a22*a34*a41*a56 + a13*a22*a34*a46*a51 + &
      a13*a22*a36*a41*a54 - a13*a22*a36*a44*a51 - a13*a24*a31*a42*a56 + &
      a13*a24*a31*a46*a52 + a13*a24*a32*a41*a56 - a13*a24*a32*a46*a51 - &
      a13*a24*a36*a41*a52 + a13*a24*a36*a42*a51 + a13*a26*a31*a42*a54 - &
      a13*a26*a31*a44*a52 - a13*a26*a32*a41*a54 + a13*a26*a32*a44*a51 + &
      a13*a26*a34*a41*a52 - a13*a26*a34*a42*a51 + a14*a21*a32*a43*a56 - &
      a14*a21*a32*a46*a53 - a14*a21*a33*a42*a56 + a14*a21*a33*a46*a52 + &
      a14*a21*a36*a42*a53 - a14*a21*a36*a43*a52 - a14*a22*a31*a43*a56 + &
      a14*a22*a31*a46*a53 + a14*a22*a33*a41*a56 - a14*a22*a33*a46*a51 - &
      a14*a22*a36*a41*a53 + a14*a22*a36*a43*a51 + a14*a23*a31*a42*a56 - &
      a14*a23*a31*a46*a52 - a14*a23*a32*a41*a56 + a14*a23*a32*a46*a51 + &
      a14*a23*a36*a41*a52 - a14*a23*a36*a42*a51 - a14*a26*a31*a42*a53 + &
      a14*a26*a31*a43*a52 + a14*a26*a32*a41*a53 - a14*a26*a32*a43*a51 - &
      a14*a26*a33*a41*a52 + a14*a26*a33*a42*a51 - a16*a21*a32*a43*a54 + &
      a16*a21*a32*a44*a53 + a16*a21*a33*a42*a54 - a16*a21*a33*a44*a52 - &
      a16*a21*a34*a42*a53 + a16*a21*a34*a43*a52 + a16*a22*a31*a43*a54 - &
      a16*a22*a31*a44*a53 - a16*a22*a33*a41*a54 + a16*a22*a33*a44*a51 + &
      a16*a22*a34*a41*a53 - a16*a22*a34*a43*a51 - a16*a23*a31*a42*a54 + &
      a16*a23*a31*a44*a52 + a16*a23*a32*a41*a54 - a16*a23*a32*a44*a51 - &
      a16*a23*a34*a41*a52 + a16*a23*a34*a42*a51 + a16*a24*a31*a42*a53 - &
      a16*a24*a31*a43*a52 - a16*a24*a32*a41*a53 + a16*a24*a32*a43*a51 + &
      a16*a24*a33*a41*a52 - a16*a24*a33*a42*a51)*d_inv

    A_inv(6, 6) = (a11*a22*a33*a44*a55 - a11*a22*a33*a45* &
      a54 - a11*a22*a34*a43*a55 + a11*a22*a34*a45*a53 + a11*a22*a35*a43 &
      *a54 - a11*a22*a35*a44*a53 - a11*a23*a32*a44*a55 + a11*a23*a32* &
      a45*a54 + a11*a23*a34*a42*a55 - a11*a23*a34*a45*a52 - a11*a23*a35 &
      *a42*a54 + a11*a23*a35*a44*a52 + a11*a24*a32*a43*a55 - a11*a24* &
      a32*a45*a53 - a11*a24*a33*a42*a55 + a11*a24*a33*a45*a52 + a11*a24 &
      *a35*a42*a53 - a11*a24*a35*a43*a52 - a11*a25*a32*a43*a54 + a11* &
      a25*a32*a44*a53 + a11*a25*a33*a42*a54 - a11*a25*a33*a44*a52 - a11 &
      *a25*a34*a42*a53 + a11*a25*a34*a43*a52 - a12*a21*a33*a44*a55 + &
      a12*a21*a33*a45*a54 + a12*a21*a34*a43*a55 - a12*a21*a34*a45*a53 - &
      a12*a21*a35*a43*a54 + a12*a21*a35*a44*a53 + a12*a23*a31*a44*a55 - &
      a12*a23*a31*a45*a54 - a12*a23*a34*a41*a55 + a12*a23*a34*a45*a51 + &
      a12*a23*a35*a41*a54 - a12*a23*a35*a44*a51 - a12*a24*a31*a43*a55 + &
      a12*a24*a31*a45*a53 + a12*a24*a33*a41*a55 - a12*a24*a33*a45*a51 - &
      a12*a24*a35*a41*a53 + a12*a24*a35*a43*a51 + a12*a25*a31*a43*a54 - &
      a12*a25*a31*a44*a53 - a12*a25*a33*a41*a54 + a12*a25*a33*a44*a51 + &
      a12*a25*a34*a41*a53 - a12*a25*a34*a43*a51 + a13*a21*a32*a44*a55 - &
      a13*a21*a32*a45*a54 - a13*a21*a34*a42*a55 + a13*a21*a34*a45*a52 + &
      a13*a21*a35*a42*a54 - a13*a21*a35*a44*a52 - a13*a22*a31*a44*a55 + &
      a13*a22*a31*a45*a54 + a13*a22*a34*a41*a55 - a13*a22*a34*a45*a51 - &
      a13*a22*a35*a41*a54 + a13*a22*a35*a44*a51 + a13*a24*a31*a42*a55 - &
      a13*a24*a31*a45*a52 - a13*a24*a32*a41*a55 + a13*a24*a32*a45*a51 + &
      a13*a24*a35*a41*a52 - a13*a24*a35*a42*a51 - a13*a25*a31*a42*a54 + &
      a13*a25*a31*a44*a52 + a13*a25*a32*a41*a54 - a13*a25*a32*a44*a51 - &
      a13*a25*a34*a41*a52 + a13*a25*a34*a42*a51 - a14*a21*a32*a43*a55 + &
      a14*a21*a32*a45*a53 + a14*a21*a33*a42*a55 - a14*a21*a33*a45*a52 - &
      a14*a21*a35*a42*a53 + a14*a21*a35*a43*a52 + a14*a22*a31*a43*a55 - &
      a14*a22*a31*a45*a53 - a14*a22*a33*a41*a55 + a14*a22*a33*a45*a51 + &
      a14*a22*a35*a41*a53 - a14*a22*a35*a43*a51 - a14*a23*a31*a42*a55 + &
      a14*a23*a31*a45*a52 + a14*a23*a32*a41*a55 - a14*a23*a32*a45*a51 - &
      a14*a23*a35*a41*a52 + a14*a23*a35*a42*a51 + a14*a25*a31*a42*a53 - &
      a14*a25*a31*a43*a52 - a14*a25*a32*a41*a53 + a14*a25*a32*a43*a51 + &
      a14*a25*a33*a41*a52 - a14*a25*a33*a42*a51 + a15*a21*a32*a43*a54 - &
      a15*a21*a32*a44*a53 - a15*a21*a33*a42*a54 + a15*a21*a33*a44*a52 + &
      a15*a21*a34*a42*a53 - a15*a21*a34*a43*a52 - a15*a22*a31*a43*a54 + &
      a15*a22*a31*a44*a53 + a15*a22*a33*a41*a54 - a15*a22*a33*a44*a51 - &
      a15*a22*a34*a41*a53 + a15*a22*a34*a43*a51 + a15*a23*a31*a42*a54 - &
      a15*a23*a31*a44*a52 - a15*a23*a32*a41*a54 + a15*a23*a32*a44*a51 + &
      a15*a23*a34*a41*a52 - a15*a23*a34*a42*a51 - a15*a24*a31*a42*a53 + &
      a15*a24*a31*a43*a52 + a15*a24*a32*a41*a53 - a15*a24*a32*a43*a51 - &
      a15*a24*a33*a41*a52 + a15*a24*a33*a42*a51)*d_inv
    
    end function

    pure function solve_array6_v(A, b, d_known) result(x)
    real(real64), intent(in) :: A(6,6), b(6)
    real(real64), intent(in), optional :: d_known
    real(real64) :: x(6), d, A_inv(6,6)
            
        if(present(d_known)) then
            d = d_known
        else
            d = det_array6_m(A)
        end if
        if( abs(d) <= tiny ) then
            error stop "Matrix is singular."
        end if
        A_inv = inv_array6(A, d)
        
        x = matmul(A_inv, b)
                
    end function
    
    pure function solve_array6_m(A, b, d_known) result(x)
    real(real64), intent(in) :: A(6,6), b(:,:)
    real(real64), intent(in), optional :: d_known
    real(real64) :: x(6, size(b,2)), d, A_inv(6,6)
            
        if(size(b, 1) /= 6) then
            error stop "RHS needs to have 6 rows."
        end if
        if(present(d_known)) then
            d = d_known
        else
            d = det_array6_m(A)
        end if
        if( abs(d) <= tiny ) then
            error stop "Matrix is singular."
        end if
        A_inv = inv_array6(A, d)
        
        x = matmul(A_inv, b)
                
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