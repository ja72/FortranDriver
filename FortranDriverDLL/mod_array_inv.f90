  
module mod_array_inv
use mod_common
implicit none

    !integer, parameter :: wp = real64
    
    interface det
        module procedure :: mat_det
    end interface
    interface inv
        module procedure :: mat_inv
    end interface
    interface solve
        module procedure :: mat_solve_vec, mat_solve_mat
        module procedure :: lui_solve_vec, lui_solve_mat
    end interface

    contains
    
    pure function zero(n) result(A)
    integer, intent(in) :: n
    real(real64) :: A(n,n)
    
        A = 0.0_real64
    
    end function
    
    pure function zeros(n,m) result(A)
    integer, intent(in) :: n, m
    real(real64) :: A(n,m)
    
        A = 0.0_real64
    
    end function
    
    pure function eye(n,x) result(A)
    integer, intent(in) :: n
    real(real64), intent(in), optional :: x
    real(real64) :: A(n,n), s
    integer :: i
    
        if(present(x)) then
            s=x
        else
            s=1
        end if
    
        A = 0.0_real64
        forall(i=1:n)
            A(i,i) = s
        end forall
    
    end function
    
    pure function diag(values) result(A)
    real(real64), intent(in) :: values(:)
    real(real64) :: A(size(values), size(values))
    integer :: i
    
        A = 0.0_real64
        forall(i=1:size(values))
            A(i,i) = values(i)
        end forall
    
    end function
    
    
    pure function vec_inner(a,b) result(ab)
    real(real64), intent(in) :: a(:), b(:)
    real(real64) :: ab
        ab = dot_product(a,b)
    end function
    
    pure function vec_outer(a,b) result(ab)
    real(real64), intent(in) :: a(:), b(:)
    real(real64) :: ab(size(a),size(b))
    integer :: n,m,i,j
        n = size(a)
        m = size(b)
        !allocate(ab(n,m))
        forall(i=1:n, j=1:m)
            ab(i,j) = a(i)*b(j)
        end forall
    end function
        
    pure function mat_det(A) result(d)
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
            d = mat2_det(A)
        case(3)
            d = mat3_det(A)
        case(4)
            d = mat4_det(A)
        !case(5)
        !    d = mat5_det(A)
        !case(6)
        !    d = mat6_det(A)
        case default
            d = lu_mat_det(A)
        end select
    end function
    
    pure function mat_inv(A) result(B)
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
            B = mat2_inv(A)
        case (3)
            B = mat3_inv(A)
        case (4)
            B = mat4_inv(A)
        case (5)
            B = mat5_inv(A)
        case (6)
            B = mat6_inv(A)
        case default
            B = lu_mat_invert(A)
        end select
    end function
    
    pure function mat_solve_vec(A, b) result(x)
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
            x = mat2_solve_vec(A, b)
        case (3)
            x = mat3_solve_vec(A, b)
        case (4)
            x = mat4_solve_vec(A, b)
        case (5)
            x = mat5_solve_vec(A, b)
        case (6)
            x = mat6_solve_vec(A, b)
        case default
            x = lu_mat_solve_vec(A, b)
        end select
    end function
    
    pure function mat_solve_mat(A, B) result(X)
    real(real64), intent(in) :: A(:,:), B(:,:)
    real(real64) :: X(size(B,1), size(B,2))
    integer :: n
        n = size(A, 1)
        select case(n)
        case (1)
            x = B/A(1,1)
        case (2)   
            x = mat2_solve_mat(A, B)
        case (3)
            x = mat3_solve_mat(A, B)
        case (4)
            x = mat4_solve_mat(A, B)
        case (5)
            x = mat5_solve_mat(A, B)
        case (6)
            x = mat6_solve_mat(A, B)
        case default
            X = lu_mat_solve_mat(A, B)
        end select
        
    end function
    
    pure function lui_solve_vec(A, b) result(x)
    use mod_lu
    class(lu_info(*)), intent(in) :: A
    real(real64), intent(in) :: b(:)
    real(real64) :: x(size(b))
        x = A%solve(b)
    end function
    
    pure function lui_solve_mat(A, B) result(x)
    use mod_lu
    class(lu_info(*)), intent(in) :: A
    real(real64), intent(in) :: b(:,:)
    real(real64) :: x(size(b,1),size(b,2))
        x = A%solve(b)
    end function
    
    pure function mat2_det(A) result(d)
    implicit real(real64) (T)
    real(real64) :: d
    real(real64), intent(in) :: A(2,2)
    real(real64) :: t2, t5
        t2 = A(1,1)*A(2,2)
        t5 = A(1,2)*A(2,1)
        d = t2-t5
    end function
    
    pure function mat2_inv(A, d_known) result(A_inv)
    real(real64), intent(in) :: A(2,2)        
    real(real64), intent(in), optional :: d_known
    real(real64) :: A_inv(2,2), d_inv, d
        if(present(d_known)) then
            d = d_known
        else
            d = mat2_det(A)
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
    
    pure function mat2_solve_vec(A,b,d_known) result(x)
    real(real64), intent(in) :: A(2,2), b(2)
    real(real64), intent(in), optional :: d_known
    real(real64) :: x(2), d, s(2)
        if(present(d_known)) then
            d = d_known
        else
            d = mat2_det(A)
        end if
        if( abs(d) <= tiny ) then
            error stop "Matrix is singular."
        end if
        s = b/d
        x(1) = (A(2,2)*s(1) - A(1,2)*s(2))
        x(2) = (A(1,1)*s(2) - A(2,1)*s(1))    
    end function
    
    pure function mat2_solve_mat(A, B, d_known) result(x)
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
            d = mat2_det(A)
        end if
        if( abs(d) <= tiny ) then
            error stop "Matrix is singular."
        end if
        do j=1, h
            x(:, j) = mat2_solve_vec(A, B(:,j), d)
        end do
    end function
    
    pure function mat3_det(A) result(d)
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
    
    pure function mat3_inv(A, d_known) result(B)
    real(real64), intent(in) :: A(3,3)
    real(real64), intent(in), optional :: d_known
    real(real64) :: B(3,3), d_inv, d
        if(present(d_known)) then
            d = d_known
        else
            d = mat3_det(A)
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
    
    pure function mat3_solve_vec(A, b, d_known) result(x)
    real(real64), intent(in) :: A(3,3), b(3)
    real(real64), intent(in), optional :: d_known
    real(real64) :: x(3), d, s(3)
        if(present(d_known)) then
            d = d_known
        else
            d = mat3_det(A)
        end if
        if( abs(d) <= tiny ) then
            error stop "Matrix is singular."
        end if
        s = b/d
        x(1) = (A(1,2)*(A(2,3)*s(3)-A(3,3)*s(2))+A(1,3)*(A(3,2)*s(2)-A(2,2)*s(3))+s(1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2)))
        x(2) = (A(1,1)*(A(3,3)*s(2)-A(2,3)*s(3))+A(1,3)*(A(2,1)*s(3)-A(3,1)*s(2))-s(1)*(A(2,1)*A(3,3)-A(2,3)*A(3,1)))
        x(3) = (A(1,1)*(A(2,2)*s(3)-A(3,2)*s(2))+A(1,2)*(A(3,1)*s(2)-A(2,1)*s(3))+s(1)*(A(2,1)*A(3,2)-A(2,2)*A(3,1)))
    end function
    
    pure function mat3_solve_mat(A, B, d_known) result(x)
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
            d = mat3_det(A)
        end if
        if( abs(d) <= tiny ) then
            error stop "Matrix is singular."
        end if
        do j = 1, h
            x(:,j) = mat3_solve_vec(A, b(:,j), d)
        end do
    end function
    
    pure function mat4_det(A) result(d)
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
    
    
    pure function mat4_inv(A, d_known) result(A_inv)
    real(real64) :: A_inv(4,4), d_inv, d
    real(real64), intent(in) :: A(4,4)
    real(real64), intent(in), optional :: d_known
        if(present(d_known)) then
            d = d_known
        else
            d = mat4_det(A)
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

    pure function mat4_solve_vec(A, b, d_known) result(x)
    real(real64), intent(in) :: A(4,4), b(4)
    real(real64), intent(in), optional :: d_known
    real(real64) :: x(4), d, s(4)
    
        !x = matmul(mat4_inv(A), b)
        
        if(present(d_known)) then
            d = d_known
        else
            d = mat4_det(A)
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
    
    pure function mat4_solve_mat(A,B, d_known) result(x)
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
            d = mat4_det(A)
        end if
        do j=1, h
            x(:,j) = mat4_solve_vec(A, B(:,j), d)
        end do
    end function
    
    pure function mat5_inv(A) result(A_inv)
    real(real64), intent(in) :: A(5,5)    
    real(real64) :: A_inv(5,5), I(5,5)
    
        I = eye(5)
        A_inv = solve(A, I)
    
    end function
    
    
    pure function mat5_solve_vec(A, b) result(x)
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
    
    pure function mat5_solve_mat(A, B) result(x)
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

    pure function mat6_inv(A) result(A_inv)
    real(real64), intent(in) :: A(6,6)    
    real(real64) :: A_inv(6,6), I(6,6)
    
        I = eye(6)
        A_inv = solve(A, I)
    
    end function
    
    pure function mat6_solve_vec(A, b) result(x)
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
    
    !pure function mat6_solve_mat(A, B) result(x)
    !real(real64), intent(in) :: A(6,6), B(:,:)
    !real(real64) :: x(6, size(B, 2)), d
    !integer :: m, h, j
    !    m = size(B, 1)
    !    if( m/= 6) then
    !        error stop "Expecting a 6 rows in B."
    !    end if
    !    h = size(B, 2)
    !    do j=1, h
    !        x(:,j) = mat6_solve_vec(A, B(:,j))
    !    end do
    !end function
    pure function mat6_solve_mat(A, B) result(x)
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
    
    pure function lu_mat_det(A) result(d)
    real(real64), intent(in) :: A(:,:)
    real(real64) :: d
    integer :: n
        n = size(A,1)
        if( n/= size(A,2)) then
            error stop "Expecting a square matrix."
        end if
        d = lu_mat_det_fix(n, A)
    end function
    
    pure function lu_mat_det_fix(n, A) result(d)
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
    
    pure function lu_mat_invert(A) result(A_inv)
    real(real64), intent(in) :: A(:,:)
    real(real64) :: A_inv(size(A,1),size(A,2))
    integer :: n
    
        n = size(A,1)
        if( n/= size(A,2)) then
            error stop "Expecting a square matrix."
        end if
        A_inv = lu_mat_invert_fix(n, A)
            
    end function
    pure function lu_mat_invert_fix(n, A) result(A_inv)
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
    
    pure function lu_mat_solve_vec(A, b) result(x)
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
        x = lu_mat_solve_vec_fix(n, A, b)
    end function
    
    pure function lu_mat_solve_vec_fix(n, A, b) result(x)
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

    
    pure function lu_mat_solve_mat(A, b) result(x)
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
        x = lu_mat_solve_mat_fix(n, k, A, b)
    end function
    
    pure function lu_mat_solve_mat_fix(n, k, A, b) result(x)
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
    
    function lu_mat_block_solve_vec(A, b) result(x)
    use mod_lu
    real(real64), intent(in) :: A(:,:), b(:)
    real(real64) :: x(size(b))
    integer :: n
        n = size(A,1)
        if(n /= size(A,2) ) then
            error stop "Expecting a square matrix."
        end if
        if( n/= size(b) ) then
            error stop "Incompatible matrix sizes."
        end if
        x = lu_mat_block_solve_vec_fix(n,A,B)
    end function
    
    function lu_mat_block_solve_vec_fix(n, A, b) result(x)
    use mod_lu
    integer, intent(in) :: n
    real(real64), intent(in) :: A(n,n), b(n)
    real(real64) :: x(n)
    type(lu_info(n)) :: LU_1, LU_2, LU_3, LU_4
    real(real64) :: A_11(n/2,n/2), A_22(n-n/2,n-n/2), A_12(n/2,n-n/2), A_21(n-n/2,n/2)
    real(real64) :: B_12(n/2,n-n/2), B_21(n-n/2,n/2), y_1(n/2), y_2(n-n/2)
    real(real64) :: b_1(n/2), b_2(n-n/2), x_1(n/2), x_2(n-n/2)
    integer      :: k, l
    
        if(n == 1) then
            x = b/A(1, 1) 
            return
        end if
            
        k = n/2
        l = n-k
        
        !$OMP PARALLEL 
        !$OMP SECTIONS 
                
        !$OMP SECTION
        b_1 = b(1:k)
        A_11 = A(1:k, 1:k)
        A_12 = A(1:k, k+1:n)
        
        !$OMP SECTION
        b_2 = b(k+1:n)
        A_21 = A(k+1:n, 1:k)
        A_22 = A(k+1:n, k+1:n)
        !$OMP END SECTIONS
                        
        !$OMP SECTIONS
        !$OMP SECTION
        !call LU decomposition routine on A_11
        LU_1 = lu(A_11)
        !call LU solver on intermediate results
        y_1  = solve(LU_1, b_1)
        B_12 = solve(LU_1, A_12)
        !call LU decomposition routine on schure inverses
        LU_4 = lu( A_22 - matmul(A_21, B_12) )
        !call LU solver on final results
        x_2 = solve(LU_4, b_2 - matmul(A_21, y_1) )
        
        !$OMP SECTION
        !call LU decomposition routine on A_22
        LU_2 = lu(A_22)
        !call LU solver on intermediate results
        y_2  = solve(LU_2, b_2)        
        B_21 = solve(LU_2, A_21)
        !call LU decomposition routine on schure inverses
        LU_3 = lu( A_11 - matmul(A_12, B_21) )
        !call LU solver on final results
        x_1 = solve(LU_3, b_1 - matmul(A_12, y_2) )
        
        !$OMP END SECTIONS
        x = [x_1, x_2]
        
        !$OMP END PARALLEL

    end function
    
    
    function lu_mat_block_solve_mat(A, b) result(x)
    use mod_lu
    real(real64), intent(in) :: A(:,:), b(:,:)
    real(real64) :: x(size(b,1), size(b, 2))
    integer :: n, h
        n = size(A, 1)
        h = size(b, 2)
        if(n /= size(A,2) ) then
            error stop "Expecting a square matrix."
        end if
        if( n/= size(b,1) ) then
            error stop "Incompatible matrix sizes."
        end if
        x = lu_mat_block_solve_mat_fix(n,h,A,B)
    end function
    
    function lu_mat_block_solve_mat_fix(n, h, A, b) result(x)
    use mod_lu
    integer, intent(in) :: n, h
    real(real64), intent(in) :: A(n,n), b(n,h)
    real(real64) :: x(n,h)
    type(lu_info(n)) :: LU_1, LU_2, LU_3, LU_4
    real(real64) :: A_11(n/2,n/2), A_22(n-n/2,n-n/2), A_12(n/2,n-n/2), A_21(n-n/2,n/2)
    real(real64) :: B_12(n/2,n-n/2), B_21(n-n/2,n/2), y_1(n/2), y_2(n-n/2)
    real(real64) :: b_1(n/2), b_2(n-n/2), x_1(n/2), x_2(n-n/2)
    integer      :: k, j, l
            
        if(n == 1) then
            x = b/A(1, 1) 
            return
        end if
        
        k = n/2
        l = n - k
        
        !$OMP PARALLEL 
        !$OMP SECTIONS 
        
        !$OMP SECTION
        A_11 = A(1:k, 1:k)
        A_12 = A(1:k, k+1:n)
        !$OMP SECTION
        A_21 = A(k+1:n, 1:k)
        A_22 = A(k+1:n, k+1:n)
        !$OMP END SECTIONS 
        
        !$OMP SECTIONS 
        !$OMP SECTION
        !call LU decomposition routine on A_11
        LU_1 = lu(A_11)
        !call LU solver on intermediate results
        B_12 = solve(LU_1, A_12)
        !call LU decomposition routine on schure inverses
        LU_4 = lu( A_22 - matmul(A_21, B_12) )
        
        !$OMP SECTION
        !call LU decomposition routine on A_22
        LU_2 = lu(A_22)        
        !call LU solver on intermediate results
        B_21 = solve(LU_2, A_21)
        !call LU decomposition routine on schure inverses
        LU_3 = lu( A_11 - matmul(A_12, B_21) )
        !$OMP END SECTIONS
        
        do j=1, h
            
            !$OMP SECTIONS 
                                                
            !call LU solver on intermediate results
            !$OMP SECTION
            b_1 = b(1:k,j)
            !call LU solver on intermediate results
            y_1  = solve(LU_1, b_1)
            !call LU decomposition routine on schure inverses
            x_2 = solve(LU_4, b_2 - matmul(A_21, y_1) )
            
            !$OMP SECTION
            b_2 = b(k+1:n,j)
            !call LU solver on intermediate results
            y_2  = solve(LU_2, b_2)
            !call LU decomposition routine on schure inverses
            x_1 = solve(LU_2, b_1 - matmul(A_12, y_2) )
        
            !$OMP END SECTIONS
            !call LU solver on final results
            x(:,j) = [x_1, x_2]
        end do
        !$OMP END PARALLEL
    end function
    
    pure recursive function mat_inv_reduce(M) result(W)
    real(real64), intent(in) :: M(:,:)
    real(real64), allocatable :: W(:,:)
    real(real64), allocatable :: A(:,:), b(:), c(:), d
    real(real64), allocatable :: A_inv(:,:), A_bc(:,:)
    integer :: n
        n = size(M,1)
        allocate(W(n,n))
        if( n>1) then
            A = M(1:n-1, 1:n-1)
            b = M(1:n-1, n)
            c = M(n, 1:n-1)
            d = M(n, n)
            A_bc = A - vec_outer(b, c)/d
            A_inv = mat_inv_reduce(A)
            W(1:n-1, 1:n-1) = mat_inv_reduce(A_bc)
            W(n,n) = 1/(d - dot_product(c, matmul(A_inv,b)))
            W(1:n-1, n) = matmul(A_inv, b)*W(n,n)
            W(n, 1:n-1) = matmul(c, W(1:n-1, 1:n-1))/d
        else
            W(1,1) = 1/M(1,1)
        end if
    end function
    
    pure recursive function mat_solve_vec_reduce(M,z) result(w)
    real(real64), intent(in) :: M(:,:), z(:)
    real(real64), allocatable :: w(:)
    real(real64), allocatable :: A(:,:), b(:), c(:), d, u(:), v
    real(real64), allocatable :: A_bc(:,:)
    integer :: n
        n = size(M,1)
        allocate(w(n))
        if( n>1) then
            A = M(1:n-1, 1:n-1)
            b = M(1:n-1, n)
            c = M(n, 1:n-1)
            d = M(n, n)
            u = z(1:n-1)
            v = z(n)
            A_bc = A - vec_outer(b,c)/d
            w(1:n-1) = mat_solve_vec_reduce(A_bc, u - b*(v/d))
            w(n) = (v-dot_product(c, w(1:n-1)))/d
        else
            w(1) = z(1)/M(1,1)
        end if
    end function
    
    
end module