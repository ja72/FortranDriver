    !*******************************************************
    !*    LU decomposition routines used by test_lu.f90    *
    !*                                                     *
    !*                 F90 version by J-P Moreau, Paris    *
    !* --------------------------------------------------- *
    !* Reference:                                          *
    !*                                                     *
    !* "Numerical Recipes By W.H. Press, B. P. Flannery,   *
    !*  S.A. Teukolsky and W.T. Vetterling, Cambridge      *
    !*  University Press, 1986" [BIBLI 08].                *
    !*                                                     *
    !*******************************************************
    MODULE mod_lu
    use mod_common
    implicit none

    integer, parameter :: wp = real64
    integer, parameter :: nmax = 100    
    
    type :: lu_info(n)
        integer, len :: n
        real(real64) :: data(n,n)
        integer(int32) :: indx(n)
        real(real64) :: sgn
        integer :: ierr
    contains
        procedure :: det => lu_det
        procedure :: inv => lu_inv
        procedure :: solve_vec => lu_solve_vec
        procedure :: solve_mat => lu_solve_mat
        generic :: solve => solve_vec, solve_mat
    end type
    
    interface lu
        module procedure :: lu_decomp_fix, lu_decomp_dyn
    end interface

    CONTAINS
    
    pure function lu_decomp_fix(n, A) result(lu)
    integer, intent(in) :: n
    real(real64), intent(in) :: A(n,n)
    real(real64) :: T(n,n), sgn
    type(lu_info(n)) :: lu
    integer :: ierr, indx(n), i
        T = A
        indx = [ (i, i=1,n) ]
        sgn = 1
        ierr = 0
        
        ! 1×1 systems have no LU decomposition
        if( n > 1 ) then
            call LUDCMP(T, n, indx, sgn, ierr)
            if( ierr /= 0 ) then 
                error stop "Singular Matrix"                
            end if            
        end if
        
        lu%data = T
        lu%indx = indx
        lu%sgn = sgn
        lu%ierr = ierr
        
    end function
    
    pure function lu_decomp_dyn(A) result(lu)
    real(real64), intent(in) :: A(:,:)
    type(lu_info(:)), allocatable :: lu
    real(real64) :: lum(size(A,1),size(A,1))
    integer :: indx(size(A,1))
    real(real64) :: d
    integer :: ierr, n
        n = size(A,1)
        lum = A
        call LUDCMP(lum, n, indx, d, ierr)
        if( ierr /= 0 ) then
            error stop "Singular Matrix"
        end if
        allocate(lu_info(n) :: lu)
        lu%data = lum
        lu%indx = indx
        lu%sgn = d
        lu%ierr = ierr
    end function
    
    pure function lu_solve_vec(lu,b) result(x)
    class(lu_info(*)), intent(in) :: lu
    real(real64), intent(in) :: b(:)
    real(real64) :: x(size(b,1))
        x = b
        call LUBKSB(lu%data, lu%n, lu%indx, x)
    end function
    
    pure function lu_solve_mat(lu,b) result(x)
    class(lu_info(*)), intent(in) :: lu
    real(real64), intent(in) :: b(:,:)
    real(real64) :: x(size(b,1),size(b,2))
    integer :: j, k
        k = size(b,2)
        x = b
        do j=1, k
            call LUBKSB(lu%data, lu%n, lu%indx, x(:,j))
        end do
    end function
    
    pure function lu_det(lu) result(d)
    class(lu_info(*)), intent(in) :: lu
    real(real64) :: d
    integer :: i, n
        n = lu%n
        d = lu%sgn
        do i=1,n
            d = d * lu%data(i,i)
        end do
    end function
    
    pure function lu_inv(lu) result(A_inv)
    class(lu_info(*)), intent(in) :: lu
    real(real64) :: A_inv(lu%n, lu%n)
    integer :: j, k
        k = lu%n
        A_inv = 0.0_wp
        forall(j=1:k)
            A_inv(j,j) = 1.0_wp
        end forall
        do j=1, k
            call LUBKSB(lu%data, lu%n, lu%indx, A_inv(:,j))
        end do
                
    end function
    
    pure Subroutine LUDCMP(A,N,INDX,D,CODE)
    !  ***************************************************************
    !  * Given an N x N matrix A, this routine replaces it by the LU *
    !  * decomposition of a rowwise permutation of itself. A and N   *
    !  * are input. INDX is an output vector which records the row   *
    !  * permutation effected by the partial pivoting; D is output   *
    !  * as -1 or 1, depending on whether the number of row inter-   *
    !  * changes was even or odd, respectively. This routine is used *
    !  * in combination with LUBKSB to solve linear equations or to  *
    !  * invert a matrix. Return code is 1, if matrix is singular.   *
    !  ***************************************************************
    IMPLICIT NONE

    real(real64), intent(inout), dimension(N,N) :: A
    integer, intent(in) :: N
    integer, intent(out) :: CODE
    integer, intent(out), dimension(N) :: INDX
    real(real64), intent(out) :: d    

    real(real64)  :: AMAX, DUM, SUMM, VV(nmax)
    INTEGER :: i, j, k, imax

    D=1
    CODE=0

    DO I=1,N
        AMAX=0.d0
        DO J=1,N
            IF (DABS(A(I,J)) > AMAX) AMAX=DABS(A(I,J))
        END DO ! j loop
        IF(AMAX < TINY) THEN
            CODE = 1
            RETURN
        END IF
        VV(I) = 1 / AMAX
    END DO ! i loop

    DO J=1,N
        DO I=1,J-1
            SUMM = A(I,J)
            DO K=1,I-1
                SUMM = SUMM - A(I,K)*A(K,J)
            END DO ! k loop
            A(I,J) = SUMM
        END DO ! i loop
        AMAX = 0.d0
        DO I=J,N
            SUMM = A(I,J)
            DO K=1,J-1
                SUMM = SUMM - A(I,K)*A(K,J)
            END DO ! k loop
            A(I,J) = SUMM
            DUM = VV(I)*DABS(SUMM)
            IF(DUM >= AMAX) THEN
                IMAX = I
                AMAX = DUM
            END IF
        END DO ! i loop

        IF(J /= IMAX) THEN
            DO K=1,N
                DUM = A(IMAX,K)
                A(IMAX,K) = A(J,K)
                A(J,K) = DUM
            END DO ! k loop
            D = -D
            VV(IMAX) = VV(J)
        END IF

        INDX(J) = IMAX
        IF(DABS(A(J,J)) < TINY) A(J,J) = TINY

        IF(J /= N) THEN
            DUM = 1 / A(J,J)
            DO I=J+1,N
                A(I,J) = A(I,J)*DUM
            END DO ! i loop
        END IF
    END DO ! j loop

    RETURN
    
    END subroutine LUDCMP    
    
    pure Subroutine LUBKSB(A, N, INDX, B)
    !  ******************************************************************
    !  * Solves the set of N linear equations A . X = B.  Here A is     *
    !  * input, not as the matrix A but rather as its LU decomposition, *
    !  * determined by the routine LUDCMP. INDX is input as the permuta-*
    !  * tion vector returned by LUDCMP. B is input as the right-hand   *
    !  * side vector B, and returns with the solution vector X. A, N and*
    !  * INDX are not modified by this routine and can be used for suc- *
    !  * cessive calls with different right-hand sides. This routine is *
    !  * also efficient for plain matrix inversion.                     *
    !  ******************************************************************
    implicit none
    integer, intent(in) :: N
    real(real64), intent(in), dimension(N,N) :: A
    integer, intent(in), dimension(N) :: INDX
    real(real64), intent(inout), dimension(N) :: B

    integer :: i,j,ii,ll
    

    real(real64)  SUMM

    II = 0

    DO I=1,N
        LL = INDX(I)
        SUMM = B(LL)
        B(LL) = B(I)
        IF(II /= 0) THEN
            DO J=II,I-1
                SUMM = SUMM - A(I,J)*B(J)
            END DO ! j loop
        ELSE IF(SUMM /= 0.d0) THEN
            II = I
        END IF
        B(I) = SUMM
    END DO ! i loop

    DO I=N,1,-1
        SUMM = B(I)
        IF(I < N) THEN
            DO J=I+1,N
                SUMM = SUMM - A(I,J)*B(J)
            END DO ! j loop
        END IF
        B(I) = SUMM / A(I,I)
    END DO ! i loop

    RETURN
    END subroutine LUBKSB
    
    pure Subroutine LUBKSB2(A, N, K, INDX, B)
    implicit none
    integer, intent(in) :: N, K
    real(real64), intent(in), dimension(N,N) :: A
    integer, intent(in), dimension(N) :: INDX
    real(real64), intent(inout), dimension(N, K) :: B
    integer :: j
        do j=1,K
            call LUBKSB(A, N, INDX, B(:,j))
        end do
    end subroutine LUBKSB2

    subroutine LU_TEST()
    real(real64), pointer ::  A(:,:)   !real matrix (n x n)
    real(real64), pointer ::  B(:)     !real vector (n)
    real(real64), pointer ::  temp(:)  !real temporary vector (n+1)
    integer,pointer ::  INDX(:)  !integer vector (n)
    real(real64) :: d
    integer :: i, rc, n = 4

    !dynamic allocations
    allocate(A(n,n))
    allocate(B(n))
    allocate(temp(n+1))
    allocate(INDX(n))

    ! Fill matrix A
    ! 8  2    3  12     25.0
    ! 2  4    7   0.25  13.25
    ! 3  7    3   5     18.0
    ! 12  0.25 5   2     19.25

    A(1,1) = 8;   A(1,2) = 2;     A(1,3) = 3;     A(1,4) = 12
    A(2,1) = 2;   A(2,2) = 4;     A(2,3) = 7;     A(2,4) = 0.25
    A(3,1) = 3;   A(3,2) = 7;     A(3,3) = 3;     A(3,4) = 5
    A(4,1) = 12;   A(4,2) = 0.25;     A(4,3) = 5;     A(4,4) = 2

    ! Vector B
    B(1) = 25
    B(2) = 13.25
    B(3) = 18.0
    B(4) = 19.25

    !call LU decomposition routine
    call LUDCMP(A,n,INDX,D,rc)

    !call appropriate solver if previous return code is ok
    if (rc.eq.0) then
        call LUBKSB(A,n,INDX,B)
    endif

    !print results or error message
    if (rc.eq.1) then
        write(*,*) ' The system matrix is singular, no solution !'
    else
        write(*,*) '  System solution:'
        do i=1, n
            write(*,*) i,B(i)
        end do
    end if

    end subroutine

    END MODULE