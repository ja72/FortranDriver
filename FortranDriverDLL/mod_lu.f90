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

    CONTAINS
    
    function LUSOLVE(A,b,x) result(ok)
    real(wp), intent(in) :: A(:,:), b(:)
    real(wp), intent(in out) :: x(:)
    logical :: ok
    real(wp), allocatable :: temp(:), LU(:,:)
    integer, allocatable :: indx(:)
    real(wp) :: d
    integer :: i, rc, n

    ok = .false.
    n = size(A,1)
    allocate(LU(n,n))
    allocate(temp(n+1))
    allocate(INDX(n))
    LU = A
    x = b
    !call LU decomposition routine
    call LUDCMP(LU,n,INDX,D,rc)

    !call appropriate solver if previous return code is ok
    if (rc == 0) then
        call LUBKSB(LU,n,INDX,x)
        ok = .true.
    endif    
    end function
    
    function LUSOLVEX(A,b,x) result(ok)
    real(wp), intent(in) :: A(:,:), b(:,:)
    real(wp), intent(in out) :: x(:,:)
    logical :: ok
    real(wp), allocatable :: temp(:), LU(:,:)
    integer, allocatable :: indx(:)
    real(wp) :: d

    integer :: i, j, rc, n, m

    ok = .false.
    n = size(A, 1)
    m = size(b, 2)
    allocate(LU(n,n))
    allocate(temp(n+1))
    allocate(INDX(n))
    LU = A
    x = b
    !call LU decomposition routine
    call LUDCMP(LU,n,INDX,D,rc)

    !call appropriate solver if previous return code is ok
    if (rc == 0) then
        do j=1, m
            call LUBKSB(LU,n,INDX,x(:,j))
        end do
        ok = .true.
    endif
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

    real(wp), intent(inout), dimension(N,N) :: A
    integer, intent(in) :: N
    integer, intent(out) :: CODE
    integer, intent(out), dimension(N) :: INDX
    real(wp), intent(out) :: d    

    real(wp)  :: AMAX, DUM, SUMM, VV(nmax)
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
    real(wp), intent(in), dimension(N,N) :: A
    integer, intent(in), dimension(N) :: INDX
    real(wp), intent(inout), dimension(N) :: B

    integer :: i,j,ii,ll
    !f2py depend(N) A, INDX, B

    real(wp)  SUMM

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

    subroutine LU_TEST()
    real(wp), pointer ::  A(:,:)   !real matrix (n x n)
    real(wp), pointer ::  B(:)     !real vector (n)
    real(wp), pointer ::  temp(:)  !real temporary vector (n+1)
    integer,pointer ::  INDX(:)  !integer vector (n)
    real(wp) :: d
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