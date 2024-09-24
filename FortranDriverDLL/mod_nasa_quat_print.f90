    module mod_nasa_quat_print
    use mod_nasa_quat
    implicit none

    contains

    ! *****************************************************************************
    ! ** PRINT FUNCTIONS                                                         **
    ! *****************************************************************************
    subroutine q8_transpose_print ( q, title )

    !*****************************************************************************80
    !
    !! Q8_TRANSPOSE_PRINT prints a Q8 "transposed".
    !
    !  Discussion:
    !
    !    A Q8 is a quaternion using R8 arithmetic.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    04 August 2018
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real ( real64 ) Q(4), the quaternion to be printed.
    !
    !    Input, character ( len = * ) TITLE, a title.
    !
    implicit none

    real ( real64 ) q(4)
    character ( len = * ) title

    write ( *, '(a,2x,4g16.8)' ) trim ( title ), q(1:4)

    return
    end
    subroutine r8mat_print ( m, n, a, title )

    !*****************************************************************************80
    !
    !! R8MAT_PRINT prints an R8MAT.
    !
    !  Discussion:
    !
    !    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    12 September 2004
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( int32 ) M, the number of rows in A.
    !
    !    Input, integer ( int32 ) N, the number of columns in A.
    !
    !    Input, real ( real64 ) A(M,N), the matrix.
    !
    !    Input, character ( len = * ) TITLE, a title.
    !
    implicit none

    integer ( int32 ) m
    integer ( int32 ) n

    real ( real64 ) a(m,n)
    character ( len = * ) title

    call r8mat_print_some ( m, n, a, 1, 1, m, n, title )

    return
    end
    subroutine r8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

    !*****************************************************************************80
    !
    !! R8MAT_PRINT_SOME prints some of an R8MAT.
    !
    !  Discussion:
    !
    !    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    10 September 2009
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( int32 ) M, N, the number of rows and columns.
    !
    !    Input, real ( real64 ) A(M,N), an M by N matrix to be printed.
    !
    !    Input, integer ( int32 ) ILO, JLO, the first row and column to print.
    !
    !    Input, integer ( int32 ) IHI, JHI, the last row and column to print.
    !
    !    Input, character ( len = * ) TITLE, a title.
    !
    implicit none

    integer ( int32 ), parameter :: incx = 5
    integer ( int32 ) m
    integer ( int32 ) n

    real ( real64 ) a(m,n)
    character ( len = 14 ) ctemp(incx)
    integer ( int32 ) i
    integer ( int32 ) i2hi
    integer ( int32 ) i2lo
    integer ( int32 ) ihi
    integer ( int32 ) ilo
    integer ( int32 ) inc
    integer ( int32 ) j
    integer ( int32 ) j2
    integer ( int32 ) j2hi
    integer ( int32 ) j2lo
    integer ( int32 ) jhi
    integer ( int32 ) jlo
    character ( len = * ) title

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )

    if ( m <= 0 .or. n <= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
    end if

    do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )

        inc = j2hi + 1 - j2lo

        write ( *, '(a)' ) ' '

        do j = j2lo, j2hi
            j2 = j + 1 - j2lo
            write ( ctemp(j2), '(i8,6x)' ) j
        end do

        write ( *, '(''  Col   '',5a14)' ) ctemp(1:inc)
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) ' '

        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )

        do i = i2lo, i2hi

            do j2 = 1, inc

                j = j2lo - 1 + j2

                if ( a(i,j) == real ( int ( a(i,j) ), real64 ) ) then
                    write ( ctemp(j2), '(f8.0,6x)' ) a(i,j)
                else
                    write ( ctemp(j2), '(g14.6)' ) a(i,j)
                end if

            end do

            write ( *, '(i5,a,5a14)' ) i, ':', ( ctemp(j), j = 1, inc )

        end do

    end do

    return
    end
    subroutine r8vec_print ( n, a, title )

    !*****************************************************************************80
    !
    !! R8VEC_PRINT prints an R8VEC.
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
    !    22 August 2000
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( int32 ) N, the number of components of the vector.
    !
    !    Input, real ( real64 ) A(N), the vector to be printed.
    !
    !    Input, character ( len = * ) TITLE, a title.
    !
    implicit none

    integer ( int32 ) n

    real ( real64 ) a(n)
    integer ( int32 ) i
    character ( len = * ) title

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
    write ( *, '(a)' ) ' '

    do i = 1, n
        write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
    end do

    return
    end


    end module

