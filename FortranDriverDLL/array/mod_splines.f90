!***************************************************************************
!
! FILE:         : mod_spline.f90
!
! PURPOSE:      : MODULE DEFININING CUBIC SPLINE INTERPOLATION.
!
! DEPENDENCIES  : NONE
!
! HISTORY       : Written by John Alexiou - Aug, 2021
!
!  Modified by   Date   Change(s) made
!  -----------  ------  ------------------------------------------------
!
! REFERENCES    : Code taken from Numerical Recipies in Fortran 77, Section 3.3, page 109
!                 Download free online from: http://www.nrbook.com/a
!                 "The Art of Scientific Computing", ISBN 0-521-43064-X
    
    module mod_splines
    use mod_common
    implicit none
        
    enum, bind(c)
    ! End conditions for spline
        enumerator :: set_yp                = 1
        enumerator :: set_ypp               = 2
    end enum
    
    enum, bind(c)
    ! Error Codes
        enumerator :: no_error              = 0
        enumerator :: size_error            = 130
        enumerator :: non_monotonic         = 131
        enumerator :: other_error           = 132
        enumerator :: end_condition_error   = 133
        enumerator :: domain_error          = 134
    end enum
    
    type :: spline(count)
        integer, len :: count
        real(real64) :: x(count)
        real(real64) :: y(count)
        real(real64) :: ypp(count)
    contains
        procedure :: yp => spline_yp
        procedure :: inty => spline_inty
        procedure :: index_of_x => spline_index_of_x
        procedure :: index_of_max => spline_index_of_max
        procedure :: spline_value_x, spline_value_index
        procedure :: spline_slope_x, spline_slope_index 
        procedure :: spline_accel_x, spline_accel_index 
        procedure :: spline_interpolate_point
        procedure :: spline_interpolate_data, spline_interpolate_domain
        
        generic :: value => spline_value_x, spline_value_index
        generic :: slope => spline_slope_x, spline_slope_index
        generic :: accel => spline_accel_x, spline_accel_index
        generic :: interpolate => spline_interpolate_data, spline_interpolate_domain
    end type
    
    interface spline
        module procedure :: spline_from_domain
        module procedure :: spline_from_data
    end interface

    contains
    
    pure function spline_from_domain(x_start, x_end, y_data, start_condition, end_condition, start_value, end_value) result(cs)
    real(real64), intent(in)           :: x_start, x_end, y_data(:)
    integer, intent(in), optional      :: start_condition, end_condition
    real(real64), intent(in), optional :: start_value, end_value
    type(spline(size(y_data)))         :: cs
    integer      :: in_start_condition, in_end_condition
    real(real64) :: in_start_value, in_end_value
    integer :: n_count, ierr
        n_count = size(y_data)        
        cs%x = linspace(x_start, x_end, cs%count)
        cs%y = y_data
        if(present(start_condition)) then
            in_start_condition = start_condition
        else
            in_start_condition = set_ypp
        end if
        
        if(present(start_value)) then
            in_start_value = start_value
        else
            in_start_value = 0d0
        end if
        
        if(present(end_condition)) then
            in_end_condition = end_condition
        else
            in_end_condition = set_ypp
        end if
        
        if(present(end_value)) then
            in_end_value = end_value
        else
            in_end_value = 0d0
        end if
        call spline_calculate_ypp(cs, in_start_condition, in_end_condition, in_start_value, in_end_value, ierr)
    end function
    
    pure function spline_from_data(x_data, y_data, start_condition, end_condition, start_value, end_value) result(cs)
    real(real64), intent(in)           :: x_data(:), y_data(:)
    integer, intent(in), optional      :: start_condition, end_condition
    real(real64), intent(in), optional :: start_value, end_value
    type(spline(size(y_data)))         :: cs
    integer      :: in_start_condition, in_end_condition
    real(real64) :: in_start_value, in_end_value
    integer      :: n_count, ierr
        n_count = size(y_data)
        cs%x = x_data
        cs%y = y_data
        if(present(start_condition)) then
            in_start_condition = start_condition
        else
            in_start_condition = set_ypp
        end if
        
        if(present(start_value)) then
            in_start_value = start_value
        else
            in_start_value = 0d0
        end if
        
        if(present(end_condition)) then
            in_end_condition = end_condition
        else
            in_end_condition = set_ypp
        end if
        
        if(present(end_value)) then
            in_end_value = end_value
        else
            in_end_value = 0d0
        end if
        call spline_calculate_ypp(cs, in_start_condition, in_end_condition, in_start_value, in_end_value, ierr)
    end function
    
    pure subroutine spline_calculate_ypp(cs, start_condition, end_condition, start_value, end_value, ierr)
    type(spline(count=*)), intent(inout) :: cs
    integer, intent(in)      :: start_condition, end_condition
    real(real64), intent(in) :: start_value, end_value
    integer, intent(out)     :: ierr
    integer                  :: i,n_count
    real(real64)             :: u(size(cs%y))
    real(real64)             :: p, sig, qn, t, h1, h2, hsum
        ierr = 0
        !n_count = size(cs%y)
        n_count = cs%count
        if(n_count<2 .or. size(cs%x)/=n_count) then
            ierr = size_error
            return
        end if
        
        if( any( cs%x(2:n_count)-cs%x(1:n_count-1) <= 0) ) then
            ierr = non_monotonic
            return
        end if

        select case(start_condition)
        case (set_yp)
            cs%ypp(1) = -0.5d0
            h1        = cs%x(2)-cs%x(1)
            u(1)      = (3/h1)*( (cs%y(2)-cs%y(1))/h1 - start_value )
        case (set_ypp)
            cs%ypp(1) = 0d0
            u(1)      = 0d0
        case default
            ierr      = end_condition_error
            return
        end select
        
        do i=2, n_count-1
            h1        = cs%x(i) - cs%x(i-1)
            h2        = cs%x(i+1) - cs%x(i)
            hsum      = h1 + h2
            sig       = h1/hsum
            p         = sig*cs%ypp(i-1)+2d0
            cs%ypp(i) = (sig-1)/p
            t         = (cs%y(i+1)-cs%y(i))/h2 - (cs%y(i)-cs%y(i-1))/h1
            u(i)      = (6*t - h1*u(i-1))/(hsum*p)
        end do
        
        select case(end_condition)
        case (set_yp)
            qn         = 0.5d0
            h1         = cs%x(n_count) - cs%x(n_count-1)
            u(n_count) = (3/h1)*( end_value - (cs%y(n_count) - cs%y(n_count-1))/h1 )
        case (set_ypp)
            qn         = 0d0
            u(n_count) = 0d0
        case default
            ierr       = end_condition_error
            return
        end select
        
        cs%ypp(n_count) = (u(n_count)-qn*u(n_count-1))/(qn*cs%ypp(n_count-1)+1d0)
        do i=n_count-1, 1, -1
            cs%ypp(i)  = cs%ypp(i)*cs%ypp(i+1)+u(i)
        end do
        
        if( start_condition == set_ypp ) then
            cs%ypp(1)  = start_value
        end if
        if( end_condition == set_ypp ) then
            cs%ypp(n_count) = end_value
        end if
        
    end subroutine
    
    pure function spline_yp(cs) result(yp)
    class(spline(*)), intent(in) :: cs
    real(real64) :: yp(size(cs%y))
    integer :: i
    real(real64) :: h
        h = cs%x(2)-cs%x(1)
        yp(1) = (cs%y(2)-cs%y(1))/h - h*(2*cs%ypp(1)+cs%ypp(2))/6
        do i=1, cs%count-1
            h = cs%x(i+1)-cs%x(i)
            yp(i+1) = (cs%y(i+1)-cs%y(i))/h + h*(cs%ypp(i)+2*cs%ypp(i+1))/6
        end do
    end function

    pure function spline_inty(cs, inty_1) result(inty)
    class(spline(*)), intent(in) :: cs
    real(real64), optional, intent(in) :: inty_1
    real(real64) :: inty(size(cs%y))
    integer :: i
    real(real64) :: h
        if( present(inty_1)) then
            inty(1) = inty_1
        else
            inty(1) = 0d0
        end if
        do i=1, cs%count-1
            h = cs%x(i+1)-cs%x(i)
            inty(i+1) = inty(i) + h*(cs%y(i+1)+cs%y(i))/2 - h**3*(cs%ypp(i+1)+cs%ypp(i))/24
        end do
    end function
    
    pure function spline_index_of_x(cs, x) result(i_target)
    class(spline(*)), intent(in) :: cs
    real(real64), intent(in) :: x
    integer :: i_target
    
        if(x < cs%x(1) ) then
            i_target = 0
            return
        end if
        if(x == cs%x(1) ) then
            i_target = 1
            return
        end if        
        if( x > cs%x(cs%count) ) then
            i_target = cs%count + 1
            return
        end if
        if( x == cs%x(cs%count) ) then
            i_target = cs%count
            return
        end if
        
        i_target = index_of(cs%x, x)        
        
    end function    
    
    function spline_index_of_max(cs, dx) result(i_target)
    class(spline(*)), intent(in) :: cs
    real(real64), intent(out) :: dx
    integer :: i_target
    real(real64), allocatable :: yp(:)
    real(real64) :: h, y_1, y_2, ypp_1, ypp_2
        yp = spline_yp(cs)
        i_target = index_of(yp, 0d0)
        if( i_target == 0) then
            dx = 0d0
            return
        end if
        if( i_target == cs%count) then
            dx = 0d0
            return
        end if
        h = cs%x(i_target+1) - cs%x(i_target)
        y_1 = cs%y(i_target)
        y_2 = cs%y(i_target+1)
        ypp_1 = cs%ypp(i_target)
        ypp_2 = cs%ypp(i_target+1)
        
        if( ypp_1 == ypp_2) then
            dx = h/2 + ( y_1-y_2)/(h*ypp_1) 
            dx = min(h, max(0d0, dx))
            return
        end if
        
        if( ypp_1 > ypp_2 ) then    
            dx = -h*ypp_1/(ypp_2-ypp_1) + sqrt( h**2/3*(ypp_1**2+ypp_1*ypp_2+ypp_2**2)/(ypp_2-ypp_1)**2 + 2*(y_1-y_2)/(ypp_2-ypp_1))
        else
            dx = -h*ypp_1/(ypp_2-ypp_1) - sqrt( h**2/3*(ypp_1**2+ypp_1*ypp_2+ypp_2**2)/(ypp_2-ypp_1)**2 + 2*(y_1-y_2)/(ypp_2-ypp_1))
        end if
                
        dx = min(h, max(0d0, dx))
    end function
    
    pure function spline_value_x(cs, x) result(y)
    class(spline(*)), intent(in) :: cs
    real(real64), intent(in) :: x
    real(real64) :: y
    integer :: i_target, n_count
    real(real64) :: dx    
        n_count = size(cs%y)
        i_target = spline_index_of_x(cs, x)
        if( i_target < 1  ) then
            i_target = 1
        end if
        if( i_target > cs%count) then
            i_target = cs%count
        end if
        dx = x - cs%x(i_target)        
        y = spline_value_index(cs, i_target, dx)
    end function
    
    pure function spline_value_index(cs, i_target, dx) result(y)
    class(spline(*)), intent(in) :: cs
    integer, intent(in) :: i_target
    real(real64), intent(in), optional :: dx
    real(real64) :: y
    reaL(real64) :: h, y_1, y_2, ypp_1, ypp_2, t, a, b, c, d
    integer :: n_count, i
    
        n_count = cs%count
        if( i_target==1 .and. dx < 0) then
            ! Extrapolate to the left using a line
            h = cs%x(2) - cs%x(1)
            
            y_1 = cs%y(1)
            y_2 = cs%y(2)
            ypp_1 = cs%ypp(1)
            ypp_2 = cs%ypp(2)
        
            a = 1-t
            b = t
            c = -h**2*t/3
            d =  h**2*t/6
        
            y = a*y_1 + b*y_2 + c*ypp_1 + d*ypp_2
            return
        end if
        
        if( i_target ==n_count .and. dx >0 ) then
            ! Extrapolate to the right using a line
            
            h = cs%x(n_count) - cs%x(n_count-1)
            
            y_1 = cs%y(n_count-1)
            y_2 = cs%y(n_count)
            ypp_1 = cs%ypp(n_count-1)
            ypp_2 = cs%ypp(n_count)
        
            a = 1-t
            b = t
            c = h**2*(t-1)/6
            d = h**2*(t-1)/3
        
            y = a*y_1 + b*y_2 + c*ypp_1 + d*ypp_2
            return
        end if
        
        if( i_target == n_count) then
            i = n_count - 1
            h = cs%x(n_count) - cs%x(n_count-1)
            if( present(dx) ) then
                t = 1+dx/h
            else
                t = 1d0
            end if
        else    
            i = i_target
            h = cs%x(i_target+1) - cs%x(i_target)   
            if( present(dx) ) then
                t = dx/h
            else
                t = 0d0
            end if
        end if
        
        y_1 = cs%y(i)
        y_2 = cs%y(i+1)
        ypp_1 = cs%ypp(i)
        ypp_2 = cs%ypp(i+1)
        
        a = 1-t
        b = t
        c = h**2*a*(a**2-1)/6
        d = h**2*b*(b**2-1)/6
        
        y = a*y_1 + b*y_2 + c*ypp_1 + d*ypp_2
    
    end function
    
    pure function spline_slope_x(cs, x) result(yp)
    class(spline(*)), intent(in) :: cs
    real(real64), intent(in) :: x
    real(real64) :: yp
    integer :: i_target
    real(real64) :: dx    
        i_target = spline_index_of_x(cs, x)
        if( i_target <1  ) then
            i_target = 1
        end if
        if( i_target > cs%count) then
            i_target = cs%count
        end if
        dx = x - cs%x(i_target)        
        yp = spline_slope_index(cs, i_target, dx)
    end function
    
    pure function spline_slope_index(cs, i_target, dx) result(yp)
    class(spline(*)), intent(in) :: cs
    integer, intent(in) :: i_target
    real(real64), intent(in) :: dx
    real(real64) :: yp
    reaL(real64) :: h, y_1, y_2, ypp_1, ypp_2, t, a, b, c, d
    integer :: n_count, i
    
        n_count = size(cs%y)
        if( i_target==1 .and. dx <0) then
            ! Extrapolate to the left using a line
            h = cs%x(2) - cs%x(1)
            
            y_1 = cs%y(1)
            y_2 = cs%y(2)
            ypp_1 = cs%ypp(1)
            ypp_2 = cs%ypp(2)
        
            c = -h/3
            d = -h/6
        
            yp = (y_2-y_1)/h + c*ypp_1 + d*ypp_2
            return
        end if
        
        if( i_target==n_count .and. dx >0 ) then
            ! Extrapolate to the right using a line
            
            h = cs%x(n_count) - cs%x(n_count-1)
            
            y_1 = cs%y(n_count-1)
            y_2 = cs%y(n_count)
            ypp_1 = cs%ypp(n_count-1)
            ypp_2 = cs%ypp(n_count)
        
            c =  h/6
            d =  h/3
        
            yp = (y_2-y_1)/h + c*ypp_1 + d*ypp_2
            return
        end if
        
        if( i_target == n_count) then
            i = n_count - 1
            h = cs%x(n_count) - cs%x(n_count-1)
            t = 1+dx/h
        else  
            i = i_target
            h = cs%x(i_target+1) - cs%x(i_target)   
            t = dx/h
        end if
        
        y_1 = cs%y(i)
        y_2 = cs%y(i+1)
        ypp_1 = cs%ypp(i)
        ypp_2 = cs%ypp(i+1)
        
        a = 1-t
        b = t
        c = -h*(3*a**2-1)/6
        d =  h*(3*b**2-1)/6
        
        yp = (y_2-y_1)/h + c*ypp_1 + d*ypp_2
    
    end function

    pure function spline_accel_x(cs, x) result(ypp)
    class(spline(*)), intent(in) :: cs
    real(real64), intent(in) :: x
    real(real64) :: ypp
    integer :: i_target
    real(real64) :: dx    
        i_target = spline_index_of_x(cs, x)
        if( i_target <1  ) then
            i_target = 1
        end if
        if( i_target > cs%count) then
            i_target = cs%count
        end if
        dx = x - cs%x(i_target)        
        ypp = spline_accel_index(cs, i_target, dx)
    end function
    
    pure function spline_accel_index(cs, i_target, dx) result(ypp)
    class(spline(*)), intent(in) :: cs
    integer, intent(in) :: i_target
    real(real64), intent(in) :: dx
    real(real64) :: ypp
    reaL(real64) :: h, ypp_1, ypp_2, t, a, b
    integer :: n_count, i
    
        n_count = size(cs%y)
        if( i_target==1 .and. dx <0 ) then
            ! Extrapolate to the left using a line
            ypp = 0d0
            return
        end if
        
        if( i_target==n_count .and. dx >0 ) then
            ! Extrapolate to the right using a line                    
            ypp = 0d0
            return
        end if
        
        if( i_target == n_count) then
            i = n_count - 1
            h = cs%x(n_count) - cs%x(n_count-1)
            t = 1+dx/h
        else    
            i = i_target
            h = cs%x(i_target+1) - cs%x(i_target)   
            t = dx/h
        end if
        
        ypp_1 = cs%ypp(i)
        ypp_2 = cs%ypp(i+1)
        
        a = 1-t
        b = t
        
        ypp = a*ypp_1 + b*ypp_2
    
    end function
    
    pure subroutine spline_interpolate_point(cs, x, y, yp, ypp) 
    class(spline(*)), intent(in) :: cs
    real(real64), intent(in) :: x
    real(real64), intent(out) :: y, yp, ypp
    integer :: i_target, n_count
    real(real64) :: dx    
        n_count = size(cs%y)
        i_target = spline_index_of_x(cs, x)
        if( i_target < 1  ) then
            i_target = 1
        end if
        if( i_target > cs%count) then
            i_target = cs%count
        end if
        dx  = x - cs%x(i_target)        
        y   = spline_value_index(cs, i_target, dx)
        yp  = spline_slope_index(cs, i_target, dx)
        ypp = spline_accel_index(cs, i_target, dx)
    end subroutine
    
    pure function spline_interpolate_domain(cs, x_start, x_end, n_count) result(ip)
    class(spline(*)), intent(in) :: cs
    real(real64), intent(in) :: x_start, x_end
    integer, intent(in) :: n_count
    type(spline(n_count)) :: ip
    real(real64), allocatable :: x(:)
        x = linspace(x_start, x_end, n_count)
        ip = spline_interpolate_data(cs, x)    
    end function
    
    pure function spline_interpolate_data(cs, x) result(ip)
    class(spline(*)), intent(in) :: cs
    real(real64), intent(in) :: x(:)
    type(spline(size(x))) :: ip
    real(real64) :: y(size(x)), ypp(size(x))
    real(real64) :: dx
    integer :: i, i_target, x_count
        x_count = size(x)
        do i=1, x_count
            i_target = spline_index_of_x(cs, x(i))
            if( i_target < 1  ) then
                i_target = 1
            elseif( i_target > cs%count) then
                i_target = cs%count
            endif
            dx = x(i) - cs%x(i_target)
            y(i) = spline_value_index(cs, i_target, dx)
            ypp(i) = spline_accel_index(cs, i_target, dx)
        end do
        
        ip%x = x
        ip%y = y
        ip%ypp = ypp
    end function
    
    
    pure subroutine call_spline_calc_ypp_array(n,x,y,ypp,start_condition, end_condition,start_value, end_value) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_spline_calc_ypp_array
    integer, intent(in), value :: n
    real( real64 ), intent(in)  :: x(n), y(n)
    real( real64 ), intent(out) :: ypp(n)    
    integer, intent(in),value :: start_condition, end_condition
    real(real64), intent(in),value :: start_value, end_value
    type( spline(n) ) :: cs
    
        cs = spline(x, y, start_condition, end_condition, start_value, end_value)
        
        ypp = cs%ypp
    
    end subroutine
    
    pure subroutine call_spline_calc_ypp_domain(n,x_start, x_end, y, x, ypp, start_condition, end_condition,start_value, end_value) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_spline_calc_ypp_domain
    integer, intent(in), value :: n
    real( real64 ), intent(in)  :: y(n)
    real( real64 ), intent(in), value :: x_start, x_end
    real( real64 ), intent(out) :: x(n), ypp(n)    
    integer, intent(in),value :: start_condition, end_condition
    real(real64), intent(in),value :: start_value, end_value
    type( spline(n) ) :: cs
    
        cs = spline(x_start, x_end, y, start_condition, end_condition, start_value, end_value)
        x = cs%x
        ypp = cs%ypp
    
    end subroutine
    
    pure subroutine call_spline_interpolate_point(n,x,y,ypp,xe,ye,ype,yppe) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_spline_interpolate_point
    integer, intent(in), value :: n
    real( real64 ), intent(in)         :: x(n), y(n), ypp(n)
    real( real64 ), intent(in), value  :: xe
    real( real64 ), intent(out)        :: ye, ype, yppe
    type( spline(n) )                  :: cs
    
        ! Define spline    
        cs = spline(n)(x, y, ypp)
        ! get values
        call cs%spline_interpolate_point(xe,ye,ype,yppe)
        
    end subroutine
    
    pure subroutine call_spline_interpolate_array(n,x,y,ypp,m,xe,ye,yppe) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_spline_interpolate_array
    integer, intent(in), value :: n, m
    real( real64 ), intent(in)  :: x(n), y(n), ypp(n)
    real( real64 ), intent(in)  :: xe(m)
    real( real64 ), intent(out) :: ye(m), yppe(m)
    type( spline(n) ) :: cs
    type( spline(m) ) :: ip
        cs   = spline(n)(x,y,ypp)
        ip   = cs%interpolate(xe)
        ye   = ip%y        
        yppe = ip%ypp
    end subroutine
    
    pure subroutine call_spline_interpolate_domain(n,x,y,ypp,m,x_start,x_end,xe,ye,yppe) bind(c)
    !DEC$ ATTRIBUTES DLLEXPORT :: call_spline_interpolate_domain
    integer, intent(in), value :: n, m
    real( real64 ), intent(in)  :: x(n), y(n), ypp(n)
    real( real64 ), intent(in), value :: x_start, x_end
    real( real64 ), intent(out) :: xe(m), ye(m), yppe(m)
    type( spline(n) ) :: cs
    type( spline(m) ) :: ip
        cs   = spline(n)(x,y,ypp)
        ip   = cs%interpolate(x_start, x_end, m)
        xe   = ip%x
        ye   = ip%y
        yppe = ip%ypp
    end subroutine
    
        
    end module