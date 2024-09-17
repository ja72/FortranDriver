    !!DEC$ REAL:8

    module mod_common
    use, intrinsic :: iso_fortran_env, only: &
        real64, &
        real32, &
        int8, &
        int32, &
        int64, &
        error_unit

    implicit none
    
    ! JA - Precission
    real(real64), parameter :: eps  = 1.0_real64/4503599627370496_int64
    real(real64), parameter :: tiny  = 1.0_real64/68719476736_int64
    real(real64), parameter :: huge  = 1/tiny

    ! JA - Trig constants
    real(real64), parameter :: pi    = 3.1415926535897932d0
    real(real64), parameter :: pid2  = 1.5707963267948966d0
    real(real64), parameter :: twopi = 6.2831853071795864d0
    real(real64), parameter :: pi_sq = 9.8690440108935862d0
    real(real64), parameter :: rad   = 1.7453292519943296d-2
    real(real64), parameter :: deg   = 57.29577951308232d0
    real(real64), parameter :: div_pi= 0.31830988618379067d0
    real(real64), parameter :: oned3 = 3.333333333333333d-1
    real(real64), parameter :: tend3 = 3.3333333333333333d0

    ! JA - Unit Conversion Factors
    real(real64), parameter :: mm_per_inch = 25.4d0                         ! in -> mm
    real(real64), parameter :: inch_per_mm = 1.0d0/mm_per_inch              ! mm -> in
    real(real64), parameter :: inch_per_ft = 12d0                           ! ft -> in
    real(real64), parameter :: n_per_lbf = 4.448221615d0                    ! lbf-> n
    real(real64), parameter :: nmm_per_lbin = 112.984829d0                  ! lbin -> nmm
    real(real64), parameter :: fts2_per_gee = 32.1740486d0                  ! gee -> ft/s^2
    real(real64), parameter :: deg_per_rad = 180d0/pi                       ! rad -> deg
    real(real64), parameter :: rad_per_deg = 1d0/deg_per_rad                ! deg -> rad
    real(real64), parameter :: rps_per_rpm = pi/30d0                        ! rpm -> rad/s
    real(real64), parameter :: dps_per_rpm = 6d0                            ! rpm -> deg/s
    real(real64), parameter :: mpa_per_ksi = 6.89475908677537d0             ! ksi -> mpa
    real(real64), parameter :: kg_per_lbm = 0.45359237d0                    ! lbm -> kg
    real(real64), parameter :: kph_per_mph = 1.609344d0                     ! mph -> km/h
    
    interface are_equal
        module procedure :: &
            are_equal_integer, &
            are_equal_scalar, &
            are_equal_vector, &
            are_equal_matrix
    end interface    

    contains

    function are_equal_integer(a,b) result(ok)
    integer, intent(in) :: a, b
    logical :: ok
        ok = abs(a-b) == 0
    end function
    
    function are_equal_scalar(a,b,tol) result(ok)
    real(real64), intent(in) :: a, b
    real(real64), optional, intent(in) :: tol
    real(real64) :: delta
    logical :: ok
        if( present(tol) ) then
            delta = tol
        else
            delta = tiny
        end if
        ok = abs(a-b) < delta
    end function
    
    function are_equal_vector(a,b,tol) result(ok)
    real(real64), intent(in) :: a(:), b(:)
    real(real64), optional, intent(in) :: tol
    real(real64) :: delta
    logical :: ok
        ok = size(a) == size(b)        
        if( ok ) then
            if( present(tol) ) then
                delta = tol
            else
                delta = tiny
            end if
            ok = all( abs(a-b) < delta  )
        end if        
    end function
    
    function are_equal_matrix(a,b,tol) result(ok)
    real(real64), intent(in) :: a(:,:), b(:,:)
    real(real64), optional, intent(in) :: tol
    real(real64) :: delta
    logical :: ok
        ok = size(a,1) == size(b,1) .and. size(a,2) == size(b,2)
        if( ok ) then
            if( present(tol) ) then
                delta = tol
            else
                delta = tiny
            end if
            ok = all( abs(a-b) < delta  )
        end if        
    end function
        

    ! Calculates side of a triangle using the law of cosines
    elemental function triangle_side_ssa(side_a,side_b,angle_ab) result(side_c)
    !tex: Given two sides and an angle, the opposing side is
    ! $$c = \sqrt{a^2 + b^2 - 2 a b \cos(\gamma)}$$
    real(real64), intent(in) :: side_a, side_b, angle_ab
    real(real64) :: side_c, t
        if( abs(angle_ab)<tiny ) then
            side_c = side_b - side_a
            return
        end if
        t = side_a**2 + side_b**2 - 2*side_a*side_b*cos(angle_ab)
        side_c = sqrt( t )
    end function
    
    ! Calculates angle of a triangle using the law of cosines
    elemental function triangle_angle_sss(side_a,side_b,side_c) result(angle_ab)
    !tex: Given two adjacent sides and a opposing side, the included angle is
    ! $$\cos(\gamma) = \frac{a^2-b^2-c^2}{2 a b}$$
    real(real64), intent(in) :: side_a, side_b, side_c
    real(real64) :: angle_ab, t
        if( abs( side_c + side_a - side_b )<tiny ) then
            angle_ab = 0d0
            return
        end if
        if( abs( side_c + side_b - side_a )<tiny ) then
            angle_ab = pi
            return
        end if
        t = (side_a**2 + side_b**2 - side_c**2)/(2*side_a*side_b)
        angle_ab = acos( t )
    end function

    ! Calculates side of a trianle using the law of sines
    elemental function triangle_side_saa(side_a,angle_bc,angle_ab) result(side_c)
    !tex: Given a side and two angles, the opposing side is
    ! $$c = a\, \frac{\sin \gamma}{\sin \alpha}$$
    real(real64), intent(in) :: side_a, angle_bc, angle_ab
    real(real64) :: side_c
        if(abs(side_a)<tiny) then
            side_c = 0d0
            return
        end if
        if(abs(angle_bc-angle_ab)<tiny) then
            side_c = side_a
            return
        end if
        side_c = side_a * sin(angle_ab)/sin(angle_bc)
    end function
    
    ! Calculates angle of a triangle using the law of sines
    elemental function triangle_angle_ssa(side_a,side_c,angle_bc) result(angle_ab)
    !tex: Given two adjacent sides and a opposing side, the included angle is
    ! $$\gamma = \mathrm{asin}\left( \frac{c}{a} \sin \alpha \right)$$
    real(real64), intent(in) :: side_a, angle_bc, side_c
    real(real64) :: angle_ab, t
        if(abs(side_c)<tiny) then
            angle_ab = 0d0
            return
        end if
        if(abs(angle_bc)<tiny) then
            angle_ab = 0d0
            return
        end if
        t = side_c*sin(angle_bc)/side_a
        angle_ab = asin( t )
    end function
    
    pure function linspace(x_start, x_end, n_count) result(x)
    real(real64), intent(in) :: x_start, x_end
    integer, intent(in) :: n_count
    real(real64), allocatable :: x(:)
    integer :: i
        x = [ ( x_start + (x_end-x_start)*dble(i-1)/(n_count-1), i=1, n_count) ]
    end function
    
    pure function index_of(x, x_target) result(i_target)
    real(real64), intent(in) :: x(:), x_target
    integer :: i_target, i_low, i_high, i_mid, n_count
    real(real64) :: x_low, x_high, x_mid
        n_count = size(x)
        i_low = 1
        i_high = n_count
        x_low = x(i_low)
        x_high = x(i_high)
        
        do while(i_high-i_low>1)
            i_mid = (i_high + i_low)/2
            x_mid = x(i_mid)
            if( sign(1d0, x_high-x_target) == sign(1d0, x_mid-x_target) ) then
                i_high = i_mid
                x_high = x_mid
            else
                i_low = i_mid
                x_low = x_mid
            end if
        end do
        
        if( x_target == x_high ) then
            i_target = i_high
        else
            i_target = i_low
        end if
            
    end function
    
    
    end module