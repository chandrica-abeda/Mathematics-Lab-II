program euler_method
    implicit none
    real:: z(10000), t(10000), y(10000), e(10000)
    real :: a = 0.0, b = 2.0, y0 = 1.0 
    real:: f1, f2, as, h
    integer:: i, n=20

    h = (b-a)/n
    y(1) = y0
    z(1) = 0.0
    e(1) = 0.0

    open(12, file='output_offline2.txt')
    write(12, "(T8, 't(i)', T24, 'x(i)', T40, 'as(i)' T57, 'e(i)')")
        
    do i=1, n+1
        t(i) = a+ (i-1)*h
    end do

    do i = 1, n
        y(i+1) = y(i)+ 0.5*h*(f1(t(i), y(i), z(i))+f1(t(i+1), t(i)+h*f1(t(i), y(i), z(i)), z(i)+h*f2(t(i), y(i), z(i))))
        z(i+1) = z(i)+ 0.5*h*(f2(t(i), y(i), z(i))+f2(t(i+1), y(i+1), z(i)+h*f2(t(i), y(i), z(i))))
        e(i+1) = abs(y(i+1)-as(t(i+1)))
    end do

    do i = 1, n+1
        write(12,*) t(i), y(i), as(t(i)), e(i)
    end do

end program euler_method

function as(t)
    implicit none
    real:: as, t
    as = cos(5*t)
end function

function f1(t, y, z)
    implicit none
    real:: f1, t, y, z
    f1 = z
end function

function f2(t, y, z)
    implicit none
    real:: f2, t, y, z
    f2 = -25*y
end function
