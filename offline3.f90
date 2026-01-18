program rk4
    implicit none
    real:: z(10000), t(10000), y(10000),  e(10000)
    real:: k1, k2, k3, k4
    real:: a = 1.0, b = 3.0, y0 = 0.0 
    real:: f1, f2, h
    integer:: i, n=10

    h = (b-a)/n
    y(1) = y0


    do i=1, n+1
        t(i) = a+ (i-1)*h
    end do

    do i = 1, n
        k1 = h*f1(t(i), y(i))
        k2 = h*f1(t(i)+h/2, y(i)+k1/2)
        k3 = h*f1(t(i)+h/2, y(i)+k2/2)
        k4 = h*f1(t(i)+h, y(i)+ k3)

        y(i+1) = y(i) + (k1 + 2*k2 + 2*k3 + k4)/6
    end do

    do i = 1, n+1
        write(*,*) t(i), y(i)
    end do

end program rk4

function f1(t, y)
    implicit none
    real:: f1, t, y
    f1 = 1 + (y/t) +(y/t)**2
end function
