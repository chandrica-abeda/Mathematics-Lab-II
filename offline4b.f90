PROGRAM LinearFiniteDifference
    implicit none
  
    real:: c(10000), d(10000), w(10000), z(10000), l(10000), e(10000)
    real:: a = 1.0, b = 2.0, al = 0.0, be = 24.0
    real :: h, x, as
    integer:: i, N >= 2

    h = (b-a) / (N+1)
    x = a+h
    a(1) = 2+ (-8/x^2)*(h^2)
    b(1) = -1 + (5/x)*(h/2)
    d(1) = (1 + (h/2)*(5/x)) * al

    do i = 2, (N-1)
        x = a + i*h
        a(i) = 2+ (-8/x^2)*(h^2)
        b(i) = -1 + (5/x)*(h/2)
        c(i) = -1 - (5/x)*(h/2)
    end do

    x = b-h
    a(N) = 2+ (-8/x^2)*(h^2)
    b(N) = -1 - (5/x)*(h/2)
    d(N) = (1 - (h/2)*(5/x)) * be

    l(1) = a(1)
    u(1) = b(1)/a(1)
    z(1) = d(1)/l(1)

    do i = 2, (N-1)
        l(i) = a(i) - c(i)* u (i-1)
        u(i) = b(i)/a(i)
        z(i) = (d(i) - c(i)*z(i-1))/l(i)
    end do

    l(N) = a(N) - c(N)* u (N-1)
    z(i) = (d(i) - c(i)*z(i-1))/l(i)

    w(0) = al
    w(N+1) = be
    w(N) = z(N)

    do i=(N-1), 1
        w(i) = z(i) - (u(i)*w(i+1))
    end do

    do i = 0, (N+1)
        x = a + (i*h)
        write(*,*) x, w(i)
    end do