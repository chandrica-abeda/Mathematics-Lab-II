PROGRAM LSM
    IMPLICIT NONE
    
    INTEGER, PARAMETER :: n = 20 
    REAL(8) :: a, b, alpha, beta, h
    REAL(8) :: x(100), y1(100), yp1(100), y2(100), yp2(100) 
    REAL(8) :: w1(100), w2(100), wp1(100), wp2(100), as(100), e(100)
    INTEGER :: i
    
    a = 1.0
    b = 2.0
    alpha = 0.0
    beta = 24.0
    h = (b-a)/n
    
    x(1) = a
    y1(1) = alpha
    yp1(1) = 0.0
    y2(1) = 0.0
    yp2(1) = 1.0 
    
    !Euler method
    DO i = 1, n+1
      
      yp1(i+1) = yp1(i) + h * ((5.0/x(i)) * yp1(i) - (8.0/x(i)**2) * y1(i))
      y1(i+1) = y1(i) + h * yp1(i)

      yp2(i+1) = yp2(i) + h * ((5.0/x(i)) * yp2(i) - (8.0/x(i)**2) * y2(i))
      y2(i+1) = y2(i) + h * yp2(i)
      x(i+1) = x(i) + h
    END DO

    w1(1) = alpha
    w2(1) = (beta - y1(n+1))/y2(n+1)

    PRINT *,w1(1), w2(1)
    
    DO i = 1, n+1
        as(i) = 2*(x(i)**4 - x(i)**2)
        x(i+1) = x(i) + h
    END DO
  
    PRINT *,  "Approximations (x, y_i, as_i, e_i):"
    DO i = 1, n+1
        wp1(i+1) = y1(i) + (w2(1)*y2(i))
        !wp2(1) = yp1(i) + (w2(1)*yp2(i))
        e(i) = abs(as(i) - wp1(i+1))
        !PRINT *, x(i), y1(i), y2(i), wp1(i+1), as(i), e(i)
        PRINT *, x(i), wp1(i+1), as(i), e(i)
    END DO
  
END PROGRAM LSM

   