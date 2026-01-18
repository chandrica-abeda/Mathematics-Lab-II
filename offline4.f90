PROGRAM FDM
  IMPLICIT NONE

  INTEGER, PARAMETER :: NMAX = 1000
  REAL(8) :: a, b, h, alpha, beta
  REAL(8) :: x(1000), a_coef(1000), b_coef(1000), c_coef(1000), d_coef(1000)
  REAL(8):: w(1000), l(1000), u(1000), z(1000), as(1000), e(1000) 
  INTEGER :: N, i


  a = 1.0
  b = 2.0
  alpha = 0.0
  beta = 24.0
  N = 19
   
  h = (b-a)/(N+1)
  x(1) = a + h
  a_coef(1) = 2.0 + (h**2 * (-8.0 / x(1)**2))
  b_coef(1) = -1.0 + ((h / 2.0) * (5.0 / x(1)))
  d_coef(1) = (1.0 + ((h / 2.0) * (5.0 / x(1)))) * alpha

  DO i = 2, (N-1)
    x(i) = a + (i * h)
    a_coef(i) = 2.0 +(h**2 * (-8.0 / x(i)**2))
    b_coef(i) = -1.0 + ((h / 2.0) * (5.0 / x(i)))
    c_coef(i) = -1.0 - ((h / 2.0) * (5.0 / x(i)))
    d_coef(i) = -(h**2 * 0.0)
  END DO

  x(N) = b - h
  a_coef(N) = 2.0 + (h**2 * (-8.0 / x(N)**2))
  c_coef(N) = -1.0 - ((h / 2.0) * (5.0 / x(N)))
  d_coef(N) = (1.0 - ((h / 2.0) * (5.0 / x(N)))) * beta

  l(1) = a_coef(1)
  u(1) = b_coef(1)/a_coef(1)
  z(1) = d_coef(1)/l(1)
  
  DO i = 2, (N-1)
      l(i) = a_coef(i) - (c_coef(i)*u(i-1))
      u(i) = b_coef(i)/l(i)
      z(i) = (d_coef(i) - (c_coef(i)*z(i-1)))/l(i)
  END DO
  
  l(N) = a_coef(N) - (c_coef(N)*u(N-1))
  z(N) = (d_coef(N) - (c_coef(N)*z(N-1)))/l(N)


  !w(0) = alpha
  w(N) = z(N)
  w(N+1) = beta

  as(0) = 0.0
  as(1) = 2*(a**4 - a**2)
  DO i = 1, (N)
      x(i) = a + (i * h)
      as(i) = 2*(x(i)**4 - x(i)**2)
  END DO
  as(N+1) = 2*(b**4 - b**2)

  e(0) = as(0) - alpha
  DO i = (N), 1, -1
    w(i) = z(i) - (u(i) * w(i+1))
    e(i) = as(i) - w(i)
  END DO

  PRINT *, "Approximations (x, w_i, as_i, e_i):"
  PRINT *, a, alpha, as(0), 0.0
  DO i = 1, (N+1)
    x(i) = a + (i * h)
    PRINT *, x(i), w(i), as(i), e(i)
  END DO

END PROGRAM FDM