program eigen
    implicit none
    integer, parameter :: n = 3
    real(8) :: A(n,n) = reshape([1, 2, 0, -2, 1, 2, 1, 3, 1], [n,n])
    real(8) :: x(n), y(n)
    real(8) :: l
    integer :: i, i_m
  

    x = 1
    i_m = 10
  
    ! Power method with scaling
    do i = 1, i_m
      y = matmul(A, x)
      l = maxval(y)
      x = y / l
  
    end do
  
    ! Output results
    print *, "Dominant eigenvalue: ", l
    print *, "Dominant eigenvector: ", x
  
  contains
  
    real(8) function norm2(v)
      real(8), intent(in) :: v(:)
      norm2 = sqrt(sum(v**2))
    end function norm2
  
  end program eigen
  