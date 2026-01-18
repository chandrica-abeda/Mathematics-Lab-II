program pascal_triangle
    implicit none
    integer:: i, j, n, r, binomial
    i = 5
    do n = 0, i
        write(*,*)(binomial(n, r), r = 0, n)
    end do

end program pascal_triangle

function binomial(n,r) result(j)
    implicit none
    integer:: n, r, i, j
    if(n == 0) then
        j = 1
    elseif(n == 1) then
        j = 1
    elseif(r == 0) then
        j = 1
    else
        j = 1
        do i = 1, n-r
            j = j *(n-i+1)/i
        end do
    end if
end function