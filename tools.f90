module tools
implicit none
private

public rectangle
public trapezoid
contains
    real function rectangle(a, b, n)
    implicit none
    integer :: i,j,n
    real :: a,b,h,m,temp
    h = (b - a) / n
    do i = 0,n-1
        temp = temp + (a + h*i)**3 
    end do
    rectangle = temp * h
    
    end function

    real function trapezoid(a, b, n)
    implicit none
    integer :: i,j,n
    real :: a,b,h,m,temp
    h = (b - a) / n
    do i = 0,n-1
        temp = temp + (a + h*(i+1))**3 + (a + h*i)**3
    end do
    trapezoid = temp * h/2
    
    end function


end module tools
