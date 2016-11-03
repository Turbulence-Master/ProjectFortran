!> Module with tools
module tools
implicit none
private

public rectangle
public trapezoid
public midpoint

contains

    !> Subroutine for using the rectangle method
    !! method
    !! @param a, b Lower and Upper range
    !! @param n Number of domain division
    !! @todo more test cases
    subroutine rectangle(a, b, n)
    implicit none
    integer :: i,j,n
    real :: a,b,h,temp,xi,res,error
    temp = 0
    h = (b - a) / n
    do i = 0,n-1
        xi = (a + h*i)
        !temp = temp + xi**4 -xi**3
        temp = temp + (3 + 2*xi + 5*xi**2)
    end do
    res = temp * h
    error = (((b - a)**3)/(24*(n**2)))*10
    print*, 'Rectangle =', res, 'error =', error, 'total =', res + error
    end subroutine


    !> Subroutine for using the midpoint method
    !! method
    !! @param a, b Lower and Upper range
    !! @param n Number of domain division
    !! @todo more test cases
    subroutine midpoint(a, b, n)
    implicit none
    integer :: i,j,n
    real :: a,b,h,temp,xi,res,error
    h = (b - a) / n
    temp = 0
    do i = 0,n-1
        xi = (a + h)/2 + h*i
        !temp = temp + h*(xi**4 - xi**3)
        temp = temp + (3 + 2*xi + 5*xi**2)
    end do
    res = temp * h
    error = ((b - a)**3/(24*n**2))*10
    print*, 'Midpoint =', res, 'error =', error, 'total =', res + error
   
    end subroutine


    !> Subroutine for using the trapezoid method
    !! method
    !! @param a, b Lower and Upper range
    !! @param n Number of domain division
    !! @todo more test cases
    subroutine trapezoid(a, b, n)
    implicit none
    integer :: i,j,n
    real :: a,b,h,temp,xi,xi1,res,error
    h = (b - a) / n
    temp = 0
    do i = 0,n-1
        xi1 = a + h*(i+1)
        xi = a + h*i
        !temp = temp + (xi1**4 - xi1**3) + (xi**4 - xi**3)
        temp = temp + (3 + 2*xi1 + 5*xi1**2) + (3 + 2*xi + 5*xi**2)
    end do
    res = temp * h/2
    error = -((b - a)**3/(12*n**2))*10
    print*, 'Trapezoid =', res, 'error =', error, 'total =', res + error

    end subroutine


end module tools
