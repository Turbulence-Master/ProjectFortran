!------------------------------------------------------------------------------
! IMPT, ProjectFortran
!------------------------------------------------------------------------------
!
! Program: Linear fit
!
!> @author
!> Program Author Anand Utsav Kapoor
!
! DESCRIPTION: 
!>  Approximating a straight line for a given data.
!
! REVISION HISTORY:
! 9/1/2017 : 1st 
!



program least_square
implicit none
character :: filename
integer, parameter :: LS = 21 !I/O unit for the file to be read
integer :: inp_error !input error
integer :: n = 0 !number of input points (x,y)
real :: slope ! slope of the line
real :: sum_x = 0 ! sum of all x values of the points
real :: sum_x2 = 0 ! sum of x^2
real :: sum_xy = 0 ! sum of x*y
real :: sum_y = 0 ! sum of y
real :: x ! input x
real :: x_bar ! average of x values
real :: y ! input y
real :: y_bar ! average of y values
real :: y_int ! y intercept of the line

!write(*,*) "enter name of the file containing the input set of points"
filename = "data_lsr.dat"

open (unit= LS, file= 'data_lsr.dat' , status='old', iostat=inp_error)

if (inp_error > 0) then

    write(*,*) "file doesnt exist"
else
do
    read(LS,*,iostat=inp_error) x,y !pair
    if (inp_error /= 0) exit
    n = n+1                  !
    sum_x = sum_x + x        !statistical
    sum_y = sum_y + y        ! calculations
    sum_x2 = sum_x2 + x**2   !
    sum_xy = sum_xy + x*y    !
end do

    !slope and intercept calculations
    x_bar = sum_x/ real(n)
    y_bar = sum_y/ real(n)
    slope = (sum_xy - sum_x* y_bar)/(sum_x2 -sum_x * x_bar)
    y_int = y_bar - slope* x_bar

    write (*,*) " slope :" ,slope 
    write (*,*) "y_intercept", y_int 
    write(*,*) "number of points", n
    close (unit =LS)
end if
end program least_square
