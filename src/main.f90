!> Main program
program integrator
use tools
implicit none
integer :: n
real :: a,b

a = 0
b = 1
n = 10

call rectangle(a,b,n)

call midpoint(a,b,n)

call trapezoid(a,b,n)

end program integrator
