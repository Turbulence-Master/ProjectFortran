!! teste doxygen
program integrator
use tools
implicit none
integer :: n
real :: a,b,res

a = 0
b = 10
n = 200

res = rectangle(a,b,n)
print*, 'rectangle =', res

res = trapezoid(a,b,n)
print*, 'trapezoid =', res

end program integrator
