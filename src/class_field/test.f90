program test_types

use class_field

integer(ik) :: i
real(rk) :: y
integer(
type(field) :: my1dfield

my1dfield%nx = 2.32

y = 2.123456789

do i=1,10
    print*, i
    print*, i*y
end do


end program test_types
