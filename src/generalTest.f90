program lapacktesting

use precision
use class_field

implicit none
type(field) :: matrixA, vectorB, vectorX
integer(ik) :: i,j, pivot(3), ok

call field_init(matrixA,'Matrix A',3,3,1)
call field_init(vectorB,'Vector B',3,1,1)
call field_init(vectorX,'Vector X',3,1,1)

print*, 'Dimension of ',matrixA%name,'=', matrixA%nx, matrixA%ny, matrixA%nz

matrixA%f(1,1,1)=-3.0
matrixA%f(1,2,1)=2.0
matrixA%f(1,3,1)=-6.0
matrixA%f(2,1,1)=5.0
matrixA%f(2,2,1)=7.0
matrixA%f(2,3,1)=-5.0
matrixA%f(3,1,1)=1.0
matrixA%f(3,2,1)=4.0
matrixA%f(3,3,1)=-2.0

print*, matrixA%name
do i=1,3
    do j=1,3
        print*, matrixA%f(i,j,1)
    end do
end do

vectorB%f(1,1,1)=6.0
vectorB%f(2,1,1)=6.0
vectorB%f(3,1,1)=8.0

print*, vectorB%name
do i=1,3
    print*, vectorB%f(i,1,1)
end do

call DGESV(3, 1, matrixA%f, 3, pivot, vectorB%f, 3, ok)
print*, ok

do i=1,3
    write(*,*) vectorB%f(i,1,1)
end do

end program lapacktesting
