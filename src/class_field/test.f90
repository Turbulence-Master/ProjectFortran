program test_types
  use class_field
  use precision
  implicit none

  integer(ik) :: i
  ! declare a derived type variable
  type(field) :: my1dfield


  ! Type field always requires it's init
  call field_init(my1dfield,'name_my1dfield',10,1,1)

  ! Show dimension of the type field
  print*, my1dfield%nx, my1dfield%ny, my1dfield%nz

  ! Add data in to the type field array, which is 'f'
  do i=1,10
    my1dfield%f(i,1,1) = i
  end do

  ! Print the array
  print*, my1dfield%f

end program test_types
