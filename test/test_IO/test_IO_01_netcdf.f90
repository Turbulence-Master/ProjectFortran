!------------------------------------------------------------------------------
! IMPT, ProjectFortran
!------------------------------------------------------------------------------
!
! PROGRAM: Program test_class_io
!
!> @author
!> Module Author Ilkay Solak
!
! DESCRIPTION: 
!>  Test for Input/Output module, by using netcdf files.
!
! REVISION HISTORY:
! 13/11/2016 - Initial Version by Ilkay Solak
!
! @todo Needs to be tested, waiting for CMakeList.txt, class_field.f90 update
!------------------------------------------------------------------------------
program test_class_io
  use class_field
  implicit none

  !missing variables
  
  !-> Init. Fields (not complete)
  call field_init(x1D, n, 1, 1)
  call field_init(x3D, n, n, n)
  
  !-> Read netcdf field 1D
  call read_field('myField_read.nc',x,var_name,istart,icount)

  !-> Write netcdf field 1D
  call write_field('myField_write.nc',x,istart,icount)

  !-> Read netcdf field 3D
  call read_field('myField_read.nc',x,var_name,istart,icount)

  !-> Write netcdf field 3D
  call write_field('myField_write.nc',x,istart,icount)

end program test_class_io
