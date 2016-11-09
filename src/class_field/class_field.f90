module class_field
!------------------------------------------------------------------------------
! IMPT, ProjectFortran
!------------------------------------------------------------------------------
!
! MODULE: class_field
!
!> @author Guilherme Lindner
!> Module Author Name and Affiliation
!
! DESCRIPTION: 
!> Type fields to be used by other modules
!
! REVISION HISTORY:
! 07 10 2016 - Initial Version
! TODO_dd_mmm_yyyy - TODO_describe_appropriate_changes - TODO_name
!------------------------------------------------------------------------------
  use precision
  implicit none

  type field
     !-> dimensions
     integer(ik) :: nx,ny,nz
     !-> dimensions names
     !-> f(t,x,y,z)
     real(rk),allocatable :: f(:,:,:,:)
     !-> field name
     character(len=512) :: name="noname"
  end type field


  !-> define overloaded operator for type field
  interface operator(+) 
     module procedure field_add 
     module procedure field_add_scal1
  end interface

contains

! =======================================================================
! =======================================================================
! field : operators methods
! =======================================================================
! =======================================================================

  subroutine field_assign(x1,x2)
! -----------------------------------------------------------------------
! field : assign a field type in another field type variable
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 10/2012
!
    implicit none
    type(field),intent(in) :: x2
    type(field),intent(inout) :: x1
    x1%f=x2%f
  end subroutine field_assign

  subroutine field_assign_scalar(x1,x2)
! -----------------------------------------------------------------------
! field : assign a scalar in a field type variable
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 10/2012
!
    implicit none
    real(rk),intent(in) :: x2
!    type(field),intent(out) :: x1
    type(field),intent(inout) :: x1
    x1%f=x2
  end subroutine field_assign_scalar

  function field_add(x1,x2)
! -----------------------------------------------------------------------
! field : add two field type variables
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 10/2012
!
    implicit none
    type(field),intent(in) :: x1,x2
    type(field) :: field_add
    character(len=512) :: name
    name=trim(x1%name) !//"+"//trim(x2%name)
    call field_init(field_add,name,x1%nx,x1%ny,x1%nz)
    field_add%f=x1%f+x2%f
  end function field_add

  function field_add_scal1(x1,x2)
! -----------------------------------------------------------------------
! field : add a scalar and a field type variables
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 10/2012
!
    implicit none
    type(field),intent(in) :: x1
    real(rk),intent(in) :: x2
    type(field) :: field_add_scal1
    character(len=512) :: name
    Character(len=15) :: num
!    write(num,'(f0.2)')x2
    name=trim(x1%name) !//"+"//num
    call field_init(field_add_scal1,name,x1%nx,x1%ny,x1%nz)
    field_add_scal1%f=x1%f+x2
  end function field_add_scal1

! =======================================================================
! =======================================================================
! field : initialization and destruction methods
! =======================================================================
! =======================================================================
  subroutine field_init(x,name,n1,n2,n3,n1n,n2n,n3n)
! -----------------------------------------------------------------------
! field : initialize field type variable
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 04/2011
!
    implicit none
    type(field),intent(inout) :: x 
    character(len=*),intent(in) :: name
    integer(ik),intent(in) :: n1,n2,n3
    character(len=*),optional,intent(in) :: n1n,n2n,n3n

    if (present(n1n)) x%nxn=n1n
    if (present(n2n)) x%nyn=n2n
    if (present(n3n)) x%nzn=n3n
    x%name=name
    call field_allocate(x,n1,n2,n3)
    x%f=0._rk
  end subroutine field_init

  subroutine field_allocate(x,n1,n2,n3)
! -----------------------------------------------------------------------
! field : (re)allocation field type array
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 04/2011
!
    implicit none
    type(field),intent(inout) :: x 
    integer(ik),intent(in) :: n1,n2,n3

    if (.not.allocated(x%f)) then
       x%nx=n1 ; x%ny=n2 ; x%nz=n3
       allocate(x%f(x%nx,x%ny,x%nz))
    elseif (x%nx/=n1.or.x%ny/=n2.or.x%ny/=n3) then 
       x%nx=n1 ; x%ny=n2 ; x%nz=n3
       deallocate(x%f) ; allocate(x%f(x%nx,x%ny,x%nz))
    else
       x%nx=n1 ; x%ny=n2 ; x%nz=n3
    endif
  end subroutine field_allocate
end module class_field
