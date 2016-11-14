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
  use class_io
  use precision
  implicit none

  type field
     !-> dimensions
     integer(ik) :: nx,ny,nz
     !-> dimensions names
     !-> f(t,x,y,z)
     real(rk),allocatable :: f(:,:,:)
     !-> field name
     character(len=20) :: name="noname"
  end type field


  !-> define overloaded operator for type field
  interface operator(+) 
     module procedure field_add 
     module procedure field_add_scal1
  end interface

contains

!> Assign a field type in another field type variable
!! method
!! @param x1 target field to be assigned
!! @param x2 field to assign
  subroutine field_assign(x1,x2)
    implicit none
    type(field),intent(in) :: x2
    type(field),intent(inout) :: x1
    x1%f=x2%f
  end subroutine field_assign

!> Assign a scalar in a field type variable
!! method
!! @param x1 target field to be assigned
!! @param x2 scalar to assign
  subroutine field_assign_scalar(x1,x2)
    implicit none
    real(rk),intent(in) :: x2
    type(field),intent(inout) :: x1
    x1%f=x2
  end subroutine field_assign_scalar

!> Add two field type variables
!! method
!! @param x1 first field
!! @param x2 second field
  function field_add(x1,x2)
    implicit none
    type(field),intent(in) :: x1,x2
    type(field) :: field_add
    character(len=512) :: name
    name=trim(x1%name) !//"+"//trim(x2%name)
    call field_init(field_add,name,x1%nx,x1%ny,x1%nz)
    field_add%f=x1%f+x2%f
  end function field_add

!> Add a scalar and afield type variables
!! method
!! @param x1 first field
!! @param x2 scalar
  function field_add_scal1(x1,x2)
    implicit none
    type(field),intent(in) :: x1
    real(rk),intent(in) :: x2
    type(field) :: field_add_scal1
    character(len=512) :: name
    Character(len=15) :: num
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
!
    implicit none
    type(field),intent(inout) :: x 
    character(len=*),intent(in) :: name
    integer(ik),intent(in) :: n1,n2,n3
    character(len=*),optional,intent(in) :: n1n,n2n,n3n

    !if (present(n1n)) x%nxn=n1n
    !if (present(n2n)) x%nyn=n2n
    !if (present(n3n)) x%nzn=n3n
    x%name=name
    call field_allocate(x,n1,n2,n3)
    x%f=0._rk
  end subroutine field_init

  subroutine field_allocate(x,n1,n2,n3)
! -----------------------------------------------------------------------
! field : (re)allocation field type array
! -----------------------------------------------------------------------
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
  
  !---------------------------------------------------------------------------  
  !> @author 
  !> Routine Author Matthieu Marquillie
  !
  ! DESCRIPTION: 
  !> Create/Open and Write/Add field variable in output file
  !> @brief
  !> Create/Open and Write/Add field variable in output file
  !
  ! REVISION HISTORY:
  ! 01/07/2011 - Initial Version by Matthieu Marquillie
  ! 13/11/2016 - Updated Version by Ilkay Solak
  !
  !> @param[in] file_name              - name of the file
  !> @param[inout] x                   - variable to read
  !> @param[in] istart(ndim),optional  - start points, per process if MPI
  !> @param[in] icount(ndim),optional  - end points, per process if MPI
  !---------------------------------------------------------------------------  
  subroutine write_field(file_name,x,istart,icount)
    use class_io
    implicit none
    
    character(len=*),intent(in) :: file_name
    type(field),intent(in) :: x
    integer(ik),optional,intent(in) :: istart(3),icount(3)

    integer(ik) :: iistart(3),irang

    !-> write 3d variable
    if (present(istart)) then
      if (present(icount)) then
        call write_var3d(file_name//".nc",[character(len=512) :: x%nxn,x%nyn,x%nzn],&
                         (/x%nx,x%ny,x%nz/),x%name,x%f,istart,icount)
      else
        call write_var3d(file_name//".nc",[character(len=512) :: x%nxn,x%nyn,x%nzn],&
                       (/x%nx,x%ny,x%nz/),x%name,x%f,istart,(/x%nx,x%ny,x%nz/))
      endif
    else
      if (present(icount)) then
        call write_var3d(file_name//".nc",[character(len=512) :: x%nxn,x%nyn,x%nzn],&
                         (/x%nx,x%ny,x%nz/),x%name,x%f,(/1,1,1/),icount)
      else 
        call write_var3d(file_name//".nc",[character(len=512) :: x%nxn,x%nyn,x%nzn],&
                         (/x%nx,x%ny,x%nz/),x%name,x%f,(/1,1,1/),(/x%nx,x%ny,x%nz/))
      endif
    endif

  end subroutine write_field
  
  !---------------------------------------------------------------------------  
  !> @author 
  !> Routine Author Matthieu Marquillie
  !
  ! DESCRIPTION: 
  !> Read field variable from input file
  !> @brief
  !> Read field variable from input file
  !
  ! REVISION HISTORY:
  ! 01/07/2011 - Initial Version by Matthieu Marquillie
  ! 13/11/2016 - Updated Version by Ilkay Solak
  !
  !> @param[in] file_name       - name of the file
  !> @param[inout] x            - variable to read
  !> @param[in] var_name        - name of the variable
  !> @param[in] istart(ndim)    - start points, per process if MPI
  !> @param[in] icount(ndim)    - end points, per process if MPI
  !---------------------------------------------------------------------------  
  subroutine read_field(file_name,x,var_name,istart,icount)
    use class_io
    implicit none
    
    character(len=*),intent(in) :: file_name,var_name
    type(field),intent(inout) :: x
    integer(ik),optional,intent(in) :: istart(3),icount(3)
    
    integer(ik) :: dim_len(3)
    character(len=512) :: dim_name(3)

    integer(ik) :: iistart(3),irang

    !-> get field informations in input file 
    call get_var3d_info(file_name(1:len_trim(file_name))//".nc",var_name,dim_name,dim_len)

    !-> initialize field
    if (present(icount)) then
      call field_init(x,var_name,icount(1),icount(2),icount(3),&
                      n1n=dim_name(1),n2n=dim_name(2),n3n=dim_name(3))
    else
      call field_init(x,var_name,dim_len(1),dim_len(2),dim_len(3),&
                      n1n=dim_name(1),n2n=dim_name(2),n3n=dim_name(3))
    endif

    !-> read 3d variable
    if (present(istart)) then
	    if (present(icount)) then
		    call read_var3d(file_name(1:len_trim(file_name))//".nc",x%name,x%f,istart,icount)
	    else
		    call read_var3d(file_name(1:len_trim(file_name))//".nc",x%name,x%f,istart,(/x%nx,x%ny,x%nz/))
	    endif
    else
	    if (present(icount)) then
		    call read_var3d(file_name(1:len_trim(file_name))//".nc",x%name,x%f,(/1,1,1/),icount)
	    else
		    call read_var3d(file_name(1:len_trim(file_name))//".nc",x%name,x%f,(/1,1,1/),(/x%nx,x%ny,x%nz/))
	    endif
    endif

  end subroutine read_field  
  
end module class_field
