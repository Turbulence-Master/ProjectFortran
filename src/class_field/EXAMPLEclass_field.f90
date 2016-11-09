module class_field
! -----------------------------------------------------------------------
! Name :
! class field 
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 05/2011
!
  use precision
  implicit none

  type field
     !-> dimensions
     integer(ik) :: nx,ny,nz
     !-> dimensions names
     character(len=512) :: nxn="resolution_x",nyn="resolution_y",nzn="resolution_z"
     !-> field array
     real(rk),allocatable :: f(:,:,:)
     !-> field name
     character(len=512) :: name="noname"
!   contains
!     procedure :: derx
!     procedure :: dderx
  end type field

  type boundary_condition
     !-> dimensions
     integer(ik) :: nx,ny,nz
     !-> field array
     real(rk),allocatable :: bcx(:,:,:),bcy(:,:,:),bcz(:,:,:)
  end type boundary_condition

  !-> define overloaded operator for type field
  interface operator(+) 
     module procedure field_add 
     module procedure field_add_scal1,field_add_scal2
  end interface
  interface operator(-) 
     module procedure field_sub 
     module procedure field_sub_scal1,field_sub_scal2
     module procedure field_neg
  end interface
  interface operator(*) 
     module procedure field_mul 
     module procedure field_mul_scal1,field_mul_scal2
  end interface
  interface operator(/) 
     module procedure field_div
     module procedure field_div_scal,scal_div_field
  end interface
  interface operator(**) 
     module procedure field_pow
  end interface
  interface assignment(=) 
     module procedure field_assign,field_assign_scalar
     module procedure bc_assign
  end interface

contains

! =======================================================================
! =======================================================================
! field : bc operators methods
! =======================================================================
! =======================================================================

  subroutine bc_assign(x1,x2)
! -----------------------------------------------------------------------
! field : assign a scalar in a bc type variable
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 10/2012
!
    implicit none
    real(rk),intent(in) :: x2
    type(boundary_condition),intent(out) :: x1
    call boundary_condition_init(x1,x1%nx,x1%ny,x1%nz)
    x1%bcx=x2 ; x1%bcy=x2 ; x1%bcz=x2
  end subroutine bc_assign
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

  function field_add_scal2(x2,x1)
! -----------------------------------------------------------------------
! field : add a scalar and a field type variables
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 10/2012
!
    implicit none
    type(field),intent(in) :: x1
    real(rk),intent(in) :: x2
    type(field) :: field_add_scal2
    character(len=512) :: name
    Character(len=15) :: num
!    write(num,'(f0.2)')x2
    name=trim(x1%name) !//"+"//num
    call field_init(field_add_scal2,name,x1%nx,x1%ny,x1%nz)
    field_add_scal2%f=x1%f+x2
  end function field_add_scal2

  function field_pow(x1,x2)
! -----------------------------------------------------------------------
! field : substract two field type variables
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 10/2012
!
    implicit none
    type(field),intent(in) :: x1
    real(rk),intent(in) :: x2 
    type(field) :: field_pow
    character(len=512) :: name
    Character(len=15) :: num
!    write(num,'(f0.2)')x2
    name=trim(x1%name) !//"**"//num
    call field_init(field_pow,name,x1%nx,x1%ny,x1%nz)
    field_pow%f=x1%f**x2
  end function field_pow

  function field_div(x1,x2)
! -----------------------------------------------------------------------
! field : divide two field type variables
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 10/2012
!
    implicit none
    type(field),intent(in) :: x1,x2
    type(field) :: field_div
    character(len=512) :: name
    name=trim(x1%name) !//"/"//trim(x2%name)
    call field_init(field_div,name,x1%nx,x1%ny,x1%nz)
    field_div%f=x1%f/x2%f
  end function field_div

  function field_div_scal(x1,x2)
! -----------------------------------------------------------------------
! field : divide a field type variables by a scalar
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 10/2012
!
    implicit none
    type(field),intent(in) :: x1
    real(rk),intent(in) :: x2
    type(field) :: field_div_scal
    character(len=512) :: name
    Character(len=15) :: num
!    write(num,'(f0.2)')x2
    name=trim(x1%name) !//"/"//num
    call field_init(field_div_scal,name,x1%nx,x1%ny,x1%nz)
    field_div_scal%f=x1%f/x2
  end function field_div_scal

function scal_div_field(x1,x2)
! -----------------------------------------------------------------------
! field : divide a field type variables by a scalar
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 10/2012
!
    implicit none
    type(field),intent(in) :: x2
    real(rk),intent(in) :: x1
    type(field) :: scal_div_field
    character(len=512) :: name
    Character(len=15) :: num
    !write(num,'(f0.2)')x2
    name=trim(x2%name) !//"/"//num
    call field_init(scal_div_field,name,x2%nx,x2%ny,x2%nz)
    scal_div_field%f=x1/x2%f
  end function scal_div_field

  function field_mul(x1,x2)
! -----------------------------------------------------------------------
! field : multiply two field type variables
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 10/2012
!
    implicit none
    type(field),intent(in) :: x1,x2
    type(field) :: field_mul
    character(len=512) :: name
    name=trim(x1%name) !//"*"//trim(x2%name)
    call field_init(field_mul,name,x1%nx,x1%ny,x1%nz)
    field_mul%f=x1%f*x2%f
  end function field_mul

  function field_mul_scal1(x1,x2)
! -----------------------------------------------------------------------
! field : multiply a scalar and a field type variables
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 10/2012
!
    implicit none
    type(field),intent(in) :: x1
    real(rk),intent(in) :: x2
    type(field) :: field_mul_scal1
    character(len=512) :: name
    Character(len=15) :: num
!    write(num,'(f0.2)')x2
    name=trim(x1%name) !//"*"//num
    call field_init(field_mul_scal1,name,x1%nx,x1%ny,x1%nz)
    field_mul_scal1%f=x1%f*x2
  end function field_mul_scal1

  function field_mul_scal2(x2,x1)
! -----------------------------------------------------------------------
! field : multiply a scalar and a field type variables
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 10/2012
!
    implicit none
    type(field),intent(in) :: x1
    real(rk),intent(in) :: x2
    type(field) :: field_mul_scal2
    character(len=512) :: name
    Character(len=15) :: num
!    write(num,'(f0.2)')x2
    name=trim(x1%name) !//"*"//num
    call field_init(field_mul_scal2,name,x1%nx,x1%ny,x1%nz)
    field_mul_scal2%f=x1%f*x2
  end function field_mul_scal2

  function field_sub(x1,x2)
! -----------------------------------------------------------------------
! field : substract two field type variables
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 10/2012
!
    implicit none
    type(field),intent(in) :: x1,x2
    type(field) :: field_sub
    character(len=512) :: name
    name=trim(x1%name) !//"-"//trim(x2%name)
    call field_init(field_sub,name,x1%nx,x1%ny,x1%nz)
    field_sub%f=x1%f-x2%f
  end function field_sub

  function field_sub_scal1(x1,x2)
! -----------------------------------------------------------------------
! field : substract a scalar and a field type variables
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 10/2012
!
    implicit none
    type(field),intent(in) :: x1
    real(rk),intent(in) :: x2
    type(field) :: field_sub_scal1
    character(len=512) :: name
    Character(len=15) :: num
!    write(num,'(f0.2)')x2
    name=trim(x1%name) !//"-"//num
    call field_init(field_sub_scal1,name,x1%nx,x1%ny,x1%nz)
    field_sub_scal1%f=x1%f-x2
  end function field_sub_scal1

  function field_sub_scal2(x2,x1)
! -----------------------------------------------------------------------
! field : substract a scalar and a field type variables
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 10/2012
!
    implicit none
    type(field),intent(in) :: x1
    real(rk),intent(in) :: x2
    type(field) :: field_sub_scal2
    character(len=512) :: name
    Character(len=15) :: num
!    write(num,'(f0.2)')x2
    name=trim(x1%name) !//"-"//num
    call field_init(field_sub_scal2,name,x1%nx,x1%ny,x1%nz)
    field_sub_scal2%f=x1%f-x2
  end function field_sub_scal2

  function field_neg(x1)
! -----------------------------------------------------------------------
! field : negate a field type variable
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 10/2012
!
    implicit none
    type(field),intent(in) :: x1
    type(field) :: field_neg
    character(len=512) :: name
!    name="-"//trim(x1%name)
    name=trim(x1%name)
    call field_init(field_neg,name,x1%nx,x1%ny,x1%nz)
    field_neg%f=-x1%f
  end function field_neg

  subroutine field_zero_edges(x1)
! -----------------------------------------------------------------------
! field : negate a field type variableput zero in edges and corner
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 11/2012
!
    implicit none
    type(field),intent(inout) :: x1
    
    x1%f(1,1,:)=0._rk
    x1%f(x1%nx,1,:)=0._rk
    x1%f(1,x1%ny,:)=0._rk
    x1%f(x1%nx,x1%ny,:)=0._rk

    x1%f(1,:,1)=0._rk
    x1%f(x1%nx,:,1)=0._rk
    x1%f(1,:,x1%nz)=0._rk
    x1%f(x1%nx,:,x1%nz)=0._rk

    x1%f(:,1,1)=0._rk
    x1%f(:,x1%ny,1)=0._rk
    x1%f(:,1,x1%nz)=0._rk
    x1%f(:,x1%ny,x1%nz)=0._rk

  end subroutine field_zero_edges

  subroutine field_put_boundary(x1,bc,inter)
! -----------------------------------------------------------------------
! field : put bc into field
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 04/2013
!
    implicit none
    type(field),intent(inout) :: x1
    type(boundary_condition),intent(in) :: bc
    integer(ik) :: l,m,inter(3,2)
    
    do m=1,2
       do l=1,3
          if (inter(l,m)<0) then
             if (l==1.and.m==1) x1%f(1,2:x1%ny-1,2:x1%nz-1)=bc%bcx(:,:,1)
             if (l==1.and.m==2) x1%f(x1%nx,2:x1%ny-1,2:x1%nz-1)=bc%bcx(:,:,2)

             if (l==2.and.m==1) x1%f(2:x1%nx-1,1,2:x1%nz-1)=bc%bcy(:,:,1)
             if (l==2.and.m==2) x1%f(2:x1%nx-1,x1%ny,2:x1%nz-1)=bc%bcy(:,:,2)

             if (l==3.and.m==1) x1%f(2:x1%nx-1,2:x1%ny-1,1)=bc%bcz(:,:,1)
             if (l==3.and.m==2) x1%f(2:x1%nx-1,2:x1%ny-1,x1%nz)=bc%bcz(:,:,2)
          endif
       enddo
    enddo

  end subroutine field_put_boundary

  subroutine boundary_put_field(x1,bc,inter)
! -----------------------------------------------------------------------
! field : put field into bc
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 04/2013
!
    implicit none
    type(field),intent(in) :: x1
    type(boundary_condition),intent(inout) :: bc
    integer(ik) :: l,m,inter(3,2)
    
    do m=1,2
       do l=1,3
          !if (inter(l,m)<0) then
             if (l==1.and.m==1) bc%bcx(:,:,1)=x1%f(1,2:x1%ny-1,2:x1%nz-1)
             if (l==1.and.m==2) bc%bcx(:,:,2)=x1%f(x1%nx,2:x1%ny-1,2:x1%nz-1)

             if (l==2.and.m==1) bc%bcy(:,:,1)=x1%f(2:x1%nx-1,1,2:x1%nz-1)
             if (l==2.and.m==2) bc%bcy(:,:,2)=x1%f(2:x1%nx-1,x1%ny,2:x1%nz-1)

             if (l==3.and.m==1) bc%bcz(:,:,1)=x1%f(2:x1%nx-1,2:x1%ny-1,1)
             if (l==3.and.m==2) bc%bcz(:,:,2)=x1%f(2:x1%nx-1,2:x1%ny-1,x1%nz)
          !endif
       enddo
    enddo

  end subroutine boundary_put_field
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

  subroutine field_destroy(x)
! -----------------------------------------------------------------------
! field :  destroy field type variable
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 04/2011
!
    implicit none
    type(field),intent(inout) :: x 
    x%nx=-1 ; x%ny=-1 ; x%nz=-1
    x%name='noname' 
    x%nxn='resolution_x'
    x%nyn='resolution_y'
    x%nzn='resolution_z'
    if (allocated(x%f)) then
       deallocate(x%f)
    else
       call error_stop("ERROR : Type field not allocated -> cannot destroy")
    endif
  end subroutine field_destroy

! =======================================================================
! =======================================================================
! field : input and output methods
! =======================================================================
! =======================================================================

  subroutine write_field(file_name,x,mpid,dbl,inter)
! -----------------------------------------------------------------------
! field : create/open and add field variable in output file
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 07/2011
!
    use class_md
    use class_io
    implicit none
    type(field),intent(in) :: x
    character(len=*),intent(in) :: file_name
    type(mpi_data),intent(in),optional :: mpid
    character(*),optional :: dbl,inter

    if (present(mpid)) then
       if (present(dbl)) then
          if (present(inter)) then
             call write_var3d(file_name//".nc",&
                  [character(len=512) :: x%nxn,x%nyn,x%nzn],&
                  (/x%nx,x%ny,x%nz/),x%name,x%f,mpid=mpid,dbl=dbl,inter=inter)
          else
             call write_var3d(file_name//".nc",&
                  [character(len=512) :: x%nxn,x%nyn,x%nzn],&
                  (/x%nx,x%ny,x%nz/),x%name,x%f,mpid=mpid,dbl=dbl)
          endif
       else
          if (present(inter)) then
             call write_var3d(file_name//".nc",&
                  [character(len=512) :: x%nxn,x%nyn,x%nzn],&
                  (/x%nx,x%ny,x%nz/),x%name,x%f,mpid=mpid,inter=inter)
          else
             call write_var3d(file_name//".nc",&
                  [character(len=512) :: x%nxn,x%nyn,x%nzn],&
                  (/x%nx,x%ny,x%nz/),x%name,x%f,mpid=mpid)
          endif
       endif
    else
       if (present(dbl)) then
          call write_var3d(file_name//".nc",&
               [character(len=512) :: x%nxn,x%nyn,x%nzn],&
               (/x%nx,x%ny,x%nz/),x%name,x%f,dbl=dbl)
       else
          call write_var3d(file_name//".nc",&
               [character(len=512) :: x%nxn,x%nyn,x%nzn],&
               (/x%nx,x%ny,x%nz/),x%name,x%f)
       endif
    endif

  end subroutine write_field

  subroutine read_field(file_name,x,var_name,mpid,inter)
! -----------------------------------------------------------------------
! field : read field variable in input file
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 04/2013
!
    use class_md
    use class_io
    implicit none
    type(field),intent(inout) :: x
    character(len=*),intent(in) :: file_name,var_name
    integer(ik) :: dim_len(3)
    character(len=512) :: dim_name(3)
    type(mpi_data),intent(in),optional :: mpid
    character(*),optional :: inter

    !-> get field informations in input file 
    call get_var3d_info(file_name//".nc",var_name,dim_name,dim_len)

    !-> initialize field
    if (present(mpid)) then
       dim_len(1)=x%nx
       dim_len(2)=x%ny
       dim_len(3)=x%nz
    endif
    call field_init(x,var_name,dim_len(1),dim_len(2),dim_len(3),&
         n1n=dim_name(1),n2n=dim_name(2),n3n=dim_name(3))

    !-> read field array in input file
    if (present(mpid)) then
       if (present(inter)) then
          call read_var3d(file_name//".nc",x%name,x%f,mpid=mpid,inter=inter)
       else
          call read_var3d(file_name//".nc",x%name,x%f,mpid=mpid)
       endif
    else
       call read_var3d(file_name//".nc",x%name,x%f)
    endif

  end subroutine read_field

! =======================================================================
! =======================================================================
! field : derivatives methods
! =======================================================================
! =======================================================================

  function derx(dc,x)
! -----------------------------------------------------------------------
! field : compute first derivative in x direction
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 05/2011
!
!$ use OMP_LIB
    use class_derivatives
    implicit none
    type(field) :: derx
    type(derivatives_coefficients),intent(in) :: dc
    type(field),intent(in) :: x
    integer(ik) :: i,j,k
    real(rk) :: in(x%nx),out(x%nx)
    call field_init(derx,"DXU",x%nx,x%ny,x%nz)

    in=0._rk ; out=0._rk
    if (dertype(dc)) then
!$OMP PARALLEL 
!$OMP DO  PRIVATE(k,in,out) SCHEDULE(RUNTIME)
       do k=2,x%nz-1
          in(:)=x%f(:,1,k) ; call der_s(dc,in,out) ; derx%f(:,1,k)=out(:)
          in(:)=x%f(:,x%ny,k) ; call der_s(dc,in,out) ; derx%f(:,x%ny,k)=out(:)
       enddo
!$OMP END DO
!$OMP DO  PRIVATE(j,in,out) SCHEDULE(RUNTIME)
       do j=2,x%ny-1
          in(:)=x%f(:,j,1) ; call der_s(dc,in,out) ; derx%f(:,j,1)=out(:)
          in(:)=x%f(:,j,x%nz) ; call der_s(dc,in,out) ; derx%f(:,j,x%nz)=out(:)
       enddo
!$OMP END DO
!$OMP DO  PRIVATE(k,in,out) SCHEDULE(RUNTIME)
       do k=2,x%nz-1
          do j=2,x%ny-1
             in(:)=x%f(:,j,k)
             call der(dc,in,out)
             derx%f(:,j,k)=out(:)
          enddo
       enddo
!$OMP END DO
!$OMP END PARALLEL
    else
!$OMP PARALLEL PRIVATE(k,in,out)
!$OMP DO SCHEDULE(RUNTIME)
       do k=1,x%nz
          do j=1,x%ny
             in(:)=x%f(:,j,k)
             call der(dc,in,out)
             derx%f(:,j,k)=out(:)
!            call der(dc,x%f(:,j,k),derx%f(:,j,k))
          enddo
       enddo
!$OMP END DO
!$OMP END PARALLEL
    end if

  end function derx

  function dderx(dc,x)
! -----------------------------------------------------------------------
! field : compute second derivative in x direction
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 05/2011
!
!$ use OMP_LIB
    use class_derivatives
    implicit none
    type(field) :: dderx
    type(derivatives_coefficients),intent(in) :: dc
    type(field),intent(in) :: x
    integer(ik) :: i,j,k
    real(rk) :: in(x%nx),out(x%nx)
    real(rk) :: t1,t2
    call field_init(dderx,"DDXU",x%nx,x%ny,x%nz)
    
    in=0._rk ; out=0._rk
    if (dertype(dc)) then
!$OMP PARALLEL 
!$OMP DO  PRIVATE(k,in,out) SCHEDULE(RUNTIME)
       do k=2,x%nz-1
          in(:)=x%f(:,1,k) ; call dder_s(dc,in,out) ; dderx%f(:,1,k)=out(:)
          in(:)=x%f(:,x%ny,k) ; call dder_s(dc,in,out) ; dderx%f(:,x%ny,k)=out(:)
       enddo
!$OMP END DO
!$OMP DO  PRIVATE(j,in,out) SCHEDULE(RUNTIME)
       do j=2,x%ny-1
          in(:)=x%f(:,j,1) ; call dder_s(dc,in,out) ; dderx%f(:,j,1)=out(:)
          in(:)=x%f(:,j,x%nz) ; call dder_s(dc,in,out) ; dderx%f(:,j,x%nz)=out(:)
       enddo
!$OMP END DO
!$OMP DO  PRIVATE(k,in,out) SCHEDULE(RUNTIME)
       do k=2,x%nz-1
          do j=2,x%ny-1
             in(:)=x%f(:,j,k)
             call dder(dc,in,out)
             dderx%f(:,j,k)=out(:)
          enddo
       enddo
!$OMP END DO
!$OMP END PARALLEL
    else
!$OMP PARALLEL PRIVATE(k,in,out)
!$OMP DO SCHEDULE(RUNTIME)
       do k=1,x%nz
          do j=1,x%ny
             in(:)=x%f(:,j,k)
             call dder(dc,in,out)
             dderx%f(:,j,k)=out(:)
          enddo
       enddo
!$OMP END DO
!$OMP END PARALLEL
    endif
  end function dderx

  function dery(dc,x)
! -----------------------------------------------------------------------
! field : compute first derivative in x direction
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 05/2011
!
!$ use OMP_LIB
    use class_derivatives
    implicit none
    type(field) :: dery
    type(derivatives_coefficients),intent(in) :: dc
    type(field),intent(in) :: x
    integer(ik) :: i,j,k
    real(rk) :: in(x%ny),out(x%ny)
    call field_init(dery,"DXU",x%nx,x%ny,x%nz)

    in=0._rk ; out=0._rk
    if (dertype(dc)) then
!$OMP PARALLEL 
!$OMP DO  PRIVATE(k,in,out) SCHEDULE(RUNTIME)
       do k=2,x%nz-1
          in(:)=x%f(1,:,k) ; call der_s(dc,in,out) ; dery%f(1,:,k)=out(:)
          in(:)=x%f(x%nx,:,k) ; call der_s(dc,in,out) ; dery%f(x%nx,:,k)=out(:)
       enddo
!$OMP END DO
!$OMP DO  PRIVATE(i,in,out) SCHEDULE(RUNTIME)
       do i=2,x%nx-1
          in(:)=x%f(i,:,1) ; call der_s(dc,in,out) ; dery%f(i,:,1)=out(:)
          in(:)=x%f(i,:,x%nz) ; call der_s(dc,in,out) ; dery%f(i,:,x%nz)=out(:)
       enddo
!$OMP END DO
!$OMP DO  PRIVATE(k,in,out) SCHEDULE(RUNTIME)
       do k=2,x%nz-1
          do i=2,x%nx-1
             in(:)=x%f(i,:,k)
             call der(dc,in,out)
             dery%f(i,:,k)=out(:)
          enddo
       enddo
!$OMP END DO
!$OMP END PARALLEL
    else
!$OMP PARALLEL PRIVATE(k,in,out)
!$OMP DO SCHEDULE(RUNTIME)
       do k=1,x%nz
          do i=1,x%nx
             in(:)=x%f(i,:,k)
             call der(dc,in,out)
             dery%f(i,:,k)=out(:)
          enddo
       enddo
!$OMP END DO
!$OMP END PARALLEL
    endif

  end function dery

  function ddery(dc,x)
! -----------------------------------------------------------------------
! field : compute second derivative in x direction
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 05/2011
!
!$ use OMP_LIB
    use class_derivatives
    implicit none
    type(field) :: ddery
    type(derivatives_coefficients),intent(in) :: dc
    type(field),intent(in) :: x
    integer(ik) :: i,j,k
    real(rk) :: in(x%ny),out(x%ny)
    call field_init(ddery,"DDXU",x%nx,x%ny,x%nz)
    
    in=0._rk ; out=0._rk
    if (dertype(dc)) then
!$OMP PARALLEL 
!$OMP DO  PRIVATE(k,in,out) SCHEDULE(RUNTIME)
       do k=2,x%nz-1
          in(:)=x%f(1,:,k) ; call dder_s(dc,in,out) ; ddery%f(1,:,k)=out(:)
          in(:)=x%f(x%nx,:,k) ; call dder_s(dc,in,out) ; ddery%f(x%nx,:,k)=out(:)
       enddo
!$OMP END DO
!$OMP DO  PRIVATE(i,in,out) SCHEDULE(RUNTIME)
       do i=2,x%nx-1
          in(:)=x%f(i,:,1) ; call dder_s(dc,in,out) ; ddery%f(i,:,1)=out(:)
          in(:)=x%f(i,:,x%nz) ; call dder_s(dc,in,out) ; ddery%f(i,:,x%nz)=out(:)
       enddo
!$OMP END DO
!$OMP DO  PRIVATE(k,in,out) SCHEDULE(RUNTIME)
       do k=2,x%nz-1
          do i=2,x%nx-1
             in(:)=x%f(i,:,k)
             call dder(dc,in,out)
             ddery%f(i,:,k)=out(:)
          enddo
       enddo
!$OMP END DO
!$OMP END PARALLEL
    else
!$OMP PARALLEL PRIVATE(k,in,out)
!$OMP DO SCHEDULE(RUNTIME)
       do k=1,x%nz
          do i=1,x%nx
             in(:)=x%f(i,:,k)
             call dder(dc,in,out)
             ddery%f(i,:,k)=out(:)
          enddo
       enddo
!$OMP END DO
!$OMP END PARALLEL
    endif

  end function ddery

  function derz(dc,x)
! -----------------------------------------------------------------------
! field : compute first derivative in z direction
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 05/2011
!
!$ use OMP_LIB
    use class_derivatives
    implicit none
    type(field) :: derz
    type(derivatives_coefficients),intent(in) :: dc
    type(field),intent(in) :: x
    integer(ik) :: i,j,k
    real(rk) :: in(x%nz),out(x%nz)
    call field_init(derz,"DZU",x%nx,x%ny,x%nz)
    
    in=0._rk ; out=0._rk
    if (dertype(dc)) then
!$OMP PARALLEL 
!$OMP DO  PRIVATE(j,in,out) SCHEDULE(RUNTIME)
       do j=2,x%ny-1
          in(:)=x%f(1,j,:) ; call der_s(dc,in,out) ; derz%f(1,j,:)=out(:)
          in(:)=x%f(x%nx,j,:) ; call der_s(dc,in,out) ; derz%f(x%nx,j,:)=out(:)
       enddo
!$OMP END DO
!$OMP DO  PRIVATE(i,in,out) SCHEDULE(RUNTIME)
       do i=2,x%nx-1
          in(:)=x%f(i,1,:) ; call der_s(dc,in,out) ; derz%f(i,1,:)=out(:)
          in(:)=x%f(i,x%ny,:) ; call der_s(dc,in,out) ; derz%f(i,x%ny,:)=out(:)
       enddo
!$OMP END DO
!$OMP DO  PRIVATE(j,in,out) SCHEDULE(RUNTIME)
       do j=2,x%ny-1
          do i=2,x%nx-1
             in(:)=x%f(i,j,:)
             call der(dc,in,out)
             derz%f(i,j,:)=out(:)
          enddo
       enddo
!$OMP END DO
!$OMP END PARALLEL
    else
!$OMP PARALLEL PRIVATE(j,in,out)
!$OMP DO SCHEDULE(RUNTIME)
       do j=1,x%ny
          do i=1,x%nx
             in(:)=x%f(i,j,:)
             call der(dc,in,out)
             derz%f(i,j,:)=out(:)
          enddo
       enddo
!$OMP END DO
!$OMP END PARALLEL
    endif

  end function derz

  function dderz(dc,x)
! -----------------------------------------------------------------------
! field : compute first derivative in z direction
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 05/2011
!
!$ use OMP_LIB
    use class_derivatives
    implicit none
    type(field) :: dderz
    type(derivatives_coefficients),intent(in) :: dc
    type(field),intent(in) :: x
    integer(ik) :: i,j,k
    real(rk) :: in(x%nz),out(x%nz)
    call field_init(dderz,"DDZU",x%nx,x%ny,x%nz)
    
    in=0._rk ; out=0._rk
    if (dertype(dc)) then
!$OMP PARALLEL 
!$OMP DO  PRIVATE(j,in,out) SCHEDULE(RUNTIME)
       do j=2,x%ny-1
          in(:)=x%f(1,j,:) ; call dder_s(dc,in,out) ; dderz%f(1,j,:)=out(:)
          in(:)=x%f(x%nx,j,:) ; call dder_s(dc,in,out) ; dderz%f(x%nx,j,:)=out(:)
       enddo
!$OMP END DO
!$OMP DO  PRIVATE(i,in,out) SCHEDULE(RUNTIME)
       do i=2,x%nx-1
          in(:)=x%f(i,1,:) ; call dder_s(dc,in,out) ; dderz%f(i,1,:)=out(:)
          in(:)=x%f(i,x%ny,:) ; call dder_s(dc,in,out) ; dderz%f(i,x%ny,:)=out(:)
       enddo
!$OMP END DO
!$OMP DO  PRIVATE(j,in,out) SCHEDULE(RUNTIME)
       do j=2,x%ny-1
          do i=2,x%nx-1
             in(:)=x%f(i,j,:)
             call dder(dc,in,out)
             dderz%f(i,j,:)=out(:)
          enddo
       enddo
!$OMP END DO
!$OMP END PARALLEL
    else
!$OMP PARALLEL PRIVATE(j,in,out)
!$OMP DO SCHEDULE(RUNTIME)
       do j=1,x%ny
          do i=1,x%nx
             in(:)=x%f(i,j,:)
             call dder(dc,in,out)
             dderz%f(i,j,:)=out(:)
          enddo
       enddo
!$OMP END DO
!$OMP END PARALLEL
    endif

  end function dderz
  
! =======================================================================
! =======================================================================
! field : solver methods
! =======================================================================
! =======================================================================

  subroutine solve_poisson(dc,rhs,sol)
! -----------------------------------------------------------------------
! field : solve 1d compact difference poisson problems
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 02/2012
!
    use class_solver_1d
    implicit none
    type(solver_coefficients),intent(in) :: dc
    type(field),intent(in) :: rhs
    type(field),intent(inout) :: sol
    integer(ik) :: i,j,k
    real(rk) :: in(rhs%nx),out(rhs%nx)

    do k=2,sol%nz-1
       do j=2,sol%ny-1
          in(:)=rhs%f(:,j,k)
          out(1)=sol%f(1,j,k)
          out(sol%nx)=sol%f(sol%nx,j,k)

          call solve_poisson_1d(dc,in,out)
          sol%f(2:sol%nx-1,j,k)=out(2:sol%nx-1)

       enddo
    enddo

  end subroutine solve_poisson

  subroutine boundary_condition_init(x,n1,n2,n3)
! -----------------------------------------------------------------------
! field : initialize boundary condition type variable
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 07/2012
!
    implicit none
    type(boundary_condition),intent(inout) :: x 
    integer(ik),intent(in) :: n1,n2,n3

    if (x%nx/=n1-2.or.x%ny/=n2-2.or.x%nz/=n3-2) then
       x%nx=n1-2 ; x%ny=n2-2 ; x%nz=n3-2

       !-> bc x-direction
       if (.not.allocated(x%bcx)) then
          allocate(x%bcx(x%ny,x%nz,2))
       else
          deallocate(x%bcx) ; allocate(x%bcx(x%ny,x%nz,2))
       endif
       
       !-> bc y-direction
       if (.not.allocated(x%bcy)) then
          allocate(x%bcy(x%nx,x%nz,2))
       else
          deallocate(x%bcy) ; allocate(x%bcy(x%nx,x%nz,2))
       endif
       
       !-> bc z-direction
       if (.not.allocated(x%bcz)) then
          allocate(x%bcz(x%nx,x%ny,2))
       else
          deallocate(x%bcz) ; allocate(x%bcz(x%nx,x%ny,2))
       endif

    else
       x%nx=n1-2 ; x%ny=n2-2 ; x%nz=n3-2

       !-> bc x-direction
       if (.not.allocated(x%bcx)) then
          allocate(x%bcx(x%ny,x%nz,2))
       endif

       !-> bc y-direction
       if (.not.allocated(x%bcy)) then
          allocate(x%bcy(x%nx,x%nz,2))
       endif
       
       !-> bc z-direction
       if (.not.allocated(x%bcz)) then
          allocate(x%bcz(x%nx,x%ny,2))
       endif
       
    endif

  end subroutine boundary_condition_init

  subroutine solver_3d(sc,g,u,bc,sigma)
! -----------------------------------------------------------------------
! field : solve 3d system 
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 05/2012
!
!$ use OMP_LIB
    use class_solver_3d
    implicit none
    type(solver_coeffs_3d),intent(in) :: sc
    type(boundary_condition),intent(in) :: bc 
    type(field) :: g,u
    real(rk),intent(in) :: sigma
    real(rk) :: gr(u%nx-2,u%ny-2,u%nz-2)!,ur(u%nx-2,u%ny-2,u%nz-2)
    integer(ik) :: n1,n2,n3,k
    real(rk) :: t1,t2

    n1=u%nx ; n2=u%ny ; n3=u%nz

!    call cpu_time(t1)
!$OMP PARALLEL PRIVATE(k)
!$OMP DO SCHEDULE(RUNTIME)
!    gr(1:n1-2,1:n2-2,1:n3-2)=g%f(2:n1-1,2:n2-1,2:n3-1)
    do k=1,n3-2
       gr(1:n1-2,1:n2-2,k)=g%f(2:n1-1,2:n2-1,k+1)
    enddo
!$OMP END DO
!$OMP END PARALLEL
!    call cpu_time(t2)
!    print*,'copy',t2-t1

    !ur(1:n1-2,1:n2-2,1:n3-2)=u%f(2:n1-1,2:n2-1,2:n3-1)
    
    call solve_3d(sc,gr,sigma,u%f,bc%bcx,bc%bcy,bc%bcz,n1-2,n2-2,n3-2)
    
    
  end subroutine solver_3d

  
! =======================================================================
! =======================================================================
! field : multidomain methods
! =======================================================================
! =======================================================================

  subroutine md_set_guess(mpid,inf,nt,it,bc,x)
! -----------------------------------------------------------------------
! navier : solve u helmholtz problem
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 12/2012
!
    use class_md
    implicit none
    type(mpi_data) :: mpid
    type(mpi_inf_mat),intent(inout) :: inf
    integer(ik) :: nt,it(nt),ex(3,2)
    type(field),intent(in) :: x(nt)
    type(boundary_condition),intent(inout) :: bc(nt)
    real(rk) :: bcx(bc(1)%ny,bc(1)%nz,2),bcy(bc(1)%nx,bc(1)%nz,2)
    real(rk) :: bcz(bc(1)%nx,bc(1)%ny,2)

    !-> compute extrema
    ex(1,1)=2 ; ex(2,1)=2 ; ex(3,1)=2 
    ex(1,2)=x(1)%nx-1 ; ex(2,2)=x(1)%ny-1 ; ex(3,2)=x(1)%nz-1

    !-> compute guess
    bcx=bc(it(nt))%bcx
    bcy=bc(it(nt))%bcy
    bcz=bc(it(nt))%bcz

!    bcx(:,:,1)=x(it(nt))%f(1,ex(2,1):ex(2,2),ex(3,1):ex(3,2))
!    bcx(:,:,2)=x(it(nt))%f(x(1)%nx,ex(2,1):ex(2,2),ex(3,1):ex(3,2))
!    bcy(:,:,1)=x(it(nt))%f(ex(1,1):ex(1,2),1,ex(3,1):ex(3,2))
!    bcy(:,:,2)=x(it(nt))%f(ex(1,1):ex(1,2),x(1)%ny,ex(3,1):ex(3,2))
!    bcz(:,:,1)=x(it(nt))%f(ex(1,1):ex(1,2),ex(2,1):ex(2,2),1)
!    bcz(:,:,2)=x(it(nt))%f(ex(1,1):ex(1,2),ex(2,1):ex(2,2),x(1)%nz)

!    bcx=0._rk
!    bcy=0._rk
!    bcz=0._rk
    
    !-> set values in petsc sol vector
    call md_vector_sol_setvalues(mpid,inf,bcx,bcy,bcz,x(1)%nx-2,x(1)%ny-2,x(1)%nz-2)

  end subroutine md_set_guess

! -----------------------------------------------------------------------
! field : create influence matrix (from scratch or read)
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 09/2012
!
  subroutine md_boundary_condition_init(mpid,inf,bctype)
    use class_md
    implicit none
    type(mpi_data),intent(inout) :: mpid
    type(mpi_inf_mat),intent(inout) :: inf
    integer(ik) :: bctype(6)

    call md_bc_init(mpid,inf,bctype)

  end subroutine md_boundary_condition_init

! -----------------------------------------------------------------------
! field : create influence matrix (from scratch or read)
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 09/2012
!
  subroutine influence_matrix_init_start(mpid,inf,sc,bc,u,g,sigma,dcx,dcy,dcz,var)
    use class_md
    use class_derivatives
    use class_solver_3d
    implicit none
    type(mpi_data),intent(inout) :: mpid
    type(mpi_inf_mat),intent(inout) :: inf
    type(boundary_condition),intent(inout) :: bc
    type(solver_coeffs_3d),intent(in) :: sc
    type(field),intent(inout) :: u,g
    type(derivatives_coefficients),intent(in) :: dcx,dcy,dcz
    real(rk),intent(in) :: sigma
    integer(8) :: t1,t2,irate
    character(md_lenght_char) :: filename
    character(*) :: var
    logical :: file_exist
    real(rk) :: time,timet
    
    !-> set filename of influence matrix
    call md_influence_matrix_filename(mpid,filename,var)
    !-> test if file exist
    inquire(file=filename,exist=file_exist)

    !-> compute parameters and initialize the influence matrix 
    call md_influence_matrix_init_start(mpid,inf,file_exist)

  end subroutine influence_matrix_init_start
! -----------------------------------------------------------------------
! field : create influence matrix (from scratch or read)
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 09/2012
!
  subroutine influence_matrix_init_end(mpid,inf,sc,bc,u,g,sigma,dcx,dcy,dcz,&
       var,null)
    use class_md
    use class_derivatives
    use class_solver_3d
    implicit none
    type(mpi_data),intent(inout) :: mpid
    type(mpi_inf_mat),intent(inout) :: inf
    type(boundary_condition),intent(inout) :: bc
    type(solver_coeffs_3d),intent(in) :: sc
    type(field),intent(inout) :: u,g
    type(derivatives_coefficients),intent(in) :: dcx,dcy,dcz
    real(rk),intent(in) :: sigma
    integer(8) :: t1,t2,irate
    character(md_lenght_char) :: filename
    character(*) :: var
    logical :: file_exist
    real(rk) :: time,timet
    integer(ik) :: m
    integer(ik),optional :: null
    
    !-> set filename of influence matrix
    call md_influence_matrix_filename(mpid,filename,var)
    !-> test if file exist
    inquire( file=filename,exist=file_exist)

    !-> compute parameters and initialize the influence matrix 
!    call md_influence_matrix_init_start(mpid,inf,file_exist)

    call system_clock(t1,irate)
    if (file_exist) then
       !-> read matrix from file
       call md_influence_matrix_read(mpid,inf,filename)
    else
       !-> set location of values in the influence matrix
       call md_influence_matrix_set(mpid,inf,sc,bc,u,g,sigma,dcx,dcy,dcz)
       !->  final assembly : put values in the influence matrix
       call md_influence_matrix_init_end(mpid,inf)
    endif
    call system_clock(t2,irate)

    !-> pressure singular method
    if (var=='p') then 
       if (present(null)) then
          if (null==2) then
             call md_add_pert(mpid,inf)
             call md_influence_matrix_init_end(mpid,inf)
          endif
       endif
    endif

    !-> print matrix (only for small tests case < 1024 rows)
    !call md_influence_matrix_view(mpid,inf)

    !-> get matrix infos
    call md_influence_matrix_infos(mpid,inf)

    !-> write matrix in binary format
    if (.not.file_exist) then
       call md_influence_matrix_write(mpid,inf,filename)
    endif

    time=real(t2-t1)/real(irate)
   ! call MPI_Reduce(time,timet,1,MPI_DOUBLE,MPI_SUM,0,MPI_COMM_WORLD)
    call md_mpi_reduce_double_sum(mpid,time,timet)
    if (mpid%rank==0) print*,'time set matrix',timet/mpid%processus

    !-> initialize auxiliary fields
    u%f=0._rk ; g%f=0._rk
    bc%bcx=0._rk ;bc%bcy=0._rk ;bc%bcz=0._rk  


  end subroutine influence_matrix_init_end

! -----------------------------------------------------------------------
! field : solve influence matrix
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 09/2012
!
  subroutine multidomain_solve(mpid,inf,sc,bc,u,g,du,sigma,dcx,dcy,dcz,null,&
       inf_sol,var)
    use mpi
    use class_md
    use class_derivatives
    use class_solver_3d
    implicit none
    type(mpi_data),intent(inout) :: mpid
    type(mpi_inf_mat),intent(inout) :: inf
    type(mpi_inf_sol),intent(inout),optional :: inf_sol
    type(boundary_condition),intent(inout) :: bc
    type(solver_coeffs_3d),intent(in) :: sc
    type(field),intent(inout) :: u,g,du
    integer(ik),optional :: null
    type(derivatives_coefficients),intent(in) :: dcx,dcy,dcz
    real(rk),intent(in) :: sigma
    real(rk) :: t(6),time(3),timet(3),sign
    real(rk) :: bcx(bc%ny,bc%nz,2),bcy(bc%nx,bc%nz,2),bcz(bc%nx,bc%ny,2)
    integer(ik) :: i,j,k
    character(*),optional :: var

    !-> integrate nonhomogeneous system
    t(1)=MPI_WTIME()
    u%f=0._rk
    du%f=0._rk
    call solver_3d(sc,g,u,bc,sigma)
  
    !-> compute derivatives
    sign=1._rk
    du=derx(dcx,u)
    bcx(:,:,1)=-sign*du%f(1,2:u%ny-1,2:u%nz-1)
    bcx(:,:,2)=sign*du%f(u%nx,2:u%ny-1,2:u%nz-1)
    du=dery(dcy,u)
    bcy(:,:,1)=-sign*du%f(2:u%nx-1,1,2:u%nz-1)
    bcy(:,:,2)=sign*du%f(2:u%nx-1,u%ny,2:u%nz-1)
    du=derz(dcz,u)
    bcz(:,:,1)=-sign*du%f(2:u%nx-1,2:u%ny-1,1)
    bcz(:,:,2)=sign*du%f(2:u%nx-1,2:u%ny-1,u%nz)
  
  !-> put values in rhs petsc vector
    call md_vector_setvalues(mpid,inf,bcx,bcy,bcz,bc%nx,bc%ny,bc%nz)
    t(2)=MPI_WTIME()
    !call md_rhs_view(mpid,inf)

    !-> pressure singular method
    if (present(var)) then
       if (var=='p') then
          if (present(null)) then
             !-> set rhs nullspace if singular system
             if (null==1) call md_rhs_nullspace(mpid,inf)
             !->
             if (null==2) then
                call md_vector_zero_lastpoint(mpid,inf)
             endif
          endif
       endif
    endif

    !-> compute interface condition
    t(3)=MPI_WTIME()
    if (present(inf_sol)) then
       call md_solve(mpid,inf,inf_sol=inf_sol)
    else
       call md_solve(mpid,inf)
    endif
    t(4)=MPI_WTIME()
    !call md_sol_view(mpid,inf)

    !-> get values from solution petsc vector
    call md_vector_getvalues(mpid,inf,bc%bcx,bc%bcy,bc%bcz,bc%nx,bc%ny,bc%nz)

!    print*,'solve',maxval(abs(bcx))
    !if (mpid%rank==0) print'(8es13.5)',bc%bcx(:,:,2)

    !-> compute solution
    t(5)=MPI_WTIME()
    call solver_3d(sc,g,u,bc,sigma)

    !-> interpolate vertex solution
    !call md_vertex_interpolate(mpid,inf,bc%bcx,bc%bcy,bc%bcz,bc%nx,bc%ny,bc%nz)
    t(6)=MPI_WTIME()

    !-> time output
    time(1)=t(2)-t(1) ; time(2)=t(4)-t(3) ; time(3)=t(6)-t(5)
!    call md_mpi_reduce_double_sum(mpid,time(1),timet(1))
!    call md_mpi_reduce_double_sum(mpid,time(2),timet(2))
!    call md_mpi_reduce_double_sum(mpid,time(3),timet(3))
!    timet(:)=timet(:)/mpid%processus
!    if (mpid%rank==0) print*,'Time Solution',timet
    
  end subroutine multidomain_solve

! -----------------------------------------------------------------------
! field : set values in influence matrix
! -----------------------------------------------------------------------
! Matthieu Marquillie
! 09/2012
!
  subroutine md_influence_matrix_set(mpid,inf,sc,bc,u,g,sigma,dcx,dcy,dcz)
    use class_md
    use class_derivatives
    use class_solver_3d
    implicit none
    type(mpi_data),intent(inout) :: mpid
    type(mpi_inf_mat),intent(inout) :: inf
    type(boundary_condition),intent(inout) :: bc
    type(solver_coeffs_3d),intent(in) :: sc
    type(field),intent(inout) :: u,g
    real(rk),intent(in) :: sigma
    type(derivatives_coefficients),intent(in) :: dcx,dcy,dcz
    integer(ik) :: l,m,c(3),inter(3,2),n1,n2,i,j,idr
    type(field) :: deri
    real(rk) :: dirac,sign
    
    call md_mpi_getcoord(mpid,c)
    call md_get_interfaces_number(inf,c,inter)

    call field_init(deri,"DERI",mpid%nx,mpid%ny,mpid%nz)

    !-> m : left-right ; l : directions (x,y,z)
    do m=1,2
       do l=1,3
          !-> only process effective interface
          !if (inter(l,m)<0) cycle
          if (inter(l,m)>0) then

             !-> set number of points of interface
             if (l==1) then ; n1=bc%ny ; n2=bc%nz ; endif
             if (l==2) then ; n1=bc%nx ; n2=bc%nz ; endif
             if (l==3) then ; n1=bc%nx ; n2=bc%ny ; endif
             
             !-> process all points of interface
             idr=0
             do j=1,n2
                do i=1,n1

                   !-> initialize field and rhs
                   g%f=0._rk ; u%f=0._rk

                   !-> initialize boundary condition
                   bc%bcx=0._rk ; bc%bcy=0._rk ; bc%bcz=0._rk

                   !-> put dirac in boundary condition and field
                   dirac=1._rk
                   if (l==1.and.m==1) then 
                      bc%bcx(i,j,1)=dirac !; u%f(1,i+1,j+1)=dirac
                   endif
                   if (l==1.and.m==2) then
                      bc%bcx(i,j,2)=dirac !; u%f(u%nx,i+1,j+1)=dirac
                   endif
                   if (l==2.and.m==1) then
                      bc%bcy(i,j,1)=dirac !; u%f(i+1,1,j+1)=dirac
                   endif
                   if (l==2.and.m==2) then 
                      bc%bcy(i,j,2)=dirac !; u%f(i+1,u%ny,j+1)=dirac
                   endif
                   if (l==3.and.m==1) then 
                      bc%bcz(i,j,1)=dirac !; u%f(i+1,j+1,1)=dirac
                   endif
                   if (l==3.and.m==2) then 
                      bc%bcz(i,j,2)=dirac !; u%f(i+1,j+1,u%nz)=dirac
                   endif

                   !-> solve laplacian
                   call solver_3d(sc,g,u,bc,sigma)
                   
                   !-> compute derivatives in three directions
                   if (m==1) sign=1._rk
                   if (m==2) sign=-1._rk

                   if (m==1) then
                      deri=derx(dcx,u)
                      bc%bcx(:,:,1)=sign*deri%f(1,2:u%ny-1,2:u%nz-1)
                      bc%bcx(:,:,2)=-sign*deri%f(u%nx,2:u%ny-1,2:u%nz-1)
                      deri=dery(dcy,u)
                      bc%bcy(:,:,1)=sign*deri%f(2:u%nx-1,1,2:u%nz-1)
                      bc%bcy(:,:,2)=-sign*deri%f(2:u%nx-1,u%ny,2:u%nz-1)
                      deri=derz(dcz,u)
                      bc%bcz(:,:,1)=sign*deri%f(2:u%nx-1,2:u%ny-1,1)
                      bc%bcz(:,:,2)=-sign*deri%f(2:u%nx-1,2:u%ny-1,u%nz)
                   endif

                   if (m==2) then
                      deri=derx(dcx,u)
                      bc%bcx(:,:,1)=-sign*deri%f(1,2:u%ny-1,2:u%nz-1)
                      bc%bcx(:,:,2)=sign*deri%f(u%nx,2:u%ny-1,2:u%nz-1)
                      deri=dery(dcy,u)
                      bc%bcy(:,:,1)=-sign*deri%f(2:u%nx-1,1,2:u%nz-1)
                      bc%bcy(:,:,2)=sign*deri%f(2:u%nx-1,u%ny,2:u%nz-1)
                      deri=derz(dcz,u)
                      bc%bcz(:,:,1)=-sign*deri%f(2:u%nx-1,2:u%ny-1,1)
                      bc%bcz(:,:,2)=sign*deri%f(2:u%nx-1,2:u%ny-1,u%nz)
                   endif

                   !-> set values
                   call md_influence_matrix_setvalues(mpid,inf,c,inter(l,m), &
                        bc%bcx,bc%bcy,bc%bcz,bc%nx,bc%ny,bc%nz,idr)

                   idr=idr+1
                enddo
             enddo
          endif

          !-> flush values in influence matrix
          call md_influence_matrix_start_flush(mpid,inf)
          call md_influence_matrix_end_flush(mpid,inf)
          
       enddo
    enddo
    
    call field_destroy(deri)

  end subroutine md_influence_matrix_set

end module class_field
