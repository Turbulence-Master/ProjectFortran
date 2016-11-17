!------------------------------------------------------------------------------
! IMPT, ProjectFortran
!------------------------------------------------------------------------------
!
! MODULE: Module class_io
!
!> @author
!> Module Author Ilkay Solak
!
! DESCRIPTION: 
!>  Input/Output module, by using netcdf files. Brief description of module.
!
! REVISION HISTORY:
! 01/07/2007 - Initial Version by Matthieu Marquillie
! 13/11/2016 - Updated Version by Ilkay Solak
!
! @todo Whole class only works for ndim = 3 in the current version
! @todo Needs to be tested, waiting for CMakeList.txt update
!------------------------------------------------------------------------------

module class_io
  use netcdf
  use precision
  use class_field 
!  use class_mpi    ! not planned yet
  implicit none

  !-> Declare everything private by default
  private

  !-> Declare exported procedure
  public :: write_var3d, read_var3d
  public :: get_dim_size
  public :: get_var3d_info

contains
  
  !---------------------------------------------------------------------------  
  !> @author 
  !> Routine Author Matthieu Marquillie
  !
  ! DESCRIPTION: 
  !> Write 3d variable in a netcdf file
  !> @brief
  !> Write 3d variable in a netcdf file
  !
  ! REVISION HISTORY:
  ! 01/07/2011 - Initial Version by Matthieu Marquillie
  ! 13/11/2016 - Updated Version by Ilkay Solak
  !
  !> @param[in] file_name       - name of the file
  !> @param[in] dim_name(ndim)  - name of the dimensions
  !> @param[in] dim_len(ndim)   - length of the dimensions
  !> @param[in] var_name        - name of the variable
  !> @param[in] var             - variable to save
  !> @param[in] istart(ndim)    - start points, per process if MPI
  !> @param[in] icount(ndim)    - end points, per process if MPI
     
  !> @param[out] N/A      
  !> @return N/A
  !---------------------------------------------------------------------------  
  
  subroutine write_var3d(file_name,dim_name,dim_len,&
                         var_name,var,istart,icount)
    implicit none                     
    integer(ik),parameter :: ndim=3
    character(len=*),intent(in) :: file_name, dim_name(ndim)
    character(len=*),intent(in) :: dim_len(ndim), var_name
    real(rk),intent(in) :: var(:,:,:)
    integer(ik),intent(in) :: istart(3),icount(3)


    logical :: file_exist
    integer(ik) :: varid(1),i
    integer(ik) :: ncid,dimid(ndim),
    integer(ik) :: dim_len_check,dim_var(ndim),imap(3)
    
    !-> check if file exist
    inquire(file=file_name,exist=file_exist)

    !-> open/create file
    if (file_exist) then
      call io_check(nf90_open(path=file_name,mode=nf90_write,ncid=ncid))
      call io_check(nf90_redef(ncid))
    else
      call io_check(nf90_create(path=file_name,cmode=nf90_clobber,ncid=ncid))
    endif

    !-> create/add dimensions
    do i=1,ndim
       if (nf90_inq_dimid(ncid,dim_name(i),dimid(i))/=nf90_noerr) then 
          call io_check(nf90_def_dim(ncid,dim_name(i),dim_len(i),dimid(i)))
       else
          call io_check(nf90_inquire_dimension(ncid,dimid(i),len=dim_len_check))
          if (dim_len_check/=dim_len(i)) call error_stop("NETCDF Error : wrong dimensions")
       endif
    enddo

    !-> if variable exist         : get variable id
    !-> if variable doesn't exist : define variable
    if (nf90_inq_varid(ncid,var_name,varid(1))/=nf90_noerr) then 
       call io_check(nf90_def_var(ncid,var_name,nf90_real,dimid,varid(1)))
    endif

    !-> end of definition
    call io_check(nf90_enddef(ncid))

    dim_var=get_dim_size(var)

    imap(1)=1
    imap(2)=dim_var(1)*imap(1)
    imap(3)=dim_var(2)*imap(2)

    !-> write field variable
    call io_check(nf90_put_var(ncid,varid(1),var,start=istart,count=icount,map=imap))

    !-> close file
    call io_check(nf90_close(ncid))

  end subroutine write_var3d

  !---------------------------------------------------------------------------  
  !> @author 
  !> Routine Author Matthieu Marquillie
  !
  ! DESCRIPTION: 
  !> Read 3d variable from a netcdf file
  !> @brief
  !> Read 3d variable from a netcdf file
  !
  ! REVISION HISTORY:
  ! 01/07/2011 - Initial Version by Matthieu Marquillie
  ! 13/11/2016 - Updated Version by Ilkay Solak
  !
  !> @param[in] file_name       - name of the file
  !> @param[in] var_name        - name of the variable
  !> @param[in] istart(ndim)    - start points, per process if MPI
  !> @param[in] icount(ndim)    - end points, per process if MPI
     
  !> @param[inout] var          - variable to read      
  !> @return var(:,:,:) real(rk)
  !---------------------------------------------------------------------------  
  subroutine read_var3d(file_name,var_name,var,istart,icount) 
    implicit none
    character(len=*),intent(in) :: file_name,var_name
    real(rk) :: var(:,:,:)
    integer(ik),intent(in) :: istart(3),icount(3)


    integer(ik) :: varid(1),i,ncid

    !-> open file
    call io_check(nf90_open(path=file_name,mode=nf90_nowrite,ncid=ncid))

    !-> get var id
    call io_check(nf90_inq_varid(ncid,var_name,varid(1)))

    !-> get var
    call io_check(nf90_get_var(ncid,varid(1),var,start=istart,count=icount))
   
    !-> close file
    call io_check(nf90_close(ncid))
  
  end subroutine read_var3d

  !---------------------------------------------------------------------------  
  !> @author 
  !> Routine Author Matthieu Marquillie
  !
  ! DESCRIPTION: 
  !> Get 3d variable dimension length in a netcdf file
  !> @brief
  !> Get 3d variable dimension length in a netcdf file
  !
  ! REVISION HISTORY:
  ! 01/07/2011 - Initial Version by Matthieu Marquillie
  ! 13/11/2016 - Updated Version by Ilkay Solak
  !
  !> @param[in] file_name          - name of the file
  !> @param[in] var_name           - name of the variable
  !> @param[inout] dim_name(ndim)  - name of the dimensions
  !> @param[inout] dim_len(ndim)   - length of the dimensions
          
  !> @return dim_name(ndim), dim_len(ndim) character(len=*)
  !--------------------------------------------------------------------------- 
  subroutine get_var3d_info(file_name,var_name,dim_name,dim_len)
    implicit none
    integer(ik),parameter :: ndim=3
    character(len=*),intent(in) :: file_name,var_name
    character(len=*),intent(inout) :: dim_name(ndim), dim_len(ndim)

    integer(ik) :: varid(1)
    integer(ik) :: i,ncid,dimid(ndim)

    !-> open file
    call io_check(nf90_open(path=file_name,mode=nf90_nowrite,ncid=ncid))

    !-> get variable id
    call io_check(nf90_inq_varid(ncid,var_name,varid(1)))

    !-> get variable dimensions id
    call io_check(nf90_inquire_variable(ncid,varid(1),dimids=dimid))

    !-> get dimensions length
    do i=1,ndim
       call io_check(nf90_inquire_dimension(ncid,dimid(i),name=dim_name(i),len=dim_len(i)))
    enddo

    !-> close file
    call io_check(nf90_close(ncid))

  end subroutine get_var3d_info

  !---------------------------------------------------------------------------  
  !> @author 
  !> Routine Author Matthieu Marquillie
  !
  ! DESCRIPTION: 
  !> Returns the size of each dimension for 3d field
  !> @brief
  !> Returns the size of each dimension for 3d field
  !
  ! REVISION HISTORY:
  ! 01/06/2011 - Initial Version by Matthieu Marquillie
  ! 13/11/2016 - Updated Version by Ilkay Solak
  !
  !> @param[in] x          - name of the file
  !> @param[in] var_name           - name of the variable
  !> @param[inout] dim_name(ndim)  - name of the dimensions
  !> @param[inout] dim_len(ndim)   - length of the dimensions
          
  !> @todo Should be only in class_field, not here
  !--------------------------------------------------------------------------- 
  function get_dim_size(x)
    implicit none
    integer :: get_dim_size(3)
    real(rk),intent(in) :: x(:,:,:)
    integer(ik) :: i
    do i=1,3
       get_dim_size(i)=size(x,dim=i)
    enddo
    
  end function get_dim_size

  !---------------------------------------------------------------------------  
  !> @author 
  !> Routine Author Matthieu Marquillie
  !
  ! DESCRIPTION: 
  !> Check netcdf error output and stop code if needed 
  !> @brief
  !> Check netcdf error output and stop code if needed
  !
  ! REVISION HISTORY:
  ! 01/06/2011 - Initial Version by Matthieu Marquillie
  ! 13/11/2016 - Updated Version by Ilkay Solak
  !
  !> @param[in] status          - error status for netcdf
  !--------------------------------------------------------------------------- 
  subroutine io_check(status)
    implicit none
    integer,intent(in) :: status
     
    if(status /= nf90_noerr) then
       print*,trim(nf90_strerror(status))
       print'(a)',"Netcdf Error : aborting"
       stop
    end if
    
  end subroutine io_check

end module class_io
