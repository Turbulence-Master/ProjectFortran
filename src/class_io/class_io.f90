!------------------------------------------------------------------------------
! IMPT, ProjectFortran
!------------------------------------------------------------------------------
!
! MODULE: Module Name
!
!> @author
!> Module Author Name and Affiliation
!
! DESCRIPTION: 
!> Brief description of module.
!
! REVISION HISTORY:
! DD Mmm YYYY - Initial Version
! TODO_dd_mmm_yyyy - TODO_describe_appropriate_changes - TODO_name
!------------------------------------------------------------------------------

module class_io
  use netcdf
  use precision
!  use class_field  ! N/A yet
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
  !> Routine Author Name and Affiliation.
  !
  ! DESCRIPTION: 
  !> Brief description of routine. 
  !> @brief
  !> Flow method (rate of change of position) used by integrator.
  !> Compute \f$ \frac{d\lambda}{dt} , \frac{d\phi}{dt},  \frac{dz}{dt} \f$
  !
  ! REVISION HISTORY:
  ! TODO_dd_mmm_yyyy - TODO_describe_appropriate_changes - TODO_name
  !
  !> @param[in] inParam      
  !> @param[out] outParam      
  !> @return returnValue
  !---------------------------------------------------------------------------  
   
  function someFunction
    use AnotherModule  
     
    real(rk), intent(in) :: inParam        
    real(rk), intent(out) :: outParam       
    real(rk), intent(inout) :: inOutParam   !TODO_description
    real(rk) :: returnValue                 
      
    real(rk) :: someVariable                !> @var Variable description
 
    ! TODO_insert_code_here
 
  end function someFunction
   
end module class_io
