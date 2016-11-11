!> Module Fannie on linear solver
!
! DESCRIPTION: 
!> This module has the objective to solve linear sistem using Lu decomposition and to compute the thomas's algorithm for tri-diagonal matrix,
! for the moment it just work with square matrix
!
! REVISION HISTORY:
! 11 11 2016 - Initial Version
!------------------------------------------------------------------------------

module linear_solver
  use netcdf
  use precision
! use module from lapack (still work in progress)

  implicit none

  !-> Declare everything private by default
  private

  !-> Declare exported procedure
  public :: lu_solver
  public :: thomas_solver


	interface 
!! interface for the subroutine for the LU decomposition from Lapack
	end interface


contains


	!---------------------------------------------------------------------------  
  !> @author 
  !> bksub -> backward substitution.
  !
  ! DESCRIPTION: 
  !> Method for solve linear system with square upper triangular matrices. 
  !
  ! REVISION HISTORY:
  ! TODO_dd_mmm_yyyy - TODO_describe_appropriate_changes - TODO_name
  !
  !> @param[in] inParam: A upper triangular matrix, b right side array (known term)   
  !> @param[out] outParam: x solution of the system      
  !--------------------------------------------------------------------------- 

	subroutine bksub(n,A,b,x)
		implicit none 
		!! define A,b as intent in allocatable, n is the size of the matix
    !! define x as output same
		! define i,j

    !catch the error in case of non square matrix
		x(n)=b(n)/A(n,n)		
		do i=n-1,1,-1
		!if element on the diagonal is 0 -> error
			do j=i+1,n,1
				x(i)=(b(i)-(A(i,j)*b(j)))/A(i,i)			
			end do		
		end do

	end subroutine bksub

	!---------------------------------------------------------------------------  
  !> @author 
  !> fwsub -> forkward substitution.
  !
  ! DESCRIPTION: 
  !> Method for solve linear system with square lower triangular matrices. 
  !
  ! REVISION HISTORY:
  ! TODO_dd_mmm_yyyy - TODO_describe_appropriate_changes - TODO_name
  !
  !> @param[in] inParam: A lower triangular matrix, b right side array (known term)   
  !> @param[out] outParam: x solution of the system    
  !--------------------------------------------------------------------------- 

	subroutine fwsub(A,b,x)
		implicit none 
		!! define A,b as intent in allocatable, n is the size of the matix
    !! define x as output same
		!! define i,j

    !catch the error in case of non square matrix
		x(1)=b(1)/A(1,1)		
		do i=2,n,1
		!if element on the diagonal is 0 -> error
			do j=1,i-1,1
				x(i)=(b(i)-(A(i,j)*b(j)))/A(i,i)			
			end do		
		end do
	end subroutine fwsub


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
  
	subroutine lu_decomp(n,A,L,U)
		implicit none
		!declaration of all the variable with intent, and precision from our module
		!! solving the linear sistem using decomposition LU from Lapack
	end subroutine lu_decomp
	
	subroutine lu_solver(n,A,b,x)
		implicit none
		!declaration of all the variable with intent, and precision from our module
		!! solving the linear sistem using decomposition LU from Lapack

   !sfrutta lu_decomp per ricavare LU e bksub + fwsub to solve the two system
		call lu_decomp(n,A,L,U)
		
		call fwsub(L,b,y)
		call bksub(U,y,x)

	end subroutine lu_solver



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
!! thomas algorithm create the 3 vector and than we can compute the 
	subroutine thomas_algorithm(n,A,alpha,betha,theta)
		implicit none 
		!!declaration of all the variable with intent, and precision from our module
		!integer(ik), intent(in)::n
		!real(rk),intent(in)::A(:,:) ...
		!! alpha array of n elements, betha n-1 elements, theta n-1 elements
		alpha(1)=A(1,1)
		do i=2,n
    	betha(i-1)=A(i,i-1)/alpha(i-1)
			theta(i-1)=A(i-1,i)
			alpha(i)=A(i,i)-betha(i-1)*theta(i-1)
		end do

	end subroutine thomas_algorithm

	subroutine thomas_solver(n,A,b,x)
		implicit none
		!declaration of all the variable with intent, and precision from our module
!
!  all declaration of variables
!
		call thomas_algorithm(n,A,alpha,betha,theta)
		
		y(1)=b(1)		
		do i=2,n
			y(i)=b(i)-betha(i-1)*y(i-1)		
		end do

		x(n)=y(n)/alpha(n)		
		do i=n-1,1
			x(i)=(y(i)-theta(i)*x(i+1))/alpha(i)		
		end do

	end subroutine thomas_solver



	subroutine print_sol(x)
		implicit none
		!subroutine for print the solution on a file maybe
	end subroutine print_sol
   
end module linear_solver
