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
  use precision
	!use class_field 

  implicit none

  !-> Declare everything private by default
  private

  !-> Declare exported procedure
  public :: lu_solver
  public :: thomas_solver


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
  !> @param[in] inParam: A upper triangular matrix, b right side array (known term), n size of A   
  !> @param[out] outParam: x solution of the system      
  !--------------------------------------------------------------------------- 

	subroutine bksub(n,A,b,x)
		implicit none 

		integer(ik),intent(in)::n
		real(rk), intent(in) :: A(n,n), b(n)
		real(rk), intent(out) :: x(n)
		
		real(rk)::i,j		
		
		if (A(n,n)==0) stop "null element on the diagonal"
		x(n)=b(n)/A(n,n)		
		do i=n-1,1,-1
			if (A(i,i)==0) stop "null element on the diagonal" !if element on the diagonal is 0 -> error
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
  !> @param[in] inParam: A lower triangular matrix, b right side array (known term), n size of the matrix   
  !> @param[out] outParam: x solution of the system    
  !--------------------------------------------------------------------------- 

	subroutine fwsub(n,A,b,x)
		implicit none 
		integer(ik),intent(in)::n
		real(rk), intent(in) :: A(n,n), b(n)
		real(rk), intent(out) :: x(n)
		
		real(rk)::i,j		
    if (A(1,1)==0) stop "null element on the diagonal"
		x(1)=b(1)/A(1,1)		
		do i=2,n,1
			if (A(i,i)==0) stop "null element on the diagonal" !if element on the diagonal is 0 -> error
			do j=1,i-1,1
				x(i)=(b(i)-(A(i,j)*b(j)))/A(i,i)			
			end do		
		end do
	end subroutine fwsub


  !---------------------------------------------------------------------------  
  !> @author 
  !> lu_solver
  !
  ! DESCRIPTION: 
  !> Solve a linear system with a generic matrix A (square) using the LU 
  !> decomposition from lapack
  !> 
  !> 
  !
  ! REVISION HISTORY:
  ! TODO_dd_mmm_yyyy - TODO_describe_appropriate_changes - TODO_name
  !
  !> @param[in] n size of A b x, b right side array    
  !> @param[out] x solution array
  !> @param[inout] A input as the matrix of coeff but output as combination of LU 
  !---------------------------------------------------------------------------  
  
	
	subroutine lu_solver(n,A,b,x)
		implicit none

		integer(ik),intent(in)::n
		real(rk),intent(in)::b(n)
		real(rk),intent(inout) :: A(n,n)
		real(rk),intent(out) :: x(n)
		real(rk)::L(n)
		real(rk)::i,j	

		!using the subroutine from lapack for the LU decomposition of the matrix A
		! --> info of the function to add 
		call dgetrf(n,n,A,n,pivots_array,info)
		L=A
		do i=1,n
			if A(i,i)/=0
				L(i,i)=1
			end if
		end do
		
		call fwsub(L,b,y)
		call bksub(A,y,x) ! we use A instead to create a new matrix U

	end subroutine lu_solver



  !---------------------------------------------------------------------------  
  !> @author 
  !> thomas_algorithm
  !
  ! DESCRIPTION: 
  !> select from A the vector alpha,betha e theta. 
	!>It works for tridiagonal matrix
	!
  ! REVISION HISTORY:
  ! TODO_dd_mmm_yyyy - TODO_describe_appropriate_changes - TODO_name
  !
  !> @param[in] n size of A, A tridiagonal coefficient matrix      
  !> @param[out] alpha array containing the principal diagola of A,
	!>             betha lower diagonal elements, 
	!>             theta upper diagonal elements
  !---------------------------------------------------------------------------  
 
	subroutine thomas_algorithm(n,A,alpha,betha,theta)
		implicit none 
		
		integer(ik),intent(in)::n
		real(rk), intent(in) :: A(n,n)
		real(rk), intent(out) :: alpha(n),betha(n-1),theta(n-1)
		real(rk)::i	
		!! alpha array of n elements, betha n-1 elements, theta n-1 elements
		
		alpha(1)=A(1,1)
		do i=2,n
    	betha(i-1)=A(i,i-1)/alpha(i-1)
			theta(i-1)=A(i-1,i)
			alpha(i)=A(i,i)-betha(i-1)*theta(i-1)
		end do

	end subroutine thomas_algorithm


  !---------------------------------------------------------------------------  
  !> @author 
  !> thomas_solver
  !
  ! DESCRIPTION: 
  !> Using the vector alpha,betha,theta from thomas_algorithm solve the 
	!> tridiagonal system
	!
  ! REVISION HISTORY:
  ! TODO_dd_mmm_yyyy - TODO_describe_appropriate_changes - TODO_name
  !
  !> @param[in] inParam: A tridiagonal matrix, b right side array (known term), n size of A   
  !> @param[out] outParam: x solution of the system  
  !---------------------------------------------------------------------------  
 
	subroutine thomas_solver(n,A,b,x)
		implicit none
		integer(ik),intent(in)::n
		real(rk), intent(in) :: A(n,n),b(n)
		real(rk), intent(out) :: x(n)
		real(rk) :: alpha(n),betha(n-1),theta(n-1)
		real(rk)::i
		!! alpha array of n elements, betha n-1 elements, theta n-1 elements
		
		call thomas_algorithm(n,A,alpha,betha,theta)
		
		y(1)=b(1)		
		do i=2,n
			y(i)=b(i)-betha(i-1)*y(i-1)		
		end do

		x(n)=y(n)/alpha(n)		
		do i=n-1,1,-1
			x(i)=(y(i)-theta(i)*x(i+1))/alpha(i)		
		end do

	end subroutine thomas_solver



!	subroutine print_sol(x)
!		implicit none
!	  !subroutine for print the solution on a file maybe
!	end subroutine print_sol
   
end module linear_solver
