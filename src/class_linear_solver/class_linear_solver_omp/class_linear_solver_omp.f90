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
	use omp_lib

  implicit none

  !-> Declare everything private by default
  private

  !-> Declare exported procedure
  interface solver
    module procedure solver_matrix
		module procedure thomas
  end interface 

  public :: solver
 
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

	subroutine bksub(A,b,x,n,param)
		implicit none 

		integer(ik),intent(in)::n,param
		real(rk), intent(in) :: A(n,n), b(n)
		real(rk), intent(out) :: x(n)
		
		integer(ik)::i,j,m=0
		integer(ik)::s,r
		real(rk)::coeff
		coeff=0.0000000000000000
	
		
		if (A(n,n)==0) stop "null element on the diagonal"
		x(n)=b(n)/A(n,n)	
!$OMP PARALLEL DO	
		do i=n-1,1,-1
			if (A(i,i)==0) stop "null element on the diagonal" !if element on the diagonal is 0 -> error
			do j=i+1,n,1
				!print*,"coeff prima",coeff
				coeff=coeff+(A(i,j)*x(j))
	
				!print*, "i:", i, "j:" , j , "coeff", coeff	
			end do
			x(i)=(b(i)-coeff)/A(i,i)	
			print*, "i",i,"x(i):", x(i)
			coeff=0.0000000000000000
			
		r=OMP_GET_NUM_THREADS()
		s=OMP_GET_THREAD_NUM()
		print*, "thread num", s
			print*, "num thread", r
		
			!m=omp_get_num_threads()
			print*,m	
		end do

!$OMP END PARALLEL DO	
print*,"inside bksub"

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

	subroutine fwsub(A,b,x,n,param)
		implicit none 
		integer(ik),intent(in)::n,param
		real(rk), intent(in) :: A(n,n), b(n)
		real(rk), intent(out) :: x(n)
		
		integer(ik)::i,j	
		real(rk)::coeff	
    if (A(1,1)==0) then 
			stop "null element on the diagonal"
		end if 
		x(1)=b(1)/A(1,1)		
!$OMP PARALLEL DO	
		do i=2,n,1
			coeff=0.0000000000000000
			if (A(i,i)==0) then 
				stop "null element on the diagonal" !if element on the diagonal is 0 -> error
			end if

			do j=1,i-1,1
				coeff=coeff+(A(i,j)*x(j))			
			end do	
			x(i)=(b(i)-coeff)/A(i,i)	
		end do
!$OMP END PARALLEL DO	
print*,"inside fwsub"
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
  !> @param[in] n size of A b x, b right side array, lu parameter for selection of lu methods    
  !> @param[out] x solution array
  !> @param[inout] A input as the matrix of coeff but output as combination of LU 
  !---------------------------------------------------------------------------  
  
	
	subroutine lu_solver(A,b,x,n,param)
		implicit none

		integer(ik),intent(in)::n,param
		real(rk),intent(inout)::b(n,1)
		real(rk),intent(inout) :: A(n,n)
		real(rk),intent(out) :: x(n,1)
		!real(rk)::L(n)
		!real(rk)::i,j	
		integer(ik)::info,pivots_array(n),nrhs


		!using the subroutine from lapack for the LU decomposition of the matrix A
		! --> info of the function to add 

  
		nrhs=1 !coloumn of b


		call dgetrf( n, n, a, n, pivots_array, info )
			if( info.eq.0 ) then
		   	call dgetrs( 'no transpose', n, nrhs, a, n,pivots_array, b, n, info )
			end if

		x=b

print*,"inside lu solver"
	end subroutine lu_solver

!! thomas algorithm in case of matrix 

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
		integer(rk)::i 
		!! alpha array of n elements, betha n-1 elements, theta n-1 elements
		
		alpha(1)=A(1,1)
!$OMP PARALLEL DO			
		do i=2,n
    	betha(i-1)=A(i,i-1)/alpha(i-1)
			theta(i-1)=A(i-1,i)
			alpha(i)=A(i,i)-betha(i-1)*theta(i-1)
		end do
!$OMP END PARALLEL DO	
print*,"inside thomas algorithm "
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
  !> @param[in] inParam: A tridiagonal matrix, b right side array (known term), n size of A, thom paramether for the selection of    
  !> @param[out] outParam: x solution of the system  
  !---------------------------------------------------------------------------  
 
	subroutine thomas_solver(A,b,x,n,param)
		implicit none
		integer(ik),intent(in)::n,param
		real(rk), intent(in) :: A(n,n),b(n)
		real(rk), intent(out) :: x(n)
		real(rk) :: alpha(n),betha(n-1),theta(n-1), y(n)
		integer(rk)::i 
		!! alpha array of n elements, betha n-1 elements, theta n-1 elements
		
		call thomas_algorithm(n,A,alpha,betha,theta)
		
		y(1)=b(1)		
		!$OMP PARALLEL DO			
		do i=2,n
			y(i)=b(i)-betha(i-1)*y(i-1)		
		end do
		!$OMP END PARALLEL DO	

		x(n)=y(n)/alpha(n)		
		
		!$OMP PARALLEL DO	
		do i=n-1,1,-1
			x(i)=(y(i)-theta(i)*x(i+1))/alpha(i)		
		end do
		!$OMP END PARALLEL DO	
print*,"inside thomas solver"
	end subroutine thomas_solver



	subroutine solver_matrix(A,b,x,n,param)
		implicit none 
		integer(ik),intent(in)::n,param
		real(rk), intent(inout) :: A(n,n)
		real(rk),intent(inout) ::b(n)
		real(rk), intent(out) :: x(n)

		if (param==1) then 
			call bksub(A,b,x,n,param)
		else if (param==2) then 
			call fwsub(A,b,x,n,param)
		else if (param==3) then 
			call lu_solver(A,b,x,n,param)
		else if (param==4) then 
			call thomas_solver(A,b,x,n,param)
		else 
			print*, "ERROR MESSAGE: select function paramether not valid" 
		endif

  end subroutine solver_matrix



!! thomas algorithm with vectors (best choice)


  !---------------------------------------------------------------------------  
  !> @author 
  !> thomas
  !
  ! DESCRIPTION: 
  !> Using the vector alpha,betha,theta from thomas_algorithm solve the 
	!> tridiagonal system
	!
  ! REVISION HISTORY:
  ! TODO_dd_mmm_yyyy - TODO_describe_appropriate_changes - TODO_name
  !
  !> @param[in] inParam: b right side array (known term), n size of A   
  !> @param[inout] inoutParam: alpha,betha,theta arrays of the tridiag matrix
  !> @param[out] outParam: x solution of the system  
  !---------------------------------------------------------------------------

	subroutine thomas(alpha,betha,theta,b,x,n)
		implicit none
		integer,intent(in)::n
		real(rk),intent(inout)::alpha(1:n), betha(1:n-1), theta(1:n-1)
		real(rk),intent(in):: b(1:n)
		real(rk), intent(out) :: x(1:n)
		real(rk) :: y(1:n)
		integer(ik) :: i
		!! alpha array of n elements, betha n-1 elements, theta n-1 elements

		
		do i=2,n
    	betha(i-1)=betha(i-1)/alpha(i-1)
			alpha(i)=alpha(i)-betha(i-1)*theta(i-1)
		end do



!print*,"fine dich variabili"
	!	print*,"definizione di y "		
		y(1)=b(1)		
		do i=2,n
			y(i)=b(i)-betha(i-1)*y(i-1)		
		end do
!print*,"y thomas: ", y
		!print*,"definizione di x "
		x(n)=y(n)/alpha(n)		
		do i=n-1,1,-1
			x(i)=(y(i)-(theta(i)*x(i+1)))/alpha(i)		
		end do
print*,"inside thomas array"
	end subroutine thomas
   
end module linear_solver
