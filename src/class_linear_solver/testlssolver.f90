!> Program Fannie on linear solver
!
! DESCRIPTION: 
!> This test has the objective to solve linear sistem using Lu decomposition, backward and forkward substitution and to compute the
!> thomas's algorithm for tri-diagonal matrix.
!> Just work with square matrix.
!
! REVISION HISTORY:
! 11 11 2016 - Initial Version
!------------------------------------------------------------------------------

program testlssolver
	use linear_solver
	use precision 
	implicit none 
	
  !---------------------------------------------------------------------------  
  ! DESCRIPTION: 
  !> Test for solving linear system using different methods:  
  !- thomas  
	!- lu decomposition
	!- backward substitution
	!- forkward substitution
	!
	! PARAM:
  !> matrix: test matrix (square) 
  !> rside: right side of the system  
  !> sol: solution of the linear system 
	!> dimens: dimension of the matrix
	!> select_param: paramether for the selection of method
	!> diag,lowerdiag,upperdiag: paramether for construction of test matrix 
  !---------------------------------------------------------------------------

	real(rk),allocatable :: matrix(:,:), rside(:),sol(:)
	integer(ik), parameter::dimens=4 
	integer(ik):: select_param  
	real(rk)::diag(1:dimens), lowerdiag(1:dimens-1), upperdiag(1:dimens-1)

	do select_param=1,4																																			! do for change of methods
			!attention: fortran read the matric by column-majot order
		allocate(matrix(1:dimens,1:dimens),rside(1:dimens),sol(1:dimens))
		if (select_param .eq. 1) then 
			matrix=reshape((/1,0,0,0,1,1,0,0,1,1,1,0,1,1,1,1/),(/dimens,dimens/))								!! select_param=1
			rside=(/10,9,7,4/)	!! exact sol:(1,2,3,4)
			print*, "backward substitution"
		else if (select_param .eq. 2) then 
			matrix=reshape((/1,1,1,1,0,1,1,1,0,0,1,1,0,0,0,1/),(/dimens,dimens/))								!! select_param=2
			rside=(/1,3,6,10/)	!! exact sol: (1,2,3,4)
			print*, "forkward substitution"
		else if (select_param .eq. 3) then 
			matrix=reshape((/1,1,1,1,0,1,1,1,0,0,1,1,0,0,0,1/),(/dimens,dimens/))								!! select_param=3
			rside=(/1,3,6,10/)	!! exact sol: (1,2,3,4)
			print*, "lu solver" 
		else if (select_param .eq. 4) then
			matrix=reshape((/4,-1,0,0,-1,4,-1,0,0,-1,4,-1,0,0,-1,4/),(/dimens,dimens/)) 				!! select_param=4
			rside=(/2,4,6,13/)	!! exact sol: (1,2,3,4) 
			print*, "thomas algorithm"
		end if 
		
		call solver(matrix,rside,sol,dimens,select_param) 
			print*, "solution:" ,sol
		deallocate(matrix,rside,sol)
	end do
	
	print*,"thomas algorithm with array input"																							! thomas algorithm with array input
	diag=4																																									! definition of test array
	lowerdiag=-1
	upperdiag=-1
	allocate(rside(1:dimens),sol(1:dimens))
	rside=(/2,4,6,13/)      !! exact sol: (1,2,3,4)

	call solver(diag,lowerdiag,upperdiag,rside,sol,dimens)
	print*, "solution:" ,sol		
	deallocate(rside,sol)
end program testlssolver



