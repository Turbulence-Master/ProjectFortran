program testlssolver
	use linear_solver
	implicit none 
	

	real(8),allocatable :: matrix(:,:), rside(:),sol(:)
	integer, parameter::dimens=4 !3
	integer:: select_param    !,i
	real(8)::diag(1:dimens), lowerdiag(1:dimens-1), upperdiag(1:dimens-1)
	!! here we can change the paramether in orther to use a different methods
	!select_param=4

	
				!! non funziona   ! funziona 
	do select_param=1,4
		!attention: fortran read the matric by column-majot order
		allocate(matrix(1:dimens,1:dimens),rside(1:dimens),sol(1:dimens))
		if (select_param .eq. 1) then 
			matrix=reshape((/1,0,0,0,1,1,0,0,1,1,1,0,1,1,1,1/),(/dimens,dimens/))	!! select_param=1
			rside=(/10,9,7,4/)	!! sol:(1,2,3,4)
		else if (select_param .eq. 2) then 
					!matrix=reshape((/-3,1,-1,0,1,3,0,0,1/),(/dimens,dimens/))	!! select_param=2, dim=3
					!rside=(/-6,3,3/)	!! sol: (2,1,2)
			matrix=reshape((/1,1,1,1,0,1,1,1,0,0,1,1,0,0,0,1/),(/dimens,dimens/))	!! select_param=2, dim=4
			rside=(/1,3,6,10/)	!! sol: (1,2,3,4)
		else if (select_param .eq. 3) then 
			matrix=reshape((/1,1,1,1,0,1,1,1,0,0,1,1,0,0,0,1/),(/dimens,dimens/))	!! select_param=3, dim=4
			rside=(/1,3,6,10/)	!! sol: (1,2,3,4)
		else if (select_param .eq. 4) then
			matrix=reshape((/4,-1,0,0,-1,4,-1,0,0,-1,4,-1,0,0,-1,4/),(/dimens,dimens/))  !select_param=4
			rside=(/2,4,6,13/)	!! sol: (1,2,3,4) 
		end if 
		
		call solver(matrix,rside,sol,dimens,select_param)
			!print*,"i= ",i	
			print*, "solution:" ,sol
		deallocate(matrix,rside,sol)
	end do
	
print*,"entra in thomas vect"
	diag=4
	lowerdiag=-1
	upperdiag=-1
	allocate(rside(1:dimens),sol(1:dimens))
	rside=(/2,4,6,13/)

	call solver(diag,lowerdiag,upperdiag,rside,sol,dimens)
	print*, "solution:" ,sol		
	deallocate(rside,sol)
end program testlssolver



