module DerivativeClass
implicit none 
  !first declaration for input data for veriable 
  REal,dimension (1:10000,1:10000,1:20) :: u,roh,T,p !3Darray 1stD for x direction , secD for y direction , thrD for time 
   ! declaration of the drivatives types 
   real, dimension (1:10000) :: dux,
 !declaration for dimension input data 
  real :: deltaT,deltaX,deltaY
  !decleration for the order of the derivative 
  integer :: n
  !decleration for the truncation error order 
  real :: terrorfsx11,terrorfsx12,terrorcsx12,terrorfsy11,terrorfsy12,terrorcsy12,terrorcsxy21,terrort11, terrorcx22,terrorcy22,terrorcx24,terrorcy24
  !decleration for the rest veriables 
 integer :: i,j,t
  !input DATA for velocity array and all the other veriabls                     
    ! recomended to input the velocity array using a data.dat file open("filename")
  ! 1D space scheme for constant time (t=const) and constant y
      j=
      t=
    
 do i= initial11 , final11 
    
   Dux11(i) = (u(i+1,j,t)-u(i,j,t))/deltaX
 end do 
!initail12 & final12 must be adapted to the scheme or you will have to define a special boundary condition for each problem  to solve at the boundry        
   do i= initial12, final12
      Dux12(i)=(-3*u(i,j,t)+4*u(i+1,j,t)-u(i+2,j,t))/2*deltaX
   end do
    do i= initaialc12 , finalc12
        Duxc12(i)= (u(i+1,j,t)-u(i-1,j,t))/2*deltax 
    end do         
     do i= inititalc22, finalc22
        Duxc22(i)=(u(i+1,j,t)-2*u(i,j,t)+u(i-1,j,t))/deltax**2
     end do

      do i=initialc24,finalc24
         Duxc24(i)=(-u(i-2,j,t)+16*u(i-1,j,t)-3*u(i,j,t)+16*u(i,j,t)-u(i+2,h,t))/(12*(deltax**2))
      end do 
 ! you can do the same for y direction with  constant x and constant t

  ! 2d space scheme for constant time 
      t=
    do i= initialcx2d11, finalcx2d11
       do j= initialcy2d11, finalcy2d11
           Duxy1111(i,j)=(u(i+1,j+1,t)-u(i+1,j,t)-u(i,j+1,t)+u(i,j,t))/deltax/deltay
         end do 
   end do 
    !time scheme Euler explicit scheme for constant location in space 
       i=
        j= 
         do t=initialt11,finalt11
            Dut11(i,j,t)=(u(i,j,t+1)-u(i,j,t))/deltaT
          end do 
     ! we can generalize this to be variable in space and time in case we deal with multi grid method and we want to intersict both solution to converge  faster and less expensive 
         !i will complete the truncation erro part with the stability after i get a general algorithm and code for stability and concistency 
          ! and hopefuly i will be able to generize the code for any order for the scheme and the drivative 

     

 



  
end module DerivativeClass
