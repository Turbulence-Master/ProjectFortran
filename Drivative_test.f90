! we will make test for the derivative class
!the test will calc the error between the calculated direvative and the exactdirevative
!we can use harmonic fun to mathc all the condation for all the subroutine >>common case 
! the test will be to the velocity direvative .
! the U = F(x) so X will be function veriable 
!any veriable contain DF>> represent the direvative 




!............................................................................

program Drivative_test
 use precision 
 use drivativ_Class
 implicit none 

! defination of veriables  
 real, dimension(:),allocatable :: u ,Ndf,exdf ! numerical, exact direvatives 

 integer(rk):: np,dri_order,i,j,schtype,schmeh,schor !number of point,direvative order, scheme type , shceme method ,scheme order
 real(rk)::deltax,pi,error,errof
 allocate(u(n))
 allocate(fdf(n))
 allocate(bdf(n))
 allocate(cdf(n))
 allocate(exdf(n))
 print*,"numberofpoints,schemetype,scheme_mehtod,sheme_order,derivative_order"
 read*, np
 read*,schtype
 read*,schmeh
 read*, schor
 read*,dri_order
 
 pi= 4*atan(1)
 deltax = (pi-0)/(n-1)
 do i=1,n
    u(i)=sin((i-1)*deltax) ! fuction decleration as vector 
 end do 
 do i=1,n
    exdf=cos((i-1)*deltax)) ! exact solution for the derivative 
 end do 

 if (schtype==1 .and. schemeh==1 .and.schor==1 .and. dri_order==1) then 
    call fo_explicit (u,Np,dri_order,schtype,deltax,ndf)

 else if (schtype==2 .and. schemeh==1 .and.schor==1 .and. dri_order==2) then
    call fo_explicit (u,Np,dri_order,schtype,deltax,ndf)

 else if (schtype==2 .and. schemeh==1 .and.schor==2 .and. dri_order==1) then 
    call so_explicit (u,Np,dri_order,schtype,deltax,ndf)

 else if (schtype==1 .and. schemeh==1 .and.schor==2 .and. dri_order==2) then 
    call so_explicit (u,Np,dri_order,schtype,deltax,ndf)

 else if (schtype==3 .and. schemeh==1 .and.schor==2 .and. dri_order==2) then 
    call ce_explicit (u,Np,dri_order,schtype,deltax,ndf)

 else if (schtype==4 .and. schemeh==1 .and.schor==4 .and. dri_order==1) then 
    call ce_explicit (u,Np,dri_order,schtype,deltax,ndf)
 end if 
!..............................................................................



!...............................................................................
 ! error calculation 
 ! we will calc the average error of the derivative for all of the points  
!..........................................................................
 error=0
 do j=1,n
  error=error+(ndf**2-exdf**2)
 end do 
 errorf= sqrt(deltax*error)
end program 









