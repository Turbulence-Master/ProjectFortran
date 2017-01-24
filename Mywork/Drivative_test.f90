! we will make test for the derivative class
!the test will calc the error between the calculated direvative and the exactdirevative
!we can use harmonic fun to mathc all the condation for all the subroutine >>common case 
! the test will be to the velocity direvative .
! the U = F(x) so X will be function veriable 
!any veriable contain DF>> represent the direvative 
! the scheme type >>> mean you want it forward (1) or backward (2), centeral 2nd order(3), cemtrale 4order(4)
! the scheme method >>> mean explicit(1) or implicit(2) 
! the scheme order>>> mean the order of the scheme first order(1), second order(2)
! the drivative order>>> mean the order of the derivative max 2 but for implicit scheme its only first order driv 


!............................................................................

program Drivative_test
use precision 
use New_class_deriv
implicit none 
! defination of veriables  
 real(rk), dimension(:),allocatable :: u, Ndf,exdf ! numerical, exact direvatives 
  !real,dimension(10):: exdf
 integer(ik):: np,dri_order,i,j,schtype,schmeh,schor !number of point,direvative order, scheme type , shceme method ,scheme order
 real(rk)::deltax,pi,error,errorf
print*,"numberofpoints,schemetype,scheme_mehtod,scheme_order,derivative_order,look to the comment near the print line" ! the meaning of the these prameter is up in the description
 read*, np
 read*,schtype
 read*,schmeh
 read*, schor
 read*,dri_order
 allocate(u(np))
 allocate(ndf(np))
 allocate(exdf(np))

 
 pi= 4*atan(1.0)
 deltax = (pi-0)/(np-1)
 do i=1,np
    u(i)=sin((i-1)*deltax) ! fuction decleration as vector 
 end do 
 do j=2,np-1
    exdf(j)=cos((j-1)*deltax) ! exact solution for the derivative 
          !print*, exdf
 end do  
       print*, "exact derivative"
       print*, exdf
 if (schtype==1 .and. schmeh==1 .and.schor==1 .and. dri_order==1) then 
    call fo_explicit (u,Np,dri_order,schtype,deltax,ndf)
       print*, "Numirical derivative"
       print*, ndf

 else if (schtype==2 .and. schmeh==1 .and.schor==1 .and. dri_order==2) then
    call fo_explicit (u,Np,dri_order,schtype,deltax,ndf)
               print*, ndf
 else if (schtype==2 .and. schmeh==1 .and.schor==2 .and. dri_order==1) then 
    call so_explicit (u,Np,dri_order,schtype,deltax,ndf)
                   print*, ndf
 else if (schtype==1 .and. schmeh==1 .and.schor==2 .and. dri_order==2) then 
    call so_explicit (u,Np,dri_order,schtype,deltax,ndf)

 else if (schtype==3 .and. schmeh==1 .and.schor==2 .and. dri_order==2) then 
    call ce_explicit (u,Np,dri_order,schtype,deltax,ndf)
                    print*, ndf
 else if (schtype==4 .and. schmeh==1 .and.schor==4 .and. dri_order==1) then 
    call ce_explicit (u,Np,dri_order,schtype,deltax,ndf)
               print*, ndf
 end if 
!..............................................................................



!...............................................................................
 ! error calculation 
 ! we will calc the average error of the derivative for all of the points  
!..........................................................................
 error=0
 do j=2,np-1
  error=error+((ndf(j))**2-(exdf(j))**2)
 end do 
  print*,"error"
 print*, error 
 errorf= sqrt(deltax*error)
 print*,"errorf"
 print*, errorf
end program 









