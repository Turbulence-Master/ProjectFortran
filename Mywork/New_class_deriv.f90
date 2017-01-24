
!describtion 
! you can calc the drivative of any function for any veriable at any point under the following conditions 
!we will use subroutine for explicit schemes 
! we will go for first and second order derivative 
! we will go for first and second order scheme 
!single veriable derivative 
!we will drive the forward ,backward and central derivative 
! we generalize the methods later to be for any orderof schemses and any order of derivative 
! imade the coef as vector cause in the general case i will creat a subroutine that calc the coef then pass it to the other subroutine 
!!.......................................................................................................
module New_class_deriv
use precision 
implicit none
!!.......................................................................................................
 !< description 

 ! for any variable with letter F : it represent the function we want the derivative for
 ! for any variable with letter h of contain h : it represent the grid displacement 
 !for any variable with letter n : it represent the number of points you want to calc the derivative for 
 ! for any variable with letter st: it mean the scheme type (forward or back ward ) 
 !for any variable with letter M : it represent the order of the drevative (first or second )
 !for any variable with letter DF or contain df : it represent the drivative vector 
 !you can control the domain you want to get the drivative for using the value of n like if you make n =1 then you get the derivative 
 !for a single     point 
 


!!.......................................................................................................

!first order scheme forward and backward 
contains 
 subroutine fo_explicit (f,N,M,ST,h,DFX)
 implicit none
 ! defination of veriables  
 real(rk), dimension(:),allocatable :: f , coef ,dfx 

 integer(ik):: n,m,i,j,st,so 
 real(rk)::h
 allocate(coef(5))
 !allocate(f(n))
 !allocate(dfx(n))
  do i=2,n-1
     dfx(i)=0
  end do 
  coef=(/-1,1,1,-2,1/) ! the coeffecient of the scheme prameters 
 if (st==1 .and. m==1) then
   do j=1,n
     dfx(j)=(coef(1)*f(j)+coef(2)*f(j+1))/h !the derivative calculation 
   end do 
 else if (st==1 .and. m==2) then
    do i=2,n-1
       dfx(i)=(coef(3)*f(i)+coef(4)*f(i+1)+coef(5)*f(i+2))/(h**2)
    end do 
 end if 
 !...................................................................
 ! now backward scheme 
  if (st==2 .and. m==1) then
   do j=2,n-1
     dfx(j)=(coef(1)*f(j-1)+coef(2)*f(j))/h
   end do 
 else if (st==2 .and. m==2) then
    do i=2,n-1
       dfx(i)=(coef(3)*f(i-2)+coef(4)*f(i-1)+coef(5)*f(i))/(h**2)
    end do 
 end if 
 return
 end subroutine 
!...................................................................
 ! second order scheme 
 subroutine so_explicit (f,N,M,ST,h,DFX)
 implicit none 
 real(rk), dimension(:),allocatable :: f , coef ,dfx 
 integer(ik):: n,m,i,j,st,so 
 real(rk)::h
 allocate(coef(5))
 !allocate(f(n))
 !allocate(dfx(n))
  do i=2,n-1
     dfx(i)=0
  end do 
   coef=(/-3,4,-1,2,-5,4,-1/)
 !.................................................................................................
  !now forward scheme 
  if (st==1 .and. m==1) then
   do j=2,n-1
     dfx(j)=(coef(1)*f(j)+coef(2)*f(j+1)+coef(3)*f(i+2))/(2*h)
   end do 
  else if (st==1 .and. m==2) then
    do i=2,n-1
       dfx(i)=(coef(4)*f(i)+coef(5)*f(i+1)+coef(6)*f(i+2)+coef(7)*f(i+3))/(h**2)
    end do 
  end if 
 !......................................................................
 ! now backward scheme 
   if (st==2 .and. m==1) then
   do j=2,n-1
     dfx(j)=(-coef(1)*f(j)-coef(2)*f(j-1)-coef(3)*f(i-2))/(2*h)
   end do 
  else if (st==2 .and. m==2) then
    do i=2,n-1
       dfx(i)=(-coef(4)*f(i)-coef(5)*f(i-1)-coef(6)*f(i-2)-coef(7)*f(i-3))/(h**2)
    end do 
  
  end if 
  return
  end subroutine 
 !................................
 !now we will go for centeral scheme with second and forth order accurecy 

  subroutine Ce_explicit (f,N,M,ST,h,DFX)
   implicit none 
   real(rk), dimension(:),allocatable :: f , coef2,coef4 ,dfx 
   integer(ik):: n,m,i,j,st,so 
   real(rk)::h
   allocate(coef2(5))
   allocate(coef4(9))
   !allocate(f(n))
   !allocate(dfx(n))
   do i=2,n-1
     dfx(i)=0
    
  end do  
   coef2=(/-1,1,1,-2,1/)
   coef4=(/1,-8,8,-1,-1,16,-30,16,-1/)
 !............................................................................................................
 ! first we clc driv for second order 
   if (st==3 .and. m==1) then
     do j=2,n-1
       dfx(j)=(coef2(1)*f(j-1)+coef2(2)*f(j+1))/(2*h)
     end do 
   else if (st==3 .and. m==2) then
      do i=2,n-1
       dfx(i)=(coef2(3)*f(i-1)+coef2(4)*f(i)+coef2(5)*f(i+1))/(h**2)
      end do 
   end if 
 !...........................................................................................
 ! second we go for fourth order 
  if (st==4 .and. m==1) then
     do j=2,n-1
       dfx(j)=(coef4(1)*f(j-2)+coef4(2)*f(j-1)+coef4(3)*f(j+1)+coef4(4)*f(j+2))/(12*h)
     end do 
   else if (st==4 .and. m==2) then
      do i=2,n-1
       dfx(i)=(coef4(5)*f(i-2)+coef4(6)*f(i-1)+coef4(7)*f(i)+coef4(8)*f(i+1)+coef4(9)*f(i+2))/(12*h**2)
      end do 
   end if 
    return
  end subroutine 
 !........................................................................................................
 !now we will go for some spetial schemes 
 ! first ; second order drivative in 2D space for structure mesh 
 ! for any variable with letterh: represnt the grid desplaicment 



!.............................................................................................
 subroutine spe2d_schemes(f2,hx,hy,DF2,n)
  implicit none 
  real(rk), dimension(:,:),allocatable :: f2 ,df2 
  integer(ik):: n,m,i,j,st,so 
  real(rk)::hx,hy
  !allocate(f2(n,n))
  !allocate(df2(n,n))
  do i=2,n-1
  do j=2,n-1
     df2(i,j)=0
  end do 
  end do 
   do i=2,n-1
   do j=2,n-1
      Df2(i,j)=(f2(i+1,j)-f2(i-1,j)-f2(i,j-1)+f2(i,j+1))/hx/hy   
   end do 
   end do 
   return
  end subroutine 

!...........................................................................................................
 ! forth order implicit scheme scheme  central for harmonic BC
 !we will use thomas algorithm to calc drevative it self cause i choose the coef to make it tri diag 
 ! incase non periodic fun we can't use thomas algorethm and in this case we will one of fanni algorethm 
 ! we can generalize the way later to be for other orders 
 




 !.......................................................................................
  subroutine sfortho_implicit(f,h,n,dfi)
   implicit none 

   real(rk), dimension(:),allocatable :: f , coef ,dfi ,a,b,c,d,xd
   integer(ik):: n,m,i,j,st
   real(rk)::h
   allocate(coef(5))
  ! allocate(f(n))
   !allocate(dfi(n))
   do i=1,n
     dfi(i)=0
     xd(i)=0
   end do
       
     coef=(/0.0,0.250,1.5,0.0,0.0/) ! you can change the coef so you can change the order of the scheme but i choose those coef to make it tri-diagonal 
    do j=1,n 
      a(j)= coef(2)
      b(j)=1
      c(j)=coef(2)
    end do
         b(1)=coef(3)*f(2)-coef(3)*f(n)
         b(n)=coef(3)*f(1)-coef(3)*f(n-1)
        do i=2,n-1
         d(j)=-coef(3)*f(i-1)+coef(3)*f(i+1)
        end do 
    call thomasalgorithm(a,b,c,d,dfi,n) ! my algorithm but we can use one of fanni algorithm 
  end subroutine 
  !.........................................................................................................................
  
!...........................................................................................................

      subroutine thomasalgorithm(as,bs,cs,ds,Xs,ns)  
     implicit none 
    real(rk), dimension(1:1000000):: ds, bs,as,cs,xs
       integer(ik) :: i,j,ns
   ! forward elemination 
      do i=2, ns
        bs(i)=bs(i)-(as(I)/bs(i-1)*cs(i-1))
        ds(i)=ds(i)-(as(I)/Bs(I-1)*ds(i-1))
     end do 
     xs(ns)= ds(ns)/bs(ns)
    !backward substitution
      do j= ns-1,1,-1
          Xs(j)=(ds(j)-cs(j)*xs(j+1))/bs(j)
      end do 
      return
   end subroutine 

end module
