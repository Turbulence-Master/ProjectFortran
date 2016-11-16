Program Approx
parameter(SIZE=25)
integer i,ij,j,k,n,n1,m,m1,m2
real*8  C(SIZE,SIZE)
real*8  A(SIZE),B(SIZE),X(SIZE),Xc(SIZE),Y(SIZE),Yx(SIZE)
real*8  p,xx,s,yc
  write(*,10,advance='no'); read *, n
  n=n-1
  write(*,20,advance='no'); read *, m
  n1=n+1; m1=m+1; m2=m+2

 print *,' Function to approximate:'
  do i=1, n1
if (i<10) then 
  write(*,30,advance='no') i, i
    else
  write(*,31,advance='no') i, i
    end if
read *, X(i), Y(i)
  end do
  do k=1, m2
    Xc(k)=0.d0
    do i=1, n1
  Xc(k) = Xc(k) + X(i)**k
    end do
  end do
  yc=0.d0
  do i=1, n1  
    yc = yc + Y(i)
  end do
  do k=1, m
    Yx(k)=0.d0
    do i=1, n1
  Yx(k) =  Yx(k) + Y(i)*X(i)**k
    end do 
  end do
  do i=1, m1
do j=1, m1
      ij=i+j-2
      if (i==1.and.j==1)  then
    C(1,1) = n1
      else 
    C(i,j)=Xc(ij)
      end if 
    end do
  end do
  B(1)=yc; 
  do i=2,m1
    B(i)=Yx(i-1)
  end do 
  do k=1, m
    do i=k+1, m1
      B(i) = B(i) - C(i,k)/C(k,k)*B(k)
      do j=k+1, m1
        C(i,j) = C(i,j) - C(i,k)/C(k,k)*C(k,j)
      end do
    end do
  end do
  A(m1)=B(m1)/C(m1,m1)
  do i=m, 1, -1
    s=0.d0
    do k=i+1, m1  
  s = s + C(i,k)*A(k)
    end do 
    A(i) = (B(i)-s)/C(i,i)
  end do
  print *,' '
  write(*,40)  m, n+1
  print *,' Coefficients of polynomial:'
  do i=1, m1
    write(*,50)  i-1, A(i)
  end do
  print *,' ' 
  print *,' Approximated function:'
  print *,'       X           Y  '
  do i=1, n1
   xx=X(i); p=0.d0
    do k=1, m1
  p = p*xx + A(m1+1-k)
    end do 
    write(*,60)  xx, p
  end do
END PROGRAM Approx

