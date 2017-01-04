program numerical_integration_test
    USE precision
    USE numerical_integration
    IMPLICIT NONE
    
    INTEGER(ik) :: j,N
    REAL(rk) :: Arear, Areat, Aream, Areas, a, b, h
    print*,"Enter the value of N:"
    read*, N
    print*,"Enter the value for upper bound:"
    read*, b
    print*,"Enter the value for lower bound:"
    read*, a
      h = (b-a)/N
      Arear = 0
      Areat = 0
      Aream = 0
      Areas = 0
      do j = 0, N
        CALL rectangular(N,j,h,a,Arear)
        CALL trapezoidal(N,j,h,a,Areat)
        CALL mid_point(N,j,h,a,Aream)
        CALL simpson(N,j,h,a,Areas)
      end do
      print*, "Rectangular rule area = ",Arear
      print*, "Trapezoidal rule area = ",Areat
      print*, "Mid Point rule area   = ",Aream
      print*, "Simpson rule area     = ",Areas
end program numerical_integration_test
